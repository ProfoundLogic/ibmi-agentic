**FREE
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(Main);

// External Procedures
// ----------------------------------------------------------------------
DCL-PR SQLDIAG EXTPGM;
  @PGM CHAR(10) CONST;
  @SCSNOTIFY CHAR(1) CONST;
  @SCSPRINT  CHAR(1) CONST;
  @INIT CHAR(1) CONST;
END-PR;

DCL-PR PUISRCHUI EXTPGM;
  @SRAPP     CHAR(10) CONST;
  @SRNAM     CHAR(30);
  @SRVAL     CHAR(2000);
  @SRDFT     CHAR(1) CONST;
END-PR;

DCL-PR BuildSQLOrder EXTPGM('SQLORDER');
  ORDR1   CHAR(50);
  ORDR2   CHAR(50);
  ORDR3   CHAR(50);
  ORDRTXT CHAR(200);
END-PR;

DCL-PR PODTLUI EXTPGM;
  @EOJ       CHAR(1);
  @RET       ZONED(5); // Current record #
  @SIZ       ZONED(5); // Total # of records
  @OHYY      CHAR(2);
  @OH#       ZONED(6); // 0 = New PO Mode
END-PR;

DCL-PR USRRTV EXTPGM;
  @USER      CHAR(10) CONST;
  @PGM       CHAR(10) CONST;
  @LVL       CHAR(10);
END-PR;

DCL-PR SQL2XLSXD EXTPGM;
  @PGM        CHAR(10) CONST;
  @XLSFILE    CHAR(50) CONST;
  @NBR        CHAR(14) CONST;
  @XLSSHEET   CHAR(31) CONST;
  @XLSSEQ     PACKED(3) CONST;
  @XLSFINAL   CHAR(1) CONST;
  @EMAIL      CHAR(50) CONST;
  @LIST       CHAR(10) CONST;
END-PR;

// File Declarations
// ----------------------------------------------------------------------
DCL-F POSELUI WORKSTN HANDLER('PROFOUNDUI(HANDLER)')
  SFILE(SFLFMT:SFLR#);

DCL-S PGM        CHAR(10) INZ('POSELUI');
DCL-S USER       CHAR(10) INZ(*USER);
// Maximum number of records to show on the screen
DCL-S SFLMAX     PACKED(5) INZ(1000);
DCL-S USRLVL     CHAR(10); // NOV, INT, EXP, PGMR...
DCL-S USRMODE    PACKED(1); // Number version of above
DCL-S OH#        ZONED(6);
DCL-S SQLWHERE   VARCHAR(5000);
DCL-S FIRSTLOAD  IND INZ(*OFF);

DCL-PROC Main;
  // *ENTRY Prototype
  DCL-PI *N EXTPGM('POSELUI');
    @EOJ       CHAR(1);
    @LOAD      CHAR(1) CONST; // Y to load subfile at program start
    @NN        CHAR(2);
    @WH        CHAR(2);
  END-PI;
  DCL-S @RET       ZONED(5);
  DCL-S @SIZ       ZONED(5);
  DCL-S UISAP      CHAR(10);
  // *INZSR - Initialization
  EXEC SQL
    SET OPTION NAMING = *SYS,
                          COMMIT = *none,
                          DATFMT = *ISO,
                          TIMFMT = *ISO;
  IF NOT %OPEN(POSELUI);
    OPEN POSELUI;
  ENDIF;

  // Set constants for search
  CTLNN = @NN;
  CTLWH = @WH;

  // Fallback Search values
  SRNN = @NN;
  SRWH = @WH;
  SRCODEI = *ON;
  SRCODED = *ON;
  SRCODEE = *ON;
  SRSTAT_ = *ON;
  SRSTATH = *ON;
  SRSTATX = *OFF;
  SRSTATC = *OFF;
  SRMETHY = *ON;
  SRMETHN = *ON;
  SRVEND = 0;
  SRSORDR = *BLANKS;
  SRFRT = *BLANKS;
  SRREQ = *BLANKS;
  SRRDTELO = %DATE(%CHAR(%SUBDT(%DATE():*YEARS)) + '-01-01');
  SRRDTEHI = *LOVAL;
  SRCHBY = *BLANKS;
  SRCHDTLO = *LOVAL;
  SRCHDTHI = *LOVAL;
  SRPARTX = *OFF;
  SRPART = *BLANKS;
  SRDESCX = *OFF;
  SRDESC = *BLANKS;
  SRDDATLO = *LOVAL;
  SRDDATHI = *LOVAL;
  SRIREQ = 0;
  SRWSJOB = 0;

  CTLORDR1 = 'OHCHDT';
  CTLORDR2 = 'OHVEND';
  CTLORDR3 = 'OHSTAT';

  // Check for a default search and set if found
  PSRDFT = 'Y';
  //
  PUISRCHUI(PGM:PSRNAM:PSRVAL:PSRDFT);
  IF @LOAD = 'Y' AND PSRVAL <> *BLANKS;
    CTLVAL = PSRVAL;
  ELSEIF @LOAD = 'Y';
    // If not found, load fallback search above
    SFLCHG = *ON;
  ELSE;
    // No load, prime search for refresh
    //    CTLOLDVAL = PSRVAL;
    CTLVAL = PSRVAL;
    FIRSTLOAD = *ON;
  ENDIF;

  // Retrieve user update level
  USRRTV(USER:'OHMGR':USRLVL);
  SELECT;
  WHEN USRLVL = 'NOV';
    USRMODE = 1;
  WHEN USRLVL = 'INT';
    USRMODE = 2;
  WHEN USRLVL = 'EXP';
    USRMODE = 3;
  WHEN USRLVL = 'PGMR';
    USRMODE = 4;
  OTHER;
    USRMODE = 0;
  ENDSL;

  CTLNEWTT = 'Create a New PO';
  // get user name info for UISAP
  EXEC SQL
    SELECT uisap
      INTO :uisap
      FROM userids
      WHERE uiuser = :user;
  // No create if no perms or SAP ID
  IF USRMODE = 0 OR UISAP = *BLANKS;
    CTLNEWDIS = *ON;
    CTLNEWTT = 'Bad SAP ID or Authority Level';
  ENDIF;

  // ------------------------------------------------------------------- //
  DOW 1 = 1;
    IF @EOJ = 'Y'; // (Keep this first) Exit button from downstream
      RETURN;
    ENDIF;
    SRSAVEDIS = *ON;
    SRSAVETT = 'Create a custom search to save';
    IF SFLCHG = *ON; // New search or reload
      IF CTLOLDVAL = CTLVAL;
        CTLVAL = '';
        SRSAVEDIS = *OFF;
        SRSAVETT = 'Save custom search';
      ENDIF;
      CTLOLDVAL = CTLVAL;
      IF FIRSTLOAD = *ON;
        FIRSTLOAD = *OFF;
      ELSE;
        LoadHeaderSubfile();
      ENDIF;
      SFLCHG = *OFF;
    ELSE;
      IF CTLOLDVAL <> *BLANKS;
        CTLVAL = CTLOLDVAL;
        SRSAVEDIS = *ON;
        SRSAVETT = 'Create a custom search to save';
      ENDIF;
      IF CTLREFRESH = *ON;
        LoadHeaderSubfile();
      ENDIF;
    ENDIF;
    PSRNAM = *BLANKS;
    PSRVAL = *BLANKS;
    PSRDFT = *BLANKS;
    EXFMT CTLFMT;
    IF SFLR# > 0;
      CHAIN SFLR# SFLFMT;
    ENDIF;
    SELECT;
    WHEN CTLBACK = *ON;
      RETURN;
    WHEN CTLEXIT = *ON;
      @EOJ = 'Y';
      RETURN;
    WHEN CTLGOTO = *ON;
      GoToDetail(@EOJ);
    WHEN CTLDTL = *ON;
      LoadDetail(@EOJ);
    WHEN CTLNEW = *ON;
      @RET = 0;
      @SIZ = 0;
      OHYY = *BLANKS;
      OH# = 0;
      PODTLUI(@EOJ:@RET:@SIZ:OHYY:OH#);
    WHEN SRSAVE = *ON AND PSRNAM <> *BLANKS;
      PUISRCHUI(PGM:PSRNAM:PSRVAL:PSRDFT);
      CTLVAL = PSRVAL;
      CTLOLDVAL = CTLVAL;
    WHEN SREDIT = *ON;
      PUISRCHUI(PGM:PSRNAM:PSRVAL:PSRDFT);
      IF PSRVAL <> CTLVAL;
        SFLCHG = *ON;
      ENDIF;
    WHEN CTLDL = *ON;
      DownloadResults();
    WHEN CTLREFRESH = *ON;
      IF PSRNAM = 'Last search' and FIRSTLOAD = *OFF;
        PUISRCHUI(PGM:PSRNAM:PSRVAL:PSRDFT);
      ENDIF;
    OTHER;
    ENDSL;
  ENDDO;
ON-EXIT;
  CLOSE POSELUI;
END-PROC;
// ------------------------------------------------------------------- //

// ------------------------------------------------------------------- //
DCL-PROC LoadHeaderSubfile;
  DCL-S SFLSTMT    VARCHAR(5000);
  DCL-S SFLORDR    CHAR(200);

  BuildSQLOrder(CTLORDR1:CTLORDR2:CTLORDR3:SFLORDR);

  SQLWHERE = '+
  WHERE OHPP = COALESCE(NULLIF(?, ''''), OHPP) +
    AND OHDC = COALESCE(NULLIF(?, ''''), OHDC) +
    AND ((? = 1 AND OHCODE = ''I'') +
      OR (? = 1 AND OHCODE = ''D'') +
      OR (? = 1 AND OHCODE = ''E'')) +
    AND ((? = 1 AND OHSTAT = '' '') +
      OR (? = 1 AND OHSTAT = ''H'') +
      OR (? = 1 AND OHSTAT = ''X'') +
      OR (? = 1 AND OHSTAT = ''C'')) +
    AND ((? = 1 AND OHMETH = ''Y'') +
      OR (? = 1 AND OHMETH IN (''N'',''A''))) +
    AND OHVEND = COALESCE(NULLIF(?, 0), OHVEND) +
    AND LOCATE(TRIM(COALESCE(?, '''')), COALESCE(ORSORDR, '''')) > 0 +
    AND OHFRT = COALESCE(NULLIF(?, ''''), OHFRT) +
    AND OHREQ = COALESCE(NULLIF(?, ''''), OHREQ) +
    AND OHRDTE BETWEEN COALESCE(NULLIF(?,''0001-01-01''), OHRDTE) +
                   AND COALESCE(NULLIF(?,''0001-01-01''), OHRDTE) +
    AND OHCHBY = COALESCE(NULLIF(?, ''''), OHCHBY) +
    AND OHCHDT BETWEEN COALESCE(NULLIF(?,''0001-01-01''), OHCHDT) +
                   AND COALESCE(NULLIF(?,''0001-01-01''), OHCHDT) +
    AND EXISTS ( +
      SELECT 1 FROM ORITM +
      WHERE OH.OHYY = OIYY +
        AND OH.OHPP = OIPP +
        AND OH.OH# = OI# +
        AND ((? = 1 AND OIPART = ?) +
          OR (? = 0 AND LOCATE(TRIM(COALESCE(?,'''')), OIPART) > 0)) +
        AND ((? = 1 AND OIDESC = ?) +
          OR (? = 0 AND LOCATE(TRIM(COALESCE(?,'''')), OIDESC) > 0)) +
        AND OIDDAT BETWEEN COALESCE(NULLIF(?,''0001-01-01''), OIDDAT) +
                       AND COALESCE(NULLIF(?,''0001-01-01''), OIDDAT) +
        AND OIREQ = COALESCE(NULLIF(?, 0), OIREQ) +
        AND OIWSJOB = COALESCE(NULLIF(?, 0), OIWSJOB) +
        )';

  SFLSTMT = '+
  SELECT OH.OHSTAT, +
          CASE OH.OHSTAT +
            WHEN '' '' THEN ''Open'' +
            WHEN ''C'' THEN ''Closed'' +
            WHEN ''H'' THEN ''On Hold'' +
            WHEN ''X'' THEN ''Canceled'' +
            ELSE '''' +
          END AS OHSTATDSC, +
          OH.OHYY, +
          OH.OHPP, +
          OH.OH#, +
          ''PR'' || OHYY || OH# AS PONUM, +
          OH.OHVEND, +
          OH.OHREQ, +
          OH.OHRDTE, +
          OH.OHDC, +
          OH.OHCHDT, +
          OH.OHCHBY, +
          OH.OHCODE, +
          CASE OH.OHCODE +
            WHEN ''D'' THEN ''Drop Ship'' +
            WHEN ''I'' THEN ''Inventory'' +
            WHEN ''E'' THEN ''Ecom'' +
            ELSE '''' +
          END AS OHCODEDSC, +
          VN.VNNAME, +
          COALESCE(ORSORDR, '''') AS PRANUM +
  FROM ORHDR OH +
  JOIN VENDMST VN +
    ON OH.OHVEND = VN.VNUMB +
  LEFT JOIN ORECOMM +
         ON OHORDID = ORORDID'
  + ' ' + %TRIM(SQLWHERE)
  + ' ' + %TRIM(SFLORDR)
  + ' LIMIT ' + %CHAR(SFLMAX);

  EXEC SQL
    PREPARE sflstmt FROM :sflstmt;
  //  SQLDIAG(PGM:'N':'N':'N');

  EXEC SQL
    DECLARE sflcsr CURSOR FOR sflstmt;
  //  SQLDIAG(PGM:'N':'N':'N');

  SFLCLR = *ON;
  WRITE CTLFMT;
  SFLCLR = *OFF;
  SFLR# = 0;
  SFLSIZ = 0;

  EXEC SQL
    OPEN sflcsr USING :srnn,
                      :srwh,
                      :srcodei,
                      :srcoded,
                      :srcodee,
                      :srstat_,
                      :srstath,
                      :srstatx,
                      :srstatc,
                      :srmethy,
                      :srmethn,
                      :srvend,
                      :srsordr,
                      :srfrt,
                      :srreq,
                      :srrdtelo,
                      :srrdtehi,
                      :srchby,
                      :srchdtlo,
                      :srchdthi,
                      :srpartx,
                      :srpart,
                      :srpartx,
                      :srpart,
                      :srdescx,
                      :srdesc,
                      :srdescx,
                      :srdesc,
                      :srddatlo,
                      :srddathi,
                      :srireq,
                      :srwsjob;
  //  SQLDIAG(PGM:'N':'N':'N');

  DOW SFLR# < SFLMAX;
    RESET SFLFMT;
    EXEC SQL
      FETCH sflcsr
        INTO :ohstat,
             :ohstatdsc,
             :ohyy,
             :ohpp,
             :oh#,
             :ponum,
             :ohvend,
             :ohreq,
             :ohrdte,
             :ohdc,
             :ohchdt,
             :ohchby,
             :ohcode,
             :ohcodedsc,
             :vnname,
             :pranum;
    IF %SUBST(SQLSTATE:1:2) = '02';
      LEAVE;
    ENDIF;
    SFLR# += 1;

    WRITE SFLFMT;
  ENDDO;
  //  CTLSFLUPD = *OFF;
  SFLSIZ = SFLR#;
  IF SFLSIZ = SFLMAX AND %SUBST(SQLSTATE:1:2) <> '02';
    SFLSIZCOL = 'orange';
    SFLSIZTT = 'Search is limited to ' + %CHAR(SFLMAX) + ' results';
  ELSE;
    SFLSIZCOL = 'white';
    SFLSIZTT = '';
  ENDIF;
  // Position cursor
  IF SFLSIZ > 0;
    SFLR# = 1;
  ELSE;
    SFLR# = 0;
  ENDIF;
  EXEC SQL
    CLOSE sflcsr;
END-PROC;
// ------------------------------------------------------------------- //

// ------------------------------------------------------------------- //
DCL-PROC LoadDetail;
  DCL-PI *N;
    @EOJ       CHAR(1);
  END-PI;
  DCL-S @RET       ZONED(5);
  DCL-S @SIZ       ZONED(5);
  DOW 1 = 1;
    CHAIN SFLR# SFLFMT;
    @RET = SFLR#;
    @SIZ = SFLSIZ;
    PODTLUI(@EOJ:@RET:@SIZ:OHYY:OH#);
    IF @RET = 0 OR @EOJ = 'Y';
      LEAVE;
    ENDIF;
    SFLR# = @RET; // Maintain current focused record
  ENDDO;
END-PROC;
// ------------------------------------------------------------------- //

// ------------------------------------------------------------------- //
DCL-PROC GoToDetail;
  DCL-PI *N;
    @EOJ       CHAR(1);
  END-PI;
  DCL-S OHCT       INT(10);
  DCL-S @RET       ZONED(5);
  DCL-S @SIZ       ZONED(5);
  DCL-S DTL#Z      ZONED(6);
  DOW 1 = 1;
    EXFMT DTLFMT;
    SELECT;
    WHEN DTLSUBMIT = *ON;
      IF DTLYY = *BLANKS OR DTL# = 0;
        DTL#ERM = 'Invalid PO Number!';
        DTL#ERR = *ON;
        ITER;
      ENDIF;
      EXEC SQL
        SELECT COUNT(*)
          INTO :ohct
          FROM orhdr
          WHERE ohyy = :dtlyy
                AND oh# = :dtl#;
      IF OHCT = 0;
        DTL#ERM = 'PO Not Found!';
        DTL#ERR = *ON;
        ITER;
      ENDIF;

      @RET = 0;
      @SIZ = 0;
      DTL#Z = DTL#;
      PODTLUI(@EOJ:@RET:@SIZ:DTLYY:DTL#Z);
      DTL# = DTL#Z;
      IF @EOJ = 'Y';
        LEAVE;
      ENDIF;
    WHEN DTLBACK = *ON;
      LEAVE;
    ENDSL;
  ENDDO;
END-PROC;
// ------------------------------------------------------------------- //

// ------------------------------------------------------------------- //
DCL-PROC DownloadResults;
  DCL-S SQLSELECT  VARCHAR(5000);
  DCL-S SQLTABLE   VARCHAR(5000);
  DCL-S SQLSTMT    VARCHAR(5000);
  DCL-S SQLORDR    CHAR(200);
  DCL-S NBR        CHAR(14);

  BuildSQLOrder(CTLORDR1:CTLORDR2:CTLORDR3:SQLORDR);

  // Similar SQL to LoadSubfile but with nice column headings
  // And different layout if desired
  // Added JOIN to PO Line to generate all rows as flat file

  SQLSELECT = '+
    SELECT ''PR'' || ohyy || oh# AS "PO Number", +
       CASE oh.ohstat +
         WHEN '' '' THEN ''Open'' +
         WHEN ''C'' THEN ''Closed'' +
         WHEN ''H'' THEN ''On Hold'' +
         WHEN ''X'' THEN ''Canceled'' +
         ELSE '''' +
       END AS "Status", +
       CASE oh.ohcode +
         WHEN ''D'' THEN ''Drop Ship'' +
         WHEN ''I'' THEN ''Inventory'' +
         WHEN ''E'' THEN ''Ecom'' +
         ELSE '''' +
       END AS "Type", +
       oh.ohvend || '' '' || vn.vnname AS "Vendor", +
       oh.ohreq AS "Created By", +
       oh.ohrdte AS "Date", +
       oh.ohdc AS "Warehouse", +
       oh.ohchdt AS "Changed By", +
       oh.ohchby AS "Last Change", +
       COALESCE(orsordr, '''') AS "ECOM Order #", +
       oi.oilin# AS "PO Line", +
       CASE +
         WHEN oistat = '' '' THEN ''Open'' +
         WHEN oistat = ''C'' THEN ''Closed'' +
         WHEN oistat = ''D'' THEN ''Deleted'' +
         WHEN oistat = ''X'' THEN ''Cancelled'' +
       END AS "Line Status", +
       oiqty AS "QTY", +
       oipart AS "Part Number", +
       oiddat AS "Due Date", +
       oirqty AS "Received QTY", +
       oibqty AS "Balance QTY" +
  FROM orhdr oh +
       JOIN vendmst vn +
         ON oh.ohvend = vn.vnumb +
       LEFT JOIN orecomm +
         ON ohordid = orordid +
       LEFT JOIN oritm oi +
         ON oh.ohyy = oi.oiyy +
           AND oh.ohpp = oi.oipp +
           AND oh.oh# = oi.oi#';

  // Create EMPTY table with select statement
  // To determine column widths, types, names
  // Can't use parameters in a create table stmt

  SQLTABLE = 'CREATE OR REPLACE TABLE QTEMP/SQLRESULT AS ('
    + SQLSELECT
    + ') WITH NO DATA ON REPLACE DELETE ROWS';

  EXEC SQL
    PREPARE sqltable FROM :sqltable;
  SQLDIAG(PGM:'N':'N':'N');

  EXEC SQL
    EXECUTE sqltable;
  SQLDIAG(PGM:'N':'N':'N');

  SQLSTMT = 'INSERT INTO QTEMP/SQLRESULT ('
    + %TRIM(SQLSELECT)
    + ' ' + %TRIM(SQLWHERE)
    + ' ' + %TRIM(SQLORDR)
    + ' LIMIT ' + %CHAR(SFLMAX)
    + ')';

  EXEC SQL
    PREPARE sqlstmt FROM :sqlstmt;
  SQLDIAG(PGM:'N':'N':'N');

  EXEC SQL
    EXECUTE sqlstmt USING :srnn,
                          :srwh,
                          :srcodei,
                          :srcoded,
                          :srcodee,
                          :srstat_,
                          :srstath,
                          :srstatx,
                          :srstatc,
                          :srmethy,
                          :srmethn,
                          :srvend,
                          :srsordr,
                          :srfrt,
                          :srreq,
                          :srrdtelo,
                          :srrdtehi,
                          :srchby,
                          :srchdtlo,
                          :srchdthi,
                          :srpartx,
                          :srpart,
                          :srpartx,
                          :srpart,
                          :srdescx,
                          :srdesc,
                          :srdescx,
                          :srdesc,
                          :srddatlo,
                          :srddathi,
                          :srireq,
                          :srwsjob;
  SQLDIAG(PGM:'N':'N':'N');

  NBR = %CHAR(%TIMESTAMP():*ISO0);

  SQL2XLSXD(
  PGM
  :CTLDLNAM
  :NBR
  :'Results'
  :1
  :'Y'
  :'LNK'
  :PGM
  );

END-PROC;
// ------------------------------------------------------------------- //

