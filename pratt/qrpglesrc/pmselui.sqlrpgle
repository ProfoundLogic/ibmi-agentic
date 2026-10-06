**FREE
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(MAIN);

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

//DCL-PR PMDTLUI EXTPGM;
//  @EOJ       CHAR(1);
//  @RET       ZONED(5); // Current record #
//  @SIZ       ZONED(5); // Total # of records
//  @OHYY      CHAR(2);
//  @OH#       ZONED(6); // 0 = New PO Mode
//END-PR;

DCL-PR OMMGRCL EXTPGM('OMMGRCLO');
  @UPDAT     CHAR(1);
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

DCL-PR WPMGRCL EXTPGM('WPMGRCLO');
  @WP        CHAR(2);
  @UPDAT     CHAR(1);
  @CHORA     CHAR(1);
  @SORP      CHAR(1);
  @LVL       CHAR(1);
END-PR;

// File Declarations
// ----------------------------------------------------------------------
DCL-F PMSELUI WORKSTN HANDLER('PROFOUNDUI(HANDLER)')
  SFILE(SFLFMT:SFLR#);

DCL-S PGM        CHAR(10) INZ('PMSELUI');
DCL-S USER       CHAR(10) INZ(*USER);
// Maximum number of records to show on the screen
DCL-S SFLMAX     PACKED(5) INZ(1000);
DCL-S USRLVL     CHAR(10); // NOV, INT, EXP, PGMR...
DCL-S USRMODE    PACKED(1); // Number version of above
DCL-S SQLWHERE   VARCHAR(5000);
DCL-S FIRSTLOAD  IND INZ(*OFF);
DCL-S OMPO#      ZONED(6);
DCL-S @UPDAT     CHAR(1) INZ('I'); // For OMMGRCL

DCL-PROC MAIN;
  // *ENTRY Prototype
  DCL-PI *N EXTPGM('PMSELUI');
    @EOJ       CHAR(1);
    @LOAD      CHAR(1) CONST; // Y to load subfile at program start
    @NN        CHAR(2);
    @DC        CHAR(2);
  END-PI;
  DCL-S @RET       ZONED(5);
  DCL-S @SIZ       ZONED(5);

  // For WPMGRCL
  DCL-S @WP        CHAR(2) INZ;
  DCL-S @UPDAT     CHAR(1) INZ('X');
  DCL-S @CHORA     CHAR(1) INZ('C');
  DCL-S @SORP      CHAR(1) INZ('S');
  DCL-S @LVL       CHAR(1) INZ('1');

  // *INZSR - Initialization
  EXEC SQL
    SET OPTION NAMING = *SYS,
                          COMMIT = *NONE,
                          DATFMT = *ISO,
                          TIMFMT = *ISO;
  IF NOT %OPEN(PMSELUI);
    OPEN PMSELUI;
  ENDIF;

  // Set constants for search
  CTLNN = @NN;
  CTLDC = @DC;

  // Fallback Search values
  SRNN = @NN;
  SRDC = @DC;
  SRCODE_ = *ON;
  SRCODEE = *ON;
  SRCODEI = *ON;
  SRCODED = *ON;
  SRCODEF = *ON;
  SRSTAT_ = *ON;
  SRSTATA = *ON;
  SRSTATE = *ON;
  SRSTATC = *ON;
  SRSTATR = *ON;
  SRSTATD = *OFF;
  SRPART = *BLANKS;
  SRPARTX = *OFF;
  SRDESC = *BLANKS;
  SRDESCX = *OFF;
  SRPOIT = *BLANKS;
  SRPOITX = *OFF;
  SRVEND = 0;
  SRBY = *BLANKS;
  SRRQDTLO = %DATE(%CHAR(%SUBDT(%DATE():*YEARS)) + '-01-01');
  SRRQDTHI = *LOVAL;
  SRUSER = *BLANKS;
  SRCDATLO = *LOVAL;
  SRCDATHI = *LOVAL;

  CTLORDR1 = 'OMCDAT';
  CTLORDR2 = 'OMVN#';
  CTLORDR3 = 'OMSTAT';

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
  USRRTV(USER:'OMMGR':USRLVL);
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

  CTLNEWDIS = *ON;
  CTLNEWTT = 'Create a New Req';
  // get user name info for UISAP
  //  EXEC SQL
  //    SELECT uisap
  //      INTO :uisap
  //      FROM userids
  //      WHERE uiuser = :user;
  //  // No create if no perms or SAP ID
  //  IF USRMODE = 0 OR UISAP = *BLANKS;
  //    CTLNEWDIS = *ON;
  //    CTLNEWTT = 'Bad SAP ID or Authority Level';
  //  ENDIF;

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
        LoadSubfile();
      ENDIF;
      SFLCHG = *OFF;
    ELSE;
      IF CTLOLDVAL <> *BLANKS;
        CTLVAL = CTLOLDVAL;
        SRSAVEDIS = *ON;
        SRSAVETT = 'Create a custom search to save';
      ENDIF;
      IF CTLREFRESH = *ON;
        LoadSubfile();
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
      OMMGRCL(@UPDAT);
    //      LoadDetail(@EOJ);
    WHEN CTLWP = *ON;
      WPMGRCL(@WP:@UPDAT:@CHORA:@SORP:@LVL);
    WHEN CTLPODTL = *ON;
      @RET = 0;
      @SIZ = 0;
      PODTLUI(@EOJ:@RET:@SIZ:OMPOYY:OMPO#);
    WHEN CTLNEW = *ON;
    //      @RET = 0;
    //      @SIZ = 0;
    //      OHYY = *BLANKS;
    //      OH# = 0;
    //      PODTLUI(@EOJ:@RET:@SIZ:OHYY:OH#);
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
  CLOSE PMSELUI;
END-PROC;
// ------------------------------------------------------------------- //

// ------------------------------------------------------------------- //
DCL-PROC LoadSubfile;
  DCL-S SFLSTMT    VARCHAR(5000);
  DCL-S SFLORDR    CHAR(200);

  BuildSQLOrder(CTLORDR1:CTLORDR2:CTLORDR3:SFLORDR);

  SQLWHERE = '+
  WHERE OMPP = COALESCE(NULLIF(?, ''''), OMPP) +
    AND OMDC = COALESCE(NULLIF(?, ''''), OMDC) +
    AND ((? = 1 AND OMCODE = '' '') +
      OR (? = 1 AND OMCODE = ''E'') +
      OR (? = 1 AND OMCODE = ''I'') +
      OR (? = 1 AND OMCODE = ''D'') +
      OR (? = 1 AND OMCODE = ''F'')) +
    AND ((? = 1 AND OMSTAT = '' '') +
      OR (? = 1 AND OMSTAT = ''A'') +
      OR (? = 1 AND OMSTAT = ''E'') +
      OR (? = 1 AND OMSTAT = ''C'') +
      OR (? = 1 AND OMSTAT = ''R'') +
      OR (? = 1 AND OMSTAT = ''D'')) +
    AND ((? = 1 AND OMPART = ?) +
      OR (? = 0 AND LOCATE(TRIM(COALESCE(?,'''')), OMPART) > 0)) +
    AND ((? = 1 AND OMDESC = ?) +
      OR (? = 0 AND LOCATE(TRIM(COALESCE(?,'''')), OMDESC) > 0)) +
    AND ((? = 1 AND OMPOIT = ?) +
      OR (? = 0 AND LOCATE(TRIM(COALESCE(?,'''')), OMPOIT) > 0)) +
    AND OMVN# = COALESCE(NULLIF(?, 0), OMVN#) +
    AND LOCATE(TRIM(COALESCE(?, '''')), OMCUPO) > 0 +
    AND OMBY = COALESCE(NULLIF(?, ''''), OMBY) +
    AND OMRQDT BETWEEN COALESCE(NULLIF(?,''0001-01-01''), OMRQDT) +
                   AND COALESCE(NULLIF(?,''0001-01-01''), OMRQDT) +
    AND OMUSER = COALESCE(NULLIF(?, ''''), OMUSER) +
    AND OMCDAT BETWEEN COALESCE(NULLIF(?,''0001-01-01''), OMCDAT) +
                   AND COALESCE(NULLIF(?,''0001-01-01''), OMCDAT)';

  SFLSTMT = '+
    SELECT omyy, +
       omreq, +
       omcode, +
       CASE omcode +
           WHEN '' '' THEN ''Sales'' +
           WHEN ''E'' THEN ''Ecom'' +
           WHEN ''I'' THEN ''Inventory'' +
           WHEN ''D'' THEN ''Drop Ship'' +
           WHEN ''F'' THEN ''Freight'' +
       END AS omcodedsc, +
       omstat, +
       CASE omstat +
           WHEN '' '' THEN ''Approved'' +
           WHEN ''A'' THEN ''Audit Req'' +
           WHEN ''E'' THEN ''EDI Audit'' +
           WHEN ''C'' THEN ''PO Created'' +
           WHEN ''R'' THEN ''Rate set'' +
           WHEN ''D'' THEN ''Deleted'' +
       END AS omstatdsc, +
       ompart, +
       omdesc, +
       omqty, +
       omuom, +
       ompp, +
       ompoyy, +
       ompo#, +
       CASE +
           WHEN ompo# > 0 THEN ''PR'' || ompoyy || CHAR(ompo#) +
           ELSE '''' +
       END AS ponum, +
       omvn#, +
       vnname, +
       (SELECT +
               CASE +
                   WHEN COUNT(*) > 0 THEN 1 +
                   ELSE 0 +
               END +
               FROM wipship +
               WHERE wsnn = SUBSTR(CHAR(omwo#), 1, 2) +
                     AND CHAR(wsjob) = SUBSTR(CHAR(omwo#), 3, 8) +
                     AND wswh = omdc +
                     AND wsymd = ompoqr +
                     AND wsloc = omwslc +
                     AND wstime = ommsd#) AS sflwpvis, +
       omwo#, +
       omdc, +
       ompoqr, +
       omwslc, +
       ommsd#, +
       omcupo, +
       omby, +
       omrqdt, +
       omuser, +
       omcdat +
    FROM ormsr om +
         JOIN vendmst vn +
             ON om.omvn# = vn.vnumb'
  + ' ' + %TRIM(SQLWHERE)
  + ' ' + %TRIM(SFLORDR)
  + ' LIMIT ' + %CHAR(SFLMAX);

  EXEC SQL
    PREPARE sflstmt FROM :sflstmt;
  //    SQLDIAG(PGM:'N':'N':'N');

  EXEC SQL
    DECLARE sflcsr CURSOR FOR sflstmt;
  //    SQLDIAG(PGM:'N':'N':'N');

  SFLCLR = *ON;
  WRITE CTLFMT;
  SFLCLR = *OFF;
  SFLR# = 0;
  SFLSIZ = 0;

  EXEC SQL
    OPEN sflcsr USING :srnn,
                      :srdc,
                      :srcode_,
                      :srcodee,
                      :srcodei,
                      :srcoded,
                      :srcodef,
                      :srstat_,
                      :srstata,
                      :srstate,
                      :srstatc,
                      :srstatr,
                      :srstatd,
                      :srpartx,
                      :srpart,
                      :srpartx,
                      :srpart,
                      :srdescx,
                      :srdesc,
                      :srdescx,
                      :srdesc,
                      :srpoitx,
                      :srpoit,
                      :srpoitx,
                      :srpoit,
                      :srvend,
                      :srcupo,
                      :srby,
                      :srrqdtlo,
                      :srrqdthi,
                      :sruser,
                      :srcdatlo,
                      :srcdathi;
  //    SQLDIAG(PGM:'N':'N':'N');

  DOW SFLR# < SFLMAX;
    RESET SFLFMT;
    EXEC SQL
      FETCH sflcsr
        INTO :omyy,
             :omreq,
             :omcode,
             :omcodedsc,
             :omstat,
             :omstatdsc,
             :ompart,
             :omdesc,
             :omqty,
             :omuom,
             :ompp,
             :ompoyy,
             :ompo#,
             :ponum,
             :omvn#,
             :vnname,
             :sflwpvis,
             :omwo#,
             :omdc,
             :ompoqr,
             :omwslc,
             :ommsd#,
             :omcupo,
             :omby,
             :omrqdt,
             :omuser,
             :omcdat;

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
    //    PODTLUI(@EOJ:@RET:@SIZ:OHYY:OH#);
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
  DCL-S OMCT       INT(10);
  DCL-S @RET       ZONED(5);
  DCL-S @SIZ       ZONED(5);
  DCL-S DTLYY      CHAR(2);
  DCL-S DTL#       ZONED(6);
  DTLGO = *OFF;
  DOW 1 = 1;
    EXFMT DTLFMT;
    SELECT;
    WHEN DTLSUBMIT = *ON AND DTLGO = *OFF;
      IF DTLNUM = *BLANKS OR %LEN(%TRIM(DTLNUM)) <> 8;
        DTLNUMERM = 'Invalid Req Number!';
        DTLNUMERR = *ON;
        ITER;
      ENDIF;
      DTLYY = %SUBST(DTLNUM:1:2);
      DTL# = %DEC(%SUBST(DTLNUM:3:6):6:0);
      EXEC SQL
        SELECT COUNT(*)
          INTO :omct
          FROM ormsr
          WHERE omyy = :dtlyy
                AND omreq = :dtl#;
      IF OMCT = 0;
        DTLNUMERM = 'Req Not Found!';
        DTLNUMERR = *ON;
        ITER;
      ENDIF;

      DTLGO = *ON;
      ITER;

    //      @RET = 0;
    //      @SIZ = 0;
    //      PMDTLUI(@EOJ:@RET:@SIZ:DTLYY:DTL#);
    //      IF @EOJ = 'Y';
    //        LEAVE;
    //      ENDIF;
    WHEN DTLGO = *ON;
      OMMGRCL(@UPDAT);
      DTLGO = *OFF;
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
    SELECT omyy || omreq AS "Req Number", +
       CASE omcode +
           WHEN '' '' THEN ''Sales'' +
           WHEN ''E'' THEN ''Ecom'' +
           WHEN ''I'' THEN ''Inventory'' +
           WHEN ''D'' THEN ''Drop Ship'' +
           WHEN ''F'' THEN ''Freight'' +
       END AS "Req Type", +
       CASE omstat +
           WHEN '' '' THEN ''Approved'' +
           WHEN ''A'' THEN ''Audit Req'' +
           WHEN ''E'' THEN ''EDI Audit'' +
           WHEN ''C'' THEN ''PO Created'' +
           WHEN ''R'' THEN ''Rate set'' +
           WHEN ''D'' THEN ''Deleted'' +
       END AS "Req Status", +
       ompart AS "Req Part", +
       omdesc AS "Req Part Desc", +
       omqty AS "QTY", +
       omuom AS "UOM", +
       CASE +
           WHEN ompo# > 0 THEN ''PR'' || ompoyy || CHAR(ompo#) +
           ELSE '''' +
       END AS "Assigned PO", +
       ompp AS "NN", +
       omdc AS "DC", +
       VARCHAR(omvn#) || '' '' || vnname AS "Vendor", +
       omcupo AS "Customer PO", +
       omwo# AS "WIP Job", +
       omby AS "Created By", +
       omrqdt AS "Create Date", +
       omuser AS "Changed By", +
       omcdat AS "Change Date" +
    FROM ormsr om +
         JOIN vendmst vn +
             ON om.omvn# = vn.vnumb;';

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
                          :srdc,
                          :srcode_,
                          :srcodee,
                          :srcodei,
                          :srcoded,
                          :srcodef,
                          :srstat_,
                          :srstata,
                          :srstate,
                          :srstatc,
                          :srstatr,
                          :srstatd,
                          :srpartx,
                          :srpart,
                          :srpartx,
                          :srpart,
                          :srdescx,
                          :srdesc,
                          :srdescx,
                          :srdesc,
                          :srvend,
                          :srcupo,
                          :srby,
                          :srrqdtlo,
                          :srrqdthi,
                          :sruser,
                          :srcdatlo,
                          :srcdathi;
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

