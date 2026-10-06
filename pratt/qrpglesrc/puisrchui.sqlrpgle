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

// File Declarations
// ----------------------------------------------------------------------
DCL-F PUISRCHUI WORKSTN HANDLER('PROFOUNDUI(HANDLER)');

DCL-S USER       CHAR(10) INZ(*USER);

DCL-PROC MAIN;
  DCL-PI *N EXTPGM('PUISRCHUI');
    @SRAPP     CHAR(10) CONST;
    @SRNAM     CHAR(30);
    @SRVAL     CHAR(2000);
    @SRDFT     CHAR(1) CONST;
  END-PI;

  EXEC SQL
    SET OPTION NAMING = *SYS,
                          COMMIT = *none,
                          DATFMT = *ISO;

  IF @SRNAM <> *BLANKS AND @SRVAL <> *BLANKS;
    AddSavedSearch(@SRAPP:@SRNAM:@SRVAL:@SRDFT);
  ELSEIF @SRVAL = *BLANKS AND @SRDFT = 'Y';
    RetrieveDefaultSearch(@SRAPP:@SRNAM:@SRVAL);
  ELSE;
    ShowSearchUI(@SRAPP:@SRNAM:@SRVAL);
  ENDIF;

END-PROC;

DCL-PROC AddSavedSearch;
  DCL-PI *N;
    @SRAPP     CHAR(10) CONST;
    @SRNAM     CHAR(30) CONST;
    @SRVAL     CHAR(2000) CONST;
    @SRDFT     CHAR(1) CONST;
  END-PI;
  DCL-S PSRCT      INT(10);

  EXEC SQL
    UPDATE puisrch
      SET psrval = :@srval
      WHERE psrapp = :@srapp
      AND psrusr = :user
      AND psrnam = :@srnam;
  //    SQLDIAG(@SRAPP:'N':'N':'N');
  IF SQLERRD(3) = 0; // No records found for update
    EXEC SQL
      INSERT INTO puisrch (
            psrapp,
            psrusr,
            psrnam,
            psrdft,
            psrval
          )
        VALUES
          (
            :@srapp,
            :user,
            :@srnam,
            :@srdft,
            :@srval
          );
  //    SQLDIAG(@SRAPP:'N':'N':'N');
  ENDIF;
END-PROC;

DCL-PROC RetrieveDefaultSearch;
  DCL-PI *N;
    @SRAPP     CHAR(10) CONST;
    @SRNAM     CHAR(30);
    @SRVAL     CHAR(2000);
  END-PI;

  EXEC SQL
    SELECT psrnam,
           psrval
      INTO :@srnam,
           :@srval
      FROM puisrch
      WHERE psrapp = :@srapp
            AND psrusr = :user
            AND psrdft = 'Y';

  IF %SUBST(SQLSTATE:1:2) = '02';
    // Retrieve global default
    EXEC SQL
      SELECT psrnam,
             psrval
        INTO :@srnam,
             :@srval
        FROM puisrch
        WHERE psrapp = :@srapp
              AND psrusr = ''
              AND psrdft = 'Y';
  ENDIF;

END-PROC;

DCL-PROC ShowSearchUI;
  DCL-PI *N;
    @SRAPP     CHAR(10) CONST;
    @SRNAM     CHAR(30);
    @SRVAL     CHAR(2000);
  END-PI;
  DCL-S OLDNAM     LIKE(CTLNAM);
  IF NOT %OPEN(PUISRCHUI);
    OPEN PUISRCHUI;
  ENDIF;
  CTLAPP = @SRAPP;
  CTLUSR = USER;
  DOU CTLBACK = *ON;
    // Clear last operation
    IF (CTLOPT <> *BLANKS);
      SFLCHG = *ON;
      CTLOPT = *BLANKS;
      CTLNAM = *BLANKS;
    ENDIF;
    EXFMT CTLFMT;
    SELECT;
    WHEN CTLBACK = *ON;
      RETURN;
    WHEN CTLOPT = 'DEL' AND CTLNAM <> *BLANKS;
      EXEC SQL
        DELETE FROM puisrch WHERE psrapp = :ctlapp
        AND psrusr = :ctlusr
        AND psrnam = :ctlnam;

      IF @SRNAM = CTLNAM;
        @SRVAL = *BLANKS;
      ENDIF;
    WHEN CTLOPT = 'RENAM' AND CTLNAM <> *BLANKS AND CTLNEWNAM <> *BLANKS;
      EXEC SQL
        UPDATE puisrch
          SET psrnam = :ctlnewnam
          WHERE psrapp = :ctlapp
          AND psrusr = :ctlusr
          AND psrnam = :ctlnam;

      IF SQLCODE = -803; // Dupkey
        CTLFMTERR = *ON;
        CTLFMTERM = 'Name already exists!';
      ENDIF;
    WHEN CTLOPT = 'DFT' AND CTLNAM <> *BLANKS;
      EXEC SQL
        SELECT psrnam
          INTO :oldnam
          FROM puisrch
          WHERE psrapp = :ctlapp
                AND psrusr = :ctlusr
                AND psrdft = 'Y';

      EXEC SQL
        UPDATE puisrch
          SET psrdft = 'N'
          WHERE psrapp = :ctlapp
          AND psrusr = :ctlusr;

      IF CTLNAM <> OLDNAM; // Allow unselecting default
        EXEC SQL
          UPDATE puisrch
            SET psrdft = 'Y'
            WHERE psrapp = :ctlapp
            AND psrusr = :ctlusr
            AND psrnam = :ctlnam;
      ENDIF;
    ENDSL;
  ENDDO;
ON-EXIT;
  CLOSE PUISRCHUI;
END-PROC;
