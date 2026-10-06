**FREE
// ---------------------------------------------------------------------------
// USRRTV - return a user's authority level for an application.
//
// RECONSTRUCTED HELPER.  Contract from Pratt's prototype:
//     DCL-PR USRRTV EXTPGM;
//       @USER CHAR(10) CONST; @PGM CHAR(10) CONST; @LVL CHAR(10);
//     END-PR;
// The calling programs map the answer onto USRMODE:
//     NOV=1  INT=2  EXP=3  PGMR=4  anything else = 0 (no access).
// Levels are held in USRLEVEL.  A user with no row for the application falls
// back to their '*DEFAULT' row, and failing that to PGMR so that a freshly
// restored demo library is usable without seeding every profile.
// ---------------------------------------------------------------------------
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(Main);

DCL-PROC Main;
  DCL-PI *N EXTPGM('USRRTV');
    @USER      CHAR(10) CONST;
    @PGM       CHAR(10) CONST;
    @LVL       CHAR(10);
  END-PI;

  EXEC SQL SET OPTION NAMING = *SYS, COMMIT = *NONE, DATFMT = *ISO;

  @LVL = *BLANKS;

  EXEC SQL
    SELECT ULLVL INTO :@LVL
      FROM USRLEVEL
      WHERE ULUSER = :@USER
        AND ULPGM  = :@PGM
      FETCH FIRST 1 ROW ONLY;

  IF SQLCODE <> 0 OR @LVL = *BLANKS;
    EXEC SQL
      SELECT ULLVL INTO :@LVL
        FROM USRLEVEL
        WHERE ULUSER = :@USER
          AND ULPGM  = '*DEFAULT'
        FETCH FIRST 1 ROW ONLY;
  ENDIF;

  IF SQLCODE <> 0 OR @LVL = *BLANKS;
    @LVL = 'PGMR';
  ENDIF;
END-PROC;
