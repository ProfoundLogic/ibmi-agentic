**FREE
// ---------------------------------------------------------------------------
// SQLORDER - build an SQL ORDER BY clause from up to three column names.
//
// RECONSTRUCTED HELPER.  Pratt reference this program from POSELUI and PMSELUI
// as BuildSQLOrder but did not supply the source.  The contract is taken from
// their prototype:
//     DCL-PR BuildSQLOrder EXTPGM('SQLORDER');
//       ORDR1 CHAR(50); ORDR2 CHAR(50); ORDR3 CHAR(50); ORDRTXT CHAR(200);
//     END-PR;
// The three inputs hold column names chosen from the sort drop-downs (whose
// values come from the HELP table), optionally suffixed with ASC / DESC.
// Blank entries are skipped; if all three are blank the result is blank so the
// caller's statement simply has no ORDER BY.
// ---------------------------------------------------------------------------
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(Main);

DCL-PROC Main;
  DCL-PI *N EXTPGM('SQLORDER');
    ORDR1   CHAR(50);
    ORDR2   CHAR(50);
    ORDR3   CHAR(50);
    ORDRTXT CHAR(200);
  END-PI;

  DCL-S COLS     CHAR(50) DIM(3);
  DCL-S CLAUSE   VARCHAR(200) INZ('');
  DCL-S I        INT(5);

  COLS(1) = ORDR1;
  COLS(2) = ORDR2;
  COLS(3) = ORDR3;

  FOR I = 1 TO 3;
    IF COLS(I) = *BLANKS;
      ITER;
    ENDIF;
    IF CLAUSE <> '';
      CLAUSE += ', ';
    ENDIF;
    CLAUSE += %TRIM(COLS(I));
  ENDFOR;

  IF CLAUSE = '';
    ORDRTXT = *BLANKS;
  ELSE;
    ORDRTXT = 'ORDER BY ' + CLAUSE;
  ENDIF;
END-PROC;
