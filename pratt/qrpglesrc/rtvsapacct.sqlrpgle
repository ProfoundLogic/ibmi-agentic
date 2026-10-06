**FREE
// ---------------------------------------------------------------------------
// RTVSAPACCT - derive the SAP account assignment for a PO line.
//
// RECONSTRUCTED HELPER.  Contract from Pratt's prototype:
//     DCL-PR RTVSAPACCT EXTPGM;
//       @DC CHAR(2) CONST; @PART CHAR(30) CONST;
//       SAPPURG ZONED(3); SAPMTLG CHAR(7); SAPMTLN CHAR(9);
//       SAPCSTC CHAR(7); SAPGLACT ZONED(6);
//     END-PR;
// In the real system this reads the SAP interface tables.  Here it derives the
// values that exist in the reconstructed database: the material group/number
// come from the vendor part's major/minor codes and SKU, the cost centre comes
// from the warehouse's COADDRES row, and the purchasing group and G/L account
// follow the material group.
// ---------------------------------------------------------------------------
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(Main);

DCL-PROC Main;
  DCL-PI *N EXTPGM('RTVSAPACCT');
    @DC       CHAR(2) CONST;
    @PART     CHAR(30) CONST;
    SAPPURG   ZONED(3);
    SAPMTLG   CHAR(7);
    SAPMTLN   CHAR(9);
    SAPCSTC   CHAR(7);
    SAPGLACT  ZONED(6);
  END-PI;

  DCL-S PART   CHAR(25);
  DCL-S MAJ    CHAR(3);
  DCL-S MIN_   CHAR(3);
  DCL-S SKU    CHAR(15);
  DCL-S CSTC   CHAR(10);

  EXEC SQL SET OPTION NAMING = *SYS, COMMIT = *NONE, DATFMT = *ISO;

  PART = @PART;
  CLEAR MAJ;
  CLEAR MIN_;
  CLEAR SKU;
  CLEAR CSTC;

  EXEC SQL
    SELECT VRMAJ, VRMIN, VRIT INTO :MAJ, :MIN_, :SKU
      FROM VENDPART
      WHERE VRPART = :PART
      FETCH FIRST 1 ROW ONLY;

  EXEC SQL
    SELECT PAYOTQ INTO :CSTC
      FROM COADDRES
      WHERE COAPLT = :@DC
      FETCH FIRST 1 ROW ONLY;

  SAPMTLG  = MAJ + MIN_;
  SAPMTLN  = %SUBST(SKU : 1 : 9);
  SAPCSTC  = %SUBST(CSTC : 1 : 7);
  SAPPURG  = 0;
  SAPGLACT = 0;

  IF MAJ <> *BLANKS;
    MONITOR;
      SAPPURG = %DEC(%SUBST(MAJ : 1 : 3) : 3 : 0);
    ON-ERROR;
      SAPPURG = 100;
    ENDMON;
  ENDIF;
  IF SAPPURG = 0;
    SAPPURG = 100;
  ENDIF;

  IF CSTC <> *BLANKS;
    MONITOR;
      SAPGLACT = %DEC(%SUBST(CSTC : 1 : 6) : 6 : 0);
    ON-ERROR;
      SAPGLACT = 700100;
    ENDMON;
  ENDIF;
  IF SAPGLACT = 0;
    SAPGLACT = 700100;
  ENDIF;
END-PROC;
