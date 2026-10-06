**FREE
// ---------------------------------------------------------------------------
// RTVPOTOT - total a purchase order and report it against the approval limit.
//
// RECONSTRUCTED HELPER.  Contract from Pratt's prototype:
//     DCL-PR RTVPOTOT EXTPGM;
//       @OHYY CHAR(2) CONST; @OH# ZONED(6) CONST;
//       @POTOT PACKED(22:2); @POMAX PACKED(22:2); @LIMIT IND;
//     END-PR;
// PODTLUI shows "PO $x > max $y" as an informational error when the total
// exceeds the limit, and blocks Approve.  Cancelled and deleted lines are
// excluded from the total.
// ---------------------------------------------------------------------------
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(Main);

DCL-PROC Main;
  DCL-PI *N EXTPGM('RTVPOTOT');
    @OHYY   CHAR(2) CONST;
    @OH#    ZONED(6) CONST;
    @POTOT  PACKED(22:2);
    @POMAX  PACKED(22:2);
    @LIMIT  IND;
  END-PI;

  DCL-S TOT    PACKED(22:2) INZ(0);
  DCL-C APPROVAL_LIMIT 25000;

  EXEC SQL SET OPTION NAMING = *SYS, COMMIT = *NONE, DATFMT = *ISO;

  EXEC SQL
    SELECT COALESCE(SUM(OIQTY * OIUNIT), 0) INTO :TOT
      FROM ORITM
      WHERE OIYY = :@OHYY
        AND OI#  = :@OH#
        AND OISTAT NOT IN ('X','D');

  IF SQLCODE <> 0;
    TOT = 0;
  ENDIF;

  @POTOT = TOT;
  @POMAX = APPROVAL_LIMIT;
  @LIMIT = (TOT > APPROVAL_LIMIT);
END-PROC;
