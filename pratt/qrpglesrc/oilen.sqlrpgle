**FREE
// ---------------------------------------------------------------------------
// OILEN - return the number of active lines on a purchase order.
// RECONSTRUCTED HELPER.  Contract from Pratt's prototype; PODTLUI uses the
// answer to decide whether a PO still has any lines left on it.
// ---------------------------------------------------------------------------
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(Main);

DCL-PROC Main;
  DCL-PI *N EXTPGM('OILEN');
    @OHYY CHAR(2);
    @OHPP CHAR(2);
    @OH#  PACKED(6);
    @LEN  ZONED(2);
  END-PI;

  DCL-S CNT INT(10) INZ(0);

  EXEC SQL SET OPTION NAMING = *SYS, COMMIT = *NONE, DATFMT = *ISO;

  EXEC SQL
    SELECT COUNT(*) INTO :CNT
      FROM ORITM
      WHERE OIYY = :@OHYY AND OI# = :@OH#
        AND OISTAT NOT IN ('X','D');

  IF SQLCODE <> 0 OR CNT > 99;
    CNT = 0;
  ENDIF;
  @LEN = CNT;
END-PROC;
