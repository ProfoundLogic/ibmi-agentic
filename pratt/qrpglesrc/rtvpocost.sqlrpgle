**FREE
// ---------------------------------------------------------------------------
// RTVPOCOST - retrieve the unit cost for a PO line.
//
// RECONSTRUCTED HELPER.  Contract from Pratt's prototype:
//     DCL-PR RTVPOCOST EXTPGM;
//       @OHYY CHAR(2) CONST; @OH# ZONED(6) CONST; @OILIN# ZONED(3) CONST;
//       @OIPART CHAR(30) CONST; @OIUNIT PACKED(15:5);
//     END-PR;
// @OIUNIT is in/out: PODTLUI calls this while setting up a line for update.
// Priority is the cost already on the PO line, then the vendor part's first
// price break.  If neither is found the caller's value is left untouched so a
// manually keyed cost is never wiped out.
// ---------------------------------------------------------------------------
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(Main);

DCL-PROC Main;
  DCL-PI *N EXTPGM('RTVPOCOST');
    @OHYY   CHAR(2) CONST;
    @OH#    ZONED(6) CONST;
    @OILIN# ZONED(3) CONST;
    @OIPART CHAR(30) CONST;
    @OIUNIT PACKED(15:5);
  END-PI;

  DCL-S COST PACKED(15:5) INZ(0);
  DCL-S PART CHAR(25);

  EXEC SQL SET OPTION NAMING = *SYS, COMMIT = *NONE, DATFMT = *ISO;

  PART = @OIPART;

  EXEC SQL
    SELECT OIUNIT INTO :COST
      FROM ORITM
      WHERE OIYY = :@OHYY AND OI# = :@OH# AND OILIN# = :@OILIN#
      FETCH FIRST 1 ROW ONLY;

  IF SQLCODE <> 0 OR COST = 0;
    EXEC SQL
      SELECT VRCST1 INTO :COST
        FROM VENDPART
        WHERE VRPART = :PART
        FETCH FIRST 1 ROW ONLY;
  ENDIF;

  IF SQLCODE = 0 AND COST > 0;
    @OIUNIT = COST;
  ENDIF;
END-PROC;
