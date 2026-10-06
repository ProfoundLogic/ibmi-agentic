**FREE
// ---------------------------------------------------------------------------
// SQL2XLSXD - queue a spreadsheet download request.
//
// RECONSTRUCTED HELPER.  Pratt's version renders the prepared cursor into an
// .xlsx on the IFS and hands the browser a link.  That machinery was not part
// of the supplied bundle, so this version records the request in XLSLIST and
// returns; the search screens' Download button therefore completes cleanly and
// the queued request is visible in the table.
// ---------------------------------------------------------------------------
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(Main);

DCL-PROC Main;
  DCL-PI *N EXTPGM('SQL2XLSXD');
    @PGM      CHAR(10) CONST;
    @XLSFILE  CHAR(50) CONST;
    @NBR      CHAR(14) CONST;
    @XLSSHEET CHAR(31) CONST;
    @XLSSEQ   PACKED(3) CONST;
    @XLSFINAL CHAR(1) CONST;
    @EMAIL    CHAR(50) CONST;
    @LIST     CHAR(10) CONST;
  END-PI;

  DCL-S USER CHAR(10) INZ(*USER);
  DCL-S FILE CHAR(50);
  DCL-S NBR  CHAR(14);

  EXEC SQL SET OPTION NAMING = *SYS, COMMIT = *NONE, DATFMT = *ISO;

  FILE = @XLSFILE;
  NBR  = @NBR;

  EXEC SQL
    INSERT INTO XLSLIST (XLSUSER, XLSNBR, XLSFILE, XLSSTS)
      VALUES (:USER, :NBR, :FILE, 'Q');
END-PROC;
