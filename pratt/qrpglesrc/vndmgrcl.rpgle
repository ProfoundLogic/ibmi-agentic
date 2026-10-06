**FREE
// ---------------------------------------------------------------------------
// VNDMGRCL - Vendor Manager green screen
//
// RECONSTRUCTION STUB.  Pratt's bundle calls this program but did not include
// its source. The parameter list below is taken verbatim from their prototype
// so the call is type safe; it returns immediately.
// ---------------------------------------------------------------------------
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(Main);

DCL-PROC Main;
  DCL-PI *N EXTPGM('VNDMGRCL');
    @UPDATE    CHAR(1) CONST;
  END-PI;

  // no-op

END-PROC;
