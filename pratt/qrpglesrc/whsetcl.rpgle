**FREE
// ---------------------------------------------------------------------------
// WHSETCL - set the working warehouse
//
// RECONSTRUCTION STUB.  Pratt's bundle calls this program but did not include
// its source. The parameter list below is taken verbatim from their prototype
// so the call is type safe; it leaves the warehouse unchanged.
// ---------------------------------------------------------------------------
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(Main);

DCL-PROC Main;
  DCL-PI *N EXTPGM('WHSETCL');
    @WH        CHAR(2);
  END-PI;

  // no-op

END-PROC;
