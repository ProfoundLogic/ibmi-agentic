**FREE
// ---------------------------------------------------------------------------
// CXOHPP - cross reference a PO to another business area
//
// RECONSTRUCTION STUB.  Pratt's bundle calls this program but did not include
// its source. The parameter list below is taken verbatim from their prototype
// so the call is type safe; it reports the PO's own
// business area back, i.e. no cross reference exists.
// ---------------------------------------------------------------------------
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(Main);

DCL-PROC Main;
  DCL-PI *N EXTPGM('CXOHPP');
    @OHYY      CHAR(2);
    @OHPP      CHAR(2);
    @OH#       PACKED(6);
    @TOPP      CHAR(2);
  END-PI;

  @TOPP = @OHPP;

END-PROC;
