**FREE
// ---------------------------------------------------------------------------
// SAPPOINT - send a PO cancellation to the SAP interface
//
// RECONSTRUCTION STUB.  Pratt's bundle calls this program but did not include
// its source. The parameter list below is taken verbatim from their prototype
// so the call is type safe; it does nothing: there is
// no SAP interface in this environment.
// ---------------------------------------------------------------------------
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(Main);

DCL-PROC Main;
  DCL-PI *N EXTPGM('SAPPOINT');
    @OHYY      CHAR(2);
    @OHPP      CHAR(2);
    @OH#       ZONED(6);
  END-PI;

  // no-op

END-PROC;
