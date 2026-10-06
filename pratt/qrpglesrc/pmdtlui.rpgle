**FREE
// ---------------------------------------------------------------------------
// PMDTLUI - Material Requisition detail screen
//
// RECONSTRUCTION STUB.  Pratt's bundle calls this program but did not include
// its source. The parameter list below is taken verbatim from their prototype
// so the call is type safe; it returns with no page position.
// Pratt never built this program either - in their system requisition detail is
// reached by macroing into the OMMGR green screen from PMSELUI.
// ---------------------------------------------------------------------------
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(Main);

DCL-PROC Main;
  DCL-PI *N EXTPGM('PMDTLUI');
    @EOJ       CHAR(1);
    @RET       ZONED(5);
    @SIZ       ZONED(5);
    @OMYY      CHAR(2);
    @OMREQ     CHAR(6);
  END-PI;

  @EOJ = 'N';
  @RET = 0;

END-PROC;
