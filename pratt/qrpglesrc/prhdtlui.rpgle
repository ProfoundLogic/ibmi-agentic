**FREE
// ---------------------------------------------------------------------------
// PRHDTLUI - PO receipt history detail screen
//
// RECONSTRUCTION STUB.  Pratt's bundle calls this program but did not include
// its source. The parameter list below is taken verbatim from their prototype
// so the call is type safe; it returns immediately with no
// page position, which leaves the caller's screen unchanged.
// ---------------------------------------------------------------------------
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(Main);

DCL-PROC Main;
  DCL-PI *N EXTPGM('PRHDTLUI');
    @EOJ       CHAR(1);
    @RET       ZONED(5);
    @SIZ       ZONED(5) CONST;
    @ORYY      CHAR(2) CONST;
    @OR#       ZONED(6) CONST;
  END-PI;

  @EOJ = 'N';
  @RET = 0;

END-PROC;
