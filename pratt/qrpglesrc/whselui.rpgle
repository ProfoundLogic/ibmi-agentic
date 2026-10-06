**FREE
// ---------------------------------------------------------------------------
// WHSELUI - Warehouse selection Rich Display screen
//
// RECONSTRUCTION STUB.  Pratt's bundle calls this program but did not include
// its source. The parameter list below is taken verbatim from their prototype
// so the call is type safe; it returns with no selection.
// ---------------------------------------------------------------------------
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(Main);

DCL-PROC Main;
  DCL-PI *N EXTPGM('WHSELUI');
    @EOJ       CHAR(1);
    @LOAD      CHAR(1) CONST;
    @WH        CHAR(2);
  END-PI;

  @EOJ = 'N';

END-PROC;
