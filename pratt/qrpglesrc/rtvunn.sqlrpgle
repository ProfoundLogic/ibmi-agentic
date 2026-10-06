**FREE
// ---------------------------------------------------------------------------
// RTVUNN - return the default plant (business area) for the current user.
// RECONSTRUCTED HELPER.  Called from POAPPUICL / PMAPPUICL as CALL RTVUNN (&NN)
// with a 2 character return variable.  Falls back to Pratt's Conyers GA plant.
// ---------------------------------------------------------------------------
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(Main);

DCL-PROC Main;
  DCL-PI *N EXTPGM('RTVUNN');
    @NN CHAR(2);
  END-PI;
  DCL-S USER CHAR(10) INZ(*USER);

  EXEC SQL SET OPTION NAMING = *SYS, COMMIT = *NONE, DATFMT = *ISO;

  @NN = *BLANKS;
  EXEC SQL
    SELECT UINN INTO :@NN FROM USERIDS WHERE UIUSER = :USER FETCH FIRST 1 ROW ONLY;

  IF SQLCODE <> 0 OR @NN = *BLANKS;
    @NN = '94';
  ENDIF;
END-PROC;
