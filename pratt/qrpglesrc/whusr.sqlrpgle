**FREE
// ---------------------------------------------------------------------------
// WHUSR - return the default warehouse for the current user.
// RECONSTRUCTED HELPER.  Called from POAPPUICL / PMAPPUICL as CALL WHUSR (&WH).
// Picks the first selectable warehouse (COADEL 'S'/'W') belonging to the user's
// plant, so the search screens open with a sensible default.
// ---------------------------------------------------------------------------
CTL-OPT OPTION(*SRCSTMT:*NODEBUGIO);
CTL-OPT DFTACTGRP(*NO) ACTGRP(*CALLER);
CTL-OPT MAIN(Main);

DCL-PROC Main;
  DCL-PI *N EXTPGM('WHUSR');
    @WH CHAR(2);
  END-PI;
  DCL-S USER CHAR(10) INZ(*USER);
  DCL-S NN   CHAR(2);

  EXEC SQL SET OPTION NAMING = *SYS, COMMIT = *NONE, DATFMT = *ISO;

  NN = '94';
  EXEC SQL
    SELECT UINN INTO :NN FROM USERIDS WHERE UIUSER = :USER FETCH FIRST 1 ROW ONLY;
  IF SQLCODE <> 0 OR NN = *BLANKS;
    NN = '94';
  ENDIF;

  @WH = *BLANKS;
  EXEC SQL
    SELECT C.COAPLT INTO :@WH
      FROM COADDRES C
      JOIN LOCGEO  L ON L.L2WH = C.COAPLT
      WHERE C.COADEL IN ('S','W')
        AND L.L2DEL <> 'D'
        AND L.PRTM = :NN
      ORDER BY C.COAPLT
      FETCH FIRST 1 ROW ONLY;

  IF SQLCODE <> 0;
    @WH = *BLANKS;
  ENDIF;
END-PROC;
