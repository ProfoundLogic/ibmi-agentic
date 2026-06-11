    HCOPYRIGHT('(C) Copyright Fiserv, Inc., 2007')
    H*********************************************************************
    H/TITLE EDI Research w/ROES
    H*********************************************************************
    H*  CALLED FROM:                                                     *
    H*                                                                   *
    H*  CALLS TO:  W8079C                                                *
    H*                                                                   *
    H*  CREATED: 12/21/07      C:184     PROJECT: 135WKI                 *
    H*********************************************************************
    H*  REVISIONS:                                                       *
    H*                                                                   *
    H* 11/02/10  C:183 Project: SY195                                    *
    H*                 Remove hard coding for QDLS. Call RTVDOCROOT to   *
    H*                 get the root folder name.                         *
    H*                                                                   *
    H*********************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW8079F    CF   E             WORKSTN INDDS(INDDSP)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     D*
     D*** DATA STRUCTURE TO NAME THE INDICATORS
     D INDDSP          DS
     D ExitPgm                 3      3N
     D Rtncde          S              1    INZ
     D*
     D*
      *** Beg add ***** 11/02/10 ***********************************************
     D root            s             10    inz
      *** End add ***** 11/02/10 ***********************************************
     D**********************************************************************
     C*** Main Processing Starts
      *** Beg add ***** 11/02/10 ***********************************************
     C                   call      'RTVDOCROOT'
     C                   parm                    root
     c                   eval      SCFOLD = %trim(Root) + '/DOWNLOAD folder.'
      *** End add ***** 11/02/10 ***********************************************
     C*
     C                   EXFMT     WDW01
     C                   DOW       Not(ExitPgm)
     C*
     C                   CALL      'W8079C'
     C                   PARM                    Rtncde
     C*
     C                   IF        Rtncde = '1'
     C                   Leave
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   EVAL      *INLR = *ON
     C****************MAIN PROCESSING ENDS**********************************
