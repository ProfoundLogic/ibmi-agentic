     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SC916R2 - VDARTS SINGLE REMARK COPY/TRANSFER SEARCH               *
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  This program is the search program for the Visual DARTS Single Remark *
     *  Copy/Transfer option.                                                 *
     *                                                                        *
     * CALLED BY : VDARTS                                                     *
     *                                                                        *
     * CALLS  TO : Various Search programs                                    *
     *                                                                        *
     * CREATED   : 01/31/2000                                                 *
     * PROGRAMMER: C:113                                                      *
     * PROJECT   : DR095                                                      *
     *========================================================================*
     * REVISIONS:                                                             *
      *             03/27/07  C:002 P:114COF
      *                    Parm list to call Sxx04R programs is WRONG!!
      *                    FIX THIS by del the type from the call
      *
     *                                                                        *
     *                                                                        *
     * ---------------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.                    *
     * ---------------------------------------------------------------------- *
     *========================================================================*
     * NOTES:                                                                 *
     *                                                                        *
     *                                                                        *
     *========================================================================*
     *========================================================================*
     * File Definition                                                        *
     *========================================================================*
     FSMKTL     IF   E           K DISK
    
     *========================================================================*
     * Includes                                                               *
     *========================================================================*
      /COPY PDSINC

     *========================================================================*
     * Variable Definitions                                                   *
     *========================================================================*
     D eKey            S             40a
     D eKeyType        S              2a
     D eRtnCode        S              1a

     D pCall           S              1a
     D pKey            S             40a
     D pKeyType        S              2a
     D pProgram        S             10a
     D pRtnCode        S              1a

     D SearchPgm       S             10a
    
     *========================================================================*
     * KList Definitions                                                      *
     *========================================================================*
     ** DARTS Key Type file
     C     KTkey         KList
     C                   KFld                    eKeyType
    
     *========================================================================*
     * PList Definitions                                                      *
     *========================================================================*
     ** Entry parameters
     C     *entry        PList
     C                   Parm                    eKey
     C                   Parm                    eKeyType
     C                   Parm                    eRtnCode
    
     ** Search program parameters
     C     SearchPL      PList
     C                   Parm                    pKey
     *** Del next Line *** 03/27/07 ********************************************
     C*** Del 03/27/07   Parm                    pKeyType
     *** END Del ***** 03/27/07 ********************************************
     C                   Parm                    pRtnCode
    
     *========================================================================*
     * Main                                                                   *
     *========================================================================*
     C                   If        PgmStatus <> ERROR

      ** Call search program.
     C                   CALL      SearchPgm     SearchPL
    
     * END THE PROGRAM
     C                   CALL      'SC916C1'
     C                   Parm      *blanks       pProgram
     C                   Parm      '3'           pCall
    
     C                   Eval      eKey = pKey
     C                   Eval      eRtnCode = pRtnCode

     C                   EndIf

     C                   Move      *on           *inlr
    
     *========================================================================*
     * *inzsr - Initialization Subroutine                                     *
     *========================================================================*
     C     *inzsr        BEGSR
    
     ** Get the key type record.
     C     KTkey         Chain     PKT01                              60
     C                   If        *in60 = *off
    
     C                   If        KTSRC4 <> *blanks
     C                   Eval      SearchPgm = KTSRC4
     C                   Else

      ** No search program defined.
     C                   Eval      eRtnCode = 'B'
     C                   Eval      PgmStatus = ERROR
     C                   EndIf

     C                   Else

      ** Key type not valid.
     C                   Eval      eRtnCode = 'A'
     C                   Eval      pgmStatus = ERROR
     C                   EndIf
    
     C                   ENDSR
