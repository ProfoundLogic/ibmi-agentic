     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SWE04R - WC Employer Search
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Allows the user to select an Employer. If one is selected, it returns *
     *  the DARTS key for that Employer, otherwise it returns an error code.  *
     *                                                                        *
     * CALLED BY : VDARTS                                                     *
     *                                                                        *
     * CALLS  TO : W0050R - Work With Employers                               *
     *             SC916C1 - Set up LIBL for searches                         *
     *                                                                        *
     * CREATED   : 01/31/2000                                                 *
     * PROGRAMMER: C:113                                                      *
     * PROJECT   : DR095                                                      *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     *                                                                        *
     *========================================================================*
     * NOTES:                                                                 *
     *                                                                        *
     *                                                                        *
     *========================================================================*
      ** Workers' Comp Claim Master File
     FWMEMP     IF   E             DISK

     *========================================================================*
     * Prototypes
     *========================================================================*
     D LIBLpgm         PR                  ExtPgm('SC916C1')
     D  program                      10a
     D  call                          1a

     D SearchPgm       PR                  ExtPgm('W0050R')
     D  kc                            1a
     D  kl                            1a
     D  mode                          1a
     D  rrn                           9  0
     D  rrn1                          9  0
     D  quote                         1a
     D  view                          1  0
     D  rrn3                          9  0

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D eKey            S             40a
     D eRtnCode        S              1a

     D pCall           S              1a
     D pKC             S              1a   Inz(*off)
     D pKL             S              1a   Inz(*off)
     D pMode           S              1a   Inz(*blank)
     D pRRN            S              9  0 Inz(*zeros)
     D pRRN1           S              9  0 Inz(*zeros)
     D pQuote          S              1    Inz('W')
     D pView           S              1  0 Inz(*zero)
     D pRRN3           S              9  0 Inz(*zeros)
     D pProgram        S             10a   Inz('W0050R')

      ** Key structure
     D                 DS
     D xKey                          40a
     D  xCompany                      3a   Overlay(xKey:1)
     D  xGroup                        3a   Overlay(xKey:4)
     D  xEmployer                     9a   Overlay(xKey:7)
     D  xDivision                     5a   Overlay(xKey:16)
     D  xQuote                        1a   Overlay(xKey:40)

     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    eKey
     C                   Parm                    eRtnCode

     *========================================================================*
     * Main
     *========================================================================*
      ** Copy the program into QTEMP to avoid recursive call.
     C                   Eval      pCall = '1'
     C                   CALLP     LIBLpgm(pProgram : pCall)

     ** Call Work With Claims for a search.
     C                   Eval      pMode = '2'
     C                   CALLP     SearchPgm(pKC : pKL : pMode : pRRN :
     C                                       pRRN1 : pQuote : pView : pRRN3)

     C                   Eval      pCall = '2'
     C                   CALLP     LIBLpgm(pProgram : pCall)

      ** If F3 was pressed in the search, exit.
     C                   If        pKC = *on
     C                   Eval      eRtnCode = '3'
     C                   Else
    
     C                   If        pRRN > *zero
     C     pRRN          Chain     WMEMP                              60
    
     C                   If        *in60 = *on
     C                   Eval      eRtnCode = 'D'
     C                   Else
      ** Create key
     C                   Move      EMCO#         xCompany
     C                   Move      EMFND         xGroup
     C                   Move      EMEMP#        xEmployer
     C                   Move      EMDIV         xDivision
     C                   Clear                   eRtnCode
     C                   MoveL     xKey          eKey

     C                   EndIf

     C                   EndIf

     C                   EndIf

     C                   Eval      pProgram = *blanks
     C                   Eval      pCall = '3'
     C                   CALLP     LIBLpgm(pProgram : pCall)
    
     C                   Eval      *inLR = *on
