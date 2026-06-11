     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE STP04R - P&C Policy Search
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Allows the user to select a Policy.    If one is selected, it returns *
     *  the DARTS key for that Policy, otherwise it returns an error code.  *
     *                                                                        *
     * CALLED BY : vDarts                                                     *
     *                                                                        *
     * CALLS  TO : C0533R - P&C Policy Search (one of 3)                      *
     *             SC916C1 - Set up LIBL for searches                         *
     *                                                                        *
     * CREATED   : 03/27/2007                                                 *
     * PROGRAMMER: C:002                                                      *
     * PROJECT   : 114COF                                                     *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     *                                                                        *
     *========================================================================*
     * NOTES:                                                                 *
     *                                                                        *
     *                                                                        *
     *========================================================================*

     *========================================================================*
     * Prototypes
     *========================================================================*
     D LIBLpgm         PR                  ExtPgm('SC916C1')
     D  program                      10a
     D  call                          1a

     D SearchPgm       PR                  ExtPgm('C0533R')
     D zprds                         40a
     D zcall                          1a
     D zpol#                         13a
     D zrtc                           1a


     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D eKey            S             40a
     D eRtnCode        S              1a

     D @prds           S             40a
     D @call           S              1a
     D @Pol#           S             13a
     D @rtc            S              1a


     D pCall           S              1a
     D pKC             S              1a   Inz(*off)
     D pKL             S              1a   Inz(*off)
     D pMode           S              1a   Inz(*blank)
     D pRRN            S              9  0 Inz(*zeros)
     D pRRN1           S              9  0 Inz(*zeros)
     D pQuote          S              1    Inz('W')
     D pView           S              1  0 Inz(*zero)
     D pRRN3           S              9  0 Inz(*zeros)
     D pProgram        S             10a   Inz('C0533R')

      ** Key structure
     D                 DS
     D xKey                          40a
     D  xCompany                      3a   Overlay(xKey:1)
     D  xPolicy                      13a   Overlay(xKey:4)

      ** e Key structure
     D                 DS
     D  eekey                        40a
     D  eCompany                      3a   Overlay(eeKey:1)
     D  ePolicy                      13a   Overlay(eeKey:4)
     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    eKey
     C                   Parm                    eRtnCode

     *========================================================================*
     * Main
     *========================================================================*

     C                   Eval      eekey = eKey

      ** Copy the program into QTEMP to avoid recursive call.
     C                   Eval      pCall = '1'
     C                   CALLP     LIBLpgm(pProgram : pCall)

     ** Call Policy Search.
     C                   Eval      pMode = '2'
     C                   CALLP     SearchPgm(@prds : @call : @pol# : @rtc)

     C     @rtc          ifeq      *blanks
     C     @rtc          oreq      '*'
     C                   Eval      xPolicy = @pol#
     C                   Else
     C                   Eval      eRtnCode = @rtc
     c                   Eval      xPolicy = *blanks
     C                   endif

     C******             Eval      pCall = '2'
     C*********          CALLP     LIBLpgm(pProgram : pCall)

     C                   Eval      xCompany = eCompany
     C                   Eval      eKey = xKey


     C                   Eval      pProgram = *blanks
     C********           Eval      pCall = '3'
     C************       CALLP     LIBLpgm(pProgram : pCall)
    
     C                   Eval      *inLR = *on
