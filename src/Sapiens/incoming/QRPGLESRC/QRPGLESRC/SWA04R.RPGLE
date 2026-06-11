     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SWA04R - WC Agent Search
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Allows the user to select an Agent. If one is selected, it returns    *
     *  the DARTS key for that Agent, otherwise it returns an error code.     *
     *                                                                        *
     * CALLED BY : VDARTS                                                     *
     *                                                                        *
     * CALLS  TO : W0040R - Work With Agents                                  *
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
     FWMAGP     IF   E             DISK

     *========================================================================*
     * Prototypes
     *========================================================================*
     D LIBLpgm         PR                  ExtPgm('SC916C1')
     D  program                      10a
     D  call                          1a

     D SearchPgm       PR                  ExtPgm('W0040R')
     D  kc                            1a
     D  kl                            1a
     D  mode                          1a
     D  rrn                           9  0
     D  view                          1  0

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
     D pView           S              1  0 Inz(*zero)
     D pProgram        S             10a   Inz('W0040R')

      ** Key structure
     D                 DS
     D xKey                          12a
     D xFEIN                          9a   Overlay(xKey:1)
     D xMod                           3a   Overlay(xKey:10)

     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    eKey
     C                   Parm                    eRtnCode

     *========================================================================*
     * Main
     *========================================================================*
      ** Set the LIBL.
     C                   Eval      pCall = '1'
     C                   CALLP     LIBLpgm(pProgram : pCall)

     ** Call Work With Claims for a search.
     C                   Eval      pMode = '2'
     C                   CALLP     SearchPgm(pKC : pKL : pMode :
     C                                       pRRN : pView )

      ** If F3 was pressed in the search, exit.
     C                   If        pKC = *on
     C                   Eval      eRtnCode = '3'
     C                   Else
    
     C                   If        pRRN > *zero
     C     pRRN          Chain     WMAGP                              60
    
     C                   If        *in60 = *on
     C                   Eval      eRtnCode = 'F'
     C                   Else
      ** Create key
     C                   Move      AGFEIN        xFEIN
     C                   Move      AGMOD         xMod
     C                   Clear                   eRtnCode
     C                   MoveL     xKey          eKey

     C                   EndIf

     C                   EndIf

     C                   EndIf
    
     C                   Eval      *inLR = *on
