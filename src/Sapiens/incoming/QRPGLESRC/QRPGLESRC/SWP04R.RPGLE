     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SWP04R - WC Provider Search
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Allows the user to select an Payee. If one is selected, it returns    *
     *  the DARTS key for that Payee, otherwise it returns an error code.     *
     *                                                                        *
     * CALLED BY : VDARTS                                                     *
     *                                                                        *
     * CALLS  TO : W0030R - Work With Agents                                  *
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

     *========================================================================*
     * Prototypes
     *========================================================================*
     D LIBLpgm         PR                  ExtPgm('SC916C1')
     D  program                      10a
     D  call                          1a

     D SearchPgm       PR                  ExtPgm('W0030R')
     D  mode                          1a
     D  fein                          9a
     D  mod                           3a
     D  rtncode                       1a

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D eKey            S             40a
     D eRtnCode        S              1a

     D pCall           S              1a
     D pMode           S              1a   Inz(*blank)
     D pFEIN           S              9a   Inz(*blanks)
     D pMod            S              3a   Inz(*blanks)
     D pRtnCode        S              1a
     D pProgram        S             10a   Inz('W0030R')

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
     C                   CALLP     SearchPgm(pMode : pFEIN : pMod : pRtnCode)

      ** If F3 was pressed in the search, exit.
     C                   If        pRtnCode = 'A'
     C                   Eval      eRtnCode = '3'
     C                   Else
    
     C                   If        pFEIN <> *blanks
     C                             OR pFEIN <> *zeros
      ** Create key
     C                   Move      pFEIN         xFEIN
     C                   Move      pMOD          xMod
     C                   Clear                   eRtnCode
     C                   MoveL     xKey          eKey
     C                   EndIf

     C                   EndIf

     C                   Eval      *inLR = *on
