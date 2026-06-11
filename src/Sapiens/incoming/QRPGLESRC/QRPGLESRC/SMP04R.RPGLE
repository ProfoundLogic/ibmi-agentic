     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SMP04R - MC Provider Search
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Allows the user to select a Payee. If one is selected, it returns     *
     *  the DARTS key for that Payee, otherwise it returns an error code.     *
     *                                                                        *
     * CALLED BY : VDARTS                                                     *
     *                                                                        *
     * CALLS  TO : F0032R - Work With Providers                               *
     *             SC916C1 - Set up LIBL for searches                         *
     *                                                                        *
     * CREATED   : 02/07/2000                                                 *
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

     D SearchPgm       PR                  ExtPgm('F0032R')
     D  mode                          1a
     D  fein                          9a
     D  mod                           3a
     D  rtncode                       1a
     D  type                          1a
     D  oneNfour                      1a
     D  subset                        1a

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D eKey            S             40a
     D eRtnCode        S              1a

     D pCall           S              1a
     D pMode           S              1a   Inz(*blank)
     D pFEIN           S              9a   Inz(*blanks)
     D pMod            S              3a   Inz(*blanks)
     D pRtnCode        S              1a   Inz(*blank)
     D pType           S              1a   Inz(*blank)
     D p1N4            S              1a   Inz(*blank)
     D pSubset         S              1a   Inz(*blank)
     D pProgram        S             10a   Inz('F0032R')

      ** Key structure
     D                 DS
     D Key                           40a
     D  FEIN                          9a   Overlay(Key:1)
     D  Mod                           3a   Overlay(Key:10)

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
     C                   CALLP     SearchPgm(pMode:pFEIN:pMod:pRtnCode:
     C                                       pType:p1N4:pSubset)

      ** If F3 was pressed in the search, exit.
     C                   If        pRtnCode = 'A'
     C                   Eval      eRtnCode = '3'
     C                   Else
    
     C                   If        pFEIN <> *blanks
     C                             OR pFEIN <> *zeros
      ** Create key
     C                   Move      pFEIN         FEIN
     C                   Move      pMOD          Mod
     C                   Clear                   eRtnCode
     C                   MoveL     Key           eKey
     C                   EndIf

     C                   EndIf

     C                   Eval      *inLR = *on
