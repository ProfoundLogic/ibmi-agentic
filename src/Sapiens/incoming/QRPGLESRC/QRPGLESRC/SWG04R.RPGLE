     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SWG04R - WC Group Search
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Allows the user to select a Group. If one is selected, it returns     *
     *  the DARTS key for that Group, otherwise it returns an error code.     *
     *                                                                        *
     * CALLED BY : VDARTS                                                     *
     *                                                                        *
     * CALLS  TO : W0010R - Work With Groups                                  *
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

     D SearchPgm       PR                  ExtPgm('W0010R')
     D  kc                            1a
     D  kl                            1a
     D  mode                          1a
     D  company                       3a
     D  group                         3a
     D  policy_year                   3a
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
     D pProgram        S             10a   Inz('W0010R')
     D pCompany        S              3a   Inz(*blanks)
     D pGroup          S              3a   Inz(*blanks)
     D pPolYear        S              3a   Inz(*blanks)

      ** Key structure
     D                 DS
     D xKey                           6a
     D xCompany                       3a   Overlay(xKey:1)
     D xGroup                         3a   Overlay(xKey:4)

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
     C                                       pCompany : pGroup :
     C                                       pPolYear : pView )

      ** If F3 was pressed in the search, exit.
     C                   If        pKC = *on
     C                   Eval      eRtnCode = '3'
     C                   Else
    
     C                   If        pCompany <> *blanks
     C                             OR pCompany <> *zeros
      ** Create key
     C                   Move      pCompany      xCompany
     C                   Move      pGroup        xGroup
     C                   Clear                   eRtnCode
     C                   MoveL     xKey          eKey

     C                   EndIf

     C                   EndIf
    
     C                   Eval      *inLR = *on
