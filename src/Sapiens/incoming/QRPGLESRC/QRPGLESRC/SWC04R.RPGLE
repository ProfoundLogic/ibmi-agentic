     *========================================================================*
     * (C) Copyright Fiserv, Inc. 2000 - 2007
     *========================================================================*
    H/TITLE SWC04R - WC Claim Search
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Allows the user to select a Claim.  If one is selected, it returns    *
     *  the DARTS key for that Claim, otherwise it returns an error code.     *
     *                                                                        *
     * CALLED BY : VDARTS                                                     *
     *                                                                        *
     * CALLS  TO : None                                                       *
     *                                                                        *
     * CREATED   : 01/31/2000                                                 *
     * PROGRAMMER: C:113                                                      *
     * PROJECT   : DR095                                                      *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     *  05/25/07 C:191 Project WC1515                                         *
     *                 Check claims authority                                 *
     *                                                                        *
     *========================================================================*
     * NOTES:                                                                 *
     *                                                                        *
     *                                                                        *
     *========================================================================*
      ** Workers' Comp Claim Master File
     FWMCMP     IF   E             DISK

     *========================================================================*
     * Prototypes
     *========================================================================*
     D LIBLpgm         PR                  ExtPgm('SC916C1')
     D  program                      10a
     D  call                          1a

     D SearchPgm       PR                  ExtPgm('W0070R')
     D  call                          1a
     D  kc                            1a
     D  kl                            1a
     D  mode                          1a
     D  rrn                           9  0
     D  sort                          2a
     D  rrn1                          9  0
     D  year                          3  0

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
     D pSort           S              2a   Inz('CN')
     D pRRN1           S              9  0 Inz(*zeros)
     D pYear           S              3  0 Inz(*zeros)
     D pProgram        S             10a   Inz('W0070R')

     D*** BEG ADD ****** 05/25/07 ********************************************
      ** External data structure
     D psds          esds                  extname(szq1p)
     D*** END ADD ****** 05/25/07 ********************************************
     D*
      ** Key structure
     D                 DS
     D xKey                          16a
     D xCompany                       3a   Overlay(xKey:1)
     D xGroup                         3a   Overlay(xKey:4)
     D xPolYear                       3a   Overlay(xKey:7)
     D xCase                          7a   Overlay(xKey:10)

     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    eKey
     C                   Parm                    eRtnCode
     C*
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** parameter list for claims secuirty API
     C     cuspl         plist
     C                   parm                    ##co#             3
     C                   parm                    ##fnd             3
     C                   parm                    ##fyr             3
     C                   parm                    ##case            7
     C                   parm      q1user        ##usr            10
     C                   parm                    ##rtcd            1
     C*** END ADD ****** 05/25/07 ********************************************

     *========================================================================*
     * Main
     *========================================================================*
      ** Set the LIBL.
     C                   Eval      pCall = '1'
     C                   CALLP     LIBLpgm(pProgram : pCall)

     ** Call Work With Claims for a search.
     C                   Eval      pCall = '2'
     C                   CALLP     SearchPgm(pCall : pKC : pKL :
     C                                      pMode : pRRN : pSort :
     C                                      pRRN1 : pYear)

      ** If F3 was pressed in the search, exit.
     C                   If        pKC = *on
     C                   Eval      eRtnCode = '3'
     C                   Else
    
     C                   If        pRRN > *zero
     C     pRRN          Chain     WMCMP                              60
    
     C                   If        *in60 = *on
     C                   Eval      eRtnCode = 'E'
     C                   Else
     C                   Move      CMCO#         xCompany
     C                   Move      CMFND         xGroup
     C                   Move      CMFYR         xPolYear
     C                   Move      CMCASE        xCase
     C*** BEG ADD ****** 05/25/07 *************************************************
     C*** Check claim authority
     C                   move      cmco#         ##co#
     C                   move      cmfnd         ##fnd
     C                   move      cmfyr         ##fyr
     C                   move      cmcase        ##case
     C                   move      *blanks       ##rtcd
     C*
     C                   call      'WA067R'      cuspl
     C* authorised when rtcd <>'2'
     C                   if        ##rtcd <> '2'
     C*** END ADD ****** 05/25/07 *************************************************
     C                   Clear                   eRtnCode
     C                   MoveL     xKey          eKey
     C*** BEG ADD ****** 05/25/07 *************************************************
     C                   Else                                                   ##rtcd <> '2'
     C                   Eval      eRtnCode = 'E'
     C                   Endif                                                  ##rtcd <> '2'
     C*** END ADD ****** 05/25/07 *************************************************

     C                   EndIf

     C                   EndIf

     C                   EndIf
    
     C                   Eval      *inLR = *on
