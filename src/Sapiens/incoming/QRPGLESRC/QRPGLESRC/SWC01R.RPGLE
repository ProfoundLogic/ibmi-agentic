     *========================================================================*
     * (C) Copyright Fiserv, Inc. 2000 - 2007
     *========================================================================*
    H/TITLE SWE01R - WC Claim Name Search
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Searches the Workers' Comp Claim Master File for a matching name.     *
     *  If only one is found, it returns the DARTS key for that Claim,        *
     *  otherwise it returns an error code.                                   *
     *                                                                        *
     * CALLED BY : IDRTMR - DARTS E-mail Server Edit Program                  *
     *                                                                        *
     * CALLS  TO : None                                                       *
     *                                                                        *
     * CREATED   : 01/18/2000                                                 *
     * PROGRAMMER: C:113                                                      *
     * PROJECT   : DR086                                                      *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     *  05/25/07 C:191 Project WC1515                                         *
     *                 Check claims authority                                 *
     *                                                                        *
     * ---------------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.                    *
     * ---------------------------------------------------------------------- *
     *========================================================================*
     * FILE USAGE                                                             *
     *                                                                        *
      *  File    Description                    Rcdfmt  Key                    *
     *  WMCML12 WC Claim Master File            PCM01  (LSN, FSN)             *
     *                                                                        *
     *========================================================================*
     * Indicator Usage                                                        *
     *                                                                        *
     *  60 - General Chain indicator.                                         *
     *                                                                        *
     *======================================================================= *
     * NOTES:                                                                 *
     *                                                                        *
     *  ** IMPORTANT **                                                       *
     *  -This program must be compiled to a target release of V3R7M0 or       *
     *   greater due to the use of the BIF %Scan.                             *
     *                                                                        *
     *========================================================================*
      ** Workers' Comp Claim Master File
     FWMCML12   IF   E           K DISK

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D @@Name          S             40a
     D @@Key           S             40a
     D @@RtnCode       S              1a

     D xxFrstName      S                   Like(CMFSN)
     D xxLastName      S                   Like(CMLSN)
     D xxDelim         S              3  0
     D***
     D*** BEG ADD ****** 05/25/07 ********************************************
      ** External data structure
     D psds          esds                  extname(szq1p)
     D*** END ADD ****** 05/25/07 ********************************************
     D                 DS
     D xxKey                         16a
     D xxCompany                      3a   Overlay(xxKey:1)
     D xxGroup                        3a   Overlay(xxKey:4)
     D xxPolYear                      3a   Overlay(xxKey:7)
     D xxCase                         7a   Overlay(xxKey:10)
     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    @@Name
     C                   Parm                    @@Key
     C                   Parm                    @@RtnCode
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
     * KLIST definitions
     *========================================================================*
     C     CMkey         KList
     C                   KFld                    xxLastName
     C                   KFld                    xxFrstName

     *========================================================================*
     * Main
     *========================================================================*
     C                   If        @@RtnCode = *blank

     C     CMkey         Chain     PCM01                              60
     C                   If        *in60 = *off
     C                   Move      CMCO#         xxCompany
     C                   Move      CMFND         xxGroup
     C                   Move      CMFYR         xxPolYear
     C                   Move      CMCASE        xxCase
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
     C     CMkey         ReadE     PCM01                                  60

     C                   If        *in60 = *off
     C                   Eval      @@RtnCode = '1'
     C                   EndIf
     C*** BEG ADD ****** 05/25/07 *************************************************
     C                   Else                                                   ##rtcd <> '2'
     C                   Eval      @@RtnCode = '2'
     C                   endif                                                  ##rtcd <> '2'
     C*** END ADD ****** 05/25/07 *************************************************

     C                   Else
     C                   Eval      @@RtnCode = '2'
     C                   EndIf

     C                   If        @@RtnCode = *blanks
     C                   Eval      @@Key = xxKey
     C                   EndIf

     C                   EndIf

     C                   Eval      *inLR = *on
     *========================================================================*
     * *inzsr - Initialization subroutine
     *========================================================================*
     C     *inzsr        BEGSR

     C                   Eval      xxDelim = %scan(',':@@Name)

     C                   If        xxDelim = 0
     C                   Eval      @@RtnCode = '2'
     C                   Else
     C                   Eval      xxLastName= %trim(%subst(@@Name:1:xxDelim-1))
     C                   Eval      xxFrstName =  %trim(%subst(@@Name:xxDelim+1))
     C                   EndIf

     C                   ENDSR                                                  *inzsr
