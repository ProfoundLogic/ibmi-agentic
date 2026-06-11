     *========================================================================*
     * (C) Copyright Fiserv, Inc. 2000 - 2007
     *========================================================================*
    H/TITLE SWC03R - WC Claim Name Search/Return
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Returns a list of WC Claim Master File starting at the incoming    .  *
     *  name.  If rtncode pass in is "A" (1st time) it returns the first      *
     *  5 including the Originator.  If "B" (subsequent calls) then the inc   *
     *  incoming name is skipped, and the next 5 are returned.                *
      *
     * CALLED BY : Vdarts - Visual Darts                                      *
     *                                                                        *
     * CALLS  TO : None                                                       *
     *                                                                        *
     * CREATED   : 01/18/2000                                                 *
     * PROGRAMMER: C:02                                                       *
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
     * Indicator Usage                                                        *
     *                                                                        *
     *  60 - General Chain indicator.                                         *
     *  61 - General Chain indicator.                                         *
     *                                                                        *
     *======================================================================= *
      ** Workers' Comp claim    Master File
     FWMCML1    IF   E           K DISK
     *========================================================================*
     * Constant Definitions
     *========================================================================*
     *========================================================================*
     * Array, Table & Data Structure Definitions
     *========================================================================*
     DKeys             S             40a   DIM(5)
     DNams             S             40a   DIM(5)
     DDT1s             S              8a   DIM(5)
     DDT2s             S              8a   DIM(5)
     DSS#              S              9a   DIM(5)
     DMisc             S             40a   DIM(5)

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D @@NmKy          S             40a
     D @@Keys          S            200a
     D @@Nams          S            200a
     D @@DT1s          S             40a
     D @@DT2s          S             40a
     D @@SS#           S             45a
     D @@Misc          S            200a
     D @@RtnCode       S              1a

     D count           S              2s 0
     D SavRtn          S              1a
     D @@LnFn          S             40a



     D xxFrstName      S                   Like(CMFSN)
     D xxLastName      S                   Like(CMLSN)
     D xxDelim         S              3  0
     D err             S              1a

      ** Date check/conversion parameters
     D qqDateIn        S              8  0
     D qqFromFmt       S              8
     D qqToFmt         S              8
     D qqDateOut       S             10
     D qqError         S              1
     D*
     D*** BEG ADD ****** 05/25/07 ********************************************
      ** External data structure
     D psds          esds                  extname(szq1p)
     D*** END ADD ****** 05/25/07 ********************************************
     D*
     D                 DS
     D xxKey                         40a
     D xxCompany                      3s 0 Overlay(xxKey:1)
     D xxGroup                        3s 0 Overlay(xxKey:4)
     D xxPolYear                      3s 0 Overlay(xxKey:7)
     D xxCase                         7s 0 Overlay(xxKey:10)
     D                 DS
     D TSKey                         40a
     D TSCompany                      3a   Overlay(tsKey:1)
     D TSGroup                        3a   Overlay(tsKey:4)
     D TSPolYear                      3a   Overlay(tsKey:7)
     D TSCase                         7a   Overlay(tsKey:10)
     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    @@NmKy
     C                   Parm                    @@Keys
     C                   Parm                    @@Nams
     C                   Parm                    @@DT1s
     C                   Parm                    @@DT2s
     C                   Parm                    @@SS#
     C                   Parm                    @@Misc
     C                   Parm                    @@RtnCode
     ** Date check/conversion parameters
     C     dtCYMD        PList
     C                   Parm                    qqDateIn
     C                   Parm      '*CYMD'       qqFromFmt
     C                   Parm      '*MDYY'       qqToFmt
     C                   Parm                    qqDateOut
     C                   Parm                    qqError
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
     *========================================================================*
     C     CMkey         KList
     C                   KFld                    xxCompany
     C                   KFld                    xxgroup
     C                   KFld                    xxpolyear
     C                   KFld                    xxcase

     *========================================================================*
     * Main
     *========================================================================*


     C                   if        SavRtn  = 'B'
     C     CMkey         SetGT     PCM01
     C                   goto      RdHere
     C                   Endif
     C     CMkey         Chain     PCM01                              60
     C                   If        *in60 = *on
      ** No Hit, with Full name, DO SETLL, and fill up 5 FIelds
     C     CMkey         Setll     PCM01
     C     RdHere        Tag
     C                   Read      PCM01                                  61
     C                   exsr      Fill5
      ** we have filled up 5 items time to return....

     C                   Else
     C     JustGo        Tag
      ** HIT, Fill 5
     C                   exsr      Fill5
      ** We have filled up 5 items.  Time to return...

     C                   EndIf

     C                   Movea     keys          @@keys
     C                   Movea     Nams          @@Nams
     C                   Movea     DT1s          @@Dt1s
     C                   Movea     Dt2s          @@Dt2s
     C                   Movea     SS#           @@SS#
     C                   Movea     Misc          @@Misc




     C                   Eval      *inLR = *on
     *========================================================================*
     * *inzsr - Initialization subroutine
     *========================================================================*
     C     *inzsr        BEGSR


     C                   Eval      SavRtn    = @@RtnCode
     C************       Eval      xxDelim = %scan(',':@@NmKy)
     C*                  IF        XXDelim <> 0
     C*                  Eval      xxLastName= %trim(%subst(@@NmKy:1:xxDelim-1))
     C**************     Eval      xxFrstName =  %trim(%subst(@@Nmky:xxDelim+1))
     c*                  else
     c*                  eval      xxlastname = %trim(@@NmKy)
     c**************     endif


     C                   Eval      @@rtncode = *blanks
      *** test incoming key value to verify it is numeric

     C                   Eval      tskey  = @@NmKy
     C*
     c                   Testn                   tscompany            66  77
     c                   if        *in66 = *off
     c                   If        *in77 = *on
     c                   eval      tscompany = *zeros
     c                   eval      err = ' '
     c                   else
     c                   eval      err = '1'
     c                   goto      doneit
     c                   endif
     c                   eval      err = '1'
     c                   goto      doneit
     c                   else
     c                   endif

     C*
     c                   Testn                   tsGroup              66  77
     c                   if        *in66 = *off
     c                   If        *in77 = *on
     c                   eval      tsGroup   = *zeros
     c                   eval      err = ' '
     c                   else
     c                   eval      err = '2'
     c                   goto      doneit
     c                   endif
     c                   eval      err = '2'
     c                   goto      doneit
     c                   else
     c                   endif

     c                   Testn                   tsPolYear            66  77
     c                   if        *in66 = *off
     c                   If        *in77 = *on
     c                   eval      tsPolYear  = *zeros
     c                   eval      err = ' '
     c                   else
     c                   eval      err = '3'
     c                   goto      doneit
     c                   endif
     c                   eval      err = '3'
     c                   goto      doneit
     c                   else
     c                   endif


     c                   Testn                   tsCase               66  77
     c                   if        *in66 = *off
     c                   If        *in77 = *on
     c                   eval      tsCase     = *zeros
     c                   eval      err = ' '
     c                   else
     c                   eval      err = '4'
     c                   goto      doneit
     c                   endif
     c                   eval      err = '4'
     c                   goto      doneit
     c                   else
     c                   endif


     c     doneit        tag
     c                   if        err <> ' '
     c                   eval      @@rtnCode = err
     c                   Select
     c                   when      err = '1'
     c                   eval      tskey = *zeros
     C                   movel     tskey         xxkey
     c                   when      err = '2'
     c                   eval      tsgroup   = *zeros
     c                   eval      tsPolYear  = *zeros
     c                   eval      tscase     = *zeros
     c                   when      err = '3'
     c                   eval      tsPolYear  = *zeros
     c                   eval      tsCase     = *zeros
     c                   when      err = '4'
     c                   eval      tsCase     = *zeros
     c                   endsl

     c                   movel     tskey         xxkey
     c*
     c                   else
     c                   eval      xxkey = tskey
     c                   endif



     C                   Eval      @@keys = *blanks
     C                   Eval      @@Nams = *blanks
     C                   Eval      @@Dt1s = *blanks
     C                   Eval      @@Dt2s = *blanks
     C                   Eval      @@SS#  = *blanks
     C                   Eval      @@Misc  = *blanks

     C                   ENDSR                                                  *inzsr
     *========================================================================*
     * Fill5  - Fill upto 5 items to return to caller
     *========================================================================*
     C     Fill5         BEGSR

     C                   eval      count = 1

     C     Count         DOWLE     5
      ** Create key
     C                   Move      CMCO#         xxcompany
     C                   Move      CMFND         xxgroup
     C                   Move      CMfyr         xxpolYear
     C                   Move      CMcase        xxCase
     C*
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

     C                   Eval      keys(count) = xxkey
     C*** Name is the cat. of last comma space first

     C                   EVAL      @@lnFn =
     C                             %TRIM(cmlsn) + ', '+ %TRIM(cmfsn)

     c                   Eval      nams(count) = @@LnFn

      *** Accident date & soc#
     c                   Eval      ss#(count) = CMSS#


      * * * change the accident  date into MMDDYYYY
     C                   Eval      qqDateIn = CMACDT
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MoveL     qqDateOut     dt1s(count)

     C*** BEG ADD ****** 05/25/07 *************************************************
     C                   Else                                                   ##rtcd <> '2'
     C                   Eval      count = count - 1
     C                   endif                                                  ##rtcd <> '2'
     C*** END ADD ****** 05/25/07 *************************************************


     C                   READ      PCM01                                  61

     C     *in61         ifeq      *on
     C                   eval      @@rtncode = 'E'
     C                   Goto      donefill5
     C                   Endif
     C                   Eval      count = count + 1
     c                   enddo

     C     donefill5     tag

     C                   ENDSR                                                  *inzsr
