     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2002                   *
     *========================================================================*
    H/TITLE SMC05R - MC Claim Info for Web Display
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Returns Managed Care Claim info to display on web page.            .  *
      *
     * CALLED BY : CGI programs for Web                                       *
     *                                                                        *
     * CALLS  TO : None                                                       *
     *                                                                        *
     * CREATED   : 08/12/2002                                                 *
     * PROGRAMMER: C:145                                                      *
     * PROJECT   : DR176                                                      *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     * XX/XX/XXXX C:XXX PROJECT: XXXXX                                        *
     *            XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
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
      ** Claim Master File
     FFMCML1    IF   E           K DISK
      ** Company Master File
     FSMCOL     IF   E           K DISK
      ** Group Master File
     FWMFNL     IF   E           K DISK
     *========================================================================*
     * Constant Definitions
     *========================================================================*
     *========================================================================*
     * Array, Table & Data Structure Definitions
     *========================================================================*
      *
     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D SavRtn          S              1a
     D err             S              1a
     D   Dateres       S               D   DATFMT(*USA)

     D                 DS
     D xxKey                         40a
     D xxCompany                      3s 0 Overlay(xxKey:1)
     D xxSubscriber                   5s 0 Overlay(xxKey:4)
     D xxGroup                        3s 0 Overlay(xxKey:9)
     D xxPolYear                      3s 0 Overlay(xxKey:12)
     D xxCase                         7s 0 Overlay(xxKey:15)
      *
     D                 DS
     D TSKey                         21a
     D TSCompany                      3a   Overlay(TSKey:1)
     D TSSubscriber                   5a   Overlay(TSKey:4)
     D TSGroup                        3a   Overlay(tSKey:9)
     D TSPolYear                      3a   Overlay(tSKey:12)
     D TSCase                         7a   Overlay(tSKey:15)
      *
     D xxSS#                         40a
     D xxS1                           3a   Overlay(xxSS#:1)
     D xxS2                           2a   Overlay(xxSS#:4)
     D xxS3                           4a   Overlay(xxSS#:6)
      *
     D                 DS
     D xxpol#                        40a
     D xpPolYear                      3a   Overlay(xxpol#:1)
     D xxphyp1                        1a   inz('-')overlay(xxpol#:4)
     D xpCase                         7a   Overlay(xxpol#:5)
      *
     DiDrtKey          s             40a
     DiPnam            s             10a
     DvHead1           s             40a
     DvHead2           s             40a
     DvHead3a          s             40a
     DvHead3b          s             40a
     DvDHd01           s             40a
     DvDl1c1           s             40a
     DvDl1c2           s             40a
     DvDl1c3           s             40a
     DvDl1c4           s             40a
     DvDl2c1           s             40a
     DvDl2c2           s             40a
     DvDl2c3           s             40a
     DvDl2c4           s             40a
     DvDl3c1           s             40a
     DvDl3c2           s             40a
     DvDl3c3           s             40a
     DvDl3c4           s             40a
     DvDl4c1           s             40a
     DvDl4c2           s             40a
     DvDl4c3           s             40a
     DvDl4c4           s             40a
     DvDl5c1           s             40a
     DvDl5c2           s             40a
     DvDl5c3           s             40a
     DvDl5c4           s             40a
     DvDl6c1           s             40a
     DvDl6c2           s             40a
     DvDl6c3           s             40a
     DvDl6c4           s             40a
     DvDl7c1           s             40a
     DvDl7c2           s             40a
     DvDl8c1           s             40a
     DvDl8c2           s             40a
     DvDl8c3           s             40a
     DvDl8c4           s             40a
     DvDl9c1           s             40a
     DvDl9c2           s             40a
     DvDl9c3           s             40a
     DvDl9c4           s             40a
     DvRtcd            s              2a
     D******
     Dxxdt8            s              8  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D******
     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C*
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtmdy         plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDY'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtmdy1        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C
     C     *entry        PList
     C                   parm                    idrtkey
     C                   parm                    ipnam
     C                   parm                    vHead1
     C                   parm                    vHead2
     C                   parm                    vHead3a
     C                   parm                    vHead3b
     C                   parm                    vDHd01
     C                   parm                    vDl1c1
     C                   parm                    vDl1c2
     C                   parm                    vDl1c3
     C                   parm                    vDl1c4
     C                   parm                    vDl2c1
     C                   parm                    vDl2c2
     C                   parm                    vDl2c3
     C                   parm                    vDl2c4
     C                   parm                    vDl3c1
     C                   parm                    vDl3c2
     C                   parm                    vDl3c3
     C                   parm                    vDl3c4
     C                   parm                    vDl4c1
     C                   parm                    vDl4c2
     C                   parm                    vDl4c3
     C                   parm                    vDl4c4
     C                   parm                    vDl5c1
     C                   parm                    vDl5c2
     C                   parm                    vDl5c3
     C                   parm                    vDl5c4
     C                   parm                    vDl6c1
     C                   parm                    vDl6c2
     C                   parm                    vDl6c3
     C                   parm                    vDl6c4
     C                   parm                    vDl7c1
     C                   parm                    vDl7c2
     C                   parm                    vDl8c1
     C                   parm                    vDl8c2
     C                   parm                    vDl8c3
     C                   parm                    vDl8c4
     C                   parm                    vDl9c1
     C                   parm                    vDl9c2
     C                   parm                    vDl9c3
     C                   parm                    vDl9c4
     C                   parm                    vRtcd
     C*
     *========================================================================*
     * KLIST definitions
     *========================================================================*
     C     CMkey         KList
     C                   KFld                    xxcompany
     C                   KFld                    xxSubscriber
     C                   KFld                    xxGroup
     C                   KFld                    xxPolYear
     C                   KFld                    xxCase
     C     COkey         KList
     C                   KFld                    xxcompany
     C     FNkey         KList
     C                   KFld                    xxcompany
     C                   KFld                    xxGroup
     *========================================================================*
     * Main
     *========================================================================*

     C                   TIME                    Dateres

     C     CMkey         Chain     PCM01                              60
     C                   if        not %eof
     C                   move      xxPolYear     xpPolYear
     C                   move      xxCase        xpCase
     C*
     C     JustGo        Tag
      ** HIT, Fill 5
     C                   exsr      Fill5
      ** We have filled up 5 items.  Time to return...


     C                   else
     C**** error Claim not found
     C                   endif


     C                   Eval      *inLR = *on
     *========================================================================*
     * *inzsr - Initialization subroutine
     *========================================================================*
     C     *inzsr        BEGSR

     C                   Eval      SavRtn    = vrtcd
     C                   Eval      vrtcd    = *blanks

      *** test incoming key value to verify it is numeric

     C                   Eval      tskey  = iDrtKey
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
     c
     c
     c                   Testn                   tsSubscriber         66  77
     c                   if        *in66 = *off
     c                   If        *in77 = *on
     c                   eval      tscompany = *zeros
     c                   eval      err = ' '
     c                   else
     c                   eval      err = '2'
     c                   goto      doneit
     c                   endif
     c                   eval      err = '2'
     c                   goto      doneit
     c                   else
     c                   endif
     c
     C*
     c                   Testn                   tsGroup              66  77
     c                   if        *in66 = *off
     c                   If        *in77 = *on
     c                   eval      tsGroup   = *zeros
     c                   eval      err = ' '
     c                   else
     c                   eval      err = '3'
     c                   goto      doneit
     c                   endif
     c                   eval      err = '3'
     c                   goto      doneit
     c                   else
     c                   endif
     C*

     c                   Testn                   tsPolYear            66  77
     c                   if        *in66 = *off
     c                   If        *in77 = *on
     c                   eval      tsPolYear = *zeros
     c                   eval      err = ' '
     c                   else
     c                   eval      err = '4'
     c                   goto      doneit
     c                   endif
     c                   eval      err = '4'
     c                   goto      doneit
     c                   else
     c                   endif


     c                   Testn                   tscase               66  77
     c                   if        *in66 = *off
     c                   If        *in77 = *on
     c                   eval      tscase = *zeros
     c                   eval      err = ' '
     c                   else
     c                   eval      err = '5'
     c                   goto      doneit
     c                   endif
     c                   eval      err = '5'
     c                   goto      doneit
     c                   else
     c                   endif

     c     doneit        tag
     c                   if        err <> ' '
     c                   eval      vRtcd = err
     c                   Select
     c                   when      err = '1'
     c                   eval      tskey = *zeros
     C                   movel     tskey         xxkey
     c                   when      err = '2'
     c                   eval      tsSubscriber = *zeros
     c                   eval      tsgroup   = *zeros
     c                   eval      tsPolYear = *zeros
     C                   eval      tscase    = *zeros
     c                   when      err = '3'
     c                   eval      tsgroup   = *zeros
     c                   eval      tsPolYear = *zeros
     c                   eval      tscase    = *zeros
     c                   when      err = '4'
     c                   eval      tsPolYear = *zeros
     c                   eval      tscase    = *zeros
     c                   when      err = '5'
     c                   eval      tscase    = *zeros
     c                   endsl

     c                   movel     tskey         xxkey
     c*
     c                   else
     c                   eval      xxkey = tskey
     c                   endif



     C                   ENDSR                                                  *inzsr
     *========================================================================*
     * Fill5  - Load data to return to calling CGI
     *========================================================================*
     C     Fill5         BEGSR

     C     COkey         chain     PCO01
     C                   if        not %eof
     C                   MOVEL     CONAME        vHead1
     C                   else
     C                   MOVEL     'Not Found'   vHead1
     C                   endif
     C*
     C     FNkey         chain     PFN01
     C                   if        not %eof
     C                   MOVEL     FNNAME        vHead2
     C                   else
     C                   MOVEL     'Not Found'   vHead2
     C                   endif
     C*
     **** Move SS# into xxSS#
     C                   MOVEL     CMSS#         xxSS#
     C*
     C**** vhead3a is Current Date
     C*
     C                   eval      vHead3a = ('Current Date: ' +
     C                             %CHAR(dateres))
     C*
     C                   eval      vDHd01 = 'CLAIMANT INFORMATION'
     C*
     C                   eval      vDl2c1 = ('Claimant Name:')
     C*
     C                   eval      vDl2c2 = (CMFSN + ' ' + CMLSN)
     C*
     C                   eval      vDl2c3 = ('Subscriber Number:')
     C*
     C                   MOVEL     CMSUB#        vDl2c4
     C*
     C                   eval      vDl4c1 = ('Social Security Number:')
     C*
     C                   eval      vDl4c2 = xxS1 + '-' + xxS2 + '-' + xxS3
     C*
     C                   eval      vDl4c3 = ('Policy Number:')
     C*
     C                   eval      vDl4c4 = xxpol#
     C*
     C                   eval      vDl6c1 = ('Date of Injury:')
     C*
     C                   eval      srdti  = CMACDT
     C*
     C                   call      'SRCVTDAT'    dtmdyy
     C*
     C                   MOVEL     srdto         xxdt8
     C
     C                   eval      vDl6c2 = (%editc(xxdt8: 'Y'))
     C*
     C     donefill5     tag

     C                   ENDSR                                                  *inzsr
     C*
