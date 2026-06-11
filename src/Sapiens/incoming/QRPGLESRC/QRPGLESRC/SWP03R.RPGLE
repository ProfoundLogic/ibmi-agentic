     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SWP02R -
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Returns a list of             Master File starting at the incoming .  *
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
      **
      ** Workers' Comp Provider Demographics File
     FWMPDL     IF   E           K DISK

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

     D err             S              1a


     D*xxName           S                   Like(PDNAME)
     D                 DS
     D xxKey                         40a
     D xxFEIN                         9s 0 Overlay(xxKey:1)
     D xxMod                          3s 0 Overlay(xxKey:10)

     D                 DS
     D TSKey                         12a
     D TSFein                         9a   Overlay(tSKey:1)
     D TSMod                          3a   Overlay(TSKey:10)


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
     *========================================================================*
     * KLIST definitions
     *========================================================================*
     C*
     C     PDkey         KList
     C                   KFld                    xxFein
     C                   KFld                    xxMod

     *========================================================================*
     * Main
     *========================================================================*



     C                   if        SavRtn  = 'B'
     C     PDkey         SetGT     PPD01
     C                   goto      RdHere
     C                   Endif
     C     PDkey         Chain     PPD01                              60
     C                   If        *in60 = *on
      ** No Hit, with Full name, DO SETLL, and fill up 5 FIelds
     C     PDkey         Setll     PPD01
     C     RdHere        Tag
     C                   Read      PPD01                                  61
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


     C*************      Eval      xxName = @@Nmky
     C                   Eval      SavRtn    = @@RtnCode
     C                   Eval      @@rtncode = *blanks


      *** test incoming key value to verify it is numeric

     C                   Eval      tskey  = @@NmKy
     C*
     c                   Testn                   tsFein               66  77
     c                   if        *in66 = *off
     c                   If        *in77 = *on
     c                   eval      tsFein    = *zeros
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
     c                   Testn                   tsMod                66  77
     c                   if        *in66 = *off
     c                   If        *in77 = *on
     c                   eval      tsMod     = *zeros
     c                   eval      err = ' '
     c                   else
     c                   eval      err = '2'
     c                   goto      doneit
     c                   endif
     c                   eval      err = '2'
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
     c                   eval      tsMod      = *zeros
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
     C                   Move      PDFEIN        xxFEIN
     C                   Move      PDMOD         xxMod

     C                   Eval      keys(count) = xxkey
     c                   Eval      nams(count) = PDNAME



     C                   READ      PPD01                                  61

     C     *in61         ifeq      *on
     C                   eval      @@rtncode = 'E'
     C                   Goto      donefill5
     C                   Endif
     C                   Eval      count = count + 1
     c                   enddo

     C     donefill5     tag

     C                   ENDSR                                                  *inzsr
