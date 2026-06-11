     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SWC02R - WC Claim Name Search/Return
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Returns a list of WC GROUP Master File starting at the incoming    .  *
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
      ** Workers' Comp Group Master File
     FWMFNL3    IF   E           K DISK
     F*****WMFNL4    IF   E           K DISK    Rename(PFN01:PFN02)
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



     D xxName          S                   Like(FNNAME)
     D xxShortNm       S                   Like(FNSHNM)
      ** Date check/conversion parameters
     D qqDateIn        S              8  0
     D qqFromFmt       S              8
     D qqToFmt         S              8
     D qqDateOut       S             10
     D qqError         S              1
      ** DARTS Key structure
     D                 DS
     D xxKey                         40a
     D xxCompany                      3a   Overlay(xxKey:1)
     D xxGroup                        3a   Overlay(xxKey:4)


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

     *========================================================================*
     * KLIST definitions
     *========================================================================*
     C     FNkey         KList
     C                   KFld                    xxName

     C     FNkey2        KList
     C                   KFld                    xxShortNm

     *========================================================================*
     * Main
     *========================================================================*


     C                   if        SavRtn  = 'B'
     C     FNkey         SetGT     PFN01
     C                   goto      RdHere
     C                   Endif
     C     FNkey         Chain     PFN01                              60
     C                   If        *in60 = *on
      ** No Hit, with Full name, DO SETLL, and fill up 5 FIelds
     C     FNkey         Setll     PFN01
     C     RdHere        Tag
     C                   Read      PFN01                                  61
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

     C                   Eval      xxName = @@NmKy
     C                   Eval      SavRtn    = @@RtnCode
     C                   Eval      @@rtncode = *blanks

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
     C                   Move      FNCO#         xxCompany
     C                   Move      FNFND         xxGroup

     C                   Eval      keys(count) = xxkey
     c                   Eval      nams(count) = FNNAME
      * * * change the effective date into MMDDYYYY
     C                   Eval      qqDateIn = FNEFDT
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MoveL     qqDateOut     dt1s(count)


     C*****************  Eval      Dt1s(count) = FNEFDT


     C                   READ      PFN01                                  61

     C     *in61         ifeq      *on
     C                   eval      @@rtncode = 'E'
     C                   Goto      donefill5
     C                   Endif
     C                   Eval      count = count + 1
     c                   enddo

     C     donefill5     tag

     C                   ENDSR                                                  *inzsr
