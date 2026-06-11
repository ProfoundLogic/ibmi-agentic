     *========================================================================*
     * (c) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SMC02R - MC Medical Cases Key Search/Return
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Returns a list of MC Claimants, starting at the incoming key.         *
      *                                                                        *
     * CALLED BY : IDRTMR2 - DARTS E-mail Server name retrieval               *
     * CALLS  TO : None                                                       *
     *                                                                        *
     * CREATED   : 02/03/2000                                                 *
     * PROGRAMMER: C:113                                                      *
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
     * FILE USAGE:                                                            *
     *                                                                        *
      *  File    Description                    Rcdfmt  Key                    *
     *  FMCML1  MC Medical Cases Master File   PCM01   (CO#/SUB#/FND/FYR/CASE)*
     *                                                                        *
     *========================================================================*
     * INDICATOR USAGE:                                                       *
     *                                                                        *
     *  60 - General Chain indicator.                                         *
     *                                                                        *
     *======================================================================= *
     * NOTES:                                                                 *
     *                                                                        *
     *  If the RtnCode passed in is 'A' (1st time), it returns the first five *
     *  names, including the originator.  If RtnCode is 'B' (subsequent calls)*
     *  the incoming name is skipped, and the next 5 are returned.            *
     *                                                                        *
     *========================================================================*
      ** Managed Care Medical Cases Master File
     FFMCML1    IF   E           K DISK

     *========================================================================*
     * Array, Table & Data Structure Definitions
     *========================================================================*
     D Keys            S             40a   DIM(5)
     D Names           S             40a   DIM(5)
     D Dt1s            S              8a   DIM(5)
     D Dt2s            S              8a   DIM(5)
     D SS#             S              9a   DIM(5)
     D Misc            S             40a   DIM(5)

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D @InKey          S             40a
     D @Keys           S            200a
     D @Names          S            200a
     D @Dt1s           S             40a
     D @Dt2s           S             40a
     D @SS#            S             45a
     D @Misc           S            200a
     D @RtnCode        S              1a

     D count           S              2s 0
     D SaveRtn         S              1a
     D FirstName       S                   Like(CMFSN)
     D LastName        S                   Like(CMLSN)
     D x               S              3  0
     D error           S              1a

      ** Date check/conversion parameters
     D qqDateIn        S              8  0
     D qqFromFmt       S              8
     D qqToFmt         S              8
     D qqDateOut       S             10
     D qqError         S              1

     D                 DS
     D Key                           40a
     D  Company                       3s 0 Overlay(Key:1)
     D  Subscriber                    5s 0 Overlay(Key:4)
     D  Group                         3s 0 Overlay(Key:9)
     D  PolicyYear                    3s 0 Overlay(Key:12)
     D  Case                          7s 0 Overlay(Key:15)

     D                 DS
     D tsKey                         40a
     D  tsCompany                     3a   Overlay(tsKey:1)
     D  tsSubs                        5a   Overlay(tsKey:4)
     D  tsGroup                       3a   Overlay(tsKey:9)
     D  tsPolYear                     3a   Overlay(tsKey:12)
     D  tsCase                        7a   Overlay(tsKey:15)

     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    @InKey
     C                   Parm                    @Keys
     C                   Parm                    @Names
     C                   Parm                    @Dt1s
     C                   Parm                    @Dt2s
     C                   Parm                    @SS#
     C                   Parm                    @Misc
     C                   Parm                    @RtnCode

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
     C     CMkey         KList
     C                   KFld                    Company
     C                   KFLd                    Subscriber
     C                   KFld                    Group
     C                   KFld                    PolicyYear
     C                   KFld                    Case

     *========================================================================*
     * Main
     *========================================================================*
     C                   If        SaveRtn  = 'B'
      ** First time through.
     C     CMkey         SetGT     PCM01
     C                   READ      PCM01                                  60
     C                   Else

      ** Subsequent passes.
     C     CMkey         Chain     PCM01                              60
     C                   If        *in60 = *on
      ** No Hit, so SetLL and fill up 5 fields.
     C     CMkey         SetLL     PCM01
     C                   READ      PCM01                                  60
     C                   EndIf

     C                   EndIf

     C                   EXSR      Fill5
      ** We have filled up 5 items.  Time to return...

     C                   MoveA     Keys          @Keys
     C                   MoveA     Names         @Names
     C                   MoveA     Dt1s          @Dt1s
     C                   MoveA     Dt2s          @Dt2s
     C                   MoveA     SS#           @SS#
     C                   MoveA     Misc          @Misc

     C                   Eval      *inLR = *on

     *========================================================================*
     * *inzsr - Initialization subroutine
     *========================================================================*
     C     *inzsr        BEGSR

     C                   Eval      SaveRtn  = @RtnCode
     C                   Eval      @RtnCode = *blanks

      ** Test the incoming key value to verify it is numeric.
     C                   Eval      tsKey = @InKey

     C                   TestN                   tsCompany            66
     C                   If        *in66 = *off
     C                   Eval      Error = '1'
     C                   EndIf

     C                   If        Error = *blank
     C                   TestN                   tsSubs               66
     C                   If        *in66 = *off
     C                   Eval      Error = '2'
     C                   EndIf
     C                   EndIf

     C                   If        Error = *blank
     C                   TestN                   tsGroup              66
     C                   If        *in66 = *off
     C                   Eval      Error = '3'
     C                   EndIf
     C                   EndIf

     C                   If        Error = *blank
     C                   TestN                   tsPolYear            66
     C                   If        *in66 = *off
     C                   Eval      Error = '4'
     C                   EndIf
     C                   EndIf

     C                   If        Error = *blank
     C                   TestN                   tsCase               66
     C                   If        *in66 = *off
     C                   Eval      Error = '5'
     C                   EndIf
     C                   EndIf

     C                   If        Error = *blank
     C                   Eval      Key = tsKey
     C                   Else

     C                   Eval      @RtnCode  = Error

     C                   Select
     C                   When      error = '1'
     C                   Eval      tsKey = *zeros

     C                   When      error = '2'
     C                   Eval      tsSubs     = *zeros
     C                   Eval      tsGroup    = *zeros
     C                   Eval      tsPolYear  = *zeros
     C                   Eval      tsCase     = *zeros

     C                   When      error = '3'
     C                   Eval      tsGroup    = *zeros
     C                   Eval      tsPolYear  = *zeros
     C                   Eval      tsCase     = *zeros

     C                   When      error = '4'
     C                   Eval      tsPolYear  = *zeros
     C                   Eval      tsCase     = *zeros

     C                   When      error = '5'
     C                   Eval      tsCase     = *zeros
     C                   EndSL

     C                   MoveL     tsKey         Key

     C                   EndIf

     C                   Eval      @Keys  = *blanks
     C                   Eval      @Names = *blanks
     C                   Eval      @Dt1s  = *blanks
     C                   Eval      @Dt2s  = *blanks
     C                   Eval      @SS#   = *blanks
     C                   Eval      @Misc  = *blanks

     C                   ENDSR                                                  *inzsr

     *========================================================================*
     * Fill5  - Fill upto 5 items to return to caller
     *========================================================================*
     C     Fill5         BEGSR

     C                   Eval      count = 1

     C                   Do        5             count
      ** Create key.
     C                   Move      CMCO#         Company
     C                   Move      CMSUB#        Subscriber
     C                   Move      CMFND         Group
     C                   Move      CMFYR         PolicyYear
     C                   Move      CMCASE        Case

      ** Load keys and names.
     C                   Eval      Keys(count) = Key
     C                   Eval      Names(count) =
     C                               %TRIM(CMLSN) + ', ' + %TRIM(CMFSN)

      ** Load accident date & SS#.
     C                   Eval      SS#(count) = CMSS#

     C                   Eval      qqDateIn = CMACDT
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MoveL     qqDateOut     Dt1s(count)

     C                   READ      PCM01                                  60

     C                   If        *in60 = *on
      ** End of file, so leave with end return code.
     C                   Eval      @RtnCode = 'E'
     C                   Leave
     C                   Endif

     C                   EndDo

     C                   ENDSR                                                  *inzsr
