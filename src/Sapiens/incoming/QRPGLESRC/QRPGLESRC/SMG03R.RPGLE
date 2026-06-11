     *========================================================================*
     * (c) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SMG02R - MC Group Key Search/Return
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Returns a list of MC Groups, starting at the incoming key.            *
      *                                                                        *
     * CALLED BY : IDRTMR2 - DARTS E-mail Server name retrieval               *
     * CALLS  TO : None                                                       *
     *                                                                        *
     * CREATED   : 02/07/2000                                                 *
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
     *  FMFNL   MC Group Master File           PFN01   (CO#/SUB#/FND#)        *
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
      ** Managed Care Group Master File
     FFMFNL     IF   E           K DISK

     *========================================================================*
     * Array, Table & Data Structure Definitions
     *========================================================================*
     D keys            S             40a   DIM(5)
     D names           S             40a   DIM(5)
     D dates1          S              8a   DIM(5)
     D dates2          S              8a   DIM(5)
     D SS#             S              9a   DIM(5)
     D misc            S             40a   DIM(5)

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D @inKey          S             40a
     D @keys           S            200a
     D @names          S            200a
     D @dates1         S             40a
     D @dates2         S             40a
     D @SS#            S             45a
     D @misc           S            200a
     D @rtnCode        S              1a

     D i               S              2s 0
     D saveRtn         S              1a
     D error           S              1a

     D                 DS
     D Key                           40
     D  company                       3s 0 Overlay(Key:1)
     D  subscriber                    5s 0 Overlay(Key:4)
     D  group                         3s 0 Overlay(Key:9)

     D                 DS
     D tsKey                         40a
     D  tsCompany                     3a   Overlay(tsKey:1)
     D  tsSubs                        5a   Overlay(tsKey:4)
     D  tsGroup                       3a   Overlay(tsKey:9)

     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    @inKey
     C                   Parm                    @keys
     C                   Parm                    @names
     C                   Parm                    @dates1
     C                   Parm                    @dates2
     C                   Parm                    @SS#
     C                   Parm                    @misc
     C                   Parm                    @rtnCode

     *========================================================================*
     * KLIST definitions
     *========================================================================*
     C     FNkey         KList
     C                   KFld                    company
     C                   KFLd                    subscriber
     C                   KFLd                    group

     *========================================================================*
     * Main
     *========================================================================*
     C                   If        saveRtn  = 'B'
      ** First time through.
     C     FNkey         SetGT     PFN01
     C                   READ      PFN01                                  60
     C                   Else

      ** Subsequent passes.
     C     FNkey         Chain     PFN01                              60
     C                   If        *in60 = *on
      ** No Hit, so SetLL and fill up 5 fields.
     C     FNkey         SetLL     PFN01
     C                   READ      PFN01                                  60
     C                   EndIf

     C                   EndIf

     C                   EXSR      Fill5
      ** We have filled up 5 items.  Time to return...

     C                   MoveA     keys          @keys
     C                   MoveA     names         @names
     C                   MoveA     dates1        @dates1
     C                   MoveA     dates2        @dates2
     C                   MoveA     SS#           @SS#
     C                   MoveA     misc          @misc

     C                   Eval      *inLR = *on

     *========================================================================*
     * *inzsr - Initialization subroutine
     *========================================================================*
     C     *inzsr        BEGSR

     C                   Eval      saveRtn  = @rtnCode
     C                   Eval      @rtnCode = *blanks

      ** Test the incoming key value to verify it is numeric.
     C                   Eval      tsKey = @inKey

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
     C                   Eval      Key = tsKey
     C                   Else

     C                   Eval      @RtnCode  = Error

     C                   Select
     C                   When      error = '1'
     C                   Eval      tsKey = *zeros

     C                   When      error = '2'
     C                   Eval      tsSubs = *zeros
     C                   Eval      tsGroup = *zeros

     C                   When      error = '3'
     C                   Eval      tsGroup = *zeros
     C                   EndSL

     C                   MoveL     tsKey         Key

     C                   EndIf

     C                   Eval      @keys   = *blanks
     C                   Eval      @names  = *blanks
     C                   Eval      @dates1 = *blanks
     C                   Eval      @dates2 = *blanks
     C                   Eval      @SS#    = *blanks
     C                   Eval      @misc   = *blanks

     C                   ENDSR                                                  *inzsr

     *========================================================================*
     * Fill5  - Fill upto 5 items to return to caller
     *========================================================================*
     C     Fill5         BEGSR

     C                   Do        5             i
      ** Create key.
     C                   Move      FNCO#         company
     C                   Move      FNSUB#        subscriber
     C                   Move      FNFND         group

      ** Load keys and names.
     C                   Eval      keys(i) = Key
     C                   Eval      names(i) = FNNAME

     C                   READ      PFN01                                  60

     C                   If        *in60 = *on
      ** End of file, so leave with end return code.
     C                   Eval      @rtnCode = 'E'
     C                   Leave
     C                   Endif

     C                   EndDo

     C                   ENDSR                                                  *inzsr
