     *========================================================================*
     * (c) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SUG03R - Summit Claim Search/Return
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Returns a list of Claimss, starting at the incoming key.              *
      *                                                                        *
     * CALLED BY : IDRTMR2 - DARTS E-mail Server name retrieval               *
     * CALLS  TO : None                                                       *
     *                                                                        *
     * CREATED   : 02/05/2002                                                 *
     * PROGRAMMER: C:128                                                      *
     * PROJECT   : 10IBS                                                      *
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
      *  File     Description              Rcdfmt    Key                       *
     *  EMPLOY4L Claims Master File       EMPLYEPF  (INSDNO/CALNYR/CASENO)    *
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
      ** Summit Claims Master File
     FEMPLOY4L  IF   E           K DISK

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

     D kyFnd           S              4a
     D kyFyr           S              3s 0
     D kyClaim         S              6s 0

     D                 DS                  INZ
     D ucKey                         40a
     D  ucCo#                         3a   Overlay(ucKey:1)
     D  ucFnd                         3a   Overlay(ucKey:4)
     D  ucFyr                         3a   Overlay(ucKey:6)
     D  ucClaim                       7a   Overlay(ucKey:9)

     D                 DS                  INZ
     D tsKey                         40a
     D  tsCo#                         3a   Overlay(tsKey:1)
     D  tsFnd                         3a   Overlay(tsKey:4)
     D  tsFyr                         3a   Overlay(tsKey:6)
     D  tsClaim                       7a   Overlay(tsKey:9)

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
     C     EMkey         KList
     C                   KFld                    kyFnd
     C                   KFLd                    kyFyr
     C                   KFLd                    kyClaim

     *========================================================================*
     * Main
     *========================================================================*
     C                   If        saveRtn  = 'B'
      ** First time through.
     C     EMkey         SetGT     EMPLOY4L
     C                   READ      EMPLOY4L                               60
     C                   Else

      ** Subsequent passes.
     C     EMkey         Chain     EMPLOY4L                           60
     C                   If        *in60 = *on
      ** No Hit, so SetLL and fill up 5 fields.
     C     EMkey         SetLL     EMPLOY4L
     C                   READ      EMPLOY4L                               60
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

     C                   TestN                   tsCo#                66
     C                   If        *in66 = *off
     C                   Eval      Error = '1'
     C                   EndIf

     C                   If        Error = *blank
     C                   TestN                   tsFnd                66
     C                   If        *in66 = *off
     C                   Eval      Error = '2'
     C                   EndIf
     C                   EndIf

     C                   If        Error = *blank
     C                   TestN                   tsFyr                66
     C                   If        *in66 = *off
     C                   Eval      Error = '3'
     C                   EndIf
     C                   EndIf

     C                   If        Error = *blank
     C                   TestN                   tsClaim              66
     C                   If        *in66 = *off
     C                   Eval      Error = '4'
     C                   EndIf
     C                   EndIf

     C                   If        Error = *blank
     C                   Eval      ucKey = tsKey
     C                   Else

     C                   Eval      @RtnCode  = Error

     C                   Select
     C                   When      error = '1'
     C                   Eval      tsKey = *zeros

     C                   When      error = '2'
     C                   Eval      tsFnd = *zeros
     C                   Eval      tsFyr = *zeros
     C                   Eval      tsClaim = *zeros

     C                   When      error = '3'
     C                   Eval      tsFyr = *zeros
     C                   Eval      tsClaim = *zeros

     C                   When      error = '4'
     C                   Eval      tsClaim = *zeros
     C                   EndSL

     C                   MoveL     tsKey         ucKey

     C                   EndIf

     C                   Move      *zeros        kyFnd
     C                   Move      ucFnd         kyFnd
     C                   Move      ucFyr         kyFyr
     C                   Move      ucClaim       kyClaim

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
     C                   Move      '001'         ucCo#
     C                   Move      INSDNO        ucFnd
     C                   Move      CALNYR        ucFyr
     C                   Move      CASENO        ucClaim

      ** Load keys and names.
     C                   Eval      keys(i) = ucKey
     C                   Eval      names(i) = EENAME

     C                   READ      EMPLOY4L                               60

     C                   If        *in60 = *on
      ** End of file, so leave with end return code.
     C                   Eval      @rtnCode = 'E'
     C                   Leave
     C                   Endif

     C                   EndDo

     C                   ENDSR                                                  *inzsr
