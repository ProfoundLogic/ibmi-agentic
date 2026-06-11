     *========================================================================*
     * (c) Copyright Progressive Data Solutions, Inc., 2002                   *
     *========================================================================*
    H/TITLE SUG02R - Summit Name Search/Return
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Returns a list of Claims, starting at the incoming name.              *
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
      *  File    Description                    Rcdfmt      Key                *
      *  EMPLEL1 Claims Master by Name         EMPLYEPF     (EENAME)           *
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
      ** Summit Claim Master File by Name
     FEMPLYEL1  IF   E           K DISK

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
     D @name           S             40a
     D @keys           S            200a
     D @names          S            200a
     D @dates1         S             40a
     D @dates2         S             40a
     D @SS#            S             45a
     D @misc           S            200a
     D @rtnCode        S              1a

     D i               S              2s 0
     D saveRtn         S              1a
     D name            S                   Like(EENAME)

     D                 DS                  INZ
     D ucKey                         40a
     D  ucCo#                         3a   Overlay(ucKey:1)
     D  ucFnd                         3a   Overlay(ucKey:4)
     D  ucFyr                         3a   Overlay(ucKey:6)
     D  ucClaim                       7a   Overlay(ucKey:9)

     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    @name
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
     C     EEkey         KList
     C                   KFld                    name

     *========================================================================*
     * Main
     *========================================================================*
     C                   If        saveRtn  = 'B'
     C     EEkey         SetGT     EMPLYEL1
     C                   READ      EMPLYEL1                               60
     C                   Else

     C     EEkey         Chain     EMPLYEL1                           60
     C                   If        *in60 = *on
      ** If no hit with full name, Do SETLL on the closest name.
     C     EEkey         SetLL     EMPLYEL1
     C                   READ      EMPLYEL1                               60
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

     C                   Eval      name = @name

     C                   Eval      saveRtn  = @rtnCode
     C                   Eval      @rtnCode = *blanks

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

     C                   READ      EMPLYEL1                               60

     C                   If        *in60 = *on
      ** End of file, so leave with end return code.
     C                   Eval      @rtnCode = 'E'
     C                   Leave
     C                   Endif

     C                   EndDo

     C                   ENDSR                                                  *inzsr
