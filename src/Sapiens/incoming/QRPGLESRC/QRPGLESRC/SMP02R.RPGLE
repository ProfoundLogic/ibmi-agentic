     *========================================================================*
    H*     Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SMP02R - MC Provider Name Search/Return
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Returns a list of MC Providers, starting at the incoming name.        *
      *                                                                        *
     * CALLED BY : IDRTMR2 - DARTS E-mail Server name retrieval               *
     * CALLS  TO : None                                                       *
     *                                                                        *
     * CREATED   : 02/074/2000                                                 *
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
     *  FMPDL1  MC Provider Master File        PPD01   (NAME)                 *
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
     FFMPDL1    IF   E           K DISK

     *========================================================================*
     * Array, Table & Data Structure Definitions
     *========================================================================*
     D Keys            S             40a   DIM(5)
     D Names           S             40a   DIM(5)
     D DT1s            S              8a   DIM(5)
     D DT2s            S              8a   DIM(5)
     D SS#             S              9a   DIM(5)
     D Misc            S             40a   DIM(5)

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D @Name           S             40a
     D @Keys           S            200a
     D @Names          S            200a
     D @Dt1s           S             40a
     D @Dt2s           S             40a
     D @SS#            S             45a
     D @Misc           S            200a
     D @RtnCode        S              1a

     D i               S              2s 0
     D SaveRtn         S              1a

     D Name            S                   Like(PDNAME)

     D                 DS
     D Key                           40a
     D  FEIN                          9a   Overlay(Key:1)
     D  Mod                           3a   Overlay(Key:10)

     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    @Name
     C                   Parm                    @Keys
     C                   Parm                    @Names
     C                   Parm                    @DT1s
     C                   Parm                    @DT2s
     C                   Parm                    @SS#
     C                   Parm                    @Misc
     C                   Parm                    @RtnCode

     *========================================================================*
     * KLIST definitions
     *========================================================================*
     C     PDkey         KList
     C                   KFld                    Name

     *========================================================================*
     * Main
     *========================================================================*
     C                   If        SaveRtn  = 'B'
     C     PDkey         SetGT     PPD01
     C                   READ      PPD01                                  60
     C                   Else

     C     PDkey         Chain     PPD01                              60
     C                   If        *in60 = *on
      ** If no hit with full name, Do SETLL on the closest name.
     C     PDkey         SetLL     PPD01
     C                   READ      PPD01                                  60
     C                   EndIf

     C                   EndIf

     C                   EXSR      Fill5

      ** We have filled up 5 items.  Time to return...
     C                   MoveA     Keys          @Keys
     C                   MoveA     Names         @Names
     C                   MoveA     DT1s          @Dt1s
     C                   MoveA     Dt2s          @Dt2s
     C                   MoveA     SS#           @SS#
     C                   MoveA     Misc          @Misc

     C                   Eval      *inLR = *on

     *========================================================================*
     * *inzsr - Initialization subroutine
     *========================================================================*
     C     *inzsr        BEGSR

     C                   Eval      Name = @Name

     C                   Eval      SaveRtn  = @RtnCode
     C                   Eval      @RtnCode = *blanks

     C                   Eval      @Keys = *blanks
     C                   Eval      @Names = *blanks
     C                   Eval      @Dt1s = *blanks
     C                   Eval      @Dt2s = *blanks
     C                   Eval      @SS#  = *blanks
     C                   Eval      @Misc = *blanks

     C                   ENDSR                                                  *inzsr
     *========================================================================*
     * Fill5  - Fill upto 5 items to return to caller
     *========================================================================*
     C     Fill5         BEGSR

     C                   Do        5             i
      ** Create key.
     C                   Move      PDFEIN        FEIN
     C                   Move      PDMOD         Mod

      ** Load keys and names.
     C                   Eval      Keys(i) = Key
     C                   Eval      Names(i) = PDNAME

     C                   READ      PPD01                                  60

     C                   If        *in60 = *on
      ** End of file, so leave with end return code.
     C                   Eval      @RtnCode = 'E'
     C                   Leave
     C                   Endif

     C                   EndDo

     C                   ENDSR                                                  *inzsr
