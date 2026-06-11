     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SMS01R - MC Subscriber Name Validation
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Searches the Managed Care Subscriber Master File for a matching name. *
     *  If only one is found, it returns the DARTS key for that Subscriber,   *
     *  otherwise it returns an error code.                                   *
     *                                                                        *
     * CALLED BY : DARTMail Server Program                                    *
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
     *  FMSML2  MC Subscriber Master File      PSM01   (NAME)                 *
     *                                                                        *
     *========================================================================*
     * INDICATOR USAGE:                                                       *
     *                                                                        *
     *  60 - General Chain indicator.                                         *
     *                                                                        *
     *========================================================================*
     * NOTES:                                                                 *
     *                                                                        *
     *                                                                        *
     *========================================================================*
      ** Managed Care Subscriber Master File
     FFMSML2    IF   E           K DISK

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D @Name           S             40a
     D @Key            S             40a
     D @RtnCode        S              1a

     D SubName         S                   Like(SMNAME)

     *========================================================================*
     * Data Structure & Array Definitions
     *========================================================================*
     D                 DS                  Inz
     D Key                           40a
     D  Company                       3a   Overlay(Key:1)
     D  Subscriber                    5a   Overlay(Key:4)

     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    @Name
     C                   Parm                    @Key
     C                   Parm                    @RtnCode

     *========================================================================*
     * KLIST definitions
     *========================================================================*
     C     SMkey         KList
     C                   KFld                    SubName

     *========================================================================*
     * Main
     *========================================================================*
      ** Look for the name.
     C     SMkey         Chain     PSM01                              60
     C                   If        *in60 = *off
     C                   Move      SMCO#         Company
     C                   Move      SMSUB#        Subscriber

      ** We've found one, now let's look for another matching name.
     C     SMkey         ReadE     PSM01                                  60

     C                   If        *in60 = *off
      ** Too many names match, return error code.
     C                   Eval      @RtnCode = '1'
     C                   EndIf

     C                   Else
      ** Name not found, return error code.
     C                   Eval      @RtnCode = '2'
     C                   EndIf

     C                   If        @RtnCode = *blanks
      ** No errors, so name found.  Return the DARTS key.
     C                   Eval      @Key = Key
     C                   EndIf

     C                   Eval      *inLR = *on
     *========================================================================*
     * *inzsr - Initialization subroutine
     *========================================================================*
     C     *inzsr        BEGSR

     C                   Eval      SubName = @Name

     C                   ENDSR                                                  *inzsr
