     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SMP01R - MC Provider Name Search
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Searches the Managed Care Provider Master File for a matching name.   *
     *  If only one is found, it returns the DARTS key for that Provider,     *
     *  otherwise it returns an error code.                                   *
     *                                                                        *
     * CALLED BY : IDRTMR - DARTS E-mail Server Edit Program                  *
     *                                                                        *
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
     * Indicator Usage                                                        *
     *                                                                        *
     *  60 - General Chain indicator.                                         *
     *                                                                        *
     *======================================================================= *
      ** Managed Care Provider Demographics File
     FFMPDL1    IF   E           K DISK

     *========================================================================*
     * Array, Table & Data Structure Definitions
     *========================================================================*
      ** DARTS Key structure
     D                 DS
     D Key                           40a
     D  FEIN                          9a   Overlay(Key:1)
     D  Mod                           3a   Overlay(Key:10)

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D @Name           S             40a
     D @Key            S             40a
     D @RtnCode        S              1a
     D Name            S                   Like(PDNAME)

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
     C     PDkey         KList
     C                   KFld                    Name

     *========================================================================*
     * Main
     *========================================================================*
     C     PDkey         Chain     PPD01                              60
     C                   If        *in60 = *on
     C                   Eval      @RtnCode = '2'
     C                   Else

      ** Create key
     C                   Move      PDFEIN        FEIN
     C                   Move      PDMOD         Mod

      ** Check for multiple matching records.
     C     PDkey         ReadE     PPD01                                  60
     C                   If        *in60 = *off
     C                   Eval      @RtnCode = '1'
     C                   Else
     C                   Eval      @Key = Key
     C                   EndIf

     C                   EndIf

     C                   Eval      *inLR = *on
     *========================================================================*
     * *inzsr - Initialization subroutine
     *========================================================================*
     C     *inzsr        BEGSR

     C                   Eval      Name = @Name

     C                   ENDSR                                                  *inzsr
