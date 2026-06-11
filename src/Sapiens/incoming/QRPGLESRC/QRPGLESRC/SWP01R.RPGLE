     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SWP01R - WC Provider Name Search
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Searches the Workers' Comp Provider Master File for a matching name.  *
     *  If only one is found, it returns the DARTS key for that Provider,     *
     *  otherwise it returns an error code.                                   *
     *                                                                        *
     * CALLED BY : IDRTMR - DARTS E-mail Server Edit Program                  *
     *                                                                        *
     * CALLS  TO : None                                                       *
     *                                                                        *
     * CREATED   : 01/18/2000                                                 *
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
      ** Workers' Comp Provider Demographics File
     FWMPDL1    IF   E           K DISK

     *========================================================================*
     * Array, Table & Data Structure Definitions
     *========================================================================*
      ** DARTS Key structure
     D                 DS
     D xxKey                         12a
     D xxFEIN                         9a   Overlay(xxKey:1)
     D xxMod                          3a   Overlay(xxKey:10)

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D @@Name          S             40a
     D @@Key           S             40a
     D @@RtnCode       S              1a
     D xxName          S                   Like(PDNAME)

     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    @@Name
     C                   Parm                    @@Key
     C                   Parm                    @@RtnCode

     *========================================================================*
     * KLIST definitions
     *========================================================================*
     C     PDkey         KList
     C                   KFld                    xxName

     *========================================================================*
     * Main
     *========================================================================*
     C     PDkey         Chain     PPD01                              60
     C                   If        *in60 = *on
     C                   Eval      @@RtnCode = '2'
     C                   Else

      ** Create key
     C                   Move      PDFEIN        xxFEIN
     C                   Move      PDMOD         xxMod

      ** Check for multiple matching records.
     C     PDkey         ReadE     PPD01                                  60
     C                   If        *in60 = *off
     C                   Eval      @@RtnCode = '1'
     C                   Else
     C                   Eval      @@Key = xxKey
     C                   EndIf

     C                   EndIf

     C                   Eval      *inLR = *on
     *========================================================================*
     * *inzsr - Initialization subroutine
     *========================================================================*
     C     *inzsr        BEGSR

     C                   Eval      xxName = @@Name

     C                   ENDSR                                                  *inzsr
