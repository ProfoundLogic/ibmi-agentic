     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SWE01R - WC Group Name Search
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Searches the Workers' Comp Group Master File for a matching name.     *
     *  If only one is found, it returns the DARTS key for that Group,        *
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
      ** Workers' Comp Group Master File
     FWMFNL3    IF   E           K DISK
     FWMFNL4    IF   E           K DISK    Rename(PFN01:PFN02)

     *========================================================================*
     * Array, Table & Data Structure Definitions
     *========================================================================*
      ** DARTS Key structure
     D                 DS
     D xxKey                          6a
     D xxCompany                      3a   Overlay(xxKey:1)
     D xxGroup                        3a   Overlay(xxKey:4)

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D @@Name          S             40a
     D @@Key           S             40a
     D @@RtnCode       S              1a
     D xxName          S                   Like(FNNAME)
     D xxShortNm       S                   Like(FNSHNM)

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
     C     FNkey         KList
     C                   KFld                    xxName

     C     FNkey2        KList
     C                   KFld                    xxShortNm

     *========================================================================*
     * Main
     *========================================================================*
     C     FNkey         Chain     PFN01                              60
     C                   If        *in60 = *on
     C     FNkey2        Chain     PFN02                              60
     C                   If        *in60 = *on
     C                   Eval      @@RtnCode = '2'
     C                   Else

      ** Create key
     C                   Move      FNCO#         xxCompany
     C                   Move      FNFND         xxGroup

      ** Check for multiple matching records.
     C     FNkey2        ReadE     PFN02                                  60
     C                   If        *in60 = *off
     C                   Eval      @@RtnCode = '1'
     C                   Else
     C                   Eval      @@Key = xxKey
     C                   EndIf

     C                   EndIf

     C                   Else
      ** Create key
     C                   Move      FNCO#         xxCompany
     C                   Move      FNFND         xxGroup

      ** Check for multiple matching records.
     C     FNkey         ReadE     PFN01                                  60
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
     C                   Eval      xxShortNm = @@Name

     C                   ENDSR                                                  *inzsr
