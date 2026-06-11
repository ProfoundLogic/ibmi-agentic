     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SWE01R - WC Employer Name Search
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Searches the Workers' Comp Employer Master File for a matching name.  *
     *  If one is found, it returns the DARTS key for that employer,          *
     *  otherwise it returns an error code.                                   *
     *  If it is called with a 'Q' in the return code, then it is a search    *
     *  for a Quote Employer's name.  The LIBL has been set in the calling    *
     *  program, so all that is necessary is to put a 'Q' in the 40th         *
     *  position of the key.                                                  *
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
      ** Workers' Comp Employer Master File
     FWMEML15   IF   E           K DISK
     *========================================================================*
     * Constant Definitions
     *========================================================================*
     *========================================================================*
     * Array, Table & Data Structure Definitions
     *========================================================================*
     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D @@Name          S             40a
     D @@Key           S             40a
     D @@RtnCode       S              1a

     D xxName          S                   Like(EMNAME)
     D                 DS
     D xxKey                         40a
     D xxCompany                      3a   Overlay(xxKey:1)
     D xxGroup                        3a   Overlay(xxKey:4)
     D xxEmployer                     9a   Overlay(xxKey:7)
     D xxDivision                     5a   Overlay(xxKey:16)
     D xxQuote                        1a   Overlay(xxKey:40)
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
     C     EMkey         KList
     C                   KFld                    xxName
     *========================================================================*
     * Main
     *========================================================================*
     C     EMkey         Chain     PEM01                              60
     C                   If        *in60 = *on
     C                   Eval      @@RtnCode = '2'
     C                   Else

      ** Create key
     C                   Move      EMCO#         xxCompany
     C                   Move      EMFND         xxGroup
     C                   Move      EMEMP#        xxEmployer
     C                   Move      EMDIV         xxDivision

      ** Check for multiple matching records.
     C     EMkey         ReadE     PEM01                                  60
     C                   If        *in60 = *off
     C                   Eval      @@RtnCode = '1'
     C                   Eval      @@Key = *blanks
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

     C                   If        @@RtnCode = 'Q'
     C                   Eval      xxQuote = @@RtnCode
     C                   Eval      @@RtnCode = *blank
     C                   EndIf

     C                   ENDSR                                                  *inzsr
