     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SME01R - MC Employer Name Validation
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Searches the Managed Care Employer Master File for a matching name.   *
     *  If only one is found, it returns the DARTS key for that Employer,     *
     *  otherwise it returns an error code.                                   *
     *                                                                        *
     * CALLED BY : DARTMail Server Program                                    *
     * CALLS  TO : None                                                       *
     *                                                                        *
     * CREATED   : 02/04/2000                                                 *
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
     *  FMEML5  MC Employer Master File        PEM01   (NAME)                 *
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
      ** Managed Care Employer Master File
     FFMEML5    IF   E           K DISK

      *========================================================================*
      * Prototypes
      *========================================================================*
     D McCvtApi        PR                   ExtPgm('FA719R')
     D  rtncode                       1a
     D  wcin                          1a
     D  wdck                          1a
     D  company                       3a
     D  subscriber                    5a
     D  group                         3a
     D  employer                      9a
     D  division                      5a
     D  key                          40a

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D @name           S             40a
     D @key            S             40a
     D @rtnCode        S              1a

     D wcin            S              1a
     D wdck            S              1a
     D company         S              3a
     D subscriber      S              5a
     D group           S              3a
     D employer        S              9a
     D division        S              5a
     D empName         S                   Like(EMNAME)
     D rtnCode         S              1a
     D tempKey         S             40a

     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    @name
     C                   Parm                    @key
     C                   Parm                    @rtnCode

     *========================================================================*
     * KLIST definitions
     *========================================================================*
     C     EMkey         KList
     C                   KFld                    empName

     *========================================================================*
     * Main
     *========================================================================*
     C     EMkey         Chain     PEM01                              60
     C                   If        *in60 = *on
     C                   Eval      @rtnCode = '2'
     C                   Else

      ** Create key
     C                   Eval      wcin = 'Y'
     C                   Eval      wdck = 'Y'
     C                   Eval      rtnCode = 'K'
     C                   Move      EMCO#         company
     C                   Move      EMSUB#        subscriber
     C                   Move      EMFND         group
     C                   Move      EMEMP#        employer
     C                   Move      EMDIV         division

     C                   CALLP     McCvtApi(rtnCode:
     C                                      wcin:
     C                                      wdck:
     C                                      company:
     C                                      subscriber:
     C                                      group:
     C                                      employer:
     C                                      division:
     C                                      tempKey)

      ** Check for multiple matching records.
     C     EMkey         ReadE     PEM01                                  60
     C                   If        *in60 = *off
     C                   Eval      @rtnCode = '1'
     C                   EndIf

     C                   EndIf

     C                   If        @rtnCode = *blank
     C                   Eval      @key = tempKey
     c                   Else
     C                   Eval      @key = *blanks
     C                   EndIf

     C                   Eval      *inLR = *on
     *========================================================================*
     * *inzsr - Initialization subroutine
     *========================================================================*
     C     *inzsr        BEGSR

     C                   Eval      empName = @name

     C                   ENDSR                                                  *inzsr
