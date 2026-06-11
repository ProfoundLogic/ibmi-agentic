     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SMC01R - MC Claim Name Validation
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Searches the Managed Care Claim Master File for a matching name.      *
     *  If only one is found, it returns the DARTS key for that Claim,        *
     *  otherwise it returns an error code.                                   *
     *                                                                        *
     * CALLED BY : IDRTMR - DARTS E-mail Server Edit Program                  *
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
     * FILE USAGE:                                                            *
     *                                                                        *
      *  File    Description                    Rcdfmt  Key                    *
     *  FMCMLR  MC Medical Cases Master File   PCM01   (LSN, FSN)             *
     *                                                                        *
     *========================================================================*
     * INDICATOR USAGE:                                                       *
     *                                                                        *
     *  60 - General Chain indicator.                                         *
     *                                                                        *
     *========================================================================*
     * NOTES:                                                                 *
     *                                                                        *
     *  ** IMPORTANT **                                                       *
     *  -This program must be compiled to a target release of V3R7M0 or       *
     *   greater due to the use of the BIF %Scan.                             *
     *                                                                        *
     *========================================================================*
      ** Managed Care Medical Cases Master File
     FFMCMLR    IF   E           K DISK

      *========================================================================*
      * Prototypes
      *========================================================================*
     D McCvtApi        PR                   ExtPgm('FA703R')
     D  rtncode                       1a
     D  wcin                          1a
     D  wdck                          1a
     D  company                       3a
     D  subscriber                    5a
     D  group                         3a
     D  policyYear                    3a
     D  case                          7a
     D  key                          40a

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D @Name           S             40a
     D @Key            S             40a
     D @RtnCode        S              1a

     D FirstName       S                   Like(CMFSN)
     D LastName        S                   Like(CMLSN)
     D x               S              3  0

     D wcin            S              1a
     D wdck            S              1a
     D company         S              3a
     D subscriber      S              5a
     D group           S              3a
     D policyYear      S              3a
     D case            S              7a
     D rtnCode         S              1a
     D tempKey         S             40a

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
     C     CMkey         KList
     C                   KFld                    LastName
     C                   KFld                    FirstName

     *========================================================================*
     * Main
     *========================================================================*
     C                   If        @RtnCode = *blank

      ** Look for the name.
     C     CMkey         Chain     PCM01                              60
     C                   If        *in60 = *off
      ** Create key
     C                   Move      CMCO#         company
     C                   Move      CMSUB#        subscriber
     C                   Move      CMFND         group
     C                   Move      CMFYR         policyYear
     C                   Move      CMCASE        case
     C                   Eval      wcin = 'Y'
     C                   Eval      wdck = 'Y'
     C                   Eval      rtnCode = 'K'

     C                   CALLP     McCvtApi(rtnCode:
     C                                      wcin:
     C                                      wdck:
     C                                      company:
     C                                      subscriber:
     C                                      group:
     C                                      policyYear:
     C                                      case:
     C                                      tempKey)

      ** We've found one, now let's look for another matching name.
     C     CMkey         ReadE     PCM01                                  60

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
     C                   Eval      @Key = tempKey
     c                   Else
     C                   Eval      @key = *blanks
     C                   EndIf

     C                   EndIf

     C                   Eval      *inLR = *on
     *========================================================================*
     * *inzsr - Initialization subroutine
     *========================================================================*
     C     *inzsr        BEGSR

      ** Look for the comma in the name.
     C                   Eval      x = %scan(',' : @Name)

      ** If no comma, the name is in the wrong format; return error code.
     C                   If        x = 0
     C                   Eval      @RtnCode = '2'
     C                   Else
      ** Otherwise, parse the first and last names.
     C                   Eval      LastName  = %trim(%subst(@Name : 1 : x-1))
     C                   Eval      FirstName = %trim(%subst(@Name : x+1))
     C                   EndIf

     C                   ENDSR                                                  *inzsr
