      *========================================================================
      * (c) Copyright Progressive Data Solutions, Inc., 2000
      *========================================================================
     H/TITLE ISIV61R - SIS Claim Key Validation Program
      *========================================================================*
      *
      * PROGRAM DESCRIPTION:
      *  Validates a SIS Claim Key
      *
      * CALLED BY : Various
      * CALLS  TO : None
      *
      * CREATED   : 06/26/2000
      * PROGRAMMER: C:113
      * PROJECT   :
      *========================================================================
      * REVISIONS:
      *
      * XX/XX/XXXX C:XXX PROJECT: XXXXX
      *            XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      *
      * ----------------------------------------------------------------------
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.
      * ----------------------------------------------------------------------
      *========================================================================
      * FILE USAGE:
      *
      *  File      Description                     Rcdfmt
      *  CMOCUR04  SIS CMS Occurrence Information  CMOCURR
      *
      *========================================================================
      * INDICATOR USAGE:
      *
      *  21 - General TestN indicator.
      *  60 - General Chain indicator.
      *
      *========================================================================
      * NOTES:
      *
      * A Return code of '1' means that the key contained non-numeric values.
      * A Return code of '2' means that the key was not a valid Claim.
      *
      *========================================================================
      * SIS Claim Master File
     FCMOCUR04  IF   E           K DISK

      *========================================================================
      * Procedure Prototypes
      *========================================================================
     D ValKey          PR             1a
     D  company                       2a    Const
     D  prefix                        4a    Const
     D  policy                        9  0  Const
     D  branch                        2a    Const
     D  claim                        10a    Const

     D TestNum         PR             1a

      *========================================================================
      * Array, Table & Data Structure Definitions
      *========================================================================
      ** DARTS Key structure
     D                 ds
     D Key                           40a
     D  company                       2    overlay(Key)
     D  prefix                        4    overlay(Key:3)
     D  policy                        9    overlay(Key:7)
     D  branch                        2    overlay(Key:16)
     D  claim                        10    overlay(Key:18)

      *========================================================================
      * Stand-alone Field Definitions
      *========================================================================
     D tsPolicy        S              9  0

     D @Key            S             40a
     D @RtnCode        S              1a

      *========================================================================
      * PLIST Definitions
      *========================================================================
     C     *entry        plist
     C                   parm                    @Key                           DARTS Key
     C                   parm                    @RtnCode                       Return Code

      *========================================================================
      * Main
      *========================================================================
     C                   Eval      @RtnCode = TestNum
     C                   If        @RtnCode = *blank
     C                   Eval      @RtnCode = ValKey(company:prefix:tsPolicy:
     C                                               branch:claim)
     C                   EndIf

     C                   Move      *on           *inlr

      *========================================================================
      * *inzsr - Initialization subroutine
      *========================================================================
     C     *inzsr        BEGSR

     C                   Eval      Key = @Key

     C                   endsr

      *========================================================================
      * ValKey
      *========================================================================
      * Description Performs the validation of the key.
      *
      * Returns 1a - Error code
      *
      * Parameters  3a  - Company
      *             4a  - Prefix
      *             9.0 - Policy Number
      *             2a  - Branch
      *            10a  - Claim
      *
      *========================================================================
     P ValKey          B

     **** Procedure Interface ***
     D ValKey          PI             1a
     D  pCompany                      2a   Const
     D  pPrefix                       4a   Const
     D  pPolicy                       9  0 Const
     D  pBranch                       2a   Const
     D  pClaim                       10a   Const

     **** KList Definitions ***
      ** SIS Claim Master File
     C     CMkey         KList
     C                   KFld                    pCompany
     C                   KFld                    pPrefix
     C                   KFld                    pPolicy
     C                   KFld                    pBranch
     C                   KFld                    pClaim

     **** Procedure Definition ***
      ** Validate SIS Claim
     C     CMkey         Chain     CMOCURR                            60
     C                   If        *in60 = *on
     C                   Return    '2'
     C                   Else
     C                   Return    *blank
     C                   EndIf

     P ValKey          E

      *========================================================================
      * TestNum
      *========================================================================
      * Description: Tests the key for numeric values.
      *
      * Parameters : None
      *
      * Returns    :  1 a - Error code
      *========================================================================
     P TestNum         B

     **** Procedure Interface ***
     D TestNum         PI             1a

     **** Procedure Definition ***
      ** Test the key for numeric values.
     C                   TestN                   policy               21
     C                   If        *in21 = *on
     C                   Move      policy        tsPolicy
     C                   Else
     C                   Return    '1'
     C                   EndIf

     C                   Return    *blank

     P TestNum         E
