      *========================================================================
      * (c) Copyright StoneRiver Inc., 2009
      *========================================================================
     H/TITLE ISIV62R - SIS Claim Key Validation Program
      *========================================================================*
      *
      * PROGRAM DESCRIPTION:
      *  Validates a SIS Claim Key
      *
      * CALLED BY : Various
      * CALLS  TO : None
      *
      * CREATED   : 04/06/2009
      * PROGRAMMER: C:062
      * PROJECT   : 115IAK
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
      *  CMOCUR01  SIS CMS Occurrence Information  CMOCURR
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
     FCMOCUR01  IF   E           K DISK

      *========================================================================
      * Procedure Prototypes
      *========================================================================
     D ValKey          PR             1a
     D  claim                        10a    Const

      *========================================================================
      * Array, Table & Data Structure Definitions
      *========================================================================
      ** DARTS Key structure
     D                 ds
     D Key                           40a
     D  claim                        10    overlay(Key)

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

     C                   Eval      @RtnCode = ValKey(claim)

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
      * Parameters 10a  - Claim
      *
      *========================================================================
     P ValKey          B

     **** Procedure Interface ***
     D ValKey          PI             1a
     D  pClaim                       10a   Const

     **** KList Definitions ***
      ** SIS Claim Master File
     C     CMky          KList
     C                   KFld                    pClaim

     **** Procedure Definition ***
      ** Validate SIS Claim
     C     CMky          Chain     CMOCURR                            60
     C                   If        *in60 = *on
     C                   Return    '2'
     C                   Else
     C                   Return    *blank
     C                   EndIf

     P ValKey          E
