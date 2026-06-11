     *========================================================================*
     * (c) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE ISIV23R - MC Medical Cases Key Validation Program
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Validates a Managed Care Medical Cases Key (Standalone)               *
      *                                                                        *
     * CALLED BY : Various                                                    *
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
     *  FMCML1  MC Medical Cases Master File   PCM01   (CO#/SUB#/FND/FYR/CASE)*
     *  SMKTL   Key Type Master File           PKT01   (KTYP)                 *
     *                                                                        *
     *========================================================================*
     * INDICATOR USAGE:                                                       *
     *                                                                        *
     *  21 - General TestN indicator.                                         *
     *  60 - General Chain indicator.                                         *
     *                                                                        *
     *======================================================================= *
     * NOTES:                                                                 *
     *                                                                        *
     * ** IMPORTANT **                                                        *
     *  -This program must be compiled to a target release of V3R7M0 or       *
     *   greater due to the use of the BIF %Scan.                             *
     *                                                                        *
     * A Return code of '1' means that the key contained non-numeric values.  *
     * A Return code of '2' means that the key was not a valid Claim.         *
     *                                                                        *
     *========================================================================*
     FFMCML1    IF   E           K DISK
     FSMKTL     IF   E           K DISK

     *========================================================================*
     * Procedure Prototypes                                                   *
     *========================================================================*
     D ValKey          PR             1a
     D  company                       3  0  Const
     D  subscriber                    5  0  Const
     D  group                         3  0  Const
     D  policyYear                    3  0  Const
     D  case                          7  0  Const

     D TestNum         PR             1a

     *========================================================================*
     * Array, Table & Data Structure Definitions                              *
     *========================================================================*
     D                 ds
     D Key                           40a
     D  Company                       3a   overlay(Key)
     D  Subscriber                    5a   overlay(Key:4)
     D  Group                         3a   overlay(Key:9)
     D  PolicyYear                    3a   overlay(Key:12)
     D  Case                          7a   overlay(Key:15)
    
     *========================================================================*
     * Stand-alone Field Definitions                                          *
     *========================================================================*
     D  tsCompany      S              3  0
     D  tsSub#         S              5  0
     D  tsGroup        S              3  0
     D  tsPolYear      S              3  0
     D  tsCase         S              7  0
    
     D @Key            S             40a
     D @RtnCode        S              1a

     *========================================================================*
     * PLIST Definitions                                                      *
     *========================================================================*
     C     *entry        plist
     C                   parm                    @Key
     C                   parm                    @RtnCode

     *========================================================================*
     * Main                                                                   *
     *========================================================================*
     C                   If        %scan(' ':@Key) < 22
      ** We need to check the Workers Comp Claim key type.
     C     'WC'          Chain     PKT01                              60
     C                   If        *in60 = *off
     C                   CALL      KTVPGM
     C                   Parm                    @Key
     C                   Parm                    @RtnCode
     C                   Else
     C                   Eval      @RtnCode = '2'
     C                   EndIf

     C                   Else

     C                   Eval      @RtnCode = TestNum
     C                   If        @RtnCode = *blank
     C                   Eval      @RtnCode = ValKey(tsCompany:tsSub#:tsGroup:
     C                                               tsPolYear:tsCase)
     C                   EndIf

     C                   EndIf

     C                   Move      *on           *inlr

     *========================================================================*
     * *inzsr - Initialization subroutine                                     *
     *========================================================================*
     C     *inzsr        BEGSR

     C                   Eval      Key = @Key
    
     C                   endsr

     *========================================================================*
     * ValKey                                                                 *
     *========================================================================*
      * Description: Performs the validation of the key.                       *
      *                                                                        *
     * Parameters :  3.0 - Company                                            *
      *               5.0 - Subscriber                                         *
      *               3.0 - Group                                              *
      *               3.0 - PolicyYear                                         *
      *               7.0 - Case                                               *
      *                                                                        *
      * Returns    :  1 a - Error code                                         *
     *========================================================================*
     P ValKey          B

     **** Procedure Interface ***
     D ValKey          PI             1a
     D  Company                       3  0 Const
     D  Subscriber                    5  0 Const
     D  Group                         3  0 Const
     D  PolicyYear                    3  0 Const
     D  Case                          7  0 Const

     **** KList Definitions ***
     ** Medical Cases Master file
     C     CMkey         KList
     C                   KFld                    Company
     C                   KFld                    Subscriber
     C                   KFld                    Group
     C                   KFld                    PolicyYear
     C                   KFld                    Case
    
     **** Procedure Definition ***
     ** Validate MC Medical Case
     C     CMkey         Chain     PCM01                              60
     C                   If        *in60 = *on
     C                   Return    '2'
     C                   Else
     C                   Return    *blank
     C                   EndIf

     P ValKey          E

     *========================================================================*
     * TestNum                                                                *
     *========================================================================*
      * Description: Tests the key for numeric values.                         *
      *                                                                        *
     * Parameters : None                                                      *
      *                                                                        *
      * Returns    :  1 a - Error code                                         *
     *========================================================================*
     P TestNum         B

     **** Procedure Interface ***
     D TestNum         PI             1a

     **** Procedure Definition ***
     ** Test the key for numeric values.
     C                   TestN                   Company              21
     C                   If        *in21 = *on
     C                   Move      Company       tsCompany
     C                   Else
     C                   Return    '1'
     C                   EndIf
    
     C                   TestN                   Subscriber           21
     C                   If        *in21 = *on
     C                   Move      Subscriber    tsSub#
     C                   Else
     C                   Return    '1'
     C                   EndIf
    
     C                   TestN                   Group                21
     C                   If        *in21 = *on
     C                   Move      Group         tsGroup
     C                   Else
     C                   Return    '1'
     C                   EndIf

     C                   TestN                   PolicyYear           21
     C                   If        *in21 = *on
     C                   Move      PolicyYear    tsPolYear
     C                   Else
     C                   Return    '1'
     C                   EndIf
    
     C                   TestN                   Case                 21
     C                   If        *in21 = *on
     C                   Move      Case          tsCase
     C                   Else
     C                   Return    '1'
     C                   EndIf
    
     C                   Return    *blank

     P TestNum         E
