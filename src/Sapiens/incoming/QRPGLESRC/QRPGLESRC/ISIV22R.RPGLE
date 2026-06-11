     *========================================================================*
     * (c) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE ISIV22R - MC Employer Key Validation Program
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Validates a Managed Care Employer (Standalone).                       *
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
     *  FMEML   MC Employer Master File        PEM01   (CO#/SUB#/FND/EMP#/DIV)*
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
     * A Return code of '1' means that the key contained non-numeric values.  *
     * A Return code of '2' means that the key was not a valid Group.         *
     *                                                                        *
     *========================================================================*
     FFMEML     IF   E           K DISK
     FSMKTL     IF   E           K DISK

     *========================================================================*
     * Procedure Prototypes                                                   *
     *========================================================================*
     D ValKey          PR             1a
     D  company                       3  0  Const
     D  subscriber                    5  0  Const
     D  group                         3  0  Const
     D  employer                      9  0  Const
     D  division                      5  0  Const

     D TestNum         PR             1a

     *========================================================================*
     * Array, Table & Data Structure Definitions                              *
     *========================================================================*
     D                 ds
     D Key                           40a
     D  Company                       3a   overlay(Key)
     D  Subscriber                    5a   overlay(Key:4)
     D  Group                         3a   overlay(Key:9)
     D  Employer                      9a   overlay(Key:12)
     D  Division                      5a   overlay(Key:21)
    
     *========================================================================*
     * Stand-alone Field Definitions                                          *
     *========================================================================*
     D  tsCompany      S              3  0
     D  tsSub#         S              5  0
     D  tsGroup        S              3  0
     D  tsEmployer     S              9  0
     D  tsDivision     S              5  0
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
     C                   If        %scan(' ':@Key) < 26
      ** We need to check the Workers Comp Employer key type.
     C     'WE'          Chain     PKT01                              60
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
     C                                               tsEmployer:tsDivision)
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
      *               9.0 - Employer                                           *
      *               5.0 - Division                                           *
      *                                                                        *
      * Returns    :  1 a - Error code                                         *
     *========================================================================*
     P ValKey          B

     **** Procedure Interface ***
     D ValKey          PI             1a
     D  Company                       3  0 Const
     D  Subscriber                    5  0 Const
     D  Group                         3  0 Const
     D  Employer                      9  0 Const
     D  Division                      5  0 Const

     **** KList Definitions ***
     ** Group Master File
     C     EMkey         KList
     C                   KFld                    Company
     C                   KFld                    Subscriber
     C                   KFld                    Group
     C                   KFld                    Employer
     C                   KFld                    Division
    
     **** Procedure Definition ***
     ** Validate MC Employer
     C     EMkey         Chain     PEM01                              60
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

     C                   TestN                   Employer             21
     C                   If        *in21 = *on
     C                   Move      Employer      tsEmployer
     C                   Else
     C                   Return    '1'
     C                   EndIf
    
     C                   TestN                   Division             21
     C                   If        *in21 = *on
     C                   Move      Division      tsDivision
     C                   Else
     C                   Return    '1'
     C                   EndIf
    
     C                   Return    *blank

     P TestNum         E
