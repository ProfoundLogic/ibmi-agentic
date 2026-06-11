      *========================================================================*
      * (c) Copyright Progressive Data Solutions, Inc., 2000                   *
      *========================================================================*
     H/TITLE ISIV20R - MC Group Key Validation Program
      *========================================================================*
      * PROGRAM DESCRIPTION:                                                   *
      *  Validates a Managed Care Group (Standalone).                          *
      *                                                                        *
      * CALLED BY : Various                                                    *
      * CALLS  TO : None                                                       *
      *                                                                        *
      * CREATED   : 02/07/2000                                                 *
      * PROGRAMMER: C:113                                                      *
      * PROJECT   : DR086                                                      *
      *========================================================================*
      * REVISIONS:                                                             *
      *                                                                        *
      * XX/XX/XXXX C:XXX PROJECT: XXXXX                                        *
      *            XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                                                        *
      * ---------------------------------------------------------------------- *
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.                    *
      * ---------------------------------------------------------------------- *
      *========================================================================*
      * FILE USAGE:                                                            *
      *                                                                        *
      *  File    Description                    Rcdfmt  Key                    *
      *  FMFNL   MC Group Master File           PFN01   (CO#/SUB#/FND)         *
      *  SMKTL   Key Type Master File           PKT01   (KTYP)                 *
      *                                                                        *
      *========================================================================*
      * INDICATOR USAGE:                                                       *
      *                                                                        *
      *  21 - General TestN indicator.                                         *
      *  60 - General Chain indicator.                                         *
      *                                                                        *
      *========================================================================*
      * NOTES:                                                                 *
      *                                                                        *
      * A Return code of '1' means that the key contained non-numeric values.  *
      * A Return code of '2' means that the key was not a valid Group.         *
      *                                                                        *
      *========================================================================*
     FFMFNL     IF   E           K DISK
     FSMKTL     IF   E           K DISK

      *========================================================================*
      * Procedure Prototypes                                                   *
      *========================================================================*
     D ValKey          PR             1a
     D  company                       3  0  Const
     D  subscriber                    5  0  Const
     D  group                         3  0  Const

     D TestNum         PR             1a

      *========================================================================*
      * Array, Table & Data Structure Definitions                              *
      *========================================================================*
     D                 ds
     D Key                           40a
     D  Company                       3    overlay(Key)
     D  Subscriber                    5    overlay(Key:4)
     D  Group                         3    overlay(Key:9)

      *========================================================================*
      * Stand-alone Field Definitions                                          *
      *========================================================================*
     D tsCompany       S              3  0
     D tsSub#          S              5  0
     D tsGroup         S              3  0

     D @Key            S             40a
     D @RtnCode        S              1a

      *========================================================================*
      * PLIST Definitions                                                      *
      *========================================================================*
     C     *entry        plist
     C                   parm                    @Key
     C                   parm                    @RtnCode

      *========================================================================*
      * Main                                                                   *
      *========================================================================*
     C                   If        %scan(' ':@Key) < 12
      ** We need to check the Workers Comp key type
     C     'WG'          Chain     PKT01                              60
     C                   If        *in60 = *off
     C                   CALL      KTVPGM
     C                   Parm                    @Key
     C                   Parm                    @RtnCode
     C                   Else
     C                   Eval      @RtnCode = '2'
     C                   EndIf

     C                   Else

     C                   If        TestNum = *blank
     C                   Eval      @RtnCode = ValKey(tsCompany:tsSub#:tsGroup)
     C                   Else
     C                   Eval      @RtnCode = '1'
     C                   EndIf

     C                   EndIf

     C                   Move      *on           *inlr

      *========================================================================*
      * *inzsr - Initialization subroutine                                     *
      *========================================================================*
     C     *inzsr        BEGSR

     C                   Eval      Key = @Key

     C                   endsr

      *========================================================================*
      * ValKey                                                                 *
      *========================================================================*
      * Description: Performs the validation of the key.                       *
      *                                                                        *
      * Parameters :  3.0 - Company                                            *
      *               5.0 - Subscriber                                         *
      *               3.0 - Group                                              *
      *                                                                        *
      * Returns    :  1 a - Error code                                         *
      *========================================================================*
     P ValKey          B

     **** Procedure Interface ***
     D ValKey          PI             1a
     D  Company                       3  0 Const
     D  Subscriber                    5  0 Const
     D  Group                         3  0 Const

     **** KList Definitions ***
      ** Group Master file
     C     FNkey         KList
     C                   KFld                    Company
     C                   KFld                    Subscriber
     C                   KFld                    Group

     **** Procedure Definition ***
      ** Validate MC Group
     C     FNkey         Chain     PFN01                              60
     C                   If        *in60 = *on
     C                   Return    '2'
     C                   Else
     C                   Return    *blank
     C                   EndIf

     P ValKey          E

      *========================================================================*
      * TestNum                                                                *
      *========================================================================*
      * Description: Tests the key for numeric values.                         *
      *                                                                        *
      * Parameters : None                                                      *
      *                                                                        *
      * Returns    :  1 a - Error code                                         *
      *========================================================================*
     P TestNum         B

     **** Procedure Interface ***
     D TestNum         PI             1a

     **** Procedure Definition ***
      ** Test the key for numeric values.
     C                   TestN                   Company              21
     C                   If        *in21 = *on
     C                   Move      Company       tsCompany
     C                   Else
     C                   Return    '1'
     C                   EndIf

     C                   TestN                   Subscriber           21
     C                   If        *in21 = *on
     C                   Move      Subscriber    tsSub#
     C                   Else
     C                   Return    '1'
     C                   EndIf

     C                   TestN                   Group                21
     C                   If        *in21 = *on
     C                   Move      Group         tsGroup
     C                   Else
     C                   Return    '1'
     C                   EndIf

     C                   Return    *blank

     P TestNum         E
