     *========================================================================*
     * (c) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE ISIV24R - MC Provider Key Validation Program
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Validates a Managed Care Provider key.                                *
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
     *  FMPDL   MC Payee Demographics File     PPD01   (FEIN/MOD)             *
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
     * A Return code of '2' means that the key was not a valid Provider.      *
     *                                                                        *
     *========================================================================*
     FWMPDL     IF   E           K DISK
    
     *========================================================================*
     * Procedure Prototypes                                                   *
     *========================================================================*
     D ValKey          PR             1a
     D  FEIN                          9  0 Const
     D  mod                           3  0 Const

     D TestNum         PR             1a

     *========================================================================*
     * Array, Table & Data Structure Definitions                              *
     *========================================================================*
     D                 ds
     D Key                           40a
     D  FEIN                          9a   overlay(Key)
     D  Mod                           3a   overlay(Key:10)
    
     *========================================================================*
     * Stand-alone Field Definitions                                          *
     *========================================================================*
     D  tsFEIN         S              9  0
     D  tsMod          S              3  0
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
     C                   If        TestNum = *blank
     C                   Eval      @RtnCode = ValKey(tsFEIN:tsMod)
     C                   Else
     C                   Eval      @RtnCode = '1'
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
     * Parameters :  9.0 - FEIN                                               *
      *               3.0 - Modifier                                           *
      *                                                                        *
      * Returns    :  1 a - Error code                                         *
     *========================================================================*
     P ValKey          B

     **** Procedure Interface ***
     D ValKey          PI             1a
     D  FEIN                          9  0 Const
     D  Mod                           3  0 Const

     **** KList Definitions ***
     ** Pre-Cert Master file
     C     PDkey         KList
     C                   KFld                    FEIN
     C                   KFld                    Mod
    
     **** Procedure Definition ***
     ** Validate MC Provider key
     C     PDkey         Chain     PPD01                              60
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
     C                   TestN                   FEIN                 21
     C                   If        *in21 = *on
     C                   Move      FEIN          tsFEIN
     C                   Else
     C                   Return    '1'
     C                   EndIf
    
     C                   TestN                   Mod                  21
     C                   If        *in21 = *on
     C                   Move      Mod           tsMod
     C                   Else
     C                   Return    '1'
     C                   EndIf

     C                   Return    *blank

     P TestNum         E
