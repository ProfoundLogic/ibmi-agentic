     *========================================================================*
     * (c) Copyright Progressive Data Solutions, Inc., 2002                   *
     *========================================================================*
    H/TITLE ISIV50R - PMSC Policy Index Program
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Validates a PMSC Policy Index Key (Standalone)                        *
      *                                                                        *
     * CALLED BY : Various                                                    *
     * CALLS  TO : None                                                       *
     *                                                                        *
     * CREATED   : 02/26/2002                                                 *
     * PROGRAMMER: C:87                                                       *
     * PROJECT   : DR173                                                      *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     * 11/12/03 C:128 PROJECT: IM081                                          *
     *          If only 2 parms were sent to this program and you tried to XX *
     *          use the entry parm @RtnField the program would fail.          *
     *                                                                        *
     * ---------------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.                    *
     * ---------------------------------------------------------------------- *
     *========================================================================*
     * FILE USAGE:                                                            *
     *                                                                        *
      *  File      Description                    Rcdfmt   Key                 *
     *  ASAAREL1  PMSC Location file             FAAREA0                      *
     *  ASABREL1  PMSC Master company file       FABREA3                      *
     *  PMSP0200  PMSC Policy file               REC02FMT                     *
     *  SMSYL     Symbol file                    REC02FMT                     *
     *  SMKTL     Key Type Master File           PKT01   (KTYP)               *
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
     F*
    F** PMSC Location file
     Fasaarel1  IF   E           K DISK
     F*
    F** PMSC Master Company file
     Fasabrel1  IF   E           K DISK
     F*
    F** PMSC Policy file
     Fpmsp0200  IF   E           K DISK
     F*
    F** Symbol file
     Fsmsyl     IF   E           K DISK    Rename(rec02fmt:psy01)
     F*
    F** Key type file
     Fsmktl     IF   E           K DISK

     D PSDS          ESDS                   extname(SZQ1P)

     *========================================================================*
     * Procedure Prototypes                                                   *
     *========================================================================*
     D ValKey          PR             1a
     D  location                      2     Const
     D  masterCompany                 2     Const
     D  symbol                        3     Const
     D  policy#                       7     Const
     D  module                        2     Const

     D TestNum         PR             1a

     *========================================================================*
     * Array, Table & Data Structure Definitions                              *
     *========================================================================*
     D                 ds
     D Key                           40a
     D  Location                      2a   overlay(Key)
     D  MasterCompany                 2a   overlay(Key:3)
     D  Symbol                        3a   overlay(Key:5)
     D  Policy#                       7a   overlay(Key:8)
     D  Module                        2a   overlay(Key:15)
    
     *========================================================================*
     * Stand-alone Field Definitions                                          *
     *========================================================================*
     D  tsLocation     S              2
     D  tsMCompany     S              2
     D  tsSymbol       S              3
     D  tsPolicy#      S              7
     D  tsModule       S              2
    
     D @Key            S             40a
     D @RtnCode        S              1a
     D @RtnField       S             20a

     *========================================================================*
     * Constants                                                              *
     *========================================================================*
     D  LOC_CONST      C                   const('LOCATION')
     D  MC_CONST       C                   const('MASTERCOMPANY')
     D  SYM_CONST      C                   const('SYMBOL')
     D  POL_CONST      C                   const('POLICY#')
     D  MOD_CONST      C                   const('MODULE')

     *========================================================================*
     * PLIST Definitions                                                      *
     *========================================================================*
     C     *entry        plist
     C                   parm                    @Key
     C                   parm                    @RtnCode
     C                   parm                    @RtnField

      * if @RtnField is passed in, then use it to pass back the field
      * that produced the error.  This is an optional parm.  Values that
      * could be passed back are defined by the constants:
      *                            LOC_CONST for location
      *                            MCO_CONST for master company
      *                            SYM_CONST for symbol
      *                            POL_CONST for policy#
      *                            MOD_CONST for module

     *========================================================================*
     * Main                                                                   *
     *========================================================================*
     C                   If        TestNum = *blank
     C                   Eval      @RtnCode = ValKey( tsLocation :
     C                                                tsMCompany :
     C                                                tsSymbol :
     C                                                tsPolicy# :
     C                                                tsModule)
     C                   Else
     C                   Eval      @RtnCode = '1'
     C                   EndIf

     C                   Move      *on           *inlr

     *========================================================================*
     * *inzsr - Initialization subroutine                                     *
     *========================================================================*
     C     *inzsr        BEGSR

     C                   Eval      Key = @Key
     C                   move      symbol        tsSymbol
    
     C                   endsr

     *========================================================================*
     * ValKey                                                                 *
     *========================================================================*
      * Description: Performs the validation of the key.                       *
      *                                                                        *
     * Parameters :  2   - Location                                           *
      *               2   - MasterCompany                                      *
      *               3   - Symbol                                             *
      *               7.0 - Policy#                                            *
      *               2   - Module                                             *
      *                                                                        *
      * Returns    :  1 a - Error code                                         *
     *========================================================================*
     P ValKey          B

     **** Procedure Interface ***
     D ValKey          PI             1a
     D  location                      2     Const
     D  masterCompany                 2     Const
     D  symbol                        3     Const
     D  policy#                       7     Const
     D  module                        2     Const

     D RtnVal          s              1
     D field           s             20
     D kkid02          s              2     inz('02')

     **** KList Definitions ***
    C***************
    C*  PMSC files
    C***************
    C** PMSC Location file
     C     locky         klist
     C                   kfld                    location
    C*
    C** PMSC Master Company file
     C     mcoky         klist
     C                   kfld                    location
     C                   kfld                    masterCompany
    C*
    C** PMSC Policy file
     C     polky         klist
     C                   kfld                    kkid02
     C                   kfld                    location
     C                   kfld                    policy#
    C*
    C** PMSC Symbol key
     C     symky         klist
     C*                  kfld                    kkid02
     C*                  kfld                    location
     C*                  kfld                    policy#
     C                   kfld                    symbol
    C*
    C** PMSC Module key
     C     modky         klist
     C                   kfld                    kkid02
     C                   kfld                    location
     C                   kfld                    policy#
     C                   kfld                    symbol
     C                   kfld                    masterCompany
     C                   kfld                    module
    
     **** Procedure Definition ***
     ** Validate location
     C                   if        *in90 = *off
     C     locky         chain     asaarel1                           61
     C                   if        *in61 = *on
     C                   move      *on           *in90
     C                   eval      field = LOC_CONST
     C                   endif                                                  *in61=*on
     C                   endif                                                  *in90=*on
    C*
     ** Validate Master Company
     C                   if        *in90 = *off
     C     mcoky         chain     asabrel1                           62
     C                   if        *in62 = *on
     C                   move      *on           *in90
     C                   eval      field = MC_CONST
     C                   endif                                                  *in62=*on
     C                   endif                                                  *in90=*on
    C*
     ** Validate Policy Number
     C                   if        *in90 = *off
     C     polky         chain     pmsp0200                           63
     C                   if        *in63 = *on
     C                   move      *on           *in90
     C                   eval      field = POL_CONST
     C                   endif                                                  *in63=*on
     C                   endif                                                  *in90=*on
    C*
     ** Validate Symbol
     C                   if        *in90 = *off
     C     symky         chain     psy01                              64
     C                   if        *in64 = *on
     C                   move      *on           *in90
     C                   eval      field = SYM_CONST
     C                   endif                                                  *in64=*on
     C                   endif                                                  *in90=*on
    C*
     ** Validate Module
     C                   if        *in90 = *off
     C     modky         chain     pmsp0200                           65
     C                   if        *in65 = *on
     C                   move      *on           *in90
     C                   eval      field = MOD_CONST
     C                   endif                                                  *in65=*on
     C                   endif                                                  *in90=*on
    
     C*  Return appropriate value to call
     C                   if        *in90 = *on
     C
     C                   if        q1prms > 2
     C                   eval        @RtnField = field
     C                   endif
     C                   return    '2'
     C
     C                   else
     C
      *** Begin Change 11/12/03 ********************************************
     C******             eval        @RtnField = *blank
     C******             return    ' '
      *
     C                   if        q1prms > 2
     C                   eval        @RtnField = *blank
     C                   endif
     C                   return    ' '
      *** End   Change 11/12/03 ********************************************
     C
     C                   endif                                                  *in90=*on

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
      *           *** Location ***
     C                   TestN                   Location             21
     C                   If        *in21 = *on
     C                   Move      Location      tsLocation
     C                   Else
     C
     C                   if        q1prms > 2
     C                   eval        @RtnField = LOC_CONST
     C                   endif
     C
     C                   Return    '1'
     C                   EndIf
    
      *           *** Master Company ***
     C                   TestN                   MasterCompany        21
     C                   If        *in21 = *on
     C                   Move      MasterCompany tsMCompany
     C                   Else
     C
     C                   if        q1prms > 2
     C                   eval        @RtnField = mc_CONST
     C                   endif
     C
     C                   Return    '1'
     C                   EndIf

      *           *** Policy # ***
     C                   TestN                   Policy#              21
     C                   If        *in21 = *on
     C                   Move      Policy#       tsPolicy#
     C                   Else
     C
     C                   if        q1prms > 2
     C                   eval        @RtnField = POL_CONST
     C                   endif
     C
     C                   Return    '1'
     C                   EndIf
    
      *           *** Module ***
     C                   TestN                   Module               21
     C                   If        *in21 = *on
     C                   Move      Module        tsModule
     C                   Else
     C
     C                   if        q1prms > 2
     C                   eval        @RtnField = MOD_CONST
     C                   endif
     C
     C                   Return    '1'
     C                   EndIf
    
     C                   Return    *blank

     P TestNum         E
