     *========================================================================*
     * (c) Copyright Progressive Data Solutions, Inc., 2002                   *
     *========================================================================*
    H/TITLE ISIV51R - PMSC Agent Index Program
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Validates a PMSC Agent Index Key (Standalone)                         *
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
     * xx/xx/xx C:xxx PROJECT: xxxxx                                          *
     *          xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxXX *
     *                                                                        *
     * ---------------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.                    *
     * ---------------------------------------------------------------------- *
     *========================================================================*
     * FILE USAGE:                                                            *
     *                                                                        *
      *  File      Description                    Rcdfmt   Key                 *
     *  ASABREL1  PMSC Master company file       FABREA3                      *
     *  PMSPAG00  PMSC Agent file                AGCYMST                      *
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
    F** PMSC Master Company file
     Fasabrel1  IF   E           K DISK
     F*
    F** PMSC Agent file
     Fpmspag00  IF   E           K DISK
     F*
    F** Key type file
     Fsmktl     IF   E           K DISK

     D PSDS          ESDS                   extname(SZQ1P)

     *========================================================================*
     * Procedure Prototypes                                                   *
     *========================================================================*
     D ValKey          PR             1a
     D  masterCompany                 2     Const
     D  agent#                        7     Const

     D TestNum         PR             1a

     *========================================================================*
     * Array, Table & Data Structure Definitions                              *
     *========================================================================*
     D                 ds
     D Key                           40a
     D  MasterCompany                 2a   overlay(Key:1)
     D  Agent#                        7a   overlay(Key:3)
    
     *========================================================================*
     * Stand-alone Field Definitions                                          *
     *========================================================================*
     D  tsMCompany     S              2
     D  tsAgent#       S              7
    
     D @Key            S             40a
     D @RtnCode        S              1a
     D @RtnField       S             20a

     *========================================================================*
     * Constants                                                              *
     *========================================================================*
     D  MC_CONST       C                   const('MASTERCOMPANY')
     D  AGT_CONST      C                   const('AGENT#')

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
      *                            MCO_CONST for master company
      *                            AGT_CONST for agent#

     *========================================================================*
     * Main                                                                   *
     *========================================================================*
     C                   If        TestNum = *blank
     C                   Eval      @RtnCode = ValKey( tsMCompany :
     C                                                tsAgent# )
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
     * Parameters :  2   - MasterCompany                                      *
      *               7.0 - Agent#                                             *
      *                                                                        *
      * Returns    :  1 a - Error code                                         *
     *========================================================================*
     P ValKey          B

     **** Procedure Interface ***
     D ValKey          PI             1a
     D  masterCompany                 2     Const
     D  agent#                        7     Const

     D RtnVal          s              1
     D field           s             20
     D kkid02          s              2     inz('02')
     D location        s              2     inz('00')

     **** KList Definitions ***
    C***************
    C*  PMSC files
    C***************
    C** PMSC Master Company file
     C     mcoky         klist
     C                   kfld                    location
     C                   kfld                    masterCompany
    C*
    C** PMSC Agent file
     C     agtky         klist
     C                   kfld                    masterCompany
     C                   kfld                    agent#
    
     **** Procedure Definition ***
     ** Validate Master Company
     C                   if        *in90 = *off
     C     mcoky         chain     asabrel1                           62
     C                   if        *in62 = *on
     C                   move      *on           *in90
     C                   eval      field = MC_CONST
     C                   endif                                                  *in62=*on
     C                   endif                                                  *in90=*on
    C*
     ** Validate Agent Number
     C                   if        *in90 = *off
     C     agtky         chain     pmspag00                           63
     C                   if        *in63 = *on
     C                   move      *on           *in90
     C                   eval      field = AGT_CONST
     C                   endif                                                  *in63=*on
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
     C                   eval        @RtnField = *blank
     C                   return    ' '
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
      *           *** Master Company ***
     C                   TestN                   MasterCompany        21
     C                   If        *in21 = *on
     C                   Move      MasterCompany tsMCompany
     C                   Else
     C
     C                   if        q1prms > 2
     C                   eval        @RtnField = MC_CONST
     C                   endif
     C
     C                   Return    '1'
     C                   EndIf

      *           *** Agent # ***
     C                   TestN                   Agent#               21
     C                   If        *in21 = *on
     C                   Move      Agent#        tsAgent#
     C                   Else
     C
     C                   if        q1prms > 2
     C                   eval        @RtnField = AGT_CONST
     C                   endif
     C
     C                   Return    '1'
     C                   EndIf
    
     C                   Return    *blank

     P TestNum         E
