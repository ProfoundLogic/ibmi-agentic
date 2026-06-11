     *========================================================================*
     * (c) Copyright Progressive Data Solutions, Inc., 2002                   *
     *========================================================================*
    H/TITLE ISIV70R - P&C  Policy Index Program
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Validates a P&C Policy Index Key (Standalone)                         *
      *                                                                        *
     * CALLED BY : Various                                                    *
     * CALLS  TO : None                                                       *
     *                                                                        *
     * CREATED   : 03/25/2007                                                 *
     * PROGRAMMER: C:02                                                       *
     * PROJECT   : 114COF                                                     *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     *                                                                        *
     * ---------------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.                    *
     * ---------------------------------------------------------------------- *
     *========================================================================*
     * FILE USAGE:                                                            *
     *                                                                        *
      *  File      Description                    Rcdfmt   Key                 *
     *  CMPML     P&C  Policy file               PPM01  T                     *
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
     * A Return code of '2' means that the key was not a valid Policy.        *
     *                                                                        *
     *========================================================================*
     F*
     FCMPML     IF   E           K DISK
     F*
    F** Key type file
     Fsmktl     IF   E           K DISK

     D PSDS          ESDS                   extname(SZQ1P)

     *========================================================================*
     * Procedure Prototypes                                                   *
     *========================================================================*
     D ValKey          PR             1a
     D  Company#                      3  0  Const
     D  policy#                      13     Const

     D TestNum         PR             1a

     *========================================================================*
     * Array, Table & Data Structure Definitions                              *
     *========================================================================*
     D                 ds
     D Key                           40a
     D  Company#                      3    overlay(Key)
     D  Policy#                      13    overlay(Key:4)
    
     *========================================================================*
     * Stand-alone Field Definitions                                          *
     *========================================================================*
     D  tsCompany#     S              3  0
     D  tsPolicy#      S             13
    
     D @Key            S             40a
     D @RtnCode        S              1a
     D @RtnField       S             20a

     *========================================================================*
     * Constants                                                              *
     *========================================================================*
     D  CO#_CONST      C                   const('COMPANY')
     D  POL_CONST      C                   const('POLICY#')

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
      *                            CO#_CONST for Company#
      *                            POL_CONST for policy#

     *========================================================================*
     * Main                                                                   *
     *========================================================================*
     C                   If        TestNum = *blank
     C                   Eval      @RtnCode = ValKey( tsCompany#:tsPolicy#)
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
     * Parameters :  3.0 - Company#                                           *
      *              13   - Policy  #                                          *
      *                                                                        *
      * Returns    :  1 a - Error code                                         *
     *========================================================================*
     P ValKey          B

     **** Procedure Interface ***
     D ValKey          PI             1a
     D  Company#                      3  0 Const
     D  policy#                      13    Const

     D RtnVal          s              1
     D field           s             20
     D kkid02          s              2     inz('02')

     **** KList Definitions ***
    C***************
    C***************
    C*
    C** Policy file
     C     polky         klist
     C                   kfld                    Company#
     C                   kfld                    policy#
    C*
    
     **** Procedure Definition ***
    C*

     ** Validate Policy Number
     C                   if        *in90 = *off
     C     polky         chain     ppm01                              63
     C                   if        *in63 = *on
     C                   move      *on           *in90
     C                   eval      field = POL_CONST
     C                   endif                                                  *in63=*on
     C                   endif                                                  *in90=*on
    C*
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
      *
     C                   if        q1prms > 2
     C                   eval        @RtnField = *blank
     C                   endif
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
      *           *** Company# ***
     C                   TestN                   Company#             21
     C                   If        *in21 = *on
     C                   Move      Company#      tsCompany#
     C                   Else
     C
     C                   if        q1prms > 2
     C                   eval        @RtnField = CO#_CONST
     C                   endif
     C
     C                   Return    '1'
     C                   EndIf
    
     C                   eval      tsPolicy#     = Policy#

    
     C                   Return    *blank

     P TestNum         E
