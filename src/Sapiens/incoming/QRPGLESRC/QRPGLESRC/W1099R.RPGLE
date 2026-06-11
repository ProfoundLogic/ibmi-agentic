     H** =========================================================== **
     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H** =========================================================== **
     h/title w1099r - create 1099'S, REPORTS AND WORK FILE - PRINT PGM
     H option(*nodebugio: *srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H debug(*yes)
     H** =========================================================== **
     H*  PROGRAMMER NOTE:                                             *
     H*                                                               *
     H*    Printer files W1099O(3-5) are being used in place of       *
     H*    QSYSPRT(3-5) in order to accomodate for printer alignment  *
     H*    at individual clinet sites.  If an adjustment needs to be  *
     H*    made for a particular client, copy W1099O3 inot their      *
     H*    environment.  Make the necessary changes to W1099O3, build *
     H*    it and test.  When the changes have been completed, copy   *
     H*    W1099O3 to W1099O4 & 5 and build them.  Install only the   *
     H*    Printer files at the client site.  W1099O3, O4 & O5 ard    *
     H*    identical.                                                 *
     H*                                                               *
     H*    * * * IMPORTANT NOTE * * *                                 *
     H*      If fields are being added or subtracted from the printer *
     H*      file, this RPG will have to be copied into the client    *
     H*      Environment, changed there, built and sent to the client *
     H*      along with the printer files.  Again, make changes to    *
     H*      W1099O3, build, test and copy W1099O3 to W1099O4 & 5.    *
     H** =========================================================== **
     H*  CALLED FROM:                                                 *
     H*         W1099C - Create Workfile and Report - OVERRIDE        *
     H*  CALLS TO:                                                    *
     H*         NONE                                                  *
     H*                                                               *
     H** =========================================================== **
     H*  Rewrite: 09/17/02   C:132  PROJECT: WC1096                   *
     H** =========================================================== **
     H* REVISIONS:                                                    *
     H*                                                               *
     h*  03/04/24  C:291 Project: 140WCE                              *
     h*                  Call W1099RTP to populate files for UBIC     *
     H*                  Third Party Vendor.                          *
     H*                                                               *
     h*  02/06/24  C:079 Support: 115040                              *
     h*                  Remove revision 4/29/22                      *
     H*                                                               *
     h*  11/09/22  C:062 Project WC2890                               *
     h*                  Add year to print.                           *
     H*                                                               *
     h*  04/29/22  C:062 Ticket: 101249(114)                          *
     h*                  Override for each group.                     *
     H*                                                               *
     h*  01/25/22  C:062 Project: WC2833                              *
     h*                  Correct B record format.                     *
     H*                                                               *
     h*  12/22/21  C:291 Project: WC2825                              *
     h*                  Skip VPay Payment from 1099 Reporting        *
     H*                                                               *
     h*  10/14/21  C:062 Project: WC2833                              *
     h*                  Print NEC form three times per page
     H*                                                               *
     h*  01/11/21  C:062 Project: WC2750(93619(111))                  *
     h*                  Add MPE changes
     H*                                                               *
     h*  01/11/21  C:062 Project: WC2750(93596(114))                  *
     h*                  Add 1099-NEC Print                           *
     h*                                                               *
     h*  10/08/20  C:062 Project: WC2750                              *
     h*                  Add 1099-NEC Print                           *
     h*                                                               *
     h*  12/06/19  C:079 Project: 129WVK                              *
     h*                  Add Vocational Rehab Paid Under Comp field   *
     H*                                                               *
     H*  10/02/19  C:062 Project 114WPM                               *
     H*                  Use 1099 Fein Override if set.               *
     H*                                                               *
     H*  03/30/17  C:180 SUPPORT 46449                                *
     H*                  Restore logic for box 14: PV1 code with      *
     H*                  placement value 'U' and payee type '5'       *
     H*                                                               *
     H*  01/29/16  C:300 SUPPORT: 39519 (xrefn 33062)                 *
     H*                  1099 processing for CareWest                 *
     H*                  second vendor entry prints a box below       *
     H*                  and Field 14 (XPMTC) got out of alignment    *
     H*                ++Added 33062 changes from V15R3MC to V16R0    *
     H*                                                               *
     H*  03/20/15  C:279 Support: 33062 (Phaseware)                   *
     H*                  Enable support 81524 changes for CareWest.   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H*  09/11/15  C:295 Project: WC2362                              *
     H*                - 1099 Process include Manual Entries          *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
    H*                                                               *
     H*  03/30/15 C:062 SUPPORT: 33934 (129)                          *
     H*                 Initialize fields for manual 1099s.           *
     H*                                                               *
     H*  02/27/15  C:180 SUPPORT 33513                                *
     H*                  Fix position of box 7                        *
     H*                                                               *
     H*  01/20/15-BC:180 SUPPORT: 32771                               *
     H*                  Fix page overflow.                           *
     H*                                                               *
     H*  01/20/15  C:283 SUPPORT: 32794                               *
     H*                  Support 81524 positing of the text printing  *
     H*                  on the lower page should be moved down       *
     H*                  3 lines only for LCTA 045.                   *
     H*                                                               *
     H*  02/01/13  C:283 SUPPORT: WC045 81524                         *
     H*                  For Re Compilation Only.                     *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY - Added activation groups   *
     H*  02/25/11  C:246 Project: WC1981/Support# 75873               *
     H*                - As per revised IRS 1099 form postion 26 and  *
     H*                  27 are changed to have type of return with   *
     H*                  2 length (i.e) 26 and 27 position will be 1  *
     H*                  field with 2 char.                           *
     H*                  This field should have a 1 char value of     *
     H*                  type of return in 26th postion               *
     H*                  type of return and blank on postion 27.      *
     H*                                                               *
     H*  01/07/11  C:246 Project: WC1981/Support# 75358               *
     H*                - As per revised IRS 1099 form Gross Attorney  *
     H*                  fee should be reported more than $600 Box14. *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:283  Retrieve brand Name                          *
     H*                                                               *
     H*  12/18/07  C:132 Project: 90WFT                               *
     H*                - If client 090 and Group# is 187, include     *
     H*                  payments if transaction date is equal or     *
     H*                  greater than 07/01/2007.                    *
     H*                - If client 090 and Group# is 615, include     *
     H*                  payments if transaction date is equal or     *
     H*                  greater than 06/04/2007.                    *
     H*                                                               *
     H*  10/31/07  C:132 Project: WC1640                              *
     H*                - Drop fields from "A" record: AOFIND, ARFIND, *
     H*                  ACFIND.                                      *
     H*                - Expand AFILL2 from 6 to 10 Positions.        *
     H*                                                               *
     H*  08/02/07  C:132 Project: WC1491                              *
     H*                - PID Return Funds to include Deposti and      *
     H*                  Independent Cash Distribution to Agents.     *
     H*                                                               *
     H*  08/22/07  C:33  Project: 114WMF                              *
     H*                  Back to Base                                 *
     H*        11/13/02  PROJECT: WC1096 - 114WDB, 114WCZ changes     *
     H*                  Add changes from 01/14/02 114WDB not to load *
     H*                  payment voucher. Add changes from 01/03/02   *
     H*                  114WCZ for Attorney Comp Payments to report  *
     H*                  all. If Payment Reserve Type is a "C" and    *
     H*                  Provider type = "5", then report amounts.    *
     H*                  These applied to re-written RPGLE base pgm.  *
     H*                  Both dates will appear in added changes.     *
     H*                                                               *
     H*  01/11/06  C:015 Project: 135WFV                              *
     H*                  Use the COFEIN instead of F2FEIN.            *
     H*                                                               *
     H*  02/03/04  C:132 WC081  SEQ: 41149                            *
     H*                - If last Provider was SS#, Indicator 51       *
     H*                  remains on, and if have manual records will  *
     H*                  use this last provider SS#, unless hit a SS# *
     H*                  in the manuals, then will use new SS# unitl  *
     H*                  end of file. - Corrected to use FEINs        *
     H*                                                               *
     H*  01/29/04  C:132 WC065  SEQ: 41045                            *
     H*                - If provider mail to record not found use the *
     H*                  record being processs for name & address     *
     H*                                                               *
     H*  01/27/04  C:132 WC065  SEQ: 41045                            *
     H*                - Second address line carried over from prior  *
     H*                  record if current record had DBA flag = "Y"  *
     H*                                                               *
     H*  01/08/04  C:132 WC1157     Revisions for 2003 Tax Year       *
     H*                - Load Constant in BPACT "B" record Payers     *
     H*                  Acct # for PAYEE - any number for ID         *
     H*                                                               *
     H*  10/16/03  C:132 WC1157     Revisions for 2003 Tax Year       *
     H*                - New fields in record types A, B, and C       *
     H*                  which are not used but must be output in     *
     H*                  WATNP file.                                  *
     H*                                                               *
     H*  10/10/03  C:132 WC1157     Revisions for 2003 Tax Year       *
     H*                - If have Gross Attorny Amts for Box 14, then  *
     H*                  print a 1099 form, is not subject to the     *
     H*                  $600 limit. Add New fields in B and C recrds *
     H*                                                               *
     H*  09/24/03  C:132 WC1153     Revisions for 2003 Tax Year       *
     H*                - Check for Negative amount in any box field,  *
     H*                  if so, print on error report (QSYSPRT1) and  *
     H*                  zero amount before printing on 1099 form.    *
     H*                - Total all amounts, then check against the    *
     H*                  IRS limit from SMCOL/CO1099 field, print if  *
     H*                  if over amount.                              *
     H*                                                               *
     H*  02/26/03  C:132 WC072 Seq#: 36145                            *
     H*                  Move Check on Deductible codes to BT5RCD     *
     H*                  subr, so still build records if Ded record   *
     H*                  is last one read for provider.               *
     H*                                                               *
     H*  01/29/03  C:132 WC055 Seq#: 35710                            *
     H*                  Chg check on reserve types for box 14. crt   *
     H*                  1099 for all PDTYPE = "5". Put amounts in    *
     H*                  BOX 14 for all except B2PLCD of "M". The "M" *
     H*                  types go into BOX 7.                         *
     H*                                                               *
     H*  01/14/03  C:132 WC048 Seq#: 35511                            *
     H*                  Chg to get correct 'Mail To" Agent MOD info  *
     H*                  for print and file name & address fields.    *
     H*                                                               *
     H*  01/03/03  C:132 WC065 Seq#: 35326                            *
     H*                  Chg to check medical payments to see if go   *
     H*                  in BOX 14. Changed to print multiple groups  *
     H*                  on the Routing page instead of the reports   *
     H*                                                               *
     H*  12/31/02  C:132 WC065 Seq#: 35302                            *
     H*                  Changed arrays to handle 999 groups instead  *
     H*                  of 20. NOTE: sent to 065 with 99 Groups on   *
     H*                  12/31/02 before increased to 999 groups      *
     H*                                                               *
     H*  12/23/02  C:132 WC048 Seq#: 35257                            *
     H*                  Change Pinfeed Box 14 location from 72 to 93 *
     H*                  Change to set off flag for box 14 before     *
     H*                  process next record. Was only setting off    *
     H*                  when using subroutine CHKLGL.                *
     H*                                                               *
     H*  11/05/02  C:132 SEQ#: WC1096                                 *
     H*                  Change WPAL to WPALF logical file & check    *
     H*                  transaction date in tax year range. Add      *
     H*                  code to print all groups in headings.        *
     H*                                                               *
     H*  09/17/02  C:132 SEQ#: WC1096                                 *
     H*                  Converted from RPG III to RPG ILE            *
     H*                                                               *
     H*                  Changes to accumulate amounts for like IRS   *
     H*                  FEIN numbers across Groups. Records will be  *
     H*                  written to an intemediate workfile which     *
     H*                  will be processed after all pay records are  *
     H*                  have been processed.                         *
     H*                  The Provider records will be accumulated by  *
     H*                  by the Payee FEIN, and not by modifier when  *
     H*                  writing into the WXT6P workfile              *
     H*                                                               *
     H*                  Manual 1099s from WATNPB will be accumulated *
     H*                  by MOD and printed by FEIN number. These     *
     H*                  will not be put in the workfile WXT6P file   *
     H*                  and will processed immediatly.               *
     H*                                                               *
     H*                  Changes for Projects WC1095 and WC967 have   *
     H*                  been included in this change.                *
     H*                                                               *
     H*                  SEE RPG III code for prior changes           *
     H*                                                               *
     H** =========================================================== **
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  81  -  PINFEED PRINTER                                       *
     H*  82  -  LASER PRINTER                                         *
     H** =========================================================== **
     fSMORL     IF   E           K DISK
     fSMCOL     IF   E           K DISK
      *
     fWATNP     O  A F  750        DISK
     f*** Begin Add *** 01/11/21 ************************************
     fWATNPNEC  O  A F  750        DISK
     f*** End   Add *** 01/11/21 ************************************
     fWATNPBL   IF   E           K DISK
      *
     fWDPALF    IF   E           K DISK
     fWMUPL     IF   E           K DISK
     fWDF2L     IF   E           K DISK
     fWMAAL1    IF   E           K DISK
     fWMAGL     IF   E           K DISK
     fWMCML1    IF   E           K DISK
     f                                     RENAME(PCM01:PCM02)
     fWMCML2    IF   E           K DISK
     fWMCDL     IF   E           K DISK
     fWMFNL     IF   E           K DISK
     fWMPDL     IF   E           K DISK
      *
      * Ignore all the table Record IDs that are not used
     fWTABL     IF   E           K DISK    IGNORE(PAT01)
     f                                     IGNORE(PBA01)
     f                                     IGNORE(PBC01)
     f                                     IGNORE(PBS01)
     f                                     IGNORE(PB701)
     f                                     IGNORE(PCA01)
     f                                     IGNORE(PCC01)
     f                                     IGNORE(PCN01)
     f                                     IGNORE(PCS01)
     f                                     IGNORE(PHY01)
     f                                     IGNORE(PID01)
     f                                     IGNORE(PIS01)
     f                                     IGNORE(PLR01)
     f                                     IGNORE(PNA01)
     f                                     IGNORE(PNC01)
     f                                     IGNORE(PPA01)
     f                                     IGNORE(PPC01)
     f                                     IGNORE(PRC01)
     f                                     IGNORE(PRS01)
     f                                     IGNORE(PRT01)
     f                                     IGNORE(PSP01)
     f                                     IGNORE(PSY01)
     f                                     IGNORE(PTR01)
     f                                     IGNORE(PTT01)
     f                                     IGNORE(PVS01)
      *
      * Agent Commission Work File.
     fWXACL     IF   E           K DISK
     fWXT5P     IF   E           K DISK                                         Input Group Selects
     fWXT6P     UF A E           K DISK                                         Intermediate B Recds
     fWSCKL     IF   E           K DISK
      *
      * Return Premium Check Request File
     fWAR4L8    UF A E           K DISK
      *
      * Print output files
     fQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     fQSYSPRT1  O    F  198        PRINTER OFLIND(*INOB)
     fQSYSPRT2  O    F  127        PRINTER OFLIND(*INOC)
     f                                     USROPN
     fQSYSPRT6  O    F  102        PRINTER OFLIND(*INOG)
     f                                     USROPN
     fW1099O3   O    E             PRINTER OFLIND(*IN01)
     f                                     USROPN
     f                                     RENAME(F10991:F51101)
     f                                     RENAME(F10992:F51102)
     f                                     RENAME(F1099CLT:F5110CLT)
     fW1099O4   O    E             PRINTER OFLIND(*IN02)
     f                                     USROPN
     f                                     RENAME(F10991:F51111)
     f                                     RENAME(F10992:F51112)
     f                                     RENAME(F1099CLT:F5111CLT)
     fW1099O5   O    E             PRINTER OFLIND(*IN03)
     f                                     USROPN
     f                                     RENAME(F10991:F51121)
     f                                     RENAME(F10992:F51122)
     f                                     RENAME(F1099CLT:F5112CLT)
     f*** Begin Add *** 10/08/20 *******************************************
     f*** Begin Chg *** 10/14/21 *******************************************
     f***W1099ONEC3O    E             PRINTER OFLIND(*IN04)
     f**********                           USROPN
     f**********                           RENAME(F10991NEC:F51101NEC)
     f**********                           RENAME(F10992NEC:F51102NEC)
     f**********                           RENAME(F1099CLTn:F5110CLTN)
     f***W1099ONEC4O    E             PRINTER OFLIND(*IN05)
     f**********                           USROPN
     f**********                           RENAME(F10991NEC:F51111NEC)
     f**********                           RENAME(F10992NEC:F51112NEC)
     f**********                           RENAME(F1099CLTN:F5111CLTN)
     f***W1099ONEC5O    E             PRINTER OFLIND(*IN06)
     f**********                           USROPN
     f**********                           RENAME(F10991NEC:F51121NEC)
     f**********                           RENAME(F10992NEC:F51122NEC)
     f**********                           RENAME(F1099CLTN:F5112CLTN)
     fW1099ONEC3O    E             PRINTER OFLIND(*IN04)
     f                                     USROPN
     f                                     RENAME(F10991NEC:F51101NEC)
     f                                     RENAME(F10992NEC:F51102NEC)
     f                                     RENAME(F10993NEC:F51103NEC)
     fW1099ONEC4O    E             PRINTER OFLIND(*IN05)
     f                                     USROPN
     f                                     RENAME(F10991NEC:F51111NEC)
     f                                     RENAME(F10992NEC:F51112NEC)
     f                                     RENAME(F10993NEC:F51113NEC)
     fW1099ONEC5O    E             PRINTER OFLIND(*IN06)
     f                                     USROPN
     f                                     RENAME(F10991NEC:F51121NEC)
     f                                     RENAME(F10992NEC:F51122NEC)
     f                                     RENAME(F10993NEC:F51123NEC)
     f*** End   Chg *** 10/14/21 *******************************************
     f*** End   Add *** 10/08/20 *******************************************
     f*
     d*
     d RtvCltID        PR                  extpgm('RTVCLTID')
     d  cltID                         3
      *** Beg Add ***** 03/04/24 ****************************
     d ThirdPartyFile  PR                  extpgm('W1099RTP')
     d  ##Year                        4
      *** End Add ***** 03/04/24 ****************************
      * Change Grps Varible to Nbr grp Array Elements in *INZSR Subr
     d grp             S              3  0 DIM(999)                             Group Numbers
     d gnm             S             60    DIM(999)                             Group Names
      *  Payer's FEIN Edit Word
     d                 DS
     d  APRIN1                 1     10
     d  APEONE                 1      2
     d  APEHYP                 3      3
     d  APETWO                 4     10
     d*  FEIN Edit Word
     d                 DS
     d  BFEIN1                 1     10
     d  BFEONE                 1      2
     d  BFEHYP                 3      3
     d  BFETWO                 4     10
     d*  ZIPCODE Edit word
     d                 DS
     d  ##zipc                 1     10
     d  ##zip5                 1      5
     d  ##zhyp                 6      6
     d  ##zip4                 7     10
     d*  SocSec Edit Word
     d                 DS
     d  BFSS#1                 1     11
     d  BFSONE                 1      3
     d  BF1HYP                 4      4
     d  BFSTWO                 5      6
     d  BF2HYP                 7      7
     d  BFSTHR                 8     11
     d*
     d                 DS
     d  DSDTTM                 1     14  0
     d  DSTIME                 1      6  0
     d  DSDATE                 7     14  0
     d*
     d TITLE           C                   CONST('Report and Workfile -
     d                                     Build for IRS 1099''-
     d                                     s and Magnetic Media')
     d*
     d PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     d ##DATA          S             80
     d ##LENG          S              3  0
     d ##SS#           S              9
     d AAIND           S             14
     d*** Begin Add *** 01/11/21 ************************************
     d AAINDNEC        S             14
     d*** End   Add *** 01/11/21 ************************************
     d AFILL2          S             10
     d ACFIND          S              1
     d ACRLF           S              2
     d AFCOI           S              1
     d AFILL1          S              6
     d AFILL3          S            100
     d AFILL4          S              9
     d ALFIN           S              1
     d AOFIND          S              1
     d APCTY           S             40
     d APPH#           S             13
     d APPHN#          S             15
     d APRAD           S             40
     d APRAD2          S             40
     d APRCS           S             40
     d APRIN           S              9
     d APRNC           S              4
     d APRN1           S             40
     d APRN2           S             40
     d APST            S              2
     d APTYR           S              4
     d APZIP           S              9
     d ARFIND          S              1
     d ARTTP           S              2
     d*** Begin Add *** 01/11/21 ************************************
     d ARTTPNEC        S              2
     d*** End   Add *** 01/11/21 ************************************
     d ARTYP           S              1
     d ATCC            S              5
     d ATRAG           S              1
     d BCRLF           S              2
     d BCRTN           S              1
     d BFCIN           S              1
     d #FEIN           S              9
     d BFEIN           S              9
     d BFILL1          S             10
     d*** Begin Chg *** 01/25/22 *******************************************
     d*BFILL2***       S             24
     d BFILL2          S             16
     d*** End   Chg *** 01/25/22 *******************************************
     d BFILL3          S             40
     d*** Begin Chg *** 01/25/22 *******************************************
     d*BFILL4***       S             45
     d BFILL4          S             36
     d*** End   Chg *** 01/25/22 *******************************************
     d BFILL5          S            119
     d BFILL6          S             26
     d*** Begin Add *** 01/25/22 ******************************************
     d BFILL7          S              1
     d BSEQ#           S              8
     d BSEQ#NEC        S              8
     d*** End   Add *** 01/25/22 ******************************************
     d BINTP           S              1
     d BNCTL           S              4
     d BPACT           S             20
     d BPCTY           S             40
     d BPMTA           S             12
     d BPMTB           S             12
     d BPMTC           S             12
     d*** Begin Add *** 01/11/21 ************************************
     d BPMTC0          S             12
     d*** End   Add *** 01/11/21 ************************************
     d BPMTD           S             12
     d BPMTE           S             12
     d*** Begin Chg *** 01/25/22 *******************************************
     d BPMTF           S             12
     d BPMTG           S             12
     d BPMTH           S             12
     d BPMTJ           S             12
     d*** End   Chg *** 01/25/22 *******************************************
     d BPMT1           S             12
     d BPMT2           S             12
     d BPMT3           S             12
     d*** Begin Add *** 01/11/21 ************************************
     d BPMT30          S             12
     d*** End   Add *** 01/11/21 ************************************
     d BPMT4           S             12
     d BPMT5           S             12
     d BPMT6           S             12
     d*** Begin Add *** 01/11/21 ************************************
     d BPMT60          S             12
     d*** End   Add *** 01/11/21 ************************************
     d BPMT7           S             12
     d*** Begin Add *** 01/11/21 *******************************************
     d BPMT70          S             12
     d*** End   Add *** 01/11/21 *******************************************
     d BPMT8           S             12
     d BPMT9           S             12
     d BPNM1           S             40
     d BPNM2           S             40
     d BPOFC           S              4
     d BPRAD           S             40
     d BPRA2           S             40
     d BPST            S              2
     d BPTYR           S              4
     d BPZIP           S              9
     d BRTYP           S              1
     d BSTCD           S             60
     d BXCSZ           S             40
     d BYCSZ           S             40
     d C#PAE           S              8
     d*** Begin Add *** 01/11/21 ************************************
     d C#PAENEC        S              8
     d*** End   Add *** 01/11/21 ************************************
     d CCRLF           S              2
     d CFILL1          S              6
     d CFILL2          S            100
     d CFILL3          S             17
     d COUNTB          S              8  0
     d*** Begin Add *** 01/11/21 ************************************
     d COUNTBNEC       S              8  0
     d*** End   Add *** 01/11/21 ************************************
     d CRTYP           S              1
     d CTOTA           S             18
     d CTOTB           S             18
     d CTOTC           S             18
     d*** Begin Add *** 01/11/21 ************************************
     d CTOTC0          S             18
     d*** End   Add *** 01/11/21 ************************************
     d CTOTD           S             18
     d CTOTE           S             18
     d*** Begin Add *** 01/25/22 ************************************
     d CTOTF           S             18
     d CTOTG           S             18
     d CTOTH           S             18
     d CTOTJ           S             18
     d*** End   Add *** 01/25/22 ************************************
     d*** Begin Chg *** 01/25/22 ************************************
     d*CFILLB***       S             64
     d CFILLB          S             92
     d*** End   Chg *** 01/25/22 ************************************
     d CTOT1           S             18
     d CTOT2           S             18
     d CTOT3           S             18
     d*** Begin Add *** 01/11/21 ************************************
     d CTOT30          S             18
     d*** End   Add *** 01/11/21 ************************************
     d CTOT4           S             18
     d CTOT5           S             18
     d CTOT6           S             18
     d*** Begin Add *** 01/11/21 ************************************
     d CTOT60          S             18
     d*** End   Add *** 01/11/21 ************************************
     d CTOT7           S             18
     d*** Begin Add *** 01/11/21 *******************************************
     d CTOT70          S             18
     d*** End   Add *** 01/11/21 *******************************************
     d CTOT8           S             18
     d CTOT9           S             18
     d F1099#          S              3  0
     d*** Begin Add *** 10/08/20 *******************************************
     d F1099#N         S              3  0
     d*** End   Add *** 10/08/20 *******************************************
     d includePayment  s              1n
     d N               S              3  0
     d QQCO#           S              3
     d QQCOMM          S              1
     d QQFND           S              3
     d QQINCL          S              1
     d QQPOFL          S              1
     d QQYEAR          S              4
     d*** Begin Add *** 11/09/22 *******************************************
     d rptYear         S              2
     d rptYearMPE      S              2
     d*** End   Add *** 11/09/22 *******************************************
     d SRDTI           S              8  0
     d SRDTO           S             10
     d SRERR           S              1
     d SRFFMT          S              8
     d SRTFMT          S              8
     d TYPE3T          S             18  2
     d WKA06           S              6
     d WKA09           S              9
     d X               S              3  0
     d XBOX14          S              1
     d XPMTC           S             12  2
     d XPMTC_CLT       S             12  2
     d XPMT3           S             12  2
     d XPMT6           S             12  2
     d XPMT7           S             12  2
     d XXCLNM          S             40
     d XXCSZ           S             40
     d XXPCSZ          S             40
     d XXRCSZ          S             40
     d XXCTY           S             25
     d XXC003          S              3
     d XXC004          S              4
     d XXC007          S              7
     d XXDATA          S             60
     d XXFEIN          S              9  0
     d XXFEIN1         S             11
     d ##FEIN          S              9  0
     d XXFRST          S             15
     d XXLAST          S             56
     d XXMBR           S             10    inz
     d XXMOD           S              3  0
     d XXRADR          S             40
     d XXSTZP          S             13
     d XXST10          S             10
     d XXTAMT          S             13  2 inz
     d XXTERM          S              1
     d XXYEAR          S              4  0
     d XXZIP4          S              4
     d XXZIP5          S              5
     d XX5             S              5
     d X1ADDR          S             40
     d X1ADD2          S             40
     d X1CSZ           S             40
     d X1NAME          S             40
     d X2ADDR          S             40
     d X2ADD2          S             40
     d X2CSZ           S             40
     d X2NAME          S             40
     d ZIP2            S              4
     d ZZCO#           S              3
     d ZZFEIN          S              9
     d ZZPMT3          S             12  2 inz
     d ZZPMT6          S             12  2 inz
     d ZZPMT7          S             12  2 inz
     d xxprt           s                   like(orfil#)
     d xxco#           s                   like(paco#)
     d xxfnd           s                   like(pafnd)
     d xtfnd           s                   like(pafnd)
     d xxyr            s                   like(tnptyr)
     d xxmamt          s                   like(xxtamt)
     d xxamt3          s                   like(xxtamt)
     d xxamt6          s                   like(xxtamt)
     d xxamt7          s                   like(xxtamt)
     d xxamtc          s                   like(xxtamt)
     d xxTAMTS         s                   like(t6amtc)
     d type6t          s                   like(type3t)
     d type7t          s                   like(type3t)
     d typect          s                   like(type3t)
     d xxincl          s                   like(qqincl)
     d svfein          s                   like(xxfein)
     d svmod           s                   like(xxmod)
     d xxtcc           s                   like(t5tcc)
     d xxkey           s                   like(b2key)
     d xapfdt          s              8    inz
     d xaptdt          s              8    inz
     d xxpfdt          s              7  0 inz
     d xxptdt          s              7  0 inz
     d zztrdt          s              8    inz
     d xxtrdt187       s              7  0 inz
     d xxtrdt615       s              7  0 inz
     d xxCKDT          s              7  0 inz
     d proces          s                   like(*in60)
     d ##mod           s                   like(pamod)
     d ##rtyp          s              1
     d grps            s              3  0 inz
     d grp#s           s              3  0 inz
     d Z               S              3  0 inz
     d cltID           s              3
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
      *_______________________________________________________________
     c                   callp     RtvCltID(cltID)
      /Free
       // If cltID = '045';

       If cltID = '045' or cltId = '136';
          *In52 = *On;
       Else;
          *In52 = *Off;
       Endif;
       //*** Begin Add *** 01/11/21 ****************************************
       // If cltID = '111';
       *In53 = *Off;
       If cltID = '111';
          *In53 = *On;
       Endif;
       //*** End   Add *** 01/11/21 ****************************************
      /End-Free
      *
      *  Chain to the Company File
     c     xxco#         chain     pco01                              60
      *
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
      *  Chain to the Incoming Work file WXT5P to get First group
      *  number and the Transmitter Control Code (T5TCC)
     c     t5ky          chain     pt501                              60
     c                   if        *in60 = *on
     c                   goto      TERM
     c                   ELSE
     c                   move      t5fnd         xxfnd
     c                   move      t5tcc         xxtcc
     c                   end
      *
      *  Chain to the Fund Master File for group demographic info
     c     fnky          chain     pfn01                              60
      *
      *  Get T5 Record to Retrieve First TAXID and TCC #
     c     f2ky          chain     pf201                              60
     C                   if        cltID = '135'
     C                   eval      F2FEIN = COFEIN
     C                   endif
     C                   If        F21099Fein <> 0
     C                   Eval      F2fein = F21099Fein
     C                   EndIf
      *
     c* Load arrary with Group number and name
     c                   z-add     1             grp#s
     c                   z-add     1             z
     c                   move      fnfnd         grp(z)
     c                   movel(p)  fnname        gnm(z)
      *
     c                   exsr      sr1st
      *
     c                   if        (f2fein <= *zero) or
     c                             (f2tcc = *blanks)
     c                   except    err2
     c                   endif                                                  F2FEIN<=*ZERO
      *
      *  Assign Indicator for PINFEED or LASER Printer
     c     qqpofl        comp      'P'                                    81    81 = Pinfeed
     c     qqpofl        comp      'L'                                    82    82 = Laser
      *
      *  Open Appropriate Print files
     c                   if        (*in81)
     c                   open      qsysprt2
     c                   endif                                                  *IN81=*ON
      *
     c                   if        (*in82)
     c                   open      w1099o3
     c                   open      w1099o4
     c                   open      w1099o5
     c*** Begin Add *** 10/08/20 *******************************************
     c                   open      w1099oNEC3
     c                   open      w1099oNEC4
     c                   open      w1099oNEC5
     c*** End   Add *** 10/08/20 *******************************************
     c                   open      qsysprt6
     c                   except    label1
     c                   endif                                                  *IN82=*ON
      *  _______________________________________________________________________
      *
     c                   if        (qqcomm = 'P') or                            P = Providers Only
     c                             (qqcomm = 'B')                               B = Providers+Agents
      *
     c     NXTGRP        TAG
      *
      *  Get ALL Provider payments & Write a "B" record for Each
     c     paky          chain     ppa01                              60
      *
     c                   eval      ##mod = *zeros
     c                   eval      ##rtyp = 'P'                                 P = Provider Code
      *
     c                   dow       (not *in60)

      *** Beg Add ***** 12/22/21 *****************************
      * Skip VPay Payments
     c                   If        PAvtyp = 'CLVCP'
     c                   Goto      Nxtrcd
     c                   Endif
      *** End Add ***** 12/22/21 *****************************
      *
      *  If Trans date not in year range, bypass and get another record
      *
     c                   if        patrdt < xxpfdt  OR patrdt > xxptdt
     c                   goto      Nxtrcd
     c                   endif
      *
      * 90WFT - Check for transactions to be bypassed.
     c                   if        CltID  = '090'
      *
     c                   IF        xxfnd  = 187 AND patrdt < xxtrdt187          bef 07/01/2007
     c                   goto      NxtRcd
     c                   EndIf
     c                   IF        xxfnd  = 615 AND patrdt < xxtrdt615          bef 06/04/2007
     c                   goto      NxtRcd
     c                   EndIf
      *
     c                   endif                                                  End CltID = 090
      *
      *  Only Include 'C' Type Payments in 1099 Calculations if made to Attorney
      *  and bypass all records with a zero FEIN number
     c                   move      *off          proces
     c                   move      *blank        xbox14
      *
      * Also check Medical payments for Legal into box 14
     c                   if        (paretp = 'M') AND (pafein > *zeros)
     c                   exsr      chklgl
     c                   endif                                                  Paretp=M
      *
     c                   if        (paretp = 'C')
     c                   exsr      chklgl
     c                   else
     c                   if        pafein > *zeros
     c                   move      *on           proces
     c                   endif
     c                   endif                                                  PARETP=C
      *
     c                   if        (proces = *on)
      *
      *
      *  Do nt include if Payment is a voucher, for CLT114
     c     ckky          chain     pck01                              99
     c   99              eval      ckvchr = *blanks
      *
     C                   select
     C                   when      cltID = '114' and ckvchr = 'Y'
     C                   eval      includePayment = *off
     C                   other
     C                   eval      includePayment = *on
     C                   endsl
     C*
     C                   if        includePayment
      *
      *  If Provider break, Write "B" Record, Reload Hold Field, Write Detail Line
     c                   if        (pafein <> xxfein)
     c                   move      pafein        xxfein
     c                   exsr      pbrk
      *
      *  Determine if this record is to be Processed
     c                   move      *off          *in69
     c     pdky          chain     ppd01                              62
     c                   if        (*in62)
     c                   move      pafein        ##ss#
     c     cmky          chain     pcm01                              66
      *
      *  PAYEE Does NOT Exist in Payee Master File
     c                   if        (*in66)
     c                   move      *on           *in69
     c                   endif                                                  *IN66=*ON
      *
      *  Set the 1099 Flag off first, then check after chain to file
     c                   move      'N'           pd1099
      *
     c                   endif                                                  *IN62=*ON
     c                   endif                                                  PAFEIN<>XXFEIN
      *
     c                   if        (*in69)
     c                   move      *blank        xxclnm                         CLEAR CLMANT NAME
     c     cmky1         chain     pcm02                              54
      *
     c                   if        (not *in54)
      *
     c     cdky          chain     pcd01                              55
     c                   if        (not *in55)
      *
     c                   if        cdfsn = *blanks
     c                   movel     cdlsn         xxclnm
     c                   else
     c                   eval      xxclnm = %trimr(cdlsn) + ', ' + %trimr(cdfsn)
     c                   endif
      *
     c                   else
      *
     c                   move      *blank        xxclnm
      *
     c                   endif                                                  *IN55=*OFF
     c                   endif                                                  *IN54=*OFF
      *
     c                   if        (*INOB)
     c                   except    head1
      *
     c                   move      *off          *INOB
     c                   endif                                                  *INOB=*ON
      *
     c                   except    err1
      *
     c                   endif                                                  *IN68=*ON
      *
      *  1099 Flag must be a 'Y' (YES) in the Payee Master File to process a 1099
     c                   if        (pd1099 = 'Y')
      *
     c                   eval      ##fein = pafein
     c                   exsr      BT6Rcd                                       Write WXT6P Rcd
      *
     c                   endif                                                  PD1099=Y
      *
      *
     c                   endif                                                  Voucher <> 'Y'
      *
     c                   endif                                                  PROCESS=ON
      *
     C     Nxtrcd        tag
      *
     c     paky          reade     ppa01                                  60
     c                   enddo                                                  *IN60=*OFF
      *
      * Check Group Selection File for more Groups, if so go back and process
      *  se *IN80, so if get hit will reset Provider Information
     c     t5ky          reade     pt501                                  80
     c                   if        *in80 = *off
     c                   move      t5fnd         xxfnd
     c                   move      t5tcc         xxtcc
      *  Check and clear unused group members.
     c                   eval      xxMBR = ('IRS' + zzco# + t5fnd + ' ')
     c                   call      'W1099C5'                            99
     c                   parm                    xxMBR
      *
     c                   goto      nxtgrp
     c                   endif
      *
      *  Write the Workfile Records in WXT6P if only providers wanted
      *
     c                   if        qqcomm = 'P'
     c                   exsr      RcdBsr
     c                   endif
      *
     c                   endif                                                  QQCOMM=P/B
      *_______________________________________________________________
      *
      *  Get Agent Commission Payments if Requested
      *_______________________________________________________________
      *
     c                   if        (qqcomm = 'A') or
     c                             (qqcomm = 'B')
      *
      *  Chain to the Incoming Work file WXT5P to get First group
     c     t5ky          chain     pt501                              60
     c                   if        *in60 = *on
     c                   goto      TERM
     c                   ELSE
     c                   move      t5fnd         xxfnd
     c                   move      t5tcc         xxtcc
     c                   end
      *
     c                   eval      xxtamt = *zero
     c                   eval      xxmamt = *zero
     c                   move      *zero         bpmt3
     c                   move      *zero         bpmt6
     c                   move      *off          *in69
     c                   eval      ##rtyp = 'A'
      *
     c     NXTAGT        TAG
      *
      *  Start Reading the Agent Commission file
     c     acky          chain     pac01                              60
      *
     c                   if        (*IN60 = *off)
     c                   eval      xxfein = acfein
     c                   eval      xxmod = acmod
     c                   exsr      abrk
     c                   endif
      *
     c                   dow       (not *in60)
      *
      *  Check for Agent Federal ID break.  All Modifiers within a Agent FEIN
      *  number will be Accumulated into a single record in WXT6P file.
      *  If the total amount for all Modifiers Exceeds the 1099 Amount then a single
      *  1099 will be created for the controlling Modifier in the Agent Master file.
      *
     c                   if        (acfein <> xxfein)
     c                   exsr      abrk
     c                   eval      xxfein = acfein
     c                   eval      xxmod = acmod
     c                   endif                                                  ACFEIN<>XXFEIN
      *
      *  Get WMAAP & WMAGP record to Determine if this Record is to be Processed
     c                   setoff                                       252669
      *
     c     aaky          chain     paa01                              62
     c                   move      *in62         *in25
     c                   move      *in62         *in69
      *
     c                   if        (not *in62)
     c     agky          chain     pag01                              62
     c                   move      *in62         *in26
     c                   move      *in62         *in69
     c                   endif                                                  *IN62=*OFF
      *
      *  If Error occurred, print EXCEPTION
      *
     c                   if        (*in69)
      *
     c                   if        (*INOB)
     c                   except    head1
     c                   move      *off          *INOB
     c                   endif                                                  *INOB=*ON
     c                   except    err3
     c                   setoff                                       2526
     c                   endif                                                  *IN69=*ON
      *
      *  Accumulate Agent Commission Payments                          IF 0)
     c                   if        (not *in69) and
     c                             (ag1099 = 'Y')
      *
     c                   select
     c                   when      (accotp = 'P')
     c                   eval      xxtamt = (xxtamt + acamt)
     c                   eval      xxmamt = (xxmamt + acamt)
     c                   when      (accotp = 'V')
     c                   eval      xxtamt = (xxtamt - acamt)
     c                   eval      xxmamt = (xxmamt - acamt)
     c                   endsl
      *
      * Now build/update the WXT6P record for the agent.
     c                   eval      ##fein = acfein
     c                   exsr      BT6Rcd
      *
     c                   endif                                                  *IN69=*OFF
      *
     c     acky          reade     pac01                                  60
     c                   enddo                                                  *IN60=*OFF
      *
      * Check Return of Premium work file for any agent amounts within this
      * Tax Year.  These are identified by code "A" in WAR4P/R4PTFL.
      *
      *  Check check date in this Tax Year. If so, check R4BTYP = an "I".
      *  IF R4BTYP = "I", and R4VDST = *BLANK, then add/update WXT6P file with amount.
      *  IF R4BTYP = "I", and R4VDST <> *BLANK, then check the maintenance date
      *  in the current Tax Year. If so, then create/update WXT6P with the amount as
      *  a negative amount, since it is a voided or stop pay record.
      *
     c     r4ky1         setll     pr401
     c     r4ky          reade     pr401                                  60
      *
     c                   eval      xxtamt = *zero
     c                   eval      xxmamt = *zero
     c                   move      *zero         bpmt3
     c                   move      *zero         bpmt6
     c                   move      *off          *in69
     c                   eval      ##rtyp = 'A'
      *
     c                   dow       (not *in60)
      *
     c                   if        R4PTFL = 'A' AND R4BTYP = 'I' AND
     c                             R4CKDT >= XXPFDT AND R4CKDT <= XXPTDT
     c*
     c                   if        R4VDSP =  *BLANKS OR                         PAYMENT
     c                             R4VDSP <> *BLANKS AND                        VOID OR STOP PAYMNT
     c                             R4DATE >= XXPFDT AND R4DATE <= XXPTDT
     c*
     c                   z-add     r4agt#        acagt#
     c                   z-add     r4fyr         acfyr
     c*
     c                   setoff                                       252669
     c*
     c* Each time get agent information
     c*
     c                   exsr      abrk
      *
     c                   eval      *in25 = *in62
     c                   eval      *in69 = *in62
     c                   if        (not *in62)
     c                   move      *in62         *in26
     c                   move      *in62         *in69
     c                   endif                                                  *IN62=*OFF
      *
      *  If Agent Error occurred, print EXCEPTION
      *
     c                   if        (*in69)
     c                   if        (*INOB)
     c                   except    head1
     c                   move      *off          *INOB
     c                   endif                                                  *INOB=*ON
     c                   except    err3
     c                   setoff                                       2526
     c                   endif                                                  *IN69=*ON
      *
      *  Accumulate Agent Return Cash Payments
      *
     c                   if        (not *in69)
      *
     c                   if        (aafein <> xxfein)
     c                   exsr      abrk
     c                   eval      xxfein = agfein
     c                   eval      xxmod = agmod
     c                   endif                                                  ACFEIN<>XXFEIN
      *
     c                   select
     c                   when      (r4vdsp = ' ')                               Check payment
     c                   eval      xxtamt = (xxtamt + r4amt)
     c                   eval      xxmamt = (xxmamt + r4amt)
     c                   when      (r4vdsp <> ' ')                              Void/Stop Pay on Chk
     c                   eval      xxtamt = (xxtamt - r4amt)
     c                   eval      xxmamt = (xxmamt - r4amt)
     c                   endsl
      * Setup Amount fields
 04  c                   eval      xxamt3 = *zeros
 04  c                   eval      xxamt6 = *zeros
 04  c                   eval      xxamt7 = xxmamt
 04  c                   eval      xxamtc = *zeros
      *
      * Now build/update the WXT6P record for the agent here, since have all info needed
      *
      * If have existing record in WXT6P, add to totals and update
      * ELSE setup and write a new record.
      *
     c     t6ky          chain     pt601                              60
      *
     c                   if        *in60
     c                   eval      t6tcc  = xxtcc
     c                   eval      t6fein = ##fein
     c                   eval      t6rtyp = ##rtyp
     c                   eval      t6mod  = *zeros
      * Load Recipient Info
     c                   eval      t6rfen = xxfein1
     c                   if        *in51
     c                   eval      t6rss# = 'Y'
     c                   else
     c                   eval      t6rss# = *blank
     c                   endif
     c
      * Load Payee Info
     c                   eval      t6pfen = aprin1
     c                   eval      t6pnam = aprn1
     c                   eval      t6padr = aprad
     c                   eval      t6pad2 = aprad2
     c                   eval      t6pcsz = xxpcsz
     c                   eval      t6pphn = apph#
     c
     c                   eval      t6amt3 = xxamt3
     c                   eval      t6amt6 = xxamt6
     c                   eval      t6amt7 = xxamt7
     c                   eval      t6amtc = xxamtc
     c                   write     pt601
     c
     c                   else
     c
     c                   eval      t6amt3 = (t6amt3 + xxamt3)
     c                   eval      t6amt6 = (t6amt6 + xxamt6)
     c                   eval      t6amt7 = (t6amt7 + xxamt7)
     c                   eval      t6amtc = (t6amtc + xxamtc)
     c                   update    pt601
     c                   endif
      *
     c                   eval      xxmamt = *zeros
     c                   eval      xxamt3 = *zeros
     c                   eval      xxamt6 = *zeros
     c                   eval      xxamt7 = *zeros
     c                   eval      xxamtc = *zeros
     c*
      *
     c                   endif                                                  *IN69=*ON
     c*
     c                   endif                                                  R4DVST = *BLANKS
     c*
     c                   endif                                                  R4PTFL = "A"
      *
     c     r4ky          reade     pr401                                  60
     c                   enddo                                                  *IN60=*OFF
      *
      *
      * Check Group Selection File for more Groups, if so go back and process
      * Use *IN80, so if get hit will reset Provider Information
     c     t5ky          reade     pt501                                  80
     c                   if        *in80 = *off
     c                   move      t5fnd         xxfnd
     c                   move      t5tcc         xxtcc
      *  Check and clear unused group members.
     c                   eval      xxMBR = ('IRS' + zzco# + t5fnd + ' ')
     c                   call      'W1099C5'                            99
     c                   parm                    xxMBR
      *
     c                   goto      nxtagt
     c                   endif
      *
      *  Write the Workfile Records in WXT6P and print 1099 forms for Agents
      *  (and providers if wanted both)
     c                   exsr      RcdBsr
      *
     c                   endif                                                  QQCOMM=A/B
      *_______________________________________________________________
      *
      * Manual 1099 - Create and print section
      *_______________________________________________________________
      *
      *  If the incoming parm is set to process manual entries then:
      *  Read each record from Manual Entry file & Write a "B" record
      *  The manual does not build a record in the WXT6P file, but
      *  creates a 1099 for each combined mod.  Check each group from
      *  file WXT5P for manual entries
      *
     c                   if        (xxincl = 'Y')
      *
      * In case 51 still on, set it off for manual processing.
     c                   move      *off          *in51
      *
      *  Chain to the Incoming Work file WXT5P and reset at First group
     c     t5ky          chain     pt501                              60
     c                   if        *in60 = *on
     c                   goto      TERM
     c                   ELSE
     c                   move      t5fnd         xxfnd
     c                   move      t5tcc         xxtcc
     c                   end
      *
     c                   move      *on           *INOA
     c                   move      *on           *in78
      *
      *  Tag for processing Next Group from WXT5P for manual 1099s
     c     NXTMAN        TAG
      *
     c                   eval      zzpmt3 = *zeros
     c                   eval      zzpmt6 = *zeros
     c                   eval      zzpmt7 = *zeros
     c                   eval      bpMtc = *Zeros
     c                   move      *blanks       zzfein
     c     tnky          setll     ptn02
     c     tnky          reade     ptn02                                  60
      *
     c                   dow       (not *in60)
      *
     c                   If        TNcombine3 = 'Y'
     c                   goto      ManEnd
     c                   EndIf

      * If same FEIN, accumulate amounts until it changes
     c                   if        (tnfein = zzfein)
     c                   eval      zzpmt3 = (zzpmt3 + tnpmt3)
     c                   eval      zzpmt6 = (zzpmt6 + tnpmt6)
     c                   eval      zzpmt7 = (zzpmt7 + tnpmt7)
     c                   goto      manend
      *
     c                   else
      *
      * If FEIN not same, print if not first record
     c                   if        (zzpmt3 >= CO1099) or
     c                             (zzpmt6 >= CO1099) or
     c                             (zzpmt7 >= CO1099)
     c                   move      zzpmt3        xxamt3
     c                   move      zzpmt6        xxamt6
     c                   move      zzpmt7        xxamt7
     c                   eval      type3t = (type3t + xxamt3)
     c                   move      zzpmt3        bpmt3
     c                   eval      type6t = (type6t + xxamt6)
     c                   move      zzpmt6        bpmt6
     c                   eval      type7t = (type7t + xxamt7)
     c                   move      zzpmt7        bpmt7
     c*** Begin Chg *** 01/11/21 ************************************
     c**********         eval      countb = (countb + 1)
      *
     c**********         except    rcdb
     c                   If        bPmt3 <> '000000000000' Or
     c                             bPmt6 <> '000000000000' Or
     c                             bPmtC <> '000000000000'
     c                   eval      countb = (countb + 1)
     c                   except    rcdb
     c                   EndIf
     c                   If        bPmt7 <> '000000000000'
     c                   eval      countbNEC = (countbNEC + 1)
     c                   except    rcdbNEC
     c                   EndIf
     c*** End   Chg *** 01/11/21 ************************************
      *
     c                   exsr      srfrm
      *
     c                   if        (*INOA)
     c                   except    head
     c                   except    headb
     c                   move      *off          *INOA
     c                   endif                                                  *INOA=*ON
      *
     c                   move      *blanks       xxcty
     c                   movel     bpcty         xxcty
     c                   except    detlb
      *
     c                   endif                                                  TNPMT3>=CO1099
      *
      *  After Write or Bypass, ZERO Out Accumulators
     c                   eval      zzpmt3 = *zeros
     c                   eval      zzpmt6 = *zeros
     c                   eval      zzpmt7 = *zeros
      *  Set up Next Record with new FEIN
     c                   move      tnfein        zzfein
     c                   eval      zzpmt3 = (zzpmt3 + tnpmt3)
     c                   eval      zzpmt6 = (zzpmt6 + tnpmt6)
     c                   eval      zzpmt7 = (zzpmt7 + tnpmt7)
      *
      *  Setup Fields for "B" Type Record
     c                   move      tnfein        bfein
     c                   movel     tnpnm1        bpnm1
     c                   movel     tnprad        bprad
     c                   movel(p)  tnprad        xxradr
     c                   movel     *blanks       bpra2
     c                   movel     tnpcty        bpcty
     c                   movel     tnpst         bpst
     c                   move      tnpzip        bpzip
     c                   movel(p)  tnpzip        ##zip5
     c                   move(p)   tnpzip        ##zip4
     c                   movel(P)  '01'          bpact
      *
     c                   if        (##zip4 > *blanks)
     c                   move      '-'           ##zhyp
     c                   else
     c                   move      ' '           ##zhyp
     c                   endif                                                  ZIP2='0000'
     c                   eval      bxcsz = %trimr(tnpcty) + ', ' + tnpst + ' ' +
     c                             %trimr(##zipc)
      *
      * If TNINTP is a '2' it is a SS# Code, Else is a FEIN Number
      * Build the Edited FEIN or SS# for printing on 1099 Forms
     c                   if        (tnintp = '2')
     c                   move      *ON           *in51
     c                   move      '2'           bintp
     c                   movel     bfein         bfsone
     c                   move      bfein         bfsthr
     c                   move      bfein         wka06                          WRK FLD A6
     c                   movel     wka06         bfstwo
     c                   else
     c                   move      *off          *in51
     c                   move      *blank        bintp
     c                   movel     bfein         bfeone
     c                   move      bfein         bfetwo
     c                   endif
      *
     c                   move(p)   bpzip         zip2
     c                   if        (zip2 = '0000')
     c                   move      '    '        bpzip
     c                   endif                                                  ZIP2='0000'
     c                   endif                                                  TNFEIN=ZZFEIN
      *
     c     manend        tag
      *
     c     tnky          reade     ptn02                                  60
      *
     c                   if        (*in60)
      *
      * Check to see if have any left over manual Records to print
     c                   if        (zzpmt3 >= CO1099) or
     c                             (zzpmt6 >= CO1099) or
     c                             (zzpmt7 >= CO1099)
     c                   move      zzpmt3        xxamt3
     c                   move      zzpmt6        xxamt6
     c                   move      zzpmt7        xxamt7
     c                   eval      type3t = (type3t + xxamt3)
     c                   move      zzpmt3        bpmt3
     c                   eval      type6t = (type6t + xxamt6)
     c                   move      zzpmt6        bpmt6
     c                   eval      type7t = (type7t + xxamt7)
     c                   move      zzpmt7        bpmt7
     c*** Begin Chg *** 01/11/21 ************************************
     c**********         eval      countb = (countb + 1)
      *
     c**********         except    rcdb
     c                   If        bPmt3 <> '000000000000' Or
     c                             bPmt6 <> '000000000000' Or
     c                             bPmtC <> '000000000000'
     c                   eval      countb = (countb + 1)
     c                   except    rcdb
     c                   EndIf
     c                   If        bPmt7 <> '000000000000'
     c                   eval      countbNEC = (countbNEC + 1)
     c                   except    rcdbNEC
     c                   EndIf
     c*** End   Chg *** 01/11/21 ************************************
      *
     c                   exsr      srfrm
      *
     c                   if        (*INOA)
     c                   except    head
     c                   except    headb
     c                   move      *off          *INOA
     c                   endif                                                  *INOA=*ON
      *
     c                   move      *blanks       xxcty
     c                   movel     bpcty         xxcty
     c                   except    detlb
      *
     c                   endif                                                  TNPMT3>=CO1099
      *
     c                   eval      zzpmt3 = *zeros
     c                   eval      zzpmt6 = *zeros
     c                   eval      zzpmt7 = *zeros
     c                   endif
      *
     c                   enddo                                                  *IN60=*OFF
      *
      * Check Group Selection File for more Groups, if so go back and process
      *
     c     t5ky          reade     pt501                                  80
     c                   if        *in80 = *off
     c                   move      t5fnd         xxfnd
     c                   move      t5tcc         xxtcc
      *  Check and clear unused group members.
     c                   eval      xxMBR = ('IRS' + zzco# + t5fnd + ' ')
     c                   call      'W1099C5'                            99
     c                   parm                    xxMBR
     c                   goto      NXTMAN
     c                   endif
      *
     c                   endif                                                  XXINCL=Y
      *
      *_______________________________________________________________
      *
      *  Create the 'C' Record, Control Totals
      *_______________________________________________________________
      *
     c                   move      'C'           crtyp
     c                   move      countb        c#pae
     c*** Begin Add ***01/11/21 ********************************************
     c                   move      countbNEC     c#paeNEC
     c*** End   Add ***01/11/21 ********************************************
     c                   move      *blank        cfill1
     c                   move      *zero         ctot1
     c                   move      *zero         ctot2
     c                   move      type3t        ctot3
     c                   move      *zero         ctot4
     c                   move      *zero         ctot5
     c                   move      type6t        ctot6
     c                   move      type7t        ctot7
     c*** Begin Add ***01/11/21 ********************************************
     c                   move      *zero         ctot70
     d*** End   Add *** 01/11/21 *******************************************
     c                   move      *zero         ctot8
     c                   move      *zero         ctot9
     c                   move      *zero         ctota
     c                   move      *zero         ctotb
     c                   move      typect        ctotc
     c                   move      *zero         ctotd
     c                   move      *zero         ctote
     c*** Begin Add *** 01/25/22 ************************************
     c                   move      *zero         ctotf
     c                   move      *zero         ctotg
     c                   move      *zero         ctoth
     c                   move      *zero         ctotj
     c*** End   Add *** 01/25/22 ************************************
     c                   move      *blank        cfillB
     c                   move      *blank        cfill2
     c                   move      *blank        cfill3
     c                   move      *blank        ccrlf
      *
     c                   except    rcdc
     c*** Begin Add *** 01/11/21 ************************************
     c                   except    rcdcNEC
     c*** End   Add *** 01/11/21 ************************************
     c                   except    totl
      *
      *  Close the Appropriate Print Files
     c                   if        (*in81)
     c                   close     qsysprt2
     c                   endif                                                  *IN81=*ON
      *
     c                   if        (*in82)
     c                   close     w1099o3
     c                   close     w1099o4
     c                   close     w1099o5
     c*** Begin Add *** 10/08/20 *******************************************
     c                   close     w1099oNEC3
     c                   close     w1099oNEC4
     c                   close     w1099oNEC5
     c*** End   Add *** 10/08/20 *******************************************
      *
     c                   if        (*inog)
     c                   except    label1
     c                   else
     c                   if        (x1add2 = *blanks) and
     c                             (x2add2 = *blanks)
     c                   except    label
     c                   else
     c                   if        (x1add2 = *blanks)
     c                   movel     x1csz         x1add2
     c                   move      *blanks       x1csz
     c                   endif
     c                   if        (x2add2 = *blanks)
     c                   movel     x2csz         x2add2
     c                   move      *blanks       x2csz
     c                   endif
     c                   except    label2
     c                   endif
     c                   endif                                                  *INOG=*ON
      *
     c                   close     qsysprt6
      *
     c                   endif                                                  *IN82=*ON
      //*** Beg Add ***** 03/04/24 ***********************
       If cltId = '140';
          ThirdPartyFile(qqYear);
       Endif;
      //*** End Add ***** 03/04/24 ***********************
      *_______________________________________________________________
      *
     c     TERM          tag
      /Free
       Chain orky por01;
       If %found;
          Except Route;
          z= 1;
          Dou z > grps OR z > grp#s;
             If *inoa = *on;
                Except route;
                *inoa = *off;
             Endif;
          Except ROUTEG;
          z = ( z + 1 );
          Enddo;
          Except ROUTEC;
       Endif;
      /End-free
     c                   move      *on           *inlr
      *_______________________________________________________________
      *
     c/EJECT
      *
      *===============================================================
      * ABRK   - AGENT BREAK SUBROUTINE
      *===============================================================
     c     abrk          begsr
      *
      * Get the mail to address from the correct Mod and load "B" values from this record
      *
     c                   eval      xxmod  = *zeros
      *
     c     aaky          chain     paa01                              62        xxco#,acfnd,acagt#
      *
     c     agky          chain     pag01                              61        xxco#,aafein,aamod
      *
      * Setup WXT6P Fields first in case only have one MOD for Agent
     c                   if        (not *in61)
     c                   move      agfein        #fein
     c                   eval      ##fein = agfein
      *
      * Check Flag for FEIN being a Soc Sec #
     c                   move      *off          *in51
     c                   if        (agss# = 'Y')
     c                   move      *on           *in51
     c                   endif
      *
     c                   movel(p)  agname        T6rnam
      * If Agent Federal Name Exists, Use it in the B Record
     c                   if        (agfdnm > *blanks)
     c                   movel     agfdnm        t6rnam
     c                   endif
     c                   movel(p)  agmad1        t6radr
     c                   movel(p)  *blanks       t6rad2
     c                   movel(p)  agmcty        t6rcty
     c                   movel     agmst         t6rst
     c                   move      agmzip        t6rzip
      *
     c                   move(p)   t6rzip        zip2
      *
     c                   if        (zip2 = '0000')
     c                   move      '    '        t6rzip
     c                   endif                                                  ZIP2='0000'
      *
      * If SS# Indicator is on, Move the SS# Code to output File
      * Will default to Blank, don't know if is an FEIN. If know is
      * an FEIN, then would use a '1' instead of a blank
      * Build the Edited FEIN or SS# for printing on 1099 Forms
      *
     c                   if        (*in51)
     c                   move      '2'           bintp
     c                   movel     #fein         bfsone
     c                   move      #fein         bfsthr
     c                   move      #fein         wka06                          WRK FLD A6
     c                   movel     wka06         bfstwo
     C                   movel     bfss#1        xxfein1
     c                   eval      t6rss#  = 'Y'
     c                   else
     c                   move      *blank        bintp
     c                   movel     #fein         bfeone
     c                   move      #fein         bfetwo
     c                   movel     bfein1        xxfein1
     c                   eval      t6rss#  = ' '
     c                   endif
      *
     c                   endif                                                  *in51 = *off
      *
      *
      *  Check to see if the Modifier that will receive the 1099
      *  is the not same as current Modifier. If so then Load name
      *  and Address from new record
      *  If the mail to Modifier is not the same as mod being processed then
      *  get the mail to Mod information for printing and IRS File
     c                   if        ag109m <> agmod
     c                   move      agfein        #fein
     c                   eval      xxmod = ag109m
     c     agky2         chain     pag01                              63        xxco#,agfein,xxmod
      *
     c                   if        *in63
     c                   else
     c                   movel(p)  agname        t6rnam
      *
      * If Agent Federal Name Exists, Use it on 1099s
     c                   if        (agfdnm > *blanks)
     c                   movel     agfdnm        t6rnam
     c                   endif
     c                   movel(p)  agmad1        t6radr
     c                   movel(p)  *blanks       t6rad2
     c                   movel(p)  agmcty        t6rcty
     c                   movel     agmst         t6rst
     c                   move      agmzip        t6rzip
      *
     c                   move(p)   t6rzip        zip2
      *
     c                   if        (zip2 = '0000')
     c                   move      '    '        t6rzip
     c                   endif                                                  ZIP2='0000'
      *
     c                   endif                                                  NOT *IN63
      *
      *  Setup Fields for "B" Type Record
      *
     c                   endif                                                  AG109M = AGMOD
      *
      *
     c                   endsr                                                  ABRK
      *================================================================
      * BT6Rcd - Build WXT6P "B" records
      *================================================================
     c     BT6Rcd        begsr
      *
      *  ADD to the Totals by FEIN
     c                   eval      xxmamt = *zeros
      *
 03   * If provider type put in correct Box number
     c                   if        ##rtyp  = 'P' AND
      *  Do NOT Include deductible recoveries in the 1099 Calculations
     c                             (patrcd <> fnmetr) and
     c                             (patrcd <> fncotr) and
     c                             (patrcd <> fnextr) and
     c                             (patrcd <> fnletr)
      *
     c                   if        (patype = 'P') or
     c                             (patype = 'M') or
     c                             (patype = 'Z') or
     c                             (patype = 'A')
     c                   eval      xxmamt = (xxmamt + paamt)                    MODIFIER TOTAL
     c                   else
     c                   eval      xxmamt = (xxmamt - paamt)                    MODIFIER TOTAL
     c                   endif                                                  PATYPE=P/M/Z/A
 03   *
     c                   select
     c                   when      (PAreTp= 'C') and
     c                             (B2vHpC = 'Y')
 04  c                   eval      xxamt3 = *zeros
 04  c                   eval      xxamt6 = *zeros
 04  c                   eval      xxamt7 = xxMaMt
 04  c                   eval      xxamtc = *zeros
b04  c                   when      (pdtype = '1') or
 04  c                             (pdtype = '4')
 04  c                   eval      xxamt3 = *zeros
 04  c                   eval      xxamt6 = xxmamt
 04  c                   eval      xxamt7 = *zeros
 04  c                   eval      xxamtc = *zeros
b04  c                   when      (pdtype = '5') and
 04  c                             (xbox14 = 'Y')
 04  c                   eval      xxamt3 = *zeros
 04  c                   eval      xxamt6 = *zeros
 04  c                   eval      xxamt7 = *zeros
 04  c                   eval      xxamtc = xxmamt
      *
x04  c                   other
 04  c                   eval      xxamt3 = *zeros
 04  c                   eval      xxamt6 = *zeros
 04  c                   eval      xxamt7 = xxmamt
 04  c                   eval      xxamtc = *zeros
e04  c                   endsl
     c                   endif
      *
 03   * If Agent Commission Type, process only Payments or Void Records
     c                   if        ##rtyp  = 'A'
      *
     c                   select
     c                   when      (accotp = 'P')
     c                   eval      xxmamt = (xxmamt + acamt)
     c                   when      (accotp = 'V')
     c                   eval      xxmamt = (xxmamt - acamt)
     c                   endsl
 04  c                   eval      xxamt3 = *zeros
 04  c                   eval      xxamt6 = *zeros
 04  c                   eval      xxamt7 = xxmamt
 04  c                   eval      xxamtc = *zeros
      *
     c                   endif
      *
      * If have existing record in WXT6P, add to totals and update
      * ELSE setup and write a new record.
      *
     c     t6ky          chain     pt601                              60
      *
     c                   if        *in60
     c                   eval      t6tcc  = xxtcc
     c                   eval      t6fein = ##fein
     c                   eval      t6rtyp = ##rtyp
     c                   eval      t6mod  = *zeros
      * Load Recipient Info
     c                   eval      t6rfen = xxfein1
     c                   if        *in51
     c                   eval      t6rss# = 'Y'
     c                   else
     c                   eval      t6rss# = *blank
     c                   endif
     c
      * Load Payee Info
     c                   eval      t6pfen = aprin1
     c                   eval      t6pnam = aprn1
     c                   eval      t6padr = aprad
     c                   eval      t6pad2 = aprad2
     c                   eval      t6pcsz = xxpcsz
     c                   eval      t6pphn = apph#
     c
      /Free
       // Include manual entries in Box 3 calculations,if flagged.
          TNCo# = xxCo# ;
          TNfnd = xxFnd ;
          TNptYr= xxYr;
          TNFein= %Editc(##Fein:'X');
          Setll (TNCo#:TNfnd:TNptYr:TNFein) ptn02;
          Reade (TNCo#:TNfnd:TNptYr:TNFein) ptn02;
          Dow not(%eof);
             If TNcombine3 = 'Y';
                xxAmt3 = xxAmt3 + TNpmt3;
             Endif;
             Reade (TNCo#:TNfnd:TNptYr:TNFein) ptn02;
          Enddo;
      /End-Free
     c                   eval      t6amt3 = xxamt3
     c                   eval      t6amt6 = xxamt6
     c                   eval      t6amt7 = xxamt7
     c                   eval      t6amtc = xxamtc
     c                   write     pt601
     c
     c                   else
     c
     c                   eval      t6amt3 = (t6amt3 + xxamt3)
     c                   eval      t6amt6 = (t6amt6 + xxamt6)
     c                   eval      t6amt7 = (t6amt7 + xxamt7)
     c                   eval      t6amtc = (t6amtc + xxamtc)
     c                   update    pt601
     c                   endif
      *
     c                   eval      xxmamt = *zeros
     c                   eval      xxamt3 = *zeros
     c                   eval      xxamt6 = *zeros
     c                   eval      xxamt7 = *zeros
     c                   eval      xxamtc = *zeros
      *
     c                   endsr                                                  ABRK
      *
      *================================================================
      * CHKLGL - Determine if COMP Payment was made to an ATTORNEY
      *================================================================
     c     chklgl        begsr
      *
     c                   move      xxfein        svfein
     c                   move      xxmod         svmod
     c                   movel(p)  pabr2         xxkey
     c                   movel     *blanks       b2plcd
     c                   movel     *blanks       pdtype
     c     tbky          chain     pb201                              60
      *
     c                   move      pafein        xxfein
     c                   move      pamod         xxmod
     c     pdky          chain     ppd01                              60
      *
      * If Medical payment and made to Attorney, gets a 1099 with amt in Box 7
     c                   if        (b2plcd = 'M') and
     c                             (pdtype = '5')
     c                   move      *on           proces
     c                   endif                                                  B2PLCD=L
      *
      * Else will be a regular Legal Payment in Box 7  (114WCZ  01/03/02)
      * if Payment Reserve type is "C" and Provider Type is "5"
      *
     c                   if        (pdtype = '5')
     c                             and cltID = '114'
     c                   move      *on           proces
     c                   endif                                                  B2PLCD=L
      *                                                    PDTYPE=5
      * If the Pay Value is <> an "M" & Attorney (pdtype = 5) then goes in Box 14
      *                                                    PDTYPE=5
      * Else will be a regular Legal Payment in Box 7
      *
      /Free
        Select;
        //*** Begin Add *** 01/11/21 ***************************************
        When cltID = '111' And B2plCd <> 'M' And PDtype = '5';
           xbox14 = 'Y';
           Proces = *on;
        //*** End   Add *** 01/11/21 ***************************************
        When B2plcd = 'U' and PDtype = '5';        //Box 14
           xbox14 = 'Y';
           Proces = *on;
        Other;
           xbox14 = *blanks;
        Endsl;
           if PAreTp = 'C' and B2vHpC = 'Y';
              proces = *on;
           endif;
      /End-Free
     c                   move      svfein        xxfein
     c                   move      svmod         xxmod
      *
     c                   endsr                                                  CHKLGL
      *
      *===============================================================
      * PBRK   - PROVIDER BREAK SUBROUTINE
      *===============================================================
      *
     c     pbrk          begsr
 03   *
 03   *  Get the Modifier that will receive the 1099 by reading all
 03   *  the Mods for FEIN number.  If mod is zeros, then use that
 03   *  for Name & Address on 1099s.
 01   *
     c                   move      pafein        svfein
     c                   move      pamod         svmod
 01   *
 02  c                   move      *zeros        xxmod
 02  c     pdky1         chain     ppd01                              61
 04  c                   move      pd109m        xxmod
b03  c                   dow       (not *in61)
b04  c                   if        xxmod <> pd109m AND xxmod = *zeros
 04  c                   move      pd109m        xxmod
e04  c                   endif                                                  MOD,X<>PD109M
     c     pdky1         reade     ppd01                                  61
     c                   enddo
 03   *
 03   *  Now get the SEND TO Mod Name and Address
 04  c     pdky          chain     ppd01                              61
      * If NOT mail to mod not found, use original mod for address
     c                   if        *in61
     c                   move      pamod         xxmod
 04  c     pdky          chain     ppd01                              61
     c                   endif
b04  c                   if        (NOT *in61)
 03   *
 03  c                   move      xxfein        #fein
      *
      * Check Flag for FEIN being a SS#
     c                   move      *off          *in51
     c                   if        (pdss# = 'Y')
     c                   move      *on           *in51
     c                   endif
      *
      * If SS# Indicator is on, Move the SS# Code to output File
      * Will default to Blank, don't know if is an FEIN. If know is
      * an FEIN, then would use a '1' instead of a blank
      * Build the Edited FEIN or SS# for printing on 1099 Forms
      *
     c                   eval      xxfein1 = *blanks
     c                   if        (*in51)
     c                   move      '2'           bintp
     c                   movel     #fein         bfsone
     c                   move      #fein         bfsthr
     c                   move      #fein         wka06                          WRK FLD A6
     c                   movel     wka06         bfstwo
     c                   eval      xxfein1 = bfss#1
     c                   else
     c                   move      *blank        bintp
     c                   movel     #fein         bfeone
     c                   move      #fein         bfetwo
     c                   movel     bfein1        xxfein1
     c                   endif
 03   *
b05  c                   if        (pdfsn = *blanks)
 05  c                   movel     pdname        t6rnam
x05  c                   else
     c                   eval      t6rnam = %trimr(pdname) + ', ' +
 03  c                             %trimr(pdfsn)
e05  c                   endif                                                  PDFSN=*BLANKS
 06  c                   move      *blanks       t6rnm2
 04   *
e04  c                   endif                                                  PDMOD=PD109M
 05   *
 03  c                   if        (pddba1 = 'Y')
 03  c                   movel     pdadr2        t6radr
      * If load 2nd address line into t6radr, then blank out t6rad2
     c                   movel(p)  *blanks       t6rad2
 03  c                   else
 03  c                   movel     pdadr1        t6radr
     c                   movel(p)  pdadr2        t6rad2
 03  c                   endif                                                  PDDBA1 = Y
      *
 03  c                   movel     pdcty         t6rcty
 03  c                   movel     pdst          t6rst
 03  c                   move      pdzip         t6rzip
     c                   move(p)   t6rzip        zip2
      *
     c                   if        (zip2 = '0000')
     c                   move      '    '        t6rzip
     c                   eval      xxrcsz =%trimr(pdcty) + ', ' + pdst + ' ' +
     c                             %trimr(t6rzip)
     c                   else
     c                   eval      xxrcsz =%trimr(pdcty) + ', ' + pdst + ' ' +
     c                             t6rzip
     c                   endif                                                  ZIP2='0000'
 03   *
      * Now get back to original WMPDP record for checking flags
     c                   move      svfein        xxfein
     c                   move      svmod         xxmod
 01   *
 02  c     pdky          chain     ppd01                              61
 01   *
 01  c                   eval      xxtamt = *zero
      *
     c                   endsr                                                  PBRK
      *
      *================================================================
      * RcdBsr - Build "B" records in WATNP and print 1099s
      *================================================================
     c     RcdBsr        begsr
      *
      * Read the WXT6P file and see if any amount is over limit for 1099 for each record,
      * If so, Write the WANTP "B" record and print a 1099 form.
     c                   eval      ##fein = *zeros
     c                   eval      ##rtyp = 'P'
     c     t6ky2         setll     pt601
     c                   read      pt601                                  61
     c                   dow       (not *in61)
      *
      * If all amounts > IRS Limit, then process and zero out any Negative Amts.
      *
     c                   eval      xxtamts = t6amt3 + t6amt6 + t6amt7 + t6amtc
      *
      * As per the revised law from IRS if Gross Attorney Amt is
      * positive amount and less the $600 limit then no need to report.
      * If Gross Attorney Amt is positive (no $600 limit) create a 1099 Form
      * OR the total of all boxes is > IRS limit, create a 1099 form
     C                   if        xxtamts >= co1099
      *
      *
      * If have a negative amount, Print error line on QSYSPRT1, then zero field.
      *
     c                   if        t6amt3 < *zeros  OR
     c                             t6amt6 < *zeros  OR
     c                             t6amt7 < *zeros  OR
     c                             t6amtc < *zeros
     c                   except    ERR4
      *
     c                   if        t6amt3 < *zeros
     c                   eval      t6amt3 = *zeros
     c                   endif
     c                   if        t6amt6 < *zeros
     c                   eval      t6amt6 = *zeros
     c                   endif
     c                   if        t6amt7 < *zeros
     c                   eval      t6amt7 = *zeros
     c                   endif
     c                   if        t6amtc < *zeros
     c                   eval      t6amtc = *zeros
     c                   endif
      *
     c                   endif
      *
     c                   eval      xxamt3  = t6amt3
     c                   eval      xxamt6  = t6amt6
     c                   eval      xxamt7  = t6amt7
     c                   eval      xxamtc  = t6amtc
      *
      * Add to the 'C' record totals, and move fields to the WATNP 'B' record fields
     c                   eval      type3t = (type3t + t6amt3)
     c                   move      xxamt3        bpmt3
     c                   eval      type6t = (type6t + t6amt6)
     c                   move      xxamt6        bpmt6
     c                   eval      type7t = (type7t + t6amt7)
     c                   move      xxamt7        bpmt7
     c                   eval      typect = (typect + t6amtc)
     c                   move      xxamtc        bpmtc
      *
     c                   eval      aprn1  = t6pnam
     c                   eval      aprad  = t6padr
     c                   eval      aprad2 = t6pad2
     c                   eval      aprcs  = t6pcsz
      *
     c                   eval      bpnm1  = t6rnam
     c                   eval      bpnm2  = t6rnm2
     c                   eval      bprad  = t6radr
     c                   eval      bpra2  = t6rad2
     c                   eval      bpcty  = t6rcty
     c                   eval      bpst   = t6rst
     c                   move      *blanks       xxcty
     c                   movel     t6rcty        xxcty
     c                   movel(p)  t6rzip        bpzip
     c                   movel(p)  t6rzip        ##zip5
     c                   move(p)   t6rzip        ##zip4
      *
     c                   if        (##zip4 > *blanks)
     c                   move      '-'           ##zhyp
     c                   else
     c                   move      ' '           ##zhyp
     c                   endif                                                  ZIP2='0000'
     c                   eval      bxcsz = %trimr(t6rcty) + ', ' + t6rst + ' ' +
     c                             %trimr(##zipc)
      *
      * Concatenate the Two Address Lines for Output in WATNP file
     c                   move      *blanks       xxradr
     c                   if        (bpra2 <> *blanks)
     c     bprad         cat       ',':0         xxradr
     c                   cat       bpra2:1       xxradr
     c                   else
     c                   movel     bprad         xxradr
     c                   endif
      *
      * If is a SS#, use as edited, if not, move the FEIN left
     c                   if        t6rss# = 'Y'
     c                   eval      *in51  = *on
     c                   eval      bfss#1 = t6rfen
     c                   eval      bfein1 = *blanks
     c                   move      '2'           bintp
     c                   else
     c                   eval      bfss#1 = *blanks
     c                   movel     t6rfen        bfein1
     c                   eval      *in51  = *off
     c                   move      *BLANK        bintp
     c                   endif
     c                   move      t6fein        bfein
     c                   movel(P)  '01'          bpact
      *
     c*** Begin Del *** 01/11/21 ************************************
     c**********         eval      countb = (countb + 1)
     c*** End   Del *** 01/11/21 ************************************
      *
      * If first agent record, print new heading for agents
     c                   if        (t6rtyp = 'A') AND (NOT *in30)
     c                   move      *on           *in30
     c                   move      *on           *INOA
     c                   endif
      *
     c                   if        (*INOA)
     c                   except    head
     c                   except    headb
     c                   move      *off          *INOA
     c                   endif                                                  *INOA=*ON
     c                   except    detlb
      *
     c*** Begin Add *** 01/11/21 ************************************
     c**********         except    rcdb
     c                   If        bPmt3 <> '000000000000' Or
     c                             bPmt6 <> '000000000000' Or
     c                             bPmtC <> '000000000000'
     c                   eval      countb = (countb + 1)
     c                   except    rcdb
     c                   EndIf
     c                   If        bPmt7 <> '000000000000'
     c                   eval      countbNEC = (countbNEC + 1)
     c                   except    rcdbNEC
     c                   EndIf
     c*** End   Add *** 01/11/21 ************************************
     c                   exsr      srfrm
      *
     c                   endif
      *
     c                   read      pt601                                  61
     c                   enddo
      *
     c                   endsr                                                  PBRK
      *
      *===============================================================
      * SR1ST  - First Time Subroutine - INITIALIZE
      *===============================================================
      *
     c     sr1st         begsr
      *
     c* Read rest WXT5P records and load array w/grp number and name
     c     t5ky          reade     pt501                                  80
     c                   dow       *in80 = *off
     c                   move      t5fnd         xtfnd
     c     fnky1         chain     pfn01                              60
     c                   add       1             grp#s
     c                   add       1             z
     c                   move      fnfnd         grp(z)
     c                   movel(p)  fnname        gnm(z)
     c     t5ky          reade     pt501                                  80
     c                   enddo
      * Now set pointer back to beginning of file
     c     t5ky          chain     pt501                              80
     c     fnky          chain     pfn01                              60
      *
      *   Get the Disribution Information & Print it
     c                   move      '0'           xxprt
      *
     c     orky          chain     por01                              61
     c                   if        (ortitl <> *blanks)
     c                   move(p)   ortitl        xxdata                         FIELD, LENGTH
     c     ' '           check     xxdata        n
      *
     c                   if        (n <= 60)                                    LENGTH
     c                   eval      ortitl = %SUBST(XXDATA:N)                    FIELD
     c                   endif                                                  N<=60
      *
     c                   else
      *
     c                   movel(p)  title         ortitl
     c                   endif                                                  ORTITL<>*BLANKS
      *
     c                   if        (not *in61)
     C                   if        ordis1 = *blanks
     c     q1user        chain     pup01                              60
     c                   movel(P)  upusnm        ordis1
     c                   endif
      *
      * If have multiple groups, print on routing page, overflow if needed, then
      * print copyright line.
     c                   endif                                                  *IN61=*OFF
      *
      *  Center Text in Field
     c                   movel(p)  ortitl        ##data
     c                   call      'SRCENTER'                           99
     c                   parm                    ##data
     c                   parm      60            ##leng
     c                   movel     ##data        ortitl
      *
      *  Center Company name in display field
     c                   movel(p)  coname        ##data
     c                   call      'SRCENTER'                           99
     c                   parm                    ##data
     c                   parm      40            ##leng
     c                   movel(p)  ##data        coname
      *
     c                   except    head
     c                   except    head1
      *
     c                   movel     f2tcc         atcc
      *
      *  Move Values to output Fileds to Create the 'A' Record
     c                   movel     'A'           artyp
     c                   move      xxyear        aptyr
     c                   move      *blank        afill1
     c                   move      f2fein        aprin
     c                   move      f2fein        wka09
      *
     c                   movel     wka09         apeone
     c                   move      wka09         apetwo
      *
     c                   move      *blanks       aprnc
     c                   move      *blank        alfin
     c                   movel     'A'           arttp
     c*** Begin Add *** 01/11/21 ************************************
     c                   movel     'NE'          arttpNEC
     c*** End   Add *** 01/11/21 ************************************
      * ADD C TO CODES FOR AMOUNTS THAT MAY APPEAR ON 1099 FORM
     c*** Begin Chg *** 01/11/21 *******************************************
     c**********         movel(p)  '367C'        aaind
     c                   movel(p)  '36C'         aaind
     c                   movel(p)  '1'           aaindnec
     c*** End   Chg *** 01/11/21 *******************************************
     c                   move      *blank        afill2
     c                   move      *blanks       aofind
     c                   move      *blanks       arfind
     c                   move      *blanks       acfind
     c                   move      *blanks       afcoi
     c                   movel     fnname        aprn1
     c                   move      *blanks       aprn2
     c                   move      '0'           atrag
     c                   move      fnmad1        aprad
     c                   move      fnmad2        aprad2
      *
      *  Combine City, State, ZIP form FNMCTY,FNMST,FNMZIP
     c                   movel(p)  fnmcty        xxlast
     c                   movel(p)  fnmst         xxstzp
     c                   movel(p)  fnmzip        xxzip5
     c                   move      fnmzip        xxstzp
     c                   move(p)   xxstzp        xxzip4
      *
     c                   if        (xxzip4 = *zero)
     c                   move      '    '        xxstzp
     c                   else
     c                   movel     '-'           xx5
     c                   move      xxzip4        xx5
     c                   movel     xxzip5        xxst10
     c                   move      xx5           xxst10
     c                   move      xxst10        xxstzp
     c                   endif                                                  XXZIP4=*ZERO
      *
      *  Concatenate City, State and ZIP
     c                   movel(p)  xxstzp        xxfrst
     c                   call      'SRNAME'      plname
     c                   movel(p)  xxlast        xxcsz
     c                   movel(p)  xxcsz         aprcs
     c                   movel(p)  fnmcty        apcty
     c                   movel(p)  fnmst         apst
     c                   move(p)   fnmzip        apzip
     c                   move(p)   fnmzip        zip2
      *
     c                   if        (zip2 = '0000')
     c                   move      '    '        apzip
     c                   eval      xxpcsz =%trimr(fnmcty) + ', ' + fnmst + ' ' +
     c                             xxzip5
     c                   else
     c                   eval      xxpcsz =%trimr(fnmcty) + ', ' + fnmst + ' ' +
     c                             xxst10
     c                   endif                                                  ZIP2='0000'
      *
     c                   if        (fnph1 <> *zero)
     c                   move      fnph1         xxc007
     c                   move      fnph1a        xxc003
     c                   else
     c                   move      fnph2         xxc007
     c                   move      fnarea        xxc003
     c                   endif                                                  FNPH1<>*ZERO
      *
     c                   if        (xxc003 <> '000')
     c     '('           cat(p)    xxc003        apph#
     c     apph#         cat       ')':0         apph#
     c                   else
     c                   move      *blanks       apph#
     c                   endif                                                  XXC003<>000
     c*** Begin Add *** 01/11/21 *******************************************
     c                   Eval      apPh#MPE = apPh#
     c*** End   Add *** 01/11/21 *******************************************
      *
     c     xxc003        cat(p)    xxc007:0      apphn#
      *
     c                   eval      %subst(xxc003:1:3) =
     c                             %SUBST(XXC007:1:3)
     c                   eval      %subst(xxc004:1:4) =
     c                             %SUBST(XXC007:4:4)
     c     apph#         cat       xxc003:0      apph#
     c     apph#         cat       '-':0         apph#
     c     apph#         cat       xxc004:0      apph#
      *
     c                   move      *blank        afill3
     c                   move      *blank        afill4
     c                   move      *blank        acrlf
      *
      *  Write the 'A' Record into WATNP file
      *
     c                   except    rcda
     c*** Begin Add *** 01/11/21 ************************************
     c                   except    rcdaNEC
     c*** End   Add *** 01/11/21 ************************************
     c                   except    detla
     c                   if        *INOA
     c                   except    head
     c                   endif
     c                   except    headb
      *
     c                   endsr                                                  SR1ST
      *===============================================================
      * SRFRM  - Read WATNP & Print 1099 MISC Forms
      *===============================================================
      *
     c     srfrm         begsr
      *
      *  Concatenate City, State, ZIP for LABELS
      *
     c                   move      bpmt3         xpmt3
     c                   move      bpmt6         xpmt6
     c                   move      bpmt7         xpmt7
     c                   move      bpmtc         xpmtc
     c                   move      bpmtc         xpmtc_clt
      *
     c                   if        (*in81)
     c                   except    form
     c                   endif                                                  *IN81=*ON
      *
      * If Laser forms, print here, two to a page
     c                   if        (*in82)
     c*** Begin Chg *** 10/08/20 *******************************************
     c**********         eval      f1099# = (f1099# + 1)
      *
     c**********         select
     c**********         when      (f1099# = 1)
     c**********         write     f51101
     c**********         write     f51111
     c**********         write     f51121
     c**********         when      (f1099# = 2)
       //*** If *In52 = *On and cltID = '045';
          //*** write f5110CLT;
          //*** write f5111CLT;
          //*** write f5112CLT;
       //*** Else;
          //*** Begin Chg *** 10/08/20 *************************************
          //*** write f51102;
          //*** write f51112;
          //*** write f51122;
       //*** Endif;
     c**********         eval      f1099# = *zeros
     c**********         endsl
     c*********************************************************************
      *
      /Free
       //*** Begin Add *** 01/11/21 ****************************************
       aprn1MPE  = aprn1;
       apradMPE  = aprad;
       aprad2MPE = aprad2;
       aprcsMPE  = aprcs;
       apPh#MPE  = apPh#;
       bpnm1MPE  = bpnm1;
       bpradMPE  = bprad;
       bpra2MPE  = bpra2;
       bxcszMPE  = bxcsz;
       //*** End   Add *** 01/11/21 ****************************************
       // Print MISC form
       If xPmt3 <> 0 Or xPmt6 <> 0 Or xPmtC <> 0;
          f1099# = (f1099# + 1);
          Select;
          When (f1099# = 1);
             Write f51101;
             Write f51111;
             Write f51121;
          When (f1099# = 2);
             If *In52 = *On and cltID = '045';
                write f5110CLT;
                write f5111CLT;
                write f5112CLT;
             Else;
                write f51102;
                write f51112;
                write f51122;
             EndIf;
             f1099# = *zeros;
          EndSl;
       EndIf;
       // Print NEC form
       If xpMt7  <> 0;
          f1099#N = (f1099#N + 1);
          //*** Begin Add *** 10/14/21 *************************************
          //***Select;
          //***When (f1099#N = 1);
          //***   Write f51101NEC;
          //***   Write f51111NEC;
          //***   Write f51121NEC;
          //***When (f1099#N = 2);
          //***   If *In52 = *On and cltID = '045';
          //***      write f5110CLTN;
          //***      write f5111CLTN;
          //***      write f5112CLTN;
          //***   Else;
          //***      write f51102NEC;
          //***      write f51112NEC;
          //***      write f51122NEC;
          //***   EndIf;
          //***   f1099#N = *zeros;
          //***EndSl;
          Select;
          When (f1099#N = 1);
             Write f51101NEC;
             Write f51111NEC;
             Write f51121NEC;
          When (f1099#N = 2);
             write f51102NEC;
             write f51112NEC;
             write f51122NEC;
          When (f1099#N = 3);
             write f51103NEC;
             write f51113NEC;
             write f51123NEC;
             f1099#N = *zeros;
          EndSl;
          //*** End   Add *** 10/14/21 *************************************
       //*** Begin Add *** 10/14/21 ****************************************
       EndIf;
      /End-Free
       //*** End   Chg *** 10/08/20 *************************************
      *
     c                   endif                                                  *IN82=*ON
      *
      *
     c                   if        (x1name = *blanks)
     c                   movel     bpnm1         x1name
     c                   movel     bprad         x1addr
     c                   movel     bpra2         x1add2
     c                   movel     bxcsz         x1csz
      *
     c                   else
      *
     c                   movel     bpnm1         x2name
     c                   movel     bprad         x2addr
     c                   movel     bpra2         x2add2
     c                   movel     bxcsz         x2csz
      *
     c                   if        (*in82)
      *
      * If Laser forms, print labels, two across.
     c                   if        (*inog)
     c                   except    label1
     c                   move      *off          *inog
     c                   endif
     c                   if        (x1add2 = *blanks) and
     c                             (x2add2 = *blanks)
     c                   except    label
     c                   else
     c                   if        (x1add2 = *blanks)
     c                   movel     x1csz         x1add2
     c                   move      *blanks       x1csz
     c                   endif
     c                   if        (x2add2 = *blanks)
     c                   movel     x2csz         x2add2
     c                   move      *blanks       x2csz
     c                   endif
     c                   except    label2
     c                   endif                                                  *INOG=*ON
      *
     c                   endif                                                  *IN82=*ON
      *
      * Clear fields before processing next set of labels
     c                   movel     *blanks       x1name
     c                   movel     *blanks       x1addr
     c                   movel     *blanks       x1csz
     c                   movel     *blanks       x2name
     c                   movel     *blanks       x2addr
     c                   movel     *blanks       x2csz
     c                   movel     *blanks       bycsz
      *
     c                   endif                                                  X1NAME=*BLAN
      *
     c                   endsr                                                  SRFRM
      ** ========================================================= **
      *  Initialization Subroutine - Before FIRST Record Processed
      ** ========================================================= **
     c     *INZSR        begsr
      *
     c     *entry        plist
     c                   parm                    qqco#                          Company Number
     c                   parm                    qqfnd                          Initial Group Nbr
     c                   parm                    qqyear                         4 Digit Reporting YR
     c                   parm                    qqcomm                         Prov, Agent or Both
     c                   parm                    qqpofl                         Laser or Pinfeed
     c                   parm                    qqincl                         Include Manual Rcds
      *_______________________________________________________________
      *
      *  Agent Assignment file
     c     aaky          klist
     c                   kfld                    xxco#
     c                   kfld                    acfnd
     c                   kfld                    acagt#
      *
      *  Agent Commission File
     c     acky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
      *
      *  Agent Master file
     c     agky          klist
     c                   kfld                    xxco#
     c                   kfld                    aafein
     c                   kfld                    aamod
      *
     c     agky1         klist
     c                   kfld                    xxco#
     c                   kfld                    aafein
      *
     c     agky2         klist
     c                   kfld                    xxco#
     c                   kfld                    agfein
     c                   kfld                    xxmod
      *
      *  Claiment Demographic file
     c     cdky          klist
     c                   kfld                    cmss#
      *
      *  Claim File
     c     cmky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    ##ss#
     c                   kfld                    pafyr
     c                   kfld                    pacase
      *
     c     cmky1         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    pafyr
     c                   kfld                    pacase
      *
      *  Group File Keylist
     c     fnky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
      *
     c     fnky1         klist
     c                   kfld                    xxco#
     c                   kfld                    xtfnd
      *
      *  Group Auxiliary
     c     f2ky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
      *
      *  Override File
     c     orky          klist
     c                   kfld                    q1pgm
     c                   kfld                    xxprt
      *
      *  Payment Detail file
     c     paky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
      *
      *  Payee Demographic file
     c     pdky          klist
     c                   kfld                    xxfein
     c                   kfld                    xxmod
      *
     c     pdky1         klist
     c                   kfld                    xxfein
      *
      *  Payment Value 1 Table File
     c     tbky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxkey
      *
      *  Manual Entry "B" file
     c     tnky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxyr
      *
      *
      *  Bank Account Master file
     c     ckky          klist
     c                   kfld                    xxco#
     c                   kfld                    paacct
      *
      *  Select Group Workfile for 1099
     c     t5ky          klist
     c                   kfld                    zzco#
      *
      *  Workfile FEIN "B" file
     c     t6ky          klist
     c                   kfld                    xxtcc
     c                   kfld                    ##rtyp
     c                   kfld                    ##fein
     c     t6ky2         klist
     c                   kfld                    xxtcc
     c                   kfld                    ##rtyp
     c                   kfld                    ##fein
     c                   kfld                    ##mod
      *
      *
      *  Retrun Premium Check Request file
      *
     c     r4ky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
      *
     c     r4ky1         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxCKDT
      *
     c     a4ky          klist
     c                   kfld                    xxco#
     c                   kfld                    r4fnd
     c                   kfld                    r4agt#
      *
      *_______________________________________________________________
      *
     c     datchk        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      *blanks       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
      *
     c     dtyear        plist
     c                   parm                    srdti
     c                   parm      '*MDYY'       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
     c*
     c     dtcymd        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
      *
     c     plname        plist
 05  c                   parm                    xxlast
 05  c                   parm                    xxfrst
 05  c                   parm      '1'           xxterm
      *
     c                   time                    dsdttm
      *
     c                   move      qqco#         xxco#
     c                   move      qqco#         zzco#
     c                   if        qqfnd > *zeros
     c                   move      qqfnd         xxfnd
     c                   endif
     c                   eval      f1099# = *zeros
     c                   move      qqyear        xxyear
     c                   move      qqyear        xxyr
     c*** Begin Add *** 11/09/22 *******************************************
     c                   Eval      rptYear    = %Subst(qqYear:3:2)
     c                   Eval      rptYearMPE = %Subst(qqYear:3:2)
     c*** End   Add *** 11/09/22 *******************************************
      *
      *  Setup date compare fields for excluding records
      *
     c                   movel     '0101'        xapfdt
     c                   move      qqyear        xapfdt
     c*
     c                   movel     '1231'        xaptdt
     c                   move      qqyear        xaptdt
     c*
     c                   move      xapfdt        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxpfdt
     c*
     c                   movel     srdto         xxCKDT                         WAR4P Check date
     c*
     c                   move      xaptdt        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxptdt
      *
      *  Setup dates for 90WFT groups for date included on 1099s
     c                   movel     '07012007'    zztrdt
     c                   move      zztrdt        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxtrdt187
      *
     c                   movel     '06042007'    zztrdt
     c                   move      zztrdt        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxtrdt615
      *
     c                   eval      grps  = %ELEM(grp)                           Number of Grps Allow
      *
      *
     c                   move      '-'           apehyp
     c                   move      '-'           bfehyp
     c                   move      '-'           bf1hyp
     c                   move      '-'           bf2hyp
     c                   move      'Y'           xxincl
      *
     c                   if        (q1prms >= 6)
     c                   move      qqincl        xxincl
     c                   endif                                                  Q1PRMS>=6
      *
      *  Initialize Constant Fields for the "B" records
      *
     c                   move      'B'           brtyp
     c                   move      xxyear        bptyr
     c                   move      *blank        bcrtn
     c                   move      *blank        bnctl
     c                   move      *blank        bintp
     c                   movel(P)  '01'          bpact
     c                   move      *blank        bpofc
     c                   move      *blank        bfill1
     c                   move      *zero         bpmt1
     c                   move      *zero         bpmt2
     c                   move      *zero         bpmt3
     c*** Begin Add *** 01/11/21 ************************************
     c                   move      *zero         bpmt30
     c*** End   Add *** 01/11/21 ************************************
     c                   move      *zero         bpmt4
     c                   move      *zero         bpmt5
     c                   move      *zero         bpmt6
     c*** Begin Add *** 01/11/21 ************************************
     c                   move      *zero         bpmt60
     c*** End   Add *** 01/11/21 ************************************
     c                   move      *zero         bpmt7
     c*** Begin Add ***01/11/21 ********************************************
     c                   move      *zero         bpmt70
     c*** End   Add ***01/11/21 ********************************************
     c                   move      *zero         bpmt8
     c                   move      *zero         bpmt9
     c                   move      *zero         bpmta
     c                   move      *zero         bpmtb
     c                   move      *zero         bpmtc
     c*** Begin Add *** 01/11/21 ************************************
     c                   move      *zero         bpmtc0
     c*** End   Add *** 01/11/21 ************************************
     c                   move      *zero         bpmtd
     c                   move      *zero         bpmte
     c*** Begin Add *** 01/25/22 *******************************************
     c                   move      *zero         bpmtf
     c                   move      *zero         bpmtg
     c                   move      *zero         bpmth
     c                   move      *zero         bpmtj
     c*** End   Add *** 01/25/22 *******************************************
     c                   move      *blank        bfill2
     c                   move      *blank        bfcin
     c                   move      *blank        bpnm1
     c                   move      *blank        bpnm2
     c                   move      *blank        bfill3
     c                   move      *blank        bprad
     c                   move      *blank        bpra2
     c                   move      *blank        bpcty
     c                   move      *blank        bpst
     c                   move      *blank        bpzip
     c                   move      *blank        bfill4
     c                   move      *blank        bfill5
     c                   move      *blank        bstcd
     c                   move      *blank        bfill6
     c*** Begin Add *** 01/25/22 *******************************************
     c                   move      *blank        bfill7
     c*** End   Add *** 01/25/22 *******************************************
     c                   move      *blank        bcrlf
      *
      *  Assign Indicator for Pinfeed or Laser Printer
     c     qqpofl        comp      'P'                                    81
     c     qqpofl        comp      'L'                                    82
      *
      *  Initialize Total Fields
      *
     c                   eval      xxtamt = *zero
     c                   eval      xxmamt = *zero
      *
     c                   endsr
      *_______________________________________________________________
      *
     o/EJECT
     o*
      *  Create 1099 Reporting file
     oWATNP     EADD         RCDA
     o                       ARTYP                1
     o                       APTYR                5
     o                       AFILL1              11
     o                       APRIN               20
     o                       APRNC               24
     o                       ALFIN               25
     o                       ARTTP               27
     o                       AAIND               41
      *
     o                       AFILL2              51
     o                       AFCOI               52
     o                       APRN1               92
     o                       APRN2              132
     o                       ATRAG              133
     o                       APRAD              173
     o                       APCTY              213
     o                       APST               215
     o                       APZIP              224
     o                       APPHN#             239
     o                       AFILL3             339
     o                       AFILL3             439
     o                       AFILL3             539
     o                       AFILL3             639
     o                       AFILL3             739
     o                       AFILL4             748
     o                       ACRLF              750
     o*** Begin Chg *** 01/25/22 *******************************************
     o**********EADD         RCDB
     o**********             BRTYP                1
     o**********             BPTYR                5
     o**********             BCRTN                6
     o**********             BNCTL               10
     o**********             BINTP               11
     o**********             BFEIN               20
     o**********             BPACT               40
     o**********             BPOFC               44
     o**********             BFILL1              54
     o**********             BPMT1               66
     o**********             BPMT2               78
     o**********             BPMT3               90
     o**********             BPMT4              102
     o**********             BPMT5              114
     o**********             BPMT6              126
     o*** Begin Chg ***01/11/21 ********************************************
     o**********             BPMT7              138
     o**********             BPMT70             138
     o*** End   Chg ***01/11/21 ********************************************
     o**********             BPMT8              150
     o**********             BPMT9              162
     o**********             BPMTA              174
     o**********             BPMTB              186
     o**********             BPMTC              198
     o**********             BPMTD              210
     o**********             BPMTE              222
     o**********             BFILL2             246
     o**********             BFCIN              247
     o**********             BPNM1              287
     o**********             BPNM2              327
     o**********             BFILL3             367
     o**********             XXRADR             407
     o**********             BFILL3             447
     o**********             BPCTY              487
     o**********             BPST               489
     o**********             BPZIP              498
     o**********             BFILL4             543
     o**********             BFILL5             662
     o**********             BSTCD              722
     o**********             BFILL6             748
     o**********             BCRLF              750
     o          EADD         RCDB
     o                       BRTYP                1
     o                       BPTYR                5
     o                       BCRTN                6
     o                       BNCTL               10
     o                       BINTP               11
     o                       BFEIN               20
     o                       BPACT               40
     o                       BPOFC               44
     o                       BFILL1              54
     o                       BPMT1               66
     o                       BPMT2               78
     o                       BPMT3               90
     o                       BPMT4              102
     o                       BPMT5              114
     o                       BPMT6              126
     o                       BPMT70             138
     o                       BPMT8              150
     o                       BPMT9              162
     o                       BPMTA              174
     o                       BPMTB              186
     o                       BPMTC              198
     o                       BPMTD              210
     o                       BPMTE              222
     o                       BPMTF              234
     o                       BPMTG              246
     o                       BPMTH              258
     o                       BPMTJ              270
     o                       BFILL2             286
     o                       BFCIN              287
     o                       BPNM1              327
     o                       BPNM2              367
     o                       XXRADR             407
     o                       BFILL3             447
     o                       BPCTY              487
     o                       BPST               489
     o                       BPZIP              498
     o                       BFILL7             499
     o                       BSEQ#              507
     o                       BFILL4             543
     o                       BFILL5             662
     o                       BSTCD              722
     o                       BFILL6             748
     o                       BCRLF              750
     o*** End   Chg *** 01/25/22 *******************************************
     o          EADD         RCDC
     o                       CRTYP                1
     o                       C#PAE                9
     o                       CFILL1              15
     o                       CTOT1               33
     o                       CTOT2               51
     o                       CTOT3               69
     o                       CTOT4               87
     o                       CTOT5              105
     o                       CTOT6              123
     o*** Begin Chg ***01/11/21 ********************************************
     o**********             CTOT7              141
     o                       CTOT70             141
     o*** End   Chg ***01/11/21 ********************************************
     o                       CTOT8              159
     o                       CTOT9              177
     o                       CTOTA              195
     o                       CTOTB              213
     o                       CTOTC              231
     o                       CTOTD              249
     o                       CTOTE              267
     o*** Begin Chg *** 01/25/22 *******************************************
     o**********             CFILLB             331
     o**********             CFILL2             431
     o**********             CFILL2             531
     o**********             CFILL2             631
     o**********             CFILL2             731
     o**********             CFILL3             748
     o**********             CCRLF              750
     o                       CTOTF              285
     o                       CTOTG              303
     o                       CTOTH              321
     o                       CTOTJ              339
     o                       CFILLB             431
     o                       CFILL2             531
     o                       CFILL2             631
     o                       CFILL2             731
     o                       CFILL3             748
     o                       CCRLF              750
     o*** End   Chg *** 01/25/22 *******************************************
     o*** Begin Add *** 01/11/21 ************************************
      *  Create 1099-NEC Reporting file
     oWATNPNEC  EADD         RCDANEC
     o                       ARTYP                1
     o                       APTYR                5
     o                       AFILL1              11
     o                       APRIN               20
     o                       APRNC               24
     o                       ALFIN               25
     o                       ARTTPNEC            27
     o                       AAINDNEC            41
      *
     o                       AFILL2              51
     o                       AFCOI               52
     o                       APRN1               92
     o                       APRN2              132
     o                       ATRAG              133
     o                       APRAD              173
     o                       APCTY              213
     o                       APST               215
     o                       APZIP              224
     o                       APPHN#             239
     o                       AFILL3             339
     o                       AFILL3             439
     o                       AFILL3             539
     o                       AFILL3             639
     o                       AFILL3             739
     o                       AFILL4             748
     o                       ACRLF              750
     o*** Begin Chg *** 01/25/22 *******************************************
     o**********EADD         RCDBNEC
     o**********             BRTYP                1
     o**********             BPTYR                5
     o**********             BCRTN                6
     o**********             BNCTL               10
     o**********             BINTP               11
     o**********             BFEIN               20
     o**********             BPACT               40
     o**********             BPOFC               44
     o**********             BFILL1              54
     o**********             BPMT7               66
     o**********             BPMT2               78
     o**********             BPMT30              90
     o**********             BPMT4              102
     o**********             BPMT5              114
     o**********             BPMT60             126
     o**********             BPMT70             138
     o**********             BPMT8              150
     o**********             BPMT9              162
     o**********             BPMTA              174
     o**********             BPMTB              186
     o**********             BPMTC0             198
     o**********             BPMTD              210
     o**********             BPMTE              222
     o**********             BFILL2             246
     o**********             BFCIN              247
     o**********             BPNM1              287
     o**********             BPNM2              327
     o**********             BFILL3             367
     o**********             XXRADR             407
     o**********             BFILL3             447
     o**********             BPCTY              487
     o**********             BPST               489
     o**********             BPZIP              498
     o**********             BFILL4             543
     o**********             BFILL5             662
     o**********             BSTCD              722
     o**********             BFILL6             748
     o**********             BCRLF              750
     o          EADD         RCDBNEC
     o                       BRTYP                1
     o                       BPTYR                5
     o                       BCRTN                6
     o                       BNCTL               10
     o                       BINTP               11
     o                       BFEIN               20
     o                       BPACT               40
     o                       BPOFC               44
     o                       BFILL1              54
     o                       BPMT7               66
     o                       BPMT2               78
     o                       BPMT30              90
     o                       BPMT4              102
     o                       BPMT5              114
     o                       BPMT60             126
     o                       BPMT70             138
     o                       BPMT8              150
     o                       BPMT9              162
     o                       BPMTA              174
     o                       BPMTB              186
     o                       BPMTC0             198
     o                       BPMTD              210
     o                       BPMTE              222
     o                       BPMTF              234
     o                       BPMTG              246
     o                       BPMTH              258
     o                       BPMTJ              270
     o                       BFILL2             286
     o                       BFCIN              287
     o                       BPNM1              327
     o                       BPNM2              367
     o                       XXRADR             407
     o                       BFILL3             447
     o                       BPCTY              487
     o                       BPST               489
     o                       BPZIP              498
     o                       BFILL7             499
     o                       BSEQ#NEC           507
     o                       BFILL4             543
     o                       BFILL5             662
     o                       BSTCD              722
     o                       BFILL6             748
     o                       BCRLF              750
     o*** End   Chg *** 01/25/22 *******************************************
     o          EADD         RCDCNEC
     o                       CRTYP                1
     o                       C#PAENEC             9
     o                       CFILL1              15
     o                       CTOT7               33
     o                       CTOT2               51
     o                       CTOT30              69
     o                       CTOT4               87
     o                       CTOT5              105
     o                       CTOT60             123
     o                       CTOT70             141
     o                       CTOT8              159
     o                       CTOT9              177
     o                       CTOTA              195
     o                       CTOTB              213
     o                       CTOTC0             231
     o                       CTOTD              249
     o                       CTOTE              267
     o*** Begin Chg *** 01/25/22 *******************************************
     o**********             CFILLB             331
     o**********             CFILL2             431
     o**********             CFILL2             531
     o**********             CFILL2             631
     o**********             CFILL2             731
     o**********             CFILL3             748
     o**********             CCRLF              750
     o                       CTOTF              285
     o                       CTOTG              303
     o                       CTOTH              321
     o                       CTOTJ              339
     o                       CFILLB             431
     o                       CFILL2             531
     o                       CFILL2             631
     o                       CFILL2             731
     o                       CFILL3             748
     o                       CCRLF              750
     o*** End   Chg *** 01/25/22 *******************************************
     o*** End   Add *** 01/11/21 ************************************
      *
      *  Reports for Providers, Agents and Manual entries
     oQSYSPRT   E            ROUTE          1 06
     o          E            ROUTE          1
     o                                           15 'Route To:'
     o                       ORDIS1              60
     o          E            ROUTE          1
     o                       ORDIS2              60
     o          E            ROUTE          1
     o                       ORDIS3              60
     o          E            ROUTE          2
     o                       ORDIS4              60
     o          E            ROUTE          2
     o                                           19 'Report Title:'
     o                       ORTITL              80
     o          E            ROUTE          2
     o                                           19 'Company nmbr:'
     o                       qqco#               23
     o                                           37 'Tx Yr:'
     o                       qqyear              42
     o                                           56 'Type:'
     o                       qqcomm              58
     o                                           80 'Laser/pinfeed:'
     o                       qqpofl              82
     o                                          113 'Include manual entries:'
     o                       qqincl             115
     o          E            ROUTEG         1
     o                                           12 'Group:'
     o                       GRP(Z)        Z   +001
     o                       GNM(Z)            +002
      *
     O          e            routec         1 59
     O                       #brnd1
     O          e            routec         1
     O                       #brnd2
     o                       *YEAR             +001
     o                       DSDATE             190 '  /  /    '
     o          E            HEAD           1 06
     o                       DSTIME              08 '  :  :  '
     o                       CONAME             120
     o                       DSDATE             198 '  /  /    '
     o          E            HEAD           2
     o                       Q1PGM               10
     o                       ORTITL             129
     o                                          192 'Page:'
     o                       PAGE          Z    198
     o                     78                   102 '*** Manual Entries ***'
     o          E            HEADB          2
     o                    N30                    10 'Payee Fein'
     o                    N30                    22 'Payee Name'
     o                     30                    10 'Agent Fein'
     o                     30                    22 'Agent Name'
     o                                           61 'Address'
     o                                          108 'City'
     o                                          130 'ST'
     o                                          140 'ZIP Code'
     o                                          155 'Awards/Prizes'
     o                                          169 'MED/HLTH'
     o                                          184 'Non-EMPL'
     o                                          198 'Attorney'
      *
     o          E            DETLA          2
     o                                           14 'Payment Year:'
     o                       APTYR             +002
     o                                           33 'Payers FEIN:'
     o                       APRIN               43
     o                                           69 'Transmitter Ctl Code:'
     o                       ATCC                75
     o                                           95 'Type of Return:'
     o                                          105 '1099-MISC'
     o          E            DETLA          1
     o                                            7 'Payer:'
     o                       APRN1               50
     o          E            DETLA          1
     o                       APRAD               50
     o          E            DETLA          1
     o                       APRAD2              50
     o          E            DETLA          1
     o                       APRCS               50
     o          E            DETLA          2
     o                       APPH#               23
     o          E            DETLB          1
     o              N51      BFEIN1              10
     o               51      BFSS#1              11
     o                       BPNM1               52
     o                       XXRADR              94
     o                       XXCTY              125
     o                       BPST               130
     o                       BPZIP              141
     o                       BPMT3              154
     o                       BPMT6              169
     o                       BPMT7              184
     o                       BPMTC              198
     o          E            TOTL        1  1
     o                                           19 'Total # of PAYEES'
     o                                           36 'Receiving 1099s:'
     o                       COUNTB        3     44
      *
     o                       CTOT3              154
     o                       CTOT7              184
     o          E            TOTL           2
     o                       CTOT6              169
     o                       CTOTC              198
      *
      * Error Listing
     oQSYSPRT1  E            HEAD1          1 06
     o                       DSTIME              08 '  :  :  '
     o                       CONAME             120
     o                       DSDATE             198 '  /  /    '
     o          E            HEAD1          2
     o                       Q1PGM               10
     o                       ORTITL             129
     o                                          192 'Page:'
     o                       PAGE1         Z    198
      *
     o          E            HEAD1          2
     o                                          111 'E R R O R    R E P O R T'
      *
     o          E            ERR1           1
     o                                            8 'Payee #'
     o                       PAFEIN              18
     o                                           36 'Not in PAYEE Mast'
     o                                           41 'FYR-'
     o                       PAFYR               44
     o                                           50 'Case-'
     o                       PACASE              57
     o                                           62 'CK#-'
     o                       PACHK#              69
     o                                           75 'Date-'
     o                       PATRDT              81
     o                                           86 'AMT-'
     o                       PAAMT         L     96
     o                                          103 'RCDTP-'
     o                       PATYPE             104
     o                                          111 'RSVTP-'
     o                       PARETP             112
     o                                          122 'Claimant-'
     o                       XXCLNM             162
     o          E            ERR2           1
     o                                           16 '**Funds FEIN or'
     o                                           41 'Transmitter Control Code'
     o                                           66 'DOESNT EXIST in the Fund'
     o                                           78 'Detail File'
     o          E            ERR3           1
     o                       ACAGT#        Z     05
     o               25                          30 'Agent FED ID Not Found'
     o               25                          45 'for Modifier: '
     o               25      acmod               48
     o               25                          61 'and Group: '
     o               25      acfnd               64
     o               26                          31 'Agent Address Not Found'
     o               26                          41 'for FEIN: '
     o               26      acfein              50
     o               26                          65 'and Modifier: '
     o               26      acmod               68
      *
     o          E            ERR4           1
     o                                           26 'Negative Amt in 1099 for: '
     o                       t6rfen
     o                       t6rnam           +   2
     o                       t6amt3        L
     o                                        +   1 'Box 3'
     o                       t6amt6        L  +   1
     o                                        +   1 'Box 6'
     o                       t6amt7        L  +   1
     o                                        +   1 'Box 7'
     o                       t6amtc        L  +   1
     o                                        +   1 'Box 14'
      *
      * Pinfeed 1099 Forms - Two to a page
     oQSYSPRT2  E            FORM             06
     o                       APRN1               53
     o          E            FORM             07
     o                       APRAD               53
     o          E            FORM             08
     o                       APRAD2              53
     o          E            FORM             09
     o                       APRCS               53
     o          E            FORM             10
     o                       APPH#               26
     o          E            FORM             11
     o                       XPMT3         4     72
     o          E            FORM             14
     o              N51      BFEIN1              49
     o               51      BFSS#1              50
     o                       APRIN1              25
     o                       XPMT6         4     93
     o          E            FORM             18
     o                       BPNM1               53
     o                       XPMT7         4     72
     o          E            FORM             20
     o                       BPRAD               53
     o          E            FORM             21
     o                       BPRA2               53
     o          E            FORM             23
     o                       BXCSZ               53
      *
     o          E            FORM             26
      *
     o                       XPMTC         4     93
      *
      * Print for Labels
     oQSYSPRT6  E            LABEL1            5
     o          E            LABEL          1
     o                       X1NAME              45
     o                       X2NAME              95
     o          E            LABEL          1
     o                       X1ADDR              45
     o                       X2ADDR              95
     o          E            LABEL          3
     o                       X1CSZ               45
     o                       X2CSZ               95
     o          E            LABEL          1
     o          E            LABEL2         1
     o                       X1NAME              45
     o                       X2NAME              95
     o          E            LABEL2         1
     o                       X1ADDR              45
     o                       X2ADDR              95
     o          E            LABEL2         1
     o                       X1ADD2              45
     o                       X2ADD2              95
     o          E            LABEL2         2
     o                       X1CSZ               45
     o                       X2CSZ               95
     o          E            LABEL2         1
      ** ========================================================= **
