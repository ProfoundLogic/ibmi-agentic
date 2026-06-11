     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W8901R1 - WORK FILE BUILD - POLICY FILE - WHPOP
     H*****************************************************************
     H* DESCRIPTION:                                                  *
     H*                                                               *
     H*   DATA WAREHOUSE - POLICY FILE                                *
     H*                                                               *
     H* CREATED: 04/04/00  C:61                                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 08/12/09 C:33  Project: CI077                                 *
     H*                Expand name.                                   *
     H*                                                               *
     H* 09/04/08 C:62  PROJECT WC1611                                 *
     H*                USE ELNWRW                                     *
     H*                                                               *
     H* 01/23/08 C:61  SUPPORT CI132 61860                            *
     H*                - Add mailing address fields.                  *
     H*                                                               *
     H* 11/14/07 C: 132   CI071 - 60095                               *
     H*           - Change bypass date to November 26th.              *
     H*                                                               *
     H* 09/27/07 C: 132   CI071 - 60095                               *
     H*           - Test Future year bypass for ECM (071).  Will      *
     H*             bypass all policies in future year if run date is *
     H*             before Nov. 15th of run date year.  These would   *
     H*             be pending policies - WHPOP/POSTCD = "P".         *
     H*                                                               *
     H* 05/15/07 C: 132   CI091 - 58355                               *
     H*           - Correct code to not write cancel date if cancel   *
     H*             date = reinstall date in WDCXP file & cancel date *
     H*             in WMEMP file is later than these dates.          *
     H*           - Fix Cancel date when policy end date is differnt  *
     H*             than the group policy end date (cancel & audited) *
     H*                                                               *
     H* 02/09/07 C: 132   CI132 - 57079                               *
     H*             Get Correct Carrier policy number when policy     *
     H*             year changes for same employer.                   *
     H*                                                               *
     H* 07/25/06 C: 132   CI031                                       *
     H*           - Remove the code to insert Carrier Policy number   *
     H*             in WHPOP/POPONR field, use Grp-Emp#-Fyr as Policy *
     H*             number.                                           *
     H*           - If WMRPP/RPCAR# is blank, load POPONR in POCAR#.  *
     H*                                                               *
     H* 09/06/05 C: 132   CI132 - 48582                               *
     H*             Change the check on Cancel date to see if flat    *
     H*             Cancelled. The blank cancel reason before chain   *
     H*             to cancel reason file, in case don't get hit      *
     H*                                                               *
     H* 08/26/05 C: 132 WA135 - 48597                                 *
     H*             Change the Key on WMRPL to a saved DIV# and  will *
     H*             break properly when Division an Policy Year chgs. *
     H*             Then chain to get current year WMRPP/RPCAP#       *
     H*                                                               *
     H* 08/12/05 C: 132 CI093 - 47984                                 *
     H*             Retrieve the Client ID and if is KRMS (093) then  *
     H*             bypass the check on WDEHP and always write record *
     H*                                                               *
     H* 08/05/05 C: 132 WA091 - 48054                                 *
     H*             Change check on Cancel date to be LT policy end   *
     H*             date. If is EQ policy end date is a Non-Renew     *
     H*             policy.                                           *
     H*                                                               *
     H*  07/22/05       C: 58      Project: 135WEH
     H*        For extracted policy#, use WMRPP/carrier number:
     H*          For: client 135;
     H*                                                               *
     H* 04/04/05 C: 132 129QKH                                        *
     H*           - Add SA001R call to get client number from SSYPF   *
     H*             If it is client 129, do following changes:        *
     H*           - Replace underwriter First Name, Last Name and     *
     H*             Email Address fields in WHPOP w/Policy Services   *
     H*             Staff information using WMWMP/EMPSRV to get info  *
     H*             from WMUPP file.                                  *
     H*           - Remove the Underwriter print line, Policy svcs    *
     H*                                                               *
     H* 11/15/04 C: 132 CI021                                         *
     H*             Add check on WDEHP file when a policy start date  *
     H*             in WDELL is in the future. If no WDEHP record     *
     H*             for future start dates, do not write the record.  *
     H*                                                               *
     H* 08/11/04 C: 132 91DLK                                        *
     H*             Add Exclude Employers file WHXPP, Check file when *
     H*             Write WHPOP record, if exists in WHXPP file then  *
     H*             do not write the record.                          *
     H*                                                               *
     H* 03/25/04 C: 132 CI010   and CI015                             *
     H*           - Add start Date PARM for Job Date, so each file    *
     H*             has same ASOF DATE, for next incremetal.  CI010   *
     H*                                                               *
     H* 03/10/04 C: 132   CI015  (added with 03/24/04 Modification)   *
     H*           - Add start fund year PARM; Extract Date from that  *
     H*             policy year forward, skip older years.    CI015   *
     H*                                                               *
     H* 02/27/04 C:132 SUPPORT CI099 40909                            *
     H*                Check for certain hex characters in output     *
     H*                alpha fields and replaced if exists. Fields    *
     H*                from tables and static files are not checked   *
     H*                                                               *
     H* 11/04/03 C:132 SUPPORT WA121 38407                            *
     H*                Add routing file, and logic to print routing   *
     H*                page if have any errors.                       *
     H*                                                               *
     H* 10/08/03 C:132 SUPPORT WA072 39332                            *
     H*                Chg to get State Reporting Division Cancel &   *
     H*                Policy begin & ending dates.                   *
     H*                                                               *
     H* 12/11/02 C:61  SUPPORT WA081 35144                            *
     H*                Get most recent ACTIVE CX reason code record.  *
     H*                                                               *
     H* 05/30/02 C:132 PROJECT WC924                                  *
     H*                Change Carrier ID = EMCO# + EMFND left justify *
     H*                Add new fields - Insured Email address         *
     H*                               - Insured Website address       *
     H*                               - Underwriter last, first name  *
     H*                               - Underwriter Email address     *
     H*                                                               *
     H* 04/24/02  C:132 WCI091 SEQ: 32217                             *
     H*               - Change: Chg Extact date to be > Maint date    *
     H*                                                               *
     H* 03/18/02  C:132 PROJECT WC924                                 *
     H*               - Change: FIX Cancel Logic to Code future       *
     H*                 policy date as Pending "P" code               *
     H*                                                               *
     H* 03/01/02  C:132 PROJECT WC924                                 *
     H*               - Change: Chg Cancel Logic to include future    *
     H*                 Cancel Date but not mark as canceled          *
     H*                                                               *
     H* 02/22/02  C:132 PROJECT WC924                                 *
     H*               - Change: Save ELDIV to SVDIV, ELFYR to SVFYR   *
     H*                 when ELCO# or ELFND or ELEMP# changes         *
     H*                                                               *
     H* 11/07/01  C:132 PROJECT WC924                                 *
     H*               - Change: Check for WMSTRP flay in WMEMP & use  *
     H*                 that EMNAME & ADDRESS IN OUTPUT               *
     H*                                                               *
     H* 10/02/01  C:132 PROJECT WC924                                 *
     H*               - Change: Add a check new FNCICF flag in WMFNP  *
     H*                 for a 'Y' to include in Extract = Else Bypass *
     H*                                                               *
     H* 08/28/01  C:132 PROJECT WC924                                 *
     H*               - Change to check WDCXP File for Correct Cancel *
     H*                 Date for Fund Year using State Reporting DIV  *
     H*                 dnd Fund Year-GET Latest Date Record          *
     H*                                                               *
     H* 08/23/01  C:132 PROJECT WC924                                 *
     H*               - Change to check Policy From, To and Cancel    *
     H*                 Dates are same, if so is a Cancel Not Expired *
     H*                 Change Location of Date Check for correct     *
     H*                 Caluclations of Codes                         *
     H*                                                               *
     H* 08/20/01  C:132 PROJECT WC924                                 *
     H*               - Change to Chain to Output file to see if Policy
     H*                 Year Exists. If so, bypass the output of new  *
     H*                 Record to WHPOP.                              *
     H*                                                               *
     H* 08/15/01  C:132 PROJECT WC924                                 *
     H*               - If CANCEL Date between Policy From Date and
     H*                 Policy To Date, Status Code is 'C" for YR
     H*                 Only put Cancel Date in that year             *
     H*                                                               *
     H* 08/14/01  C:132 PROJECT WC924                                 *
     H*               - If FEIN is blank or Zero, Use EMFND & EMEMP#
     H*                 as POISCD (Insured Code) Left Adjusted
     H*                                                               *
     H* 08/10/01  C:132 PROJECT WC924                                 *
     H*               - Add Spool Report for Duplicate Records for
     H*                 Same Yr, Div, Fnd in WDELL files
     H*                                                               *
     H* 08/09/01  C:132 PROJECT WC924                                 *
     H*               - Chg Bypass of Divisions that are not in the
     H*               - State Reporting Flag DIV, Report on all DIVs
     H*                                                               *
     H* 07/30/01  C:132 PROJECT WC924                                 *
     H*               - Incoming Date PARM from WHXCP file - Use to   *
     H*                 Select WDELL Records, Don't Process any Rcds  *
     H*                 Equal or Before this date                     *
     H*                                                               *
     H* 07/25/01  C:03 PROJECT WC924                                  *
     H*               - CHG MAIL/PHY FLAG
     H*               - CHG INSURED CODE TO FEIN
     H*                                                               *
     H* 07/11/01  C:03 PROJECT WC924                                  *
     H*               - ADDED policy status code.                     *
     H*                  OF "P" FOR PENDING (POLICY IN THE FUTURE)    *
     H*                                                               *
     H* 02/06/01  C:61 PROJECT WC924                                  *
     H*               - Load InsuredCode = GrpEmployer                *
     H*               - Load CarrierPolicyNo                          *
     H*               - CarrierId = Grp# left aligned                 *
     H*               - Load policy status code.                      *
     H*                  (I=Inforce, E=Expired, C=Cancelled           *
     H*                                                               *
     H* 06/10/00  C:58 PROJECT WC993
     H*                 Select only non-test groups.
     H*                                                               *
     H* 05/15/00  C:58 PROJECT WC924
     H*                 ADD field: POAODT - Data AS OF date.
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
      *
      * Employer Detail
     FWDELL     IF   E           K DISK
     FWDELL8    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
      *
      * Employer Master
     FWMEML     IF   E           K DISK
      *
      * Employer Detail History
     FWDEHL1    IF   E           K DISK
      *
      * Fund Master
     FWMFNL     IF   E           K DISK
      *
      * A/R
     FWMEAL10   IF   E           K DISK
      *
      * County
     FWTCNL1    IF   E           K DISK
      *
      * Table
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PCN01)
      *
      * Generic Table
     FWTGTL     IF   E           K DISK
      *
      * Cancellation Detail - Descending Cancel Date Sequence
     FWDCXL4    IF   E           K DISK
      *
      * Rate Set code
     FWMRSL     IF   E           K DISK
      *
      *  Chapter code
     FWMCPL     IF   E           K DISK
      *
      *  Region code
     FWMREL     IF   E           K DISK
      *
      *  Carrier / state reporting master
     FWMRPL     IF   E           K DISK
      *
      *  Cancellation reason table
     FWTCRL     IF   E           K DISK
      *
      * Output - Policy file layout
     FWHPOP     IF A E           K DISK
      *
      ****** BEG ADD ***** 08/11/04 ************************************
     FWHXPL     IF   E           K DISK
      ****** END ADD ***** 08/11/04 ************************************
     F*
     F*  AUDIT REPORT
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOB)
      *
      **** BEGIN ADD *** 05/30/02 ************************************
     F*    User Parameter File.
     FWMUPL1    IF   E           K DISK
     F*
      **** END ADD ***** 05/30/02 ************************************
     F*
      ***** BEG ADD ***** 11/04/03 ***********************************
     FSMORL     IF   E           K DISK
      ***** END ADD ***** 11/04/03 ***********************************
     F*

     D*
     D* Effective and Cancel Date Arrays
     D*
      *
      ***** BEG ADD ***** 02/27/04 ************************************
     D HX              S              1    DIM(60)                              HEX CNVT ARRY
      ***** END ADD ***** 02/27/04 ************************************
      *
      *
     D                 DS
     D  ED                     1     35  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS
     D  CD                     1     35  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D                 DS
     D  X1DATE                 1      8
     D  X1MM                   1      2
     D  X1DD                   3      4
     D  X1YY                   5      8
     D*
      **** BEGIN ADD *** 05/30/02 ************************************
     D                 DS
     D  ##CID                  1      6
     D  EMCO#                  1      3  0
     D  EMFND                  4      6  0
      **** END ADD ***** 05/30/02 ************************************
      *
      ***** BEG ADD ***** 05/15/07 ***************************************
     D FLATCN          C                   CONST('FLAT CANCELLED FOR T-
     D                                     HIS POLICY YEAR')
      ***** END ADD ***** 05/15/07 ***************************************
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      ************************************************************
      *  KLISTS
      ************************************************************
      *
      *  Employer
      *

     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
      *
      *  Employer Detail History
      *
     C     EHKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      *
      *  A/R
      *
     C     EAKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
      *
      *  Table
      *
     C     TBKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    KKKEY
      *
      *  Generic Table
      *
     C     GTKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
      *
      *  Cancellation Detail & History
      *
     C     CXKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    ELFYR
      *
      *  Rate set code file
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    KKRTCD
      *
      *  Cancellation reason code file
      *
     C     CRKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    KKST
     C                   KFLD                    K1KEY
     C                   KFLD                    KKEFDT
      *
     C     CRKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    KKST
     C                   KFLD                    K1KEY
      *
      *  Employer Detail
      *
     C     ELKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    XXFYR
      *
      *  Chapter Code
      *
     C     CPKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMCHAP
      *
      *  Region
      *
     C     REKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMRGN#
      *
      *  Carrier / state reporting master
      *
      ******BEG*DEL ***** 02/09/07 ************************************
     C********** RPKY      KLIST
     C**********           KFLD           ELCO#
     C**********           KFLD           ELFND
     C**********           KFLD           ELEMP#
      ******BEG*CHG ***** 08/26/05 ************************************
     C**********           KFLD           ZZDIV
     C**********           KFLD           ZZFYR
     C***************      KFLD           ELDIV
     C***************      KFLD           ELFYR
      ******END*CHG ***** 08/26/05 ************************************
      ***** END DEL ***** 02/09/07 ************************************
      *
      ***** BEG ADD ***** 02/09/07 ************************************
     C     RPKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      ***** END ADD ***** 02/09/07 ************************************
      *
      *  Fund
      *
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
      *
      ***** BEG ADD ***** 11/04/03 ***********************************
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
      ***** END ADD ***** 11/04/03 ***********************************
      *
      *  Warehouse Output file - WHPOP
      *
     C     POKY          KLIST
     C                   KFLD                    POPONR
      *
      ****** BEG ADD ***** 08/11/04 ************************************
      *
      *  Employer Excluded file
      *
     C     XPKY          KLIST
     C                   KFLD                    SACO#
     C                   KFLD                    SAFND
     C                   KFLD                    SAEMP#
     C                   KFLD                    SADIV
     C                   KFLD                    SAFYR
      *
     C     XPKY2         KLIST
     C                   KFLD                    SACO#
     C                   KFLD                    SAFND
     C                   KFLD                    SAEMP#
      ****** END ADD ***** 08/11/04 ************************************
      *
      ************************************************************
      *   *LIKE defines
      ************************************************************
      *
     C     *LIKE         DEFINE    ELCO#         SVCO#
     C     *LIKE         DEFINE    ELFND         SVFND
     C     *LIKE         DEFINE    ELEMP#        SVEMP#
     C     *LIKE         DEFINE    ELDIV         SVDIV
     C     *LIKE         DEFINE    ELFYR         SVFYR
     C     *LIKE         DEFINE    ELRTCD        KKRTCD
     C     *LIKE         DEFINE    ELPFDT        XXRNDT
      ****** BEG ADD ***** 09/27/07 ************************************
     C     *LIKE         DEFINE    ELPFDT        XRENDT
      ****** END ADD ***** 09/27/07 ************************************
     C     *LIKE         DEFINE    ELFYR         XXFYR
      *
     C     *LIKE         DEFINE    POCLPR        SVCLPR
     C     *LIKE         DEFINE    POCLSC        SVCLSC
     C     *LIKE         DEFINE    POPTAX        SVPTAX
     C     *LIKE         DEFINE    POPONR        SVPONR
      *
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
      *
     C     *LIKE         DEFINE    CRST          KKST
     C     *LIKE         DEFINE    CREFDT        KKEFDT
     C     *LIKE         DEFINE    CRKEY         K1KEY
      *
     C     *LIKE         DEFINE    ELPFDT        XXDATE
     C     *LIKE         DEFINE    EMCAD1        XXCXDT
      *
      ****** BEG ADD ***** 08/11/04 ************************************
     C     *LIKE         DEFINE    ELCO#         SACO#
     C     *LIKE         DEFINE    ELFND         SAFND
     C     *LIKE         DEFINE    ELEMP#        SAEMP#
     C     *LIKE         DEFINE    ELDIV         SADIV
     C     *LIKE         DEFINE    ELFYR         SAFYR
      ****** END ADD ***** 08/11/04 ************************************
      *
      ***** BEG ADD ***** 08/26/05 ************************************
     C     *LIKE         DEFINE    ELDIV         ZZDIV
     C     *LIKE         DEFINE    ELFYR         ZZFYR
     C     *LIKE         DEFINE    RPCAP#        SVCAP#
      ***** END ADD ***** 08/26/05 ************************************
      *
      ************************************************************
      *  PLIST definitions
      ************************************************************
      *
      *  @@RNDT is the Last Extract Date - From WXCHP
     C     *ENTRY        PLIST
     C                   PARM                    @@RNDT            8
      ***** BEG ADD ***** 03/25/04 ************************************
     C                   PARM                    @@JBDT            8
      ***** END ADD ***** 03/25/04 ************************************
     C***** BEG ADD ******* 03/10/04 **********************************
     C                   PARM                    @@STYR            3 0
     C***** END ADD ******* 03/10/04 **********************************
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      ************************************************************
      *  Begin
      ************************************************************
      *
      *  INITIALIZE
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVEL     @@RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      *OFF
      *
     C                   MOVEL     @@RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
      **** BEGIN ADD *** 05/30/02 ************************************
     C                   MOVE      @@RNDT        ##RNDT            8 0
      **** END ADD ***** 05/30/02 ************************************
      *
     C                   ELSE
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
      **** BEGIN ADD *** 05/30/02 ************************************
     C                   Z-ADD     DSDATE        ##RNDT
      **** END ADD ***** 05/30/02 ************************************
      *
     C                   ENDIF                                                  SRERR
      *
      ****** BEG ADD ***** 09/27/07 ************************************
      * Setup a match date for ECM for new policies, for excluding renewals
      * before the Nov.15th date.
     C                   MOVEL     @@RNDT        ZRENDT            8
      ****** BEG CHG ***** 11/14/07 ************************************
     C************         MOVEL'1115'    ZRENDT
     C                   MOVEL     '1126'        ZRENDT
      ****** BEG CHG ***** 11/14/07 ************************************
     C                   MOVEL     ZRENDT        ZXRNDT            8 0
     C                   Z-ADD     ZXRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XRENDT                         cyy1115
      ****** END ADD ***** 09/27/07 ************************************
      *
     C**** Begin Add ****  07/22/05  **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C**** End Add ******  07/22/05  **************************************
      *
      ***** BEG ADD ***** 08/26/05 ************************************
     C     CLTID         IFEQ      '135'
     C                   MOVE      *ON           *IN50
     C                   ELSE
     C                   MOVE      *OFF          *IN50
     C                   ENDIF
      ***** END ADD ***** 08/26/05 ************************************
      *
      ***** BEG ADD ***** 11/04/03 ***********************************
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C*
     C*  Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF                                                  N<=60
     C                   ENDIF                                                  ORTITL<>*BLANKS
      ***** BEG ADD ***** 01/28/10 ***********************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
      ***** END ADD ***** 01/28/10 ***********************************
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C*
      ***** END ADD ***** 11/04/03 ***********************************
      *
      ***** BEG ADD ***** 04/04/05 ***********************************
     C                   CALL      'SA001R'
     C                   PARM                    @@WCIF            1
     C                   PARM                    @@MCIF            1
     C                   PARM                    @@WCIN            1
     C                   PARM                    @@AR              1
     C                   PARM                    @@NTWK            1
     C                   PARM                    @@UR              1
     C                   PARM                    @@MED             1
     C                   PARM                    @@EDI             1
     C                   PARM                    @@WCDK            1
     C                   PARM                    @@RHAB            1
     C                   PARM                    @@MSS             1
     C                   PARM                    @@FSCH            1
     C                   PARM                    @@PRVB            1
     C                   PARM                    @@NPWC            1
     C                   PARM                    @@OS              1
     C                   PARM                    @@ARLS            1
     C                   PARM                    @@CCOD            1
     C                   PARM                    @@CRVW            1
     C                   PARM                    @@PROM            1
     C                   PARM                    @@MCRV            1
     C                   PARM                    @@FREQ            1
     C                   PARM                    @@CRAC            1
     C                   PARM                    @@PREO            1
     C                   PARM                    @@PARN            1
     C                   PARM                    @@MGRI            1
     C                   PARM                    @@KCIF            1
     C                   PARM                    @@CURS            1
     C                   PARM                    @@PCIB            1
     C                   PARM                    @@CLT#            3
     C                   PARM                    @@UCPF            1
     C                   PARM                    @@DCF             1
     C                   PARM                    @@AUTB            1
     C*
     C* If is client 129, seton switch to process Policy Service rep.
     C                   MOVE      *OFF          *IN45
     C     @@CLT#        IFEQ      '129'
     C                   MOVE      *ON           *IN45
     C                   END
      ***** END ADD ***** 04/04/05 ***********************************
      *
     C     *LIKE         DEFINE    ELPFDT        XXDT
      *
      ***** BEG CHG ***** 03/25/04 ************************************
      * Change the Parm Date into the AS OF Date for output record
     C                   MOVEL     @@JBDT        ZZRNDT            8 0
     C**********           Z-ADDDSDATE    SRDTI
     C                   Z-ADD     ZZRNDT        SRDTI
      ***** END CHG ***** 03/25/04 ************************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
      *
     C                   Z-ADD     *ZERO         SVCO#
     C                   Z-ADD     *ZERO         SVFND
     C                   Z-ADD     *ZERO         SVEMP#
     C                   Z-ADD     *ZERO         SVDIV
     C                   Z-ADD     *ZERO         SVFYR
      *
     C                   Z-ADD     *ZERO         INCNT             7 0
     C                   Z-ADD     *ZERO         OTCNT             7 0
     C                   Z-ADD     *ZERO         ERCNT             7 0
     C                   Z-ADD     *ZERO         BYCNT             7 0
     C                   Z-ADD     *ZERO         LECNT             7 0          < LAST EXTRACT
     C                   Z-ADD     *ZERO         FYCNT             7 0
     C                   Z-ADD     *ZERO         DTCNT             7 0
      *
     C                   EXCEPT    HEAD
      *
      *  READ EMPLOYER DETAIL
      *
     C                   READ      PEL01                                  60
     C     *IN60         DOWEQ     '0'
     C                   ADD       1             INCNT
      *
      ****** BEG ADD ***** 08/11/04 ************************************
     C                   Z-ADD     ELCO#         SACO#
     C                   Z-ADD     ELFND         SAFND
     C                   Z-ADD     ELEMP#        SAEMP#
     C                   Z-ADD     ELDIV         SADIV
     C                   Z-ADD     ELFYR         SAFYR
      ****** END ADD ***** 08/11/04 ************************************
     C*
     C*   Selection: bypass all test groups.
     C     *LIKE         DEFINE    ELFND         XXFND
     C     ELCO#         IFEQ      *ZERO
     C                   ADD       1             BYCNT
     C                   GOTO      NEXT
     C                   ENDIF
     C*
     C     ELCO#         IFNE      SVCO#
     C     ELFND         ORNE      SVFND
     C     FNKY          CHAIN     PFN01                              63
     C     *IN63         IFEQ      *ON
     C                   MOVEL     'Y'           FNTEST
     C                   ENDIF                                                  .63 = ON
     C                   ENDIF                                                  .CO/FND <>
     C*
     C     FNTEST        IFEQ      'Y'
     C     FNCICF        ORNE      'Y'
     C*
     C                   ADD       1             BYCNT
     C                   GOTO      NEXT
     C                   ENDIF
      *
      *   Set flag to indicate if policy has been renewed
      *
     C     ELCO#         IFEQ      SVCO#
     C     ELFND         ANDEQ     SVFND
     C     ELEMP#        ANDEQ     SVEMP#
     C     ELDIV         ANDEQ     SVDIV
     C                   MOVE      'Y'           XXREN             1
      *
     C                   ELSE
      *
     C                   MOVE      'N'           XXREN
     C                   ENDIF
      *
      *   Check for same Policy Year record, if so is Error, Print it
     C     ELCO#         IFEQ      SVCO#
     C     ELFND         ANDEQ     SVFND
     C     ELEMP#        ANDEQ     SVEMP#
     C     ELDIV         ANDEQ     SVDIV
     C     ELFYR         ANDEQ     SVFYR
     C                   ADD       1             ERCNT
     C   OB              EXCEPT    HEAD
     C                   EXCEPT    DETL
     C                   GOTO      NEXT
     C                   ENDIF
      *
      *   Get state reporting division and A/R info
      *
     C     ELCO#         IFNE      SVCO#
     C     ELFND         ORNE      SVFND
     C     ELEMP#        ORNE      SVEMP#
      ***** BEG ADD ***** 08/26/05 ************************************
     C     CLTID         OREQ      '135'
     C     ELFYR         ANDNE     ZZFYR
     C                   Z-ADD     ELDIV         ZZDIV
     C                   Z-ADD     ELFYR         ZZFYR
      ***** END ADD ***** 08/26/05 ************************************
      *
     C                   EXSR      SREMP
      *
      ***** BEG CHG ***** 08/26/05 ************************************
      * If not Client 135, get A/r here
     C**************       EXSR GETAR
     C  N50              EXSR      GETAR
      ***** END CHG ***** 08/26/05 ************************************
     C*
     C                   Z-ADD     ELCO#         SVCO#
     C                   Z-ADD     ELFND         SVFND
     C                   Z-ADD     ELEMP#        SVEMP#
     C**** BEG ADD ***** 02/22/02 ************************************
     C                   Z-ADD     ELDIV         SVDIV
     C                   Z-ADD     ELFYR         SVFYR
     C**** END ADD ***** 02/22/02 ************************************
     C**** Begin Add ****  07/22/05  **************************************
      ***** BEG CHG ***** 07/25/06 ****************************************
     C*******    CLTID     IFEQ '135'
     C*******    RPKY      CHAINPRP01                77
     C*******              ENDIF
     C     RPKY          CHAIN     PRP01                              77
      ***** BEG CHG ***** 07/25/06 ****************************************
     C**** End Add ******  07/22/05  **************************************
     C*
     C***** BEG ADD ******* 03/10/04 **********************************
     C     ELFYR         IFLT      @@STYR
     C                   ADD       1             BYCNT
     C                   GOTO      NEXT
     C                   ENDIF                                                   FYR>=@@STYR
     C***** END ADD ******* 03/10/04 **********************************
      *
     C                   ENDIF
      *
      *   Write output record
      *
     C                   EXSR      SROUT
      *
     C     NEXT          TAG
      *
      ****** BEG ADD ***** 08/11/04 ************************************
     C     XPKY          SETGT     PEL01
      ****** END ADD ***** 08/11/04 ************************************
     C*
     C                   READ      PEL01                                  60
     C                   END
      *
      *******************************************************************
      * END OF PROGRAM - PRINT TOTALS AND EXIT
      *******************************************************************
      *
     C     ERCNT         IFEQ      *ZEROS
     C                   MOVE      *ON           *IN81
     C                   ENDIF
      *
     C                   EXCEPT    TOTL
      *
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
          Except Route;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
      *
      *================================================================
      * GETAR - Get values from Accounts Receivable file
      *================================================================
     C     GETAR         BEGSR
      *
     C                   Z-ADD     *ZERO         SVCLPR
     C                   Z-ADD     *ZERO         SVCLSC
     C                   Z-ADD     *ZERO         SVPTAX
      *
      *  Accumulate across all employer divisions
      *
     C     EAKY          CHAIN     PEA01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
      *  Get category type
      *
     C  N60              MOVEL(P)  EAARCD        KKKEY
     C  N60TBKY          CHAIN     PTT01                              60
      *
     C     EATRDT        IFLE      XXRNDT
     C     *IN60         ANDEQ     *OFF
      *
     C                   SELECT
      *
     C     EAARTY        WHENEQ    'P'
      *
     C     TTCTYP        IFEQ      'P'
     C     TTCTYP        OREQ      'E'
     C                   ADD       EAAMT         SVCLPR
     C                   ENDIF
      *
     C     EAARTY        WHENEQ    'Q'
      *
     C     TTCTYP        IFEQ      'T'
     C                   ADD       EAAMT         SVCLSC
     C                   ENDIF
      *
     C     TTCTYP        IFEQ      '1'
     C     TTCTYP        OREQ      '2'
     C                   ADD       EAAMT         SVPTAX
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  EATRDT <= RNDT
      *
     C     EAKY          READE     PEA01                                  60
     C                   ENDDO
      *
     C                   ENDSR                                                  GETAR
      *
      *================================================================
      * GETDAT- Return date formatted mm/dd/yy
      *================================================================
     C     GETDAT        BEGSR
      *
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         X1DATE
      *
     C     SRERR         IFEQ      *OFF
      *
     C     X1MM          CAT(P)    '/':0         X2DATE           10
     C                   CAT(P)    X1DD:0        X2DATE
     C                   CAT(P)    '/':0         X2DATE
     C                   CAT(P)    X1YY:0        X2DATE
      *
     C                   ELSE
      *
     C                   MOVEL(P)  *BLANKS       X2DATE
     C                   ENDIF                                                  SRERR
      *
     C                   ENDSR                                                  GETDAT
      *
      *================================================================
      * SREMP - Get employer record for state reporting division
      *================================================================
     C     SREMP         BEGSR
      *
      *  Get employer record flagged as state reporting division
     C     EMKY          CHAIN     PEM01                              60
      *
     C     *IN60         DOWEQ     *OFF
     C**** BEGIN CHG *** 11/07/01 ************************************
      *  Once get the State Reporting Flay Record, get out of subroutine
     C     EMSTRP        IFEQ      'Y'
     C                   GOTO      ESREMP
     C                   ENDIF
     C********   EMSTRP    ANDNE'Y'
     C**** END CHG ***** 11/07/01 ************************************
     C     EMKY          READE     PEM01                                  60
     C                   ENDDO
      *
      *  If no employer records, process next employer policy year record
      *     else get first employer record
     C   60EMKY          CHAIN     PEM01                              60
     C   60              GOTO      NEXT
      *
     C**** BEGIN CHG *** 11/07/01 ************************************
      * Remove the Extra Save Fields, are Duplicates
     C*********            Z-ADDELCO#     SVCO#
     C*********            Z-ADDELFND     SVFND
     C*********            Z-ADDELEMP#    SVEMP#
      *
     C     ESREMP        TAG
     C**** END CHG ***** 11/07/01 ************************************
      *
     C                   ENDSR                                                  SREMP
      *
      *================================================================
      * SRPHN - Format phone number for output
      *================================================================
     C     SRPHN         BEGSR
      *
     C     XXAREA        IFNE      '000'
      *
     C                   MOVEL(P)  '('           XXPHON           20
     C                   CAT(P)    XXAREA:0      XXPHON
     C                   CAT(P)    ')':0         XXPHON
     C                   CAT(P)    XXPH1:1       XXPHON
     C                   CAT(P)    '-':0         XXPHON
     C                   CAT(P)    XXPH2:0       XXPHON
      *
     C                   ELSE
      *
     C                   MOVEL(P)  *BLANKS       XXPHON
     C                   ENDIF
      *
     C                   ENDSR                                                  SRPHN
      *
      *================================================================
      * SROUT - Write record to output file WHPOP
      *================================================================
     C     SROUT         BEGSR
      *
     C                   CLEAR                   PPO01
      *
      ***** BEG ADD ***** 10/08/03 **************************************
      * Get State Reporting Division Employer Master for Cancel Dates
     C                   EXSR      SREMP
      * Get State Reporting Division Detail for Policy From-To Dates
      * By reading through 2nd Logical until match Employer Division
     C                   Z-ADD     ELFYR         XXFYR
     C     ELKY          CHAIN     PEL02                              60
      *
     C     *IN60         DOWEQ     *OFF
     C     ELDIV         IFEQ      EMDIV
     C                   LEAVE
     C                   ENDIF
     C     ELKY          READE     PEL02                                  60
     C                   ENDDO
      ***** END ADD ***** 10/08/03 **************************************
      *
      *  Policy Number
      ***** BEG DEL ***** 07/25/06 ****************************************
     C**** Begin Add ****  07/22/05  **************************************
     C********   CLTID     IFEQ '135'
     C********   *IN77     ANDEQ*OFF
     C********             MOVELRPCAP#    POPONR    P
     C*******              ELSE
     C**** End Add ******  07/22/05  **************************************
      ***** BEG DEL ***** 07/25/06 ****************************************
      *
     C                   MOVEL(P)  ELFND         POPONR
     C                   MOVEL(P)  ELEMP#        XXTMP             9
     C                   CAT(P)    XXTMP:0       POPONR
     C                   MOVEL(P)  ELFYR         XXTMP             9
     C                   CAT(P)    XXTMP:0       POPONR
     C*
      ***** BEG DEL ***** 07/25/06 ****************************************
     C**** Begin Add ****  07/22/05  **************************************
     C*********            ENDIF                           e CLTID=135
     C**** End Add ******  07/22/05  **************************************
      ***** END DEL ***** 07/25/06 ****************************************
     C*
     C* Chain to WHPOP file with KEY Just built - If have a Record-BYPASS
     C     POKY          CHAIN     PPO01                              62
     C     *IN62         IFEQ      *OFF
     C                   Z-ADD     ELDIV         SVDIV
     C                   Z-ADD     ELFYR         SVFYR
     C                   ADD       1             FYCNT
     C                   GOTO      SROEND
     C                   ENDIF
      *
      ***** BEG ADD ***** 08/26/05 ************************************
     C   50              EXSR      GETAR
      ***** END ADD ***** 08/26/05 ************************************
      *
      *  Policy number sequence
      *
     C                   Z-ADD     *ZERO         POSEQ
      *
      *  Insured FEIN
      *
     C                   MOVEL(P)  EMFEIN        POFEIN
      *
      *  Insured Code - GRP/EMPLR
      *
     C                   MOVEL(P)  EMFND         POISCD
     C                   MOVEL(P)  EMEMP#        XXTMP
     C                   CAT(P)    XXTMP:0       POISCD
      *
      *  Change to use above POISCD if EMFEIN is Blank or Zero
     C     EMFEIN        IFGT      *ZERO
     C                   MOVE      *BLANKS       POISCD
     C                   MOVEL     EMFEIN        POISCD
     C                   ENDIF                                                  EMFIEN > ZEROS
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMNAME        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMNAME
      ***** END ADD ***** 02/27/04 ************************************
     C***  Begin Add  ***  08/12/09  **************************************
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMNAM2        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMNAM2
     C***  End   Add  ***  08/12/09  **************************************
      *
     C***  Begin Chg  ***  08/12/09  **************************************
     C**********           MOVELEMNAME    PONAME    P
     C     EMNAME        CAT(P)    EMNAM2        PONAME
     C***  End   Chg  ***  08/12/09  **************************************
      *
      *  Insured State
      *
      ***** BEG DEL ******* 01/23/08 **********************************
     C*********  EMMPFL    IFEQ 'P'
      ***** END DEL ******* 01/23/08 **********************************
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMPAD1        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMPAD1
      *
     C     EMPAD2        IFGT      *BLANKS
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMPAD2        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMPAD2
     C                   ENDIF
      *
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMPCTY        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMPCTY
      ***** END ADD ***** 02/27/04 ************************************
      *
     C                   MOVEL(P)  EMPAD1        POAD1
     C                   MOVEL(P)  EMPAD2        POAD2
     C                   MOVEL(P)  EMPCTY        POCTY
     C                   MOVEL(P)  EMPST         POST
      *
     C                   MOVEL     EMPZIP        XXZIP5            5
     C                   MOVE      EMPZIP        XXZIP4            4
     C     XXZIP5        CAT(P)    '-':0         POZIP
     C                   CAT(P)    XXZIP4:0      POZIP
      *
      ***** BEG DEL ******* 01/23/08 **********************************
     C******               ELSE
      ******
      ******BEG ADD ***** 02/27/04 ************************************
      **Clear array then Check for Invalid Hex characters
     C******               MOVEA*BLANKS   HX
     C******               MOVEAEMMAD1    HX
     C******               EXSR SRHEX
     C******               MOVEAHX        EMMAD1
      ******
     C******     EMMAD2    IFGT *BLANKS
     C******               MOVEA*BLANKS   HX
     C******               MOVEAEMMAD2    HX
     C******               EXSR SRHEX
     C******               MOVEAHX        EMMAD2
     C******               ENDIF
      ******
     C******               MOVEA*BLANKS   HX
     C******               MOVEAEMMCTY    HX
     C******               EXSR SRHEX
     C******               MOVEAHX        EMMCTY
      ******END ADD ***** 02/27/04 ************************************
      ******
     C******               MOVELEMMAD1    POAD1     P
     C******               MOVELEMMAD2    POAD2     P
     C******               MOVELEMMCTY    POCTY     P
     C******               MOVELEMMST     POST      P
      ******
     C******               MOVELEMMZIP    XXZIP5  5
     C******               MOVE EMMZIP    XXZIP4  4
     C******     XXZIP5    CAT  '-':0     POZIP     P
     C******               CAT  XXZIP4:0  POZIP     P
     C******               ENDIF                           EMMPFL
      ***** END DEL ******* 01/23/08 **********************************
      ***** BEG ADD ******* 01/23/08 **********************************
      *
      * Clear array then Check for Invalid Hex characters
      *
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMMAD1        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMMAD1
      *
     C     EMMAD2        IFGT      *BLANKS
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMMAD2        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMMAD2
     C                   ENDIF
      *
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMMCTY        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMMCTY
      *
     C                   MOVEL(P)  EMMAD1        POMAD1
     C                   MOVEL(P)  EMMAD2        POMAD2
     C                   MOVEL(P)  EMMCTY        POMCTY
     C                   MOVEL(P)  EMMST         POMST
      *
     C                   MOVEL     EMMZIP        XXZIP5            5
     C                   MOVE      EMMZIP        XXZIP4            4
     C     XXZIP5        CAT(P)    '-':0         POMZIP
     C                   CAT(P)    XXZIP4:0      POMZIP
      ***
     C                   MOVE      EMMPFL        POMPFL
      *
     C     POAD1         IFEQ      *BLANK
     C                   MOVE      POMAD1        POAD1
     C                   MOVE      POMAD2        POAD2
     C                   MOVE      POMCTY        POCTY
     C                   MOVE      POMST         POST
     C                   MOVE      POMZIP        POZIP
     C                   ENDIF
      *
     C     POMAD1        IFEQ      *BLANK
     C                   MOVE      POAD1         POMAD1
     C                   MOVE      POAD2         POMAD2
     C                   MOVE      POCTY         POMCTY
     C                   MOVE      POST          POMST
     C                   MOVE      POZIP         POMZIP
     C                   ENDIF
      *
      ***** END ADD ******* 01/23/08 **********************************
      *
     C                   Z-ADD     ELPFDT        XXDATE
     C                   EXSR      GETDAT
     C                   MOVEL(P)  X2DATE        POPFDT
      *
     C                   Z-ADD     ELPTDT        XXDATE
     C                   EXSR      GETDAT
     C                   MOVEL(P)  X2DATE        POPTDT
      *
     C                   MOVE      XXREN         POREN
      *
     C     ELFAF         IFEQ      'A'
     C                   MOVE      'Y'           POAUD
      *
     C                   ELSE
      *
     C                   MOVE      'N'           POAUD
     C                   ENDIF
      *
     C                   Z-ADD     SVCLPR        POCLPR
     C                   Z-ADD     SVCLSC        POCLSC
     C                   Z-ADD     SVPTAX        POPTAX
      *
     C                   MOVE      EMFAXA        XXAREA            3
     C                   MOVEL     EMFAX#        XXPH1             3
     C                   MOVE      EMFAX#        XXPH2             4
     C                   EXSR      SRPHN
     C                   MOVEL(P)  XXPHON        POFAX
      *
     C                   MOVE      EMAREA        XXAREA            3
     C                   MOVEL     EMPHN#        XXPH1             3
     C                   MOVE      EMPHN#        XXPH2             4
     C                   EXSR      SRPHN
     C                   MOVEL(P)  XXPHON        POPHN
      *
      *  Get county code description
      *
     C     EMPST         IFNE      *BLANKS
     C     EMPST         CAT       EMCNTY:0      ZZKEY             4
     C                   ELSE
     C     EMMST         CAT       EMCNTY:0      ZZKEY
     C                   ENDIF                                                  SCPST<>*BLANKS
      *
     C                   MOVEL(P)  ZZKEY         KKKEY
     C     TBKY          CHAIN     PCN01                              60
     C  N60              MOVEL(P)  CNDESC        POCNTY
     C   60              MOVEL(P)  *BLANKS       POCNTY
      *
      *  Get business description
      *
     C                   MOVEL(P)  'BUSI'        KKTBID
     C                   MOVEL(P)  EMTPOB        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        POTPOB
     C   60              MOVE(P)   *BLANKS       POTPOB
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMNAOB        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMNAOB
      ***** END ADD ***** 02/27/04 ************************************
      *
     C                   MOVEL(P)  EMNAOB        POINAO
      *
      *  Get Employer Effective/Cancellation date
      *
     C                   Z-ADD     5             X                 3 0
      *
     C     ED(X)         DOWEQ     *ZERO
     C     X             ANDGT     1
     C                   SUB       1             X
     C                   ENDDO
      *
     C                   Z-ADD     CD(X)         XXDATE
     C                   EXSR      GETDAT
     C                   MOVEL(P)  X2DATE        #WKA10           10
      *
     C                   Z-ADD     CD(X)         XXCXDT
      *
      *  If policy has been renewed, load prior year policy number
      *
     C     XXREN         IFEQ      'Y'
     C                   MOVEL(P)  SVPONR        POPONP
     C                   ENDIF                                                  XXREN
      *
     C                   MOVEL(P)  POPONR        SVPONR
      *
     C                   Z-ADD     ELDIV         SVDIV
     C                   Z-ADD     ELFYR         SVFYR
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters
     C     EMNAIC        IFGT      *BLANKS
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMNAIC        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMNAIC
     C                   ENDIF
      ***** END ADD ***** 02/27/04 ************************************
      *
     C                   MOVEL(P)  EMNAIC        PONAIC
     C                   MOVEL(P)  EMSIC         POSIC
     C                   MOVEL(P)  EMOFF#        POOFF#
      *
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNCARR        PONCCI
     C   60              MOVEL(P)  *BLANKS       PONCCI
      *
     C                   Z-ADD     EMFND         POGRP
      *
      *  GET CX DATE
      ***** BEG ADD ******* 03/01/02 *************************************
      *  IF Cancelled Date > ZERO and in Policy Year
     C     XXCXDT        IFGT      *ZEROS
     C     XXCXDT        ANDGT     ELPFDT
     C     XXCXDT        ANDLE     ELPTDT
      ***** END ADD ******* 03/01/02 *************************************
      ***** BEG ADD ***** 08/06/05 ***************************************
      * If Cancel Date > ZERO and Flat Cancel
     C     XXCXDT        ORGT      *ZEROS
     C     XXCXDT        ANDEQ     ELPFDT
     C     XXCXDT        ANDEQ     ELPTDT
      ***** END ADD ***** 08/06/05 ***************************************
      *
      * Check WDCXL4 File for CANCEL DATE for this fund year & Print Msgs
      * If NO Matching Cancel Record is an Error, Also if have WMEMP cancel
      * and NO WDCXL4 record for that policy year, treat like not Cancelled
      * ALWAYS Use Date from WDCXP file instead of WMEMP file
      *
     C     CXKY          CHAIN     PCX01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   SELECT
      * CASE: Is Reinstated
     C     CXCXDT        WHENEQ    CXREDT
      ***** BEG ADD ******* 03/01/02 *************************************
     C     XXCXDT        ANDEQ     CXCXDT
      ***** END ADD ******* 03/01/02 *************************************
     C                   Z-ADD     *ZEROS        XXCXDT
      * CASE: Have WDCXP Record and no match on WMEMP
     C     XXCXDT        WHENEQ    *ZEROS
     C     CXCXDT        ANDGT     CXREDT
     C     XXRNDT        IFGT      ELDT
     C                   ADD       1             DTCNT
     C     DTCNT         IFLE      100
     C   OB              EXCEPT    HEAD
     C                   EXCEPT    DETC
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     CXCXDT        XXCXDT
      * CASE: Have WDCXP Record and WMEMP Canc Date Greater
     C     XXCXDT        WHENGT    CXCXDT
     C     CXCXDT        ANDGT     CXREDT
     C     XXRNDT        IFGT      ELDT
     C                   ADD       1             DTCNT
     C     DTCNT         IFLE      100
     C   OB              EXCEPT    HEAD
     C                   EXCEPT    DETCM
     C                   ENDIF
     C                   ENDIF
      ***** BEG DEL ******* 03/01/02 *************************************
     C*******              Z-ADDCXCXDT    XXCXDT
      ***** END DEL ******* 03/01/02 *************************************
      * CASE: Have WDCXP Record and WMEMP Canc Date Less
     C     XXCXDT        WHENLT    CXCXDT
     C     CXCXDT        ANDGT     CXREDT
     C     XXRNDT        IFGT      ELDT
     C                   ADD       1             DTCNT
     C     DTCNT         IFLE      100
     C   OB              EXCEPT    HEAD
     C                   EXCEPT    DETCM
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     CXCXDT        XXCXDT
      * CASE:
     C                   OTHER
     C                   ENDSL
      *
     C                   ELSE
      *
      * Only Use Date from WMEMP when Flat Cancelled
      * And Cancel date in Current Year - Print message
      ***** BEG ADD ******* 03/01/02 *************************************
     C     XXCXDT        IFGT      *ZEROS
     C     XXCXDT        ANDGT     ELPFDT
     C     XXCXDT        ANDLT     ELPTDT
      ***** END ADD ******* 03/01/02 *************************************
     C     XXCXDT        IFGT      *ZEROS
     C     XXCXDT        ANDEQ     ELPFDT
     C     XXCXDT        ANDEQ     ELPTDT
     C                   ELSE
     C     XXRNDT        IFGT      ELDT
     C                   ADD       1             DTCNT
     C     DTCNT         IFLE      100
     C   OB              EXCEPT    HEAD
     C                   EXCEPT    DETCX
     C                   ENDIF
      ***** BEG CHG ******* 03/01/02 *************************************
      * Dont Zero if no matching CANCEL History Record
     C*****                Z-ADD*ZEROS    XXCXDT
     C                   ENDIF
     C                   ENDIF
      ***** END CHG ******* 03/01/02 *************************************
     C                   ENDIF                                                  XXCXDT = 0
      *
     C                   ENDIF                                                  *IN60=*OFF
      *
      *  Get CANCEL Reason
      *
      ***** BEG CHG ******* 03/01/02 *************************************
     C                   MOVE      *BLANKS       #WKA60           60
     C*****      XXCXDT    IFGT *ZEROS
      ***** END CHG ******* 03/01/02 *************************************
     C                   MOVE      ELRTCD        KKRTCD
     C     RSKY          CHAIN     PRS01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        KKST
     C                   MOVEL(P)  CXRSN         K1KEY
     C                   Z-ADD     XXRNDT        KKEFDT
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  60
      ***** BEG ADD ******* 12/11/02 **********************************
      *
     C     *IN60         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
      *
      ***** BEG DEL ***** 05/15/07 ***************************************
      ***** END ADD ******* 12/11/02 **********************************
     C*****N60*****        MOVELCRDESC    POCANR    P
      ***** BEG ADD ******* 03/01/02 *************************************
      ***** END DEL ***** 05/15/07 ***************************************
     C  N60              MOVEL(P)  CRDESC        #WKA60
     C                   ELSE
      ***** BEG CHG ***** 05/15/07 ***************************************
     C     XXCXDT        IFGT      *ZEROS
     C     XXCXDT        ANDEQ     ELPFDT
     C     XXCXDT        ANDEQ     ELPTDT
     C                   MOVEL(P)  FLATCN        #WKA60
     C                   MOVEL(P)  FLATCN        POCANR
     C                   ELSE
     C                   MOVE      *BLANKS       #WKA60
     C                   ENDIF                                                  XXCNDT > *ZEROS
     C**************       MOVE *BLANKS   #WKA60
      ***** END CHG ***** 05/15/07 ***************************************
     C*
      ***** END ADD ******* 03/01/02 *************************************
     C                   ENDIF                                                  *IN60=*OFF
      *
     C                   ENDIF                                                  *IN60
      *
      *  Chapter
     C                   Z-ADD     EMCHAP        POCHAP
     C     CPKY          CHAIN     PCP01                              60
     C  N60              MOVEL(P)  CPDESC        POCDSC
      *
      *  Region
     C                   Z-ADD     EMRGN#        PORGN#
     C     REKY          CHAIN     PRE01                              60
     C  N60              MOVEL(P)  REDESC        PORDSC
      *
      *  Carrier policy number
      *
     C     RPKY          CHAIN     PRP01                              60
     C  N60              MOVEL(P)  RPCAP#        POCAP#
      *
     C     *IN60         IFEQ      *ON
     C     RPCAP#        OREQ      *BLANKS
     C                   MOVEL(P)  POPONR        POCAP#
     C                   ENDIF                                                  RPCAP#
      *
     C                   Z-ADD     EMCO#         POCO#
     C                   Z-ADD     ELDIV         PODIV
      *
      *  Get initial transaction date
      *
     C     EHKY          CHAIN     PEH01                              60
      *
      ****** BEG ADD ***** 11/15/04 ************************************
     C                   MOVEL     *BLANK        XXNOEH            1            NO WDEHP FLAG
      ****** END ADD ***** 11/15/04 ************************************
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EHTRDT        XXDATE
     C                   EXSR      GETDAT
     C                   MOVEL(P)  X2DATE        POTRDT
      *
      ****** BEG ADD ***** 11/15/04 ************************************
     C                   ELSE
     C                   MOVE      'Y'           XXNOEH
      ****** END ADD ***** 11/15/04 ************************************
      *
     C                   ENDIF                                                  *IN60
      *
      *  Carrier ID
      *
      **** BEGIN CHG *** 05/30/02 ************************************
      * Carrier ID Number = EMCO# + EMFND
     C*********            MOVELEMFND     POCID     P
     C                   MOVEL(P)  ##CID         POCID
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters
     C     EMIADR        IFGT      *BLANKS
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMIADR        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMIADR
     C                   ENDIF
      ***** END ADD ***** 02/27/04 ************************************
     C                   MOVEL(P)  EMIADR        POIADR                         Web Address
      **** END CHG ***** 05/30/02 ************************************
      *
      *  Policy Status Code
      *
      ***** BGN DEL ******* 03/18/02 **********************************
     C*******    XXRNDT    IFLT ELPFDT
     C*******              MOVE 'P'       POSTCD           PENDING
     C*******              END
      ***** END DEL ******* 03/18/02 **********************************
      *
     C     XXCXDT        IFGT      *ZEROS
     C                   Z-ADD     CD(X)         XXDATE
     C                   EXSR      GETDAT
     C                   MOVEL(P)  X2DATE        #WKA10           10
     C                   ENDIF
      *
      *  Policy Status Code IF CANCELLED
      *
     C                   SELECT
      *
      *  If Policy From, To and Cancel Dates are same is flat CANCEL
      *  OR Cancel Date is less than From Date is a Cancelled
     C     XXCXDT        WHENNE    *ZERO
     C     XXCXDT        ANDLE     XXRNDT
     C     XXCXDT        ANDEQ     ELPFDT
     C     XXCXDT        ANDEQ     ELPTDT
     C                   MOVEL     #WKA10        POCAD
     C                   MOVE      *BLANKS       #WKA10
     C                   MOVE      'C'           POSTCD                         CANCELLED
      ***** BEG ADD ***** 05/15/07 ***************************************
     C     #WKA60        IFNE      *BLANKS
     C                   MOVEL(P)  #WKA60        POCANR
     C                   ELSE
     C                   MOVEL(P)  FLATCN        POCANR
     C                   ENDIF                                                  #WKA60 = *BLANK
      ***** END ADD ***** 05/15/07 ***************************************
      *
      ******BEG DEL ***** 05/15/07 ***************************************
     C*******    XXCXDT    WHNE *ZERO
     C*******    XXCXDT    ANDLEXXRNDT
      **Cancel Date & Code should be between Policy From & To Dates
     C*******    XXCXDT    ANDGTELPFDT
      ******BEG CHG ******* 08/05/05 *************************************
      **If*Cancel date = Policy to Date, is a Non-Renew policy
     C*********  XXCXDT    ANDLEELPTDT
     C*******    XXCXDT    ANDLTELPTDT
      ***** END DEL ***** 05/15/07 ***************************************
      *
      ***** END CHG ******* 08/05/05 *************************************
      *
      ***** BEG ADD ***** 05/15/07 ***************************************
      * Cancel Date & Code should be between Policy From & To Dates
     C     XXCXDT        WHENNE    *ZERO
     C     XXCXDT        ANDLE     XXRNDT
     C     XXCXDT        ANDGT     ELPFDT
     C     XXCXDT        ANDLE     ELPTDT
     C                   MOVEL(P)  #WKA60        POCANR
      ***** END ADD ***** 05/15/07 ***************************************
     C                   MOVEL     #WKA10        POCAD
     C                   MOVE      *BLANKS       #WKA10
     C                   MOVE      'C'           POSTCD                         CANCELLED
      *
      *  OR Cancel Date is less than From Date is Cancelled PRIOR
     C     XXCXDT        WHENNE    *ZERO
     C     XXCXDT        ANDLE     XXRNDT
     C     XXCXDT        ANDLT     ELPFDT
     C                   MOVEL     #WKA10        POCAD
     C                   MOVE      *BLANKS       #WKA10
      **** BEG ADD ***** 03/01/02 ************************************
     C                   MOVEL(P)  #WKA60        POCANR
      **** END ADD ***** 03/01/02 ************************************
     C                   MOVE      'C'           POSTCD                         CANCELLED
      *
      ***** BEG ADD ***** 05/15/07 ***************************************
      *  Cancel Date is Equal From Date where From Date not Group end date
     C     XXCXDT        WHENNE    *ZERO
     C     XXCXDT        ANDLE     XXRNDT
     C     XXCXDT        ANDGT     ELPFDT
     C     XXCXDT        ANDEQ     ELPTDT
     C                   MOVEL     #WKA10        POCAD
     C                   MOVE      *BLANKS       #WKA10
     C                   MOVEL(P)  #WKA60        POCANR
     C                   MOVE      'C'           POSTCD                         CANCELLED
      ***** END ADD ***** 05/15/07 ***************************************
      *
      **** BEG ADD ***** 03/01/02 ************************************
      *  OR Cancel Date is > RUN DATE & <=Policy To Date-FUTURE Cancel
     C     XXCXDT        WHENNE    *ZERO
     C     XXCXDT        ANDGT     XXRNDT
     C     XXCXDT        ANDLE     ELPTDT
     C                   MOVEL     #WKA10        POCAD
     C                   MOVE      *BLANKS       #WKA10
     C                   MOVEL(P)  #WKA60        POCANR
     C                   MOVE      'I'           POSTCD                         INFORCE
      **** END ADD ***** 03/01/02 ************************************
      *
     C     XXRNDT        WHENLT    ELPTDT
     C                   MOVE      'I'           POSTCD                         INFORCE
      *
     C                   OTHER
     C                   MOVE      'E'           POSTCD                         EXPIRED
      *
     C                   ENDSL
      *
      ***** BGN ADD ******* 03/18/02 **********************************
     C     XXRNDT        IFLT      ELPFDT
     C                   MOVE      'P'           POSTCD                         PENDING
      ****** BEG ADD ***** 11/15/04 ************************************
      * Check if no WDEHP record and bypass write if flag is on, this
     C* is only for employers in future that have not been billed.
     C     XXNOEH        IFEQ      'Y'
      ****** BEG ADD ***** 08/12/05 ************************************
     C     CLTID         ANDNE     '093'
      ****** END ADD ***** 08/12/05 ************************************
     C                   GOTO      SROEND
     C                   ENDIF
      ****** END ADD ***** 11/15/04 ************************************
      *
      ****** BEG ADD ***** 09/27/07 ************************************
      * If ECM and run date less than Nov. 15 of current year, exclude
      * these policies.
     C     XXRNDT        IFLT      XRENDT
     C     CLTID         ANDEQ     '071'
     C*** BEGIN CHG *** 09/04/08 **************************************
     C********** EHNWRW    ANDEQ'R'
     C     EHNWRW        ANDEQ     'R'
     C*** END   CHG *** 09/04/08 **************************************
     C                   GOTO      SROEND
     C                   ENDIF
      ****** END ADD ***** 09/27/07 ************************************
      *
     C                   END
      ***** END ADD ******* 03/18/02 **********************************
      *
      *      As of date
     C                   Z-ADD     XXDT          XXDATE
     C                   EXSR      GETDAT
     C                   MOVEL(P)  X2DATE        POAODT
      *
      *  Accumulate premium across all divisions for given policy year
      *
     C                   Z-ADD     ELFYR         XXFYR
     C     ELKY          CHAIN     PEL02                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     ELFAF         IFEQ      'A'
     C                   ADD       ELAAPR        POANPR
      *
     C                   ELSE
      *
     C                   ADD       ELANPR        POANPR
     C                   ENDIF
      *
     C     ELKY          READE     PEL02                                  60
     C                   ENDDO
     C*
     C* If Last extract date is > Maint Date in WDELL, Bypass Write
      **** BEG CHG ***** 04/24/02 **********************************************
     C********   XXRNDT    IFGT ELDT
     C     XXRNDT        IFGE      ELDT
      **** END CHG ***** 04/24/02 **********************************************
     C                   ADD       1             LECNT
     C                   GOTO      SROEND
     C                   ENDIF
      *
      ***** BEG ADD ***** 04/04/05 ***********************************
     C*    Policy Services Staff or Underwriter Data Fill
     C     *IN45         IFEQ      *ON                                          Is Client 129?
      *
     C                   CLEAR                   PUP01
     C                   MOVE      *BLANKS       POUFSN
     C                   MOVE      *BLANKS       POULSN
     C                   MOVE      *BLANKS       POUMAL
     C     EMPSRV        IFNE      *BLANK
     C     EMPSRV        CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     UPUSNM        POULSN
     C                   MOVEL     UPMAIL        POUMAL                         Adjuster Email
     C                   ELSE
     C                   ADD       1             UWCNT             5 0
     C     UWCNT         IFLE      100
     C   OB              EXCEPT    HEAD
     C                   EXCEPT    DETPS
     C                   ENDIF
     C                   ENDIF                                                  *IN60 *OFF
     C                   ENDIF                                                  *EMPSRV <> BLNK
      *
     C                   ELSE
      *
      ***** END ADD ***** 04/04/05 ***********************************
      *
      **** BEGIN ADD *** 05/30/02 ************************************
     C*    Underwriter Data
     C     EMUWTR        IFNE      *BLANK
     C     EMUWTR        CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PUP01
     C                   ADD       1             UWCNT             5 0
     C     UWCNT         IFLE      100
     C   OB              EXCEPT    HEAD
     C                   EXCEPT    DETUW
     C                   ENDIF
     C                   ELSE                                                   .*IN60
     C                   MOVEL     UPUSNM        POULSN
     C                   MOVEL     UPMAIL        POUMAL                         Adjuster Email
     C                   ENDIF                                                  .*IN60
     C                   ENDIF                                                  .EMUWTR<>BLNK
      **** END ADD ***** 05/30/02 ************************************
      *
      ***** BEG ADD ***** 04/04/05 ***********************************
     C                   ENDIF                                                  END *IN45 = *ON
      ***** END ADD ***** 04/04/05 ***********************************
      *
      ****** BEG ADD ***** 08/11/04 ************************************
      *  If GET hit on excluded Employer, then bypass the write of record
     C     XPKY2         CHAIN     PXP01                              64
     C     *IN64         IFEQ      *OFF
     C                   GOTO      SROEND
     C                   ENDIF
      ****** END ADD ***** 08/11/04 ************************************
      *
      *  Write to output file
      *
     C                   ADD       1             OTCNT
     C                   WRITE     PPO01
      *
     C     SROEND        TAG
     C*
     C                   MOVE      *OFF          *IN62
      *
     C                   ENDSR                                                  SROUT
      * =================================================================
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      *================================================================
      * SRHEX - Check Alpha input field for Invald Hex characters
      *         The Alpha field is loaded into the arrary w/MOVEA
      *         then Loop thru array & replace w/blank if out of
      *         is not a valid HEX Character. After exiting subr
      *         the array is moved back to the alpha field w/MOVEA
      *================================================================
     C     SRHEX         BEGSR
      *
     C                   DO        50            K                 3 0
      *
     C     HX(K)         IFLT      X'40'
     C     HX(K)         OREQ      X'41'
     C     HX(K)         ORGT      X'F9'
     C                   MOVE      X'40'         HX(K)
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   ENDSR                                                  SROUT
      *================================================================
      ***** END ADD ***** 02/27/04 ************************************
      *
      ***** BEG ADD ***** 11/04/03 ***********************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           23 'REPORT TITLE  . :'
     O                       ORTITL              84
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 23 'PROGRAM NAME  . :'
     O**********             ORPNAM              34
     O**********                                 40 'FILE:'
     O**********             ORFIL#              42
     O**********                                 62 'COPIES:'
     O**********             ORCOPY        Z     66
     O**********                                 75 'HOLD:'
     O**********             ORHOLD              80
     O**********E            ROUTE          2
     O**********                                 23 'OUTPUT QUEUE  . :'
     O**********             OROUTQ              34
     O**********                                 41 'FORMS:'
     O**********             ORFORM              52
     O**********                                 60 'SAVE:'
     O**********             ORSAVE              65
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE          2
     O                                           23 'INCREMENTAL DATE:'
     O                       ##RNDT              34 '  /  /    '
     O          E            ROUTE          2
     O                                           19 'RUN TIME/DATE . :'
     O                       DSDTTM              39 '  :  :  &  /  /    '
      ***** BEG CHG ***** 08/06/05 ***************************************
     O**************                     23 '(C) COPYRIGHT PROGRESSI'
     O**************                     46 'VE DATA SOLUTIONS. INC,'
     O**************                     52 '.1990'
      ****** BEG DLT ***** 01/28/10 **********************************
     O*****
     O***                                23 '(C) COPYRIGHT FISERV IN'
     O***                                46 'SURANCE SOLUTIONS. INC,'
     O***                                59 '.1990 - 2005'
      ****** END DLT ***** 01/28/10 **********************************
      ***** END CHG ***** 08/06/05 ***************************************
      *
      ***** END ADD ***** 11/04/03 ***********************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     OQSYSPRT   E            HEAD           1 03
     O                       DSTIME               8 '  :  :  '
      ***** BEG ADD ******* 05/30/02 **********************************
     O                                           49 ' CICENTER - WHPOP FILE '
     O                                           73 'INCREMENTAL EXTRACT FROM'
     O                       ##RNDT              84 '  /  /    '
      ***** END ADD ******* 05/30/02 **********************************
     O                       DSDATE             123 '  /  /    '
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           40 ' ERROR LISTING'
     O                       PAGE          Z    123
     O                                          118 'PAGE'
     O          E            HEAD           2
     O                                            4 'CO#'
     O                                            9 'GRP'
     O                                           20 'EMPLOYER#'
     O                                           25 'DIV'
     O                                           30 'YEAR'
     O                                           45 'INSURED NAME'
      *
      ***** BEG ADD ******* 05/30/02 **********************************
      ***** BEG CHG ***** 04/04/05 ***********************************
     O******************                 79 'UNDWTR'
     O              N45                          79 'UNDWTR'
     O               45                          80 'POL SVCS'
      ***** END CHG ***** 04/04/05 ***********************************
      ***** END ADD ******* 05/30/02 **********************************
     O*
      ***** BEG ADD ******* 11/04/03 **********************************
     O                                          100 'ERROR DESCRIPTION'
      ***** END ADD ******* 11/04/03 **********************************
     O*
     O          E            DETL           1
     O                       ELCO#         3      4
     O                       ELFND         3      9
     O                       ELEMP#        3     20
     O                       ELDIV         3     25
     O                       ELFYR         3     30
     O                       EMNAME              73
     O                                          101 '** DUPLICATE RECD ***'
     O*
     O          E            DETC           1
     O                       ELCO#         3      4
     O                       ELFND         3      9
     O                       ELEMP#        3     20
     O                       ELDIV         3     25
     O                       ELFYR         3     30
     O                       EMNAME              73
     O                                          101 '** NO EMPR CANC DATE-'
     O                                          110 'USE WDCXP'
     O                       CXCXDT             120 '   /  /  '
     O*
      ***** BEG ADD ******* 05/30/02 **********************************
     O          E            DETUW          1
     O                       ELCO#         3      4
     O                       ELFND         3      9
     O                       ELEMP#        3     20
     O                       ELDIV         3     25
     O                       ELFYR         3     30
     O                       EMNAME              73
     O                       EMUWTR              78
     O                                          104 '** NO WMUPP MATCH FOR UN'
     O                                          121 'DERWRITER CODE **'
      ***** END ADD ******* 05/30/02 **********************************
     O*
      ***** BEG ADD ***** 04/04/05 **********************************
     O          E            DETPS          1
     O                       ELCO#         3      4
     O                       ELFND         3      9
     O                       ELEMP#        3     20
     O                       ELDIV         3     25
     O                       ELFYR         3     30
     O                       EMNAME              73
     O                       EMPSRV              78
     O                                          104 '** NO WMUPP-POLICY SERVI'
     O                                          121 'CES STAFF ID  ** '
      ***** END ADD ***** 04/04/05 ************************************
     O*
     O          E            DETCX          1
     O                       ELCO#         3      4
     O                       ELFND         3      9
     O                       ELEMP#        3     20
     O                       ELDIV         3     25
     O                       ELFYR         3     30
     O                       EMNAME              73
     O                                          101 '** NO WDCXP MATCH DTE'
     O                                          111 'FOR WMEMP'
     O                       XXCXDT             122 '   /  /  '
     O*
     O          E            DETCM          1
     O                       ELCO#         3      4
     O                       ELFND         3      9
     O                       ELEMP#        3     20
     O                       ELDIV         3     25
     O                       ELFYR         3     30
     O                       EMNAME              73
     O                                          101 '** EMPR/WCXP CANC DTE'
     O                                          112 'DONT MATCH'
     O                       XXCXDT             122 '   /  /  '
     O                       CXCXDT             132 '   /  /  '
     O*
     O          E            TOTL        1  2
     O                                           25 '***** TOTALS ***** '
     O                  81                       55 '***** NO DUPLICATE '
     O                  81                       74 'RECORDS FOUND *****'
     O*
     O          E            TOTL           1
     O                                           22 'Total number of record'
     O                                           44 's read  . . . . . . .:'
     O                       INCNT         3     53
     O*
     O          E            TOTL           1
     O                                           22 'Total number of record'
     O                                           44 's Bypassed-TEST . . .:'
     O                       BYCNT         3     53
     O          E            TOTL           1
     O                                           22 'Total number-Same GpYr'
     O                                           44 ' Duplicates-NOT Added:'
     O                       ERCNT         3     53
     O*
     O          E            TOTL           1
     O                                           22 'Total number GrpYr/DIV'
     O                                           44 ' Duplicates NOT Added:'
     O                       FYCNT         3     53
     O*
     O          E            TOTL           1
     O                                           22 'Total number recds <= '
     O                                           44 'Last Extract-NO ADD .:'
     O                       LECNT         3     53
     O*
     O          E            TOTL           2
     O                                           22 'Total number records w'
     O                                           44 'ritten to WHPOP . . .:'
     O                       OTCNT         3     53
     O*
     O          E            TOTL           1
     O                                           22 'Total of CANCEL Date E'
     O                                           44 'rrors-Rcds May ADD. .:'
     O                       DTCNT         3     53
     O                                         +  5 '** ONLY 100 Rcds Listed'
     O*
      ***** BEG ADD ******* 05/30/02 **********************************
     O          E            TOTL           1
      ***** BEG CHG ***** 04/04/05 ***********************************
     O***************                    22 'Total of Underwriter C'
     O              N45                          22 'Total of Underwriter C'
     O               45                          22 'Total of Policy Scvs C'
      ***** BEG CHG ***** 04/04/05 ***********************************
     O                                           44 'ode Missing-Warning .:'
     O                       UWCNT         3     53
     O                                         +  5 '** ONLY 100 Rcds Listed'
      ***** END ADD ******* 05/30/02 **********************************
     O*
     O****************************************************************
