     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     h bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0161R - Cancellation notice workfile build
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/28/25 C:062 Project: 153WBJ                                *
     H*                Populate late fee and batch#.                  *
     H*                                                               *
     H* 05/30/25 C:333 Project: 153WBF                                *
     H*                Exclude Recent New/Renewal Policies from Mass CX
     H*                                                               *
     H* 05/04/21 C:136 Project: WC1986                                *
     H*                Adjust EFT logic to use GetEftStatus()         *
     H*                                                               *
     H* 01/08/20 C:180 PROJECT 102WIC                                 *
     H*                Add State Inclusion/Exclusion Dropdown.        *
     H*                Fix logic                                      *
     H*                                                               *
     H* 11/21/19 C:180 PROJECT 102WIC                                 *
     H*                Add State Inclusion/Exclusion Dropdown.        *
     H*                                                               *
     H* 01/10/18 C:33  Support 63341                                  *
     H*                Clear agt# before WA002R call                  *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 09/01/15 C:65  Project: Phase#34308                           *
     H*                Update cancellation count code to include      *
     H*                cancellations when there is no table setup.    *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 07/02/13 C:136 PROJECT: 147WAI                                *
     H*                EXCLUDE DIVIDEND TRANSACTIONS FROM             *
     H*                BALANCE CALCULATION                            *
     H*                                                               *
     H* 06/27/12 C:136 SUPPORT: WA046 80251                           *
     H*                FIX DEPOSIT BALANCE CALC                       *
     H*                                                               *
     H* 10/17/11 C:136 SUPPORT: WA114 78188                           *
     H*                CORRECT SELECTION FOR CASH TYPE RECORDS IN     *
     H*                A/R BALANCE CALC                               *
     H*                                                               *
     H* 08/17/11 C:190  Project: WC2019                               *
     H*                 Pull the cancellation notice days and         *
     H*                 additional mailing days from the new          *
     H*                 fields for new policies.                      *
     H*                                                               *
     H* 05/23/11 C:136 SUPPORT: WA099 73548                           *
     H*                ADJUST A/R BALANCE CALC'S FOR NEW DEPOSIT      *
     H*                REFUND AND VOID CODES.                         *
     H*                                                               *
     H* 03/24/11 C:255 Support: 75884                                 *
     H*                Added code to not include policies having Posti*
     H*                -ng date greater than Through Date for AR Type *
     H*                'H' in cancellation batch.                     *
     H*                                                               *
     H* 10/22/10 C:255 Support: SEQ73572                              *
     H*                Check object WA073R in library list instead    *
     H*                Call Stack.                                    *
     H*                                                               *
     H* 07/27/10 C:257 SUPPORT: SEQ73572                              *
     H*                IF API WA073R EXIST IN CALL STACK THEN USE     *
     H*                FILE WMEFL6 INSTEAD OF WMDBL3                  *
     H*                                                               *
     H* 08/09/10 C:191 PROJECT: 71WKU                                 *
     H*                Ignore Retro Payments with transaction dates in*
     H*                future of the cancellation date (X1DATE)       *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 08/22/08 C:136 SUPPORT: WA139 64491                           *
     H*                CORRECT PROCESSING OF MONTHLY REPORTING        *
     H*                POLICIES IN WHICH VENDOR PROCESSING IS         *
     H*                ENABLED. THE PROGRAM NEEDS TO CHECK THE        *
     H*                MONTHLY REPORTING FILE THAT USES RPMY IF       *
     H*                NOT FOUND IN THE WEEKLY FILE WHICH USES        *
     H*                SRTODT BEFORE DECIDING IF THE POLICY HAS       *
     H*                MISSED A REPORT                                *
     H*                                                               *
     H* 10/05/06 C:33  Seq#: WA129/53623                              *
     H*                Neg Adj flag should only be set for the        *
     H*                 appropriate run type (QQEAFL)                 *
     H*                                                               *
     H* 07/25/06 C:61  SUPPORT WA129 53623                            *
     H*                Set flag to indicate if negative premium audit *
     H*                adjustment after the billing thru date for     *
     H*                previous policy year.                          *
     H*                                                               *
     H* 09/02/05 C:61  PROJECT WC1359 129WLT                          *
     H*                If prompt parm indicates:                      *
     H*                  Do not skip S/R Agency Billed Employers      *
     H*                                                               *
     H* 07/25/05 C:61  PROJECT 133WBV                                 *
     H*               -Only count cancellations since number of       *
     H*                months WTXCP/XCCXMN.                           *
     H*               -Only count cancellations for reason of         *
     H*                non-payment.                                   *
     H*                                                               *
     H* 06/24/05 C:33  Project: 109WDZ                                *
     H*                {109AM}                                        *
     H*                S/R employers - ignore months not reported.    *
     H*                                                               *
     H* 05/02/05 C:09  PROJECT: WC577                                 *
     H*                Increase size of multiple occurrence data      *
     H*                structure (MODS).                              *
     H*                                                               *
     H* 03/07/05 C:136 PROJECT: WC1199 (REVAMP DUNNING II)            *
     H*                DISPLAY 'INCLUDE DEPOSIT' FLAG IN ROUTING      *
     H*                                                               *
     H* 02/19/04 C:136 PROJECT: WC1033                                *
     H*                Add code for new A/R type of 'T' (True Void)   *
     H*                                                               *
     H* 02/12/04 C:136 SUPPORT WA081 40935                            *
     H*                If one division meets criteria for canx,       *
     H*                ensure that all divisions are written to the   *
     H*                cancellation workfile.                         *
     H*                                                               *
     H* 02/01/04 C:61  SUPPORT WA045 41112 WC1098                     *
     H*                Do not change policy year key value when       *
     H*                determining if a more recent policy year       *
     H*                exits.                                         *
     H*                                                               *
     H* 01/13/04 C:136 Project: WC1116                                *
     H*                Use effective and expiration date range in     *
     H*                direct debit file to determine ACH status      *
     H*                                                               *
     H* 01/07/04 C:33  Project: WC1132                                *
     H*                Get correct Billing Sch Code Aux record        *
     H*                                                               *
     H* 10/27/03 C:33  Project: WC1132                                *
     H*                Revamp Dunning Process                         *
     H*                                                               *
     H* 10/23/03 C:61  PROJECT WC1098                                 *
     H*                Optionaly include current accounts for         *
     H*                prior-year missing reports (Y/ ).              *
     H*                                                               *
     H* 05/02/03 C:61  PROJECT WC1108                                 *
     H*                Change to compare to (amount due / amt billed) *
     H*                instead of $ amount.                           *
     H*                                                               *
     H* 02/18/03 C:136 PROJECT: WC1115 (PAYROLL/PREM EXPORT/IMPORT)   *
     H*                Modify report if vendor processing is          *
     H*                enabled to include report frequency and        *
     H*                report date for self-reporting employers.      *
     H*                                                               *
     H* 12/11/02 C:61  SUPPORT WA081 35144                            *
     H*                Get most recent ACTIVE CX reason code record.  *
     H*                                                               *
     H* 12/10/02 C:61  SUPPORT WA114 34969                            *
     H*                When accumulating prior year A/R records,      *
     H*                do not include policy years with contested     *
     H*                audits.                                        *
     H*                                                               *
     H* 06/06/01  C:35   PROJECT: SUPPORT                             *
     H*                  ADJUST DATA STRUCTURE TO ALLOW FOR 7-DIGIT   *
     H*                  DATE.                                        *
     H*                                                               *
     H* 04/05/01 C:61  PROJECT WC577                                  *
     H*              - By-pass previous years if on agency bill.      *
     H*                (Account current, Plan='A')                    *
     H*                                                               *
     H* 05/26/00  C:35   PROJECT: WC987                               *
     H*                  CHECK FOR ALTERNATE COMPANY NAME FNANAM, IF  *
     H*                  NE BLANKS LOAD IN PLACE OF COMPANY NAME.     *
     H*                                                               *
     H* 02/15/00  C:87   PROJECT: WC912                               *
     H*                  Add entry parm for balance type flag(QQEAFL) *
     H*                  Chg routing page to show new parm.  If       *
     H*                  QQEAFL<>'A' then process as before otherwise *
     H*                  accumulate the bal for all prior years where *
     H*                  ELFAF='A'.                                   *
     H*                                                               *
     H* 02/08/00  C:35   PROJECT: WC577                               *
     H*                  CHANGED HEADING OF CX/FLAG TO VIP/FLAG.      *
     H*                                                               *
     H* 01/13/00  C:35   PROJECT: SUPPORT WA100 #21382                *
     H*                  FINAL AUDIT FLAG WAS BEING COMPARED TO 'Y'   *
     H*                  INSTEAD OF 'A'.                              *
     H*                  INCORRECT POLICY EFFECTIVE DATES WERE        *
     H*                  PRINTING ON THE REPORT BECAUSE THE POLICY    *
     H*                  YEAR WAS NOT BEING LOADED INTO THE KEY.      *
     H*                                                               *
     H* 10/19/99  C:87   PROJECT: WC939                               *
     H*                  Add Premium Finance company code to output.  *
     H*                                                               *
     H* 10/07/99  C:87   PROJECT: WC894                               *
     H*                  Add Entry parm for 'Payment through date'.   *
     H*                  'Balance date' is now 'Billing through dt'.  *
     H*                  Check A/R posting date: against 'Billing     *
     H*                  through dt' for types B, A, D, V, N, R, and  *
     H*                  F; against 'Payment through dt' for types    *
     H*                  P and Q.  Chg routing page to show new parms.*
     H*                                                               *
     H* 09/28/99  C:87   PROJECT: WC757                               *
     H*                  Add columns to the 'Emps selcted for Auto cx'*
     H*                  reprt for: pol eff dt; pol yr; annual prem;  *
     H*                  agent#; agt name.  Chg size of QSYSPRT to 198*
     H*                  Add total for annual premium.  Do not load   *
     H*                  employer to workfile if they are contesting  *
     H*                  an audit (ELCTST = 'Y').                     *
     H*                                                               *
     H* 07/29/99  C:87   PROJECT: WC757                               *
     H*                  The selected for cx flag will default to 'Y' *
     H*                                                               *
     H* 07/06/99  C:87   PROJECT: SUPPORT WA095 #16762                *
     H*                  Added 'Estimated issue date' to routing page.*
     H*                  Single spaced routing page Report selections.*
     H*                                                               *
     H* 06/01/99  C:87   PROJECT: SUPPORT WA101 #17550                *
     H*                  Only process the most recent policy year for *
     H*                  inclusion in batch and only if it is current.*
     H*                                                               *
     H* 04/07/99  C:90  PROJECT: 96WAD (IN BASE: 07/19/99 C:33 WC899) *
     H*                 DO NOT PRINT ACCOUNT CURRENT POLICIES.        *
     H*                                                               *
     H* 11/26/98  C:35   PROJECT: SUPPORT WA095 14804                 *
     H*                  THE PROGRAM WAS EXCLUDING EMPLOYERS WHO HAD  *
     H*                  0 DUE FROM THE REPORT, EVEN THOUGH THEY WERE *
     H*                  INCLUDED IN THE WORKFILE BECAUSE OTHER       *
     H*                  DIVISIONS OF THE EMPLOYER HAD AN AMOUNT DUE. *
     H*                                                               *
     H* 11/25/98  C:87  PROJECT: WC757                                *
     H*                 Added parm for Estimated issue date.  Include *
     H*                 cancelled employers in workfile if the canx   *
     H*                 date > issue date + notice and mailing days.  *
     H*                                                               *
     H* 11/24/98  C:35   PROJECT: SUPPORT WA095 15213                 *
     H*                  FIXED DATE PROCESSING FOR S/R MISSING MONTH  *
     H*                  CRITERIA.                                    *
     H*                                                               *
     H* 11/10/98  C:87  PROJECT: WC852                                *
     H*                 Do not iclude employers in workfile if the    *
     H*                 canx batch control office# is not blank and   *
     H*                 the employer's office # does not match.       *
     H*                 Check validity of canx reason code before     *
     H*                 loading (based on emp juris st and pol eff dt)*
     H*                 If not valid load blanks to reason code and   *
     H*                 load zero to reinstatement fee.               *
     H*                                                               *
     H* 10/07/98  C:35  PROJECT: SUPPORT WA095 #14231                 *
     H*                 FIXED DATE PROCESSING.                        *
     H*                                                               *
     H* 07/09/98  C:35  PROJECT: SUPPORT WA095 12442                  *
     H*                 WHEN CHECKING FOR MORE RECENT FYR, CODE WAS   *
     H*                 NOT TAKING INTO ACCOUNT THE POSSIBILITY OF    *
     H*                 MORE THAN TWO FYRS IN THE WXEBP FILE.         *
     H*                                                               *
     H*  03/05/98 C:35 PROJ: CM024                                    *
     H*                CHANGED TO USE INCEPTION DATE RATHER THAN      *
     H*                POLICY EFFECTIVE DATE WHEN EXCLUDING NEW       *
     H*                POLICIES BASED ON THE DAY OF MONTH ENTERED ON  *
     H*                THE PROMPT SCREEN.                             *
     H*                                                               *
     H*****************************************************************
     H*  FILES:                                                       *
     H*                                                               *
     H*  WMEAL7 (PEA01) CO/FND/EMP/DIV/FYR/PODT/TIME     Employer A/R *
     H*  WDELL  (PEL01) CO/FND/EMP/DIV/FYR             Empoyer Detail *
     H*  WASRL2 (PSR01) CO/FND/FYR/EMP/DIV/RPMY/CLAS/EFDT   S/R Trans *
     H*  WDF1L  (PF101) CO/FND/FYR                        FUND Detail *
     H*  WDFDL  (PFD01) CO/FND/EMP/DIV/FYR/PODT/TRDT/TIME  Member Fee *
     H*  WXEBL  (PEB00) CO/FND/SRT1/SRT2        SELECT EMPLYR WRKFILE *
     H*****************************************************************
      *
     FWXEBL     IF   E           K DISK
     F                                     RENAME(PEB01:PEB00)
     FWMEML     IF   E           K DISK
     FWXCNL     IF A E           K DISK
     FWMEAL7    IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWDELL5    IF   E           K DISK
     F                                     RENAME(PEL01:PEL03)
     FWASRL2    IF   E           K DISK
      *
     F*  SELF-REPORTING PAYROLL TRANSACTION FILE
     FWASRL8    IF   E           K DISK
     F                                     RENAME(PSR01:PSR02)
      *
     F* TABLE MASTER FILE
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PCN01)
     FWXCCL     UF   E           K DISK
     FWDF1L     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMBXL1    IF   E           K DISK
     FWXCNL4    UF   E           K DISK
     F                                     RENAME(PCN01:PCN02)
     FWTCRL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWMPFL     IF   E           K DISK
     F                                     RENAME(PPF01:PPF00)
      *** beg del ***** 05/04/21 ******************
     F***WMDBL3    IF   E           K DISK
     F***                                     RENAME(PDB01:PDB03)
     F***WMEFL6    IF   E           K DISK
      *** end del ***** 05/04/21 ******************
      *
      *  Cancellation maintenance
     FWTXCL     IF   E           K DISK
      *
      *  Cancellation detail
     FWDCXL10   IF   E           K DISK
     F                                     RENAME(PCX01:PCX02)
      *
     FWXEBL1    IF   E           K DISK
     FWXEBL2    IF   E           K DISK
     F                                     RENAME(PEB01:PEB02) PREFIX(B2:2)
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
      *
      *
     D StateListDS     DS           150
     D   xxStList                   150
     D   AryStList                    2    DIM(75) OVERLAY(xxStList)
     D xxRnDt          s              7  0 inz
     D xxStinEx        s              7    inz
     D xxStList1       s            160    inz
     D xxStList2       s            160    inz
     D i               s              3  0 inz
      *** beg add ***** 05/04/21 *****************************
     D/copy wPrBank
      *** Beg Add ***** 05/30/25 *****************************
     D/copy wPrAr
      *** End Add ***** 05/30/25 *****************************
     D*** Begin Add *** 07/28/25 *****************************
     D/copy sPrString
     D/copy wPrEmplr
     D*** End   Add *** 07/28/25 *****************************
       Dcl-S chrChkType Char(10);
       Dcl-S chrEftType Char(1) Inz('E');
       Dcl-S chrEftKey  Char(30);
       Dcl-S chrEfDt Char(7);
       Dcl-S chrEobType Char(3);
       Dcl-S chrFyr Char(3);
       Dcl-S chrPre Char(1);
       Dcl-S chrRRN Char(9);
      *** Beg Add ***** 05/30/25 ***************************
     D ##Bal           s              9p 2
     D ##newDate       s              8p 0
     D @@pfdt          s              7p 0
      *** End Add ***** 05/30/25 ***************************
      *** end add ***** 05/04/21 *****************************
      /Free
       DCL-PR PassesStateCriteria ind;
       END-PR;
      /End-Free
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D                 DS
     D  EFD                    1    035  0
     D                                     DIM(5) DESCEND
     D  EMEFD5               001    007  0
     D  EMEFD4               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD2               022    028  0
     D  EMEFD1               029    035  0
      *
     D                 DS
     D  CAD                    1    035  0
     D                                     DIM(5) DESCEND
     D  EMCAD5               001    007  0
     D  EMCAD4               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD2               022    028  0
     D  EMCAD1               029    035  0
      *
     D MODS            DS           116    OCCURS(9999)
     D  DSCO#                  1      3  0
     D  DSFND                  4      6  0
     D  DSEMP#                 7     15  0
     D  DSDIV                 16     20  0
     D  DSAMTD                21     29  2
     D  DSSELC                30     30
     D  DSSTAT                31     31
     D  DSUSER                32     41
     D  DSPGM                 42     51
     D  D2TIME                52     57  0
     D  DSDT                  58     64  0
     D  DSVIP                 65     65
     D  DSADJ                 66     66
     D  DSENAM                67    106
     D  DSSRYM               107    111  0
     D  DSFYR                112    114  0
     D  DSJRST               115    116
      *
      *   DATA STRUCTURE FOR LAST DAY OF REPORTING MONTH/YEAR.
      *
     D                 DS
     D  XXLDRM                 1      7  0
     D  XXLDCY                 1      3  0
     D  XXLDMM                 4      5  0
     D  XXLDDD                 6      7  0
      *
     D                 DS
     D  XXRPMY                 1      6  0
     D  XXRPMM                 1      2  0
     D  XXRPCY                 3      6  0
      *
     D                 DS
     D  XXCOM                  1     60
     D  XXCM60                60     60
     D TITLE0          C                   CONST('EMPLOYERS SELECTED -
     D                                     FOR AUTO -
     D                                     CANCELLATION')
     D NOTACT          C                   CONST('* Code Not Active *')
     D*** Begin Add *** 07/28/25 ****************************************
     d* Get client ID
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d   outCltID                     3

     d   outCltID      s              3    Inz('000')
     D*** End   Add *** 07/28/25 ****************************************
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *=============================================================
      * KLIST
      *=============================================================
      *
      *  KEY LIST FOR THE EMPLOYER WORKFILE
      *
     C     EBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     EBKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CNFYR
     C                   KFLD                    CNEMP#
     C                   KFLD                    CNDIV
      *
      * KEY LIST FOR THE EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      * KEY LISTS FOR THE EMPLOYER A/R DETAIL FILE
      *
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXDATE
      *
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     EAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    X2FYR             3 0
      *
      * KEY LIST FOR THE EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     ELKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      * KEY LIST FOR THE SELF-REPORTING PAYROLL TRANSACTION FILE
      *
     C     SRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXRPYM
     C*
     C* KEY LIST FOR THE SELF REPORTING PAYROLL TRANSACTION FILE, VENDOR
     C* PROCESSING ENABLED
     C*
     C     SRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    KKWEDT            7 0
      *
     C     SRKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXRPYM
      *
     C     SRKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    KKWEDT            7 0
     C*
     C* KEY LIST FOR THE TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
      *
      * KEY LIST FOR THE FUND DETAIL FILE
      *
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
      *
      * KEY LIST FOR THE CANCELLATION MAINTENANCE FILE
      *
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     CXKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     C     CXKY2         KLIST
     C                   KFLD                    CNCO#
     C                   KFLD                    CNFND
     C                   KFLD                    CNEMP#
     C                   KFLD                    CNDIV
     C                   KFLD                    CNFYR
     C                   KFLD                    X3DATE
      *
     C     CXKY3         KLIST
     C                   KFLD                    CNCO#
     C                   KFLD                    CNFND
     C                   KFLD                    CNEMP#
     C                   KFLD                    CNDIV
     C                   KFLD                    CNFYR
      *
      *
      * KEY LIST FOR THE COMPANY FILE
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      * KEY LIST FOR THE OVERRIDE FILE
      *
     C     ORKY          KLIST
     C                   KFLD                    XXPGM
     C                   KFLD                    XXPRT
      *
      * KEY LIST FOR THE USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    XXUSER
      *
      * KEY LIST FOR THE FUND MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      * KEY LISTS FOR THE MASS CANCELLATION MAINTENANCE FILE
      *
     C     CNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     C     CNKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXBAT#
     C*
     C     CNKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXBAT#
     C                   KFLD                    XXEMP#
      *
      * KEY LISTS FOR THE MASS CANCELLATION SELECTION CRITERIA WORKFILE
      *
     C     CCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXBAT#
      *
      * KEY LIST FOR THE BILLING SCHEDULE AUXILIARY FILE
      *
     C     BXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXBSCD
      *
      * KEY LISTS FOR THE CANCELLATION/REINSTATEMENT STATE FILE
      *
     C     XCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
      *
     C     XCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
      *
      *  Cancellation State
      *
     C     XCKY2         KLIST
     C                   KFLD                    CNCO#
     C                   KFLD                    CNFND
     C                   KFLD                    EBJRST
     C                   KFLD                    KKEFDT
      *
     C     XCKY3         KLIST
     C                   KFLD                    CNCO#
     C                   KFLD                    CNFND
     C                   KFLD                    EBJRST
      *
     C*
     C*  Key list for cancellation reason file
     C     CRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    KKKEY
     C                   KFLD                    KKEFDT
     C*
     C     CRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    KKKEY
     C*  KEY LIST FOR THE RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKRTCD
     C*
     C     PFKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELPFIN
     C*
     C     PFKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELPFIN
     C                   KFLD                    ELPFDT
     C*
     C     DBKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *
     C     DBKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    DSDT
      *
     C     *LIKE         DEFINE    ELFYR         SVFYR
     C     *LIKE         DEFINE    EAPODT        XXDATE
     C     *LIKE         DEFINE    CCRPMY        XXRPYM
     C     *LIKE         DEFINE    ORFIL#        XXPRT
     C     *LIKE         DEFINE    COCO#         XXCO#
     C     *LIKE         DEFINE    COCO#         XHCO#
     C     *LIKE         DEFINE    FNFND         XXFND
     C     *LIKE         DEFINE    FNFND         XHFND
     C     *LIKE         DEFINE    EAPODT        X1DATE
     C     *LIKE         DEFINE    CCBAT#        XXBAT#
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMEMP#        XHEMP#
     C     *LIKE         DEFINE    EMDIV         XHDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    EAAMT         XXAMT
     C     *LIKE         DEFINE    EAAMT         XXAMTB
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    Q1USER        XXUSER
     C     *LIKE         DEFINE    BXBSCD        XXBSCD
     C     *LIKE         DEFINE    CRST          XXST
     C     *LIKE         DEFINE    CRKEY         KKKEY
     C     *LIKE         DEFINE    CREFDT        KKEFDT
     C     *LIKE         DEFINE    CREFDT        XXEFDT
     C     *LIKE         DEFINE    RSRTCD        KKRTCD
     C     *LIKE         DEFINE    CXCFDT        XXCFDT
      *
     C                   Z-ADD     0             XXDAY             2 0
     C                   Z-ADD     0             XXMIN             7 2
     C                   Z-ADD     0             XXOCUR            4 0
     C                   Z-ADD     0             X                 5 0
     C                   Z-ADD     0             XXCNT             9 0
     C                   Z-ADD     0             QNRPMY            6 0
     C                   Z-ADD     0             XXFRDT            8 0
     C                   Z-ADD     0             XXTODT            8 0
      *
     C                   MOVE      *BLANK        XXADJ             1
     C                   MOVE      *BLANK        XXAIP             1
      *
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQDATE            8            BILLINGS THRU DT
     C                   PARM                    QQPYDT            8            PAYMENTS THRU DT
     C                   PARM                    QQETYP            1
     C                   PARM                    QQMIN             7
     C                   PARM                    QQRPMY            6
     C                   PARM                    QQDAY             2
     C                   PARM                    QQBAT#            9
     C                   PARM                    QQRSN             2
     C                   PARM                    QQCOM1           60
     C                   PARM                    QQCOM2           60
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQISDT            8
     C                   PARM                    QQEAFL            1
     C                   PARM                    QQWEDT            8
     C                   PARM                    QQXADT            8
     C                   PARM                    QQFREQ            1
     C                   PARM                    @@SRPY            1
     C                   PARM                    QQINDP            1
     C                   PARM                    @@ABIL            1
     C                   PARM                    inStInEx          1
     C                   PARM                    inStList        150
      /Free
        xxRnDt = %Dec(%Char(%Date():*Cymd0):7:0);
        xxStlist = inStlist;
      /End-Free
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
     C*
     C     AGTPL         PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0
     C                   PARM                    @@FYR             3 0
     C                   PARM      *zeros        @@AGT#            5 0
     C                   PARM      *blanks       @@AGNM           60
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C                   MOVEL(P)  'WA073R'      XXPGM
      *** beg del ***** 05/04/21 **************************
     C***                   MOVEL     '*PGM'        XXTYP             7
     C***                   CALL      'SRCHKOBJ'
     C***                   PARM                    XXPGM
     C***                   PARM                    XXTYP
     C***                   PARM                    XXNACH            1
      *** end del ***** 05/04/21 **************************
       //*** Begin Add *** 07/28/25 ****************************************
       //
       // Get client id
       GetClientID(outCltID);
       //*** End   Add *** 07/28/25 ****************************************
      *
      *  MOVE PARAMETERS INTO NUMERIC WORK RECORDS.
      *
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQDAY         XXDAY
     C                   MOVE      QQDATE        XXDATE                         BILLING THRU DT
     C                   MOVE      QQDATE        QNDATE            8 0
     C                   MOVE      QQRPMY        QNRPMY
     C                   MOVE      QQWEDT        XXWEDT            8 0
     C                   MOVE      QQXADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXXADT            7 0
      *
     C     QQINDP        IFEQ      *BLANK
     C                   MOVE      'N'           QQINDP
     C                   ENDIF
      *
     C                   MOVE      QQCOM1        XXCOM
      *
     C     XXCM60        IFEQ      '¢'
      *
     C                   MOVE      *BLANK        XXCM60
     C                   MOVE      XXCOM         QQCOM1
      *
     C                   ENDIF                                                  XXCOM1 IFEQ
      *
     C                   MOVE      QQCOM2        XXCOM
      *
     C     XXCM60        IFEQ      '¢'
      *
     C                   MOVE      *BLANK        XXCM60
     C                   MOVE      XXCOM         QQCOM2
      *
     C                   ENDIF                                                  XXCOM2 IFEQ
      *
      *  SET X1DATE TO BE USED TO CHECK FOR ACTIVE ACCOUNTS AND
      *  TO SELECT A/R RECORDS.
      *
     C                   TIME                    DSDTTM
      *
     C     QQFREQ        IFNE      *BLANK
     C                   SETON                                        43
      *
     C                   SELECT
     C     QQFREQ        WHENEQ    'B'
     C                   MOVEL     'BOTH'        XXFREQ           10
     C     QQFREQ        WHENEQ    'M'
     C                   MOVEL     'MONTHLY'     XXFREQ           10
     C     QQFREQ        WHENEQ    'W'
     C                   MOVEL     'WEEKLY'      XXFREQ           10
     C                   ENDSL
      *
     C                   ELSE
     C                   SETOFF                                       43
     C                   ENDIF
      *
     C     QNDATE        IFEQ      *ZERO
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1DATE
      *
     C                   ELSE
      *
     C                   Z-ADD     QNDATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C                   MOVEL     SRDTO         XXDATE
     C                   Z-ADD     XXDATE        X1DATE
      *
     C                   ENDIF                                                  XXDATE IFEQ
     C*
     C                   MOVE      QQPYDT        QNPYDT            8 0
     C     QNPYDT        IFEQ      *ZERO
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1PYDT            7 0
     C*
     C                   ELSE
     C                   Z-ADD     QNPYDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1PYDT
     C                   ENDIF                                                  QNPYDT=0
      *
     C                   MOVE      QQMIN         XXMIN
     C     XXMIN         DIV       100           XXPCT            11 5
     C                   MOVE      QQRPMY        XXRPMY
     C                   MOVE      QQRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         XXRPYM
     C                   MOVEL     SRDTO         ##YYMM            5
      *
      *  IF QQDAY = 0, USE LAST DAY OF MONTH, ELSE USE QQDAY - 1
     C  N43XXRPMY        IFNE      *ZERO
     C                   MOVE      QQTODT        XXTODT
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXLDRM
      *
     C     XXDAY         IFNE      *ZERO
     C     XXDAY         SUB       1             XXLDDD
     C                   ENDIF                                                  XXDAY IFEQ
      *
     C                   MOVE      *ZERO         XXFRDT
     C                   MOVE      *ZERO         XXTODT
      *
     C                   ELSE
      *
     C                   MOVE      QQFRDT        XXFRDT
     C                   MOVE      QQTODT        XXTODT
      *
     C                   ENDIF                                                  XXRPMY IFNE
     C*
     C   43XXWEDT        IFNE      *ZERO
      *
     C                   MOVE      QQTODT        XXTODT
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXLDRM
     C                   MOVEL     SRDTO         XXRPYM
      *
     C     XXXADT        IFNE      *ZERO
     C                   SUB       1             XXXADT
     C                   Z-ADD     XXXADT        XXLDRM
     C                   ENDIF                                                  XXDAY IFEQ
      *
     C                   MOVE      *ZERO         XXFRDT
     C                   MOVE      *ZERO         XXTODT
      *
     C                   ELSE
      *
     C                   MOVE      QQFRDT        XXFRDT
     C                   MOVE      QQTODT        XXTODT
      *
     C                   ENDIF                                                  XXRPMY IFNE
     C*  If not blank, put estimated issue date on route screen
     C                   MOVE      QQISDT        RSISDT            8 0
     C*
     C     RSISDT        IFNE      *ZERO
     C                   MOVE      *ON           *IN91
     C                   ELSE
     C                   MOVE      *OFF          *IN91
     C                   ENDIF                                                  RSISDT<>*BLANK
      *
      *  SET UP & INITIALIZATION
      *
     C                   MOVE      QQBAT#        XXBAT#
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
      *
     C     QQMODE        IFEQ      'D'
      *
     C                   SETON                                        09        DEMAND MODE
      *
     C                   ELSE
      *
     C                   SETON                                        08        BATCH MODE
      *
     C                   ENDIF                                                  QQMODE IFEQ
      *
      *   GET OUTPUT ROUTING INFO
      *
     C     COKY          CHAIN     PCO01                              60
      *
     C                   MOVE      Q1USER        XXUSER
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
      *
     C     UPKY          CHAIN     PUP01                              60
     C     CCKY          CHAIN     PCC01                              60
     C  N60CCADPP        COMP      'P'                                    71
      *
     C                   MOVE      Q1PGM         XXPGM
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C*
     C*  Left justify report title
     C*
     C     ' '           CHECK     ORTITL        X
     C*
     C     X             IFNE      0
     C                   SUBST(P)  ORTITL:X      ORTITL           60
     C                   ENDIF
      *
     C     *IN60         IFEQ      *OFF
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     *LIKE         DEFINE    ORTITL        X0TITL
     C*
     C*  Use default title
     C                   CLEAR                   ORTITL
     C*
     C     ORTITL        IFEQ      *BLANKS
     C                   MOVEL(P)  TITLE0        X0TITL
     C                   ELSE
     C                   MOVEL(P)  ORTITL        X0TITL
     C                   ENDIF
     C*
     C                   MOVEL(P)  X0TITL        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      60            SRDLEN            3 0
     C*
     C                   MOVEL(P)  SRDFLD        X0TITL
      *
     C                   MOVE      *ON           *INOF
      *
     C     FNKY          CHAIN     PFN01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVEL     FNNAME        XXFNNM           60
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL(P)  FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
      *
     C                   ELSE
      *
     C                   MOVE      *BLANK        XXFNNM
      *
     C                   ENDIF                                                  *IN60 IFEQ
     C*   Center company name in field
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        XXCONM           40
     C                   Z-ADD     *ZERO         XXOCUR
     C                   MOVE      *OFF          *IN69
      *
     C     EBKY          CHAIN     PEB00                              40
      *
     C                   MOVE      EBCO#         XHCO#
     C                   MOVE      EBFND         XHFND
     C                   MOVE      EBEMP#        XHEMP#
     C                   MOVE      EBDIV         XHDIV
      *
     C     *IN40         DOWEQ     *OFF
      /Free
       If PassesStateCriteria() = *Off;
          Reade ebky peb00;
          If %eof(WXEBL);
             *in40 = *On;
          Else;
             *in40 = *Off;
          Endif;
          If *in40 = *On or
             (EBco# <> XHco# or EBfnd <> XHfnd or EBemp# <> XHemp#);
             Exsr EMPBRK;
             XHdiv = EBdiv;
          Endif;
          Iter;
       Endif;
      /End-Free
      *
      *  GET NEXT RECORD TO SEE IF MORE RECENT FYR FOR SAME EMP/DIV
      *
      *
     C     *IN40         DOWEQ     *OFF
     C     EBCO#         ANDEQ     XHCO#
     C     EBFND         ANDEQ     XHFND
     C     EBEMP#        ANDEQ     XHEMP#
     C     EBDIV         ANDEQ     XHDIV
      *
     C                   READ      PEB00                                  40
      *
     C                   ENDDO                                                  *IN40 DOWEQ *OFF
      *
     C                   READP     PEB00                                  40
     C*
     C                   MOVE      *OFF          SKIPFL            1
     C*
     C*    Only process for cancellation if policy yr is current
     C**                 Z-ADD     DSDATE        SRDTI
     C**                 CALL      'SRCVTDAT'    DTCYMD
     C**                 MOVEL     SRDTO         XXRNDT            7 0
     C                   SETOFF                                       72
      *
      *    If requested, include current accounts for prior year
      *    missing reports.
      *
     C     QQETYP        IFEQ      'S'                                          S/R ACCTS ONLY
     C     @@SRPY        ANDEQ     'Y'
      *
     C     XXRNDT        IFLT      EBPFDT
     C     XXRNDT        ORGT      EBPTDT
     C                   Z-ADD     EBEMP#        XXEMP#
     C                   Z-ADD     EBDIV         XXDIV
      *
     C     EBFYR         ADD       1             XXFYR
     C     ELKY          CHAIN     PEL01                              60                         INF
      *
     C     *IN60         IFEQ      *OFF
     C     XXRNDT        ANDGE     ELPFDT
     C     XXRNDT        ANDLE     ELPTDT
     C                   SETON                                        72
     C                   Z-ADD     EBFYR         SVFYR
     C                   Z-ADD     ELFYR         EBFYR
     C                   Z-ADD     ELPFDT        EBPFDT
     C                   Z-ADD     ELPTDT        EBPTDT
     C                   ENDIF                                                  ELKY
      *
     C                   ENDIF                                                  XXRNDT
      *
     C                   ENDIF                                                  QQETYP
      *
     C*
     C     XXRNDT        IFGE      EBPFDT
     C     XXRNDT        ANDLE     EBPTDT
     C*
      *
     C                   MOVE      *OFF          *IN30
     C                   MOVE      EBFYR         XXFYR
      *
      *  GET EMPLOYER MASTER RECORD
      *
     C                   MOVE      EBEMP#        XXEMP#
     C                   MOVE      EBDIV         XXDIV
      *
     C     EMKY          CHAIN     PEM01                              60
     C*
     C*  If Canx batch control office# exists then check if same as employer
     C*  office#.  If not then skip this employer.
     C                   SETOFF                                       42
     C     CCKY          CHAIN     PCC01                              41
     C     *IN41         IFEQ      *OFF
     C     CCOFF#        ANDNE     *BLANK
     C     EMOFF#        IFNE      CCOFF#
     C                   SETON                                        42
     C                   ENDIF                                                  EMOFF#<>CCOFF#
     C                   ENDIF                                                  *IN40=*OFF
     C*
     C     EBACUR        IFEQ      'Y'
     C     @@ABIL        ANDNE     'Y'
     C                   SETON                                        42
     C                   ENDIF                                                  EBACUR = Y
     C*
     C     *IN42         IFEQ      *OFF
      *
      *  GET FUND DETAIL RECORD
      *
     C     F1KY          CHAIN     PF101                              60
      *
     C                   DO        5             X
      *
     C     EFD(X)        IFEQ      *ZERO
     C                   Z-ADD     *HIVAL        EFD(X)
     C                   ENDIF                                                  EFD,X IFEQ
      *
     C                   ENDDO                                                  DO 5 X
      *
      *  CHECK IF EMPLOYER IS ACTIVE
      *
     C                   Z-ADD     1             X                 5 0
      *
     C     X1DATE        LOOKUP    EFD(X)                               6060
      *
     C*
     C     *IN60         IFEQ      *OFF
     C                   GOTO      BYPASS
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C     CAD(X)        IFNE      *ZERO
     C*
     C*  If estimated issue date is not blank then include employers
     C*  with canx date > issue date + notice and mailing days
     C                   MOVE      QQISDT        X@ISDT            8 0
     C     X@ISDT        IFNE      *ZERO
     C*
     C                   EXSR      SRDAYS
     C*
     C                   Z-ADD     ZZDAYS        QQDAYS
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      X@ISDT        QQDT              8 0
     C                   PARM                    QQDAYS            5 0
     C                   PARM                    QQERR             1
     C*
     C     QQERR         IFNE      '1'
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXISDT            7 0
     C                   ENDIF                                                  QQERR<>'1'
     C*
     C     CAD(X)        IFLT      XXISDT
     C                   MOVE      *ON           SKIPFL
     C                   GOTO      BYPASS
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   GOTO      BYPASS
     C*
     C                   ENDIF                                                  X@ISDT<>*ZERO
     C*
     C                   ENDIF                                                  CAD,X<>*ZERO
      *
      *  GET EMPLOYER DETAIL INFO
      *
     C     ELKY          CHAIN     PEL01                              50        GET EMPLR DETAIL INF
      *
     C     *IN50         IFEQ      *OFF
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '109'
      *
      *  If S/R employer, check if reporting month
      *
     C     EBSEFL        IFEQ      'S'
     C     XXRPYM        ANDNE     *ZERO
      *
     C                   MOVE      EBCO#         #@CO#
     C                   MOVE      ELPFDT        ##PFDT
     C                   MOVE      ELBSCD        ##BSCD
      *
     C                   CALL      'WA114R'
     C                   PARM                    #@CO#             3
     C                   PARM                    ##PFDT            7
     C                   PARM                    ##BSCD            3
     C                   PARM                    ##YYMM            5
     C                   PARM      *BLANK        ##EXFL            1
     C                   PARM      *BLANK        ##TDFL            1
      *
     C     ##EXFL        IFEQ      'Y'
     C                   GOTO      BYPASS
     C                   ENDIF
      *
     C                   ENDIF                                                  EBSEFL
     C*
     C                   ENDSL
      *
      *  ONLY PROCESS ACCOUNTS THAT ARE NOT AUDITED.
      *
     C     ELFAF         IFEQ      'A'                                          AUDITED ACCT
     C                   MOVE      *ON           SKIPFL
     C                   GOTO      BYPASS
     C                   ENDIF                                                  ELFAF IFEQ
     C*
     C     ELCTST        IFEQ      'Y'
     C                   MOVE      *ON           SKIPFL
     C                   GOTO      BYPASS
     C                   ENDIF
      *** Beg Add ***** 05/30/25 **********************************
        If CltID = '153';
           ##newDate = GetNextDt(ELpfdt:10);
           //*** Begin Add *** 07/28/25 ***************************
     C                   Z-ADD     ##newDate     SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##newDate7        7 0
     C                   Z-ADD     dsDate        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         dsDate7           7 0
           //*** End   Add *** 07/28/25 ***************************
           //*** Begin Chg *** 07/28/25 ***************************
           //*** If ELnwrw = 'N' and (DSDATE < ##newDate);
           If ELnwrw = 'N' and (DSDATE7 < ##newDate7);
           //*** End   Chg *** 07/28/25 ***************************
     C                   MOVE      *ON           SKIPFL
     C                   GOTO      BYPASS
           Endif;

           ##newDate = GetNextDt(ELpfdt:11);
           //*** Begin Add *** 07/28/25 ***************************
     C                   Z-ADD     ##newDate     SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##newDate7
     C                   Z-ADD     dsDate        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         dsDate7
           //*** End   Add *** 07/28/25 ***************************
           //*** Begin Chg *** 07/28/25 ***************************
           //*** If ELnwrw = 'R' and (DSDATE < ##newDate);
           If ELnwrw = 'R' and (dsDate7 < ##newDate7);
           //*** End   Chg *** 07/28/25 ***************************
              @@pfdt = GetDATE7(GetNextDt(ELpfdt:1));
              ##Bal = ARBalance(ELbsdt:@@pfdt:Elco#:ELfnd:ELfyr:ELemp#);
              If ##Bal > 0;
     C                   MOVE      *ON           SKIPFL
     C                   GOTO      BYPASS
              Endif;
           Endif;
        Endif;
      *** End Add ***** 05/30/25 **********************************
      *
      *  DO NOT INCLUDE THIS EMPLOYER IF IT WAS ALREADY SELECTED FOR
      *  AUTO CANCELLATION. (IF EXISTS IN WXCNP).
      *
     C     CNKY          CHAIN     PCN01                              60        CO#/FND/EMP/DIV
      *
     C     *IN60         IFEQ      *OFF                                         HAS CANCEL RECORD
     C                   MOVE      *ON           SKIPFL
     C                   GOTO      BYPASS                                       NO CANCEL ALLOWED
     C                   ENDIF                                                  *IN60 IFEQ
      *
      *  CHECK CANCELLATION DETAIL FILE (CDCXP). IF A RECORD
      *  ALREADY EXISTS FOR THIS EMPLOYER & HAS NOT BEEN REINSTATED,
      *  DO NOT ALLOW CANCELLATION OF THIS EMPLOYER.
      *
      *  IF S/R ACCOUNTS ARE SELECTED BY REPORT MM/YY, SKIP 'E' TYPES.
      *  OTHERWISE, PICK UP ALL DIVISIONS IF AT LEAST ONE DIVISION
      *  HAS A/R BALANCE > MIN BALANCE REGARDLESS OF ACCOUNT TYPE.
      *
      *
      * VENDOR PROCESSING NOT ENABLED
      *
     C  N43QQETYP        IFEQ      'S'                                          S/R ACCTS ONLY
     C     XXRPMY        ANDNE     *ZERO                                        BY RPMY
     C     EBSEFL        ANDNE     'S'
      *
     C                   GOTO      BYPASS
      *
     C                   ELSE
      *
     C     QQETYP        IFEQ      'S'                                          S/R ACCTS ONLY
     C     EBSEFL        ANDNE     'S'
     C     QQETYP        OREQ      'E'                                          ESTIMATING ACCTS ONL
     C     EBSEFL        ANDNE     'E'
      *
     C                   GOTO      VALID
      *
     C                   ENDIF                                                  QQETYP IFEQ
      *
     C                   ENDIF                                                  QQETYP IFEQ
      *
      *
      * VENDOR PROCESSING  ENABLED
      *
     C   43QQETYP        IFEQ      'S'                                          S/R ACCTS ONLY
     C     XXWEDT        ANDNE     *ZERO
     C     EBSEFL        ANDNE     'S'
      *
     C                   GOTO      BYPASS
      *
     C                   ELSE
      *
     C     QQETYP        IFEQ      'S'                                          S/R ACCTS ONLY
     C     EBSEFL        ANDNE     'S'
     C     QQETYP        OREQ      'E'                                          ESTIMATING ACCTS ONL
     C     EBSEFL        ANDNE     'E'
      *
     C                   GOTO      VALID
      *
     C                   ENDIF                                                  QQETYP IFEQ
      *
     C                   ENDIF                                                  QQETYP IFEQ
      *
      *  FOR SELF-REPORTING ACCOUNTS ONLY (QQETYP = 'S'),
      *  IF ACCOUNT IS BASED ON
      *
     C  N43QQETYP        IFEQ      'S'                                          S/R ACCOUNT SELECTED
     C     EBSEFL        ANDEQ     'S'                                          S/R ACCOUNT
     C     XXRPMY        ANDNE     *ZERO                                        SELECTS MISSING MONT
      *
     C     EFD(X)        IFGT      XXLDRM                                       POLICY IN EFFECT
     C                   GOTO      BYPASS
     C                   ENDIF                                                  ELPFDT IFGT
      *
     C                   MOVE      *ON           *IN31
     C                   MOVE      EMEMP#        XXEMP#
     C                   MOVE      EMDIV         XXDIV
      *
     C  N72SRKY          CHAIN     PSR01                              52
     C   72SRKY3         CHAIN     PSR01                              52
     C     *IN52         IFEQ      *ON                                          MONTH/YR MISSING
     C                   MOVE      *ON           *IN69                          ISSUE NOTICE
     C                   GOTO      VALID                                        DONT CHECK A/R BALAN
     C                   ENDIF                                                  *IN52 IFEQ
      *
     C                   ENDIF                                                  QQETYP & BSSELF = 'S
      *
     C   43QQETYP        IFEQ      'S'                                          S/R ACCOUNT SELECTED
     C     EBSEFL        ANDEQ     'S'                                          S/R ACCOUNT
     C     XXWEDT        ANDNE     *ZERO                                        SELECTS REPORT
      *
     C     EFD(X)        IFGT      XXLDRM                                       POLICY IN EFFECT
     C                   GOTO      BYPASS
     C                   ENDIF                                                  ELPFDT IFGT
      *
     C                   MOVE      *ON           *IN31
      *
     C                   MOVE      EMEMP#        XXEMP#
     C                   MOVE      EMDIV         XXDIV
     C                   Z-ADD     XXWEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKWEDT
      *
     C  N72SRKY2         CHAIN     PSR02                              52
     C   72SRKY4         CHAIN     PSR02                              52
      *
      *  LOOK IN WASRL2 (MONTHLY REPORTING) IF NOT FOUND IN
      *  WASRL8 (WEEKLY REPORTING)
      *
     C     *IN52         IFEQ      *ON
     C  N72SRKY          CHAIN     PSR01                              52
     C   72SRKY3         CHAIN     PSR01                              52
     C                   ENDIF
     C     *IN52         IFEQ      *ON                                          REPORT MISSING
      *
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
      *
     C                   SELECT
      *
     C     QQFREQ        WHENEQ    'M'
     C     BSFREQ        ANDEQ     'M'
     C     QQFREQ        OREQ      'B'
     C     BSFREQ        ANDEQ     'M'
     C                   MOVE      *ON           *IN69                          ISSUE NOTICE
     C                   GOTO      VALID                                        DONT CHECK A/R BALAN
      *
     C     QQFREQ        WHENEQ    'W'
     C     BSFREQ        ANDEQ     'W'
     C     QQFREQ        OREQ      'B'
     C     BSFREQ        ANDEQ     'W'
      *
     C                   CALL      'SRDAY8'
     C                   PARM      XXWEDT        ##WEDT            8 0
     C                   PARM      *BLANK        ##SDAY            4
     C                   PARM      *BLANK        ##LDAY            9
     C                   PARM      *BLANK        ##ERR             1
     C                   MOVEL     ##SDAY        XXDOWK            2
      *
     C     XXDOWK        IFEQ      BSDOWK
     C                   MOVE      *ON           *IN69                          ISSUE NOTICE
     C                   GOTO      VALID                                        DONT CHECK A/R BALAN
     C                   ENDIF                                                  XXDOWK
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  *IN52 IFEQ
      *
     C                   ENDIF                                                  QQETYP & BSSELF = 'S
      *
      *  PROCESS ACCOUNTS THAT ARE BASED ON A/R BALANCE REGARLESS OF TYPE.
      *
     C                   Z-ADD     *ZERO         XXAMT
     C                   Z-ADD     *ZERO         XXAMTB                         AMT BILLED
      *
     C     XXDATE        IFNE      *ZERO
     C     XXMIN         ANDNE     *ZERO
      *
      *  ACCUMULATE THE AMOUNT DUE FROM A/R FILE (AHMEAP)
      *
     C                   MOVE      EMEMP#        XXEMP#
     C                   MOVE      EMDIV         XXDIV
     C*
     C     QQEAFL        IFNE      'A'
      *
     C                   MOVE      *OFF          XXPRYR            1
     C                   EXSR      GETBAL
     C*
     C                   ENDIF                                                  QQEAFL<>'A'
     C     QQEAFL        IFEQ      'A'
     C     QQEAFL        OREQ      'B'
     C                   EXSR      SRADTA
     C                   ENDIF
      ***IF*AMOUNT OWED EXCEEDS MINIMUM THEN WE WILL PROCESS THIS EMPL
      *
     C     CCADPP        IFEQ      'P'
      *
      *  If percent owed exceeds percent limit entered then
      *  process this employer
      *
     C     XXAMT         IFGT      *ZERO                                        AMOUNT DUE
     C     XXAMTB        ANDNE     *ZERO                                        AMOUNT BILLED
     C     XXAMT         DIV       XXAMTB        XXPCTD           15 5
      *
     C     XXPCTD        IFGE      XXPCT
     C                   MOVE      *ON           *IN69
     C                   ENDIF
      *
     C                   ENDIF                                                  XXAMT
      *
     C                   ELSE
      *
      *  PROCESS AS MINIMUM AMOUNT
      *  IF AMOUNT OWED EXCEEDS MINIMUM THEN WE WILL PROCESS THIS EMPL
      *
     C     XXAMT         IFGT      XXMIN
     C                   MOVE      *ON           *IN69
     C                   ENDIF                                                  XXAMT IFGT
      *
     C                   ENDIF                                                  CCADPP
      *
     C                   ENDIF                                                  IF XXDATE & XXMIN
      *
     C                   ELSE                                                   *IN50 = 1
      *
     C                   MOVE      *ON           SKIPFL
     C                   GOTO      BYPASS
      *
     C                   ENDIF                                                  *IN50 IFEQ
      *
      *  EACH VALID EMPLOYER/DIV ADD 1 TO OCCURANCE OF DATA STRUCTURE
      *
     C     VALID         TAG
      *
     C                   ADD       1             XXOCUR
      *
     C     XXOCUR        IFNE      *ZERO
      *
      *  LOAD MULTI-OCCURANCE DATA STRUCTURE
      *
     C     XXOCUR        OCCUR     MODS
      *
     C                   MOVE      XXCO#         DSCO#
     C                   MOVE      XXFND         DSFND
     C                   MOVE      EMEMP#        DSEMP#
     C                   MOVE      EMDIV         DSDIV
     C                   MOVE      EBFYR         DSFYR
     C                   MOVE      EBJRST        DSJRST
     C                   MOVE      XXAMT         DSAMTD
     C                   MOVE      'Y'           DSSELC
     C                   MOVE      ' '           DSSTAT
     C                   MOVE      Q1USER        DSUSER
     C                   MOVE      Q1PGM         DSPGM
     C                   Z-ADD     DSTIME        D2TIME
     C                   Z-ADD     DSDATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C                   MOVEL     SRDTO         DSDT
      *
      *  IF ANY NEGATIVE ADJUSTMENTS EXIST IN A/R FILE AFTER THE BALANCE
      *  DATE, DUE TO A REVISED BILL, LOAD 'R' TO ADJUSTMENT FLAG.
      *
     C                   MOVE      *BLANK        XXADJ
     C*
     C     QQEAFL        IFEQ      'E'
     C     QQEAFL        OREQ      'B'
      *
     C     EBSEFL        IFEQ      'E'
      *
     C                   MOVE      EMEMP#        XXEMP#
     C                   MOVE      EMDIV         XXDIV
      *
     C     EAKY1         SETLL     PEA01
     C     EAKY          READE     PEA01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     EAARTY        IFEQ      'A'
     C     EAAMT         ANDLT     *ZERO
     C     EAPODT        ANDGT     XXDATE
      *
     C                   MOVE      'R'           XXADJ
     C                   MOVE      *ON           *IN60
      *
     C                   LEAVE
      *
     C                   ENDIF                                                  EAARTY IFEQ
      *
     C     EAKY          READE     PEA01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C                   ENDIF                                                  EBSEFL IFEQ
      *
     C                   ENDIF                                                  EST/BOTH
      *
     C                   MOVE      EMVIP         DSVIP
      *
     C     NEGADJ        IFEQ      'R'
     C                   MOVE      NEGADJ        XXADJ
     C                   ENDIF
      *
     C                   MOVE      XXADJ         DSADJ
     C                   MOVEL     EMNAME        DSENAM
     C                   MOVE      XXRPYM        DSSRYM
     C                   MOVE      *ON           *IN30                          EMP ADDED TO WORKFIL
      *
     C                   ENDIF                                                  XXOCUR IFNE
      *
     C     BYPASS        TAG
      *
     C                   Z-ADD     *ZERO         XXAMT
      *
      *  IF EMPLOYER DOES NOT MEET SELECTION CRITERIA BUT NEEDS TO BE
      *  ADDED TO THE WORKFILE, LOAD THE WORKFILE WITH AMT DUE OF ZERO
      *
     C     *IN30         IFEQ      *OFF                                         EMP NOT IN WORKFILE
     C     SKIPFL        ANDEQ     *OFF
      *
      *  LOAD MULTI-OCCURANCE DATA STRUCTURE. THIS RECORD SHOULD NOT
      *  SHOW UP ON THE MAINTENANCE SCREEN.
      *
     C                   ADD       1             XXOCUR
      *
     C     XXOCUR        IFNE      *ZERO
      *
     C     XXOCUR        OCCUR     MODS
      *
     C                   MOVE      XXCO#         DSCO#
     C                   MOVE      XXFND         DSFND
     C                   MOVE      EMEMP#        DSEMP#
     C                   MOVE      EMDIV         DSDIV
     C                   MOVE      EBFYR         DSFYR
     C                   MOVE      *ZERO         DSAMTD
     C                   MOVE      'Y'           DSSELC
     C                   MOVE      ' '           DSSTAT
     C                   MOVE      Q1USER        DSUSER
     C                   MOVE      Q1PGM         DSPGM
     C                   Z-ADD     DSTIME        D2TIME
     C                   Z-ADD     DSDATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C                   MOVEL     SRDTO         DSDT
     C                   MOVE      EMVIP         DSVIP
     C                   MOVE      *BLANK        DSADJ
     C                   MOVEL     EMNAME        DSENAM
     C                   MOVE      *ZERO         DSSRYM
      *
     C                   ENDIF                                                  XXOCUR IFNE
      *
     C                   ENDIF                                                  *IN30 IFEQ
      *
     C                   ENDIF                                                  *IN42=*OFF
     C                   ENDIF                                                  XXRNDT>=EBPFDT
     C*
     C     EBKY          READE     PEB00                                  40
      *
      *  IF NEXT EMPLOYER IS READ, WE MUST CHECK TO SEE IF ANY DIV
      *  OF THE EMPLOYER HAS PAST DUE > XXMIN, IF SO WE WILL
      *  WRITE ALL DIVISIONS FOR THE EMPLOYER TO THE WORK FILE.
      *
     C     *IN40         IFEQ      *ON
     C     EBCO#         ORNE      XHCO#
     C     EBFND         ORNE      XHFND
     C     EBEMP#        ORNE      XHEMP#
      *
     C                   EXSR      EMPBRK
      *
     C                   ENDIF                                                  *IN40 IFEQ
      *
     C                   MOVE      EBDIV         XHDIV
      *
     C                   ENDDO                                                  *IN40 DOWEQ*OFF
      *
      *  LIST EMPLOYER ON REPORT - READ WORKFILE BY EMP NAME & PRINT
      *
     C                   MOVE      *ZERO         XXCNT
      *
     C     CNKY2         CHAIN     PCN02                              60        CNKY=XXCO/XXFND/XXBA
      *
     C     *IN60         DOWEQ     *OFF                                         PCN02=CO/FND/BAT/ENA
      *
      *
      *  MARK ACCOUNTS WHICH ARE ON AN ANNUAL INSTALLMENT PLAN
      *
     C                   MOVE      CNEMP#        EMEMP#
     C                   MOVE      CNDIV         EMDIV
     C                   MOVE      *BLANK        XXAIP                          ANNUAL INSTALLMT PLA
     C                   MOVE      EMEMP#        XXEMP#
     C                   MOVE      EMDIV         XXDIV
     C                   Z-ADD     CNFYR         XXFYR
      *
     C     ELKY          CHAIN     PEL01                              60        GET BILLING SCHEDULE
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVEL     ELBSCD        XXBSCD
      *
     C     BXKY          CHAIN     PBX01                              60        GET INSTALL PLAN FLA
      *
     C     *IN60         IFEQ      *OFF
     C     BXTYPE        ANDEQ     'A'
      *
     C                   MOVE      'A'           XXAIP
      *
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*    Format 'pol from dt' for output
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT            8 0
     C*
     C*    Format annual premium (audited/estimated) for output
     C     ELFAF         IFNE      'A'
     C                   Z-ADD     ELANPR        XXPREM           11 2
     C                   ELSE
     C                   Z-ADD     ELAAPR        XXPREM
     C                   ENDIF                                                  ELFAF<>'Y'
     C                   ADD       XXPREM        XXPTTL           13 2          total the premium
     C*
     C*    Get agent number and name
     C                   Z-ADD     ELCO#         @@CO#
     C                   Z-ADD     ELFND         @@FND
     C                   Z-ADD     ELFYR         @@FYR
     C                   Z-ADD     ELEMP#        @@EMP#
     C                   Z-ADD     ELDIV         @@DIV
     C*
     C                   CALL      'WA002R'      AGTPL
     C*
     C                   Z-ADD     @@AGT#        XXAGT#            5 0
     C                   MOVEL(P)  @@AGNM        XXAGNM           25
     C*
     C                   MOVEL(P)  ELPFIN        XXPFIN            5
      *
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*  Get Batch CX description
     C*
     C                   CLEAR                   EBJRST
     C     EBKY1         CHAIN     PEB01                              60
     C*
     C                   MOVEL(P)  EBJRST        XXST
     C                   MOVEL     CNRECD        KKKEY
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   EXSR      CXRSIN
     C*
     C                   MOVEL     CRDESC        XBRSDS           21
     C*
     C*  Find other CX for employer
     C                   EXSR      CXCNT
     C*
     C*  Gather other employer info
     C                   CLEAR                   CNPFIN
     C                   CLEAR                   CNACH
     C*
     C                   MOVE      XXAIP         CNAIP
     C*
     C*
      *** beg chg ***** 05/04/21 **************************
     C**** CHECK IF NEW ACH MODULE IS INSTALLED ('WC1806')
     C****
     C***     XXNACH        IFEQ      *OFF
     C***     DBKY          CHAIN     PEF01                              62
     C***     *IN62         DOWEQ     *OFF
     C***     XXDATE        IFLT      EFEXDT
     C***     XXDATE        ANDGE     EFEFDT
     C***     EFEXDT        OREQ      *ZERO
     C***     XXDATE        ANDGE     EFEFDT
     C***                   MOVE      'Y'           CNACH
     C***                   LEAVE
     C***                   ENDIF
     C***     DBKY          READE     PEF01                                  62
     C***                   ENDDO
     C***                   ELSE
     C***     DBKY1         SETGT     PDB03
     C***     DBKY          READPE    PDB03                                  60
     C****
     C***     *IN60         IFEQ      *OFF
     C***     DBEXDT        ANDEQ     *ZERO
     C***     *IN60         OREQ      *OFF
     C***     DBEXDT        ANDNE     *ZERO
     C***     DSDT          ANDLT     DBEXDT
     C***                   MOVE      'Y'           CNACH
     C***                   ENDIF
     C***                   ENDIF
       chrEfDt = %Editc(xxDate:'X');
       chrEftKey = %Editc(ELemp#:'X') + %Editc(ELdiv:'X');
       chrFyr = %Editc(ELfyr:'X');
       If GetEftStatus(qqCo#:chrEftType:QQfnd:
        chrEftKey:chrEfDt:chrPre:chrRrn:chrEobType:
        chrChkType:chrFyr) = *On;
          CNach = 'Y';
       Endif;
      *** end chg ***** 05/04/21 **************************
     C*
     C     PFKY2         SETGT     PPF00
     C     PFKY          READPE    PPF00                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'Y'           CNPFIN
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   UPDATE    PCN02
     C*
     C                   MOVEL     CNENAM        XXENAM           28
      *
     C     *INOF         IFEQ      *ON
      *
     C                   EXCEPT    HEAD                                         CHECK FOR OVERFLOW
     C                   EXCEPT    HEAD1
      *
     C                   MOVE      *OFF          *INOF
      *
     C                   ENDIF                                                  *INOF IFEQ
      *
     C                   EXCEPT    DET
      *
     C                   ADD       1             XXCNT                          COUNT RECORDS
      *
     C     CNKY2         READE     PCN02                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ
      *
      *  PRINT REPORT TOTAL
      *
     C     XXCNT         IFEQ      *ZERO                                        NO GRADES ASSIGNED
      *
     C                   EXCEPT    HEAD
     C                   EXCEPT    MESG
      *
     C                   ELSE
      *
     C     *INOF         IFEQ      *ON
      *
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   EXCEPT    TOTAL
     C*
     C                   ELSE
     C                   EXCEPT    TOTAL
      *
     C                   ENDIF                                                  *INOF IFEQ
      *
     C                   ENDIF                                                  XXCNT IFEQ
      *
      *  UPDATE BATCH PROCESSING FLAG IN SELECTIION CRITERIA WORKFILE
      *
     C     CCKY          CHAIN     PCC01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      'Y'           CCBPF
      /Free
        CCstInEx = inStInEx;
        CCstList = inStList;
      /End-Free
      *
     C                   UPDATE    PCC01
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   MOVE      *ON           *INLR
      /Free
       xxPgm = q1pgm;
       xxPrt = '0';
       If inStInEx = 'E';
          xxStinEx = 'Exclude';
       Else;
          xxStInEx = 'Include';
       Endif;


       If inStList = *blanks;
          xxStList1 = 'All';
       Else;
          For i = 1 to 75;
             If AryStList(i) > *blank;
                Select;
                When i > 1 and i < 40;
                   xxStList1 = %trim(xxStList1) + ', ' + AryStList(i);
                When i > 40;
                   xxStList2 = %Trim(xxStList2) + ', ' + AryStList(i);
                When i = 1;
                   *in65 = *off;
                   xxStlist1 = AryStList(i);
                When i = 40;
                   *in65 = *on;
                   xxStlist2 = AryStList(i);
                Endsl;
             Endif;
          Endfor;
       Endif;

       Chain orKy POR01;
       If %found;
          Except Route;
       Endif;
      /End-free
     C*____________________________________________________________________
     C*
     C*====================================================================
     C*  CXCNT - Counts the number of cancellations for an employer
     C*====================================================================
     C     CXCNT         BEGSR
      *
      *  Get number of months back to look for CX
      *
     C                   CLEAR                   CXRSN
     C                   CLEAR                   CXCXDT
     C                   CLEAR                   X8CXDT
     C                   CLEAR                   CNCNC#
      *
     C     CRNCCI        IFEQ      '05'                                         NON-PAYMENT
     C                   ADD       1             CNCNC#
     C                   ENDIF                                                  CXERR
      *
     C                   Z-ADD     *ZERO         X3DATE            7 0
     C     XCKY2         SETGT     PXC01
     C     XCKY3         READPE    PXC01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     XCCXMN        ANDNE     *ZERO
     C                   TIME                    DSDTTM
     C                   Z-ADD     XCCXMN        ##MTHS
      *
     C                   CALL      'SRPMTH8'
     C                   PARM      DSDATE        ##DATE            8 0
     C                   PARM                    ##MTHS            3 0
     C                   PARM      *OFF          ##ERR             1
      *
     C     ##ERR         IFEQ      *OFF
     C                   Z-ADD     ##DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X3DATE
     C                   Endif                                                  If ##err = *off
     C*
     C                   Endif                                                  If *in60 = *off
      *
      *  Count CX
      *
     C     CXKY2         SETLL     PCX02
     C     CXKY3         READE     PCX02                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     CXERR         IFNE      'Y'
     C                   EXSR      CXEXST
      *
     C     CRNCCI        IFEQ      '05'                                         NON-PAY
     C                   ADD       1             CNCNC#
     C                   ENDIF                                                  CRNCCI
      *
     C                   ENDIF                                                  CXERR
      *
     C     CXKY3         READE     PCX02                                  60
     C                   ENDDO                                                  CXKY
      *
      *
      *
     C     CNCNC#        IFEQ      *ZERO
     C                   CLEAR                   CXRSN
     C                   CLEAR                   XCRSDS
     C                   ENDIF
      *
     C                   ENDSR                                                  CXCNT
      *
     C*
     C*====================================================================
     C*  CXEXST - Process existing cancellation information
     C*====================================================================
     C     CXEXST        BEGSR
     C*
     C                   Z-ADD     CXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X8CXDT            8 0
     C*
     C                   MOVEL(P)  CXRSN         KKKEY
     C                   MOVE      CXRST         XXST
     C*
     C                   EXSR      CXRSIN
     C*
     C                   MOVEL     CRDESC        XCRSDS           20
     C*
     C                   ENDSR                                                  CXEXST
     C*
     C*====================================================================
     C*  CXRSIN - Get cancellation reason information
     C*====================================================================
     C     CXRSIN        BEGSR
     C*
     C     CRKY          SETGT     PCR01
     C     *IN60         DOUEQ     *ON
     C     CRSTAT        ORNE      'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   CRKEY
     C                   CLEAR                   CRRFEE
     C                   CLEAR                   CRDESC
     C                   CLEAR                   CRNCCI
     C*** Begin Add *** 07/28/25 *******************************************
     C                   CLEAR                   CRLFEE
     C*** End   Add *** 07/28/25 *******************************************
     C                   ENDIF
     C*
     C     CRDESC        IFEQ      *BLANKS
     C     *IN60         ANDEQ     *ON
     C                   MOVEL(P)  NOTACT        CRDESC
     C                   ENDIF
     C*
     C                   ENDSR                                                  CXRSIN
      *
      *===================================================================
      * LEVEL BREAK ON EMPLOYEE #.  SUBROUTINE               E M P B R K *
      *===================================================================
      *
     C     EMPBRK        BEGSR
      *
      *  IF AT LEAST ONE VALID EMPLOYER WAS PROCESSED AND AT LEAST ONE
      *  OF THE DIVISIONS HAD AN AMOUNT DUE OVER XXMIN (*IN69 = *ON)
      *  THEN WE WILL WRITE ALL OF THE DIVISIONS OF THE EMPLOYER TO
      *  THE WORK FILE BY LOOPING THROUGH THE MULTI-OCCUR DATA STRUC.
      *  AND PERFORMING THE WRITES.
      *
     C     XXOCUR        IFGT      *ZERO
     C     *IN69         ANDEQ     *ON
      *
     C                   DO        XXOCUR        X
      *
     C     X             OCCUR     MODS
      *
      *  MOVE VALUES TO CN FIELDS  IN CANCELATION FILE
      *
     C                   MOVE      DSCO#         CNCO#
     C                   MOVE      DSFND         CNFND
     C                   MOVE      DSEMP#        CNEMP#
     C                   MOVE      DSDIV         CNDIV
     C                   MOVE      DSFYR         CNFYR
     C                   MOVE      *ZERO         CNDATE
     C                   MOVE      DSAMTD        CNAMTD
     C                   MOVE      DSSELC        CNSELC
     C                   MOVE      DSSTAT        CNSTAT
     C                   MOVE      DSUSER        CNUSER
     C                   MOVE      DSPGM         CNPGM
     C                   MOVE      D2TIME        CNTIME
     C                   MOVE      DSDT          CNDT
     C                   MOVEL     QQRSN         CNRECD
     C                   MOVE      QQBAT#        CNBAT#
     C                   MOVE      DSVIP         CNVIP
     C                   MOVE      DSADJ         CNADJ
     C                   MOVEL     DSENAM        CNENAM
     C                   MOVE      DSSRYM        CNSRYM
     C                   MOVEL     QQCOM1        CNCOM1
     C                   MOVEL     QQCOM2        CNCOM2
     C                   Z-ADD     KKWEDT        CNWEDT
      *
      * READ THE CANCELLATION/REINSTATEMENT STATE FILE IF THE
      * JURISDICTION STATE HAS CHANGED.
      *
     C     DSJRST        IFNE      XXST
      *
     C                   MOVE      DSJRST        XXST
      *
     C                   ENDIF                                                  DSJRST IFNE
     C*
     C                   MOVEL     CNRECD        KKKEY
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   EXSR      CXRSIN
     C*
     C                   MOVEL(P)  CRKEY         CNRECD
     C                   Z-ADD     CRRFEE        CNRFEE
       //*** Begin Add *** 07/28/25 ****************************************
       //
       // Populate the late fee information if:
       // 1)  Client 153
       // 2)  Not a PEO Group
       // 3)  State Reporting Division
       // 4a) Not(Weekly Frequency (and)
       // 4b)     Self Reporting Billing Plan (and)
       // 4c)     Non-Reporting of Payroll)
       If outCltID = '153' And FNpeoF = ' ' And
          GetStRptDv(CNco#:CNfnd:CNemp#) = CNdiv And
          Not (BSfreq = 'W' And BSseFL = 'S' And
               CCeTyp = 'S' And CCweDt <> 0);

          //
          // There must be a late fee and a late fee code
          If CRlFee <> 0 And CRlfCd <> ' ';
             CNlFee  = CRlFee;
          Else;
             CNlFee  = 0;
          EndIf;
       Else;
          CNlFee  = 0;
       EndIf;
       //*** End   Add *** 07/28/25 ****************************************
      *
     C                   WRITE     PCN01
      *
     C                   ENDDO                                                  DO XXOCUR X
      *
     C                   ENDIF                                                  XXOCUR IFGT
      *
      *  INITIALIZE THE LEVEL BREAK FIELDS AND RESET FOR NEXT EMPLOYER
      *
     C                   MOVE      *OFF          *IN69
     C                   MOVE      EBCO#         XHCO#
     C                   MOVE      EBFND         XHFND
     C                   MOVE      EBEMP#        XHEMP#
     C                   Z-ADD     *ZERO         XXOCUR
      *
     C                   ENDSR                                                  EMPBRK
     C*================================================================
     C*  SRDAYS - Checks all divs of employer to determine the number
     C*           of days to add to issue date to get cancel date.
     C*================================================================
     C     SRDAYS        BEGSR
     C*
     C                   Z-ADD     *ZERO         ZZDAYS            5 0
     C*
     C*  Cycle through all divs for employer
     C*
     C*    Get employer detail for fyr/emp#/div
     C     ELKY2         CHAIN     PEL02                              61
     C     *IN61         DOWEQ     *OFF
     C*
     C*      Get jurisdiction state
     C                   MOVE      ELRTCD        KKRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*      Chain to canx reason code file
     C                   MOVEL(P)  QQRSN         KKKEY
     C                   MOVE      RSJRST        XXST
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
      *
     C*
     C  N60              MOVEL(P)  CRDESC        XREASN           40
     C   60              MOVE      *BLANKS       XREASN
     C                   MOVE      ' '           POLNEW            1
     C     CRNDNW        IFGT      0
     c* Number of days to consider policy new
     C                   Z-ADD     CRNDNW        QQDAYS
     C*
     c** Calculate new work date
     C                   CALL      'SRPAST8'
     C                   PARM      X@ISDT        QQDT              8 0
     C                   PARM                    QQDAYS            5 0
     C                   PARM                    QQERR             1
      *
     C     QQERR         IFNE      '1'
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X@WRDT            7 0
     C                   ENDIF                                                  QQERR<>'1'
      *
     C                   Z-ADD     ELCO#         ##CO#
     C                   Z-ADD     ELFND         #FND
     C                   Z-ADD     ELEMP#        ##EMP#
     C                   Z-ADD     ELDIV         ##DIV
     C                   Z-ADD     *ZEROS        #@DATE
     C                   CALL      'WA084R'
     C                   PARM                    ##CO#             3 0
     C                   PARM                    #FND              3 0
     C                   PARM                    ##EMP#            9 0
     C                   PARM                    ##DIV             5 0
     C                   PARM                    #@DATE            7 0
     C*
     C     #@DATE        IFGE      X@WRDT
     C                   MOVE      'Y'           POLNEW
     C                   ENDIF                                                  X#DATE > X@WRDT
     C                   ENDIF                                                  CRNDNW > 0
     C*
     C*      Calculate number of days to add to issue date
     C                   Z-ADD     0             XXDAYS            5 0
     C*
     C     ELCXDY        IFNE      0
     C                   ADD       ELCXDY        XXDAYS
     C                   ELSE
     C     POLNEW        IFNE      'Y'
     C                   ADD       CRNOTC        XXDAYS
     C                   ELSE
     C                   ADD       CRNTNP        XXDAYS
     C                   ENDIF                                                  POLNEW = 'Y'
     C                   ENDIF                                                  ELCXDY<>0
     C*
     C     ELADDM        IFNE      0
     C                   ADD       ELADDM        XXDAYS
     C                   ELSE
     C     POLNEW        IFNE      'Y'
     C                   ADD       CRADDM        XXDAYS
     C                   ELSE
     C                   ADD       CRMDNP        XXDAYS
     C                   ENDIF                                                  POLNEW <>'Y'
     C                   ENDIF                                                  ELADDM<>0
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C     XXDAYS        IFGT      ZZDAYS
     C                   Z-ADD     XXDAYS        ZZDAYS
     C                   ENDIF                                                  XXDAYS>ZZDAYS
     C*
     C     ELKY2         READE     PEL02                                  61
     C                   ENDDO                                                  *IN61=*OFF
     C*
     C                   ENDSR                                                  SRDAYS
     C*
     C*================================================================
     C*  SRADTA - accumulate the balance for all prior audited policy
     C*           years (not including the year being processed).
     C*================================================================
     C     SRADTA        BEGSR
     C                   MOVE      *BLANKS       NEGADJ            1
     C*
     C     ELKY3         CHAIN     PEL03                              60
     C     *IN60         DOWEQ     *OFF
      *
     C                   CALL      'WA002R'
     C                   PARM      ELCO#         ##CO#             3 0
     C                   PARM      ELFND         ##FND             3 0
     C                   PARM      ELEMP#        ##EMP#            9 0
     C                   PARM      ELDIV         ##DIV             5 0
     C                   PARM      ELFYR         ##FYR             3 0
     C                   PARM      *zeros        ##AGT#            5 0
     C                   PARM      *blanks       ##ANAM           60
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C                   PARM                    ##AREA            3 0
     C                   PARM                    ##PHON            7 0
     C                   PARM      '*'           ##PLAN            1
     C                   PARM      *ZERO         ##DATE            8 0
      *
     C*
     C     ELFYR         IFLT      EBFYR
     C     ELFAF         ANDEQ     'A'
     C     ELCTST        ANDNE     'Y'
     C     ##PLAN        ANDNE     'A'
     C*
      ***Set*flag if negative audit adjustment since bill thru date
      ***in*prior year
     C                   MOVE      *ON           XXPRYR            1
     C                   EXSR      GETBAL
     C                   ENDIF                                                  ELFYR<EBFYR
     C     ELKY3         READE     PEL03                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C*    Reset to co/fnd/emp/div/fyr
     C     ELKY          CHAIN     PEL03                              60
     C*
     C                   ENDSR                                                  SRADTA
      *================================================================
      * GETBAL -  A/R BALANCE ROUTINE
      *================================================================
     C     GETBAL        BEGSR
      *
     C                   Z-ADD     ELFYR         X2FYR
     C     EAKY2         CHAIN     PEA01                              41
     C     *IN41         DOWEQ     *OFF
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C                   SELECT

      *
      * EXCLUDE DIVIDEND PLAN TRANSACTION FROM BALANCE CALC
      *
     C     EAARCD        WHENEQ    F1DVBL
     C     EAARCD        OREQ      F1DVPD
     C     EAARCD        OREQ      F1DVXR
      *
      * PREM AND FEE BALANCE
      *
     C     TTCTYP        WHENNE    'D'
     C                   SELECT
     C     EAARCD        WHENEQ    F1RCCD
     C     EATRDT        ANDGT     X1DATE
     C     EAARTY        WHENEQ    'P'
     C     EAARTY        OREQ      'Q'
     C     EAARTY        OREQ      'T'
     C                   SUB       EAAMT         XXAMT
     C     EAARTY        WHENEQ    'R'
     C     EAARTY        OREQ      'N'
     C     EAARTY        OREQ      'V'
     C                   ADD       EAAMT         XXAMT
     C                   OTHER
     C     EAPODT        IFLE      X1DATE
     C                   ADD       EAAMT         XXAMT
     C                   ADD       EAAMT         XXAMTB
     C                   ENDIF                                                  EAPODT<=X1DATE
     C                   ENDSL
      *
      *  Set flag if negative audit adjustment since bill thru date
      *  in prior year
      *
     C     XXPRYR        IFEQ      *ON
     C     EAARTY        IFEQ      'A'
     C     EAARTY        OREQ      'D'
     C     EAAMT         IFLT      *ZERO
     C     EAPODT        ANDGT     X1DATE
     C                   MOVE      'R'           NEGADJ
     C                   ENDIF                                                  EAAMT < 0
     C                   ENDIF                                                  EAARTY = A D
     C                   ENDIF                                                  XXPRYR = *ON
      *
      * DEPOSIT AND DEPOSIT TAX BALANCE
      *
     C     TTCTYP        WHENEQ    'D'
     C     QQINDP        ANDEQ     'Y'
     C                   SELECT
     C     EAARTY        WHENEQ    'H'
     C     EAPODT        ANDLE     X1DATE
     C     EAARTY        OREQ      'F'
     C     EAPODT        ANDLE     X1DATE
     C                   ADD       EAAMT         XXAMT
     C                   ADD       EAAMT         XXAMTB
     C     EAARTY        WHENEQ    'R'
     C     EAARTY        OREQ      'N'
     C     EAARTY        OREQ      'V'
     C                   ADD       EAAMT         XXAMT
     C     EAARTY        WHENEQ    'T'
     C     EAARTY        OREQ      'I'
     C     EAARTY        OREQ      'Q'
     C                   SUB       EAAMT         XXAMT
     C                   ENDSL
     C                   ENDSL                                                  TTCTYP <> 'D'
     C     EAKY2         READE     PEA01                                  41
     C                   ENDDO                                                  *IN41 DOWEQ
      *
     C                   ENDSR                                                  GETBAL
      *
      *****************************************************************
      *
     OQSYSPRT   E            ROUTE          1 06
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O          E            ROUTE          2 33
     O                                           18 'REPORT SELECTIONS:'
     O*    Single spacing report selections
     O          E            ROUTE          1
     O                                           26 'COMPANY        :'
     O                       XXCO#         4     33
     O          E            ROUTE          1
     O                                           26 'GROUP          :'
     O                       XXFND         4     33
     O          E            ROUTE          1
     O                                           26 'TYPE OF ACCOUNT:'
     O                       QQETYP              31
     O          E            ROUTE          1
     O                                           26 'POL EFF FROM DT:'
     O                       XXFRDT              40 '  /  /    '
     O          E            ROUTE          1
     O                                           26 'POL EFF TO DT  :'
     O                       XXTODT              40 '  /  /    '
     O          E            ROUTE          1
     O                                           26 'BILL THROUGH DT:'
     O                       QNDATE              40 '  /  /    '
     O          E            ROUTE          1
     O              N71                          26 'MINIMUM BALANCE:'
     O               71                          26 '% OF PREMIUM   :'
     O                       XXMIN         4     39
     O          E            ROUTE          1
     O              N43                          26 'REPORT MONTH/YR:'
     O              N43      QNRPMY              37 '  /    '
     O               43                          26 'REPORT DATE    :'
     O               43      XXWEDT              37 '  /  /    '
     O          E    43      ROUTE          1
     O                                           26 'REPORT FREQ    :'
     O                       XXFREQ              37
     O          E            ROUTE          1
     O                                           26 'INC CURR ACTS W/'
     O          E            ROUTE          1
     O                                           26 'PRIOR-YR MISSNG:'
     O                       @@SRPY              31
     O          E            ROUTE          1
     O                                           26 'EXCLUD NW ACCTS '
     O          E            ROUTE          1
     O                                           26 'START ON/AFTER :'
     O                       XXDAY         Z     32
     O          E            ROUTE          1
     O                                           26 'CANCEL REASON  :'
     O                       QQRSN               32
     O          E            ROUTE          1
     O                                           26 'COMMENT 1      :'
     O                       QQCOM1              90
     O          E            ROUTE          1
     O                                           26 'COMMENT 2      :'
     O                       QQCOM2              90
     O          E            ROUTE          1
     O                                           26 'EST ISSUE DATE :'
     O               91      RSISDT        Y     40
     O          E            ROUTE          1
     O                                           26 'BALANCE TYPE   :'
     O                       QQEAFL              31
     O          E            ROUTE          1
     O                                           26 'INCLUDE DEPOSIT:'
     O                       QQINDP              31
     O          E            ROUTE          1
     O                                           26 'STATES:'
     O                       xxStInEx            34
     O                       xxStList1         +001
     O          E    65      ROUTE          1
     O                       xxStList2         +001
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             119
     O                       DSDATE        Y    196
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       X0TITL             129
     O                                          190 'PAGE:'
     O                       PAGE          Z    196
     O          E            HEAD           2
     O                                          100 'BATCH NUMBER:'
     O                       CNBAT#        Z    110
     O          E            HEAD           2
     O                                           08 'GROUP:'
     O                       XXFND         Z     12
     O                       XXFNNM              74
     O          E            HEAD1          1
     O                                           62 'ANN'
     O                                           66 'NEG'
     O                                           70 'VIP'
     O                                           79 'POLICY'
     O                                           95 'ANNUAL '
     O                                          132 'PREM'
     O                                          163 '# OF'
     O          E            HEAD1          1
     O                                            9 'EMPLOYER'
     O                                           15 'DIV'
     O                                           19 'YR'
     O                                           33 'EMPLOYER NAME'
     O              N31                          59 'AMOUNT DUE '
     O               31N43                       56 'RPMY'
     O               31 43                       55 'RPDT'
     O                                           62 'INS'
     O                                           66 'ADJ'
     O                                           70 'FLG'
     O                                           80 'EFF DATE'
     O                                           95 'PREMIUM '
     O                                          101 'AGT #'
     O                                          112 'AGENT NAME'
     O                                          132 'FIN'
     O                                          158 'CURR CX REASON CODE/DESC'
     O                                          162 'CX'
     O                                          188 'EXISTING CX REASON CODE/'
     O                                         +  0 'DESC/DATE'
     O          E            DET         1  0
     O                       CNEMP#        4      9
     O                       CNDIV         4     15
     O                       CNFYR         4     19
     O                       XXENAM              50
     O              N31      CNAMTD        M     59
     O               31N43   QNRPMY              57 '  /    '
     O               31 43   XXWEDT              58 '  /  /   0'
     O                       XXAIP               61
     O                       CNADJ               65
     O                       CNVIP               69
     O                       XXPFDT        Y     81
     O                       XXPREM        M     95
     O                       XXAGT#        4    101
     O                       XXAGNM             127
     O                       XXPFIN             133
     O                       CNRECD             136
     O                       XBRSDS             158
     O                       CNCNC#        Z    162
     O                       CXRSN              166
     O                       XCRSDS             187
     O                       X8CXDT             198 '  /  /    '
     O          E            TOTAL       2  1
     O                                           24 'NUMBER OF EMPLOYERS'
     O                                           34 'SELECTED:'
     O                       XXCNT         4     46
     O                                           80 'TOTAL ANNUAL PREMIUM:'
     O                       XXPTTL        M     95
     O          E            MESG        2  1
     O                                           27 '*** NO RECORDS MEET THE'
     O                                         +001 'SELECTED CRITERIA ***'
      /Free
       Dcl-Proc PassesStateCriteria;
       Dcl-Pi *N Ind;
       End-Pi;
       Dcl-S isInList Ind;
       Dcl-s ix int(3);
       Dcl-s KKjrst Char(2);
       Dcl-s STcnt int(5);
       Dcl-s STexcl int(5);

       isInList = *Off;
       STcnt = *Zeros;
       STexcl = *Zeros;
       Setll *loval PEB02;

       Select;

       When inStInEx = 'I';
          For ix = 1 to 75;
             If AryStList(ix) > *blank;
                kkjrst = AryStList(ix);
                Setll (EBco#:EBfnd:EBfyr:EBemp#:KKjrst) PEB02;
                If %equal(WXEBL2);
                   isInList = *On;
                   Leave;
                Endif;
             Endif;
          Endfor;

       // If 'Excluded' Return *Off if any division has a state not in the state list
       When inStInEx = 'E';
          Setll (EBco#:EBfnd:EBfyr:EBemp#) PEB02;
          If %equal(WXEBL2);
             Reade (EBco#:EBfnd:EBfyr:EBemp#) PEB02;
             Dow not %eof(WXEBL2);
                STcnt = Stcnt + 1;
                ix = %LookUp(B2jrst:AryStList);
                If ix > *Zero;
                   STexcl = STexcl + 1;
                Endif;
                Reade (EBco#:EBfnd:EBfyr:EBemp#) PEB02;
             Enddo;
          Endif;
       Endsl;

       Select;
       When inStInEx = 'I';
          If isInlist;
             Return *On;
          Else;
             Return *Off;
          Endif;
       When inStInEx = 'E' and STcnt > *Zero;
         If  STcnt = STexcl;
            Return *Off;
         Else;
            Return *On;
         Endif;
       Other;
          Return *On;
       Endsl;
       End-Proc;
      /End-Free
      *** Beg Add ***** 05/30/25 **********************************
       //===================================================================
       // GetNextDt - Returning @@date + @@days date in MMDDYYYY
       //===================================================================
     P GetNextDt       B
     D GetNextDt       PI             8  0
     D @@date                         7  0
     D @@days                         3  0 CONST

     d usaDate         s               d   datfmt(*usa)
     d resultDate      s              8  0

     C     @@date        add       19000000      resultDate
     C     *iso          test(d)                 resultDate             99
     C                   If        *in99 = *off
     C     *iso          move      resultDate    usaDate
           usaDate = usaDate + %days(@@days);
     C                   move      usaDate       resultDate
     C                   Else
     C                   move      *zeros        resultDate
     C                   Endif
     C
        return resultDate;
     P GetNextDt       E

       //===================================================================
       // GetDATE7 - Returning MMDDYYYY @@date in Date7
       //===================================================================
     P GetDATE7        B
     D GetDATE7        PI             7  0
     D @@date                         8  0 CONST

     d date8           s              8  0
     d strDate         s              8
     d resultDate      s              7  0

        strdate = %editc(@@date:'X');
        strdate =  %subst(strDate:5:4) + %subst(strDate:1:4);
        date8 = %dec(strDate:8:0);
     C     date8         sub       19000000      resultdate

        return resultDate;
     P GetDATE7        E
      *** End Add ***** 05/30/25 **********************************
