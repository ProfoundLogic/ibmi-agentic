     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W0010R02 - GROUP MASTER INFORMATION
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE GROUP MASTER FILE.       *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0010R - WORK WITH GROUPS                                  *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 12/16/96  PROGRAMMER: C:33  PROJECT: WC577          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/08/25 C:333 Project: 153WBD                                *
     H*                Note of Cancel Format-Contact Info Updt.       *
     H*                - Adding Alternate Company Phone#/Email.       *
     H*                                                               *
     H* 05/08/24 C:053 Support: 115213
     H*                Add ASO flag for EDI (PEO/ASO)
     H*                                                               *
     H* 06/23/22 C:062 Project: 129WXA                                *
     H*                Expand ASCGRP to client 129                    *
     H*                                                               *
     H* 11/04/21 C:062 Project 146WJV                                 *
     H*                Add Exclude from Comparative Rating            *
     H*          C:33 -Move default edit                              *
     H*                                                               *
     H* 03/16/21 C:300 Support: 95040 (WC2196 QA Issues)              *
     H*                Save Default Vendor # in File and Show on Screen
     H*                                                               *
     H* 08/02/19 C:291 Project: 146WJF                                *
     H*                Add Excess Insurance flag                      *
     H*                                                               *
     H* 05/24/19 C:33  Project: SY406                                 *
     H*                Add Portal Group Associations                  *
     H*                                                               *
     H* 04/24/19 C:62  Project: 152WAB                                *
     H*                Allow S1 Medical access to Group Asc field     *
     H*                                                               *
     H* 03/25/19 C:33  Project: SY390                                 *
     H*                Add Portal Defaul flag                         *
     H*                                                               *
     H* 04/02/18 C:33  Project: 150WCF                                *
     H*                Add Default Vendor #                           *
     H*                                                               *
     H* 11/11/17 C:062 Project: 148WAN                                *
     H*                Add Weekly Interim Due Days                    *
     H*                                                               *
     H* 10/17/17 C:136 Project: 150WAS                                *
     H*                Add Summarize A/R for Deductible Billing       *
     H*                flag for PEOs only                             *
     H*                                                               *
     H* 07/24/17 C:136 Project: 150WAN                                *
     H*                Add 'C'combined PEO type                       *
     H*                                                               *
     H* 05/01/17 C:79  Project: MC325                                 *
     H*                Use SPImodule to determine scClElig visable.   *
     H*                                                               *
     H* 03/02/17 C:65  Project: phw#45949 (AMWINS)                    *
     H*                Set commercial flag to N instead of blank      *
     H*                when the data is written to file.              *
     H*                                                               *
     H* 11/10/16 C:309 Project: 90MIB                                 *
     H*                Map new screen field to WMFNP/FNCLELIG         *
     H*                                                               *
     H* 06/22/16 C:307 Support: 42119                                 *
     H*                Initialized checkbox  to  handle validation    *
     H*                                                               *
     H* 06/13/16 C:136 Support: 41999                                 *
     H*                Don't edit group association unless visible    *
     H*                                                               *
     H* 02/24/13 C:282 Redmine 3969                                   *
     H*                Only Recompile the program with define 'WEB'   *
     H*                option.                                        *
     H*                                                               *
     H* 11/25/13 C:282 PROJECT 129WSC                                 *
     H*                Replace legal literal with control field       *
     H*                for cltid 129 only.                            *
     H*                                                               *
     H* 07/10/13 C:190  Project: 25WSS                                *
    *                 Add a radio button Premium Verbiage setting.  *
     H*                                                               *
     H* 03/15/13 C:190  Project: 147WAZ                               *
     H*                 Added "Group Association"  on SCR04.          *
     H*                                                               *
     H* 10/19/12 C:280  SUPPORT: 80859                                *
     H*                 Added code for FEOD LOSING RRN                *
     H*                                                               *
     H* 09/18/12 C:282  PROJECT: 93WKR                                *
     H*                 Added code for Policy Notice Attention        *
     H*                 name added on scr04.                          *
     H*                                                               *
     H* 04/26/12 C:282  PROJECT: WC2033                               *
     H*                 Added code for Mid-term endorsements as       *
     H*                 resubmissions field moved from screen 3 to    *
     H*                 screen 4.                                     *
     H*                                                               *
      * 09/13/11 C:136 Support: WA093 77975                           *
      *                Disable edit on PEO resubmission flag          *
      *                                                               *
     H* 12/17/10 C:255  PROJECT: 71WLD                                *
     H*                 Added code for flag 3rd Party Premium         *
     H*                 entry. This is client 071 specific.           *
     H*                                                               *
     H* 12/14/10 C:255 Project: 133WJW                                *
     H*                 1) Added code for new field SCDEPO & SCPDE.   *
     H*                    introduced at screen.                      *
     H*                 2) Added changes for field SCACLR moved from  *
     H*                    SCR02 to SCR04.                            *
     H*                                                               *
     H* 10/25/10 C:255 Project: WC1866                                *
     H*                 Remove changes for Project WC1866 done on     *
     H*                 04/30/10.                                     *
     H*
     H* 08/12/10 C:61  Project: 71WKF / SY190 PRISS 51
     H*
     H* 08/09/10 C:235 Project: 71WKF /Priss-099                      *
     H*                Auto create Entity/Location on empl (E/L/ )    *
     H*                was not able to take the E or the L  values    *
     H*
     H* 07/06/10 C:220 Project: WC1839 / Priss                        *
     H*                Add EDI Format Identifier edits,F10 and        *
     H*                database code.                                 *
     H*                                                               *
     H* 06/03/10 C:220 Project: 133WIU                                *
     H*                Convert into BND and add SCR04.                *
     H*                - Ignored SSY01 because SWC01 is being use for *
     H*                PEO processing.                                *
     H*
     H* 04/30/10 C:255 Project: WC1866                                *
     H*                 Group Specific Billing JOB Queues             *
     H*                 Call program W0010C02 to create Billing JOBQ  *
     H*                 For new added group.                          *
     H*
     H* 01/24/08 C:189 Project: WC1621                                *
     H*                PID GROUP COPY PHASE -1                        *
     H*
     H* 08/22/07 C:33  Project: 114WMF
     H*                Allow CLT114 access to SCPEOF.
     H*
     H* 08/11/07 C:180 Validate PEO fields in SCR03 only whe
     H*                PEO module is installed.
     H*
     H* 08/07/07 C:65  Project: SUPP# 59369 (WA135/50666)             *
     H*                REMOVE PRIOR SUPP CHANGE TO ALLOW COMMERCIAL   *
     H*                GROUPS FOR PPC.                                *
     H*                                                               *
     H* 07/11/07 C:65  PROJECT: WC1249 (MIDTERM PEO ENDORSEMENTS)     *
     H*                REENABLE THE RESUBMISSION FOR PEO FLAG.        *
     H*                                                               *
     H*  04/11/07 C:33  Project: 139WFS WA099/57533                   *
     H*                 Do not display SCPEO1 if PEO processing not   *
     H*                  installed. (DDS change only)                 *
     F*                                                               *
     F* 03/20/07 C:185 Project: 71WHU                                 *
     F*                Auto Create Location 1 for Quote Entry         *
     H*                                                               *
     H* 11/15/06 C:15  Project: 139WFS                                *
     H*                Chgd pgm to allow a value of 'G' for the PEO   *
     H*                flag, for client 139.                          *
     H*
     H* 08/29/06 C:180 PROJECT:119WMF
     H*                Added FND parm for PV1 search                  *
     H*
     H* 01/06/06 C:33  Project: WA135/50666                           *
     H*                Default the commerial flag to 'N' for CLT132   *
     H*                 and do not allow for it to be changed.        *
     H*                                                               *
     H* 12/16/05 C:33  Project: WC1141                                *
     H*                Add PEO flags:                                 *
     H*                  Charge expense constant to employer #1       *
     H*                  Calculate stock discount across entire PEO   *
     H*                                                               *
     H* 10/17/05 C:136 PROJECT: WC1249 (MIDTERM PEO ENDORSEMENTS)     *
     H*                COMMENTED OUT PER CG FOR NOV 05 BASE RELEASE   *
     H*                                                               *
     H* 07/25/05 C:053 PROJECT: WC1279 (CORRECTION)                   *
     H*                Change SYFL12 to WCFL30.                       *
     H*                                                               *
     H* 05/06/05 C:136 PROJECT: WC1249                                *
     H*                ADD FLAG THAT WILL CONTROL WHETHER PEO         *
     H*                RESUBMITS MIDTERM ENDORSEMENTS AS A POLICY     *
     H*                RESUBMISSION                                   *
     H*                                                               *
     H* 04/18/05 C:65  PROJECT WC1279                                 *
     H*                Add flag for PEO processing                    *
     H*                                                               *
     H* 03/15/05 C:15  Project: 114WJE                                *
     H*                Added screen field for the EDI flag.           *
     H*                                                               *
     H* 03/31/04 C:58  Project: WC1140
     H*                Add expense default reserve code and amount.
     H*
     H* 08/26/03 C:61  SUPPORT WA126 37184                            *
     H*                If CHANGE mode and Group Effective Date is     *
     H*                changed and prior Employer Policy FROM Date    *
     H*                exists within this group, then display         *
     H*                warning message.                               *
     H*                                                               *
     H* 02/01/02 C:61  SUPPORT WA099 31080                            *
     H*                Screen 3 fields: Test group (SCTEST), and      *
     H*                CI Center (SCCICF).  Not required entry.       *
     H*
     H*  09/24/01 C:132 PROJECT WC924  CICenter Flag
     H*                 WMFNP/FNCICF text to display when SWCPF/WCFL19
     H*                 is equal to 'Y', Indicator 70 = *ON
     H*
     H*  07/20/00 C:61  PROJECT WC915
     H*                 Add prompt for # of days before auto update
     H*                 quote to lost status to SCR03.  (wmfnp/fnlost)
     H*
     H*  06/09/00 C:58  PROJECT WC993  Add 'Test' Group flag, editing.
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*
     H*  08/10/99 C:15  Support: WC071/17389
     H*                 Changed pgm so the next available case # is
     H*                 only updated in ADD mode.
     H*
     H*  07/08/99 C:118 SUPPORT: MC048/18735
     H*                 Added code to check to see if WCMC record
     H*                 exists when in UPDATE mode.
     H*
     H*  01/11/99 C:31  Project: MC152
     H*                 V4: Bill Entry re-write
     H*                 1. Add call to FA027R
     H*
     H*  10/01/98 C:65  PROJECT: SUPPORT
     H*                 PADDED FNNAME FIELD WITH BLANKS BEFORE MOVE.
     H*
     H*  07/16/98 C:108 PROJECT: WC577
     H*                 DUE DATE FOR INTERIM SHOULD NOT BE REQUIRED.
     H*
     H*  07/06/98 C:33 PROJECT: 21WAI
     H*                DO NOT VALIDATE FIELDS THAT ARE NOT DISPLAYED.
     H*
     H*  04/13/98 C:91 PROJECT: WC577
     H*                DISPLAY ERROR IF F10 IS PRESSED ON NON
     H*                SEARCHABLE FIELD  (IND. 96)
     H*
     H*  03/23/98 C:85 PROJECT: WC773
     H*                DETERMINE IF CLIENT IS ADMIN-ONLY (IND. 94),
     H*                OR CLAIMS-ONLY (IND. 95).
     H*
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.
     H*    -------------------------------------------------------
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  19 - DEDUCTIBLES INSTALLED                                   *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  65 - SECURITY LEVEL VALID                                    *
     H* N65 - SECURITY LEVEL INVALID                                  *
     H*  66 - COMMERCIAL APPLICATION INSTALLED (WCFL07=Y)             *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  92 - F10 PRESSED                                             *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*  97 - CICf flag                                              *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  20 - (WCR0502) Group # must be > zero {ADD mode only}        *
     H*  21 - (WCR0503) Group Name cannot be blank                    *
     H*  22 - (WCR0503) Group Short Name cannot be blank              *
     H*  23 - (WCR0503) Mailing City cannot be blank                  *
     H*  24 - (WCR0504) Mailing State invalid                         *
     H*  25 - (WCR0503) Mailing Zip 1 cannot be zero                  *
     H*  26 - (WCR0503) Physical City cannot be blank                 *
     H*  27 - (WCR0504) Physical State invalid                        *
     H*  28 - (WCR0503) Physical Zip 1 cannot be zero                 *
     H*  29 - (WCR0500) Effective Date invalid                        *
     H*  30 - (WCR0500) Expiration Date invalid                       *
     H*  31 - (WCR0529) Date Range not in ascending order             *
     H*  33 - (WCR2376) Max Extended Policy must be => 12 {if entered}*
     H*  34 - (WC30001) Group # cannot be used more than once         *
     H*  35 - (WCR0501) Claims Only Processing not 'Y' or 'N'         *
     H*  36 - (WCR0501) Commercial Group not 'Y' or 'N'               *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR02:                                                       *
     H*  ERROR:                                                       *
     H*  20 - (WCR0502) Next Available Case # must be entered         *
     H*  21 - (WCR0502) Next Available Employer # must be entered     *
     H*  22 - (WCR0502) Next Available Quote # must be entered        *
     H*  23 - Edit Reserve Codes                                      *
     H* N23 - Edit Deductible Codes                                   *
     H* 24-33 (WCR0513) Invalid Table {Reserve/Deductilble}           *
     H*  34 - Edit Medical Codes                                      *
     H*  35 - Edit Compensation Codes                                 *
     H*  36 - Edit Expense Codes                                      *
     H*  37 - Edit Legal Codes                                        *
     H*  38 - Edit Medical Only Claims Default Code                   *
     H*  41 - Position Cursor {Transaction Code}                      *
     H*  42 - Position Cursor {Pmt Val 1 Code}                        *
     H* 50-59 Position Cursor {Reserve/Deductible Codes}              *
     H*  70 - (WCR0571) Invalid Code  {CICenter Group Flag)           *
     H*  71 - (WCR0513) Invalid Table {Default MEDICAL Reserve code}
     H*  72 - (WCR0544) Mutually Inclusive Default MEDICAL code/reserve
     H*  73 - (WCR0690) Inactive Status {MEDICAL default reserve code}
     H*  74 - (WCR1163/4/5/6/1176) Incorrect Type
     H*  75 - (WCR1320) Net Ded Recv not 'R' Type {Transaction Codes} *
     H*  76 - (WCR0513) Invalid Table {Pmt Val 1 Codes}               *
     H*  77 - (WCR0690) Inactive Status {Pmt Val 1 Codes}             *
     H*  78 - (WCR0547) Default MEDICAL Reserve Amount < zero
     H*  79 - (WCR0501) Invoke Policy Coverage Edits not 'Y' or 'N'   *
     H*  80 - (WCR0501) Auto assign employer number not 'Y' or 'N'    *
     H*  81 - (WCR0501) Auto assign division number not 'Y' or 'N'    *
     H*  82 - (WCR0502) Allow division zero flag not 'Y' or 'N'       *
     H*  83 - (WCR0547) Default EXPENSE Reserve Amount < zero
     H*  85 - (WCR0513) Invalid Table {Default EXPENSE Reserve code}
     H*  86 - (WCR0544) Mutually Inclusive Default EXPENSE code/reserve
     H*  87 - (WCR1163/4/5/6/1176) Incorrect Type
     H*  88 - (WCR0690) Inactive Status {EXPENSE default reserve code}
     H*  POSITION CURSOR:                                             *
     H* 20-29 (SCRS01-SCRS10) Reserve Type Codes                      *
     H*  41 - (SCMETR) Medical Transaction Code                       *
     H*  42 - (SCMEP1) Medical Payment Value 1 Code                   *
     H*  43 - (SCCOTR) Compensation Transaction Code                  *
     H*  44 - (SCCOP1) Compensation Payment Value 1 Code              *
     H*  45 - (SCLETR) Legal Transaction Code                         *
     H*  46 - (SCLEP1) Legal Payment Value 1 Code                     *
     H*  47 - (SCEXTR) Expense Transaction Code                       *
     H*  48 - (SCEXP1) Expense Payment Value 1 Code                   *
     H*  49 - (SCDFRC) Default Reserve Code                           *
     H*  50 - (SCDFRA) Default Reserve Amount                         *
     H*                                                               *
     H*  SCR03:                                                       *
     H*  ERROR:                                                       *
     H*  20 - (WCR0503) Alternate Name cannot be blank                *
     H*  21 - (WCR0503) Alternate Address 1 cannot be blank           *
     H*  22 - (WCR0503) Alternate City cannot be blank                *
     H*  23 - (WCR0504) Alternate State invalid                       *
     H*  24 - (WCR0503) Alternate Zip 1 cannot be zero                *
     H*  25 - (WCR3152) Test group flag not 'Y' or 'N'
     H*  32 - (WCR0501) Employer Self-Insured not 'Y' or 'N'          *
     H*  80 - (WCR0559) Check #'s not in check range                  *
     H*  81 - (WCR0502) Harbor exp, non-harbor class cannot be zero   *
     H*  POSITION CURSOR:                                             *
     H*****************************************************************
     FW0010F02  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     INFDS(FIDS01)
     FWMFNP     UF A E             DISK
     F                                     RENAME(PFN01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMFNL     IF   E           K DISK
      *
      * Employer Policy Year
     FWDELL3    IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMEXL     IF   E           K DISK
     FSTSTL3    IF   E           K DISK
     FWTGTL     IF   E           K DISK
     FSTGTP     IF   E             DISK    rename(pgt01:pgt0p)
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     FWMCXL     IF   E           K DISK
       //***  Begin Add  ***  03/25/19  *******************************
     DdecFnd           s                   like(FNfnd) inz
       //***  End   Add  ***  03/25/19  *******************************
     D F10             S             10    DIM(19) CTDATA PERRCD(1)
       //***  Begin Add  ***  03/25/19  *******************************
     DstrPortalName    s             40    varying inz
       //***  End   Add  ***  03/25/19  *******************************
     D*** Begin Add *** 11/04/21 **************************************
     DisPortal         s               n
     D*** End   Add *** 11/04/21 **************************************
     D xxRrn           s              9  0
     D xxCo#           s              3  0
     D charLGRH        s             10                                         LEGAL RESERVE HEAD

       //***  Begin Add  ***  05/24/19  *******************************
     DdsSMCIP        e ds                  extname(SMCIP)  inz qualified
     DdsWMFIP        e ds                  extname(WMFIP)  inz qualified
     DdsWMFIPsave      ds                  likeds(dsWMFIP) inz
       //***  End   Add  ***  05/24/19  *******************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D OPTNM           C                   CONST('Group Master Inform-
     D                                     ation')
     D #OFSCR          C                   CONST(4)
     D COMAPI          C                   CONST('WA001R')
     D PEOF1A          C                   CONST('PEO group - policy -
     D                                     type . . . . . . -
     D                                     . .(S/M/C/ ):')
     D PEOF1B          C                   CONST('PEO group - policy -
     D                                     type . . . . . . . -
     D                                     .(S/M/G/ ):')
     D PEOF1C          C                   CONST('PEO master policy -
     D                                     group . . . . . . . -
     D                                     . . .(Y/ ):')
     D PEOF2A          C                   CONST('(S=Single, M=Multip-
     D                                     le, C=Combined)')
     D PEOF2B          C                   CONST('(S=Single, M=Multip-
     D                                     le,')
     D PEOF3B          C                   CONST('G=GIC Master Policy)')
     D*
     D                 DS
     D  SRS                    1     20
     D                                     DIM(10)
     D  SCRS01                 1      2
     D  SCRS02                 3      4
     D  SCRS03                 5      6
     D  SCRS04                 7      8
     D  SCRS05                 9     10
     D  SCRS06                11     12
     D  SCRS07                13     14
     D  SCRS08                15     16
     D  SCRS09                17     18
     D  SCRS10                19     20
     D                 DS
     D  FRS                    1     20
     D                                     DIM(10)
     D  FNRS01                 1      2
     D  FNRS02                 3      4
     D  FNRS03                 5      6
     D  FNRS04                 7      8
     D  FNRS05                 9     10
     D  FNRS06                11     12
     D  FNRS07                13     14
     D  FNRS08                15     16
     D  FNRS09                17     18
     D  FNRS10                19     20
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D                 DS
     D  TCD                    1     18
     D                                     DIM(9)
     D  SCMETR                 1      2
     D  SCMEP1                 3      4
     D  SCCOTR                 5      6
     D  SCCOP1                 7      8
     D  SCEXTR                 9     10
     D  SCEXP1                11     12
     D  SCLETR                13     14
     D  SCLEP1                15     16
     D  SCDFRC                17     18
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D/COPY SPRTABLE
     d/copy sPrModule
      *** Beg Add ***** 05/08/25 *************************************
     d/copy sPrGenErr
      *** End Add ***** 05/08/25 *************************************
     C/COPY SCOPYMT05C
     I*
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
      *
      *===============================================================
      *  KLIST
      *===============================================================
      *
      *  Employer Detail
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    KKPFDT
      *
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C*
     C*  ADMIN/DARTS FILE
     C*
     C     EXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C*
     C*  TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  SYSTEM TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*   KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
      *** Beg Add ***  11/25/13  ******************************************************
     C*   Claim/DARTS Control Definition File
     C*
     C     keyWMcxl      KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C*** End Add ***  11/25/13  ******************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C     SYSPL         PLIST
     C                   PARM      *BLANKS       ##WCIF            1
     C                   PARM      *BLANKS       ##MCIF            1
     C                   PARM      *BLANKS       ##WCIN            1
     C                   PARM      *BLANKS       ##AR              1
     C                   PARM      *BLANKS       ##NTWK            1
     C                   PARM      *BLANKS       ##UR              1
     C                   PARM      *BLANKS       ##MED             1
     C                   PARM      *BLANKS       ##EDI             1
     C                   PARM      *BLANKS       ##WDCK            1
     C*
     C     MCGPL         PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      FNFND         ##FND
     C*
     C     FA027P        PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##GRP             3 0
     C                   PARM      FNFND         ##FND
     C                   PARM      0             #1RRN             9 0
      *
       //***  Begin Add  ***  03/25/19  *******************************
      /Free
       Exec Sql Set Option Commit = *None;
      /End-free
       //***  End   Add  ***  03/25/19  *******************************
      *
      *================================================================
      *  *LIKE DEFN
      *================================================================
     C     *LIKE         DEFINE    ELPFDT        KKPFDT
     C     *LIKE         DEFINE    RSKEY         XXKEY
     C     *LIKE         DEFINE    SCFND         XXFND
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    FNCO#         XXCO#
     C     *LIKE         DEFINE    FNFND         ##FND
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    DBRRN         SVRRN
     C*
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*  CHECK IF OTHER FUNCTION KEYS HAVE BEEN PRESSED
     C*
     C                   MOVE      *OFF          *IN92
     C*
     C                   EXSR      OTHFKS
     C*
     C*  GROUP NUMBER MUST BE GREATER THAN ZERO (IF IN ADD MODE)
     C*
     C   40
B02  CANN90SCFND         IFLE      *ZERO
     C                   SETON                                        9020
E02  C                   ENDIF                                                  SCFND<=*ZERO
     C*
     C*  CHECK TO BE SURE GROUP # HAS NOT BEEN USED PREVIOUSLY
     C*
     C   40
B02  CANN90FNKY          CHAIN     PFN01                              60
     C   40
     CANN90
     CANN60              SETON                                        9034
     C*
     C*  GROUP NAME CANNOT BE BLANK
     C*
B02  C  N90SCNAME        IFEQ      *BLANK
     C                   SETON                                        9021
E02  C                   ENDIF                                                  SCNAME=*BLANK
     C*
B02  C  N90SCSHNM        IFEQ      *BLANK
     C                   SETON                                        9022
E02  C                   ENDIF                                                  SCSHNM=*BLANK

       //***  Begin Del  ***  03/25/19  *******************************
       //*****Begin Add  ***  03/25/19  *******************************
      /Free
       //*Check*for portal default flag set for another group
       //erPortDft*=**off;
       //If*not**in90 and scPortDft = 'Y';
       //*Reset*decFnd;
       //*Exec*sql*
       //**select*FNfnd into :decFnd
       //**from*WMFNP
       //***where*FNco# = :##co#
       //****and*FNfnd <> :scFnd
       //****and*FNportDft = 'Y';
       //*If*decFnd <> 0;
       //*****in90*= *on;
       //****erPortDft = *on;
       //****msPortDft = 'Group ' + %editc(decFnd:'Z') +
       //*****'*is*already defined as the default group.';
       //*Endif;***
       //Endif;******
      /End-free
       //*****End** Add  ***  03/25/19  *******************************
       //***  End   Del  ***  03/25/19  *******************************
     C*
     C*  IF MAILING ADDRESS 1 IS NOT BLANK NOTHING ELSE CAN BE
     C*
B02  C  N90SCMAD1        IFNE      *BLANK
     C*
B03  C     SCMCTY        IFEQ      *BLANK
     C                   SETON                                        9023
E03  C                   ENDIF                                                  SCMCTY=*BLANK
     C*
     C                   MOVEL     SCMST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        9024
     C*
B03  C  N90SCMZP1        IFEQ      *ZERO
     C                   SETON                                        9025
E03  C                   ENDIF                                                  SCMZP1=*ZERO
     C*
E02  C                   ENDIF                                                  SCMAD1=*BLANK
     C*
     C*  IF PHYSICAL ADDRESS 1 IS NOT BLANK NOTHING ELSE CAN BE
     C*
B02  C  N90SCPAD1        IFNE      *BLANK
     C*
B03  C     SCPCTY        IFEQ      *BLANK
     C                   SETON                                        9026
E03  C                   ENDIF                                                  SCPCTY=*BLANK
     C*
     C                   MOVEL     SCPST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        9027
     C*
B03  C  N90SCPZP1        IFEQ      *ZERO
     C                   SETON                                        9028
E03  C                   ENDIF                                                  SCPZP1=*ZERO
     C*
E02  C                   END
     C*
     C*  CHECK THE EFFECTIVE DATE FOR A VALID DATE
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
B02  C  N90SRERR         IFEQ      '1'
     C                   SETON                                        9029
E02  C                   ENDIF                                                  SRERR=1
     C*
     C*  CHECK THE EXPIRATION DATE FOR A VALID DATE (IF ENTERED)
     C*
     C  N90SCEXDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT
     C*
B02  C  N90SRERR         IFEQ      '1'
     C                   SETON                                        9030
E02  C                   ENDIF                                                  SRERR=1
     C*
     C*  DATES MUST BE ASCENDING ORDER
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1            8 0
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT2            8 0
     C*
     C  N90XXDAT2        IFLT      XXDAT1
     C                   SETON                                        9031
     C                   ENDIF                                                  XXDAT2<XXDAT1
     C*
     C                   ENDIF                                                  SCEXDT<>*ZEROS
      *
      * If CHANGE mode and Group Effective Date has been changed and
      * prior Group Policy Start Date exists then, display information
      * message window.
      *
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *OFF
     C     *IN09         ANDEQ     *OFF
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKPFDT
      *
     C     KKPFDT        IFNE      FNEFDT
     C     ELKY          SETLL     PEL01
     C     ELKY1         READPE    PEL01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C                   EXFMT     WDW01
     C   KL              SETON                                        90  71
     C   KL              SETOFF                                       KL
     C                   ENDIF                                                  KKPFDT
      *
     C                   ENDIF                                                  ELKY
      *
     C                   ENDIF                                                  *IN90
      *
     C*
     C*  ONLY CHECK IF COMMERCIAL APPLICATION IS INSTALLED
     C*
     C     *IN66         IFEQ      *ON
     C*
     C*  IF ENTERED, MAX EXTENDED POLICY MUST BE > 12
     C*
     C  N90SCMXEX        IFNE      0
     C     SCMXEX        ANDLT     12
     C                   SETON                                        9033
     C                   ENDIF                                                  0<>SCMXEX<12
     C*
     C*  THE COMMERCIAL GROUP FLAG MUST BE Y OR N.
     C*
B02  C  N90SCCGFL        IFNE      'Y'
     C     SCCGFL        ANDNE     'N'
     C     SCCGFL        ANDNE     ' '
     C                   SETON                                        9036
E02  C                   ENDIF                                                  SCCGFL<>Y/N
     C*
     C                   ENDIF                                                  *IN66=*ON
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90FNDT          IFGT      SAVEDT
     C     FNDT          OREQ      SAVEDT
     C     FNTIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  FNDT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C*  CHECK IF OTHER FUNCTION KEYS HAVE BEEN PRESSED
     C*
     C                   MOVE      *OFF          *IN92
     C*
     C                   EXSR      OTHFKS
     C*
     C*  CHECK NEXT AVAILABLE #'S IF IN ADD MODE OR IF THE USER
     C*  SECURITY LEVEL WILL ALLOW UPDATE
     C*
     C*  NEXT AVAILABLE CASE NUMBER MUST BE GREATER THAN ZERO
     C*
B02  C  N90SCNACS        IFLE      *ZERO
     C     *IN40         ANDEQ     *ON
     C     *IN94         ANDEQ     *OFF
     C                   SETON                                        9020
E02  C                   ENDIF                                                  SCNACS<=*ZERO
     C*
     C     *IN40         IFEQ      *ON
     C     *IN65         OREQ      *ON
     C*
     C*  NEXT AVAILABLE EMPLOYER NUMBER MUST BE GREATER THAN ZERO
     C*
B02  C  N90SCNAEN        IFLE      *ZERO
     C                   SETON                                        9021
E02  C                   ENDIF                                                  SCNAEN<=*ZERO
     C*
     C*  NEXT AVAILABLE QUOTE NUMBER MUST BE GREATER THAN ZERO
     C*
B02  C  N90SCNAQN        IFLE      *ZERO
     C     *IN95         ANDEQ     *OFF
     C                   SETON                                        9022
E02  C                   ENDIF                                                  SCNAQN<=*ZERO
     C*
     C                   ENDIF                                                  *IN40/41=*ON
     C*
     C*  THE AUTO ASSIGN EMPLOYER FLAG MUST BE Y OR N.
     C*
B02  C  N90SCAUTE        IFNE      'Y'
     C     SCAUTE        ANDNE     'N'
     C     SCAUTE        ANDNE     ' '
     C                   SETON                                        9080
E02  C                   ENDIF                                                  SCAUTE<>Y/N
     C*
     C*  THE AUTO ASSIGN DIVISION FLAG MUST BE Y OR N.
     C*
B02  C  N90SCAUTD        IFNE      'Y'
     C     SCAUTD        ANDNE     'N'
     C     SCAUTD        ANDNE     ' '
     C                   SETON                                        9081
E02  C                   ENDIF                                                  SCAUTD<>Y/N
     C*
     C*  THE DIVISION ZERO FLAG MUST BE Y OR N.
     C*
B02  C  N90SCDIV0        IFNE      'Y'
     C     SCDIV0        ANDNE     'N'
     C     SCDIV0        ANDNE     ' '
     C                   SETON                                        9082
E02  C                   ENDIF                                                  SCDIV0<>Y/N
     C*
     C*  AUTO CREATE LOCATION FLAG MUST BE Y OR BLANK.
     C*
      /Free
       erDftVend# = *off;
       If not *in90 and scDftVend# < *zero;
          *in90 = *on;
          erDftVend# = *on;
          msDftVend# = 'WCR0547';
       Endif;
      /End-free
     C*
     C*  CHECK THE RESERVES TYPES
     C*
     C  N90              MOVE      *ON           *IN23
     C*
B02  C  N901             DO        10            I1                3 0
     C*
B03  C     SRS(I1)       IFNE      *BLANK
     C*
     C                   MOVEL(P)  SRS(I1)       XXKEY
     C     I1            ADD       23            X1                3 0
     C*
     C     TBKY          CHAIN     PRT01                              60
     C*
B04  C     *IN60         IFEQ      *ON
     C*
     C                   MOVE      *ON           *IN(X1)
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C*
X04  C                   ELSE
     C*
B05  C     RTSTAT        IFNE      'A'
     C                   MOVE      *ON           *IN(X1)
     C                   MOVE      *ON           *IN90
     C                   LEAVE
E05  C                   ENDIF                                                  RTSTAT<>A
     C*
E04  C                   ENDIF                                                  *IN60=*ON
     C*
     C  N901             DO        10            X                 2 0
     C*
     C     I1            IFNE      X
     C*
     C     SRS(I1)       COMP      SRS(X)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C     X             ADD       39            X2                2 0
     C     I1            ADD       39            X3                2 0
     C*
     C                   MOVE      *ON           *IN(X2)
     C                   MOVE      *ON           *IN(X3)
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C*
     C                   ENDIF                                                  *IN99 EQ *ON
     C*
     C                   ENDIF                                                  I1 NE X
     C*
     C                   ENDDO                                                  X(1-10)
     C*
E03  C                   ENDIF                                                  SRS,I1<>*BLANK
     C*
E02  C                   ENDDO                                                  I1->10
     C*
     C  N90              MOVE      *OFF          *IN23
     C*
     C     *IN19         IFEQ      *OFF
     C*
     C*
     C*  VALIDATE THE NET DEDUCTIBLE RECOVERY CODES
     C*
     C*    MEDICAL
     C*
     C  N90              MOVE      *ON           *IN34
     C*
B03  C  N90SCMETR        IFNE      *BLANK
     C     SCMEP1        ANDEQ     *BLANK
     C     SCMETR        OREQ      *BLANK
     C     SCMEP1        ANDNE     *BLANK
     C                   SETON                                        9072
E03  C                   ENDIF                                                  SCMETR<>*BLANK
     C*
B03  C  N90SCMETR        IFNE      *BLANK
     C*
     C                   MOVEL     *BLANK        XXKEY
     C                   MOVEL     SCMETR        XXKEY
     C*
     C                   EXSR      NTDEDT
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     PCRECD        XXKEY
     C*
     C     TBKY          CHAIN     PRT01                              60
     C*
     C  N90*IN60         IFEQ      *OFF
     C     RTTYPE        ANDNE     'M'
     C                   SETON                                        9074
E03  C                   ENDIF                                                  *IN60=*OFF
     C*
E03  C                   ENDIF                                                  SCMETR<>*BLANK
     C*
B03  C  N90SCMEP1        IFNE      *BLANK
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCMEP1        XXKEY
     C*
     C                   EXSR      NTDEDP
     C*
E03  C                   ENDIF                                                  SCMEP1<>*BLANK
     C*
     C  N90              MOVE      *OFF          *IN34
     C*
     C*    COMPENSATION
     C*
     C  N90              MOVE      *ON           *IN35
     C*
B03  C  N90SCCOTR        IFNE      *BLANK
     C     SCCOP1        ANDEQ     *BLANK
     C     SCCOTR        OREQ      *BLANK
     C     SCCOP1        ANDNE     *BLANK
     C                   SETON                                        9072
E03  C                   ENDIF                                                  SCCOTR<>*BLANK
     C*
B03  C  N90SCCOTR        IFNE      *BLANK
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCCOTR        XXKEY
     C*
     C                   EXSR      NTDEDT
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     PCRECD        XXKEY
     C*
     C     TBKY          CHAIN     PRT01                              60
     C*
     C  N90*IN60         IFEQ      *OFF
     C     RTTYPE        ANDNE     'C'
     C                   SETON                                        9074
E03  C                   ENDIF                                                  *IN60=*OFF
     C*
E03  C                   ENDIF
     C*
B03  C  N90SCCOP1        IFNE      *BLANK
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCCOP1        XXKEY
     C*
     C                   EXSR      NTDEDP
     C*
E03  C                   ENDIF                                                  SCCOP1<>*BLANK
     C*
     C  N90              MOVE      *OFF          *IN35
     C*
     C*    EXPENSE
     C*
     C  N90              MOVE      *ON           *IN36
     C*
B03  C  N90SCEXTR        IFNE      *BLANK
     C     SCEXP1        ANDEQ     *BLANK
     C     SCEXTR        OREQ      *BLANK
     C     SCEXP1        ANDNE     *BLANK
     C                   SETON                                        9072
E03  C                   ENDIF                                                  SCEXTR<>*BLANK
     C*
B03  C  N90SCEXTR        IFNE      *BLANK
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCEXTR        XXKEY
     C*
     C                   EXSR      NTDEDT
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     PCRECD        XXKEY
     C*
     C     TBKY          CHAIN     PRT01                              60
     C*
     C  N90*IN60         IFEQ      *OFF
     C     RTTYPE        ANDNE     'E'
     C                   SETON                                        9074
E03  C                   ENDIF                                                  *IN60=*OFF
     C*
E03  C                   ENDIF                                                  SCEXTR<>*BLANK
     C*
B03  C  N90SCEXP1        IFNE      *BLANK
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCEXP1        XXKEY
     C*
     C                   EXSR      NTDEDP
     C*
E03  C                   ENDIF                                                  SCEXP1<>*BLANK
     C*
     C  N90              MOVE      *OFF          *IN36
     C*
     C*    LEGAL
     C*
     C  N90              MOVE      *ON           *IN37
     C*
     C*
B03  C  N90SCLETR        IFNE      *BLANK
     C     SCLEP1        ANDEQ     *BLANK
     C     SCLETR        OREQ      *BLANK
     C     SCLEP1        ANDNE     *BLANK
     C                   SETON                                        9072
E03  C                   ENDIF                                                  SCLETR<>*BLANK
     C*
B03  C  N90SCLETR        IFNE      *BLANK
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCLETR        XXKEY
     C*
     C                   EXSR      NTDEDT
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     PCRECD        XXKEY
     C*
     C     TBKY          CHAIN     PRT01                              60
     C*
     C  N90*IN60         IFEQ      *OFF
     C     RTTYPE        ANDNE     'L'
     C                   SETON                                        9074
E03  C                   ENDIF                                                  *IN60=*OFF
     C*
E03  C                   ENDIF                                                  SCLETR<>*BLANK
     C*
B03  C  N90SCLEP1        IFNE      *BLANK
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCLEP1        XXKEY
     C*
     C                   EXSR      NTDEDP
     C*
E03  C                   ENDIF                                                  SCEXP1<>*BLANK
     C*
     C  N90              MOVE      *OFF          *IN37
     C*
     C                   ENDIF                                                  *IN19=*OFF
     C*
     C*  VALIDATE DEFAULT MEDICAL Reserve CODE and AMOUNT IF ENTERED
     C*
B02  C  N90SCDFRC        IFNE      *BLANKS
     C     SCDFRA        ORNE      *ZEROS
     C*
     C  N90              MOVE      *ON           *IN38
     C*
     C*    DEFAULT RESERVE CODE AND DEFAULT RESERVE AMOUNT FIELDS ARE
     C*    MUTUALLY INCLUSIVE WHEN DEFAULT RESERVE AMOUNT IS ENTENTED
     C*
     C     SCDFRC        IFEQ      *BLANKS
     C     SCDFRA        ANDNE     *ZEROS
     C                   SETON                                        9072
E03  C                   ENDIF                                                  SCDFRC<>*BLANKS
     C*
     C*    DEFAULT RESERVE CODE MUST BE ON FILE AND ACTIVE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCDFRC        XXKEY
     C*
     C     TBKY          CHAIN     PRT01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C     *IN94         ANDEQ     *OFF
     C*
     C                   SETON                                        9071
     C*
     C                   ELSE
     C*
     C     RTSTAT        IFEQ      'I'
     C                   SETON                                        9073
     C                   ENDIF                                                  RTSTAT=I
     C*
     C*    DEFAULT MEDICAL CODE MUST BE MEDICAL TYPE
     C*
B03  C  N90RTTYPE        IFNE      'M'
     C                   SETON                                        9074
E03  C                   ENDIF                                                  RTTYPE<>M
     C*
E03  C                   ENDIF                                                  *IN60=*ON
     C*
     C*    DEFAULT RESERVE AMOUNT CANNOT BE LESS THAN ZERO
     C*
B03  C  N90SCDFRA        IFLT      *ZEROS
     C                   SETON                                        9078
E03  C                   ENDIF                                                  SCDFRA<*ZEROS
     C*
     C  N90              MOVE      *OFF          *IN38
     C*
E02  C                   ENDIF                                                  SCDFRC<>*BLANKS
     C*
     C*  VALIDATE DEFAULT EXPENSE Reserve CODE and AMOUNT IF ENTERED
     C*
B02  C  N90SCDFXC        IFNE      *BLANKS
     C     SCDFXA        ORNE      *ZEROS
     C*
     C  N90              MOVE      *ON           *IN38
     C*
     C*    DEFAULT RESERVE CODE AND DEFAULT RESERVE AMOUNT FIELDS ARE
     C*    MUTUALLY INCLUSIVE WHEN DEFAULT RESERVE AMOUNT IS ENTENTED
     C*
     C     SCDFXC        IFEQ      *BLANKS
     C     SCDFXA        ANDNE     *ZEROS
     C                   SETON                                        9086
E03  C                   ENDIF                                                  SCDFRC<>*BLANKS
     C*
     C*    DEFAULT RESERVE CODE MUST BE ON FILE AND ACTIVE
     C*
     C                   MOVEL(P)  SCDFXC        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C     *IN94         ANDEQ     *OFF
     C*
     C                   SETON                                        9085
     C*
     C                   ELSE
     C*
     C     RTSTAT        IFEQ      'I'
     C                   SETON                                        9088
     C                   ENDIF                                                  RTSTAT=I
     C*
     C*    DEFAULT EXPENSE CODE MUST BE EXPENSE TYPE
     C*
B03  C  N90RTTYPE        IFNE      'E'
     C                   SETON                                        9087
E03  C                   ENDIF                                                  RTTYPE<>M
     C*
E03  C                   ENDIF                                                  *IN60=*ON
     C*
     C*    DEFAULT RESERVE AMOUNT CANNOT BE LESS THAN ZERO
     C*
B03  C  N90SCDFXA        IFLT      *ZEROS
     C                   SETON                                        9083
E03  C                   ENDIF                                                  SCDFXA<*ZEROS
     C*
     C  N90              MOVE      *OFF          *IN38
     C*
E02  C                   ENDIF                                                  SCDFXC<>*BLANKS
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90FNDT          IFGT      SAVEDT
     C     FNDT          OREQ      SAVEDT
     C     FNTIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  FNDT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*==============================================================
     C*
     C     ERRCK3        BEGSR
     C*
     C*  CHECK IF OTHER FUNCTION KEYS HAVE BEEN PRESSED
     C*
     C                   MOVE      *OFF          *IN92
     C                   Eval      erCLELIG = *Off
     C                   Eval      msCLELIG = *Blanks
      *** Beg Add ***** 05/08/25 ******************************
     C                   Eval      erAARA = *Off
     C                   Eval      msAARA = *Blanks
     C                   Eval      erAPHN = *Off
     C                   Eval      msAPHN = *Blanks
     C                   Eval      erAMail = *Off
     C                   Eval      msAMail = *Blanks
      *** End Add ***** 05/08/25 ******************************
     C*
     C                   EXSR      OTHFKS
     C*
     C*  TEST THE CHECK NUMBER RANGE
     C*
B02  C  N90SCCHK1        IFGT      SCCHK2
     C                   SETON                                        9080
E02  C                   ENDIF                                                  SCHK#1>SCHK#2
     C*
     C*  THE SELF-INSURED FLAG MUST BE Y OR N.
     C*
B02  C  N90SCSFIN        IFNE      'Y'
     C     SCSFIN        ANDNE     'N'
     C     SCSFIN        ANDNE     ' '
     C                   SETON                                        9032
E02  C                   ENDIF                                                  SCSFIN<>Y/N
     C*
     C*
     C  N90SCSIOR        IFNE      *BLANKS
     C     SCSFIN        ANDNE     'Y'
     C                   SETON                                        9078
     C                   ENDIF
     C*
     C  N90SCSIAU        IFNE      *BLANKS
     C     SCSFIN        ANDNE     'Y'
     C                   SETON                                        9079
     C                   ENDIF
     C*
     C     CLTID         IFEQ      '114'
     C     SCPEOF        ANDNE     'Y'
     C     SCPEOF        ANDNE     ' '
     C                   SETON                                        9064
     C                   ENDIF
     C*
     C     WCFL30        IFEQ      'Y'
     C     CLTID         IFNE      '114'
     C  N90SCPEOF        IFNE      'S'
     C     SCPEOF        ANDNE     'M'
     C     SCPEOF        ANDNE     ' '
     C     SCPEOF        ANDNE     'C'
     C*  If the client ID = 139, allow a value of 'G'.
     C     CLTID         IFNE      '139'
     C     SCPEOF        ORNE      'G'
     C                   SETON                                        9064
     C                   ENDIF                                                  SCPEOF NE 'G'
     C                   ENDIF                                                  SCPEOF IFNE
     C                   ENDIF                                                  <>CLT114
     C*
     C  N90SCPEOE        IFNE      'Y'
     C     SCPEOE        ANDNE     ' '
     C                   SETON                                        90  30
     C                   ENDIF
     C*
     C  N90SCPEOE        IFEQ      'Y'
     C     SCPEOF        ANDNE     'M'
     C     SCPEOF        ANDNE     'G'
     C     SCPEOF        ANDNE     'C'
     C                   SETON                                        902930
     C                   ENDIF
     C*
     C  N90SCPEOA        IFNE      'Y'
     C     SCPEOA        ANDNE     ' '
     C                   SETON                                        90  31
     C                   ENDIF
     C*
     C  N90SCPEOA        IFEQ      'Y'
     C     SCPEOF        ANDNE     'M'
     C     SCPEOF        ANDNE     'G'
     C     SCPEOF        ANDNE     'C'
     C                   SETON                                        902931
     C                   ENDIF
     C                   ENDIF
     C*
     C*  The Test Group flag must be Y, BLANK, or N.
     C*
B02  C  N90SCTEST        IFNE      'Y'
     C     SCTEST        ANDNE     ' '
     C                   SETON                                        9025
E02  C                   ENDIF                                                  TEST<>Y/N
     C*
     C*  The EDI flag must be Y, or BLANK.
     C*
B02  C  N90SCEDI         IFNE      'Y'
     C     SCEDI         ANDNE     ' '
     C                   SETON                                        9027
E02  C                   ENDIF                                                  EDI<>Y/
     C*
     C*  The EDI Format ID Must be in the table file if entered.
     C*  If not entered, check the fund file to see if it is required.
     C*
     C     SCEDIF        IFNE      *BLANKS
     C                   MOVEL     'EDIF'        KKTBID
     C                   MOVEL     SCEDIF        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL     GTDESC        SCEDFD
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       SCEDFD
     C                   SETON                                        9020
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     *IN40         IFEQ      *ON                                           CODE FOUND
     C*
     C*  If not entered, check the fund file to see if it is required.
     C*  CALL API TO CHECK THE FUND.
     C*
     C                   CALL      'WA075R'
     C                   PARM                    XXCO#
     C                   PARM      000           XFND              3 0
     C                   PARM      *BLANK        QQRTCD
     C*
     C     QQRTCD        IFEQ      'F'                                           CODE FOUND
     C                   SETON                                        9021
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C*
     C*  The CICenter flag must be Y, BLANK, or N. The Test Group
     C*  flag also must not be a 'Y'
     C*
B02  C     SCTEST        IFEQ      'Y'
     C                   MOVE      *BLANKS       SCCICF
E02  C                   ENDIF                                                  TEST<>Y/N
     C*
B02  C  N90SCCICF        IFNE      'Y'
     C     SCCICF        ANDNE     ' '
     C                   SETON                                        90  84
E02  C                   ENDIF                                                  TEST<>Y/N
     C*
     C*  THE MONTHLY INTERIM DUE DAY MUST BE FROM 1 TO 28
     C*
     C  N90
B02  CANN95SCMIDY        IFLT      0                                            IF NOT CLAIMS ONLY
     C     SCMIDY        ORGT      28
     C                   SETON                                        9026
E02  C                   ENDIF                                                  SCMIDY IFLT
     C*
     C*  The Weekly Interim Due Day must be from 1 to 28
     C*
     C  N90
B02  CANN95SCWIDY        IFLT      0                                            IF NOT CLAIMS ONLY
     C     SCWIDY        ORGT      28
     C                   SETON                                        9087
E02  C                   ENDIF                                                  SCWIDY IFLT
     C*
     C* NAME CANNOT BE BLANK IF ADDRESS ISN'T BLANK
     C*
     C  N90SCAAD1        IFNE      *BLANKS
     C     SCACTY        ORNE      *BLANKS
     C     SCAST         ORNE      *BLANKS
     C     SCAZP1        ORNE      *ZEROS
     C*
     C  N90SCANAM        IFEQ      *BLANKS
     C*
     C                   SETON                                        9071
     C*
     C                   ENDIF                                                  SCAAD1
     C*
     C                   ENDIF                                                  SCANAM
     C*
     C* ADDRESS CANNOT BE BLANK IF ALTERNATIVE NAME ISN'T BLANK
     C*
     C  N90SCANAM        IFNE      *BLANKS
     C*
     C                   SELECT
     C*
     C     SCAAD1        WHENEQ    *BLANKS
     C*
     C                   SETON                                        9072
     C*
     C     SCACTY        WHENEQ    *BLANKS
     C*
     C                   SETON                                        9073
     C*
     C     SCAST         WHENEQ    *BLANKS
     C*
     C                   SETON                                        9074
     C*
     C     SCAZP1        WHENEQ    *ZEROS
     C*
     C                   SETON                                        9075
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  SCANAM
     C*
     C* ADDRESS 1 AND ZIP2 MUST BE BLANK IF THE REST OF THE ADDRESS IS
     C* BLANK.
     C*
     C  N90SCAAD1        IFEQ      *BLANKS
     C     SCACTY        ANDEQ     *BLANKS
     C     SCAST         ANDEQ     *BLANKS
     C     SCAZP1        ANDEQ     *ZEROS
     C*
     C                   SELECT
     C*
     C     SCAAD2        WHENNE    *BLANKS
     C*
     C                   SETON                                        9076
     C*
     C     SCAZP2        WHENNE    *ZEROS
     C*
     C                   SETON                                        9077
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  SCAAD1=' '
     C*
     C     SCAST         IFNE      *BLANKS
     C*
     C                   MOVEL     SCAST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        9023
     C*
     C                   ENDIF                                                  SCAST<>*BLANKS

      *** Beg Add ***** 05/08/25 ********************************
       // Alternate Company Phone #
       If scAAra <> *Zeros and (scAPhn1 = *Zeros and
        scAPhn2 = *Zeros);
          erAPhn = *on;
          msAPhn = 'Phone Number required.';
          *In(90) = *on;
       Endif;

       If scAAra = *Zeros and (scAPhn1 <> *Zeros or
        scAPhn2 <> *Zeros);
          erAAra = *on;
          msAAra = 'Area Code required.';
          *IN(90) = *on;
       Endif;

       // Alternate Company Email Address
       If scAMail <> *Blanks;
          erAMail = Check_Email(scAMail:msAMail);
          If erAMail = *On;
             *IN(90) = *on;
          Endif;
       EndIf;
      *** End Add ***** 05/08/25 ********************************

     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90FNDT          IFGT      SAVEDT
     C     FNDT          OREQ      SAVEDT
     C     FNTIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  FNDT>SAVEDT
     C*
     C  N90scCLELIG      IFNE      'Y'
     C     scCLELIG      ANDNE     ' '
     C                   SETON                                        90
     C                   Eval      erCLELIG = *On
     C                   Eval      msCLELIG = 'WCR1241'
     C                   ENDIF
     C*
       //***  Begin Add  ***  11/04/21  *******************************
      /Free
       // Check for portal default flag set for another group
       erPortDft = *off;
       If not *in90 and scPortDft = 'Y';
          Reset decFnd;
          Exec sql
           select FNfnd into :decFnd
           from WMFNP
            where FNco# = :##co#
             and FNfnd <> :scFnd
             and FNportDft = 'Y';
          If decFnd <> 0;
             *in90 = *on;
             erPortDft = *on;
             msPortDft = 'Group ' + %editc(decFnd:'Z') +
              ' is already defined as the default group.';
          Endif;
       Endif;
      /End-free
       //***  End   Add  ***  11/04/21  *******************************
     C***  Begin Add  ***  05/24/19  ***************************************
     C*  Check As Of Date, if entered
     C*
     C                   If        isPortal = *on and isCmpDft = *off
     C*
     C                   If        scAsOfP <> *zero
     C*
     C                   Z-ADD     SCASOFP       SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCASOFP
     C*
     C                   If        srErr = *on and not *in90
     C                   Eval      *in90 = *on
     C                   Eval      erAsOfP = *On
     C                   Eval      msAsOfP = 'WCR0500'
E02  C                   Endif
     C*
     C                   Endif
     C*
     C                   Endif
     C***  End   Add  ***  05/24/19  ***************************************
     C*
     C* INDICATES END OF SCREEN 3 PROCESSING
     C*
     C                   MOVE      *ON           *IN61
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK3
     C*
     C*===============================================================
     C* ERRCK4 - SUBROUTINE FOR ERROR CHECKING SCR04
     C*==============================================================
     C*
     C     ERRCK4        BEGSR
     C*
     C*Validate "Schedule of operations division and E/L print order"
     C  N90SCDEPO        IFNE      'A'
     C     SCDEPO        ANDNE     'D'
     C                   SETON                                        9072
E02  C                   ENDIF                                                  SCACLR<>E/L/_
     C*Validate "Print division and E/L numbers on schedule of operations"
     C  N90SCPDE         IFNE      *BLANK
     C     SCPDE         ANDNE     'Y'
     C                   SETON                                        9073
     C                   ENDIF
     C*Validate "Auto-create Entity/Location on empl"
B02  C  N90SCACLR        IFNE      ' '
     C     SCACLR        ANDNE     'E'
     C     SCACLR        ANDNE     'L'
     C                   SETON                                        9074
E02  C                   ENDIF                                                  SCACLR<>E/L/_
     C*  CHECK IF OTHER FUNCTION KEYS HAVE BEEN PRESSED
     C*
     C  N90SCAER         IFNE      *BLANK
     C     SCAER         ANDNE     'Y'
     C                   SETON                                        9071
     C                   ENDIF
      /free
       If *IN90 = *Off;
          If (scPEOS <> *blanks and scPEOS <> 'Y');
             *In90 = *On;
             *In88 = *On;
          EndIf;
       EndIf;
      /end-free
     C* 3rd party flag.
     C  n90*in68         ifeq      *on
     C     sc3ppe        andne     ' '
     C     sc3ppe        andne     'Y'
     C                   SETON                                        9075
     C                   ENDIF
      * Group Association
      /free

       If not *in90 and scGrpAsc <> *blank and
        not ValidSysGenKey(xxCo#:'GRPA':scGrpAsc)
        and indVGrpAsc = *on;
         *IN63 = *On;
         *In90 = *On;
       Endif;

      /end-free
     C                   ENDSR                                                   ERRCK4
     C*===============================================================
     C* ERRCK5 - SUBROUTINE FOR ERROR CHECKING SCR05
     C*==============================================================
     C*
     C     ERRCK5        BEGSR
     C*
     C*  CHECK IF OTHER FUNCTION KEYS HAVE BEEN PRESSED
     C*
     C                   ENDSR                                                   ERRCK4
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     KJDO          BEGSR
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C     HFLD          IFEQ      'SCSIOR'
     C                   MOVEL     ##DATA        QQPRDS           40
     C                   MOVEL(P)  'SIOR'        KKTBID
     C                   MOVE(P)   *BLANK        KKKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL            1
     C                   PARM                    KKTBID
     C                   PARM                    KKKEY
     C                   PARM                    QQRTCD            1
     C                   MOVE      *ON           *IN81                          POS CUSR
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  KKKEY         SCSIOR
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCSIOD
     C                   MOVEA     '01'          *IN(81)                        POS CUSR
     C                   END
     C                   END
     C                   END
     C*
     C     HFLD          IFEQ      'SCSIAU'
     C                   MOVEL     ##DATA        QQPRDS           40
     C                   MOVEL(P)  'SIAU'        KKTBID
     C                   MOVE(P)   *BLANK        KKKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL            1
     C                   PARM                    KKTBID
     C                   PARM                    KKKEY
     C                   PARM                    QQRTCD            1
     C                   MOVE      *ON           *IN82                          POS CUSR
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  KKKEY         SCSIAU
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCSIAD
     C                   MOVEA     '01'          *IN(82)                        POS CUSR
     C                   END
     C                   END
     C                   END
     C*
     C     HFLD          IFEQ      'SCEDIF'
     C                   MOVEL     'EDIF'        KKTBID
     C                   MOVE      SCEDIF        KKKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL            1
     C                   PARM                    KKTBID
     C                   PARM                    KKKEY
     C                   PARM                    QQRTCD            1
     C                   SETON                                        9085
     C                   MOVE      *BLANKS       SCEDFD
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL     KKKEY         SCEDIF
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL     GTDESC        SCEDFD
     C                   SETON                                        9086
     C                   SETOFF                                       85
     C                   ELSE
     C                   SETON                                        9085
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   GOTO      EKJDO
     C                   END
     C                   MOVEL     *BLANKS       QQRTC             1
     C*
     C                   Z-ADD     1             I1                3 0
     C     HFLD          LOOKUP    F10(I1)                                60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C*
     C     I1            IFLE      10
     C     I1            OREQ      19
     C*
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C*
     C                   ELSE
     C*
     C     I1            DIV       2             X1
     C                   MVR                     X1
     C*
     C     X1            IFEQ      1
     C*
     C                   CALL      'W2141R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C*
     C                   ELSE
     C*
     C                   MOVE      SCFND         ##FND
      *
     C                   CALL      'W2146R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C                   PARM      *BLANKS       ##RECD            2
     C                   PARM                    ##FND
     C*
     C                   ENDIF                                                  X1=1
     C*
     C                   ENDIF                                                  29=I1<=20
     C*
     C     SCKEY         IFNE      *BLANKS
     C*
     C     I1            ADD       20            Z1                3 0
     C*
     C     I1            IFLE      10
     C                   MOVEL     SCKEY         SRS(I1)
     C                   ENDIF                                                  I1<=10
     C*
     C     I1            IFGT      10
     C     I1            SUB       10            X1
     C                   MOVEL     SCKEY         TCD(X1)
     C                   ENDIF                                                  I1>20
     C*
     C                   ELSE
     C*
     C     I1            ADD       19            Z1
     C*
     C                   ENDIF                                                  SCKEY<>*BLANKS
     C*
     C                   MOVE      *ON           *IN92
     C                   MOVE      *ON           *IN(Z1)
     C                   MOVE      *ON           *IN91
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN96
     C*
     C                   ENDIF                                                  *IN60=*ON
      /free

       *in90 = *On;
       Select;
       When hfld = 'SCGRPASC';
          indFGrpAsc = *on;
          xxRrn = SearchSysGenKey(xxCo#:'GRPA');
          Chain(n) xxRrn PGT0P;
          If %Found;
             indFGrpAsc = *off;
             scGrpAsc = GTkey;
          Endif;
       Endsl;

      /end-free
     C*
     C     EKJDO         ENDSR                                                  KJDO
     C*
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14
     C*===============================================================
     C*
     C*  F14=DARTS
     C*
     C     KNDO          BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0010R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C                   MOVEL     FNSHNM        SCSHNM
     C***  Begin Add  ***  03/25/19  **********************************
      /Free
       scPortDft = FNportDft;
      /End-free
     C***  End   Add  ***  03/25/19  **********************************
     C                   MOVEL     FNMAD1        SCMAD1
     C                   MOVEL     FNMAD2        SCMAD2
     C                   MOVEL     FNMCTY        SCMCTY
     C                   MOVEL     FNMST         SCMST
     C                   MOVEL     FNMZIP        SCMZP1
     C                   MOVE      FNMZIP        SCMZP2
     C                   MOVEL     FNPAD1        SCPAD1
     C                   MOVEL     FNPAD2        SCPAD2
     C                   MOVEL     FNPCTY        SCPCTY
     C                   MOVEL     FNPST         SCPST
     C                   MOVEL     FNPZIP        SCPZP1
     C                   MOVE      FNPZIP        SCPZP2
     C                   MOVEL     FNIADR        SCIADR
     C                   MOVEL     FNPH1A        SCPH1A
     C                   MOVEL     FNPH1         SCPH11
     C                   MOVE      FNPH1         SCPH12
     C                   MOVEL     FNAREA        SCAREA
     C                   MOVEL     FNPH2         SCPH21
     C                   MOVE      FNPH2         SCPH22
     C                   MOVEL     FNFAXA        SCFAXA
     C                   MOVEL     FNFAX         SCFAX1
     C                   MOVE      FNFAX         SCFAX2
     C                   Z-ADD     FNMIDY        SCMIDY
     C                   Z-ADD     FNWIDY        SCWIDY
     C*
     C                   Z-ADD     FNEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   Z-ADD     FNEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C*
     C                   MOVEL     FNSFIN        SCSFIN
     C                   MOVEL     FNSIOR        SCSIOR
     C                   MOVEL(P)  'SIOR'        KKTBID
     C                   MOVEL(P)  FNSIOR        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCSIOD
     C                   ELSE
     C                   MOVE      *BLANKS       SCSIOD
     C                   END
     C                   MOVEL     FNSIAU        SCSIAU
     C                   MOVEL(P)  'SIAU'        KKTBID
     C                   MOVEL(P)  FNSIAU        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCSIAD
     C                   ELSE
     C                   MOVE      *BLANKS       SCSIAD
     C                   END
     C                   MOVEL     FNCGFL        SCCGFL
     C                   MOVE      *OFF          *IN13
     C                   SELECT
     C     CLTID         WHENEQ    '135'
     C   40              MOVEL     'N'           SCCGFL
     C                   ENDSL
     C                   Z-ADD     FNMXEX        SCMXEX
     C                   Z-ADD     FNNACS        SCNACS
     C                   Z-ADD     FNNAEN        SCNAEN
     C                   Z-ADD     FNNAQN        SCNAQN
     C                   MOVEL     FNAUTE        SCAUTE
     C                   MOVEL     FNAUTD        SCAUTD
     C                   MOVEL     FNDIV0        SCDIV0
     C     FNACLR        IFEQ      'Y'
     C                   MOVEL     'L'           FNACLR
     C                   END
     C                   MOVEL     FNACLR        SCACLR
     C                   MOVEA     FRS           SRS
     C                   MOVEL     FNMETR        SCMETR
     C                   MOVEL     FNMEP1        SCMEP1
     C                   MOVEL     FNCOTR        SCCOTR
     C                   MOVEL     FNCOP1        SCCOP1
     C                   MOVEL     FNEXTR        SCEXTR
     C                   MOVEL     FNEXP1        SCEXP1
     C                   MOVEL     FNLETR        SCLETR
     C                   MOVEL     FNLEP1        SCLEP1
     C                   MOVEL     FNDFRC        SCDFRC
     C                   MOVEL     FNDFRA        SCDFRA
     C                   MOVEL     FNDFXC        SCDFXC
     C                   MOVEL     FNDFXA        SCDFXA
     C                   MOVEL     FNCHK1        SCCHK1
     C                   MOVEL     FNCHK2        SCCHK2
     C                   MOVEL     FNANAM        SCANAM
     C                   MOVEL     FNAAD1        SCAAD1
     C                   MOVEL     FNAAD2        SCAAD2
     C                   MOVEL     FNACTY        SCACTY
     C                   MOVEL     FNAST         SCAST
     C                   MOVEL     FNAZIP        SCAZP1
     C                   MOVE      FNAZIP        SCAZP2
     C*
     C                   MOVEL     FNTEST        SCTEST
     C                   MOVEL     FNCICF        SCCICF
     C                   Z-ADD     FNLOST        SCLOST
     C                   MOVEL     FNEDI         SCEDI
     C                   MOVE      FNPEOF        SCPEOF
     C                   MOVE      FNPEOE        SCPEOE
     C                   MOVE      FNPEOA        SCPEOA
     C*** Beg Add *** 05/08/24 **********************************
     C                   MOVE      FNASOF        SCASOF
     C*** End Add *** 05/08/24 **********************************
     C*** Beg Add *** 05/08/25 **********************************
       If FNaara <> *zeros and FNaphn <> *zeros;
          scAara = FNaara;
          scAPhn1 = %Dec(%Subst(%Char(FNAPHN):1:3):3:0);  // First 3 digits
          scAPhn2 = %Dec(%Subst(%Char(FNAPHN):4:4):4:0);  // Last 4 digits
       Endif;
       scAMail = FNamail;
     C*** End Add *** 05/08/25 **********************************
     C     CLTID         IFEQ      '139'
     C     FNPEOR        ANDEQ     'Y'
     C                   MOVE      'G'           SCPEOF
     C                   ENDIF
     C                   MOVE      FNPEOS        SCPEOS
      /free
       scATTN = FNattn;
       scGrpAsc = FNgrpAsc;
       If FNprmv = ' ';
          scPRMV = 'P';
       Else;
          scPRMV = FNprmv;
       Endif;
      /end-free
     C     *IN40         IFEQ      *ON
     C                   MOVE      *BLANK        SCAER
     C                   ELSE                                                    OTHER MODE
     C                   MOVE      FNAER         SCAER
     C                   ENDIF                                                   ADD MODE
     C                   MOVE      FNEDIF        SCEDIF
     C                   IF        FNDEPO = ' '
     C                   MOVE      'A'           SCDEPO
     C                   ELSE
     C                   MOVE      FNDEPO        SCDEPO
     C                   ENDIF
     C                   MOVE      FNPDE         SCPDE
     C                   move      fn3ppe        sc3ppe
      /Free
       charLGRH = *Blanks;
       If CltId = '129';
          Chain keyWMcxl pcx01;
          charLGRH = CXlgrh;
          If %Found(wmcxl);
             If charLGRH <> *Blanks;
                scLEGL = charLGRH + '  :';
             Else;
                scLEGL = 'Legal       :';
             Endif;
          Endif;
       Else;
          scLEGL = 'Legal       :';
       EndIf;
      /End-Free

      /Free
         scCLELIG = FNclelig;
         scSumDed = FNSumDed;
         //*** Beg Add ***** 08/02/19 *******************************
         scExIns = FNExIns;
         //*** End Add ***** 08/02/19 *******************************
         //*** Beg Add ***** 03/16/21 *******************************
         scdftVend# = FNdftVend#;
         //*** End Add ***** 03/16/21 *******************************

       //***  Begin Add  ***  05/24/19  **************************
        Reset dsSMCIP;
        Exec sql
         select * into :dsSMCIP
          from SMCIP
          where CICO# = :##Co#;
        Reset dsWMFIP;
        Exec sql
         select * into :dsWMFIP
          from WMFIP
          where FICO# = :FNCO# and FIFND = :FNFND;
        dsWMFIPsave = dsWMFIP;
        tpCmpDft = *blanks;
        tpCmpDftC = *blanks;
        vsCmpOvr = *off;
        isCmpDft = *off;
        If not *in09;
           tpCmpDftC = 'Clear both fields to reset to company information.';
        Endif;
        tpCmpDft = tpCmpDftC;
        If dsWMFIP.FIasOf = *zero and dsWMFIP.FIdays = *zero;
           tpCmpDft = 'Data shown is defaulted from company information.';
           If not *in09;
              tpCmpDft = %trim(tpCmpDft) + ' ' +
               'Click ''Override Company Defaults'' to set values for ' +
               'this group.';
              vsCmpOvr = *on;
           Endif;
           isCmpDft = *on;
           dsWMFIP.FIasOf = dsSMCIP.CIasOf;
           dsWMFIP.FIdays = dsSMCIP.CIdays;
        Endif;
        scQuote  = dsWMFIP.FIquote;
        scPolicy = dsWMFIP.FIpolicy;
        scClaim  = dsWMFIP.FIclaim;
        scSR     = dsWMFIP.FIsr;
        scFROI   = dsWMFIP.FIfroi;
        //*** Begin Add *** 11/04/21 ***************************************
        scExcCmpRt = dsWMFIP.FIexcCmpRt;
        //*** End   Add *** 11/04/21 ***************************************
        //scAsOfP  = DtMDYY(dsWMFIP.FIasOf);
        srDtI = dsWMFIP.FIasOf;
      /End-free
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         scAsOfP
      /Free
        scDaysP  = dsWMFIP.FIdays;
        srDtI = dsSMCIP.CIasOf;
      /End-free
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         udAsOfP
      /Free
        udDaysP  = dsSMCIP.CIdays;
        If isCmpDft = *on;
           Clear dsWMFIP.FIasOf;
           Clear dsWMFIP.FIdays;
        Endif;
       //***  End   Add  ***  05/24/19  **************************
      /End-Free
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     XXCO#         FNCO#
     C                   Z-ADD     SCFND         FNFND
     C                   MOVEL(P)  SCNAME        FNNAME
     C                   MOVEL     SCSHNM        FNSHNM
     C***  Begin Add  ***  03/25/19  **********************************
      /Free
       If scQuote <> 'Y';
          scPortDft = ' ';
       EndIf;
       FNportDft = scPortDft;
      /End-free
     C***  End   Add  ***  03/25/19  **********************************
     C                   MOVEL     SCMAD1        FNMAD1
     C                   MOVEL     SCMAD2        FNMAD2
     C                   MOVEL     SCMCTY        FNMCTY
     C                   MOVEL     SCMST         FNMST
     C                   MOVEL     SCMZP1        FNMZIP
     C                   MOVE      SCMZP2        FNMZIP
     C                   MOVEL     SCPAD1        FNPAD1
     C                   MOVEL     SCPAD2        FNPAD2
     C                   MOVEL     SCPCTY        FNPCTY
     C                   MOVEL     SCPST         FNPST
     C                   MOVEL     SCPZP1        FNPZIP
     C                   MOVE      SCPZP2        FNPZIP
     C                   MOVEL     SCIADR        FNIADR
     C                   MOVEL     SCPH1A        FNPH1A
     C                   MOVEL     SCPH11        FNPH1
     C                   MOVE      SCPH12        FNPH1
     C                   MOVEL     SCAREA        FNAREA
     C                   MOVEL     SCPH21        FNPH2
     C                   MOVE      SCPH22        FNPH2
     C                   MOVEL     SCFAXA        FNFAXA
     C                   MOVEL     SCFAX1        FNFAX
     C                   MOVE      SCFAX2        FNFAX
     C                   Z-ADD     SCMIDY        FNMIDY
     C                   Z-ADD     SCWIDY        FNWIDY
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FNEFDT
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FNEXDT
     C*
     C                   MOVEL     SCSFIN        FNSFIN
     C                   MOVEL     SCSIOR        FNSIOR                         ADD 07/13/00
     C                   MOVEL     SCSIAU        FNSIAU                         ADD 07/13/00
     C                   Z-ADD     SCMXEX        FNMXEX
     C                   MOVEL     SCCGFL        FNCGFL
     C                   If        FNCGFL = ' '
     C                   Move      'N'           FNCGFL
     C                   Endif
     C     *IN40         IFEQ      *ON
     C                   Z-ADD     SCNACS        FNNACS
     C                   ENDIF
     C                   Z-ADD     SCNAEN        FNNAEN
     C                   Z-ADD     SCNAQN        FNNAQN
     C                   MOVEL     SCAUTE        FNAUTE
     C                   MOVEL     SCAUTD        FNAUTD
     C                   MOVEL     SCDIV0        FNDIV0
     C                   MOVEL     SCACLR        FNACLR
     C                   MOVEA     SRS           FRS
     C                   MOVEL     SCMETR        FNMETR
     C                   MOVEL     SCMEP1        FNMEP1
     C                   MOVEL     SCCOTR        FNCOTR
     C                   MOVEL     SCCOP1        FNCOP1
     C                   MOVEL     SCEXTR        FNEXTR
     C                   MOVEL     SCEXP1        FNEXP1
     C                   MOVEL     SCLETR        FNLETR
     C                   MOVEL     SCLEP1        FNLEP1
     C                   MOVEL     SCDFRC        FNDFRC
     C                   MOVEL     SCDFRA        FNDFRA
     C                   MOVEL     SCDFXC        FNDFXC
     C                   MOVEL     SCDFXA        FNDFXA
     C                   MOVEL     SCCHK1        FNCHK1
     C                   MOVEL     SCCHK2        FNCHK2
     C                   Z-ADD     1.00          FNLHF
     C                   MOVEL     SCANAM        FNANAM
     C                   MOVEL     SCAAD1        FNAAD1
     C                   MOVEL     SCAAD2        FNAAD2
     C                   MOVEL     SCACTY        FNACTY
     C                   MOVEL     SCAST         FNAST
     C                   MOVEL     SCAZP1        FNAZIP
     C                   MOVE      SCAZP2        FNAZIP
     C*
     C                   MOVE      SCTEST        FNTEST
     C                   MOVE      SCCICF        FNCICF
     C                   Z-ADD     SCLOST        FNLOST
     C                   MOVE      SCEDI         FNEDI
     C                   MOVE      SCPEOF        FNPEOF
     C                   MOVE      SCPEOE        FNPEOE
     C                   MOVE      SCPEOA        FNPEOA
     C*** Beg Add *** 05/08/24 **********************************
     C                   MOVE      SCASOF        FNASOF
     C*** No PEO, no ASO
     C     SCPEOF        IFEQ      ' '
     C                   MOVE      ' '           FNASOF
     C                   ENDIF
     C*** End Add *** 05/08/24 **********************************
     C*** Beg Add *** 05/08/25 **********************************
     C                   MOVE      SCAMAIL       FNAMAIL
     C                   MOVE      SCAARA        FNAARA
       FNAPHN = %Dec(%Trim(%EditC(scAPHN1:'X') + %EditC(scAPHN2:'X')):7:0);
     C*** End Add *** 05/08/25 **********************************
     C     CLTID         IFEQ      '139'
     C     SCPEOF        ANDEQ     'G'
     C                   MOVE      'M'           FNPEOF
     C                   MOVE      'Y'           FNPEOR
     C                   ELSE
     C                   MOVE      ' '           FNPEOR
     C                   ENDIF
     C                   MOVE      SCPEOS        FNPEOS
      /free
       FNattn = scATTN;
       FNgrpAsc = scGrpAsc;
       FNprmv = scPRMV;
      /end-free
     C                   MOVE      SCAER         FNAER
     C                   MOVE      SCEDIF        FNEDIF
     C                   MOVE      SCDEPO        FNDEPO
     C                   MOVE      SCPDE         FNPDE
     C                   move      sc3ppe        fn3ppe
      /Free
         FNclelig = scCLELIG;
         FNSumDed = scSumDed;
         //*** Beg Add ***** 08/02/19 *******************************
         FNExIns = ScExIns;
         //*** End Add ***** 08/02/19 *******************************
         //*** Beg Add ***** 03/16/21 *******************************
         FNdftVend# = scdftVend#;
         //*** End Add ***** 03/16/21 *******************************

       //***  Begin Add  ***  05/24/19  **************************
        dsWMFIP.FIquote  = scQuote;
        dsWMFIP.FIpolicy = scPolicy;
        dsWMFIP.FIclaim  = scClaim;
        dsWMFIP.FIsr     = scSR;
        dsWMFIP.FIfroi   = scFROI;
        //*** Begin Add *** 11/04/21 ***************************************
        If scQuote <> 'Y';
           scExcCmpRt = ' ';
        EndIf;
        dsWMFIP.FIexcCmpRt = scExcCmpRt;
        //*** End   Add *** 11/04/21 ***************************************
        If isCmpDft = *off;
           //dsWMFIP.FIasOf = DtCYMD(scAsOfP);
           srDtI = scAsOfP;
      /End-free
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         dsWMFIP.FIasOf
      /Free
           dsWMFIP.FIdays = scDaysP;
        Endif;
       //***  End   Add  ***  05/24/19  **************************
      /End-Free
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*===============================================================
     C* NTDEDP - SUBROUTINE FOR EDITING NET DEDUCTIBLE REC PMT VAL 1
     C*===============================================================
     C*
     C     NTDEDP        BEGSR
     C*
     C     TBKY          CHAIN     PB201                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9076
     C                   ELSE
     C     B2STAT        IFEQ      'I'
     C                   SETON                                        9077
     C                   ENDIF                                                  B2STST=I
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDSR                                                  NTDEDP
     C*
     C*===============================================================
     C* NTDEDT - SUBROUTINE FOR EDITING NET DEDUCTIBLE REC TRANS CODES
     C*===============================================================
     C*
     C     NTDEDT        BEGSR
     C*
     C     TBKY          CHAIN     PPC01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9071
     C                   ELSE
     C     PCSTAT        IFEQ      'I'
     C                   SETON                                        9073
     C                   ENDIF                                                  PCSTAT=I
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDSR                                                  NTDEDT
     C*
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C*
     C     *INKC         CASEQ     *ON           FINISH
     C                   ENDCS
     C*
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKN         CASEQ     *ON           KNDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C*
     C     *IN40         IFEQ      *ON
     C     *INKC         ANDEQ     *OFF
     C     *INKL         ANDEQ     *OFF
     C*
     C                   CALL      'W0010R07'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@MODE        ##MODE            1
     C                   PARM      DBRRN         ##RRN             9 0
     C
     C*
     C                   ENDIF                                                  *IN40=*ON
     C*
     C                   ENDSR                                                  PGMEND
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C*
     C* INITIALIZE END OF SCREEN 3 PROCESSING FLAG
     C*
     C                   MOVE      *OFF          *IN61
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL07        COMP      'Y'                                    66
     C*
     C     WCFL19        COMP      'Y'                                    70
     C*
     C*
     C*  DETERMINE IF CLIENT IS ADMIN-ONLY OR CLAIMS-ONLY
     C*
     C*  Access System Control file 'WC' record
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     WCFL08        IFNE      *BLANK
     C*
     C     WCFL08        IFEQ      'A'
     C                   MOVE      *ON           *IN94                          ADMIN ONLY
     C                   ENDIF                                                  WCFL08=A
     C*
     C     WCFL08        IFEQ      'C'
     C                   MOVE      *ON           *IN95                          CLAIMS ONLY
     C                   ENDIF                                                  WCFL08=C
     C*
     C                   ENDIF                                                  WCFL08<>*BLANK
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*
     C*  GET COMPANY NUMBER
     C*
     C  N40@@RRN         CHAIN     WMFNP                              60
     C  N40              MOVE      FNCO#         XXCO#
     C   40              MOVE      ##CO#         XXCO#
     C*
     C*  GET THE ADMIN/DARTS RECORD
     C*
     C                   CLEAR                   EXUPDL
     C                   CLEAR                   EXUPDT
     C     EXKY          CHAIN     PEX01                              60
     C*
     C*  GET SECURITY LEVEL OF THE USER
     C*
     C     WCSL04        COMP      ##LVL                              67  67
     C     ##LVL         COMP      EXUPDL                             65  65
     C                   MOVE      *OFF          *IN41
     C*
     C*  RETRIEVE SYSTEM VALUES
     C*
     C                   CALL      'SA001R'      SYSPL
     C*
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL30        COMP      'Y'                                    12
     C*  Load different options to the screen for PEO flag, for different
     C*   clients.
     C                   SELECT
     C     CLTID         WHENEQ    '139'
     C                   MOVEL     PEOF1B        SCPEO1
     C                   MOVEL     PEOF2B        SCPEO2
     C                   MOVEL     PEOF3B        SCPEO3
     C     CLTID         WHENEQ    '114'
     C                   MOVEL     PEOF1C        SCPEO1
     C                   MOVEL     *BLANKS       SCPEO2
     C                   MOVEL     *BLANKS       SCPEO3
     C                   OTHER
     C                   MOVEL     PEOF1A        SCPEO1
     C                   MOVEL     PEOF2A        SCPEO2
     C                   MOVE      *BLANKS       SCPEO3
     C                   ENDSL
     C*
     C     CLTID         COMP      '114'                                  14
     C*
     C     @@MODE        IFEQ      'P'
     C                   EXSR      COPY
     C                   ENDIF
     C*For client 071
     C                   if        cltid = '071'
     C                   eval      *in68= *on
     C                   else
     C                   eval      *in68= *off
     C                   endif
     C                   if        cltid = '148'
     C                   eval      isAmWins = *On
     C                   else
     C                   eval      isAmWins = *Off
     C                   endif
      /free
       //***  Begin Add  ***  03/25/19  *******************************
       isPortal = IsModEnabled('Portal Module');
       strPortalName = GetModuleProperty('Portal Module':'Name');
       //*** Begin Del *** 11/04/21 ***********************************
       //*** scPortDftH = 'Default Group for ' + strPortalName;
       //*** End   Del *** 11/04/21 ***********************************
       //***  End   Add  ***  03/25/19  *******************************
       //***  Begin Add  ***  05/24/19  *******************************
       scPortFlgH = 'Include Group for ' + strPortalName;
       //***  End   Add  ***  05/24/19  *******************************

       If cltid = '114' or cltid = '147' Or cltid = '152';
          indVGrpAsc = *on;
       Else;
          indVGrpAsc = *off;
       Endif;
       //*** Beg Add ***** 08/02/19 *******************************
       If cltid = '146';
          isExIns = *on;
       Else;
          isExIns = *off;
       Endif;
       //*** End Add ***** 08/02/19 *******************************

      /end-free
       //* Set the visibility for Claims Elig Flag
      /free
       //* Default to not visable
       isClElig = '0';

       if isModEnabled('Claims Export');
          isClElig = '1';
       else;
          isClElig = '0';
       endif;

      /end-free
      /Free
       Exsr GetDTm;
       scVndrSlCh = 'VPco# = ' + %EditC(xxCo#:'X') + ' and ' +
        '(VPeXdt = 0 Or VPeXdt >= ' + %Editc(saveDt:'X') + ')';
      /End-free
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
     C***  Begin Add  ***  05/24/19  *********************************
     C*===============================================================
     C* UpdateWMFIP - Update/Write records to WMFIP
     C*===============================================================
      /Free
       Begsr UpdateWMFIP;
        If dsWMFIP = dsWMFIPsave;
           Leavesr;
        Endif;
        dsWMFIP.FIuser = FNuser;
        dsWMFIP.FIpgm  = FNpgm;
        dsWMFIP.FIdt   = FNdt;
        dsWMFIP.FItime = FNtime;
        If dsWMFIP.FIfnd <> *zero;
           Exec sql
            update WMFIP
             set row = :dsWMFIP
             where FIco# = :dsWMFIP.FIco#
              and FIfnd = :dsWMFIP.FIfnd;
        Else;
           dsWMFIP.FIco#  = FNco#;
           dsWMFIP.FIfnd  = FNfnd;
           dsWMFIP.FIiUsr = FNuser;
           dsWMFIP.FIiPgm = FNpgm;
           dsWMFIP.FIiDt  = FNdt;
           dsWMFIP.FIiTim = FNtime;
           Exec sql
            insert into WMFIP
             values(:dsWMFIP);
        Endif;
       Endsr;
      /End-free
     C***  End   Add  ***  05/24/19  *********************************
     C*
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B02  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        FNUSER
     C                   MOVEL     Q1PGM         FNPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FNDT
     C                   MOVEL     DSTIME        FNTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E02  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C     ##MCIF        IFEQ      'Y'
     C*
     C*
     C* SCREEN 3 HAS BEEN DISPLAYED
     C*
     C     *IN61         ANDEQ     *ON
     C*
     C                   MOVE      FNFND         @@WCF#            3
     C                   MOVE      FNCO#         @@CO#             3
     C*
     C* CHECK IF MANAGED CARE RECORD EXISTS
     C*
     C                   CALL      'FA700R'
     C                   PARM                    @@RTCD            1
     C                   PARM      @@CO#         @@CO#
     C                   PARM                    @@WCF#
     C                   PARM      '0'           @@SUB#            5
     C                   PARM      '0'           @@FND             3
     C*
     C     @@RTCD        IFEQ      'F'
     C     @@SUB#        ORLE      *ZERO
     C     @@FND         ORLE      *ZERO
     C                   CALL      'F0062R'      MCGPL
     C                   ELSE
     C*
     C                   CALL      'FA027R'      FA027P
     C                   ENDIF                                                  @@RTCD=F
     C*
     C                   ENDIF                                                  ##MCIF=Y
     C*
     C                   MOVE      *OFF          *IN61
     C*
     C***  Begin Add  ***  05/24/19  *********************************
     C                   Exsr      UpdateWMFIP
     C***  End   Add  ***  05/24/19  *********************************
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        FNIUSR
     C                   MOVEL     Q1USER        FNUSER
     C                   MOVEL     Q1PGM         FNIPGM
     C                   MOVEL     Q1PGM         FNPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FNIDT
     C                   MOVEL     SRDTO         FNDT
     C                   MOVEL     DSTIME        FNITIM
     C                   MOVEL     DSTIME        FNTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   EVAL      SVRRN = DBRRN
     C*
     C                   FEOD      WMFNP
     C*
     C                   EVAL      DBRRN = SVRRN
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C     ##MCIF        IFEQ      'Y'
     C*
     C                   CALL      'F0062R'      MCGPL
     C*
     C                   ENDIF
     C*
     C***  Begin Add  ***  05/24/19  *********************************
     C                   Exsr      UpdateWMFIP
     C***  End   Add  ***  05/24/19  *********************************
     C*
     C                   ENDSR                                                  WRITER
     C     COPY          BEGSR
     C                   Z-ADD     FNFND         @@FGRP            3 0
     C                   CALL      'W0010R03'
     C                   PARM                    @@FGRP
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C*
     C                   SETON                                        LR
     C                   RETURN
     C*
     C                   ENDSR                                                  COPY
**  F10
SCRS01
SCRS02
SCRS03
SCRS04
SCRS05
SCRS06
SCRS07
SCRS08
SCRS09
SCRS10
SCMETR
SCMEP1
SCCOTR
SCCOP1
SCEXTR
SCEXP1
SCLETR
SCLEP1
SCDFRC
