      * (c) 2022 Sapiens. All Rights Reserved.                             *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0049R - ADMIN/DARTS CONTROL MAINTENANCE
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW THE USER TO SETUP      *
     H*                 VARIOUS SECURITY LEVELS AND OTHER ADMIN AND   *
     H*                 DARTS CRITERIA.                               *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0010R   - WORK WITH GROUPS                                *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 04/11/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 05/24/23 C:079 Support: 150/107702
     H*                Add Delete from Quote Sec lvl and type
     H*
     H* 03/02/22 C:136 Project: WC2818
     H*                Add flag to Include CSV of WFACP with Agent
     H*                Commission Statement.
     H*
     H*  04/19/21 C:53   Project WC2278
     H*                  Changes for contact enhancement
     H*                                                               *
     H* 05/07/20  C:65   Project: Phw#88975 (Amwins)                  *
     H*                  Change display conditions for Update fields. *
     H*                  Checkbox displaying on in66 only change in   *
     H*                  screen file.                                 *
     H*                                                               *
     H* 02/10/20  C:298  Project: WC2704                              *
     H*                  Add checkbox for generating CX/RE for State  *
     H*                  Reporting Div Only                           *
     H*                                                               *
     H* 10/25/19  C:33   Project: SY406                               *
     H*                  Add DARTS codes for:                         *
     H*                  -ACORD application                           *
     H*                  -Document upload                             *
     H*                                                               *
     H* 04/02/18  C:33   Project: 150WCF                              *
     H*                  Add field Require Employer Link Code         *
     H*                                                               *
     H* 08/18/17  C:291  PROJECT: C150WAO                             *
     H*                  Add New field Print Employer Link Code on    *
     H*                  Schedule of Classifications.                 *
     H*                                                               *
     H* 10/22/14  C:65   Project : WC2201 (Redmine #4713)             *
     H*                  Only display EDI contact error message when  *
     H*                  the EDI fields are visable.                  *
     H*                                                               *
     H* 07/24/14  C:65   Project: WC2277                              *
     H*                  Only display the notification fields if the  *
     H*                  system flag SYFL11(pos 6) is set to 'Y'.     *
     H*                                                               *
     H* 06/20/14  C:65   Project: WC2277                              *
     H*                  Modify the new/renewal letter fields to be   *
     H*                  dropdowns set to the department letter file. *
     H*                                                               *
     H* 04/10/14  C:65   Project: WC2277                              *
     H*                  Add checkbox to screen 4 for policy set      *
     H*                  email notification along with dcid/fold      *
     H*                  fields for policy page cover letters.        *
     H*                                                               *
     H* 03/19/14  C:295  Project : 89WAX                              *
     H*                  Added Customer ID flag.                      *
     H*                                                               *
     H* 01/21/14  C:190  Project : WC2201                             *
     H*                  Added Radio Buttons next to the 'Employer    *
     H*                  contact headings' fields to indicate which   *
     H*                  Contact should be used for the EDI           *
     H*                  submission.                                  *
     H*                                                               *
     H* 08/07/13  C:282  Project : 147WAK Priss# 25                   *
     H*                  Replace the Indicator *In09 from fields      *
     H*                  scQTIT and scQTBT by isEditMod.
     H*                                                               *
     H* 04/01/13  C:282  Project : 147WAK                             *
     H*                  Modify Program to add following new fields.  *
     H*                  i)  Transfer Quote to Live Immediate         *
     H*                 ii)  Tranfer Quote to Live Batch              *
     H*                iii) Quote Tranfer to Live Tranfer Type        *
     H*                                                               *
     H*                                                               *
     H* 02/06/12  C:255  Project : 114WBD                             *
     H*                  Modify program to add DART remark code for   *
     H*                  "Agent comm statement".                      *
     H*                                                               *
     H* 10/01/10  C:33   Project: SY190 Priss# 76 (93WIF)             *
     H*                  SSN display control for CLT093.              *
     H*                                                               *
     F* 09/28/10  C:255  SUPPORT#73851                                *
     F*                  COMPILED ONLY FOR CHANGES IN DISPLAY FILE    *
     F*                                                               *
     H* 08/27/10  C:61   PROJECT: SY190 PRISS 70                      *
     H*                  Always display "Additional Fields in Employer*
     H*                  Master".                                     *
     H*                                                               *
     H* 07/28/10  C:255  PROJECT: 132WOJ                              *
     H*                  Remove flex heading fields related changes   *
     H*                  as flex information fields removed from      *
     H*                  screen.                                      *
     H*                                                               *
     H* 05/03/10  C:220  PROJECT: WC1881                              *
     H*                  Add Ctrl flag to Final Audit Request Form    *
     H*                                                               *
     H* 01/25/10  C:232  PROJECT: WC1851                              *
     H*                  ADDED FEE REMARK CODE.                       *
     H*                                                               *
     H* 01/22/10 C:246 PROJECT: 139WNQ                                *
     H*                ADD TERTIARY AGENT IN THE DARTS.               *
     H*                                                               *
     H* 12/30/09  C:232  PROJECT: WC1838                              *
     H*           C:33   ADDED LARGE DEDUCTIBLE PROCESSING FIELDS.    *
     H*                                                               *
     H* 11/16/09  C:198  PROJECT: 132WNC                              *
     H*                  EDITS FOR ADDITIONAL FIELDS IN EMPLOYER      *
     H*                  DETAIL.                                      *
     H*                                                               *
     F* 10/05/09  C:062  PROJECT: 139WMJ                              *
     F*                  ADD FLEX HEADING AND TABLE ID.               *
     H*                                                               *
     F* 07/28/09  C:189  PROJECT: 93WIF                               *
     F*                  CHANGE SSN DISPLAY ON AUDIT REQUEST          *
     H*                                                               *
     H* 07/16/09  C:65  PROJECT WC1701 PRISS #13                      *
     H*                 REMOVE DEFAULT OF PAYROLL PRINT CONTROL.      *
     F*                                                               *
     F* 07/08/09 C:189 Project: 114WUD                                *
     F*                CHANGE RATES DEFAULT                           *
     H*                                                               *
     H* 06/06/09  C:189  PROJECT: 93WID                               *
     H*                  ADDITIONAL FLEXIBLE FIELDS IN WDELP          *
     H*                                                               *
     H* 05/15/09 C:189 PROJECT : 139WLJ                               *
     H*                WORK WITH MULTIPLE DARTS DIARY RECIPIENTS      *
     H*                                                               *
     H* 04/28/09  C:191 PROJECT WC1794                                *
     H*                 REQUIREMENT AND REMOVE THE ENTRIES FOR NCCI   *
     H*                 RISK ID                                       *
     H*                                                               *
     H* 04/17/09  C:191 PROJECT WC1701 PRISS #13                      *
     H*                 DEFAULT PAYROLL PRINT CONTROL FLAG TO 'D'     *
     H*                                                               *
     H* 01/08/09  C:189 PROJECT 71WJU                                 *
     H*                ADD A NEW "DEDUCTIBLE INVOICE" REMARK FIELD    *
     H*                                                               *
     H* 11/27/08  C:189 PROJECT WC1454                                *
     H*                DESIGNATED WCFL41 FOR CONTROLLING PROJ.        *
     H*                                                               *
     H* 11/05/08  C:190 Project WC1700
     H*                 Add a new Alternate employer remark field.    *
     H*                                                               *
     H* 10/01/08  C:197 PROJECT WC1701                                *
     H*                ADD POLICY PAGE PAYROLL PRINT CONTROL:         *
     H*                D'=DETAILED, 'S'=SUMMARY BY STATE              *
     H*                                                               *
     H* 07/01/08  C:191 Project 133WFE
     H*                 Add a new default DARTS remark field          *
     H*                                                               *
     H* 11/23/07  C:189 Project 132WJP
     H*                Produce an Auto Diary on Employer name change  *
     H*                                                               *
     H* 07/27/07  C:190 Project WC1454
     H*                   Add a new screen prompt for the new update
     H*                   across all divisions flag.
     H*
     H* 12/12/06 C:33  Project: WC1299                                *
     H*                Add flag for Re-assigning agent default rate   *
     H*                                                               *
     H* 12/06/06 C:132 PROJECT: 129WOX                                *
     H*               -Add Fourth outq for Cx Certificate Holders     *
     H*                Output Queue and edit for valid Outq.          *
     H*               -Rearrange fields on Screen two                 *
     H*                                                               *
     H* 11/04/06 C:136 PROJECT: 71WHE)                                *
     H*               -CLIENT SPECIFIC - ADD 4TH CONTACT.             *
     H*                                                               *
     H*  10/11/06  C:33  Seq#: WA099/41801                            *
     H*                  Use SC305R instead of SC580R.                *
     H*                                                               *
     H* 10/03/06 C:53  SUPPORT: 54685  REAPPLY 129WAN                 *
     H*                Add DARTS code entry SCR03 for                 *
     H*                ACH One Time Letter.                           *
     H*                                                               *
     H* 09/20/06 C:136 PROJECT: 71WHE                                 *
     H*               -ADD ALT HEADING FIELDS                         *
     H*                                                               *
     H* 02/07/06 C:168 PROJECT WC1418                                 *
     H*                Added a remark field for the default to DARTS  *
     H*                                                               *
     H* 09/02/05 C:61  PROJECT 129WLT  WC1359                         *
     H*                Add prompts for CX/REIN output queues.         *
     H*                                                               *
     H* 10/07/04 C:132 PROJECT 132WBE                                 *
     H*                ADD new fields and edits for Admin/DARTS       *
     H*                Control Information.                           *
     H*                EXSRRB - State Risk ID# required to bind       *
     H*                EXSRRQ - State Risk ID# required to quote      *
     H*                EXNRRB - NCCI Risk ID# required to bind        *
     H*                EXNRRQ - NCCI Risk ID# required to quote       *
     H*                                                               *
     H* 09/24/04 C:53  PROJECT 132WBD                                 *
     H*                ADD AGENT HEADER                               *
     H*                                                               *
     H* 11/13/03 C:65  PROJECT WC1167                                 *
     H*                REMOVE MOST OF THE BILLING RELATED FIELDS TO   *
     H*                THE NEW MAINTENANCE PROGRAM W0048R.            *
     H*                                                               *
     H* 11/17/03 C:61  SUPPORT WA099 35703                            *
     H*                Do not change to ADD mode if called in         *
     H*                display mode.                                  *
     H*                                                               *
     H* 10/21/03 C:65  PROJECT WC1017                                 *
     H*                ADD FIELD TO SCREEN 3 FOR REVISED BILL ON      *
     H*                RENEWAL POLICY REMARK CODE.                    *
     H*                                                               *
     H* 04/11/03 C:136 PROJECT: WC1116                                *
     H*                Add field for ACH Processing.                  *
     H*                                                               *
     H* 11/07/02 C:65  PROJECT WC1055                                 *
     H*                ADD FIELD TO SCREEN 1 FOR BILLING EXP CNST     *
     H*                TO THE HIGHEST STATE.                          *
     H*                                                               *
     H* 07/01/02 C:61  PROJECT 91WGS WC1086                          *
     H*                Add prompt and search for 'Audit Tracking      *
     H*                deletion remark'.                              *
     H*                                                               *
     H* 02/07/02 C:65  PROJECT WC1067                                 *
     H*                ADD FIELD TO SCREEN 1 FOR BILLING EXP CNST     *
     H*                SEPARATELY FROM MINIMUM PREMIUM.               *
     H*                                                               *
     H* 01/14/02 C:61  PROJECT WC1068                                 *
     H*                Add fields screen 1.  Security level/type      *
     H*                required for override of producer licensed for *
     H*                rating state of employer edit.                 *
     H*                                                               *
     H*  11/19/01 C:142 PROJECT: WC696                                *
     H*                -ADD ENTRY FOR DEFAULT NO DEPOSITS EXEDFL      *
     H*                                                               *
     H*  12/01/00 C:33  PROJECT: WC1019                               *
     H*                -ADD ENTRY OF SECURITY LEVEL/TYPE FOR 'AGENT   *
     H*                 COMMISSION PLAN CHANGE OVERRIDE'              *
     H*                -ADD ENTRY FOR 'AGT COMM PLAN CHANGE REMARK'   *
     H*                                                               *
     H* 10/06/00 C:90  PROJECT: WC577                                 *
     H*                Add field for Monthly Interim remark - EXMICD  *
     H*                                                               *
     H* 07/20/00 C:61  PROJECT WC994                                  *
     H*                Add new fields for security type and level:    *
     H*                  EXPDCL - Pol dt chg override level           *
     H*                  EXPDCT - Pol dt chg override type            *
     H*                                                               *
     H* 04/25/00 C:61  PROJECT WC989                                  *
     H*              - Move 'Default office #' to SCR01.              *
     H*              - Add 'FastQuote contact x-ref' code fields to   *
     H*                SCR02.                                         *
     H*                                                               *
     H*  02/09/00 C:33  PROJECT: WC919                                *
     H*                 ADD DEFAULT REGION #.                         *
     H*                                                               *
     H*  08/11/99 C:33  PROJECT: WC899                                *
     H*                 ADD ENTRY OF SECURITY LEVEL/TYPE FOR:         *
     H*                  -EMPLOYER F6=ADD, LIVE ENVIRONMENT           *
     H*                  -EMPLOYER F6=ADD, QUOTE ENVIRONMENT          *
     H*                                                               *
     H*  07/12/99 C:87  PROJECT: WC900                                *
     H*                 Add optional entry of 'Delete future A/R      *
     H*                 during cancellation (Y/ )' and processing.    *
     H*                                                               *
     H*  04/22/99 C:90  PROJECT: WC886                                *
     H*                 ADD OPTIONAL ENTRY OF 'AUDIT ADJUSTMENT'      *
     H*                 BILLING A/R CODE. IF ENTERED, CODE MUST BE    *
     H*                 VALID AND TYPE 'D'. ADD OPTIONAL ENTRY OF     *
     H*                 'QUOTE BILL' DARTS CODE.                      *
     H*                                                               *
     H*  03/04/99 C:87  PROJECT: WC885                                *
     H*                 Add to screen 'Suppress debit message on      *
     H*                 audit bill' and 'Suppress credit message on   *
     H*                 audit bill' and processing.                   *
     H*                                                               *
     H*  07/16/98 C:108 PROJECT: WC577                                *
     H*                 RELAX MANDATORY EDIT ON OFFICE NUMBER         *
     H*                                                               *
     H*  04/10/98 C:91 PROJECT: WC577                                 *
     H*                DISPLAY ERROR IF F10 SEARCH KEY PRESSED ON     *
     H*                NON SEARCHABLE FIELD (IND. 82)                 *
     H*                                                               *
     H*  01/08/98 C:65 PROJECT: WC702                                 *
     H*                ADDED DARTS REMARK FOR POLICY PAGES.           *
     H*                                                               *
     H*  12/09/97 C:80 PROJECT: WC778                                 *
     H*                ADDED EMPLOYER FEDERAL ID FLAG (EXFIFG).       *
     H*                                                               *
     H*  12/03/97 C:79 PROJECT: WC577                                 *
     H*                DELETED CODE FOR PRINTING INVOICES WITH EST.   *
     H*                BILL.                                          *
     H*                                                               *
     H*  11/11/97 C:85 PROJECT: WC577                                 *
     H*                ADDED AUDIT INTERIM DAYS FIELD (EXAUID).       *
     H*                                                               *
     H*  10/22/97 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 REPORT STANDARDS.                    *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  20 - (WCR0502) Employer Deletion security level Live <= zero *
     H*  21 - (WCR0502) Override CX Date security level <= zero       *
     H*  22 - (WCR0502) Update next Emp # security level <= zero      *
     H*  23 - (WCR0502) Employer Deletion security level Quote <= zero*
     H*  25 - (WCR0513) Invalid Table (Default Office #)              *
     H*  31 - (WCR0502) Allow submission of mass bills level <= zero  *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR02:                                                       *
     H*  ERROR:                                                       *
     H*  23 - (WCR0501) Require SIC codes not 'Y' or 'N'              *
     H*  24 - (WCR0501) Print Invoices w/Est Bills not 'Y' or 'N'     *
     H*  26 - (WCR0503) Contact Heading 1 cannot be blank             *
     H*  27 - (WCR0503) Contact Heading 2 cannot be blank             *
     H*  28 - (WCR0503) Contact Heading 3 cannot be blank             *
     H*  31 - (WCR0513) TABLE VALUE NOT VALID.                        *
     H*  32 - (WCR0610) EST A/R CODE INVALID.                         *
     H*  33 - (WCR0513) TABLE VALUE NOT VALID.                        *
     H*  34 - (WCR0611) S/R A/R CODE INVALID.                         *
     H*  35 - (WCR0513) TABLE VALUE NOT VALID.                        *
     H*  36 - (WCR0611) DA  A/R CODE INVALID.                         *
     H*  37 - (WCR0827) ORIGINAL BILL REMARK CODE NOT VALID           *
     H*  38 - (SCM0614) Remark Code Inactive -ORG                     *
     H*  39 - (WCR0827) REVISED BILL REMARK CODE NOT VALID            *
     H*  41 - (SCM0614) Remark Code Inactive -REV                     *
     H*  42 - (WCR0827) AUDIT BILL REMARK CODE NOT VALID              *
     H*  43 - (SCM0614) Remark Code Inactive -AUD                     *
     H*  44 - (WCR0827) ORIGINAL BILL REMARK CODE NOT VALID           *
     H*  45 - (SCM0614) Remark Code Inactive -ORG                     *
     H*  46 - (WCR0827) ORIGINAL BILL REMARK CODE NOT VALID           *
     H*  47 - (SCM0614) Remark Code Inactive -ORG                     *
     H*  51 - (WCR0503) FastQuote x-ref code #1 can not be blank      *
     H*  52 - (WCR0503) FastQuote x-ref code #2 can not be blank      *
     H*  53 - (WCR0503) FastQuote x-ref code #3 can not be blank      *
     H*  54 - (WCR1022) FastQuote x-ref codes mutually exclusive      *
     H*  55 - (WCR1022) FastQuote x-ref codes mutually exclusive      *
     H*  56 - (WCR1022) FastQuote x-ref codes mutually exclusive      *
     H*  57 - (WCR0501) Employer FEIN required (Y/N)                  *
     H*  81 - secondary agent                                         *
     H*  84 - Audit adjustment                                        *
     H*  85 - Quote bill                                              *
     H*  86 - Quote bill                                              *
     H*  88 - Audit adjustment                                        *
     H*  POSITION CURSOR:                                             *
     H*  22 - FastQuote contact x-ref code #3                         *
     H*  25 - SCCNT2                                                  *
     H*  29 - SCCNT3                                                  *
     H*  30 - SCSICR                                                  *
     H*  70 - Default Office # (SCDOFF)                               *
     H*  71 - Contact Heading 1 (SCCNT1)                              *
     H*  73 - EST A/R CODE                                            *
     H*  74 - S/R A/R CODE                                            *
     H*  75 - DA A/R CODE                                             *
     H*  76 - ORG BILL REMARK                                         *
     H*  77 - REV BILL REMARK                                         *
     H*  78 - AUD BILL REMARK                                         *
     H*  79 - FastQuote contact x-ref code #1                         *
     H*  80 - FastQuote contact x-ref code #2                         *
     H*  82 - F10 KEY PRESSED ON NON SEARCHABLE FIELD                 *
     H*  83 - REVISED FINAL AUDIT FLAG                                *
     H*  87 - Audit adjustment                                        *
     H*  89 - Quote bill                                              *
     H*                                                               *
     H*  SCR03:                                                       *
     H*  ERROR:                                                       *
     H*  29 - (WCR0827) Remark Code not valid                         *
     H*  30 - (SCM0614) Remark Code Inactive                          *
     H*  31 - (WCR0827) Remark Code not valid                         *
     H*  32 - (SCM0614) Remark Code Inactive                          *
     H*  POSITION CURSOR:                                             *
     H*  72 - Office Remark Code (SCOFRM)                             *
     H*  73 - LATE NOTICE CODE(SCLNRM)                                *
     H*  74 - PREMIUM NOTICE CODE(SCPNRM)                             *
     H*****************************************************************
     FW0049F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMEXP     UF A E             DISK
     F                                     RENAME(PEX01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMEXL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     FWMFNP     IF   E             DISK
     FWMOFL     IF   E           K DISK
     FWMREL     IF   E           K DISK
     FWMREP     IF   E             DISK
     F                                     RENAME(PRE01:PRE0P)
      *
      * Generic Table
     FWTGTL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     FSTCTL1    IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)
     FWMDRL     IF   E           K DISK
     FWTGIL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D                 DS
     D  FQ                     1      8
     D                                     DIM(4) ASCEND                        FQ XREF
     D  SCFQC1                 1      2
     D  SCFQC2                 3      4
     D  SCFQC3                 5      6
     D  SCFQC4                 7      8
     D*
     D                 DS
     D  scNewCover             1     97
     D  xxNewDept              1      5
     D  xxNewDoc               6     17
     D  xxNewFold             18     97
     D                 DS
     D  scRnwCover             1     97
     D  xxRnwDept              1      5
     D  xxRnwDoc               6     17
     D  xxRnwFold             18     97
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D OPTNM           C                   CONST('Admin/DARTS Control-
     D                                      Information')
     D #OFSCR          C                   CONST(4)
     D COMAPI          C                   CONST('WA001R')
     D CNT1            C                   CONST('Contact')
     D CNT2            C                   CONST('Attention')
     D CNT3            C                   CONST('Contact 3')
     D CNT4            C                   CONST('Contact 4')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*
     C     OFFPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##OFF             5
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     UPRMPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      3             ##VIEW            1 0
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY            10
      *
      *  Generic Table
      *
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
      *
     C*
     C*  OFFICE MASTER FILE
     C*
     C     OFKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCDOFF
     C*
     C*  KEY LIST FOR THE ADMIN/DARTS CONTROL FILE
     C*
     C     EXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C*
     C     REKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCDRGN
     C     GIKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID            4
     C*
      *
      * *LIKE DEFINES
      *
     C     *LIKE         DEFINE    GTTBID        XXTBID
      *
      *
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    ##RRN             9 0
     C*
     C/COPY SCOPYMT05C
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
     C*  EMPLOYER DELETION SECURITY LEVEL LIVE MUST BE > ZERO
     C*
     C  N90SCDLTL        IFLE      *ZERO
     C                   SETON                                        20  90
     C                   ENDIF                                                  SCDLTL<=*ZERO
      *** Beg Add ***** 05/24/23 ***********************
     C*
     C*  EMPLOYER DELETION SECURITY LEVEL QUOTE MUST BE > ZERO
     C*
     C  N90SCDLTLQT      IFLE      *ZERO
     C                   SETON                                        23  90
     C                   ENDIF                                                  SCDLTL<=*ZERO
      *** End Add ***** 05/24/23 ***********************
     C*
     C*  OVERRIDE CX DATE SECURITY LEVEL MUST BE > ZERO
     C*
     C  N90SCOVRL        IFLE      *ZERO
     C                   SETON                                        21  90
     C                   ENDIF                                                  SCOVRL<=*ZERO
     C*
     C*  UPDATE NEXT EMP # SECURITY LEVEL MUST BE > ZERO
     C*
     C  N90SCUPDL        IFLE      *ZERO
     C                   SETON                                        22  90
     C                   ENDIF                                                  SCUPDL<=*ZERO
     C*
     C*  ALLOW SUBMISSION OF MASS BILLS LEVEL MUST BE > ZERO
     C*
     C  N90SCSUBL        IFLE      *ZERO
     C                   SETON                                        31  90
     C                   ENDIF                                                  SCSUBL<=*ZERO
      *
      *  Allow claims outside pol eff dt range (security level) must > 0
      *
     C  N90SCPDCL        IFLE      *ZERO
     C                   SETON                                        69  90
     C                   ENDIF                                                  SCPDCL<=*ZERO
      *
     C*
     C*  EMPLOYER F6=ADD LIVE ENVIRONMENT MUST BE >= ZERO
     C*
     C  N90SCADLL        IFLT      *ZERO
     C                   SETON                                        32  90
     C                   ENDIF                                                  SCADLL<*ZERO
     C*
     C*  EMPLOYER F6=ADD QUOTE ENVIRONMENT MUST BE >= ZERO
     C*
     C  N90SCADQL        IFLT      *ZERO
     C                   SETON                                        33  90
     C                   ENDIF                                                  SCADQL<*ZERO
     C*
     C*  AGENT COMMISSION PLAN CHANGE OVERRIDE MUST BE >= ZERO
     C*
     C  N90SCPLNL        IFLT      *ZERO
     C                   SETON                                        34  90
     C                   ENDIF                                                  SCPLNL<*ZERO
      *
      *  Agent License by State Edit override security level must > 0
      *
     C  N90SCPRSL        IFLT      *ZERO
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCPRSL
     C*
      *
      *  ALLOW BILLING FOR LARGE DEDUCTIBLE ACCOUNT LEVEL MUST > 0
      *
     C  N90SCLDAL        IFLT      *ZERO
     C                   SETON                                        51  90
     C                   ENDIF                                                  SCLDAL
      /Free

       // Transfer Quote to Live Immediate
       If *IN90 = *Off;
          If scQTIL < 0;
             errQTIL = *On;
             *IN90   = *On;
          EndIf;
       EndIf;

       // Transfer Quote to Live Batch
       If *IN90 = *Off;
          If scQTBL < 0;
             errQTBL = *On;
             *IN90   = *On;
          EndIf;
       EndIf;

      /End-Free
     C*  Suppress debit/credit message must be 'Y' or blank
     C*
     C  N90SCDLAR        IFNE      'Y'
     C     SCDLAR        ANDNE     *BLANK
     C                   SETON                                        88  90
     C                   ENDIF                                                  SCCMSG='Y'
     C*
     C  N90SCDRGN        IFNE      *ZEROS
     C*
     C     REKY          CHAIN     PRE01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     RESTAT        OREQ      'I'
     C                   SETON                                        71  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  SCDRGN<>*ZEROS
      *
      *  DEFAULT OFFICE # MUST BE VALID, IF ENTERED
      *
     C  N90SCDOFF        IFNE      *BLANKS
     C     OFKY          CHAIN     POF01                              60
     C   60              SETON                                        25  90
     C                   ENDIF                                                  SCDOFF IFNE *BLANKS
      *
     C     SCRAFL        IFNE      'Y'
     C     SCRAFL        ANDNE     *BLANK
     C                   SETON                                        89  90
     C                   ENDIF
     C*
     C  N90SCAAFL        IFNE      'Y'
     C     SCAAFL        ANDNE     *BLANK
     C                   SETON                                        87  90
     C                   ENDIF
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
     C*  PRINT INV W/EST BILLS MUST BE 'Y' OR 'N'
     C*
     C* N90      SCPRIN    IFNE 'Y'
     C*
     C*  PROMPT FOR 2ND AGENT MUST BE 'Y' OR 'N'
     C*
     C  N90SCA2FL        IFNE      'Y'
     C     SCA2FL        ANDNE     'N'
     C                   SETON                                        81  90
     C                   ENDIF                                                  SCA2FL<>Y/N
     C*
     C*  CONTACT HEADINGS CANNOT BE BLANK
     C*
     C  N90SCCNT1        IFEQ      *BLANKS
     C                   SETON                                        26  90
     C                   ENDIF                                                  SCCNT1=*BLANKS
     C*
     C  N90SCCNT2        IFEQ      *BLANKS
     C                   SETON                                        27  90
     C                   ENDIF                                                  SCCNT2=*BLANKS
     C*
     C  N90SCCNT3        IFEQ      *BLANKS
     C                   SETON                                        28  90
     C                   ENDIF                                                  SCCNT3=*BLANKS
     C*
     C  N90SCCNT4        IFEQ      *BLANKS
     C     CLTID         ANDEQ     '071'
     C                   SETON                                        76  90
     C                   ENDIF                                                  SCCNT4=*BLANKS
      *
      *  'FastQuote contact x-ref' codes entered must be valid
      *
     C  N90SCFQC1        IFNE      *BLANKS
     C                   MOVEL(P)  'FQCT'        XXTBID
     C                   MOVEL(P)  SCFQC1        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C   60              SETON                                        90  51
     C                   ENDIF                                                  SCFQC1
      *
     C  N90SCFQC2        IFNE      *BLANKS
     C                   MOVEL(P)  'FQCT'        XXTBID
     C                   MOVEL(P)  SCFQC2        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C   60              SETON                                        90  52
     C                   ENDIF                                                  SCFQC2
      *
     C  N90SCFQC3        IFNE      *BLANKS
     C                   MOVEL(P)  'FQCT'        XXTBID
     C                   MOVEL(P)  SCFQC3        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C   60              SETON                                        90  53
     C                   ENDIF                                                  SCFQC3
      *
     C  N90SCFQC4        IFNE      *BLANKS
     C                   MOVEL(P)  'FQCT'        XXTBID
     C                   MOVEL(P)  SCFQC4        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C   60              SETON                                        90  55
     C                   ENDIF                                                  SCFQC4
      *
      *
      *  'FastQuote contact x-ref' codes entered must be
      *   mutually exclusive
      *
     C                   MOVE      *OFF          DUPFLG            1
     C                   DO        4             X                 1 0
     C     FQ(X)         IFNE      *BLANK
     C                   DO        4             Y                 1 0
     C     FQ(Y)         IFEQ      FQ(X)
     C     X             ANDNE     Y
     C                   MOVE      *ON           DUPFLG            1
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     DUPFLG        IFEQ      *ON
     C                   SETON                                        54  90
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF                                                  FQ,X <> ' '
     C                   ENDDO
      *
      /Free
       msEDICon = ' ';
       If scEDIgrp = ' ' and visOnEDI = *on;
          msEDICon = 'Please select one EDI Contact';
          erEDICon = *On;
          *in90 = *On;
       Endif;
      /End-Free
     C*
     C*  REQUIRE EMPLOYER SIC CODES MUST BE 'Y' OR 'N'
     C*
     C  N90SCSICR        IFNE      'Y'
     C     SCSICR        ANDNE     'N'
     C                   SETON                                        23  90
     C                   ENDIF                                                  SCSICR<>Y/N
     C*
     C*
     C*  EMPLOYER FEDERAL ID REQUIRED (Y/N)
     C*
     C  N90SCFIFG        IFNE      'Y'
     C     SCFIFG        ANDNE     'N'
     C                   SETON                                        57  90
     C                   ENDIF                                                  SCFIFG<>Y/N
      // Require employer link code
      /Free
       erReqLnkCd = *off;
       If not *in90;
          If scReqLnkCd <> 'Y' and  scReqLnkCd <> ' ';
             erReqLnkCd = *On;
             *in90 = *On;
          Endif;
       Endif;
      /End-Free
     C*
     C*  POLICY PAGE PAYROLL PRINT CONTROL
     C*
     C  N90SCPPPC        IFNE      'D'
     C     SCPPPC        ANDNE     'S'
     C     SCPPPC        ANDNE     ' '
     C                   MOVE      *ON           *IN19
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCPPPC<>D/S

     C*  Print Employer Link code on schedule
     C*
     C                   Clear                   mgPrtLnk
     C                   Clear                   ErPrtLnk
     C  N90SCPRTLNK      IFEQ      'Y'
     C     SCPPPC        ANDNE     'D'
     C                   SETON                                            90
     C                   Eval      mgPrtLnk = 'Print Employer Link code +
     C                             on Schedule is only allowed if +
     C                             Payroll printing is set to D.'
     C                   Eval      ErPrtLnk = *on
     C                   ENDIF                                                  SCPRTLNK=Y

      *  Member Output Queue must exist if entered.
      *
     C  N90SCMOUT        IFNE      *BLANK
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    SCMOUT
     C                   PARM      '*OUTQ  '     ##TYPE            7
     C                   PARM      '0'           ##ERR             1
      *
     C     ##ERR         IFEQ      '1'
     C                   SETON                                        90  72
     C                   ENDIF                                                  ##ERR
      *
     C                   ENDIF                                                  SCMOUT
      *
      *  Agent Output Queue must exist if entered.
      *
     C  N90SCAOUT        IFNE      *BLANK
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    SCAOUT
     C                   PARM      '*OUTQ  '     ##TYPE            7
     C                   PARM      '0'           ##ERR             1
      *
     C     ##ERR         IFEQ      '1'
     C                   SETON                                        90  73
     C                   ENDIF                                                  ##ERR
      *
     C                   ENDIF                                                  SCAOUT
      *
      *  State Output Queue must exist if entered.
      *
     C  N90SCSOUT        IFNE      *BLANK
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    SCSOUT
     C                   PARM      '*OUTQ  '     ##TYPE            7
     C                   PARM      '0'           ##ERR             1
      *
     C     ##ERR         IFEQ      '1'
     C                   SETON                                        90  74
     C                   ENDIF                                                  ##ERR
      *
     C                   ENDIF                                                  SCSOUT
      *
      *
      *
      *  Canceled Cert-Holder Letter Output Queue must exist if entered.
      *
     C  N90SCCROQ        IFNE      *BLANK
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    SCCROQ
     C                   PARM      '*OUTQ  '     ##TYPE            7
     C                   PARM      '0'           ##ERR             1
      *
     C     ##ERR         IFEQ      '1'
     C                   SETON                                        90  75
     C                   ENDIF                                                  ##ERR
      *
     C                   ENDIF                                                  SCCOUT
      *
     C     SCSRFL        IFNE      *BLANK
     C     SCSRFL        ANDNE     'Y'
     C                   SETON                                        90  33
     C                   ENDIF                                                  SCSRFL
      *
      *
      // REQUIRE CUSTOMER ID MUST BE 'Y' OR 'N'
      /Free
       If *in90 = *Off;
         If scCust <> 'Y' and  scCust <> ' ';
           errcustid  = *On;
          *in90 = *On;
         Endif;
       Endif;
      /End-Free
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
     C*
     C*  OFFICE # CHANGE REMARK CODE MUST BE VALID
     C*
     C  N90SCOFRM        CHAIN     PCT01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C*
     C                   SETON                                        29  90
     C*
     C                   ELSE
     C*
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        30  90
     C                   ENDIF                                                  CTSTAT=I
     C*
     C                   ENDIF                                                  *IN60=*ON
     C*
     C*  CO NAME CHANGE REMARK CODE
     C*
     C  N90SCCNDR        IFNE      *BLANKS
     C  N90SCCNDR        CHAIN     PCT01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C*
     C                   SETON                                        97  90
     C*
     C                   ELSE
     C*
     C     CTSTAT        IFEQ      'I'
     C* FIX EXISTING BUG, WRONG INDICATOR WAS USED
     C                   SETON                                        98  90
     C                   ENDIF                                                  CTSTAT=I
     C*
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCCNDR NE BLANK
     C*
     C*
     C*  CO NAME CHANGE STAFF CODE
     C*
     C  N90SCCNST        IFNE      *BLANKS
     C  N90SCCNST        CHAIN     PUP01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C*
     C* FIX EXISTING BUG, WRONG INDICATOR WAS USED (*IN60)
     C                   SETON                                        65  90
     C*
     C                   ELSE
     C*
     C     UPSTAT        IFEQ      'I'
     C* FIX EXISTING BUG, WRONG INDICATOR WAS USED (*IN60)
     C                   SETON                                        65  90
     C                   ENDIF                                                  UPSTAT=I
     C*
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCCNST NE BLANK
     C*
     C  N90SCCNDR        IFNE      *BLANKS
     C     SCCNST        ANDEQ     *BLANKS
     C* FIX EXISTING BUG, WRONG INDICATOR WAS USED
     C                   SETON                                        67  90
     C                   ENDIF
     C*
     C  N90SCCNST        IFNE      *BLANKS
     C     SCCNDR        ANDEQ     *BLANKS
     C* FIX EXISTING BUG, WRONG INDICATOR WAS USED
     C                   SETON                                        68  90
     C                   ENDIF
     C*
     C*  LATE NOTICE REMARK CODE MUST BE VALID
     C*
     C  N90SCLNRM        IFNE      *BLANKS
     C  N90SCLNRM        CHAIN     PCT01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C*
     C                   SETON                                        31  90
     C*
     C                   ELSE
     C*
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        32  90
     C                   ENDIF                                                  CTSTAT=I
     C*
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCLNRM NE BLANK
     C*
      *
     C     CLTID         IFEQ      '129'
      * ACH One Time Notification Remark Code valid and active
      *
     C  N90SCBRLT        IFNE      *BLANKS
     C     SCBRLT        CHAIN     PCT01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   SETON                                        36  90
      *
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        37  90
     C                   ENDIF                                                  CTSTAT=I
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCENLT<>*BLANK
      *
     C                   ENDIF
     C*
     C*  PREMIUM NOTICE REMARK CODE MUST BE VALID
     C*
     C  N90SCPNRM        IFNE      *BLANKS
     C  N90SCPNRM        CHAIN     PCT01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C*
     C                   SETON                                        44  90
     C*
     C                   ELSE
     C*
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        45  90
     C                   ENDIF                                                  CTSTAT=I
     C*
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCPNRM NE BLANK
     C*
     C*  Default DARTS remark field must be valid
     C*
     C  N90SCASRM        IFNE      *BLANKS
     C     SCASRM        CHAIN     PCT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   SETON                                        39  90
     C*
     C                   ELSE
     C*
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        43  90
     C                   ENDIF                                                  CTSTAT=I
     C*
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCASRM NE BLANK
     C*
     C*  Default DARTS remark field must be valid
     C*
     C  N90SCDDRM        IFNE      *BLANKS
     C     SCDDRM        CHAIN     PCT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   SETON                                        23  90
     C*
     C                   ELSE
     C*
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        24  90
     C                   ENDIF                                                  CTSTAT=I
     C*
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCDDRM NE BLANK
     C*  AUDIT REQUEST REMARK CODE MUST BE VALID
     C*
     C  N90SCAURQ        IFNE      *BLANKS
     C  N90SCAURQ        CHAIN     PCT01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C*
     C                   SETON                                        46  90
     C*
     C                   ELSE
     C*
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        47  90
     C                   ENDIF                                                  CTSTAT=I
     C*
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCAURQ NE BLANK
      *
     C     USESS#        IFEQ      *ON
      * PRINT SOCIAL SECURITY # MUST BE VALID
     C  N90SCASSF        IFNE      ' '
     C  N90SCASSF        IFNE      'Y'
     C     SCASSF        ANDNE     'N'
     C     SCASSF        ANDNE     '4'
     C*
     C                   SETON                                        808190
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   SETON                                        808990
     C*
     C                   ENDIF
     C                   ENDIF
     C*  Default DARTS remark field must be valid
     C*
     C  N90SCA1CD        IFNE      *BLANKS
     C     SCA1CD        CHAIN     PCT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   SETON                                        33  90
     C*
     C                   ELSE
     C*
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        34  90
     C                   ENDIF                                                  CTSTAT=I
     C*
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCA1CD NE BLANK
     C*
     C  N90SCA1CD        IFNE      *BLANKS
     C     SCAA#M        ANDEQ     0
     C                   SETON                                        357090
     C                   ENDIF                                                  SCA1CD NE BLANK
     C*
     C  N90SCAA#M        IFNE      0
     C     SCA1CD        ANDEQ     *BLANKS
     C                   SETON                                        346990
     C                   ENDIF                                                  SCAA#M NE BLANK
     C*
     C  N90SCA1CD        IFNE      *BLANKS
     C  N90SCA1CD        CHAIN     PDR01                              60
     C  N90*IN60         IFEQ      *ON
     C                   SETON                                        716990
     C                   ENDIF
     C                   ENDIF
      * CANCELLATION REMARK CODE MUST BE VALID
      *
     C  N90SCCXCD        IFNE      *BLANKS
     C  N90SCCXCD        CHAIN     PCT01                              60
      *
     C  N90*IN60         IFEQ      *ON
      *
     C                   SETON                                        48  90
      *
     C                   ELSE
      *
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        49  90
     C                   ENDIF                                                  CTSTAT IFEQ 'I'
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C                   ENDIF                                                  SCCXCD NE BLANK
      *
      * REVISED CANCELLATION REMARK CODE MUST BE VALID
      *
     C  N90SCRCCD        IFNE      *BLANKS
     C  N90SCRCCD        CHAIN     PCT01                              60
      *
     C  N90*IN60         IFEQ      *ON
      *
     C                   SETON                                        50  90
      *
     C                   ELSE
      *
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        51  90
     C                   ENDIF                                                  CTSTAT IFEQ 'I'
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C                   ENDIF                                                  SCRCCD NE BLANK
      *
      *
      * FEE INVOICE REMARK CODE MUST BE VALID ......
      *
     C     WCFL44        IFEQ      'Y'
      *
     C  N90SCFIRM        IFNE      *BLANKS
     C  N90SCFIRM        CHAIN     PCT01                              60
      *
     C  N90*IN60         IFEQ      *ON
      *
     C                   SETON                                        878890
      *
     C                   ELSE
      *
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        878890
     C                   ENDIF                                                  CTSTAT IFEQ 'I'
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *
     C                   SETOFF                                       60
      *
     C                   ENDIF                                                  SCFIRM NE BLANK
      *
     C                   ENDIF                                                  WCFL44 = 'Y'
      *
      *
      * REINSTATEMENT REMARK CODE MUST BE VALID
      *
     C  N90SCRECD        IFNE      *BLANKS
     C  N90SCRECD        CHAIN     PCT01                              60
      *
     C  N90*IN60         IFEQ      *ON
      *
     C                   SETON                                        52  90
      *
     C                   ELSE
      *
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        53  90
     C                   ENDIF                                                  CTSTAT IFEQ 'I'
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C                   ENDIF                                                  SCRECD NE BLANK
      *
      * REVISED REINSTATEMENT REMARK CODE MUST BE VALID
      *
     C  N90SCRRCD        IFNE      *BLANKS
     C  N90SCRRCD        CHAIN     PCT01                              60
      *
     C  N90*IN60         IFEQ      *ON
      *
     C                   SETON                                        54  90
      *
     C                   ELSE
      *
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        55  90
     C                   ENDIF                                                  CTSTAT IFEQ 'I'
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C                   ENDIF                                                  SCRRCD NE BLANK
      *
      * POLICY PAGE REMARK CODE MUST BE VALID
      *
     C  N90SCPPCD        IFNE      *BLANKS
     C  N90SCPPCD        CHAIN     PCT01                              60
      *
     C  N90*IN60         IFEQ      *ON
      *
     C                   SETON                                        56  90
      *
     C                   ELSE
      *
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        57  90
     C                   ENDIF                                                  CTSTAT IFEQ 'I'
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C                   ENDIF                                                  SCPPCD NE BLANK
      *
      * Alternate employer remark must be valid
      *
     C  N90SCAPCD        IFNE      *BLANKS
     C     SCAPCD        CHAIN     PCT01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   SETON                                        91  90
      *
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        92  90
     C                   ENDIF                                                  CTSTAT=I
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCAPCD<>*BLANK
      *
      * Agent commission statement remark must be valid
      *
     C  N90SCACSR        IFNE      *BLANKS
     C  N90SCACSR        CHAIN     PCT01                              60
      *
     C  N90*IN60         IFEQ      *ON
     C                   SETON                                        25  90
      *
     C                   ELSE
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        42  90
     C                   ENDIF                                                  CTSTAT=I
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCACSR<>*BLANK
      * POLICY PAGE REMARK CODE MUST BE VALID
      *
     C  N90SCMICD        IFNE      *BLANKS
     C  N90SCMICD        CHAIN     PCT01                              60
      *
     C  N90*IN60         IFEQ      *ON
      *
     C                   SETON                                        58  90
      *
     C                   ELSE
      *
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        59  90
     C                   ENDIF                                                  CTSTAT IFEQ 'I'
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C                   ENDIF                                                  SCMICD NE BLANK
     C*
     C* AGT COMM PLAN CHANG REMARK CODE MUST BE VALID
     C*
     C  N90SCPLDR        IFNE      *BLANKS
     C*
     C     SCPLDR        CHAIN     PCT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   SETON                                        27  90
     C*
     C                   ELSE
     C*
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        28  90
     C                   ENDIF                                                  CTSTAT=I
     C*
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  SCPLDR<>*BLANK
      *
      * Audit Tracking Deletion Remark Code must be valid and active
      *
     C  N90SCAUDL        IFNE      *BLANKS
     C     SCAUDL        CHAIN     PCT01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   SETON                                        82  90
      *
     C                   ELSE
      *
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        83  90
     C                   ENDIF                                                  CTSTAT=I
      *
     C                   ENDIF                                                  *IN60=*ON
      *
     C                   ENDIF                                                  SCAUDL<>*BLANK
      *
      * ACORD Application Remark Code must be valid and active
      /Free
       erACORD = *off;
       If not *in90 and scACORD <> *blanks;
          Chain (scACORD) pct01;
          Select;
          When not %found;
             *in90 = *on;
             erACORD = *on;
             msACORD = 'WCR0827';
             mfACORD = 'WMESGG';
          When CTstat = 'I';
             *in90 = *on;
             erACORD = *on;
             msACORD = 'SCM0614';
             mfACORD = 'SCMESG';
          Endsl;
       Endif;
      /End-free
      *
      * REVISED BILL ON RENEWAL Remark Code must be valid and active
      *
     C  N90SCRVRN        IFNE      *BLANKS
     C     SCRVRN        CHAIN     PCT01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   SETON                                        84  90
      *
     C                   ELSE
      *
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        85  90
     C                   ENDIF                                                  CTSTAT=I
      *
     C                   ENDIF                                                  *IN60=*ON
      *
     C                   ENDIF                                                  SCRVRN<>*BLANK
      *
      * Document Upload Remark Code must be valid and active
      /Free
       erDocUplod = *off;
       If not *in90 and scDocUplod <> *blanks;
          Chain (scDocUplod) pct01;
          Select;
          When not %found;
             *in90 = *on;
             erDocUplod = *on;
             msDocUplod = 'WCR0827';
             mfDocUplod = 'WMESGG';
          When CTstat = 'I';
             *in90 = *on;
             erDocUplod = *on;
             msDocUplod = 'SCM0614';
             mfDocUplod = 'SCMESG';
          Endsl;
       Endif;
      /End-free
      *
      * Resolve COI form, Remark Code must be valid and active
      *
     C  N90SCCICD        IFNE      *BLANKS
     C     SCCICD        CHAIN     PCT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        94  90
      *
     C                   ELSE
      *
     C     CTSTAT        IFEQ      'I'
     C                   SETON                                        95  90
     C                   ENDIF                                                  CTSTAT=I
      *
     C                   ENDIF                                                  *IN60=*ON
      *
     C                   ENDIF                                                  SCCICD<>*BLANK
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90EXDT          IFGT      SAVEDT
     C     EXDT          OREQ      SAVEDT
     C     EXTIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  F2DT>SAVEDT
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK3
     C*===============================================================
     C* ERRCK4 - SUBROUTINE FOR ERROR CHECKING SCR04
     C*===============================================================
     C     ERRCK4        BEGSR
     C*  PROMPT FOR THE NEW UPDATE ACROSS ALL DIVISIONS FLAG.
     C*
     C  N90SCUPDA        IFNE      'Y'
     C     SCUPDA        ANDNE     ' '
     C                   SETON                                        42  90
     C                   ENDIF                                                  SCUPDANE(Y/ )
     C  N90SC3AGT        IFNE      'Y'
     C     SC3AGT        ANDNE     ' '
     C                   SETON                                        42  90
     C                   ENDIF
     C*
     C* DO NOT ENTER A FLEX TABLE ID IF NO FLEX HEADING
     C*
     C     *IN90         IFEQ      *OFF
     C     SCFLXH        IFEQ      *BLANKS
     C     SCFLXT        ANDNE     *BLANKS
     C                   SETON                                        8690
     C                   ENDIF
     C                   ENDIF
     C*
     C* FLEX TABLE VALUE IS REQUIRED.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCFLXH        IFNE      *BLANKS
     C     SCFLXT        ANDEQ     *BLANKS
     C                   SETON                                        8590
     C                   ENDIF
     C                   ENDIF
     C*
     C* INVALID TABLE VALUE
     C*
     C     *IN90         IFEQ      *OFF
     C     SCFLXT        ANDNE     *BLANKS
     C                   MOVEL(P)  SCFLXT        KKTBID
     C     GIKY          CHAIN     PGI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        8490
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR                                                  ERRCK4
     C*===============================================================
     C* ERRCK5 - SUBROUTINE FOR ERROR CHECKING SCR05
     C*===============================================================
     C     ERRCK5        BEGSR
     C                   ENDSR                                                  ERRCK5
     C*
      *===============================================================
      * KJDO - SUBROUTINE FOR F10
      *===============================================================
      *
      *  F10=SEARCH
      *
     C     KJDO          BEGSR
     C*
     C***  Begin Add  ***  10/25/19  ****************************
     C                   Eval      sfAuDl = *off
     C                   Eval      sfACORD = *off
     C                   Eval      sfDocUplod = *off
     C***  Begin Add  ***  10/25/19  ****************************
      *
     C     HFLD          IFEQ      'SCCNST'
     C                   CALL      'S0005R'      UPRMPL
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PUP03                              99
     C*
     C                   MOVEL(P)  UPSTAF        SCCNST
     C                   MOVE      *ON           *IN73
     C                   ELSE
     C                   MOVE      *ON           *IN63
     C                   END
     C*
     C                   ENDIF
      *
     C                   SELECT
     C*_______________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCDRGN'
     C*
     C                   CALL      'W0520R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN1            9 0
     C                   PARM      ##RRN         ##RRN2            9 0
     C*
     C     ##RRN1        IFNE      *ZEROS
     C     ##RRN1        CHAIN     PRE0P                              60
     C                   Z-ADD     RERGN#        SCDRGN
     C                   MOVE      *ON           *IN70
     C                   ELSE
     C                   MOVE      *ON           *IN72
     C                   ENDIF                                                  ##RRN1<>*ZEROS
     C*_______________________________________________________________
      *
      * OFFICE NUMBER SEARCH
      *
     C     HFLD          WHENEQ    'SCDOFF'
      *
     C                   CALL      'W0002R'      OFFPL
      *
     C     ##OFF         IFNE      *BLANKS
     C                   MOVEL     ##OFF         SCDOFF
     C                   ELSE
     C                   MOVE      *ON           *IN70
     C                   ENDIF                                                  ##OFF<>*BLANKS
     C*
     C* FLEX TABLE ID SEARCH
     C*
     C     HFLD          WHENEQ    'SCFLXT'
     C*
     C                   CALL      'W2491R'
     C                   PARM                    ##PRDS           40
     C                   PARM      *BLANKS       ##CALL            1
     C                   PARM      *BLANKS       ##ID              4
     C                   PARM      *BLANKS       ##RTC             1
     C*
     C     ##ID          IFEQ      *BLANKS
     C                   SETON                                        3690
     C                   ELSE
     C                   MOVEL(P)  ##ID          SCFLXT
     C                   SETON                                        3790
     C                   ENDIF
      *
      * A/R/ TRANSACTION CODE SEARCH
      *
     C     HFLD          WHENEQ    'SCESCD'
     C     HFLD          OREQ      'SCSRCD'
     C     HFLD          OREQ      'SCDACD'
     C     HFLD          OREQ      'SCAACD'
      *
     C                   MOVE      *BLANKS       SCKEY            10
      *
     C                   CALL      'W2161R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
      *
     C     *INKL         IFNE      *ON
     C     SCKEY         ANDNE     *BLANKS
      *
     C                   SELECT
      *
     C                   ENDSL                                                  HFLD WHEQ
      *
     C                   ELSE
      *
     C     HFLD          COMP      'SCESCD'                               73
     C     HFLD          COMP      'SCSRCD'                               74
     C     HFLD          COMP      'SCDACD'                               75
     C     HFLD          COMP      'SCAACD'                               87
      *
     C                   ENDIF                                                  SCKEY IFNE
     C                   SETOFF                                       96
      *
      * DARTS REMARK CODE SEARCH
      *
     C     HFLD          WHENEQ    'SCORG'
     C     HFLD          OREQ      'SCREV'
     C     HFLD          OREQ      'SCAUD'
     C     HFLD          OREQ      'SCOFRM'
     C     HFLD          OREQ      'SCLNRM'
     C     HFLD          OREQ      'SCPNRM'
     C     HFLD          OREQ      'SCAURQ'
     C     HFLD          OREQ      'SCCXCD'
     C     HFLD          OREQ      'SCRCCD'
     C     HFLD          OREQ      'SCRECD'
     C     HFLD          OREQ      'SCRRCD'
     C     HFLD          OREQ      'SCPPCD'
     C     HFLD          OREQ      'SCRFAF'
     C     HFLD          OREQ      'SCQUOT'
     C     HFLD          OREQ      'SCMICD'
     C     HFLD          OREQ      'SCPLDR'
     C     HFLD          OREQ      'SCAUDL'
     C     HFLD          OREQ      'SCRVRN'
     C     HFLD          OREQ      'SCCICD'
     C     HFLD          OREQ      'SCBRLT'
      *
     C     HFLD          OREQ      'SCCNDR'
     C     HFLD          OREQ      'SCASRM'
     C     HFLD          OREQ      'SCAPCD'
     C     HFLD          OREQ      'SCACSR'
     C     HFLD          OREQ      'SCDDRM'
     C     HFLD          OREQ      'SCA1CD'
     C     HFLD          OREQ      'SCFIRM'
     C     HFLD          OREQ      'SCACORD'
     C     HFLD          OREQ      'SCDOCUPLOD'
     C                   MOVE      *BLANKS       QQRMCD            5
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C     QQRMCD        PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     QQRMCD        IFNE      *BLANKS
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCOFRM'
     C                   MOVEL     QQRMCD        SCOFRM
     C                   MOVE      *ON           *IN62
      *
     C                   MOVE      *ON           *IN38
      *
     C     HFLD          WHENEQ    'SCBRLT'
     C                   MOVEL(P)  QQRMCD        SCBRLT
     C* Position cursor on Premium notice remark
     C                   MOVE      *ON           *IN74
      *
     C     HFLD          WHENEQ    'SCLNRM'
     C                   MOVEL     QQRMCD        SCLNRM
     C* If ACH one time remark code is displayed (*IN06=*ON) then position
     C* cursor on it,
     C     *IN06         IFEQ      *ON
     C                   MOVE      *ON           *IN38
     C                   ELSE
     C* else position cursor on premium notice remark
     C                   MOVE      *ON           *IN74
     C                   ENDIF                                                  *IN06
      *
     C     HFLD          WHENEQ    'SCPNRM'
     C                   MOVEL     QQRMCD        SCPNRM
     C                   MOVE      *ON           *IN64
      *
     C     HFLD          WHENEQ    'SCAURQ'
     C                   MOVEL     QQRMCD        SCAURQ
     C                   MOVE      *ON           *IN93
      *
     C     HFLD          WHENEQ    'SCCXCD'
     C                   MOVEL     QQRMCD        SCCXCD
     C                   MOVE      *ON           *IN96
      *
     C     HFLD          WHENEQ    'SCRCCD'
     C                   MOVEL     QQRMCD        SCRCCD
     C  N41              MOVE      *ON           *IN78
     C   41              MOVE      *ON           *IN87
      *
     C     HFLD          WHENEQ    'SCRECD'
     C                   MOVEL     QQRMCD        SCRECD
     C                   MOVE      *ON           *IN79
      *
     C     HFLD          WHENEQ    'SCRRCD'
     C                   MOVEL     QQRMCD        SCRRCD
     C                   MOVE      *ON           *IN80
      *
     C     HFLD          WHENEQ    'SCFIRM'
     C                   MOVEL     QQRMCD        SCFIRM
     C                   MOVE      *ON           *IN78
      *
     C     HFLD          WHENEQ    'SCPPCD'
     C                   MOVEL     QQRMCD        SCPPCD
     C     *IN07         IFEQ      *ON
     C                   MOVE      *ON           *IN04
     C                   ELSE
     C                   MOVE      *ON           *IN81
     C                   ENDIF                                                  *IN07 = *ON
     C     HFLD          WHENEQ    'SCAPCD'
     C                   MOVEL     QQRMCD        SCAPCD
     C                   MOVE      *ON           *IN81
     C     HFLD          WHENEQ    'SCACSR'
     C                   MOVEL     QQRMCD        SCACSR
     C                   MOVE      *ON           *IN19
     C*
     C     HFLD          WHENEQ    'SCMICD'
     C                   MOVEL     QQRMCD        SCMICD
     C                   MOVE      *ON           *IN26
     C*
     C     HFLD          WHENEQ    'SCPLDR'
     C                   MOVEL     QQRMCD        SCPLDR
     C                   Move      *on           sfAuDl
      *
     C     HFLD          WHENEQ    'SCAUDL'
     C                   MOVEL(P)  QQRMCD        SCAUDL
     C                   Move      *on           sfACORD
      *
     C     HFLD          WHENEQ    'SCRVRN'
     C                   MOVEL(P)  QQRMCD        SCRVRN
     C                   Move      *on           sfDocUplod
     C*
     C     HFLD          Wheneq    'SCACORD'
     C                   Movel(p)  qqRmCd        scACORD
     C                   Move      *on           *IN86
     C*
     C     HFLD          Wheneq    'SCDOCUPLOD'
     C                   Movel(p)  qqRmCd        scDocUplod
     C                   Move      *on           *IN72
      *
     C     HFLD          WHENEQ    'SCCICD'
     C                   MOVEL(P)  QQRMCD        SCCICD
     C                   MOVE      *ON           *IN77
      *
      *
     C     HFLD          WHENEQ    'SCCNDR'
     C                   MOVEL(P)  QQRMCD        SCCNDR
     C                   MOVE      *ON           *IN63
      *
     C     HFLD          WHENEQ    'SCASRM'
     C                   MOVEL     QQRMCD        SCASRM
     C                   MOVE      *ON           *IN75
     C     HFLD          WHENEQ    'SCDDRM'
     C                   MOVEL     QQRMCD        SCDDRM
     C                   MOVE      *ON           *IN80
     C     HFLD          WHENEQ    'SCA1CD'
     C                   MOVEL     QQRMCD        SCA1CD
     C                   MOVE      *ON           *IN70
     C                   ENDSL                                                  HFLD WHEQ
      *
     C                   ELSE
      *
     C     HFLD          COMP      'SCQUOT'                               89
     C     HFLD          COMP      'SCORG'                                76
     C     HFLD          COMP      'SCREV'                                77
     C     HFLD          COMP      'SCAUD'                                78
     C     HFLD          COMP      'SCOFRM'                               72
     C     HFLD          COMP      'SCLNRM'                               73
     C     HFLD          COMP      'SCPNRM'                               74
     C     HFLD          COMP      'SCAURQ'                               75
     C  N76HFLD          COMP      'SCCXCD'                               76
     C  N77HFLD          COMP      'SCRCCD'                               77
     C  N78HFLD          COMP      'SCRECD'                               78
     C     HFLD          COMP      'SCRRCD'                               79
     C     HFLD          COMP      'SCPPCD'                               80
     C     HFLD          COMP      'SCRFAF'                               83
     C     HFLD          COMP      'SCMICD'                               81
     C     HFLD          COMP      'SCPLDR'                               26
     C     HFLD          COMP      'SCBRLT'                               38
     C     HFLD          COMP      'SCRVRN'                               86
     C     HFLD          COMP      'SCCICD'                               96
     C     HFLD          COMP      'SCCNDR'                               62
     C     HFLD          COMP      'SCASRM'                               64
     C     HFLD          COMP      'SCAPCD'                               04
     C     HFLD          COMP      'SCACSR'                               19
     C     HFLD          COMP      'SCDDRM'                               64
     C     HFLD          COMP      'SCA1CD'                               69
     C     HFLD          COMP      'SCFIRM'                               87
     C                   Select
     C                   When      hfld = 'SCAUDL'
     C                   Eval      sfAuDl = *on
     C                   When      hfld = 'SCACORD'
     C                   Eval      sfACORD = *on
     C                   When      hfld = 'SCDOCUPLOD'
     C                   Eval      sfDocUplod = *on
     C                   Endsl
      *
     C                   ENDIF                                                  QQRMCD IFNE
      *
      * FastQuote contact code
      *
     C     HFLD          WHENEQ    'SCFQC1'
     C     HFLD          OREQ      'SCFQC2'
     C     HFLD          OREQ      'SCFQC3'
     C     HFLD          OREQ      'SCFQC4'
      *
     C                   CALL      'W2493R'
     C                   PARM      SCPRDS        ##PRDS           40
     C                   PARM      *BLANK        ##CALL            1
     C                   PARM      'FQCT'        ##ID              4
     C                   PARM      *BLANKS       ##KEY            10
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     ##KEY         IFNE      *BLANKS
      *
     C     HFLD          IFEQ      'SCFQC1'
     C                   MOVEL(P)  ##KEY         SCFQC1
     C                   SETON                                        25
     C                   ENDIF
      *
     C     HFLD          IFEQ      'SCFQC2'
     C                   MOVEL(P)  ##KEY         SCFQC2
     C                   SETON                                        29
     C                   ENDIF
      *
     C     HFLD          IFEQ      'SCFQC3'
     C                   MOVEL(P)  ##KEY         SCFQC3
     C                   SETON                                        30
     C                   ENDIF
      *
     C     HFLD          IFEQ      'SCFQC4'
     C                   MOVEL(P)  ##KEY         SCFQC4
     C                   SETON                                        83
     C                   ENDIF
      *
     C                   ELSE
      *
     C     HFLD          IFEQ      'SCFQC1'
     C                   SETON                                        79
     C                   ENDIF
      *
     C     HFLD          IFEQ      'SCFQC2'
     C                   SETON                                        80
     C                   ENDIF
      *
     C     HFLD          IFEQ      'SCFQC3'
     C                   SETON                                        22
     C                   ENDIF
      *
     C     HFLD          IFEQ      'SCFQC4'
     C                   SETON                                        82
     C                   ENDIF
      *
     C                   ENDIF                                                  ##KEY
      *
     C                   ENDSL                                                  HFLD WHEQ
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     EXDLTL        SCDLTL
     C                   MOVEL     EXDLTT        SCDLTT
     C                   Z-ADD     EXOVRL        SCOVRL
     C                   MOVEL     EXOVRT        SCOVRT
     C                   Z-ADD     EXUPDL        SCUPDL
     C                   MOVEL     EXUPDT        SCUPDT
     C                   Z-ADD     EXSUBL        SCSUBL
     C                   MOVEL     EXSUBT        SCSUBT
     C                   Z-ADD     EXADLL        SCADLL
     C                   MOVEL     EXADLT        SCADLT
     C                   Z-ADD     EXADQL        SCADQL
     C                   MOVEL     EXADQT        SCADQT
     C                   Z-ADD     EXPLNL        SCPLNL
     C                   MOVEL     EXPLNT        SCPLNT
     C                   Z-ADD     EXPRSL        SCPRSL
     C                   MOVEL     EXPRST        SCPRST
     C                   MOVEL     EXSICR        SCSICR
     C                   MOVE      EXDLAR        SCDLAR
     C                   MOVE      EXEDFL        SCEDFL
     C                   MOVEL     EXRAFL        SCRAFL
     C                   Z-ADD     EXDRGN        SCDRGN
     C                   MOVEL     EXDOFF        SCDOFF
     C                   MOVEL     EXA2FL        SCA2FL
     C                   MOVEL     EXAHDR        SCAHDR
     C*
     C                   MOVE      EXFIFG        SCFIFG
     C                   MOVE      EXreqLnkCd    scReqLnkCd
     C                   MOVE      EXPPPC        SCPPPC
      /Free
       scCxStRp = EXcxstrp;
      /End-Free
     C*
     C                   MOVE      EXPRTL        SCPrtLnk
      /Free
       Sccust= Excust;
      /End-Free
     C*
     C*
     C     EXCNT1        IFEQ      *BLANKS
     C                   MOVEL(P)  CNT1          SCCNT1
     C                   ELSE
     C                   MOVEL(P)  EXCNT1        SCCNT1
     C                   ENDIF                                                  EXCNT1=*BLANKS
     C*
     C     EXCNT2        IFEQ      *BLANKS
     C                   MOVEL(P)  CNT2          SCCNT2
     C                   ELSE
     C                   MOVEL(P)  EXCNT2        SCCNT2
     C                   ENDIF                                                  EXCNT2=*BLANKS
     C*
     C     EXCNT3        IFEQ      *BLANKS
     C                   MOVEL(P)  CNT3          SCCNT3
     C                   ELSE
     C                   MOVEL(P)  EXCNT3        SCCNT3
     C                   ENDIF                                                  EXCNT3=*BLANKS
     C*
     C     EXCNT4        IFEQ      *BLANKS
     C                   MOVEL(P)  CNT4          SCCNT4
     C                   ELSE
     C                   MOVEL(P)  EXCNT4        SCCNT4
     C                   ENDIF                                                  EXCNT3=*BLANKS
     C*
     C                   MOVEL(P)  EXFQC4        SCFQC4
     C                   MOVEL(P)  EXFQC1        SCFQC1
     C                   MOVEL(P)  EXFQC2        SCFQC2
     C                   MOVEL(P)  EXFQC3        SCFQC3
     C*
     C                   MOVEL     EXOFRM        SCOFRM
     C                   MOVEL     EXLNRM        SCLNRM
     C                   MOVEL     EXPNRM        SCPNRM
     C                   MOVEL     EXAURQ        SCAURQ
     C                   MOVEL     EXCXCD        SCCXCD
     C                   MOVEL     EXRCCD        SCRCCD
     C                   MOVEL     EXRECD        SCRECD
     C                   MOVEL     EXRRCD        SCRRCD
     C                   MOVEL     EXPPCD        SCPPCD
     C                   Z-ADD     EXPDCL        SCPDCL
     C                   MOVE      EXPDCT        SCPDCT
     C                   MOVEL     EXMICD        SCMICD
     C                   MOVEL     EXPLDR        SCPLDR
     C                   MOVEL(P)  EXAUDL        SCAUDL
     C                   MOVE      EXBRLT        SCBRLT
     C                   MOVEL(P)  EXRVRN        SCRVRN
     C                   MOVEL(P)  EXCICD        SCCICD
     C                   MOVEL(P)  EXCNDR        SCCNDR
     C                   MOVEL(P)  EXCNST        SCCNST
     C*
     C                   Eval      scNewCover = EXNCDP + EXNCDC + EXNCFD
     C                   Eval      scRnwCover = EXRCDP + EXRCDC + EXRCFD
      *
     C                   MOVE      EXMOUT        SCMOUT
     C                   MOVE      EXAOUT        SCAOUT
     C                   MOVE      EXSOUT        SCSOUT
      *
     C                   MOVE      EXCROQ        SCCROQ
      *
     C                   MOVEL(P)  EXAHLC        SCAHLC
     C                   MOVEL(P)  EXAHWL        SCAHWL
     C                   MOVEL(P)  EXAHVP        SCAHVP
     C                   MOVEL(P)  EXAHSA        SCAHSA
     C                   MOVEL     EXASRM        SCASRM
     C                   MOVEL     EXDDRM        SCDDRM
     C                   MOVEL     EXAPCD        SCAPCD
     C                   MOVEL     EXACSR        SCACSR
     C                   Movel     EXacord       scACORD
     C                   Movel     EXdocUplod    scDocUplod
      *
     C                   MOVEL(P)  EXUPDA        SCUPDA
     C                   MOVEL(P)  EX3AGT        SC3AGT
     C                   MOVEL(P)  EXAACD        SCA1CD
     C                   Z-ADD     EXAA#M        SCAA#M
     C                   MOVEL     EXAAFL        SCAAFL
     C                   MOVEL     EXASSF        SCASSF
     C                   MOVEL(P)  EXFLXH        SCFLXH
     C                   MOVEL(P)  EXFLXT        SCFLXT
     C                   MOVEL     EXLDAL        SCLDAL
     C                   MOVEL     EXLDAT        SCLDAT
     C     WCFL44        IFEQ      'Y'
     C                   SETON                                        41
     C                   ELSE
     C                   SETOFF                                       41
     C                   ENDIF
     C                   MOVEL(P)  EXFIRM        SCFIRM
     C                   MOVEL     EXSRFL        SCSRFL
     C*
     C* Set display attributes for screen
     C                   SELECT
     C     CLTID         WHENEQ    '093'
     C                   MOVE      X'20'         AT1P                           Normal
     C                   MOVE      X'26'         AT1F                           Underscore/High
     C                   MOVE      *ON           USESS#            1
     C     *IN09         IFEQ      *ON
     C                   MOVE      X'A2'         AT1F                           Protect/High
     C                   ENDIF
     C                   OTHER
     C                   CLEAR                   SCASSF
     C                   MOVE      X'A7'         AT1P                           Non-Dsp Prot
     C                   MOVE      X'A7'         AT1F                           Non-Dsp Prot
     C                   MOVE      *OFF          USESS#            1
     C                   ENDSL
     C*
      /Free
       scTRFL  = EXtrfl;
       scQTIL  = EXqtil;
       scQTIT  = EXqtit;
       scQTBL  = EXqtbl;
       scQTBT  = EXqtbt;
      /End-Free
      /Free
       scEDIGRP = EXecnt;
      /End-free
       //*** Beg Add ***** 03/02/22 ***********************
        scAcsrCSV = EXacsrCsv;
       //*** End Add ***** 03/02/22 ***********************
       //*** Beg Add ***** 05/24/23 ***********************
       scDltLqt = EXdltLqt;
       scDltTqt = EXdltTqt;
       //*** End Add ***** 05/24/23 ***********************
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCDLTL        EXDLTL
     C                   MOVEL     SCDLTT        EXDLTT
     C                   Z-ADD     SCOVRL        EXOVRL
     C                   MOVEL     SCOVRT        EXOVRT
     C                   Z-ADD     SCUPDL        EXUPDL
     C                   MOVEL     SCUPDT        EXUPDT
     C                   Z-ADD     SCSUBL        EXSUBL
     C                   MOVEL     SCSUBT        EXSUBT
     C                   Z-ADD     SCADLL        EXADLL
     C                   MOVEL     SCADLT        EXADLT
     C                   Z-ADD     SCADQL        EXADQL
     C                   MOVEL     SCADQT        EXADQT
     C                   Z-ADD     SCPLNL        EXPLNL
     C                   MOVEL     SCPLNT        EXPLNT
     C                   Z-ADD     SCPRSL        EXPRSL
     C                   MOVEL     SCPRST        EXPRST
     C                   MOVEL     SCSICR        EXSICR
     C                   MOVE      SCEDFL        EXEDFL
     C                   MOVEL     SCRAFL        EXRAFL
     C                   MOVE      SCDLAR        EXDLAR
     C                   Z-ADD     SCDRGN        EXDRGN
     C                   MOVEL     SCDOFF        EXDOFF
     C                   MOVEL     SCA2FL        EXA2FL
     C                   MOVEL     SCAHDR        EXAHDR
     C                   MOVEL     SCCNT1        EXCNT1
     C                   MOVEL     SCCNT2        EXCNT2
     C                   MOVEL     SCCNT3        EXCNT3
     C                   MOVEL     SCCNT4        EXCNT4
     C                   MOVEL(P)  SCFQC4        EXFQC4
     C                   MOVEL(P)  SCFQC1        EXFQC1
     C                   MOVEL(P)  SCFQC2        EXFQC2
     C                   MOVEL(P)  SCFQC3        EXFQC3
     C                   MOVEL     SCOFRM        EXOFRM
     C                   MOVEL     SCLNRM        EXLNRM
     C                   MOVEL     SCPNRM        EXPNRM
     C                   MOVEL     SCAURQ        EXAURQ
     C                   MOVEL     SCCXCD        EXCXCD
     C                   MOVEL     SCRCCD        EXRCCD
     C                   MOVEL     SCRECD        EXRECD
     C                   MOVEL     SCRRCD        EXRRCD
     C                   MOVEL     SCPPCD        EXPPCD
     C                   MOVEL     SCMICD        EXMICD
     C                   MOVEL     SCPLDR        EXPLDR
     C                   MOVEL(P)  SCAUDL        EXAUDL
     C                   MOVE      SCBRLT        EXBRLT
     C                   MOVEL(P)  SCRVRN        EXRVRN
     C                   MOVEL(P)  SCCICD        EXCICD
     C                   MOVEL(P)  SCCNDR        EXCNDR
     C                   MOVEL(P)  SCCNST        EXCNST
     C*
     C                   MOVE      SCFIFG        EXFIFG
     C                   MOVE      scReqLnkCd    EXreqLnkCd
     C                   MOVE      SCPPPC        EXPPPC
     C*
     C                   Z-ADD     SCPDCL        EXPDCL
     C                   MOVE      SCPDCT        EXPDCT

     C                   MOVE      ScPrtlnk      EXPRTL
      *
     C                   Movel(p)  xxNewDept     EXNCDP
     C                   Movel(p)  xxNewDoc      EXNCDC
     C                   Movel(p)  xxNewFold     EXNCFD
     C                   Movel(p)  xxRnwDept     EXRCDP
     C                   Movel(p)  xxRnwDoc      EXRCDC
     C                   Movel(p)  xxRnwFold     EXRCFD
      *
     C                   MOVE      SCMOUT        EXMOUT
     C                   MOVE      SCAOUT        EXAOUT
     C                   MOVE      SCSOUT        EXSOUT
      *
     C                   MOVE      SCCROQ        EXCROQ
      *
     C                   MOVEL(P)  SCAHLC        EXAHLC
     C                   MOVEL(P)  SCAHWL        EXAHWL
     C                   MOVEL(P)  SCAHVP        EXAHVP
     C                   MOVEL(P)  SCAHSA        EXAHSA
     C                   MOVEL(P)  SCUPDA        EXUPDA
     C                   MOVEL(P)  SC3AGT        EX3AGT
     C                   MOVEL     SCASRM        EXASRM
     C                   MOVEL     SCDDRM        EXDDRM
     C                   MOVEL     SCAPCD        EXAPCD
     C                   Movel     scACORD       EXacord
     C                   Movel     scDocUplod    EXdocUplod
     C*
     C                   MOVEL     SCACSR        EXACSR
     C                   MOVEL(P)  SCA1CD        EXAACD
     C                   Z-ADD     SCAA#M        EXAA#M
     C                   MOVEL     SCAAFL        EXAAFL
     C                   MOVEL     SCASSF        EXASSF
     C                   MOVEL(P)  SCFLXH        EXFLXH
     C                   MOVEL(P)  SCFLXT        EXFLXT
     C                   MOVEL     SCLDAL        EXLDAL
     C                   MOVEL     SCLDAT        EXLDAT
     C                   MOVEL     SCFIRM        EXFIRM
     C                   MOVEL     SCSRFL        EXSRFL
      /Free
       Excust= Sccust;
      /End-Free
      /Free
      /Free
       EXcxstrp = scCxStRp;
      /End-Free
       EXtrfl = scTRFL;
       EXqtil = scQTIL;
       EXqtit = scQTIT;
       EXqtbl = scQTBL;
       EXqtbt = scQTBT;
      /End-Free

      /Free
       EXecnt = scEDIGRP;
      /End-free

       //*** Beg Add ***** 03/02/22 ***********************
       EXacsrCsv = scAcsrCSV;
       //*** End Add ***** 03/02/22 ***********************
       //*** Beg Add ***** 05/24/23 ***********************
       EXdltLqt = scDltLQt;
       EXdltTQt = scDltTQt;
       //*** End Add ***** 05/24/23 ***********************

     C                   ENDSR                                                  MVSCDB
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C*
     C                   Eval      showNotify = *off
     C*
     C                   CALL      'SA046R'
     C                   PARM                    ##flag            1
     C*
     C                   If        ##flag = 'Y'
     C                   Eval      showNotify = *on
     C                   Endif
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C     ##RRN         CHAIN     PFN01                              60
     C  N60              Z-ADD     FNFND         SCFND
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C     EXKY          CHAIN     PEX01                              60
     C  N60              Z-ADD     DBRRN2        @@RRN             9 0
     C   60
     CANN09              MOVEL     'A'           @@MODE
     C*
     C     'WC'          CHAIN     SWC01                              60
     C  N60WCFL24        COMP      'Y'                                    94
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C     CLTID         IFEQ      '129'
     C                   MOVE      *ON           *IN06
     C                   ENDIF
      *
     C                   MOVE      *ON           *IN13
     C     CLTID         IFEQ      '139'
     C                   MOVE      *OFF          *IN13
     C                   ENDIF
     C                   SETON                                        16
     C     CLTID         IFEQ      '071'
     C                   MOVE      *ON           *IN35
     C                   ELSE
     C                   MOVE      *OFF          *IN35
     C                   ENDIF
     C     FNPEOF        IFEQ      'M'
     C                   SETON                                        41
     C                   ELSE
     C                   SETON                                        40
     C                   ENDIF
     C     FNPEOF        IFNE      ' '
     C                   SETON                                        07
     C                   ELSE
     C                   SETOFF                                       07
     C                   ENDIF
     C*
     C     WCFL41        IFEQ      'Y'
     C                   SETON                                        66
     C                   ELSE
     C                   SETOFF                                       66
     C                   ENDIF
     C                   Move      *off          showUpdReg
     C                   Move      *off          showUpdPEO
     C     WCFL41        Ifeq      'Y'
     C     FNPEOF        Ifeq      'M'
     C     FNPEOF        oreq      'C'
     C                   Move      *on           showUpdPEO
     C                   Else
     C                   Move      *on           showUpdReg
     C                   Endif
     C                   Endif
     C*
     C     WCFL43        IFEQ      'Y'
     C                   MOVE      X'20'         FL43H                          Normal
     C                   MOVE      X'24'         FL43V                          Underscore
     C     *IN09         IFEQ      *ON
     C                   MOVE      X'A2'         FL43V                          Protect/High
     C                   ENDIF
     C                   ELSE
     C                   MOVE      X'27'         FL43H                          Non-display
     C                   MOVE      X'A7'         FL43V                          Protect/Non-display
     C                   ENDIF
     C     WCFL44        IFEQ      'Y'
     C                   SETON                                        41
     C                   ELSE
     C                   SETOFF                                       41
     C                   ENDIF
      /Free
       visOnEDI = *off;
       visOnEDI35 = *off;
       If WCfl09 = 'Y';
          visOnEDI = *On;
          If *in35 = *On;
             visOnEDI35 = *On;
          Endif;
       Endif;
      /End-free
      /Free
       If @@Mode = 'A' Or @@Mode = 'C';
          isEditMod = *Off;
       Else;
          isEditMod = *On;
       EndIf;
      /End-Free
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR
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
     C                   MOVEL     Q1USER        EXUSER
     C                   MOVEL     Q1PGM         EXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EXDT
     C                   MOVEL     DSTIME        EXTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E02  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   Z-ADD     FNCO#         EXCO#
     C                   Z-ADD     SCFND         EXFND
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        EXIUSR
     C                   MOVEL     Q1USER        EXUSER
     C                   MOVEL     Q1PGM         EXIPGM
     C                   MOVEL     Q1PGM         EXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EXIDT
     C                   MOVEL     SRDTO         EXDT
     C                   MOVEL     DSTIME        EXITM
     C                   MOVEL     DSTIME        EXTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMEXP
     C*
     C                   MOVEL     'C'           @@MODE
     C*
     C                   ENDSR                                                  WRITER
