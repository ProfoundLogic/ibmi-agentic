     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*CALLER)
     H BNDDIR('WTBNDDIR')
     H/define CONVERT
     H*
     H/TITLE W7192R - Employer's Experience Report
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM DESCRIPTION:                                       *
     H*      THIS PROGRAM ALLOWS THE USER TO PRINT THE EMPLOYER'S     *
     H*      EXPERIENCE REPORT FOR A PARTICULAR FUND, FUND YEAR,      *
     H*      EMPLOYER, AGENT OR REGION WITHIN SPECIFIED SELECTION     *
     H*      CRITERIA WITHIN SELECTED DATA SORT.                      *
     H*                                                               *
     H*    CALLED FROM:                                               *
     H*      W7191C                                                   *
     H*                                                               *
     H*    CALLS TO:                                                  *
     H*      NONE                                                     *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 06/19/18 C:062 Ticket: 71628(111)                             *
     H*                Suppress SS#.                                  *
     H*                                                               *
     H* 04/24/18 C:062 Ticket: 70529(065)                             *
     H*                Don't show summary if no claims                *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 03/11/14 C:282 PROJECT 129WSC Priss 28                        *
     H*                Aligning the 'Heading for Legal Reserve'       *
     H*                with the other reserves.                       *
     H*                                                               *
     H* 11/25/13 C:282 PROJECT 129WSC                                 *
     H*                Replace legal literal with control field       *
     H*                for cltid 129 only.                            *
     H*                                                               *
     H* 10/31/13  C:283 Seq#: DR046 83327                             *
     H*                 Pass ##DIFL from file STCTP.                  *
     H*                                                               *
     H* 01/22/13  C:287 Seq#: SY266_OVR                               *
     H*                 Added DFTACTGRP and ACTGRP compile options    *
     H*                                                               *
     H* 08/31/12  C:282  Project: 139WQS                              *
     H*                  Carrier Policy Number will we displayed for  *
     H*                  all clients, for GIC client if group is      *
     H*                  between 901 through 907, do not print the    *
     H*                  Carrier Policy Number.                       *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  12/16/09  C:234 SUPPORT: 70898 WA046                         *
     H*                  Correction to Project 132WIT. Fix Issue with *
     H*                  Employee Experience Report. When pdf version *
     H*                  of report viewed from Visual DARTS Losses    *
     H*                  info were all blanks.                        *
     H*                                                               *
     H*  11/16/09  C:220 SUPPORT:70472                                *
     H*                  CALL DA700R WITH CORRECT PARAMETERS          *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT: WC1473                                *
     H*                Add 'EESPEC' and 'EEEXT' class listing on      *
     H*                report.                                        *
     H*                                                               *
     H*  02/26/09  C:197 PROJECT-WC1670                               *
     H*                  ADD NEW SORT SEQUENCES TO EMPLOYER EXPERIENCE*
     H*                                                               *
     H*  10/24/07  C:189 PROJECT-132WIT                               *
     H*                  Automate Audit Req/Loss Exp/Darts Aggregate  *
     H*                                                               *
     H* 03/01/07 C:61  PROJECT 132WHJ                                 *
     H*              - Print last 4 digits of SS# when selected.      *
     H*                                                               *
     H*  09/20/06  C:58  Project: 71WHI
     H*            Add parms Lost time flag, Total incurred and Gt/Lt
     H*                                                               *
     H*  06/24/05 C:156 PROJECT: 133WBW                               *
     H*                 Add First Report of Incedent date.            *
     H*                                                               *
     H*  06/21/05 C:58  PROJECT: WC1318                               *
     H*                - Add parameter 'Print SS#'
     H*
     H*  02/18/05 C:132 PROJECT: WC1221                               *
     H*                - Add new fields for Recoveries - Current      *
     H*                  period and To-Date fields                    *
     H*                - Calculate new recovery amounts for both      *
     H*                  current period and To-Date.                  *
     H*                - Update new fields in output file             *
     H*                                                               *
     H* 02/18/03 C:15  Support: WC045/35829                           *
     H*                Chgd pgm to show FYR on total page, if a FYR   *
     H*                was selected on the prompt screen.             *
     H*                                                               *
     H* 05/08/02 C:61  PROJECT WC577                                  *
     H*                Add policy effective date range to report      *
     H*                output.                                        *
     H*                                                               *
     H* 10/05/01 C:53  Support: WC094/27070                           *
     H*                Removed code to allow totals to print even if  *
     H*                no claims are printed for that employer.       *
     H*                                                               *
     H* 02/14/01 C:15  Support: WC094/26360                           *
     H*                The region desc is being right justified into  *
     H*                a smaller field for the report, resulting in   *
     H*                the first 9 characters being cutoff.  I chgd   *
     H*                the pgm to left justify the field.             *
     H*                                                               *
     H*  09/26/00 C:33 SEQ#: WA065/24293                              **
     H*                INCLUDE PAGE BREAK FOR EMPLOYERS WITH NO PREM  **
     H*                BUT HAVE CLAIMS                                *
     H*                                                               *
     H* 04/10/00 C:118 PROJECT: WC987                                 *
     H*                LOAD ALTERNATE COMPANY NAME FROM FUND MASTER   *
     H*                IF AVAILABLE.                                  *
     H*                                                               *
     H* 04/10/00 C:61  SUPPORT WA090 22460                            *
     H*              - Remove unnecessary print of employer header.   *
     H*                                                               *
     H*  12/15/99 C:33 Y2K#: 058                                      *
     H*                ENLARGE SIZE OF XXLOSS.                        *
     H*                                                               *
     H*  08/09/99 C:61 SEQ#: WA091/19181                              *
     H*              - Page numbering sequential if report run for    *
     H*                single employer.                               *
     H*              - Output employer info in header of summary page *
     H*                if run for single employer.                    *
     H*                                                               *
     H*  05/12/99 C:33 SEQ#: WC082/17995                              **
     H*                PAGE BREAK ON *ZERO PREMIUM IF THE ACCOUNT HAS **
     H*                BEEN AUDITED OR NOT BEEN ESTIMATED.            *
     H*                                                               *
     H*  02/08/99 C:33 SEQ#: WC072/15357                              **
     H*                DO NOT PAGE BREAK IF THE PREMIUM IS *ZERO      **
     H*                                                               *
     H*  12/07/98 C:61 SUPPORT WA091 15429                            *
     H*                Correct output of SSN.                         *
     H*                                                               *
     H*  10/22/98 C:33 SEQ#: WC071/13580                              *
     H*                CHANGE LAST EMPLOYER PROCESSING                *
     H*                                                               *
     H*  05/29/98 C:33 SEQ#: WC078/11786                              *
     H*               -USE *LIKE DEFN TO SET DIV SIZE                 *
     H*               -PRINT LAST DIV IF *ALL DIV SELECTED            *
     H*                                                               *
     H* 11/20/97 C:58 PROJECT WC755                                   *
     H*               V3 Implementation                               *
     H*                                                               *
     H* 02/06/98 C:78 SUPPORT# 10214 / WA091                          *
     H*               Policy year to print on the Employer Experience *
     H*               report if fyr was specified.                    *
     H*                                                               *
     H* PLEASE ADD TO TOP                                             *
     H*****************************************************************
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*     08   -      BATCH PROCESSING (FOR ROUTING PAGE)           *
     H*     09   -      DEMAND PROCESSING (FOR ROUTING PAGE)          *
     H*     11   -      CONDITION FIRST TIME THROUGH PROGRAM          *
     H*     22   -      CODITIONS FOR PAGE BREAK                      *
     H*     30   -      REGION SORT SELECTED                          *
     H*     31   -      AGENT ORDER SORT SELECTED                     *
     H*     32   -      AGENT ORDER SORT SELECTED (FOR ROUTING PAGE)  *
     H*     35   -      EMPLOYER HAS NO CLAIMS                        *
     H*     40   -      CHAINING TO AGENT / REGION FILES              *
     H*     41   -      CONDITIONS CATAGORY 1 FOR PRINTING            *
     H*     42   -      CONDITIONS CATAGORY 2 FOR PRINTING            *
     H*     52   -      CONTROL HEADER DESCRIPTION ON TOTALS          *
     H*     60   -      GENERAL CHAINING                              *
     H*     78   -      ALL FUND YEARS SELECTED                       *
     H*     79   -      ALL EMPLOYERS SELECTED                        *
     H*     80   -      ALL AGENTS SELECTED                           *
     H*     81   -      ALL DIVISIONS SELECTED                        *
     H*     83   -      PRINT EFF DT RANGE IN HEADER                  *
     H*     84   -      PRINT SS# on report.
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  FILE USAGE:                                                  *
     H*                                                               *
     H*     WXEEL  (PEE01): CO#/FND/EMP#/DIV/FYR/OPCL/SORT            *
     H*      OR WXEEL1 (PEE01): CO#/FND/RGN#/EMP#/DIV/FYR/OPCL/SORT   *
     H*      OR WXEEL3 (PEE01): CO#/FND/AGT#/EMP#/DIV/FYR/OPCL/SORT   *
     H*     WMEML (PEM01): CO#/FND/EMP#/DIV                           *
     H*     WMAGL (PAG01): CO#/FEIN/MOD                               *
     H*     WMAAL1 (PAA01):CO#/FND/AGT#                               *
     H*     SMCOL (PCO01): CO#                                        *
     H*     SMORL  (POR01) ORPNAM,ORFIL#                              *
     H*     WMUPL (PUP01): USER                                       *
     H*     WMFNL (PFN01): CO#/FND                                    *
     H*****************************************************************
     F*
     FWXEEL     IF   E           K DISK
     F* * * Work file built from W7191R
     F*
     FWMFNL     IF   E           K DISK
     F* * * Fund File
     F*
     FWMEML     IF   E           K DISK
     F* * * Employer File
     F*
     FWDELL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     F* * * Batch Routing File
     F*
     FSMCOL     IF   E           K DISK
     F* * * Company File
     F*
     FWMUPL     IF   E           K DISK
     F* * * User Parameter File
     F*
     FWMAAL1    IF   E           K DISK
     F* * * AGENT ASSIGNMENT FILE
     F*
     FWMAGL     IF   E           K DISK
     F* * * AGENT MASTER FILE
     F*
     FWMCXL     IF   E           K DISK
     F* * * Claim/DARTS Control
     F*
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOA)
     F                                     USROPN
     F* * * Output Report
      *
      ***** BEG ADD ******* 10/31/13 **********************************
     F* * * Remark Type Table File
     FSTCTL1    IF   E           K DISK
      ***** END ADD ******* 10/31/13 **********************************
      *

     D                 DS
     D  XXSS#                  1      9
     D  XXSS1                  1      3
     D  XXSS2                  4      5
     D  XXSS3                  6      9
     D                 DS
     D  QCKEY                  1     40
     D  EECO#                  1      3  0
     D  EEFND                  4      6  0
     D  @EEMP#                 7     15  0
     D  @EDIV                 16     20  0
     D*
     D* CLAIM EXAMINER HEADING
     D*
     D XXCAT2          C                   CONST('CATEGORY 2')
     D                 DS
     D  EEZIP                  1      9  0
     D  XXZIP1                 1      5  0
     D  XXZIP2                 1      9  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      ***  BEG   ADD  ***  08/31/12  *********************************
     d CltId           s              3
      *
     d RtvCltID        pr                  ExtPgm('RTVCLTID')
     d cltID                          3    const
      ***  END   ADD  ***  08/31/12  *********************************
      *** Beg Add ***  11/25/13  ******************************************************
     D charLGRH        s             10                                         LEGAL RESERVE HEAD
      *** End Add ***  11/25/13  ***********************************************HEADING
      *** Beg Add ***  03/11/14  ******************************************************
     D/copy sprString
      *** End Add ***  03/11/14  ******************************************************
      ***  Begin Add  ***  06/09/20  *******************
     D/copy sPrMask
      ***  End   Add  ***  06/09/20  *******************

     C*****************************************************************
     C*   LIKE DEFINES
     C*****************************************************************
     C*
     C     *LIKE         DEFINE    AAAGT#        XSAGT#
     C     *LIKE         DEFINE    EEAGT#        XXAGT#
     C     *LIKE         DEFINE    EECO#         XXCO#
     C     *LIKE         DEFINE    EEFND         XXFND
     C     *LIKE         DEFINE    EEFYR         XXFYR
     C     *LIKE         DEFINE    EEEMP#        XXEMP#
     C     *LIKE         DEFINE    EEDIV         XXDIV
     C     *LIKE         DEFINE    EEDIV         XYDIV
     C     *LIKE         DEFINE    EERGN#        XXRGN#
     C     *LIKE         DEFINE    AGNAME        XXAGNM
     C     *LIKE         DEFINE    CXCAT2        X1CAT2
     C     *LIKE         DEFINE    EERCMP        XXTINC
     C     *LIKE         DEFINE    *IN69         @CLSFL
     C     *LIKE         DEFINE    *IN69         HDRFLG
     C*
     C*****************************************************************
     C*                   KEY LISTING
     C*****************************************************************
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR2
     C*
     C*   KEY LIST FOR THE AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
     C*
     C*   KEY LIST FOR THE AGENT ASSIGNMENT FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      ***** BEG ADD ******* 10/31/13 **********************************
     C*   KEY LIST FOR THE Remark Type Table File
     C     STKY          KLIST
     C                   KFLD                    QQMARK
      ***** END ADD ******* 10/31/13 **********************************
     C*
     C*   CLAIMS/DARTS CONTROL KEY LIST
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *** Beg Add ***  11/25/13  ******************************************************
     C*   Claim/DARTS Control Definition File
     C*
     C     keyWMcxl      KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *** End Add ***  11/25/13  ******************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQDTE             8
     C                   PARM                    QQFYR             3
     C                   PARM                    QQSELO            1
     C                   PARM                    QQSELC            1
     C                   PARM                    QQMNTH            3
     C                   PARM                    QQSORT            1
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQSRGN            1
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQAGT#            5
     C                   PARM                    QQSAGT            1
     C                   PARM                    QQEMPC            1
     C                   PARM                    QQPRSS            1
     C                   PARM                    QQLTFL            1
     C                   PARM                    QQTINC           13
     C                   PARM                    QQGTLT            1
     C                   PARM                    QQMARK            5
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQDTE         XXDTE2            8 0
     C                   MOVE      QQMODE        XXMODE            1
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C*
     C     DTCYM1        PLIST
     C                   PARM                    SRDTI1
     C*                    PARM '*CYMD'   SRFFMT
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDYY  '     SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C     QQMARK        IFNE      *BLANKS
     C                   MOVE      *ON           *IN69
     C                   ENDIF
     C*
     C                   MOVE      QQTINC        XXINCR           13 2
     C                   Z-ADD     XXINCR        XXTINC
     C* FIRST CYCLE PROCESSING
     C*
     C     *IN69         IFEQ      *ON
     C                   OPEN      QSYSPRT1
     C                   ENDIF
     C  N11              EXSR      SR1ST
     C*
     C*
     C* START TO READ THE WORK FILE
     C*
     C                   READ      PEE01                                  60
     C     *IN60         DOWEQ     '0'
     C     *IN69         IFEQ      '1'
     C*
     C     *IN88         IFEQ      '0'
     C                   Z-ADD     EEEMP#        @EEMP#
     C                   Z-ADD     EEDIV         @EDIV
     C                   ENDIF
     C                   ENDIF
     C***  BEG   ADD  ***  08/31/12  *********************************
      /free

       // Retrieve Client ID
       RtvCltID(CltId);

       If (CltId = '139' And EEfnd >= 901 And EEfnd <= 907) Or
          (CltId <> '139');
          *In55 = *On;
       Else;
          *In55 = *Off;
       EndIf;
      /end-free
     C***  END   ADD  ***  08/31/12  *********************************
     C*
     C*    FIRST SAVE
     C     *IN70         IFEQ      '0'
     C   78              MOVE      EEFYR         XXFYR
     C                   MOVE      EEFND         XXFND
     C   78              MOVE      EEFYR         XXFYR2
     C   79              MOVE      EEEMP#        XXEMP#
     C   81              MOVE      EEDIV         XXDIV
     C   80              MOVE      EEAGT#        XXAGT#
     C                   MOVE      EERGN#        XXRGN#
     C                   MOVEL     EERDES        RGDESC
     C                   MOVE      '1'           *IN70
     C*
     C*  GET AGENT NAME
     C     *IN31         IFEQ      '1'
     C     AAKY          CHAIN     PAA01                              40
     C     *IN40         IFEQ      '0'
     C     AGKY          CHAIN     PAG01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     AGNAME        XXAGNM
     C                   ELSE
     C                   MOVE      *BLANK        XXAGNM
     C                   END
     C                   END
     C                   END
     C*
     C*  GET EMPLOYER NAME
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        ENAME            40
     C                   ELSE
     C                   MOVE      *BLANK        ENAME
     C                   END
     C*
     C*
     C     *IN81         IFEQ      *ON
     C     XXDIV         OREQ      XYDIV
     C*
     C                   EXSR      SRPRM
     C*
     C                   ENDIF                                                  *IN81=*ON
     C*
     C     PGBRK         IFEQ      'Y'
     C                   MOVE      '1'           *IN22
     C                   ENDIF                                                  PGBRK=Y
     C*
     C                   END
     C*****************************************************************
     C*
     C*   NEW AGENT OR REGION
     C     *IN30         IFEQ      '1'
     C     *IN31         OREQ      '1'
     C     EERGN#        IFNE      XXRGN#
     C     *IN30         ANDEQ     '1'
     C                   EXSR      SR3TH                                        YR  SBR
     C*
     C                   EXSR      SR4TH                                        DIV SBR
     C                   EXSR      SR5TH                                        EMP SBR
     C                   EXSR      SR6TH                                        AG/RG SBR
     C                   MOVE      EEFYR         XXFYR2
     C                   MOVE      EEFYR         XXFYR
     C                   MOVE      EEDIV         XXDIV
     C                   MOVE      EEEMP#        XXEMP#
     C                   MOVE      EERGN#        XXRGN#
     C                   MOVE      EEAGT#        XXAGT#
     C                   MOVEL     EERDES        RGDESC           31
     C*
     C*  GET EMPLOYER NAME
     C     EMKY          CHAIN     PEM01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     EMNAME        ENAME            40
     C                   ELSE
     C                   MOVE      *BLANK        ENAME
     C                   END
     C*
     C     *IN81         IFEQ      *ON
     C     XXDIV         OREQ      XYDIV
     C*
     C                   EXSR      SRPRM
     C*
     C                   ENDIF                                                  *IN81=*ON
     C*
     C     PGBRK         IFEQ      'Y'
     C   82              Z-ADD     *ZERO         PAGE
     C                   MOVE      '1'           *IN22
     C                   ENDIF                                                  PGBRK=Y
     C*
     C                   END
     C*
     C     EEAGT#        IFNE      XXAGT#
     C     *IN31         ANDEQ     '1'
     C                   EXSR      SR3TH                                        YR  SBR
     C                   EXSR      SR4TH                                        DIV SBR
     C                   EXSR      SR5TH                                        EMP SBR
     C                   EXSR      SR6TH                                        AG/RG SBR
     C                   MOVE      EEFYR         XXFYR2
     C                   MOVE      EEFYR         XXFYR
     C                   MOVE      EEDIV         XXDIV
     C                   MOVE      EEEMP#        XXEMP#
     C                   MOVE      EERGN#        XXRGN#
     C                   MOVE      EEAGT#        XXAGT#
     C*
     C*  GET AGENT NAME
     C     AAKY          CHAIN     PAA01                              40
     C     *IN40         IFEQ      '0'
     C     AGKY          CHAIN     PAG01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     AGNAME        XXAGNM
     C                   ELSE
     C                   MOVE      *BLANK        XXAGNM
     C                   END
     C                   END
     C*
     C*  GET EMPLOYER NAME
     C     EMKY          CHAIN     PEM01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     EMNAME        ENAME            40
     C                   ELSE
     C                   MOVE      *BLANK        ENAME
     C                   END
     C*
     C     *IN81         IFEQ      *ON
     C     XXDIV         OREQ      XYDIV
     C*
     C                   EXSR      SRPRM
     C*
     C                   ENDIF                                                  *IN81=*ON
     C*
     C     PGBRK         IFEQ      'Y'
     C   82              Z-ADD     *ZERO         PAGE
     C                   MOVE      '1'           *IN22
     C                   ENDIF                                                  PGBRK=Y
     C*
     C                   END
     C                   END
     C*
     C**  NEW EMPLOYER - WHEN ALL EMPLOYER WAS SELECTED
     C     EEEMP#        IFNE      XXEMP#
     C     *IN79         ANDEQ     '1'
     C                   EXSR      SR3TH                                        YR  SBR
     C                   EXSR      SR4TH                                        DIV SBR
     C                   EXSR      SR5TH                                        EMP SBR
     C                   MOVE      EEFYR         XXFYR2
     C                   MOVE      EEFYR         XXFYR
     C                   MOVE      EEDIV         XXDIV
     C                   MOVE      EEEMP#        XXEMP#
     C*
     C*  GET EMPLOYER NAME
     C     EMKY          CHAIN     PEM01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     EMNAME        ENAME            40
     C                   ELSE
     C                   MOVE      *BLANK        ENAME
     C                   END
     C   82              Z-ADD     *ZERO         PAGE
     C*
     C     *IN81         IFEQ      *ON
     C     XXDIV         OREQ      XYDIV
     C*
     C                   EXSR      SRPRM
     C*
     C                   ENDIF                                                   *IN81=*ON
     C*
     C     PGBRK         IFEQ      'Y'
     C                   MOVE      '1'           *IN22
     C                   ENDIF                                                  PGBRK=Y
     C*
     C                   END
     C*
     C**  NEW DIVISION
     C     EEDIV         IFNE      XXDIV
     C     EEEMP#        ANDEQ     XXEMP#
     C     XXOP3         IFNE      *ZERO
     C     XXCS3         ORNE      *ZERO
     C     XDAMT         ORNE      *ZERO
     C                   EXSR      SR3TH                                        YR  SBR
     C*
     C     *IN81         IFEQ      *ON
     C     XXDIV         OREQ      XYDIV
     C*
     C                   EXSR      SR4TH                                        DIV SBR
     C*
     C                   ENDIF                                                  *IN81=*ON
     C*
     C                   END
     C                   MOVE      EEFYR         XXFYR2
     C                   MOVE      EEFYR         XXFYR
     C                   MOVE      EEDIV         XXDIV
     C*
     C*  GET EMPLOYER NAME
     C     EMKY          CHAIN     PEM01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     EMNAME        ENAME            40
     C                   ELSE
     C                   MOVE      *BLANK        ENAME
     C                   END
     C*
     C*
     C     *IN81         IFEQ      *ON
     C     XXDIV         OREQ      XYDIV
     C*
     C                   EXSR      SRPRM
     C*
     C                   ENDIF                                                  *IN81=*ON
     C*
     C     PGBRK         IFEQ      'Y'
     C                   MOVE      '1'           *IN22
     C   82              Z-ADD     *ZERO         PAGE
     C                   ENDIF                                                  PGBRK=Y
     C*
     C                   END
     C*
     C**  NEW FUND YEAR - WHEN ALL FUND YEAR WAS SELECTED
     C*
     C     EEFYR         IFNE      XXFYR
     C     *IN78         ANDEQ     '1'
     C     XXOP2         IFNE      *ZERO
     C     XXCS2         ORNE      *ZERO
     C     XYAMT         ORNE      *ZERO
     C                   EXSR      SR3TH                                        YR  SBR
     C                   END
     C                   MOVE      EEFYR         XXFYR
     C                   MOVE      EEFYR         XXFYR2
     C*
     C     *IN81         IFEQ      *ON
     C     XXDIV         OREQ      XYDIV
     C*
     C                   EXSR      SRPRM
     C*
     C                   ENDIF                                                  *IN81=*ON
     C*
     C     PGBRK         IFEQ      'Y'
     C   82              Z-ADD     *ZERO         PAGE
     C                   MOVE      '1'           *IN22
     C                   ENDIF                                                  PGBRK=Y
     C*
     C                   END
     C*
     C     *IN69         IFEQ      *ON
     C     EEEMP#        IFNE      @EEMP#
     C     EEDIV         ORNE      @EDIV
     C     *IN60         OREQ      '1'
     C*
     C     HDRFLG        IFEQ      '0'
     C                   EXCEPT    HEAD#
     C                   EXCEPT    H1ADE#
     C                   EXCEPT    H2ADE#
     C                   ENDIF
     C                   CLOSE     QSYSPRT1
     C*
     C                   Z-ADD     *ZEROS        PAGE1
     C*
     C                   MOVE      '1'           @CLSFL
     C*
     C                   MOVE      EECO#         #1CO#             3
     C                   MOVE      EEFND         #1FND             3
     C*
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       ##FLR            80
     C                   PARM      *BLANKS       ##DCID           12
     C                   PARM      'EX'          ##PRE             2
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM      'P'           ##RTCD            1
     C*
     C                   CALL      'CRTRSLV'
     C                   PARM                    ##FLR            80
     C                   PARM                    ##DCID           12
     C                   PARM      'QSYSPRT1'    SPLF             10
     C                   PARM                    RTCD              1
     C*
     C                   CLEAR                   SRDTO
     C                   CLEAR                   SRERR
     C                   Z-ADD     DSDATE        SRDTI1            8 0
     C                   CALL      'SRCVTDAT'    DTCYM1
     C                   MOVEL     SRDTO         QQDTE
      ***** BEG ADD ******* 10/31/13 **********************************
     C                   EVAL      ##DIFL = *BLANKS
     C     STKY          CHAIN     PCT01
     C                   IF        %FOUND(STCTL1)
     C                   EVAL      ##DIFL = CTFL01
     C                   ENDIF
      ***** END ADD ******* 10/31/13 **********************************
    C*
     C                   CALL      'DA700R'
     C                   PARM                    QCKEY            40
     C                   PARM                    ##COMM          256
     C                   PARM                    QQDTE             8
     C                   PARM                    ##DCID           12
     C                   PARM                    ##PNAM           10
     C                   PARM      '000'         ##SECL            3
     C                   PARM      UPUSR         ##USER           10
     C                   PARM      Q1PGM         ##PGM            10
     C                   PARM                    ##FLR            80
     C***** BEG CHG ******* 10/31/13 **********************************
     C*************      PARM      'Y'           ##DIFL            1
     C                   PARM                    ##DIFL            1
     C***** END CHG ******* 10/31/13 **********************************
     C                   PARM      Q1PGM         ##IPGM           10
     C                   PARM      ' '           ##FL01            1
     C                   PARM      ' '           ##FL02            1
     C                   PARM      QQMARK        ##RMCD            5
     C                   PARM      *BLANKS       ##SEQ#            5
     C                   PARM      'N'           Q#SBYL            1
     C                   PARM      *BLANKS       Q#COM2           19
     C                   PARM      *BLANKS       Q#APID            2
     C                   PARM      *BLANKS       Q#IDOC           12
     C                   PARM      *BLANKS       Q#IQUE           10
     C                   PARM      'O'           Q#IMGF            1
     C                   PARM      *BLANKS       Q#IPTH           10
     C                   PARM      *BLANKS       Q#PIDX            5
     C                   PARM      *BLANK        Q#QPTY            1
     C                   PARM      *BLANKS       Q#DCTP            5
     C                   PARM      *BLANKS       Q#CSID            2
     C                   PARM      *BLANKS       Q#CODE            2
     C*
     C                   Z-ADD     EEEMP#        @EEMP#
     C                   Z-ADD     EEDIV         @EDIV
     C*
     C                   OPEN      QSYSPRT1
     C                   MOVE      '0'           HDRFLG
     C                   ENDIF
     C                   ENDIF
     C*   DETAIL
     C                   MOVEL     EECLMN        CLNAME           28
     C                   Z-ADD     EECLDT        EEMDY             8 0
     C*
     C*  IF CATEGORY FIELDS ARE BLANK, DO NOT PRINT
     C     EECTG1        IFNE      *BLANK
     C                   MOVE      '1'           *IN41
     C                   ELSE
     C                   MOVE      '0'           *IN41
     C                   END
     C*
     C     EECTG2        IFNE      *BLANK
     C                   MOVE      '1'           *IN42
     C                   ELSE
     C                   MOVE      '0'           *IN42
     C                   END
     C*
     C     EECASE        IFNE      *ZEROS
     C     EEOPCL        IFEQ      'C'
     C                   ADD       1             XXCS1             5 0          CLOSE
     C                   ADD       1             XXCS2             5 0
     C                   ADD       1             XXCS3             5 0
     C                   ADD       1             XXCS4             5 0
     C                   ADD       1             XXCS5             5 0
     C                   ADD       1             XXCS6             5 0
     C                   ELSE
     C                   ADD       1             XXOP1             5 0          OPEN
     C                   ADD       1             XXOP2             5 0
     C                   ADD       1             XXOP3             5 0
     C                   ADD       1             XXOP4             5 0
     C                   ADD       1             XXOP5             5 0
     C                   ADD       1             XXOP6             5 0
     C                   END
     C*
     C                   ADD       EEPCMP        EEPTOT            9 2          PMT T PERIOD
     C                   ADD       EEPMED        EEPTOT
     C                   ADD       EEPLEG        EEPTOT
     C                   ADD       EEPOTH        EEPTOT
     C*
     C                   ADD       EERCMP        EERTOT            9 2          RSV CHG T PE
     C                   ADD       EERMED        EERTOT
     C                   ADD       EERLEG        EERTOT
     C                   ADD       EEROTH        EERTOT
     C*
     C                   ADD       EEDCMP        EEDTOT            9 2          PMT TO DATE
     C                   ADD       EEDMED        EEDTOT
     C                   ADD       EEDLEG        EEDTOT
     C                   ADD       EEDOTH        EEDTOT
     C*
     C                   ADD       EEOCMP        EEOTOT            9 2          OUTS RSV
     C                   ADD       EEOMED        EEOTOT
     C                   ADD       EEOLEG        EEOTOT
     C                   ADD       EEOOTH        EEOTOT
     C*
     C     EEDCMP        ADD       EEOCMP        EEICMP            9 2          TOT INC
     C     EEDMED        ADD       EEOMED        EEIMED            9 2
     C     EEDLEG        ADD       EEOLEG        EEILEG            9 2
     C     EEDOTH        ADD       EEOOTH        EEIOTH            9 2
     C     EEDTOT        ADD       EEOTOT        EEITOT            9 2
     C*
     C* Create totals for recovery fields
     C                   ADD       EEGCMP        EEGTOT            9 2          GROSS PD-TO-DT
     C                   ADD       EEGMED        EEGTOT
     C                   ADD       EEGLEG        EEGTOT
     C                   ADD       EEGEXP        EEGTOT
     C*
     C                   ADD       EEGPCO        EEGPOT            9 2          GROSS-CURR PERD
     C                   ADD       EEGPME        EEGPOT
     C                   ADD       EEGPLE        EEGPOT
     C                   ADD       EEGPEX        EEGPOT
     C*
     C                   ADD       EERTCO        EETROT            9 2          RECOVERY TO-DT
     C                   ADD       EERTME        EETROT
     C                   ADD       EERTLE        EETROT
     C                   ADD       EERTEX        EETROT
     C*
     C                   ADD       EERCCO        EERCOT            9 2          RECOVRY CURR PD
     C                   ADD       EERCME        EERCOT
     C                   ADD       EERCLE        EERCOT
     C                   ADD       EERCEX        EERCOT
     C*
     C*  ACCUMULATE TOTALS FOR YR
     C*
     C                   ADD       EEGCMP        YRGCMP           11 2          GROSS PD-TO-DT
     C                   ADD       EEGMED        YRGMED           11 2
     C                   ADD       EEGLEG        YRGLEG           11 2
     C                   ADD       EEGEXP        YRGEXP           11 2
     C                   ADD       EEGTOT        YRGTOT           11 2
     C*
     C                   ADD       EEGPCO        YRGPCO           11 2          GROSS-CURR PERD
     C                   ADD       EEGPME        YRGPME           11 2
     C                   ADD       EEGPLE        YRGPLE           11 2
     C                   ADD       EEGPEX        YRGPEX           11 2
     C                   ADD       EEGPOT        YRGPOT           11 2
     C*
     C                   ADD       EERTCO        YRRTCO           11 2          RECOVERY TO-DTE
     C                   ADD       EERTME        YRRTME           11 2
     C                   ADD       EERTLE        YRRTLE           11 2
     C                   ADD       EERTEX        YRRTEX           11 2
     C                   ADD       EETROT        YRTROT           11 2
     C*
     C                   ADD       EERCCO        YRRCCO           11 2          RCRVY-CURR PERD
     C                   ADD       EERCME        YRRCME           11 2
     C                   ADD       EERCLE        YRRCLE           11 2
     C                   ADD       EERCEX        YRRCEX           11 2
     C                   ADD       EERCOT        YRRCOT           11 2
     C*
     C*  ACCUMULATE TOTALS FOR DIVISION
     C*
     C                   ADD       EEGCMP        DVGCMP           11 2          GROSS PD-TO-DT
     C                   ADD       EEGMED        DVGMED           11 2
     C                   ADD       EEGLEG        DVGLEG           11 2
     C                   ADD       EEGEXP        DVGEXP           11 2
     C                   ADD       EEGTOT        DVGTOT           11 2
     C*
     C                   ADD       EEGPCO        DVGPCO           11 2          GROSS-CURR PERD
     C                   ADD       EEGPME        DVGPME           11 2
     C                   ADD       EEGPLE        DVGPLE           11 2
     C                   ADD       EEGPEX        DVGPEX           11 2
     C                   ADD       EEGPOT        DVGPOT           11 2
     C*
     C                   ADD       EERTCO        DVRTCO           11 2          RECOVERY TO-DTE
     C                   ADD       EERTME        DVRTME           11 2
     C                   ADD       EERTLE        DVRTLE           11 2
     C                   ADD       EERTEX        DVRTEX           11 2
     C                   ADD       EETROT        DVTROT           11 2
     C*
     C                   ADD       EERCCO        DVRCCO           11 2          RCRVY-CURR PERD
     C                   ADD       EERCME        DVRCME           11 2
     C                   ADD       EERCLE        DVRCLE           11 2
     C                   ADD       EERCEX        DVRCEX           11 2
     C                   ADD       EERCOT        DVRCOT           11 2
     C*
     C*  ACCUMULATE TOTALS FOR EMPLOYER
     C*
     C                   ADD       EEGCMP        EMGCMP           11 2          GROSS PD-TO-DT
     C                   ADD       EEGMED        EMGMED           11 2
     C                   ADD       EEGLEG        EMGLEG           11 2
     C                   ADD       EEGEXP        EMGEXP           11 2
     C                   ADD       EEGTOT        EMGTOT           11 2
     C*
     C                   ADD       EEGPCO        EMGPCO           11 2          GROSS-CURR PERD
     C                   ADD       EEGPME        EMGPME           11 2
     C                   ADD       EEGPLE        EMGPLE           11 2
     C                   ADD       EEGPEX        EMGPEX           11 2
     C                   ADD       EEGPOT        EMGPOT           11 2
     C*
     C                   ADD       EERTCO        EMRTCO           11 2          RECOVERY TO-DTE
     C                   ADD       EERTME        EMRTME           11 2
     C                   ADD       EERTLE        EMRTLE           11 2
     C                   ADD       EERTEX        EMRTEX           11 2
     C                   ADD       EETROT        EMTROT           11 2
     C*
     C                   ADD       EERCCO        EMRCCO           11 2          RCRVY-CURR PERD
     C                   ADD       EERCME        EMRCME           11 2
     C                   ADD       EERCLE        EMRCLE           11 2
     C                   ADD       EERCEX        EMRCEX           11 2
     C                   ADD       EERCOT        EMRCOT           11 2
     C*
     C*  ACCUMULATE TOTALS FOR AGENT OR REGION
     C*
     C                   ADD       EEGCMP        XGGCMP           11 2          GROSS PD-TO-DT
     C                   ADD       EEGMED        XGGMED           11 2
     C                   ADD       EEGLEG        XGGLEG           11 2
     C                   ADD       EEGEXP        XGGEXP           11 2
     C                   ADD       EEGTOT        XGGTOT           11 2
     C*
     C                   ADD       EEGPCO        XGGPCO           11 2          GROSS-CURR PERD
     C                   ADD       EEGPME        XGGPME           11 2
     C                   ADD       EEGPLE        XGGPLE           11 2
     C                   ADD       EEGPEX        XGGPEX           11 2
     C                   ADD       EEGPOT        XGGPOT           11 2
     C*
     C                   ADD       EERTCO        XGRTCO           11 2          RECOVERY TO-DTE
     C                   ADD       EERTME        XGRTME           11 2
     C                   ADD       EERTLE        XGRTLE           11 2
     C                   ADD       EERTEX        XGRTEX           11 2
     C                   ADD       EETROT        XGTROT           11 2
     C*
     C                   ADD       EERCCO        XGRCCO           11 2          RCRVY-CURR PERD
     C                   ADD       EERCME        XGRCME           11 2
     C                   ADD       EERCLE        XGRCLE           11 2
     C                   ADD       EERCEX        XGRCEX           11 2
     C                   ADD       EERCOT        XGRCOT           11 2
     C*
     C*  ACCUMULATE TOTALS FOR GRAND TOTALS
     C*
     C                   ADD       EEGCMP        LRGCMP           11 2          GROSS PD-TO-DT
     C                   ADD       EEGMED        LRGMED           11 2
     C                   ADD       EEGLEG        LRGLEG           11 2
     C                   ADD       EEGEXP        LRGEXP           11 2
     C                   ADD       EEGTOT        LRGTOT           11 2
     C*
     C                   ADD       EEGPCO        LRGPCO           11 2          GROSS-CURR PERD
     C                   ADD       EEGPME        LRGPME           11 2
     C                   ADD       EEGPLE        LRGPLE           11 2
     C                   ADD       EEGPEX        LRGPEX           11 2
     C                   ADD       EEGPOT        LRGPOT           11 2
     C*
     C                   ADD       EERTCO        LRRTCO           11 2          RECOVERY TO-DTE
     C                   ADD       EERTME        LRRTME           11 2
     C                   ADD       EERTLE        LRRTLE           11 2
     C                   ADD       EERTEX        LRRTEX           11 2
     C                   ADD       EETROT        LRTROT           11 2
     C*
     C                   ADD       EERCCO        LRRCCO           11 2          RCRVY-CURR PERD
     C                   ADD       EERCME        LRRCME           11 2
     C                   ADD       EERCLE        LRRCLE           11 2
     C                   ADD       EERCEX        LRRCEX           11 2
     C                   ADD       EERCOT        LRRCOT           11 2
     C*
     C*
     C*  ACCUMULATE TOTALS FOR YR
     C*
     C                   ADD       EEPCMP        YRPCMP           11 2
     C                   ADD       EEPMED        YRPMED           11 2
     C                   ADD       EEPLEG        YRPLEG           11 2
     C                   ADD       EEPOTH        YRPOTH           11 2
     C                   ADD       EEPTOT        YRPTOT           11 2
     C*
     C                   ADD       EERCMP        YRRCMP           11 2
     C                   ADD       EERMED        YRRMED           11 2
     C                   ADD       EERLEG        YRRLEG           11 2
     C                   ADD       EEROTH        YRROTH           11 2
     C                   ADD       EERTOT        YRRTOT           11 2
     C*
     C                   ADD       EEDCMP        YRDCMP           11 2
     C                   ADD       EEDMED        YRDMED           11 2
     C                   ADD       EEDLEG        YRDLEG           11 2
     C                   ADD       EEDOTH        YRDOTH           11 2
     C                   ADD       EEDTOT        YRDTOT           11 2
     C*
     C                   ADD       EEOCMP        YROCMP           11 2
     C                   ADD       EEOMED        YROMED           11 2
     C                   ADD       EEOLEG        YROLEG           11 2
     C                   ADD       EEOOTH        YROOTH           11 2
     C                   ADD       EEOTOT        YROTOT           11 2
     C*
     C                   ADD       EEICMP        YRICMP           11 2
     C                   ADD       EEIMED        YRIMED           11 2
     C                   ADD       EEILEG        YRILEG           11 2
     C                   ADD       EEIOTH        YRIOTH           11 2
     C                   ADD       EEITOT        YRITOT           11 2
     C*
     C*  ACCUMULATE TOTALS FOR DIVISION
     C*
     C                   ADD       EEPCMP        DVPCMP           11 2
     C                   ADD       EEPMED        DVPMED           11 2
     C                   ADD       EEPLEG        DVPLEG           11 2
     C                   ADD       EEPOTH        DVPOTH           11 2
     C                   ADD       EEPTOT        DVPTOT           11 2
     C*
     C                   ADD       EERCMP        DVRCMP           11 2
     C                   ADD       EERMED        DVRMED           11 2
     C                   ADD       EERLEG        DVRLEG           11 2
     C                   ADD       EEROTH        DVROTH           11 2
     C                   ADD       EERTOT        DVRTOT           11 2
     C*
     C                   ADD       EEDCMP        DVDCMP           11 2
     C                   ADD       EEDMED        DVDMED           11 2
     C                   ADD       EEDLEG        DVDLEG           11 2
     C                   ADD       EEDOTH        DVDOTH           11 2
     C                   ADD       EEDTOT        DVDTOT           11 2
     C*
     C                   ADD       EEOCMP        DVOCMP           11 2
     C                   ADD       EEOMED        DVOMED           11 2
     C                   ADD       EEOLEG        DVOLEG           11 2
     C                   ADD       EEOOTH        DVOOTH           11 2
     C                   ADD       EEOTOT        DVOTOT           11 2
     C*
     C                   ADD       EEICMP        DVICMP           11 2
     C                   ADD       EEIMED        DVIMED           11 2
     C                   ADD       EEILEG        DVILEG           11 2
     C                   ADD       EEIOTH        DVIOTH           11 2
     C                   ADD       EEITOT        DVITOT           11 2
     C*
     C*  ACCUMULATE TOTALS FOR EMPLOYER
     C*
     C                   ADD       EEPCMP        EMPCMP           11 2
     C                   ADD       EEPMED        EMPMED           11 2
     C                   ADD       EEPLEG        EMPLEG           11 2
     C                   ADD       EEPOTH        EMPOTH           11 2
     C                   ADD       EEPTOT        EMPTOT           11 2
     C*
     C                   ADD       EERCMP        EMRCMP           11 2
     C                   ADD       EERMED        EMRMED           11 2
     C                   ADD       EERLEG        EMRLEG           11 2
     C                   ADD       EEROTH        EMROTH           11 2
     C                   ADD       EERTOT        EMRTOT           11 2
     C*
     C                   ADD       EEDCMP        EMDCMP           11 2
     C                   ADD       EEDMED        EMDMED           11 2
     C                   ADD       EEDLEG        EMDLEG           11 2
     C                   ADD       EEDOTH        EMDOTH           11 2
     C                   ADD       EEDTOT        EMDTOT           11 2
     C*
     C                   ADD       EEOCMP        EMOCMP           11 2
     C                   ADD       EEOMED        EMOMED           11 2
     C                   ADD       EEOLEG        EMOLEG           11 2
     C                   ADD       EEOOTH        EMOOTH           11 2
     C                   ADD       EEOTOT        EMOTOT           11 2
     C*
     C                   ADD       EEICMP        EMICMP           11 2
     C                   ADD       EEIMED        EMIMED           11 2
     C                   ADD       EEILEG        EMILEG           11 2
     C                   ADD       EEIOTH        EMIOTH           11 2
     C                   ADD       EEITOT        EMITOT           11 2
     C*
     C*  ACCUMULATE TOTALS FOR AGENT OR REGION
     C*
     C                   ADD       EEPCMP        XGPCMP           11 2
     C                   ADD       EEPMED        XGPMED           11 2
     C                   ADD       EEPLEG        XGPLEG           11 2
     C                   ADD       EEPOTH        XGPOTH           11 2
     C                   ADD       EEPTOT        XGPTOT           11 2
     C*
     C                   ADD       EERCMP        XGRCMP           11 2
     C                   ADD       EERMED        XGRMED           11 2
     C                   ADD       EERLEG        XGRLEG           11 2
     C                   ADD       EEROTH        XGROTH           11 2
     C                   ADD       EERTOT        XGRTOT           11 2
     C*
     C                   ADD       EEDCMP        XGDCMP           11 2
     C                   ADD       EEDMED        XGDMED           11 2
     C                   ADD       EEDLEG        XGDLEG           11 2
     C                   ADD       EEDOTH        XGDOTH           11 2
     C                   ADD       EEDTOT        XGDTOT           11 2
     C*
     C                   ADD       EEOCMP        XGOCMP           11 2
     C                   ADD       EEOMED        XGOMED           11 2
     C                   ADD       EEOLEG        XGOLEG           11 2
     C                   ADD       EEOOTH        XGOOTH           11 2
     C                   ADD       EEOTOT        XGOTOT           11 2
     C*
     C                   ADD       EEICMP        XGICMP           11 2
     C                   ADD       EEIMED        XGIMED           11 2
     C                   ADD       EEILEG        XGILEG           11 2
     C                   ADD       EEIOTH        XGIOTH           11 2
     C                   ADD       EEITOT        XGITOT           11 2
     C*
     C*  ACCUMULATE TOTALS FOR GRAND TOTALS
     C*
     C                   ADD       EEPCMP        LRPCMP           11 2
     C                   ADD       EEPMED        LRPMED           11 2
     C                   ADD       EEPLEG        LRPLEG           11 2
     C                   ADD       EEPOTH        LRPOTH           11 2
     C                   ADD       EEPTOT        LRPTOT           11 2
     C*
     C                   ADD       EERCMP        LRRCMP           11 2
     C                   ADD       EERMED        LRRMED           11 2
     C                   ADD       EERLEG        LRRLEG           11 2
     C                   ADD       EEROTH        LRROTH           11 2
     C                   ADD       EERTOT        LRRTOT           11 2
     C*
     C                   ADD       EEDCMP        LRDCMP           11 2
     C                   ADD       EEDMED        LRDMED           11 2
     C                   ADD       EEDLEG        LRDLEG           11 2
     C                   ADD       EEDOTH        LRDOTH           11 2
     C                   ADD       EEDTOT        LRDTOT           11 2
     C*
     C                   ADD       EEOCMP        LROCMP           11 2
     C                   ADD       EEOMED        LROMED           11 2
     C                   ADD       EEOLEG        LROLEG           11 2
     C                   ADD       EEOOTH        LROOTH           11 2
     C                   ADD       EEOTOT        LROTOT           11 2
     C*
     C                   ADD       EEICMP        LRICMP           11 2
     C                   ADD       EEIMED        LRIMED           11 2
     C                   ADD       EEILEG        LRILEG           11 2
     C                   ADD       EEIOTH        LRIOTH           11 2
     C                   ADD       EEITOT        LRITOT           11 2
     C                   END
     C*
     C     EECASE        COMP      *ZEROS                                 35
     C*
     C** PRINT EMPLOYERS WITH CLAIMS, ONLY
     C*
     C     QQEMPC        IFEQ      'N'
     C*
     C*** Begin Add *** 04/24/18 ************************************
     C*****EECASE        IFNE      *ZEROS
     C                   If        EEcase <> 0 And ((qqSelO = 'Y'
     C                             And EEopCl = ' ') Or (qqSelC = 'Y'
     C                             And EEopCl = 'C'))
     C*** End   Add *** 04/24/18 ************************************
     C*
     C     *IN22         IFEQ      '1'
     C   82              Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C     *IN69         IFEQ      '1'                                          DART <> BLANKS
     C                   EXCEPT    HEAD#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C     *IN30         IFEQ      '1'
     C                   EXCEPT    HEADR
     C     *IN69         IFEQ      '1'                                          DART <> BLANKS
     C                   EXCEPT    HEADR#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   ELSE
     C     *IN31         IFEQ      '1'
     C                   EXCEPT    HEADA
     C     *IN69         IFEQ      '1'                                          DART <> BLANKS
     C                   EXCEPT    HEADA#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   END
     C                   END
      *
     C                   EXSR      SREFDT
     C                   EXCEPT    HEADE
     C                   EXCEPT    HEAD1
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADE#
     C                   EXCEPT    HEAD1#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   MOVE      '0'           *IN22
     C                   END
     C*
      *
     C     QQPRSS        IFEQ      '4'
     C                   MOVE      EESS#         XXSS4             4
     C     'XXXXX'       CAT       XXSS4         XXSS#
      *
     C                   ELSE
      *
     C*** Begin Add *** 06/19/18 ****************************
     C                   If        qqPrSS = 'Y'
     C*** End   Add *** 06/19/18 ****************************
     C                   MOVE      EESS#         XXSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   Eval      XXSS#=MaskData('SSN':'XXSS#':XXSS#)
     C***  End Add  ***  06/09/20  *******************************
     C*** Begin Add *** 06/19/18 ****************************
     C                   Else
     C                   MOVE      'XXXXXXXXX'   XXSS#
     C                   EndIf
     C*** End   Add *** 06/19/18 ****************************
     C                   ENDIF
     C*
     C                   Z-ADD     EEACDT        XXACDT
     C                   Z-ADD     EERPDT        XXRPDT
     C*
     C     EESORT        IFNE      *HIVAL
     C     EEACD2        COMP      *BLANKS                                71
     C                   EXCEPT    DETAIL
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    DETAI#
     C                   END
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEAD#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C     *IN30         IFEQ      '1'
     C                   EXCEPT    HEADR
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADR#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   ELSE
     C     *IN31         IFEQ      '1'
     C                   EXCEPT    HEADA
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADA#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   END
     C                   END
      *
     C                   EXSR      SREFDT
     C                   EXCEPT    HEADE
     C                   EXCEPT    HEAD1
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADE#
     C                   EXCEPT    HEAD1#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   MOVE      '0'           *INOF
     C                   MOVE      '0'           *INOA
     C                   END
     C                   END
     C                   END
     C*
     C** PRINT EMPLOYERS WITH AND W/O CLAIMS
     C*
     C                   ELSE
     C*
     C     *IN22         IFEQ      *ON
     C*
     C   82              Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEAD#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C*
     C     *IN30         IFEQ      *ON
     C                   EXCEPT    HEADR
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADR#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   ELSE
     C*
     C     *IN31         IFEQ      *ON
     C                   EXCEPT    HEADA
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADA#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   ENDIF                                                  *IN31=*ON
     C*
     C                   ENDIF                                                  *IN30=*ON
     C*
      *
     C                   EXSR      SREFDT
     C                   EXCEPT    HEADE
     C                   EXCEPT    HEAD1
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADE#
     C                   EXCEPT    HEAD1#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   MOVE      *OFF          *IN22
     C*
     C                   ENDIF                                                  *IN22=*ON
     C*
     C     EECASE        IFNE      *ZEROS
     C*
      *
     C     QQPRSS        IFEQ      '4'
     C                   MOVE      EESS#         XXSS4             4
     C     'XXXXX'       CAT       XXSS4         XXSS#
      *
     C                   ELSE
      *
     C*** Begin Add *** 06/19/18 ****************************
     C                   If        qqPrSS = 'Y'
     C*** End   Add *** 06/19/18 ****************************
     C                   MOVE      EESS#         XXSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   Eval      XXSS#=MaskData('SSN':'XXSS#':XXSS#)
     C***  End Add  ***  06/09/20  *******************************
     C*** Begin Add *** 06/19/18 ****************************
     C                   Else
     C                   MOVE      'XXXXXXXXX'   XXSS#
     C                   EndIf
     C*** End   Add *** 06/19/18 ****************************
     C                   ENDIF
      *
     C*
     C                   Z-ADD     EEACDT        XXACDT            8 0
     C*
     C                   Z-ADD     EERPDT        XXRPDT            8 0
     C*
     C     EESORT        IFNE      *HIVAL
     C*
     C     EEACD2        COMP      *BLANKS                                71
     C                   EXCEPT    DETAIL
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    DETAI#
     C                   END
     C*
     C     *INOF         IFEQ      *ON
     C*
     C                   EXCEPT    HEAD
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEAD#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C*
     C     *IN30         IFEQ      *ON
     C*
     C                   EXCEPT    HEADR
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADR#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   ELSE
     C*
     C     *IN31         IFEQ      *ON
     C                   EXCEPT    HEADA
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADA#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   ENDIF                                                  *IN31=*ON
     C*
     C                   ENDIF                                                  *IN30=*ON
     C*
      *
     C                   EXSR      SREFDT
     C                   EXCEPT    HEADE
     C                   EXCEPT    HEAD1
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADE#
     C                   EXCEPT    HEAD1#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   MOVE      *OFF          *INOF
     C                   MOVE      *OFF          *INOA
     C*
     C                   ENDIF                                                  *INOF=*ON
     C                   ENDIF                                                  EESORT<>*HIVAL
     C                   ENDIF                                                  EECASE<>*ZEROS
     C                   ENDIF                                                  QQEMPC='N'
     C*
     C                   Z-ADD     *ZERO         EEPTOT
     C                   Z-ADD     *ZERO         EERTOT
     C                   Z-ADD     *ZERO         EEDTOT
     C                   Z-ADD     *ZERO         EEOTOT
     C                   Z-ADD     *ZERO         EEICMP
     C                   Z-ADD     *ZERO         EEIMED
     C                   Z-ADD     *ZERO         EEILEG
     C                   Z-ADD     *ZERO         EEIOTH
     C                   Z-ADD     *ZERO         EEITOT
     C                   Z-ADD     *ZERO         EEGTOT
     C                   Z-ADD     *ZERO         EEGPOT
     C                   Z-ADD     *ZERO         EETROT
     C                   Z-ADD     *ZERO         EERCOT
     C*
     C                   READ      PEE01                                  60
     C                   MOVE      '1'           *IN88
     C                   END
     C                   EXSR      SR3TH                                        YR  SBR
     C     *IN79         IFEQ      '1'
     C     *IN81         OREQ      '1'
     C                   EXSR      SR4TH                                        DIV SBR
     C                   END
     C                   MOVE      '1'           *IN79
     C*
     C     QQEMPC        IFEQ      'Y'
     C                   EXSR      SR5TH                                        EMP SBR
     C                   ELSE
     C  N35              EXSR      SR5TH                                        EMP SBR
     C                   ENDIF                                                  QQEMPC='Y'
     C     *IN30         IFEQ      '1'
     C     *IN80         OREQ      '1'
     C                   EXSR      SR6TH                                        AG/RG SBR
     C                   END
     C     XXCS6         ADD       XXOP6         XXCLM             5 0
     C                   MOVE      LRAMT         XXAMT            11 2
     C                   MOVE      LRITOT        XXTOT            11 2
     C                   EXSR      SRLFR                                        FRQ/LSS
     C     *IN78         IFEQ      '1'
     C                   MOVE      '0'           *IN52
     C                   ELSE
     C                   MOVE      '1'           *IN52
     C                   ENDIF
     C   82              Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEAD#
     C                   MOVE      '1'           HDRFLG
     C                   END
      *
      *
     C                   EXSR      SREFDT
     C  N82              EXCEPT    HEADE
     C     *IN69         IFEQ      *ON
     C  N82              EXCEPT    HEADE#
     C                   MOVE      '1'           HDRFLG
     C                   END
      *
     C                   EXCEPT    HEAD1
     C                   EXCEPT    TOTLR
     C     *IN69         IFEQ      '1'                                          DART <> BLANKS
     C                   EXCEPT    HEAD#
     C                   EXCEPT    HEAD1#
     C                   EXCEPT    HEADE#
     C                   EXCEPT    TOTLR#
     C                   MOVE      '1'           HDRFLG
     C*
     C                   Z-ADD     EEEMP#        @EEMP#
     C                   Z-ADD     EEDIV         @EDIV
     C                   MOVE      '1'           @CLSFL
     C*
     C                   MOVE      EECO#         #1CO#             3
     C                   MOVE      EEFND         #1FND             3
     C                   CLOSE     QSYSPRT1
     C*
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       ##FLR            80
     C                   PARM      *BLANKS       ##DCID           12
     C                   PARM      'EX'          ##PRE             2
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM      'P'           ##RTCD            1
     C*
     C                   CALL      'CRTRSLV'
     C                   PARM                    ##FLR            80
     C                   PARM                    ##DCID           12
     C                   PARM      'QSYSPRT1'    SPLF             10
     C                   PARM                    RTCD              1
     C*
     C                   CLEAR                   SRDTO
     C                   CLEAR                   SRERR
     C                   Z-ADD     DSDATE        SRDTI1            8 0
     C                   CALL      'SRCVTDAT'    DTCYM1
     C                   MOVEL     SRDTO         QQDTE
      ***** BEG ADD ******* 10/31/13 **********************************
     C                   EVAL      ##DIFL = *BLANKS
     C     STKY          CHAIN     PCT01
     C                   IF        %FOUND(STCTL1)
     C                   EVAL      ##DIFL = CTFL01
     C                   ENDIF
      ***** END ADD ******* 10/31/13 **********************************
    C*
     C                   CALL      'DA700R'
     C                   PARM                    QCKEY            40
     C                   PARM                    ##COMM          256
     C                   PARM                    QQDTE             8
     C                   PARM                    ##DCID           12
     C                   PARM                    ##PNAM           10
     C                   PARM      '000'         ##SECL            3
     C                   PARM      UPUSR         ##USER           10
     C                   PARM      Q1PGM         ##PGM            10
     C                   PARM                    ##FLR            80
      ***** BEG CHG ******* 10/31/13 **********************************
     C*************      PARM      'Y'           ##DIFL            1
     C                   PARM                    ##DIFL            1
      ***** END CHG ******* 10/31/13 **********************************
     C                   PARM      Q1PGM         ##IPGM           10
     C                   PARM      ' '           ##FL01            1
     C                   PARM      ' '           ##FL02            1
     C                   PARM      QQMARK        ##RMCD            5
     C                   PARM      *BLANKS       ##SEQ#            5
     C                   PARM      'N'           Q#SBYL            1
     C                   PARM      *BLANKS       Q#COM2           19
     C                   PARM      *BLANKS       Q#APID            2
     C                   PARM      *BLANKS       Q#IDOC           12
     C                   PARM      *BLANKS       Q#IQUE           10
     C                   PARM      'O'           Q#IMGF            1
     C                   PARM      *BLANKS       Q#IPTH           10
     C                   PARM      *BLANKS       Q#PIDX            5
     C                   PARM      *BLANK        Q#QPTY            1
     C                   PARM      *BLANKS       Q#DCTP            5
     C                   PARM      *BLANKS       Q#CSID            2
     C                   PARM      *BLANKS       Q#CODE            2
     C*
     C                   ENDIF
      /Free
       // ***** Beg Add ***** 08/26/14 *******************************
       xxPrt = '0';
       Chain orKy POR01;
       If %found;
          Except Route;
       Endif;
       // ***** End Add ***** 08/26/14 *******************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C                   MOVE      QQFYR         XXFYR2            3 0
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQMNTH        XXMNTH            3 0
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XYDIV
     C     QQDIV         COMP      *BLANKS                                81    ALL DIV
     C                   MOVE      QQAGT#        XXAGT#
     C*
     C     XXTINC        COMP      *ZERO                              7676       $ INC SEL
     C     QQGTLT        COMP      'G'                                    33     GREATER THAN
     C     XXFYR         COMP      0                                      78    ALL FYR
     C     XXEMP#        COMP      0                                      79    ALL EMP#
     C     XXEMP#        COMP      0                                      82    ALL EMP#
     C     XXAGT#        COMP      0                                      80    ALL AGTS
     C*
     C     UPKY          CHAIN     WMUPL                              60
     C*
     C* * * INITIALIZE FIELDS
     C                   TIME                    DSDTTM
     C*
     C     COKY          CHAIN     SMCOL                              60
     C*
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*
     C*  GET FUND DESCRIPTION
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           55
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C                   ELSE
     C                   MOVE      *BLANKS       XFNAME
     C                   END
     C*
     C*  RETRIEVE ADJUSTER HEADING FROM CLAIMS/DARTS CONTROL FILE.
     C*
     C     CXKY          CHAIN     PCX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     CXCAT2        ANDNE     *BLANKS
     C*
     C                   MOVEL     CXCAT2        X1CAT2
     C                   ELSE
     C*
     C                   MOVEL     XXCAT2        X1CAT2
     C*
     C                   ENDIF
     C*
     C*
     C* CHECK FOR REPORT TO BE SORTED IN REGION ORDER
     C*
     C     QQSRGN        IFEQ      'Y'
     C                   MOVE      '1'           *IN30
     C                   ELSE
     C                   MOVE      '0'           *IN30
     C                   END
     C*
     C* CHECK FOR REPORT TO BE SORTED IN AGENT # ORDER
     C*
     C     QQSAGT        IFEQ      'Y'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN32
     C                   ELSE
     C                   MOVE      '0'           *IN31
     C                   END
     C     *IN30         IFNE      '1'
     C     XXAGT#        ANDNE     0
     C                   MOVE      '1'           *IN31
     C                   END
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C*
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF                                                  N<=60
     C*
     C                   ENDIF                                                  ORTITL<>*BLANKS
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
      ***** Beg Del ***** 08/26/14 *************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *************************************
     C*
     C*  CENTER TEXT IN FIELD
     C*
     C                   MOVEL     ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        ORTITL
     C*
     C*  CENTER TEXT IN FIELD
     C*
     C                   MOVEL     CONAME        XXCONM           60
     C*
     C                   MOVEL     XXCONM        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        CONAME
     C                   END
     C*** Beg Add ***  11/25/13  ******************************************************
      /Free
       RtvCltID(CltId);
       charLGRH = *Blanks;
       *IN72 = *Off;
       If CltId = '129';
          Chain keyWMcxl pcx01;
          If %Found(wmcxl);
             If CXlgrh <> *Blanks;
                charLGRH = CXlgrh;
             Else;
                charLGRH = 'LEGAL';
             EndIf;
       // *** Beg Add ***  03/11/14  **************************************
             charLGRH = RightJstfy(charLGRH:10);
       // *** End Add ***  03/11/14  ***************************************
             *IN72 = *On;
          Endif;
       EndIf;
      /End-Free
     C*** End Add ***  11/25/13  ******************************************************
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C                   MOVE      '1'           *IN22
     C                   MOVE      '1'           *IN52
     C*
     C                   ENDSR
     C*****************************************************************
     C*
     C*==================================================================*
     C*   YEAR          -  BREAK           ** Subroutine ** S R 3 TH     *
     C*==================================================================*
     C*
     CSR   SR3TH         BEGSR
     C*
     C     *IN78         IFEQ      '1'
     C     XXOP2         IFNE      *ZEROS
     C     XXCS2         ORNE      *ZEROS
     C     XYAMT         ORNE      *ZEROS
     C*
     C     QQEMPC        IFEQ      'Y'
     C*
     C     QQEMPC        OREQ      'N'
     C     XXOP2         ANDNE     *ZEROS
     C*** Begin Add *** 04/24/18 ************************************
     C     QQSELO        ANDEQ     'Y'
     C*** End   Add *** 04/24/18 ************************************
     C     QQEMPC        OREQ      'N'
     C     XXCS2         ANDNE     *ZEROS
     C*** Begin Add *** 04/24/18 ************************************
     C     QQSELC        ANDEQ     'Y'
     C*** End   Add *** 04/24/18 ************************************
     C*
     C     *IN22         IFEQ      '1'
     C   82              Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEAD#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C     *IN30         IFEQ      '1'
     C                   EXCEPT    HEADR
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADR#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   ELSE
     C     *IN31         IFEQ      '1'
     C                   EXCEPT    HEADA
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADA#
     C                   END
     C                   END
     C                   END
      *
     C                   EXSR      SREFDT
     C                   EXCEPT    HEADE
     C                   EXCEPT    HEAD1
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADE#
     C                   EXCEPT    HEAD1#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   MOVE      '0'           *IN22
     C                   END
     C                   ENDIF                                                  QQEMPC='Y'
     C     XXCS2         ADD       XXOP2         XXCLM             5 0
     C                   MOVE      XYAMT         XXAMT            11 2
     C                   MOVE      YRITOT        XXTOT            11 2
     C                   EXSR      SRLFR
     C*
     C     QQEMPC        IFEQ      'Y'
     C*
     C     QQEMPC        OREQ      'N'
     C     XXOP2         ANDNE     *ZEROS
     C*** Begin Add *** 04/24/18 ************************************
     C     QQSELO        ANDEQ     'Y'
     C*** End   Add *** 04/24/18 ************************************
     C     QQEMPC        OREQ      'N'
     C     XXCS2         ANDNE     *ZEROS
     C*** Begin Add *** 04/24/18 ************************************
     C     QQSELC        ANDEQ     'Y'
     C*** End   Add *** 04/24/18 ************************************
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEAD#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C     *IN30         IFEQ      '1'
     C                   EXCEPT    HEADR
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADR#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   ELSE
     C     *IN31         IFEQ      '1'
     C                   EXCEPT    HEADA
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADA#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   END
     C                   END
      *
     C                   EXSR      SREFDT
     C                   EXCEPT    HEADE
     C                   EXCEPT    HEAD1
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADE#
     C                   EXCEPT    HEAD1#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   MOVE      '0'           *INOF
     C                   MOVE      '0'           *INOA
     C                   END
      *
     C                   EXSR      SREFDT
     C                   EXCEPT    TOTYR
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    TOTYR#
     C                   END
     C                   ENDIF                                                  QQEMPC='Y'
     C                   END
     C                   END
     C                   Z-ADD     *ZERO         XXCS2
     C                   Z-ADD     *ZERO         XXOP2
     C                   Z-ADD     *ZERO         XYAMT
     C                   Z-ADD     *ZERO         XYPREM
     C                   Z-ADD     *ZERO         YRPCMP
     C                   Z-ADD     *ZERO         YRPMED
     C                   Z-ADD     *ZERO         YRPLEG
     C                   Z-ADD     *ZERO         YRPOTH
     C                   Z-ADD     *ZERO         YRPTOT
     C*
     C                   Z-ADD     *ZERO         YRRCMP
     C                   Z-ADD     *ZERO         YRRMED
     C                   Z-ADD     *ZERO         YRRLEG
     C                   Z-ADD     *ZERO         YRROTH
     C                   Z-ADD     *ZERO         YRRTOT
     C*
     C                   Z-ADD     *ZERO         YRDCMP
     C                   Z-ADD     *ZERO         YRDMED
     C                   Z-ADD     *ZERO         YRDLEG
     C                   Z-ADD     *ZERO         YRDOTH
     C                   Z-ADD     *ZERO         YRDTOT
     C*
     C                   Z-ADD     *ZERO         YROCMP
     C                   Z-ADD     *ZERO         YROMED
     C                   Z-ADD     *ZERO         YROLEG
     C                   Z-ADD     *ZERO         YROOTH
     C                   Z-ADD     *ZERO         YROTOT
     C*
     C                   Z-ADD     *ZERO         YRICMP
     C                   Z-ADD     *ZERO         YRIMED
     C                   Z-ADD     *ZERO         YRILEG
     C                   Z-ADD     *ZERO         YRIOTH
     C                   Z-ADD     *ZERO         YRITOT
     C*
     C                   Z-ADD     *ZEROS        YRGCMP                         GROSS PD-TO-DT
     C                   Z-ADD     *ZEROS        YRGMED
     C                   Z-ADD     *ZEROS        YRGLEG
     C                   Z-ADD     *ZEROS        YRGEXP
     C                   Z-ADD     *ZEROS        YRGTOT
     C*
     C                   Z-ADD     *ZEROS        YRGPCO                         GROSS-CURR PERD
     C                   Z-ADD     *ZEROS        YRGPME
     C                   Z-ADD     *ZEROS        YRGPLE
     C                   Z-ADD     *ZEROS        YRGPEX
     C                   Z-ADD     *ZEROS        YRGPOT
     C*
     C                   Z-ADD     *ZEROS        YRRTCO                         RECOVERY TO-DTE
     C                   Z-ADD     *ZEROS        YRRTME
     C                   Z-ADD     *ZEROS        YRRTLE
     C                   Z-ADD     *ZEROS        YRRTEX
     C                   Z-ADD     *ZEROS        YRTROT
     C*
     C                   Z-ADD     *ZEROS        YRRCCO                         RCRVY-CURR PERD
     C                   Z-ADD     *ZEROS        YRRCME
     C                   Z-ADD     *ZEROS        YRRCLE
     C                   Z-ADD     *ZEROS        YRRCEX
     C                   Z-ADD     *ZEROS        YRRCOT
     C*
     C                   ENDSR
     C*
      *==================================================================*
      *   DIVISION      -  BREAK           ** Subroutine ** S R 4 TH     *
      *==================================================================*
     C*
     CSR   SR4TH         BEGSR
     C*
     C     XXOP3         IFNE      *ZEROS
     C     XXCS3         ORNE      *ZEROS
     C     XDAMT         ORNE      *ZEROS
     C*
     C     QQEMPC        IFEQ      'Y'
     C*
     C     QQEMPC        OREQ      'N'
     C     XXOP3         ANDNE     *ZEROS
     C*** Begin Add *** 04/24/18 ************************************
     C     QQSELO        ANDEQ     'Y'
     C*** End   Add *** 04/24/18 ************************************
     C     QQEMPC        OREQ      'N'
     C     XXCS3         ANDNE     *ZEROS
     C*** Begin Add *** 04/24/18 ************************************
     C     QQSELC        ANDEQ     'Y'
     C*** End   Add *** 04/24/18 ************************************
     C*
     C*
     C     *IN22         IFEQ      '1'
     C   82              Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEAD#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C     *IN30         IFEQ      '1'
     C                   EXCEPT    HEADR
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADR#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   ELSE
     C     *IN31         IFEQ      '1'
     C                   EXCEPT    HEADA
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADA#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   END
     C                   END
      *
     C                   EXSR      SREFDT
     C                   EXCEPT    HEADE
     C                   EXCEPT    HEAD1
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADE#
     C                   EXCEPT    HEAD1#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   MOVE      '0'           *IN22
     C                   END
     C                   ENDIF                                                  QQEMPC='Y'
     C     XXCS3         ADD       XXOP3         XXCLM
     C                   MOVE      XDAMT         XXAMT
     C                   MOVE      DVITOT        XXTOT
     C                   EXSR      SRLFR
     C*
     C     QQEMPC        IFEQ      'Y'
     C*
     C     QQEMPC        OREQ      'N'
     C     XXOP3         ANDNE     *ZEROS
     C*** Begin Add *** 04/24/18 ************************************
     C     QQSELO        ANDEQ     'Y'
     C*** End   Add *** 04/24/18 ************************************
     C     QQEMPC        OREQ      'N'
     C     XXCS3         ANDNE     *ZEROS
     C*** Begin Add *** 04/24/18 ************************************
     C     QQSELC        ANDEQ     'Y'
     C*** End   Add *** 04/24/18 ************************************
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEAD#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C     *IN30         IFEQ      '1'
     C                   EXCEPT    HEADR
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADR#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   ELSE
     C     *IN31         IFEQ      '1'
     C                   EXCEPT    HEADA
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADA#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   END
     C                   END
      *
     C                   EXSR      SREFDT
     C                   EXCEPT    HEADE
     C                   EXCEPT    HEAD1
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADE#
     C                   EXCEPT    HEAD1#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   MOVE      '0'           *INOF
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    TOTDV
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    TOTDV#
     C                   END
     C                   ENDIF                                                  QQEMPC='Y'
     C                   END
     C                   Z-ADD     *ZERO         XDAMT
     C                   Z-ADD     *ZERO         XDPREM
     C                   Z-ADD     *ZERO         XXCS3
     C                   Z-ADD     *ZERO         XXOP3
     C                   Z-ADD     *ZERO         DVPCMP
     C                   Z-ADD     *ZERO         DVPMED
     C                   Z-ADD     *ZERO         DVPLEG
     C                   Z-ADD     *ZERO         DVPOTH
     C                   Z-ADD     *ZERO         DVPTOT
     C*
     C                   Z-ADD     *ZERO         DVRCMP
     C                   Z-ADD     *ZERO         DVRMED
     C                   Z-ADD     *ZERO         DVRLEG
     C                   Z-ADD     *ZERO         DVROTH
     C                   Z-ADD     *ZERO         DVRTOT
     C*
     C                   Z-ADD     *ZERO         DVDCMP
     C                   Z-ADD     *ZERO         DVDMED
     C                   Z-ADD     *ZERO         DVDLEG
     C                   Z-ADD     *ZERO         DVDOTH
     C                   Z-ADD     *ZERO         DVDTOT
     C*
     C                   Z-ADD     *ZERO         DVOCMP
     C                   Z-ADD     *ZERO         DVOMED
     C                   Z-ADD     *ZERO         DVOLEG
     C                   Z-ADD     *ZERO         DVOOTH
     C                   Z-ADD     *ZERO         DVOTOT
     C*
     C                   Z-ADD     *ZERO         DVICMP
     C                   Z-ADD     *ZERO         DVIMED
     C                   Z-ADD     *ZERO         DVILEG
     C                   Z-ADD     *ZERO         DVIOTH
     C                   Z-ADD     *ZERO         DVITOT
     C                   MOVE      '1'           *IN52
     C*
     C                   Z-ADD     *ZEROS        DVGCMP                         GROSS PD-TO-DT
     C                   Z-ADD     *ZEROS        DVGMED
     C                   Z-ADD     *ZEROS        DVGLEG
     C                   Z-ADD     *ZEROS        DVGEXP
     C                   Z-ADD     *ZEROS        DVGTOT
     C*
     C                   Z-ADD     *ZEROS        DVGPCO                         GROSS-CURR PERD
     C                   Z-ADD     *ZEROS        DVGPME
     C                   Z-ADD     *ZEROS        DVGPLE
     C                   Z-ADD     *ZEROS        DVGPEX
     C                   Z-ADD     *ZEROS        DVGPOT
     C*
     C                   Z-ADD     *ZEROS        DVRTCO                         RECOVERY TO-DTE
     C                   Z-ADD     *ZEROS        DVRTME
     C                   Z-ADD     *ZEROS        DVRTLE
     C                   Z-ADD     *ZEROS        DVRTEX
     C                   Z-ADD     *ZEROS        DVRTOT
     C*
     C                   Z-ADD     *ZEROS        DVRCCO                         RCRVY-CURR PERD
     C                   Z-ADD     *ZEROS        DVRCME
     C                   Z-ADD     *ZEROS        DVRCLE
     C                   Z-ADD     *ZEROS        DVRCEX
     C                   Z-ADD     *ZEROS        DVRCOT
     C*
     C                   ENDSR
     C*
      *==================================================================*
      *   EMPLOYER / ALL DIV/YR  BREAK       ** Subroutine ** S R 5 TH   *
      *==================================================================*
     C*
     CSR   SR5TH         BEGSR
     C*
     C     *IN79         IFEQ      '1'
     C*
     C     QQEMPC        IFEQ      'Y'
     C*
     C     QQEMPC        OREQ      'N'
     C     XXOP4         ANDNE     *ZEROS
     C*** Begin Add *** 04/24/18 ************************************
     C     QQSELO        ANDEQ     'Y'
     C*** End   Add *** 04/24/18 ************************************
     C     QQEMPC        OREQ      'N'
     C     XXCS4         ANDNE     *ZEROS
     C*** Begin Add *** 04/24/18 ************************************
     C     QQSELC        ANDEQ     'Y'
     C*** End   Add *** 04/24/18 ************************************
     C*
     C     *IN22         IFEQ      '1'
     C   82              Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEAD#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C     *IN30         IFEQ      '1'
     C                   EXCEPT    HEADR
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADR#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   ELSE
     C     *IN31         IFEQ      '1'
     C                   EXCEPT    HEADA
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADA#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   END
     C                   END
      *
     C                   EXSR      SREFDT
     C                   EXCEPT    HEADE
     C                   EXCEPT    HEAD1
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADE#
     C                   EXCEPT    HEAD1#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   MOVE      '0'           *IN22
     C                   END
     C                   ENDIF                                                  QQEMPC='Y'
     C     XXCS4         ADD       XXOP4         XXCLM
     C                   MOVE      XEAMT         XXAMT
     C                   MOVE      EMITOT        XXTOT
     C                   EXSR      SRLFR
     C*
     C     QQEMPC        IFEQ      'Y'
     C*
     C     QQEMPC        OREQ      'N'
     C     XXOP4         ANDNE     *ZEROS
     C*** Begin Add *** 04/24/18 ************************************
     C     QQSELO        ANDEQ     'Y'
     C*** End   Add *** 04/24/18 ************************************
     C     QQEMPC        OREQ      'N'
     C     XXCS4         ANDNE     *ZEROS
     C*** Begin Add *** 04/24/18 ************************************
     C     QQSELC        ANDEQ     'Y'
     C*** End   Add *** 04/24/18 ************************************
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEAD#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C     *IN30         IFEQ      '1'
     C                   EXCEPT    HEADR
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADR#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   ELSE
     C     *IN31         IFEQ      '1'
     C                   EXCEPT    HEADA
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADA#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   END
     C                   END
      *
     C                   EXSR      SREFDT
     C                   EXCEPT    HEADE
     C                   EXCEPT    HEAD1
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADE#
     C                   EXCEPT    HEAD1#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   MOVE      '0'           *INOF
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    TOTEM
     C     *IN69         IFEQ      *ON
     C                   END
     C                   END
     C                   ENDIF                                                  QQEMPC='Y'
     C                   Z-ADD     *ZERO         XEAMT
     C                   Z-ADD     *ZERO         XEPREM
     C                   MOVE      '1'           *IN52
     C                   ENDSR
     C*
     C*==================================================================*
     C*   AGENT OR REGION  -  BREAK        ** Subroutine ** S R 6 TH     *
     C*==================================================================*
     C*
     CSR   SR6TH         BEGSR
     C*
     C     *IN30         IFEQ      '1'
     C     *IN31         OREQ      '1'
     C*
     C     QQEMPC        IFEQ      'Y'
     C*
     C     QQEMPC        OREQ      'N'
     C     XXOP5         ANDNE     *ZEROS
     C*** Begin Add *** 04/24/18 ************************************
     C     QQSELO        ANDEQ     'Y'
     C*** End   Add *** 04/24/18 ************************************
     C     QQEMPC        OREQ      'N'
     C     XXCS5         ANDNE     *ZEROS
     C*** Begin Add *** 04/24/18 ************************************
     C     QQSELC        ANDEQ     'Y'
     C*** End   Add *** 04/24/18 ************************************
     C*
     C                   MOVE      '0'           *IN52
     C                   EXCEPT    HEAD
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEAD#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C     *IN79         IFEQ      '0'
      *
     C                   EXSR      SREFDT
     C                   EXCEPT    HEADE
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEADE#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   END
     C                   EXCEPT    HEAD1
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    HEAD1#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   ENDIF                                                  QQEMPC='Y'
     C     XXCS5         ADD       XXOP5         XXCLM
     C                   MOVE      XGAMT         XXAMT
     C                   MOVE      XGITOT        XXTOT
     C                   EXSR      SRLFR
     C*
     C     QQEMPC        IFEQ      'Y'
     C*
     C     QQEMPC        OREQ      'N'
     C     XXOP5         ANDNE     *ZEROS
     C*** Begin Add *** 04/24/18 ************************************
     C     QQSELO        ANDEQ     'Y'
     C*** End   Add *** 04/24/18 ************************************
     C     QQEMPC        OREQ      'N'
     C     XXCS5         ANDNE     *ZEROS
     C*** Begin Add *** 04/24/18 ************************************
     C     QQSELC        ANDEQ     'Y'
     C*** End   Add *** 04/24/18 ************************************
     C*
     C                   EXCEPT    TOTG
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    TOTG#
     C                   END
     C                   ENDIF                                                  QQEMPC='Y'
     C                   END
     C                   Z-ADD     *ZERO         XGAMT
     C                   Z-ADD     *ZERO         XGPREM
     C                   MOVE      '1'           *IN52
     C                   ENDSR
     C*
     C*==================================================================*
     C*   Compute Loss and frequency Ratio ** Subroutine **   S R L F R  *
     C*==================================================================*
     C*
     CSR   SRLFR         BEGSR
     C*
     C     XXAMT         IFNE      *ZERO
     C     XXTOT         MULT      100           XXWK92           15 2
     C     XXWK92        DIV(H)    XXAMT         XXLOSS            7 2
     C                   ELSE
     C                   Z-ADD     *ZERO         XXLOSS
     C                   END
     C**
     C     XXAMT         DIV(H)    1000          XXPREM            9 2
     C     XXPREM        IFNE      *ZERO
     C     XXCLM         DIV(H)    XXPREM        XXFREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         XXFREQ
     C                   END
     C                   ENDSR
      *
      *
      *
      *==================================================================*
      *  SREFDT - Get Emp Pol Eff Date range                             *
      *==================================================================*
     CSR   SREFDT        BEGSR
      *
     C                   SETOFF                                       6083
     C                   Z-ADD     *ZERO         XXPFDT            8 0
     C                   Z-ADD     *ZERO         XXPTDT            8 0
      *
     C     *IN30         IFEQ      *OFF
     C     *IN32         ANDEQ     *OFF
     C     ELKY          CHAIN     PEL01                              60
     C                   SETON                                        83
      *
     C                   ELSE
      *
     C     ELKY1         CHAIN     PEL01                              60
     C                   ENDIF                                                  N30 N32
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT
      *
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPTDT
     C                   ENDIF
      *
     C  N78              SETON                                        83
      *
     C                   ENDSR                                                  SREFDT
      *
      *
      *
      *==================================================================*
      *   Compute Earned Premium to Date   ** Subroutine **   S R P R M  *
      *==================================================================*
     C*
     CSR   SRPRM         BEGSR
     C**
     C**    CALL PGM TO CALCULATE EARNED PREMIUM YEAR-TO-DATE
     C**                      AND EARNED PREMIUM CURRENT PERIOD
     C**
     C                   Z-ADD     *ZERO         XAMT1
     C                   Z-ADD     *ZERO         XPREM1
     C                   MOVEL     EECO#         SRCO#             3
     C                   MOVEL     EEFND         SRFND             3
     C                   MOVEL     EEFYR         SRFYR             3
     C                   MOVEL     EEEMP#        SREMP#            9
     C                   MOVEL     EEDIV         SRDIV             5
     C                   MOVE      ' '           QQERR             1
     C                   MOVE      *BLANK        SRAMT            11
     C                   MOVE      *BLANK        SRAMT2           11
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRPRT1'
     C                   PARM                    SRCO#
     C                   PARM                    SRFND
     C                   PARM                    SRFYR
     C                   PARM                    SREMP#
     C                   PARM                    SRDIV
     C                   PARM                    QQDTE
     C                   PARM                    SRAMT
     C                   PARM                    SRAMT2
     C                   PARM                    QQTERM
     C                   MOVE      SRAMT         XAMT1            11 2
     C                   MOVE      SRAMT2        XPREM1           11 2
     C*
     C                   ADD       XAMT1         XYAMT            11 2
     C                   ADD       XPREM1        XYPREM           11 2
     C                   ADD       XAMT1         XDAMT            11 2
     C                   ADD       XPREM1        XDPREM           11 2
     C                   ADD       XAMT1         XEAMT            11 2
     C                   ADD       XPREM1        XEPREM           11 2
     C                   ADD       XAMT1         XGAMT            11 2
     C                   ADD       XPREM1        XGPREM           11 2
     C                   ADD       XAMT1         LRAMT            11 2
     C                   ADD       XPREM1        LRPREM           11 2
     C*
     C                   MOVEL     'N'           PGBRK             1
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C                   SELECT
     C     *IN60         WHENEQ    *OFF
     C     ELFAF         ANDEQ     'A'
     C     XAMT1         ANDEQ     *ZERO
     C                   MOVEL     'Y'           PGBRK
     C     *IN60         WHENEQ    *OFF
     C     ELFAF         ANDEQ     ' '
     C     XAMT1         ANDEQ     *ZERO
     C                   MOVEL     'Y'           PGBRK
     C     XAMT1         WHENNE    *ZERO
     C                   MOVEL     'Y'           PGBRK
     C     EECASE        WHENNE    *ZERO
     C                   MOVEL     'Y'           PGBRK
     C                   ENDSL
     C*
     C                   ENDSR
     C*
     C* ==============================================================
     C*    *INZSR  Initialization Subroutine
     C* ==============================================================
     C     *INZSR        BEGSR
     C*
     C*    Determine printing of SS#
     C     QQPRSS        IFEQ      'Y'
     C     QQPRSS        OREQ      '4'
     C                   MOVE      *ON           *IN84
     C                   ELSE                                                   X. PRSS = Y
     C                   MOVE      *OFF          *IN84
     C                   ENDIF                                                  e. PRSS = Y
     C                   ENDSR
     C* ==============================================================
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ***************************************
     O*********                                   8 'XXXXXXXX'
     O*********                                  20 'XXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXX     XX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXXX    XX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XXXX    XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   8 'XXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   4 'XXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   5 'XXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   6 'XX XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   7 'XX  XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX    XXXX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   8 'XX   XXX'
     O*********
     O*********                                  33 'XXXXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX    XXXX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          3
     O*********                                   9 'XX    XXX'
     O*********                                  20 'XXXXXX'
     O*********                                  32 'XXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX     XXX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                  15 'ROUTE TO:'
     O*********      08      ORDIS1              60
     O*********         09   UPUSNM              67
     O********* E            ROUTE          1
     O*********      08      ORDIS2              60
     O********* E            ROUTE          1
     O*********      08      ORDIS3              60
     O********* E            ROUTE          2
     O*********      08      ORDIS4              60
      ***** End Del ***** 08/26/14 ****************************************
      ***** Beg Add ***** 08/05/15 ************************************
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
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ****************************************
     O********* E            ROUTE          2
     O*********                                  19 'PROGRAM NAME:'
     O*********              ORPNAM              30
     O*********                                  36 'FILE:'
     O*********              ORFIL#              38
     O*********                                  46 'COPIES:'
     O*********              ORCOPY        Z     50
     O*********                                  56 'HOLD:'
     O*********              ORHOLD              61
     O********* E            ROUTE          2
     O*********                                  19 'OUTPUT QUEUE:'
     O*********              OROUTQ              30
     O*********                                  37 'FORMS:'
     O*********              ORFORM              48
     O*********                                  56 'SAVE:'
     O*********              ORSAVE              61
      ***** End Del ***** 08/26/14 ****************************************
     O          E            ROUTE       2  1
     O                                           18 'Group:    '
     O                       XXFND         Z     36
     O          E            ROUTE          1
     O                                           20 'Policy Year:'
     O              N78      XXFYR         Z     36
     O               78                          36 '*ALL'
     O          E            ROUTE          1
     O                                           13 'Date:'
     O                       XXDTE2        Y     36
     O          E            ROUTE          1
     O                                           21 'Employer/Div:'
     O              N79      XXEMP#        Z     30
     O              N79N81   QQDIV               36
     O              N79 81                       36 '/*ALL'
     O               79                          36 '*ALL'
     O          E            ROUTE          1
     O                                           18 'Agent:    '
     O              N80      XXAGT#        Z     36
     O               80                          36 '*ALL'
     O          E            ROUTE          1
     O                                           23 'Sort Selection:'
     O               30                          36 '2'
     O               32                          36 '1'
     O                                           54 '(1) Agent Sort'
     O          E            ROUTE          1
     O                                           55 '(2) Region Sort'
     O          E            ROUTE          1
     O                                           27 'Select Open Claims:'
     O                       QQSELO              36
     O          E            ROUTE          1
     O                                           29 'Select Closed Claims:'
     O                       QQSELC              36
     O                                           57 '# of Months Closed:'
     O                       XXMNTH              61
     O          E            ROUTE          1
     O                                           32 'Print Social Security #:'
     O                       QQPRSS              36
     O          E            ROUTE          2
     O                                           30 'Employers w/no Claims:'
     O                       QQEMPC              36
     O          E            ROUTE          1
     O                                           28 'LT/Med/Both (L/M/B):'
     O                       QQLTFL              36
     O          E            ROUTE       1  1
     O                                           23 'Claims based on'
     O                     33                    37 'greater than:'
     O                    N33                    37 'less than:   '
     O          E            ROUTE          1
     O                                           30 'Tot incurred amount:  '
     O                       XXTINC              48 '  ,   , $0.  '
     O          E            ROUTE          1
     O                                           22 'Sort Sequence:'
     O                       QQSORT              36
     O                                           47 '(C) Case'
     O          E            ROUTE          1
     O                                           56 '(D) Accident Date'
     O          E            ROUTE          1
     O                                           60 '(A) Alpha by Claimant'
     O                                           70 'Last Name'
     O          E            ROUTE          1
     O                                           58 '(S) Social Security'
     O                                           65 'Number'
     O          E            ROUTE          1
     O                                           52 '(J) Job Class'
     O          E            ROUTE          1
     O                                           51 '(Z) Zip Code'
     O          E            ROUTE          1
     O                                           53 '(1) Category 1'
     O          E            ROUTE          1
     O                                           53 '(2) Category 2'
     O          E            ROUTE          1
     O                                           60 '(N) Nature/Part/Cause'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             116
     O                       DSDATE        Y    198
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL             127
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD           1
     O               52                          87 'POLICY YEAR'
     O               52      XXFYR2              91
     O               52                         107 'AS OF'
     O               52      XXDTE2        Y    118
     O              N52                          95 'AS OF'
     O              N52      XXDTE2        Y    106
     O          E            HEAD           2
     O                                            6 'GROUP:'
     O                       XXFND         Z     16
     O                       XFNAME              77
     O                                          110 'SELECT CLAIMS'
     O                     33                   123 'GREATER THAN'
     O                    N33                   123 'LESS THAN   '
     O               76                         138 'TOTAL INCURRED'
     O               76                         149 'SELECTION:'
     O               76      XXTINC             169 '  ,   , $0.  '
     O          E            HEADA          2
     O                                            6 'AGENT:'
     O                       XXAGT#        Z     16
     O                       XXAGNM              62
     O          E            HEADR          2
     O                                            7 'REGION:'
     O                       XXRGN#        Z     16
     O                       RGDESC              53
      *
     O          E            HEADE          1
     O                                            9 'EMPLOYER:'
     O                       XXEMP#        Z     19
     O                       XXDIV         Z     25
     O                       ENAME               68
      *
     O          E    83      HEADE          1
     O                                           12 'POLICY YEAR:'
     O                       XXFYR         Z   +001
     O                                         +004 'EFFECTIVE:'
     O                       XXPFDT            +001 '  /  /    '
     O                                         +001 '-'
     O                       XXPTDT            +001 '  /  /    '
      *
      ***** BEG ADD ******* 08/31/12 **********************************
     O          E    55      HEADE          1
     O                                           22 'CARRIER POLICY NUMBER:'
     O                       EECAP#            +001
      ***** END ADD ******* 08/31/12 **********************************
     O          E            HEAD1       1  1
     O                                          131 'COMPENSATION'
     O                                          147 'MEDICAL'
      *** Beg Add ***  11/25/13  ******************************************************
     O*****                                     163 'LEGAL'
     O              N72                         163 'LEGAL'
     O               72      charLGRH           163
      *** End Add ***  11/25/13  ******************************************************
     O                                          179 'OTHER'
     O                                          195 'TOTAL'
     O          E            DETAIL         1
     O                                            5 'CASE:'
     O                       EECASE        Z     13
     O                                           25 'CLAIMANT:'
     O                       CLNAME              54
     O                                           60 'SS#:'
     O               84      XXSS1               64
     O               84                          65 '-'
     O               84      XXSS2               67
     O               84                          68 '-'
     O               84      XXSS3               72
     O*
     O                                           78 'AGE:'
     O                       EEAGE         Z     82
     O                                           88 'SEX:'
     O                       EESEX               90
      *
     O                                          115 'GROSS PMTS THIS PERIOD'
     O                       EEGPCO        J    134
     O                       EEGPME        J    150
     O                       EEGPLE        J    166
     O                       EEGPEX        J    182
     O                       EEGPOT        J    198
     O          E            DETAIL         1
     O                                           10 'JOB CLASS:'
     O                       EECLAS        Z     16
     O                       EESPEC              18
     O                       EEEXT               24
     O                       EECLDS              56
     O                                          114 'RECOVERED THIS PERIOD'
     O                       EERCCO        J    134
     O                       EERCME        J    150
     O                       EERCLE        J    166
     O                       EERCEX        J    182
     O                       EERCOT        J    198
     O          E            DETAIL         1
     O                  41                       11 '  LOCATION:'
     O                  41   EECTG1              22
     O                  42   X1CAT2              65
     O                  42   EECTG2              76
     O                                          113 'NET PMTS THIS PERIOD'
     O                       EEPCMP        J    134
     O                       EEPMED        J    150
     O                       EEPLEG        J    166
     O                       EEPOTH        J    182
     O                       EEPTOT        J    198
     O          E            DETAIL         2
     O                                          117 'RESERVE CHGS THIS PERIOD'
     O                       EERCMP        J    134
     O                       EERMED        J    150
     O                       EERLEG        J    166
     O                       EEROTH        J    182
     O                       EERTOT        J    198
      *
     O          E            DETAIL         1
     O                                           14 'ACCIDENT DATE:'
     O                       XXACDT              25 '  /  /    '
     O                                           31 'TIME:'
     O                       EEACTM              40 '  :  :  '
     O                                           55 'DAY OF WEEK:'
     O                       EEDAYW              65
     O                                           80 'DATE REPORTED:'
     O                       XXRPDT              91 '  /  /    '
      *
     O                                          111 'GROSS PMTS TO DATE'
     O                       EEGCMP        J    134
     O                       EEGMED        J    150
     O                       EEGLEG        J    166
     O                       EEGEXP        J    182
     O                       EEGTOT        J    198
     O          E            DETAIL         1
     O                                            7 'NATURE:'
     O                       EENATR              10
     O                                           18 'PART:'
     O                       EEPART              21
     O                                           30 'CAUSE:'
     O                       EECAUS              33
     O                                           46 'LOST TIME:'
     O                       EELSTM              48
     O                                           63 'CLOSE DATE:'
     O                       EEMDY               74 '  /  /    '
     O                                           80 'FROI:'
     O                       EERCVD              91 '  /  /    '
     O                                          110 'RECOVERED TO DATE'
     O                       EERTCO        J    134
     O                       EERTME        J    150
     O                       EERTLE        J    166
     O                       EERTEX        J    182
     O                       EETROT        J    198
     O          E            DETAIL         1
     O                                           22 'DESCRIPTION OF INJURY:'
     O                       EEACDS              83
     O                                          109 'NET PMTS TO DATE'
     O                       EEDCMP        J    134
     O                       EEDMED        J    150
     O                       EEDLEG        J    166
     O                       EEDOTH        J    182
     O                       EEDTOT        J    198
     O          E            DETAIL         1
     O              N71      EEACD2              83
     O                                          117 'OUTSTN (UNPAID) RESERVES'
     O                       EEOCMP        J    134
     O                       EEOMED        J    150
     O                       EEOLEG        J    166
     O                       EEOOTH        J    182
     O                       EEOTOT        J    198
     O          E            DETAIL         2
     O                                          107 'TOTAL INCURRED'
     O                       EEICMP        J    134
     O                       EEIMED        J    150
     O                       EEILEG        J    166
     O                       EEIOTH        J    182
     O                       EEITOT        J    198
      *
     O          E            TOTYR       2  1
     O                                           12 'POLICY YEAR:'
     O                       XXFYR2            +001
     O                                         +004 'EFFECTIVE:'
     O                       XXPFDT            +001 '  /  /    '
     O                                         +001 '-'
     O                       XXPTDT            +001 '  /  /    '
      ***** BEG ADD ******* 08/31/12 **********************************
     O          E    55      TOTYR          1
     O                                           22 'CARRIER POLICY NUMBER:'
     O                       EECAP#            +001
      ***** END ADD ******* 08/31/12 **********************************
     O          E            TOTYR          1
     O                                            5 '* * *'
     O                       XXFYR2              11
     O                                           30 'POLICY YEAR TOTAL'
     O                                           37 ' * * *'
     O                                          115 'GROSS PMTS THIS PERIOD'
     O                       YRGPCO        J    134
     O                       YRGPME        J    150
     O                       YRGPLE        J    166
     O                       YRGPEX        J    182
     O                       YRGPOT        J    198
      *
     O          E            TOTYR          1
     O                                          115 'RECOVERIES THIS PERIOD'
     O                       YRRCCO        J    134
     O                       YRRCME        J    150
     O                       YRRCLE        J    166
     O                       YRRCEX        J    182
     O                       YRRCOT        J    198
      *
     O          E            TOTYR          1
     O                                          113 'NET PAID THIS PERIOD'
     O                       YRPCMP        J    134
     O                       YRPMED        J    150
     O                       YRPLEG        J    166
     O                       YRPOTH        J    182
     O                       YRPTOT        J    198
      *
      *
     O          E            TOTYR          1
     O                                          117 'RESERVE CHGS THIS PERIOD'
     O                       YRRCMP        J    134
     O                       YRRMED        J    150
     O                       YRRLEG        J    166
     O                       YRROTH        J    182
     O                       YRRTOT        J    198
     O          E            TOTYR          1
     O                                           42 'CLAIM COUNT:'
     O                                           50 'OPEN:'
     O                       XXOP2         3     56
     O                                           67 'CLOSED:'
     O                       XXCS2         3     73
     O          E            TOTYR          1
     O                                          111 'GROSS PMTS TO DATE'
     O                       YRGCMP        J    134
     O                       YRGMED        J    150
     O                       YRGLEG        J    166
     O                       YRGEXP        J    182
     O                       YRGTOT        J    198
      *
     O          E            TOTYR          1
     O                                           20 'PREMIUM THIS PERIOD:'
     O                       XYPREM        J     44
     O                                           61 'LOSS RATIO:'
     O                       XXLOSS        J     72
     O                                          110 'RECOVERED TO DATE'
     O                       YRRTCO        J    134
     O                       YRRTME        J    150
     O                       YRRTLE        J    166
     O                       YRRTEX        J    182
     O                       YRTROT        J    198
      *
     O          E            TOTYR          1
     O                                           14 'EARNED PREMIUM'
     O                                           28 'YEAR TO DATE:'
     O                       XXAMT         J     44
     O                                           61 'FREQ RATIO:'
     O                       XXFREQ        J     72
     O                                          109 'NET PAID TO DATE'
     O                       YRDCMP        J    134
     O                       YRDMED        J    150
     O                       YRDLEG        J    166
     O                       YRDOTH        J    182
     O                       YRDTOT        J    198
      *
      *
     O          E            TOTYR          1
     O                                          117 'OUTSTN (UNPAID) RESERVES'
     O                       YROCMP        J    134
     O                       YROMED        J    150
     O                       YROLEG        J    166
     O                       YROOTH        J    182
     O                       YROTOT        J    198
     O          E            TOTYR          2
     O                                          107 'TOTAL INCURRED'
     O                       YRICMP        J    134
     O                       YRIMED        J    150
     O                       YRILEG        J    166
     O                       YRIOTH        J    182
     O                       YRITOT        J    198
     O*
     O          E            TOTDV       2  1
     O                                           14 '* * DIVISION'
     O                       XXDIV         3     19
     O                                           34 'TOTALS  * * *'
     O                                          115 'GROSS PMTS THIS PERIOD'
     O                       DVGPCO        J    134
     O                       DVGPME        J    150
     O                       DVGPLE        J    166
     O                       DVGPEX        J    182
     O                       DVGPOT        J    198
     O          E            TOTDV          1
     O                                          114 'RECOVERED THIS PERIOD'
     O                       DVRCCO        J    134
     O                       DVRCME        J    150
     O                       DVRCLE        J    166
     O                       DVRCEX        J    182
     O                       DVRCOT        J    198
     O          E            TOTDV          1
     O                                          113 'NET PMTS THIS PERIOD'
     O                       DVPCMP        J    134
     O                       DVPMED        J    150
     O                       DVPLEG        J    166
     O                       DVPOTH        J    182
     O                       DVPTOT        J    198
     O          E            TOTDV          1
     O                                          117 'RESERVE CHGS THIS PERIOD'
     O                       DVRCMP        J    134
     O                       DVRMED        J    150
     O                       DVRLEG        J    166
     O                       DVROTH        J    182
     O                       DVRTOT        J    198
     O          E            TOTDV          1
     O                                           42 'CLAIM COUNT:'
     O                                           50 'OPEN:'
     O                       XXOP3         3     56
     O                                           67 'CLOSED:'
     O                       XXCS3         3     73
     O          E            TOTDV          1
     O                                          111 'GROSS PMTS TO DATE'
     O                       DVGCMP        J    134
     O                       DVGMED        J    150
     O                       DVGLEG        J    166
     O                       DVGEXP        J    182
     O                       DVGTOT        J    198
     O          E            TOTDV          1
     O                                          110 'RECOVERED T0 DATE'
     O                       DVRTCO        J    134
     O                       DVRTME        J    150
     O                       DVRTLE        J    166
     O                       DVRTEX        J    182
     O                       DVRTOT        J    198
     O          E            TOTDV          1
     O                                          109 'PAYMENTS TO DATE'
     O                       DVDCMP        J    134
     O                       DVDMED        J    150
     O                       DVDLEG        J    166
     O                       DVDOTH        J    182
     O                       DVDTOT        J    198
     O          E            TOTDV          1
     O                                           20 'PREMIUM THIS PERIOD:'
     O                       XDPREM        J     44
     O                                           61 'LOSS RATIO:'
     O                       XXLOSS        J     72
     O                                          117 'OUTSTN (UNPAID) RESERVES'
     O                       DVOCMP        J    134
     O                       DVOMED        J    150
     O                       DVOLEG        J    166
     O                       DVOOTH        J    182
     O                       DVOTOT        J    198
     O          E            TOTDV          2
     O                                           14 'EARNED PREMIUM'
     O                                           28 'YEAR TO DATE:'
     O                       XXAMT         J     44
     O                                           61 'FREQ RATIO:'
     O                       XXFREQ        J     72
     O                                          107 'TOTAL INCURRED'
     O                       DVICMP        J    134
     O                       DVIMED        J    150
     O                       DVILEG        J    166
     O                       DVIOTH        J    182
     O                       DVITOT        J    198
     O*
     O          E            TOTEM       3  1
     O                                           16 '*  EMPLOYER:'
     O                       XXEMP#        Z     25
     O                                           40 'TOTALS  * * *'
     O               78                          63 '(FOR ALL POLICY YEARS)'
     O               79                          79 '(ALL DIVISIONS)'
     O          E            TOTEM          1
     O                                          115 'GROSS PMTS THIS PERIOD'
     O                       EMGPCO        JB   134
     O                       EMGPME        JB   150
     O                       EMGPLE        JB   166
     O                       EMGPEX        JB   182
     O                       EMGPOT        JB   198
     O          E            TOTEM          1
     O                                          114 'RECOVERED THIS PERIOD'
     O                       EMRCCO        JB   134
     O                       EMRCME        JB   150
     O                       EMRCLE        JB   166
     O                       EMRCEX        JB   182
     O                       EMRCOT        JB   198
     O          E            TOTEM          1
     O                                          113 'NET PMTS THIS PERIOD'
     O                       EMPCMP        JB   134
     O                       EMPMED        JB   150
     O                       EMPLEG        JB   166
     O                       EMPOTH        JB   182
     O                       EMPTOT        JB   198
     O          E            TOTEM          1
     O                                          117 'RESERVE CHGS THIS PERIOD'
     O                       EMRCMP        JB   134
     O                       EMRMED        JB   150
     O                       EMRLEG        JB   166
     O                       EMROTH        JB   182
     O                       EMRTOT        JB   198
     O          E            TOTEM          1
     O                                           42 'CLAIM COUNT:'
     O                                           50 'OPEN:'
     O                       XXOP4         3B    56
     O                                           67 'CLOSED:'
     O                       XXCS4         3B    73
     O          E            TOTEM          1
     O                                          111 'GROSS PMTS TO DATE'
     O                       EMGCMP        JB   134
     O                       EMGMED        JB   150
     O                       EMGLEG        JB   166
     O                       EMGEXP        JB   182
     O                       EMGTOT        JB   198
     O          E            TOTEM          1
     O                                          110 'RECOVERED TO DATE'
     O                       EMRTCO        JB   134
     O                       EMRTME        JB   150
     O                       EMRTLE        JB   166
     O                       EMRTEX        JB   182
     O                       EMTROT        JB   198
     O          E            TOTEM          1
     O                                          109 'PAYMENTS TO DATE'
     O                       EMDCMP        JB   134
     O                       EMDMED        JB   150
     O                       EMDLEG        JB   166
     O                       EMDOTH        JB   182
     O                       EMDTOT        JB   198
     O          E            TOTEM          1
     O                                           20 'PREMIUM THIS PERIOD:'
     O                       XEPREM        JB    44
     O                                           61 'LOSS RATIO:'
     O                       XXLOSS        JB    72
     O                                          117 'OUTSTN (UNPAID) RESERVES'
     O                       EMOCMP        JB   134
     O                       EMOMED        JB   150
     O                       EMOLEG        JB   166
     O                       EMOOTH        JB   182
     O                       EMOTOT        JB   198
     O          E            TOTEM          2
     O                                           14 'EARNED PREMIUM'
     O                                           28 'YEAR TO DATE:'
     O                       XXAMT         JB    44
     O                                           61 'FREQ RATIO:'
     O                       XXFREQ        JB    72
     O                                          107 'TOTAL INCURRED'
     O                       EMICMP        JB   134
     O                       EMIMED        JB   150
     O                       EMILEG        JB   166
     O                       EMIOTH        JB   182
     O                       EMITOT        JB   198
     O          E            TOTG        1  1
     O               31                          12 '* * * AGENT:'
     O               31      XXAGT#        Z     19
     O               31      XXAGNM              61
     O               31                          76 'TOTALS  * * *'
     O               30                          14 '* * *  REGION:'
     O               30      XXRGN#        Z     19
     O               30      RGDESC              53
     O               30                          64 'TOTALS  * * *'
     O                                          115 'GROSS PMTS THIS PERIOD'
     O                       XGGPCO        J    134
     O                       XGGPME        J    150
     O                       XGGPLE        J    166
     O                       XGGPEX        J    182
     O                       XGGPOT        J    198
     O          E            TOTG           1
     O                                          114 'RECOVERED THIS PERIOD'
     O                       XGRCCO        J    134
     O                       XGRCME        J    150
     O                       XGRCLE        J    166
     O                       XGRCEX        J    182
     O                       XGRCOT        J    198
     O          E            TOTG           1
     O                                          113 'NET PMTS THIS PERIOD'
     O                                          113 'PAYMENTS THIS PERIOD'
     O                       XGPCMP        J    134
     O                       XGPMED        J    150
     O                       XGPLEG        J    166
     O                       XGPOTH        J    182
     O                       XGPTOT        J    198
     O          E            TOTG           1
     O                                          117 'RESERVE CHGS THIS PERIOD'
     O                       XGRCMP        J    134
     O                       XGRMED        J    150
     O                       XGRLEG        J    166
     O                       XGROTH        J    182
     O                       XGRTOT        J    198
     O          E            TOTG           1
     O                                           42 'CLAIM COUNT:'
     O                                           50 'OPEN:'
     O                       XXOP5         3     56
     O                                           67 'CLOSED:'
     O                       XXCS5         3     73
     O          E            TOTG           1
     O                                          111 'GROSS PMTS TO DATE'
     O                       XGGCMP        J    134
     O                       XGGMED        J    150
     O                       XGGLEG        J    166
     O                       XGGEXP        J    182
     O                       XGGTOT        J    198
     O          E            TOTG           1
     O                                          110 'RECOVERED TO DATE'
     O                       XGRTCO        J    134
     O                       XGRTME        J    150
     O                       XGRTLE        J    166
     O                       XGRTEX        J    182
     O                       XGTROT        J    198
     O          E            TOTG           1
     O                                          109 'PAYMENTS TO DATE'
     O                       XGDCMP        J    134
     O                       XGDMED        J    150
     O                       XGDLEG        J    166
     O                       XGDOTH        J    182
     O                       XGDTOT        J    198
     O          E            TOTG           1
     O                                           20 'PREMIUM THIS PERIOD:'
     O                       XGPREM        J     44
     O                                           61 'LOSS RATIO:'
     O                       XXLOSS        J     72
     O                                          117 'OUTSTN (UNPAID) RESERVES'
     O                       XGOCMP        J    134
     O                       XGOMED        J    150
     O                       XGOLEG        J    166
     O                       XGOOTH        J    182
     O                       XGOTOT        J    198
     O          E            TOTG           2
     O                                           14 'EARNED PREMIUM'
     O                                           28 'YEAR TO DATE:'
     O                       XXAMT         J     44
     O                                           61 'FREQ RATIO:'
     O                       XXFREQ        J     72
     O                                          107 'TOTAL INCURRED'
     O                       XGICMP        J    134
     O                       XGIMED        J    150
     O                       XGILEG        J    166
     O                       XGIOTH        J    182
     O                       XGITOT        J    198
     O          E            TOTLR       3  1
     O                                            5 '* * *'
     O                                           26 'GRAND TOTALS  * * *'
     O                                          115 'GROSS PMTS THIS PERIOD'
     O                       LRGPCO        J    134
     O                       LRGPME        J    150
     O                       LRGPLE        J    166
     O                       LRGPEX        J    182
     O                       LRGPOT        J    198
     O          E            TOTLR          1
     O                                          114 'RECOVERED THIS PERIOD'
     O                       LRRCCO        J    134
     O                       LRRCME        J    150
     O                       LRRCLE        J    166
     O                       LRRCEX        J    182
     O                       LRRCOT        J    198
     O          E            TOTLR          1
     O                                          113 'NET PMTS THIS PERIOD'
     O                       LRPCMP        J    134
     O                       LRPMED        J    150
     O                       LRPLEG        J    166
     O                       LRPOTH        J    182
     O                       LRPTOT        J    198
     O          E            TOTLR          1
     O                                          117 'RESERVE CHGS THIS PERIOD'
     O                       LRRCMP        J    134
     O                       LRRMED        J    150
     O                       LRRLEG        J    166
     O                       LRROTH        J    182
     O                       LRRTOT        J    198
     O          E            TOTLR          1
     O                                           42 'CLAIM COUNT:'
     O                                           50 'OPEN:'
     O                       XXOP6         3     56
     O                                           67 'CLOSED:'
     O                       XXCS6         3     73
     O          E            TOTLR          1
     O                                          111 'GROSS PMTS TO DATE'
     O                       LRGCMP        J    134
     O                       LRGMED        J    150
     O                       LRGLEG        J    166
     O                       LRGEXP        J    182
     O                       LRGTOT        J    198
     O          E            TOTLR          1
     O                                          110 'RECOVERED TO DATE'
     O                       LRRTCO        J    134
     O                       LRRTME        J    150
     O                       LRRTLE        J    166
     O                       LRRTEX        J    182
     O                       LRTROT        J    198
     O          E            TOTLR          1
     O                                          109 'NET PAID TO DATE'
     O                       LRDCMP        J    134
     O                       LRDMED        J    150
     O                       LRDLEG        J    166
     O                       LRDOTH        J    182
     O                       LRDTOT        J    198
     O          E            TOTLR          1
     O                                           20 'PREMIUM THIS PERIOD:'
     O                       LRPREM        J     44
     O                                           61 'LOSS RATIO:'
     O                       XXLOSS        J     72
     O                                          117 'OUTSTN (UNPAID) RESERVES'
     O                       LROCMP        J    134
     O                       LROMED        J    150
     O                       LROLEG        J    166
     O                       LROOTH        J    182
     O                       LROTOT        J    198
     O          E            TOTLR          2
     O                                           14 'EARNED PREMIUM'
     O                                           28 'YEAR TO DATE:'
     O                       XXAMT         J     44
     O                                           61 'FREQ RATIO:'
     O                       XXFREQ        J     72
     O                                          107 'TOTAL INCURRED'
     O                       LRICMP        J    134
     O                       LRIMED        J    150
     O                       LRILEG        J    166
     O                       LRIOTH        J    182
     O                       LRITOT        J    198
     OQSYSPRT1  E            HEAD#          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             116
     O                       DSDATE        Y    198
     O          E            HEAD#          1
     O                       Q1PGM               10
     O                       ORTITL             127
     O                                          192 'PAGE:'
     O                       PAGE1         Z    198
     O          E            HEAD#          1
     O               52                          87 'POLICY YEAR'
     O               52      XXFYR2              91
     O               52                         107 'AS OF'
     O               52      XXDTE2        Y    118
     O              N52                          95 'AS OF'
     O              N52      XXDTE2        Y    106
     O          E            HEAD#          2
     O                                            6 'GROUP:'
     O                       XXFND         Z     16
     O                       XFNAME              77
     O          E            HEADA#         2
     O                                            6 'AGENT:'
     O                       XXAGT#        Z     16
     O                       XXAGNM              62
     O          E            HEADR#         2
     O                                            7 'REGION:'
     O                       XXRGN#        Z     16
     O                       RGDESC              53
      *
     O          E            HEADE#         1
     O                                            9 'EMPLOYER:'
     O                       XXEMP#        Z     19
     O                       XXDIV         Z     25
     O                       ENAME               68
      *
     O          E    83      HEADE#         1
     O                                           12 'POLICY YEAR:'
     O                       XXFYR         Z   +001
     O                                         +004 'EFFECTIVE:'
     O                       XXPFDT            +001 '  /  /    '
     O                                         +001 '-'
     O                       XXPTDT            +001 '  /  /    '
      *
      ***** BEG ADD ******* 08/31/12 **********************************
     O          E    55      HEADE#         1
     O                                           22 'CARRIER POLICY NUMBER:'
     O                       EECAP#            +001
      ***** END ADD ******* 08/31/12 **********************************
     O          E            H1ADE#         1
     O                                            9 'EMPLOYER:'
     O                       @EEMP#        Z     19
     O                       @EDIV         Z     25
     O                       ENAME               68
     O          E            H2ADE#      1
     O                                           25 'NO RECORDS FOR EMP/DIV'
      *****************************************************************
     O          E            HEAD1#      1  1
     O                                          131 'COMPENSATION'
     O                                          147 'MEDICAL'
      *** Beg Add ***  11/25/13  ******************************************************
     O*****                                     163 'LEGAL'
     O              N72                         163 'LEGAL'
     O               72      charLGRH           163
      *** End Add ***  11/25/13  ******************************************************
     O                                          179 'OTHER'
     O                                          195 'TOTAL'
     O          E            DETAI#         1
     O                                            5 'CASE:'
     O                       EECASE        Z     13
     O                                           25 'CLAIMANT:'
     O                       CLNAME              54
     O                                           60 'SS#:'
     O               84      XXSS1               64
     O               84                          65 '-'
     O               84      XXSS2               67
     O               84                          68 '-'
     O               84      XXSS3               72
     O*
     O                                           78 'AGE:'
     O                       EEAGE         Z     82
     O                                           88 'SEX:'
     O                       EESEX               90
      *
     O                                          115 'GROSS PMTS THIS PERIOD'
     O                       EEGPCO        JB   134
     O                       EEGPME        JB   150
     O                       EEGPLE        JB   166
     O                       EEGPEX        JB   182
     O                       EEGPOT        JB   198
     O          E            DETAI#         1
     O                                           10 'JOB CLASS:'
     O                       EECLAS        Z     16
     O                       EESPEC              18
     O                       EEEXT               24
     O                       EECLDS              56
     O                                          114 'RECOVERED THIS PERIOD'
     O                       EERCCO        JB   134
     O                       EERCME        JB   150
     O                       EERCLE        JB   166
     O                       EERCEX        JB   182
     O                       EERCOT        JB   198
     O          E            DETAI#         1
     O                  41                       11 '  LOCATION:'
     O                  41   EECTG1              22
     O                  42   X1CAT2              65
     O                  42   EECTG2              76
     O                                          113 'NET PMTS THIS PERIOD'
     O                       EEPCMP        JB   134
     O                       EEPMED        JB   150
     O                       EEPLEG        JB   166
     O                       EEPOTH        JB   182
     O                       EEPTOT        JB   198
     O          E            DETAI#         2
     O                                          117 'RESERVE CHGS THIS PERIOD'
     O                       EERCMP        JB   134
     O                       EERMED        JB   150
     O                       EERLEG        JB   166
     O                       EEROTH        JB   182
     O                       EERTOT        JB   198
      *
     O          E            DETAI#         1
     O                                           14 'ACCIDENT DATE:'
     O                       XXACDT              25 '  /  /    '
     O                                           31 'TIME:'
     O                       EEACTM              40 '  :  :  '
     O                                           55 'DAY OF WEEK:'
     O                       EEDAYW              65
     O                                           80 'DATE REPORTED:'
     O                       XXRPDT              91 '  /  /    '
      *
     O                                          111 'GROSS PMTS TO DATE'
     O                       EEGCMP        JB   134
     O                       EEGMED        JB   150
     O                       EEGLEG        JB   166
     O                       EEGEXP        JB   182
     O                       EEGTOT        JB   198
     O          E            DETAI#         1
     O                                            7 'NATURE:'
     O                       EENATR              10
     O                                           18 'PART:'
     O                       EEPART              21
     O                                           30 'CAUSE:'
     O                       EECAUS              33
     O                                           46 'LOST TIME:'
     O                       EELSTM              48
     O                                           63 'CLOSE DATE:'
     O                       EEMDY               74 '  /  /    '
     O                                           80 'FROI:'
     O                       EERCVD              91 '  /  /    '
     O                                          110 'RECOVERED TO DATE'
     O                       EERTCO        JB   134
     O                       EERTME        JB   150
     O                       EERTLE        JB   166
     O                       EERTEX        JB   182
     O                       EETROT        JB   198
     O          E            DETAI#         1
     O                                           22 'DESCRIPTION OF INJURY:'
     O                       EEACDS              83
     O                                          109 'NET PMTS TO DATE'
     O                       EEDCMP        JB   134
     O                       EEDMED        JB   150
     O                       EEDLEG        JB   166
     O                       EEDOTH        JB   182
     O                       EEDTOT        JB   198
     O          E            DETAI#         1
     O              N71      EEACD2              83
     O                                          117 'OUTSTN (UNPAID) RESERVES'
     O                       EEOCMP        JB   134
     O                       EEOMED        JB   150
     O                       EEOLEG        JB   166
     O                       EEOOTH        JB   182
     O                       EEOTOT        JB   198
     O          E            DETAI#         2
     O                                          107 'TOTAL INCURRED'
     O                       EEICMP        JB   134
     O                       EEIMED        JB   150
     O                       EEILEG        JB   166
     O                       EEIOTH        JB   182
     O                       EEITOT        JB   198
      *
     O          E            TOTYR#      2  1
     O                                           12 'POLICY YEAR:'
     O                       XXFYR2            +001
     O                                         +004 'EFFECTIVE:'
     O                       XXPFDT            +001 '  /  /    '
     O                                         +001 '-'
     O                       XXPTDT            +001 '  /  /    '
      ***** BEG ADD ******* 08/31/12 **********************************
     O          E    55      TOTYR#         1
     O                                           22 'CARRIER POLICY NUMBER:'
     O                       EECAP#            +001
      ***** END ADD ******* 08/31/12 **********************************
     O          E            TOTYR#         1
     O                                            5 '* * *'
     O                       XXFYR2              11
     O                                           30 'POLICY YEAR TOTAL'
     O                                           37 ' * * *'
     O                                          115 'GROSS PMTS THIS PERIOD'
     O                       YRGPCO        JB   134
     O                       YRGPME        JB   150
     O                       YRGPLE        JB   166
     O                       YRGPEX        JB   182
     O                       YRGPOT        JB   198
      *
     O          E            TOTYR#         1
     O                                          115 'RECOVERIES THIS PERIOD'
     O                       YRRCCO        JB   134
     O                       YRRCME        JB   150
     O                       YRRCLE        JB   166
     O                       YRRCEX        JB   182
     O                       YRRCOT        JB   198
      *
     O          E            TOTYR#         1
     O                                          113 'NET PAID THIS PERIOD'
     O                       YRPCMP        JB   134
     O                       YRPMED        JB   150
     O                       YRPLEG        JB   166
     O                       YRPOTH        JB   182
     O                       YRPTOT        JB   198
      *
      *
     O          E            TOTYR#         1
     O                                          117 'RESERVE CHGS THIS PERIOD'
     O                       YRRCMP        JB   134
     O                       YRRMED        JB   150
     O                       YRRLEG        JB   166
     O                       YRROTH        JB   182
     O                       YRRTOT        JB   198
     O          E            TOTYR#         1
     O                                           42 'CLAIM COUNT:'
     O                                           50 'OPEN:'
     O                       XXOP2         3B    56
     O                                           67 'CLOSED:'
     O                       XXCS2         3B    73
     O          E            TOTYR#         1
     O                                          111 'GROSS PMTS TO DATE'
     O                       YRGCMP        JB   134
     O                       YRGMED        JB   150
     O                       YRGLEG        JB   166
     O                       YRGEXP        JB   182
     O                       YRGTOT        JB   198
      *
     O          E            TOTYR#         1
     O                                           20 'PREMIUM THIS PERIOD:'
     O                       XYPREM        JB    44
     O                                           61 'LOSS RATIO:'
     O                       XXLOSS        JB    72
     O                                          110 'RECOVERED TO DATE'
     O                       YRRTCO        JB   134
     O                       YRRTME        JB   150
     O                       YRRTLE        JB   166
     O                       YRRTEX        JB   182
     O                       YRTROT        JB   198
      *
     O          E            TOTYR#         1
     O                                           14 'EARNED PREMIUM'
     O                                           28 'YEAR TO DATE:'
     O                       XXAMT         JB    44
     O                                           61 'FREQ RATIO:'
     O                       XXFREQ        JB    72
     O                                          109 'NET PAID TO DATE'
     O                       YRDCMP        JB   134
     O                       YRDMED        JB   150
     O                       YRDLEG        JB   166
     O                       YRDOTH        JB   182
     O                       YRDTOT        JB   198
      *
      *
     O          E            TOTYR#         1
     O                                          117 'OUTSTN (UNPAID) RESERVES'
     O                       YROCMP        JB   134
     O                       YROMED        JB   150
     O                       YROLEG        JB   166
     O                       YROOTH        JB   182
     O                       YROTOT        JB   198
     O          E            TOTYR#         2
     O                                          107 'TOTAL INCURRED'
     O                       YRICMP        JB   134
     O                       YRIMED        JB   150
     O                       YRILEG        JB   166
     O                       YRIOTH        JB   182
     O                       YRITOT        JB   198
     O*
     O          E            TOTDV#      2  1
     O                                           14 '* * DIVISION'
     O                       XXDIV         3     19
     O                                           34 'TOTALS  * * *'
     O                                          115 'GROSS PMTS THIS PERIOD'
     O                       DVGPCO        JB   134
     O                       DVGPME        JB   150
     O                       DVGPLE        JB   166
     O                       DVGPEX        JB   182
     O                       DVGPOT        JB   198
     O          E            TOTDV#         1
     O                                          114 'RECOVERED THIS PERIOD'
     O                       DVRCCO        JB   134
     O                       DVRCME        JB   150
     O                       DVRCLE        JB   166
     O                       DVRCEX        JB   182
     O                       DVRCOT        JB   198
     O          E            TOTDV#         1
     O                                          113 'NET PMTS THIS PERIOD'
     O                       DVPCMP        JB   134
     O                       DVPMED        JB   150
     O                       DVPLEG        JB   166
     O                       DVPOTH        JB   182
     O                       DVPTOT        JB   198
     O          E            TOTDV#         1
     O                                          117 'RESERVE CHGS THIS PERIOD'
     O                       DVRCMP        JB   134
     O                       DVRMED        JB   150
     O                       DVRLEG        JB   166
     O                       DVROTH        JB   182
     O                       DVRTOT        JB   198
     O          E            TOTDV#         1
     O                                           42 'CLAIM COUNT:'
     O                                           50 'OPEN:'
     O                       XXOP3         3B    56
     O                                           67 'CLOSED:'
     O                       XXCS3         3B    73
     O          E            TOTDV#         1
     O                                          111 'GROSS PMTS TO DATE'
     O                       DVGCMP        JB   134
     O                       DVGMED        JB   150
     O                       DVGLEG        JB   166
     O                       DVGEXP        JB   182
     O                       DVGTOT        JB   198
     O          E            TOTDV#         1
     O                                          110 'RECOVERED T0 DATE'
     O                       DVRTCO        JB   134
     O                       DVRTME        JB   150
     O                       DVRTLE        JB   166
     O                       DVRTEX        JB   182
     O                       DVRTOT        JB   198
     O          E            TOTDV#         1
     O                                          109 'PAYMENTS TO DATE'
     O                       DVDCMP        JB   134
     O                       DVDMED        JB   150
     O                       DVDLEG        JB   166
     O                       DVDOTH        JB   182
     O                       DVDTOT        JB   198
     O          E            TOTDV#         1
     O                                           20 'PREMIUM THIS PERIOD:'
     O                       XDPREM        JB    44
     O                                           61 'LOSS RATIO:'
     O                       XXLOSS        JB    72
     O                                          117 'OUTSTN (UNPAID) RESERVES'
     O                       DVOCMP        JB   134
     O                       DVOMED        JB   150
     O                       DVOLEG        JB   166
     O                       DVOOTH        JB   182
     O                       DVOTOT        JB   198
     O          E            TOTDV#         2
     O                                           14 'EARNED PREMIUM'
     O                                           28 'YEAR TO DATE:'
     O                       XXAMT         JB    44
     O                                           61 'FREQ RATIO:'
     O                       XXFREQ        JB    72
     O                                          107 'TOTAL INCURRED'
     O                       DVICMP        JB   134
     O                       DVIMED        JB   150
     O                       DVILEG        JB   166
     O                       DVIOTH        JB   182
     O                       DVITOT        JB   198
     O*
     O*       E 31             TOTEM#
     O*                                  16 '*  EMPLOYER:'
     O*                        XXEMP#Z   25
     O*                                  40 'TOTALS  * * *'
     O*                78                63 '(FOR ALL POLICY YEARS)'
     O*                79                79 '(ALL DIVISIONS)'
     O*       E  1             TOTEM#
     O*                                 115 'GROSS PMTS THIS PERIOD'
     O*                        EMGPCOJB 134
     O*                        EMGPMEJB 150
     O*                        EMGPLEJB 166
     O*                        EMGPEXJB 182
     O*                        EMGPOTJB 198
     O*       E  1             TOTEM#
     O*                                 114 'RECOVERED THIS PERIOD'
     O*                        EMRCCOJB 134
     O*                        EMRCMEJB 150
     O*                        EMRCLEJB 166
     O*                        EMRCEXJB 182
     O*                        EMRCOTJB 198
     O*       E  1             TOTEM#
     O*                                 113 'NET PMTS THIS PERIOD'
     O*                        EMPCMPJB 134
     O*                        EMPMEDJB 150
     O*                        EMPLEGJB 166
     O*                        EMPOTHJB 182
     O*                        EMPTOTJB 198
     O*       E  1             TOTEM#
     O*                                 117 'RESERVE CHGS THIS PERIOD'
     O*                        EMRCMPJB 134
     O*                        EMRMEDJB 150
     O*                        EMRLEGJB 166
     O*                        EMROTHJB 182
     O*                        EMRTOTJB 198
     O*       E  1             TOTEM#
     O*                                  42 'CLAIM COUNT:'
     O*                                  50 'OPEN:'
     O*                        XXOP4 3B  56
     O*                                  67 'CLOSED:'
     O*                        XXCS4 3B  73
     O*       E  1             TOTEM#
     O*                                 111 'GROSS PMTS TO DATE'
     O*                        EMGCMPJB 134
     O*                        EMGMEDJB 150
     O*                        EMGLEGJB 166
     O*                        EMGEXPJB 182
     O*                        EMGTOTJB 198
     O*       E  1             TOTEM#
     O*                                 110 'RECOVERED TO DATE'
     O*                        EMRTCOJB 134
     O*                        EMRTMEJB 150
     O*                        EMRTLEJB 166
     O*                        EMRTEXJB 182
     O*                        EMTROTJB 198
     O*       E  1             TOTEM#
     O*                                 109 'PAYMENTS TO DATE'
     O*                        EMDCMPJB 134
     O*                        EMDMEDJB 150
     O*                        EMDLEGJB 166
     O*                        EMDOTHJB 182
     O*                        EMDTOTJB 198
     O*       E  1             TOTEM#
     O*                                  20 'PREMIUM THIS PERIOD:'
     O*                        XEPREMJB  44
     O*                                  61 'LOSS RATIO:'
     O*                        XXLOSSJB  72
     O*                                 117 'OUTSTN (UNPAID) RESERVES'
     O*                        EMOCMPJB 134
     O*                        EMOMEDJB 150
     O*                        EMOLEGJB 166
     O*                        EMOOTHJB 182
     O*                        EMOTOTJB 198
     O*       E  2             TOTEM#
     O*                                  14 'EARNED PREMIUM'
     O*                                  28 'YEAR TO DATE:'
     O*                        XXAMT JB  44
     O*                                  61 'FREQ RATIO:'
     O*                        XXFREQJB  72
     O*                                 107 'TOTAL INCURRED'
     O*                        EMICMPJB 134
     O*                        EMIMEDJB 150
     O*                        EMILEGJB 166
     O*                        EMIOTHJB 182
     O*                        EMITOTJB 198
     O          E            TOTG#       1  1
     O               31                          12 '* * * AGENT:'
     O               31      XXAGT#        Z     19
     O               31      XXAGNM              61
     O               31                          76 'TOTALS  * * *'
     O               30                          14 '* * *  REGION:'
     O               30      XXRGN#        Z     19
     O               30      RGDESC              53
     O               30                          64 'TOTALS  * * *'
     O                                          115 'GROSS PMTS THIS PERIOD'
     O                       XGGPCO        JB   134
     O                       XGGPME        JB   150
     O                       XGGPLE        JB   166
     O                       XGGPEX        JB   182
     O                       XGGPOT        JB   198
     O          E            TOTG#          1
     O                                          114 'RECOVERED THIS PERIOD'
     O                       XGRCCO        JB   134
     O                       XGRCME        JB   150
     O                       XGRCLE        JB   166
     O                       XGRCEX        JB   182
     O                       XGRCOT        JB   198
     O          E            TOTG#          1
     O                                          113 'NET PMTS THIS PERIOD'
     O                                          113 'PAYMENTS THIS PERIOD'
     O                       XGPCMP        JB   134
     O                       XGPMED        JB   150
     O                       XGPLEG        JB   166
     O                       XGPOTH        JB   182
     O                       XGPTOT        JB   198
     O          E            TOTG#          1
     O                                          117 'RESERVE CHGS THIS PERIOD'
     O                       XGRCMP        JB   134
     O                       XGRMED        JB   150
     O                       XGRLEG        JB   166
     O                       XGROTH        JB   182
     O                       XGRTOT        JB   198
     O          E            TOTG#          1
     O                                           42 'CLAIM COUNT:'
     O                                           50 'OPEN:'
     O                       XXOP5         3B    56
     O                                           67 'CLOSED:'
     O                       XXCS5         3B    73
     O          E            TOTG#          1
     O                                          111 'GROSS PMTS TO DATE'
     O                       XGGCMP        JB   134
     O                       XGGMED        JB   150
     O                       XGGLEG        JB   166
     O                       XGGEXP        JB   182
     O                       XGGTOT        JB   198
     O          E            TOTG#          1
     O                                          110 'RECOVERED TO DATE'
     O                       XGRTCO        JB   134
     O                       XGRTME        JB   150
     O                       XGRTLE        JB   166
     O                       XGRTEX        JB   182
     O                       XGTROT        JB   198
     O          E            TOTG#          1
     O                                          109 'PAYMENTS TO DATE'
     O                       XGDCMP        JB   134
     O                       XGDMED        JB   150
     O                       XGDLEG        JB   166
     O                       XGDOTH        JB   182
     O                       XGDTOT        JB   198
     O          E            TOTG#          1
     O                                           20 'PREMIUM THIS PERIOD:'
     O                       XGPREM        JB    44
     O                                           61 'LOSS RATIO:'
     O                       XXLOSS        JB    72
     O                                          117 'OUTSTN (UNPAID) RESERVES'
     O                       XGOCMP        JB   134
     O                       XGOMED        JB   150
     O                       XGOLEG        JB   166
     O                       XGOOTH        JB   182
     O                       XGOTOT        JB   198
     O          E            TOTG#          2
     O                                           14 'EARNED PREMIUM'
     O                                           28 'YEAR TO DATE:'
     O                       XXAMT         JB    44
     O                                           61 'FREQ RATIO:'
     O                       XXFREQ        JB    72
     O                                          107 'TOTAL INCURRED'
     O                       XGICMP        JB   134
     O                       XGIMED        JB   150
     O                       XGILEG        JB   166
     O                       XGIOTH        JB   182
     O                       XGITOT        JB   198
     O          E            TOTLR#      3  1
     O                                            5 '* * *'
     O                                           26 'GRAND TOTALS  * * *'
     O                                          115 'GROSS PMTS THIS PERIOD'
     O                       LRGPCO        JB   134
     O                       LRGPME        JB   150
     O                       LRGPLE        JB   166
     O                       LRGPEX        JB   182
     O                       LRGPOT        JB   198
     O          E            TOTLR#         1
     O                                          114 'RECOVERED THIS PERIOD'
     O                       LRRCCO        JB   134
     O                       LRRCME        JB   150
     O                       LRRCLE        JB   166
     O                       LRRCEX        JB   182
     O                       LRRCOT        JB   198
     O          E            TOTLR#         1
     O                                          113 'NET PMTS THIS PERIOD'
     O                       LRPCMP        JB   134
     O                       LRPMED        JB   150
     O                       LRPLEG        JB   166
     O                       LRPOTH        JB   182
     O                       LRPTOT        JB   198
     O          E            TOTLR#         1
     O                                          117 'RESERVE CHGS THIS PERIOD'
     O                       LRRCMP        JB   134
     O                       LRRMED        JB   150
     O                       LRRLEG        JB   166
     O                       LRROTH        JB   182
     O                       LRRTOT        JB   198
     O          E            TOTLR#         1
     O                                           42 'CLAIM COUNT:'
     O                                           50 'OPEN:'
     O                       XXOP6         3B    56
     O                                           67 'CLOSED:'
     O                       XXCS6         3B    73
     O          E            TOTLR#         1
     O                                          111 'GROSS PMTS TO DATE'
     O                       LRGCMP        JB   134
     O                       LRGMED        JB   150
     O                       LRGLEG        JB   166
     O                       LRGEXP        JB   182
     O                       LRGTOT        JB   198
     O          E            TOTLR#         1
     O                                          110 'RECOVERED TO DATE'
     O                       LRRTCO        JB   134
     O                       LRRTME        JB   150
     O                       LRRTLE        JB   166
     O                       LRRTEX        JB   182
     O                       LRTROT        JB   198
     O          E            TOTLR#         1
     O                                          109 'NET PAID TO DATE'
     O                       LRDCMP        JB   134
     O                       LRDMED        JB   150
     O                       LRDLEG        JB   166
     O                       LRDOTH        JB   182
     O                       LRDTOT        JB   198
     O          E            TOTLR#         1
     O                                           20 'PREMIUM THIS PERIOD:'
     O                       LRPREM        JB    44
     O                                           61 'LOSS RATIO:'
     O                       XXLOSS        JB    72
     O                                          117 'OUTSTN (UNPAID) RESERVES'
     O                       LROCMP        JB   134
     O                       LROMED        JB   150
     O                       LROLEG        JB   166
     O                       LROOTH        JB   182
     O                       LROTOT        JB   198
     O          E            TOTLR#         2
     O                                           14 'EARNED PREMIUM'
     O                                           28 'YEAR TO DATE:'
     O                       XXAMT         JB    44
     O                                           61 'FREQ RATIO:'
     O                       XXFREQ        JB    72
     O                                          107 'TOTAL INCURRED'
     O                       LRICMP        JB   134
     O                       LRIMED        JB   150
     O                       LRILEG        JB   166
     O                       LRIOTH        JB   182
     O                       LRITOT        JB   198
