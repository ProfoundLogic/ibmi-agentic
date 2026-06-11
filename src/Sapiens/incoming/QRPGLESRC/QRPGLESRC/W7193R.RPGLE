     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('WTBNDDIR')
     H*** SY266_OVR  dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE W7193R - Employer's Experience Report - CATEGORY 1 SORT
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM ALLOWS THE USER TO PRINT THE EMPLOYER'S    **
     H**     EXPERIENCE REPORT FOR A PARTICULAR FUND, FUND YEAR,     **
     H**     EMPLOYER, AGENT OR REGION WITHIN SPECIFIED SELECTION    **
     H**     CRITERIA WITHIN SELECTED DATA SORT FOR CATEGORY 1.      **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     W7191C                                                  **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
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
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     h* 08/26/14 C:307 Project: SY354                                 *
     h*                -Implement Routing Page Changes.               *
     h*                                                               *
     H* 11/25/13 C:282 PROJECT 129WSC                                 *
     H*                Replace legal literal with control field       *
     H*                for cltid 129 only.                            *
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
     H* 04/20/09 C:190 PROJECT: WC1473                                *
     H*                Add 'EESPEC' and 'EEEXT' class listing on      *
     H*                report.                                        *
     H*                                                               *
     H*  10/24/07  C:189 PROJECT-132WIT                               *
     H*                  Automate Audit Req/Loss Exp/Darts Aggregate  *
     H*                                                               *
     H* 03/01/07 C:61  PROJECT 132WHJ                                 *
     H*              - Print last 4 digits of SS# when selected.      *
     H*              - Print SS# prompt selection on routting page.   *
     H*                                                               *
     H*  11/06/06 C:136 PROJECT: 71WHI (IMPLEMENT 71WBQ)              *
     H*                 ADD LOCATION NAME AND ADDRESS ON CLAIMS COUNT *
     H*                 BREAK                                         *
     H*                                                               *
     H*  09/20/06  C:58  Project: 71WHI                               *
     H*            Add parms Lost time flag, Total incurred and Gt/Lt *
     H*                                                               *
     H*  06/21/05 C:58  PROJECT: WC1318                               *
     H*                - Add parameter 'Print SS#'                    *
     H*                                                               *
     H* 05/08/02 C:61  PROJECT WC577                                  *
     H*                Add policy effective date range to report      *
     H*                output.                                        *
     H*                                                               *
     H*  09/26/00 C:33 SEQ#: WA065/24293                              **
     H*                INCLUDE PAGE BREAK FOR EMPLOYERS WITH NO PREM  **
     H*                BUT HAVE CLAIMS                                *
     H*                                                               *
     H* 04/10/00 C:118 PROJECT: WC987                                 *
     H*                LOAD ALTERNATE COMPANY NAME FROM FUND MASTER   *
     H*                IF AVAILABLE.                                  *
     H*                                                               *
     H*  12/15/99 C:33 Y2K#: 058                                      *
     H*                ENLARGE SIZE OF XXLOSS.                        *
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
     H*  10/22/98 C:33 SEQ#: WC071/13580                              **
     H*                CHANGE LAST EMPLOYER PROCESSING                **
     H*                                                               **
     H*  07/23/98 C:33 SEQ#: WC071/12780                              **
     H*               -CHANGE FYR ENTRY PARM FROM '2' TO '3'.         **
     H*               -ADJUST OUTPUT                                  **
     H*                                                               **
     H*  05/29/98 C:33 SEQ#: WC078/11786                              **
     H*               -USE *LIKE DEFN TO SET DIV SIZE                 **
     H*               -PRINT LAST DIV IF *ALL DIV SELECTED            **
     H*                                                               **
     H*  06/16/94 C:28 PROJ: WC350                                    **
     H*                FIX SORTING.                                   **
     H*                MODIFIED  OF CODING STRUCTURE.                 **
     H*  10/10/94 C:28   CHANGE THE CONTROL FOR THE LAST EMPLOYER     **
     H*                  TOTAL LINE.                                  **
     H*                                                               **
     H*  02/06/96 C:33 PROJ: WC464                                    **
     H*                ADD SELECTION TO PRINT EMPLOYERS WITH NO CLAIMS**
     H*                                                               **
     H*  02/12/96 C:33 PROJ: WC277                                    **
     H*                ALLOW SELECTION TO PRINT ONE EMPLOYER ALL DIV  **
     H*                                                               **
     H*  05/21/96 C:33 PROJ: WC464                                    **
     H*               -CHANGE PROCESSING OF LAST EMPLOYER IF SELECTION**
     H*                IS FOR EMPLOYERS WITH CLAIMS ONLY.             **
     H*               -COSMETIC CHANGES TO THE OUTPUT.                **
     H*                                                               **
     H*  06/03/96 C:33 PROJ: WC464                                    **
     H*                CHANGE NO CLAIM PROCESSING                     **
     H*                                                               *
     H* 05/15/97 C:62 PROJECT WC640                                   *
     H*               "CAT1" TO "LOCATION"; "CAT2" = CXCAT2;          *
     H*               "ADJUSTER" TO CXADJ.                            *
     H*                                                               *
     H*****************************************************************
     H*
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
     H*     84   -      PRINT SS#
     H*                                                               *
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**                                                             **
     H**    WXEEL2 (PEE01): CO#/FND/EMP#/DIV/FYR/CTG1/OPCL           **
     H**     OR WXEEL4 (PEE01): CO#/FND/RGN#/EMP#/DIV/FYR/CTG1/OPCL  **
     H**     OR WXEEL5 (PEE01): CO#/FND/AGT#/EMP#/DIV/FYR/CTG1/OPCL  **
     H**    WMEML (PEM01): CO#/FND/EMP#/DIV                          **
     H**    WMAGL (PAG01): CO#/FEIN/MOD                              **
     H**    WMAAL1 (PAA01):CO#/FND/AGT#                              **
     H**    SMCOL (PCO01): CO#                                       **
     H*     SMORL  (POR01) ORPNAM,ORFIL#                              *
     H**    WMUPL (PUP01): USER                                      **
     H**    WMFNL (PFN01): CO#/FND                                   **
     H*****************************************************************
     F*
     FWXEEL2    IF   E           K DISK
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
     FWMAAL1    IF   E           K DISK
     F* * * AGENT ASSIGNMENT FILE
     F*
     FWMAGL     IF   E           K DISK
     F* * * AGENT MASTER FILE
     F*
     FWMALL     IF   E           K DISK
     FWMCXL     IF   E           K DISK
     F* * * Claim/DARTS Control File
     F*
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOA)
     F                                     USROPN
     F* * * Output Printer
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
     D*
     D                 DS
     D  EEZIP                  1      9  0
     D  XXZIP1                 1      5  0
     D  XXZIP2                 1      9  0
     D*
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
      ***  Begin Add  ***  06/09/20  *******************
     D/copy sPrMask
      ***  End   Add  ***  06/09/20  *******************

     C*****************************************************************
     C*   LIKE DEFINES
     C*****************************************************************
     C*
     C     *LIKE         DEFINE    EECTG1        XXCTG1
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
     C     *LIKE         DEFINE    ALNAME        XXNAME
     C     *LIKE         DEFINE    ALNAME        XXPAD1
     C     *LIKE         DEFINE    ALNAME        XXPAD2
     C     *LIKE         DEFINE    ALNAME        XXPCSZ
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
     C*   KEY LIST FOR THE REGION MASTER FILE
     C*
     C     RGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXRGN#
     C*
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
     C*  LOCATION FILE
     C*
     C     ALKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXCTG1
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
     C     QQMARK        IFNE      *BLANKS
     C                   MOVE      *ON           *IN69
     C                   ENDIF
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
     C*
     C* FIRST CYCLE PROCESSING
     C     *IN69         IFEQ      *ON
     C                   OPEN      QSYSPRT1
     C                   ENDIF
     C*
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
     C                   MOVE      EECTG1        XXCTG1
     C   78              MOVE      EEFYR         XXFYR
     C                   MOVE      EEFND         XXFND
     C   78              MOVE      EEFYR         XXFYR2
     C   79              MOVE      EEEMP#        XXEMP#
     C   81              MOVE      EEDIV         XXDIV
     C   80              MOVE      EEAGT#        XXAGT#
     C                   MOVE      EERGN#        XXRGN#
     C                   MOVEL     EERDES        RGDESC           31
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
     C**  NEW AGENT OR REGION
     C*
     C     *IN30         IFEQ      '1'
     C     *IN31         OREQ      '1'
     C     EERGN#        IFNE      XXRGN#
     C     *IN30         ANDEQ     '1'
     C     XXOP5         IFNE      *ZERO
     C     XXCS5         ORNE      *ZERO
     C     XGAMT         ORNE      *ZERO
     C                   EXSR      SR2ND                                        CTG SBR
     C                   EXSR      SR3TH                                        YR  SBR
     C                   EXSR      SR4TH                                        DIV SBR
     C                   EXSR      SR5TH                                        EMP SBR
     C                   EXSR      SR6TH                                        AG/RG SBR
     C                   END
     C                   MOVE      EECTG1        XXCTG1
     C                   MOVE      EEFYR         XXFYR2
     C                   MOVE      EEFYR         XXFYR
     C                   MOVE      EEDIV         XXDIV
     C                   MOVE      EEEMP#        XXEMP#
     C                   MOVE      EERGN#        XXRGN#
     C                   MOVEL     EERDES        RGDESC
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
     C                   Z-ADD     *ZERO         PAGE
     C                   MOVE      '1'           *IN22
     C                   ENDIF                                                  PGBRK=Y
     C*
     C                   END
     C*
     C     EEAGT#        IFNE      XXAGT#
     C     *IN31         ANDEQ     '1'
     C     XXOP5         IFNE      *ZERO
     C     XXCS5         ORNE      *ZERO
     C     XGAMT         ORNE      *ZERO
     C                   EXSR      SR2ND                                        CTG SBR
     C                   EXSR      SR3TH                                        YR  SBR
     C                   EXSR      SR4TH                                        DIV SBR
     C                   EXSR      SR5TH                                        EMP SBR
     C                   EXSR      SR6TH                                        AG/RG SBR
     C                   END
     C                   MOVE      EECTG1        XXCTG1
     C                   MOVE      EEFYR         XXFYR2
     C                   MOVE      EEFYR         XXFYR
     C                   MOVE      EEDIV         XXDIV
     C                   MOVE      EEEMP#        XXEMP#
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
     C                   Z-ADD     *ZERO         PAGE
     C                   MOVE      '1'           *IN22
     C                   ENDIF                                                  PGBRK=Y
     C*
     C                   END
     C                   END
     C*
     C**  NEW EMPLOYER - WHEN ALL EMPLOYER WAS SELECTED
     C     EEEMP#        IFNE      XXEMP#
     C     *IN79         ANDEQ     '1'
     C     XXOP4         IFNE      *ZERO
     C     XXCS4         ORNE      *ZERO
     C     XEAMT         ORNE      *ZERO
     C                   EXSR      SR2ND                                        CTG SBR
     C                   EXSR      SR3TH                                        YR  SBR
     C                   EXSR      SR4TH                                        DIV SBR
     C                   EXSR      SR5TH                                        EMP SBR
     C                   END
     C                   MOVE      EEFYR         XXFYR2
     C                   MOVE      EEFYR         XXFYR
     C                   MOVE      EEDIV         XXDIV
     C                   MOVE      EECTG1        XXCTG1
     C                   MOVE      EEEMP#        XXEMP#
     C*
     C*  GET EMPLOYER NAME
     C     EMKY          CHAIN     PEM01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     EMNAME        ENAME            40
     C                   ELSE
     C                   MOVE      *BLANK        ENAME
     C                   END
     C                   Z-ADD     *ZERO         PAGE
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
     C*
     C**  NEW DIVISION
     C     EEDIV         IFNE      XXDIV
     C     XXOP3         IFNE      *ZERO
     C     XXCS3         ORNE      *ZERO
     C     XDAMT         ORNE      *ZERO
     C                   EXSR      SR2ND                                        CTG SBR
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
     C                   MOVE      EECTG1        XXCTG1
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
     C                   Z-ADD     *ZERO         PAGE
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
     C                   EXSR      SR2ND                                        CTG SBR
     C                   EXSR      SR3TH                                        YR  SBR
     C                   END
     C                   MOVE      EEFYR         XXFYR
     C                   MOVE      EEFYR         XXFYR2
     C                   MOVE      EECTG1        XXCTG1
     C*
     C     *IN81         IFEQ      *ON
     C     XXDIV         OREQ      XYDIV
     C*
     C                   EXSR      SRPRM
     C*
     C                   ENDIF                                                  *IN81=*ON
     C*
     C     PGBRK         IFEQ      'Y'
     C                   Z-ADD     *ZERO         PAGE
     C                   MOVE      '1'           *IN22
     C                   ENDIF                                                  PGBRK=Y
     C*
     C                   END
     C*
     C**  NEW CATEGORY 1
     C     EECTG1        IFNE      XXCTG1
     C                   EXSR      SR2ND
     C                   MOVE      EECTG1        XXCTG1
     C                   MOVE      '1'           *IN22
     C                   Z-ADD     *ZERO         PAGE
     C                   END
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
     C                   PARM      'Y'           ##DIFL            1
     C                   PARM      Q1PGM         ##IPGM           10
     C                   PARM      'Y'           ##FL01            1
     C                   PARM      'Y'           ##FL02            1
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
     C**
     C**  DETAIL
     C                   MOVEL     EECLMN        CLNAME           28
     C                   Z-ADD     EECLDT        EEMDY             8 0
     C**
     C*  IF CATEGORY FIELDS ARE BLANK, DO NOT PRINT
     C     EECTG1        IFNE      *BLANK
     C                   MOVE      '1'           *IN41
     C                   ELSE
     C                   MOVE      '0'           *IN41
     C                   END
     C**
     C     EECTG2        IFNE      *BLANK
     C                   MOVE      '1'           *IN42
     C                   ELSE
     C                   MOVE      '0'           *IN42
     C                   END
     C**
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
     C*  ACCUMULATE TOTALS FOR CATEGORY
     C*
     C                   ADD       EEPCMP        XXPCMP            9 2
     C                   ADD       EEPMED        XXPMED            9 2
     C                   ADD       EEPLEG        XXPLEG            9 2
     C                   ADD       EEPOTH        XXPOTH            9 2
     C                   ADD       EEPTOT        XXPTOT            9 2
     C*
     C                   ADD       EERCMP        XXRCMP            9 2
     C                   ADD       EERMED        XXRMED            9 2
     C                   ADD       EERLEG        XXRLEG            9 2
     C                   ADD       EEROTH        XXROTH            9 2
     C                   ADD       EERTOT        XXRTOT            9 2
     C*
     C                   ADD       EEDCMP        XXDCMP            9 2
     C                   ADD       EEDMED        XXDMED            9 2
     C                   ADD       EEDLEG        XXDLEG            9 2
     C                   ADD       EEDOTH        XXDOTH            9 2
     C                   ADD       EEDTOT        XXDTOT            9 2
     C*
     C                   ADD       EEOCMP        XXOCMP            9 2
     C                   ADD       EEOMED        XXOMED            9 2
     C                   ADD       EEOLEG        XXOLEG            9 2
     C                   ADD       EEOOTH        XXOOTH            9 2
     C                   ADD       EEOTOT        XXOTOT            9 2
     C*
     C                   ADD       EEICMP        XXICMP            9 2
     C                   ADD       EEIMED        XXIMED            9 2
     C                   ADD       EEILEG        XXILEG            9 2
     C                   ADD       EEIOTH        XXIOTH            9 2
     C                   ADD       EEITOT        XXITOT            9 2
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
     C*
     C*** Begin Add *** 04/24/18 ************************************
     C*****EECASE        IFNE      *ZEROS
     C                   If        EEcase <> 0 And ((qqSelO = 'Y'
     C                             And EEopCl = ' ') Or (qqSelC = 'Y'
     C                             And EEopCl = 'C'))
     C*** End   Add *** 04/24/18 ************************************
     C*
     C     *IN22         IFEQ      '1'
     C                   Z-ADD     *ZERO         PAGE
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
     C**
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
     C                   Z-ADD     EEACDT        XXACDT            8 0
     C                   Z-ADD     EERPDT        XXRPDT            8 0
     C*
     C     EESORT        IFNE      *HIVAL
     C                   EXCEPT    DETAIL
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    DETAI#
     C                   END
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
     C*
     C*
     C** PRINT EMPLOYERS WITH AND W/O CLAIMS
     C*
     C                   ELSE
     C*
     C     *IN22         IFEQ      *ON
     C*
     C                   Z-ADD     *ZERO         PAGE
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
     C*
     C                   Z-ADD     EEACDT        XXACDT            8 0
     C                   Z-ADD     EERPDT        XXRPDT            8 0
     C*
     C     EESORT        IFNE      *HIVAL
     C*
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
     C                   MOVE      '0'           *INOA
     C*
     C                   ENDIF                                                  *INOF=*ON
     C                   ENDIF                                                  EESORT<>*HIVAL
     C                   ENDIF                                                  EECASE<>*ZEROS
     C                   ENDIF                                                  QQEMPC='N'
     C*
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
     C*
     C                   READ      PEE01                                  60
     C                   MOVE      '1'           *IN88
     C*
     C                   END
     C*
     C                   EXSR      SR2ND                                        CTG SBR
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
      *
     C                   EXSR      SREFDT
     C   35              EXCEPT    HEADE
     C     *IN69         IFEQ      *ON
     C   35              EXCEPT    HEADE#
     C                   MOVE      '1'           HDRFLG
     C                   END
     C                   ENDIF                                                  QQEMPC='Y'
     C     *IN30         IFEQ      '1'
     C     *IN80         OREQ      '1'
     C                   EXSR      SR6TH                                        AG/RG SBR
     C                   END
     C     XXCS6         ADD       XXOP6         XXCLM             5 0
     C                   MOVE      LRAMT         XXAMT            11 2
     C                   MOVE      LRITOT        XXTOT            11 2
     C                   EXSR      SRLFR                                        FRQ/LSS
     C                   MOVE      '0'           *IN52
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   EXCEPT    TOTLR
     C     *IN69         IFEQ      '1'                                          DART <> BLANKS
     C                   EXCEPT    HEAD#
     C                   EXCEPT    HEAD1#
     C     HDRFLG        IFEQ      '0'
     C                   EXCEPT    HEADE#
     C                   ENDIF
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
     C                   PARM      'Y'           ##DIFL            1
     C                   PARM      Q1PGM         ##IPGM           10
     C                   PARM      'Y'           ##FL01            1
     C                   PARM      'Y'           ##FL02            1
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
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain orky POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
     C*==================================================================*
     C*   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
     C*==================================================================*
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
     C     QQDIV         COMP      *BLANK                                 81    ALL DIV
     C                   MOVE      QQAGT#        XXAGT#
     C*
     C     XXTINC        COMP      *ZERO                              7676       $ INC SEL
     C     QQGTLT        COMP      'G'                                    33     GREATER THAN
     C     QQSORT        COMP      'Z'                                    77     ZIP SORT
     C*
     C     XXFYR         COMP      0                                      78    ALL FYR
     C     XXEMP#        COMP      0                                      79    ALL EMP#
     C     XXAGT#        COMP      0                                      80    ALL AGTS
     C*
     C     UPKY          CHAIN     WMUPL                              60
     C*
     C* * * INITIALIZE FIELDS
     C                   TIME                    DSDTTM
     C*
     C     COKY          CHAIN     SMCOL                              60
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*
     C*  CENTER TEXT IN FIELD
     C*
     C*
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
     C*
     C*  CENTER TEXT IN FIELD
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        CONAME
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
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN31
     C                   ELSE
     C                   MOVE      '0'           *IN31
     C                   END
     C     *IN30         IFNE      '1'
     C     XXAGT#        ANDNE     0
     C                   MOVE      '1'           *IN31
     C                   END
     C*   GET THE DISTRIBUTION INFORMATION
     C*
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
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
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
     C                   END
     C*** Beg Add ***  11/25/13  ******************************************************
      /Free
       RtvCltID(CltId);
       charLGRH = *Blanks;
       *IN71 = *Off;
       If CltId = '129';
          Chain keyWMcxl pcx01;
          If %Found(wmcxl);
             If CXlgrh <> *Blanks;
                charLGRH = CXlgrh;
             Else;
                 charLGRH = 'Legal';
             EndIf;
             *IN71 = *On;
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
     C*   CATEGORY ONE  -  BREAK           ** Subroutine ** S R 2 ND     *
     C*==================================================================*
     C*
     CSR   SR2ND         BEGSR
     C**
     C     XXOP1         IFNE      *ZEROS
     C     XXCS1         ORNE      *ZEROS
     C*
     C     QQEMPC        IFEQ      'Y'
     C*
     C     QQEMPC        OREQ      'N'
     C     XXOP1         ANDNE     *ZEROS
     C*** Begin Add *** 04/24/18 ************************************
     C     QQSELO        ANDEQ     'Y'
     C*** End   Add *** 04/24/18 ************************************
     C     QQEMPC        OREQ      'N'
     C     XXCS1         ANDNE     *ZEROS
     C*** Begin Add *** 04/24/18 ************************************
     C     QQSELC        ANDEQ     'Y'
     C*** End   Add *** 04/24/18 ************************************
     C*
     C*
     C     *INOF         IFEQ      '1'
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
     C                   MOVE      '0'           *INOF
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   MOVEL     *BLANKS       XXNAME
     C                   MOVEL     *BLANKS       XXPAD1
     C                   MOVEL     *BLANKS       XXPAD2
     C                   MOVEL     *BLANKS       XXPCSZ
     C*
     C     ALKY          CHAIN     PAL01                              61
     C*
     C     *IN61         IFEQ      *OFF
     C*
     C                   MOVEL(P)  ALNAME        XXNAME
     C                   MOVEL(P)  ALPAD1        XXPAD1
     C                   MOVEL(P)  ALPAD2        XXPAD2
     C*
     C     ALPST         IFNE      *BLANK
     C     ALPCTY        CAT       ',':0         XXPCSZ
     C     XXPCSZ        CAT       ALPST:1       XXPCSZ
     C                   MOVEL     ALPZIP        XXPZIP            9
     C     XXPCSZ        CAT       XXPZIP:2      XXPCSZ
     C                   ENDIF                                                  XLPST<>*BLANK
     C*
     C                   ENDIF                                                  *IN61=*OFF
     C*
     C                   EXCEPT    TOTCG
     C     *IN69         IFEQ      '1'                                          DART <> BLANKS
     C                   EXCEPT    TOTCG#
     C                   END
     C                   ENDIF                                                  QQEMPC='Y'
     C                   END
     C                   ENDSR
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
     C                   Z-ADD     *ZERO         PAGE
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
     C                   ENDSR
     C*
      *==================================================================*
      *   DIVISION      -  BREAK           ** Subroutine ** S R 4 TH     *
      *==================================================================*
     C*
     CSR   SR4TH         BEGSR
     C*
     C     XXCS3         IFNE      *ZEROS
     C     XXOP3         ORNE      *ZEROS
     C     XDAMT         ORNE      *ZEROS
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
     C                   ENDSR
     C*
     C*==================================================================*
     C*   EMPLOYER / ALL DIV/YR  BREAK       ** Subroutine ** S R 5 TH   *
     C*==================================================================*
     C*
     CSR   SR5TH         BEGSR
     C*
     C     *IN79         IFEQ      '1'
     C     *IN22         IFEQ      '1'
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
     C                   Z-ADD     *ZERO         PAGE
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
     C                   ENDIF                                                  QQEMPC='Y'
     C                   MOVE      '0'           *IN22
     C                   END
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
     C                   ENDIF                                                  QQEMPC='Y'
     C                   END
     C                   Z-ADD     *ZERO         XXCS4
     C                   Z-ADD     *ZERO         XXOP4
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
     C                   MOVE      '0'           *IN52
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
      *
      *
     C***************************************************************
     C*
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
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
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
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
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
     O                                           22 'LT/Med/Both (L/M/B):'
     O                       QQLTFL              36
     O          E            ROUTE       1  1
     O                                           17 'Claims based on'
     O                     33                    31 'greater than:'
     O                    N33                    31 'less than:   '
     O          E            ROUTE          1
     O                                           24 'Tot incurred amount:  '
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
     O               52      XXFYR2              94
     O               52                         109 'AS OF'
     O               52      XXDTE2        Y    120
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
     O              N71                         163 'LEGAL'
     O               71      charLGRH           163
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
     O                                          113 'PAYMENTS THIS PERIOD'
     O                       EEPCMP        JB   134
     O                       EEPMED        JB   150
     O                       EEPLEG        JB   166
     O                       EEPOTH        JB   182
     O                       EEPTOT        JB   198
     O          E            DETAIL         1
     O                                           10 'JOB CLASS:'
     O                       EECLAS        Z     16
     O                       EESPEC              18
     O                       EEEXT               24
     O                       EECLDS              56
     O                                           78 'ADJUSTER:'
     O                       EEFEAD              82
     O                                          117 'RESERVE CHGS THIS PERIOD'
     O                       EERCMP        JB   134
     O                       EERMED        JB   150
     O                       EERLEG        JB   166
     O                       EEROTH        JB   182
     O                       EERTOT        JB   198
     O          E            DETAIL         1
     O                  41                       11 '  LOCATION:'
     O                  41   EECTG1              22
     O                  42   X1CAT2              65
     O                  42   EECTG2              76
     O          E            DETAIL         1
     O                                           14 'ACCIDENT DATE:'
     O                       XXACDT              25 '  /  /    '
     O                                           32 'TIME:'
     O                       EEACTM              41 '  :  :  '
     O                                           54 'DAY OF WEEK:'
     O                       EEDAYW              65
     O                                           80 'DATE REPORTED:'
     O                       XXRPDT              91 '  /  /    '
     O                                          109 'PAYMENTS TO DATE'
     O                       EEDCMP        JB   134
     O                       EEDMED        JB   150
     O                       EEDLEG        JB   166
     O                       EEDOTH        JB   182
     O                       EEDTOT        JB   198
     O          E            DETAIL         1
     O                                            7 'NATURE:'
     O                       EENATR              10
     O                                           18 'PART:'
     O                       EEPART              21
     O                                           30 'CAUSE:'
     O                       EECAUS              33
     O                                           46 'LOST TIME:'
     O                       EELSTM              48
     O                                           65 'CLOSE DATE:'
     O                       EEMDY               75 '  /  /    '
     O                                          117 'OUTSTN (UNPAID) RESERVES'
     O                       EEOCMP        JB   134
     O                       EEOMED        JB   150
     O                       EEOLEG        JB   166
     O                       EEOOTH        JB   182
     O                       EEOTOT        JB   198
     O          E            DETAIL         3
     O                                           22 'DESCRIPTION OF INJURY:'
     O                       EEACDS              83
     O                                          107 'TOTAL INCURRED'
     O                       EEICMP        JB   134
     O                       EEIMED        JB   150
     O                       EEILEG        JB   166
     O                       EEIOTH        JB   182
     O                       EEITOT        JB   198
     O          E            TOTCG       1  1
     O                                           17 '* * * * LOCATION:'
     O                       XXCTG1              29
     O                                           43 'TOTALS * * *'
     O                                          113 'PAYMENTS THIS PERIOD'
     O                       XXPCMP        JB   134
     O                       XXPMED        JB   150
     O                       XXPLEG        JB   166
     O                       XXPOTH        JB   182
     O                       XXPTOT        JB   198
     O          E            TOTCG          1
     O                                          117 'RESERVE CHGS THIS PERIOD'
     O                       XXRCMP        JB   134
     O                       XXRMED        JB   150
     O                       XXRLEG        JB   166
     O                       XXROTH        JB   182
     O                       XXRTOT        JB   198
     O          E            TOTCG          1
     O                                           42 'CLAIM COUNT:'
     O                                           50 'OPEN:'
     O                       XXOP1         3B    56
     O                                           67 'CLOSED:'
     O                       XXCS1         3B    73
     O          E            TOTCG          1
     O                       XXNAME              70
     O                                          109 'PAYMENTS TO DATE'
     O                       XXDCMP        JB   134
     O                       XXDMED        JB   150
     O                       XXDLEG        JB   166
     O                       XXDOTH        JB   182
     O                       XXDTOT        JB   198
     O          E            TOTCG          1
     O                       XXPAD1              70
     O                                          117 'OUTSTN (UNPAID) RESERVES'
     O                       XXOCMP        JB   134
     O                       XXOMED        JB   150
     O                       XXOLEG        JB   166
     O                       XXOOTH        JB   182
     O                       XXOTOT        JB   198
     O          E            TOTCG          1
     O                       XXPAD2              70
     O                                          107 'TOTAL INCURRED'
     O                       XXICMP        JB   134
     O                       XXIMED        JB   150
     O                       XXILEG        JB   166
     O                       XXIOTH        JB   182
     O                       XXITOT        JB   198
     O          E            TOTCG          2
     O                       XXPCSZ              70
     O          E            TOTYR       1  1
     O                                           12 'POLICY YEAR:'
     O                       XXFYR2            +001
     O                                         +004 'EFFECTIVE:'
     O                       XXPFDT            +001 '  /  /    '
     O                                         +001 '-'
     O                       XXPTDT            +001 '  /  /    '
      *
      ***** BEG ADD ******* 08/31/12 **********************************
     O          E    55      TOTYR          1
     O                                           22 'CARRIER POLICY NUMBER:'
     O                       EECAP#            +001
      ***** END ADD ******* 08/31/12 **********************************
      *
     O          E            TOTYR          1
     O                                            5 '* * *'
     O                       XXFYR2        Z     11
     O                                           30 'POLICY YEAR TOTAL'
     O                                           37 ' * * *'
     O                                          113 'PAYMENTS THIS PERIOD'
     O                       YRPCMP        JB   134
     O                       YRPMED        JB   150
     O                       YRPLEG        JB   166
     O                       YRPOTH        JB   182
     O                       YRPTOT        JB   198
     O          E            TOTYR          1
     O                                          117 'RESERVE CHGS THIS PERIOD'
     O                       YRRCMP        JB   134
     O                       YRRMED        JB   150
     O                       YRRLEG        JB   166
     O                       YRROTH        JB   182
     O                       YRRTOT        JB   198
     O          E            TOTYR          1
     O                                           42 'CLAIM COUNT:'
     O                                           50 'OPEN:'
     O                       XXOP2         3B    56
     O                                           67 'CLOSED:'
     O                       XXCS2         3B    73
     O          E            TOTYR          1
     O                                          109 'PAYMENTS TO DATE'
     O                       YRDCMP        JB   134
     O                       YRDMED        JB   150
     O                       YRDLEG        JB   166
     O                       YRDOTH        JB   182
     O                       YRDTOT        JB   198
     O          E            TOTYR          1
     O                                           20 'PREMIUM THIS PERIOD:'
     O                       XYPREM        JB    44
     O                                           61 'LOSS RATIO:'
     O                       XXLOSS        JB    72
     O                                          117 'OUTSTN (UNPAID) RESERVES'
     O                       YROCMP        JB   134
     O                       YROMED        JB   150
     O                       YROLEG        JB   166
     O                       YROOTH        JB   182
     O                       YROTOT        JB   198
     O          E            TOTYR          2
     O                                           14 'EARNED PREMIUM'
     O                                           28 'YEAR TO DATE:'
     O                       XXAMT         JB    44
     O                                           61 'FREQ RATIO:'
     O                       XXFREQ        JB    72
     O                                          107 'TOTAL INCURRED'
     O                       YRICMP        JB   134
     O                       YRIMED        JB   150
     O                       YRILEG        JB   166
     O                       YRIOTH        JB   182
     O                       YRITOT        JB   198
     O*
     O          E            TOTDV       2  1
     O                                           14 '* * * DIVISION'
     O                       XXDIV         3     19
     O                                           34 'TOTALS  * * *'
     O                                          113 'PAYMENTS THIS PERIOD'
     O                       DVPCMP        JB   134
     O                       DVPMED        JB   150
     O                       DVPLEG        JB   166
     O                       DVPOTH        JB   182
     O                       DVPTOT        JB   198
     O          E            TOTDV          1
     O                                          117 'RESERVE CHGS THIS PERIOD'
     O                       DVRCMP        JB   134
     O                       DVRMED        JB   150
     O                       DVRLEG        JB   166
     O                       DVROTH        JB   182
     O                       DVRTOT        JB   198
     O          E            TOTDV          1
     O                                           42 'CLAIM COUNT:'
     O                                           50 'OPEN:'
     O                       XXOP3         3B    56
     O                                           67 'CLOSED:'
     O                       XXCS3         3B    73
     O          E            TOTDV          1
     O                                          109 'PAYMENTS TO DATE'
     O                       DVDCMP        JB   134
     O                       DVDMED        JB   150
     O                       DVDLEG        JB   166
     O                       DVDOTH        JB   182
     O                       DVDTOT        JB   198
     O          E            TOTDV          1
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
     O          E            TOTDV          2
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
     O          E            TOTEM       3  1
     O                                           16 '* * *  EMPLOYER:'
     O                       XXEMP#        Z     26
     O                                           41 'TOTALS  * * *'
     O               78                          63 '(FOR ALL POLICY YEARS)'
     O               79                          80 '(ALL DIVISIONS)'
     O                                          113 'PAYMENTS THIS PERIOD'
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
     O                                          113 'PAYMENTS THIS PERIOD'
     O                       XGPCMP        JB   134
     O                       XGPMED        JB   150
     O                       XGPLEG        JB   166
     O                       XGPOTH        JB   182
     O                       XGPTOT        JB   198
     O          E            TOTG           1
     O                                          117 'RESERVE CHGS THIS PERIOD'
     O                       XGRCMP        JB   134
     O                       XGRMED        JB   150
     O                       XGRLEG        JB   166
     O                       XGROTH        JB   182
     O                       XGRTOT        JB   198
     O          E            TOTG           1
     O                                           42 'CLAIM COUNT:'
     O                                           50 'OPEN:'
     O                       XXOP5         3B    56
     O                                           67 'CLOSED:'
     O                       XXCS5         3B    73
     O          E            TOTG           1
     O                                          109 'PAYMENTS TO DATE'
     O                       XGDCMP        JB   134
     O                       XGDMED        JB   150
     O                       XGDLEG        JB   166
     O                       XGDOTH        JB   182
     O                       XGDTOT        JB   198
     O          E            TOTG           1
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
     O          E            TOTG           2
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
     O          E            TOTLR       1  1
     O                                            5 '* * *'
     O                                           26 'GRAND TOTALS  * * *'
     O                                          113 'PAYMENTS THIS PERIOD'
     O                       LRPCMP        JB   134
     O                       LRPMED        JB   150
     O                       LRPLEG        JB   166
     O                       LRPOTH        JB   182
     O                       LRPTOT        JB   198
     O          E            TOTLR          1
     O                                          117 'RESERVE CHGS THIS PERIOD'
     O                       LRRCMP        JB   134
     O                       LRRMED        JB   150
     O                       LRRLEG        JB   166
     O                       LRROTH        JB   182
     O                       LRRTOT        JB   198
     O          E            TOTLR          1
     O                                           42 'CLAIM COUNT:'
     O                                           50 'OPEN:'
     O                       XXOP6         3B    56
     O                                           67 'CLOSED:'
     O                       XXCS6         3B    73
     O          E            TOTLR          1
     O                                          109 'PAYMENTS TO DATE'
     O                       LRDCMP        JB   134
     O                       LRDMED        JB   150
     O                       LRDLEG        JB   166
     O                       LRDOTH        JB   182
     O                       LRDTOT        JB   198
     O          E            TOTLR          1
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
     O          E            TOTLR          2
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
     O               52      XXFYR2              94
     O               52                         109 'AS OF'
     O               52      XXDTE2        Y    120
     O              N52                          95 'AS OF'
     O              N52      XXDTE2        Y    106
     O          E            HEAD#          2
     O                                            6 'GROUP:'
     O                       XXFND         Z     16
     O                       XFNAME              77
     O                                          110 'SELECT CLAIMS'
     O                     33                   123 'GREATER THAN'
     O                    N33                   123 'LESS THAN   '
     O               76                         138 'TOTAL INCURRED'
     O               76                         149 'SELECTION:'
     O               76      XXTINC             169 '  ,   , $0.  '
     O          E            HEADA#         2
     O                                            6 'AGENT:'
     O                       XXAGT#        Z     16
     O                       XXAGNM              62
     O          E            HEADR#         2
     O                                            7 'REGION:'
     O                       XXRGN#        Z     16
     O                       RGDESC              53
     O          E            HEADE#         1
     O                                            9 'EMPLOYER:'
     O                       XXEMP#        Z     19
     O                       XXDIV         Z     25
     O                       ENAME               68
      *
     O          E            H1ADE#         1
     O                                            9 'EMPLOYER:'
     O                       @EEMP#        Z     19
     O                       @EDIV         Z     25
     O                       ENAME               68
     O          E            H2ADE#      1
     O                                           25 'NO RECORDS FOR EMP/DIV'
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
     O          E            HEAD1#      1  1
     O                                          131 'COMPENSATION'
     O                                          147 'MEDICAL'
      *** Beg Add ***  11/25/13  ******************************************************
     O*****                                     163 'LEGAL'
     O              N71                         163 'LEGAL'
     O               71      charLGRH           163
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
     O                                          113 'PAYMENTS THIS PERIOD'
     O                       EEPCMP        JB   134
     O                       EEPMED        JB   150
     O                       EEPLEG        JB   166
     O                       EEPOTH        JB   182
     O                       EEPTOT        JB   198
     O          E            DETAI#         1
     O                                           10 'JOB CLASS:'
     O                       EECLAS        Z     16
     O                       EESPEC              18
     O                       EEEXT               24
     O                       EECLDS              56
     O                                           78 'ADJUSTER:'
     O                       EEFEAD              82
     O                                          117 'RESERVE CHGS THIS PERIOD'
     O                       EERCMP        JB   134
     O                       EERMED        JB   150
     O                       EERLEG        JB   166
     O                       EEROTH        JB   182
     O                       EERTOT        JB   198
     O          E            DETAI#         1
     O                  41                       11 '  LOCATION:'
     O                  41   EECTG1              22
     O                  42   X1CAT2              65
     O                  42   EECTG2              76
     O          E            DETAI#         1
     O                                           14 'ACCIDENT DATE:'
     O                       XXACDT              25 '  /  /    '
     O                                           32 'TIME:'
     O                       EEACTM              41 '  :  :  '
     O                                           54 'DAY OF WEEK:'
     O                       EEDAYW              65
     O                                           80 'DATE REPORTED:'
     O                       XXRPDT              91 '  /  /    '
     O                                          109 'PAYMENTS TO DATE'
     O                       EEDCMP        JB   134
     O                       EEDMED        JB   150
     O                       EEDLEG        JB   166
     O                       EEDOTH        JB   182
     O                       EEDTOT        JB   198
     O          E            DETAI#         1
     O                                            7 'NATURE:'
     O                       EENATR              10
     O                                           18 'PART:'
     O                       EEPART              21
     O                                           30 'CAUSE:'
     O                       EECAUS              33
     O                                           46 'LOST TIME:'
     O                       EELSTM              48
     O                                           65 'CLOSE DATE:'
     O                       EEMDY               75 '  /  /    '
     O                                          117 'OUTSTN (UNPAID) RESERVES'
     O                       EEOCMP        JB   134
     O                       EEOMED        JB   150
     O                       EEOLEG        JB   166
     O                       EEOOTH        JB   182
     O                       EEOTOT        JB   198
     O          E            DETAI#         3
     O                                           22 'DESCRIPTION OF INJURY:'
     O                       EEACDS              83
     O                                          107 'TOTAL INCURRED'
     O                       EEICMP        JB   134
     O                       EEIMED        JB   150
     O                       EEILEG        JB   166
     O                       EEIOTH        JB   182
     O                       EEITOT        JB   198
     O          E            TOTCG#      1  1
     O                                           17 '* * * * LOCATION:'
     O                       XXCTG1              29
     O                                           43 'TOTALS * * *'
     O                                          113 'PAYMENTS THIS PERIOD'
     O                       XXPCMP        JB   134
     O                       XXPMED        JB   150
     O                       XXPLEG        JB   166
     O                       XXPOTH        JB   182
     O                       XXPTOT        JB   198
     O          E            TOTCG#         1
     O                                          117 'RESERVE CHGS THIS PERIOD'
     O                       XXRCMP        JB   134
     O                       XXRMED        JB   150
     O                       XXRLEG        JB   166
     O                       XXROTH        JB   182
     O                       XXRTOT        JB   198
     O          E            TOTCG#         1
     O                                           42 'CLAIM COUNT:'
     O                                           50 'OPEN:'
     O                       XXOP1         3B    56
     O                                           67 'CLOSED:'
     O                       XXCS1         3B    73
     O          E            TOTCG#         1
     O                       XXNAME              70
     O                                          109 'PAYMENTS TO DATE'
     O                       XXDCMP        JB   134
     O                       XXDMED        JB   150
     O                       XXDLEG        JB   166
     O                       XXDOTH        JB   182
     O                       XXDTOT        JB   198
     O          E            TOTCG#         1
     O                       XXPAD1              70
     O                                          117 'OUTSTN (UNPAID) RESERVES'
     O                       XXOCMP        JB   134
     O                       XXOMED        JB   150
     O                       XXOLEG        JB   166
     O                       XXOOTH        JB   182
     O                       XXOTOT        JB   198
     O          E            TOTCG#         1
     O                       XXPAD2              70
     O                                          107 'TOTAL INCURRED'
     O                       XXICMP        JB   134
     O                       XXIMED        JB   150
     O                       XXILEG        JB   166
     O                       XXIOTH        JB   182
     O                       XXITOT        JB   198
     O          E            TOTCG#         2
     O                       XXPCSZ              70
     O          E            TOTYR#      1  1
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
      *
     O          E            TOTYR#         1
     O                                            5 '* * *'
     O                       XXFYR2        Z     11
     O                                           30 'POLICY YEAR TOTAL'
     O                                           37 ' * * *'
     O                                          113 'PAYMENTS THIS PERIOD'
     O                       YRPCMP        JB   134
     O                       YRPMED        JB   150
     O                       YRPLEG        JB   166
     O                       YRPOTH        JB   182
     O                       YRPTOT        JB   198
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
     O                                          109 'PAYMENTS TO DATE'
     O                       YRDCMP        JB   134
     O                       YRDMED        JB   150
     O                       YRDLEG        JB   166
     O                       YRDOTH        JB   182
     O                       YRDTOT        JB   198
     O          E            TOTYR#         1
     O                                           20 'PREMIUM THIS PERIOD:'
     O                       XYPREM        JB    44
     O                                           61 'LOSS RATIO:'
     O                       XXLOSS        JB    72
     O                                          117 'OUTSTN (UNPAID) RESERVES'
     O                       YROCMP        JB   134
     O                       YROMED        JB   150
     O                       YROLEG        JB   166
     O                       YROOTH        JB   182
     O                       YROTOT        JB   198
     O          E            TOTYR#         2
     O                                           14 'EARNED PREMIUM'
     O                                           28 'YEAR TO DATE:'
     O                       XXAMT         JB    44
     O                                           61 'FREQ RATIO:'
     O                       XXFREQ        JB    72
     O                                          107 'TOTAL INCURRED'
     O                       YRICMP        JB   134
     O                       YRIMED        JB   150
     O                       YRILEG        JB   166
     O                       YRIOTH        JB   182
     O                       YRITOT        JB   198
     O*
     O          E            TOTDV#      2  1
     O                                           14 '* * * DIVISION'
     O                       XXDIV         3     19
     O                                           34 'TOTALS  * * *'
     O                                          113 'PAYMENTS THIS PERIOD'
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
     O*                                  16 '* * *  EMPLOYER:'
     O*                        XXEMP#Z   26
     O*                                  41 'TOTALS  * * *'
     O*                78                63 '(FOR ALL POLICY YEARS)'
     O*                79                80 '(ALL DIVISIONS)'
     O*                                 113 'PAYMENTS THIS PERIOD'
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
     O          E            TOTLR#      1  1
     O                                            5 '* * *'
     O                                           26 'GRAND TOTALS  * * *'
     O                                          113 'PAYMENTS THIS PERIOD'
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
     O                                          109 'PAYMENTS TO DATE'
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
