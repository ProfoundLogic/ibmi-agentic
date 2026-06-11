     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE W4102R - GENERATE MULTIPLE CLAIM REPORTS
     H*****************************************************************
     H* PROGRAM DESCRIPTION:
     H*   THIS PGM GENERATES DWC-4, DWC12, DWC-3, DWC-13 CLAIM FORMS
     H*   WHICH ARE SELECTED THROUGH A MAINTENANCE OPTION & GENERATED
     H*   UPON DEMAND & THROUGH THE DAILY BATCH PROCESS.
     H*
     H* CALLED FROM:
     H*   W4100C - DEMAND REQUEST
     H*   W6000C - DAILY BATCH PROCESS
     H* CALLS TO:
     H*   NONE
     H* PROGRAMMER: JACKIE JANOWIAK (ORIGINAL 8/88)
     H*             MALCOLM SHARPRE (REVISED VERSION 8/31/90)
     H*             MARIE NGUYEN    (REVISED VERSION 10/07/91)
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 06/02/04 C:53 PROJECT: WC1180                                 *
     H*               EXPAND FROM 2 DEC POS TO 4                      *
     H*                                                               *
     H* 07/17/03 C:53 Support: WC065/38013                            *
     H*               Correct weeks/days calc.  When a neg recovery   *
     H*               is processed, the weeks/days should be added.   *
     H*                                                               *
     H*  08/29/02  C:132 SEQ#: WC967
     H*                  Add code for settlement amounts tha include  *
     H*                  Attorney's Fees. This new pay value is a "U" *
     H*                  and would be similar to the pay value "L"    *
     H*                  This new pay walue is for 1099 Box 14 Amts.  *
     H*                                                               *
     H* 07/07/00 C:15  Support: WC044/22681                           *
     H*                Chgd pgm to use new field for # weeks of       *
     H*                entitlement.                                   *
     H*                                                               *
     H*  04/26/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
     H*  03/31/00 C:31  Support: 22408                                *
     H*                 1. Add initialization of CMIPGM               *
     H*                                                               *
     H* 10/19/99 C:15  Support: WC044/20033                           *
     H*                Fixed the 'date prepared' on all forms.        *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H*  11/11/98 C:90 Support: WC051/14926                           *
     H*               -DISPLAY COMPENSATION RATE ON DWC-13            *
     H*                                                               *
     H*  11/10/98 C:02 SUPPORT                                        *
     H*                REVISED TO USE CALCULATED COMP RATE            *
     H*                                                               *
     H*  10/19/98 C:02 PROJ: 48WLT                                    *
     H*               - DO NOT PRINT DATES IF ZERO, PRINT BLANKS      *
     H*                                                               *
     H*  06/01/98 C:15 Support: WC051/11935                           *
     H*               -Changed pgm to use CMJRST when calculating the *
     H*                comp rate, instead of the emp rate set code    *
     H*                jurisdiction state.                            *
     H*                                                               *
     H*  02/23/98 C:85 Project: WC577                                 *
     H*                CHANGED THE PROGRAM MOVED TO CMPNAM            *
     H*                FROM W4102R TO W0070R.                         *
     H*                CHANGED THE FIELD MOVED TO CMSMCD              *
     H*                FROM *BLANKS TO CTSMCD.                        *
     H*                                                               *
     H*  10/09/97 C:47 PROJ: WC692                                     *
     H*                CHANGED TO USE COMP RATE FROM WMCMP INSTEAD     *
     H*                OF CALCULATING FROM WDRBP.                      *
     H*                                                               *
     H* 05/13/97 C:62 PROJECT: WC674                                   *
     H*               ENSURE ALL PROGRAMS USE CMLTFL = I/M/R NOT Y/N
     H*                                                                *
     H* 03/28/95 C:15 PROJECT: WC294                                   *
     H*               IF A REMARK CODE EXISTS, WRITE THE DARTS RECORD. *
     H*                                                                *
     H* 03/24/95 C:15 PROJECT: WC294                                   *
     H*               REMOVED LEGAL EXPENSES FROM THE DWC-13 TOTAL.    *
     H*                                                                *
     H*  02/14/95 C:15 PROJECT: WC294                                  *
     H*              1.CHANGED CODE TO COMPLY WITH CHANGES TO THE      *
     H*                STATE FORMS - DWC4, DWC12, & DWC13.             *
     H*              2.ADDED CODE TO WRITE DARTS RECORDS WHEN FORMS    *
     H*                ARE PRINTED.                                    *
     H*                                                                *
     H*  01/05/94 C:10 PROJ: WC290                                     *
     H*                FIX REPORT USAGE OF UPCO#                       *
     H*                                                                *
     H*  12/27/93 C:10 PROJ: WC287                                     *
     H*                DWC-4, SCREEN 3 -- CHANGED 'RETROACTIVE TO D/A' *
     H*                TO NOT PRINT AN 'X' IF THE VALUE IS BLANK       *
     H*                                                                *
     H*  09/10/93 C:10 PROJECT WC268                                  *
     H*                1. DWC-4 CHANGED CODE SO THAT IF SS# PRINTS    *
     H*                   ON ONE FORM, THE FOLLOWING FORMS DON'T      *
     H*                   PRINT 000-00-0000.  ALSO, MOVED PI RATING   *
     H*                   ONE SPACE TO THE RIGHT.                     *
     H*                2. DWC-13 CHANGED CODE SO THAT RESERVED DON'T  *
     H*                   PRINT.                                      *
     H*                3. DWC-3 CHANGED CODE TO NOT PRINT 00/00/00    *
     H*                   FOR CLAIM LOSS OF WAGES FOR TWO WEEKS IF    *
     H*                   THE DATES WERE NOT ENTERED.  ALSO ADDED     *
     H*                   FND-FYR AND CASE# FOR CARRIER FILE#.        *
     H*                                                                *
     H*  01/19/93 C:11 PROJECT WC194                                  *
     H*                1. RE-ALIGN FORMS DWC-4, DWC-12, DWC-13.       *
     H*                2. DON'T PRINT 00/00/00 IF A DATE IS ZERO.     *
     H*                3. DON'T PRINT COMPANY NAME & ADDRESS          *
     H*                   BECAUSE THEY'RE PRE-PRINTED.                *
     H*                ALL CHANGES WERE MADE ON OUTPUT SPECS ONLY.    *
     H*                                                               *
     H*  04/15/92 LPK  PROJECT WC123                                  *
     H*                -DWC13- IF A RECORD EXISTS IN WSFRP FOR A CASE,*
     H*                USE THAT DATA FOR THE DWC13.                   *
     H*                                                               *
     H*  03/16/92 LPK  PROJECT WC123                                  *
     H*                -DWC13- CHANGED TOTAL CALCULATIONS FOR ITEM 33.*
     H*                ALL CALCULATIONS USING ARRAY 'PRI' EXCEPT ITEM *
     H*                33, WERE CHANGED. CHANGED CALCULATION FOR WEEKS*
     H*                AND DAYS.                                      *
     H*
     H*  12/13/91 WLD  PROJECT WC104                                  *
     H*                REMOVED INDICATOR 31 FROM OUTPUT SPECS OF      *
     H*                DWC-4 FORM SO THAT FORM WOULD PRINT.           *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H******************************************************************
     H*  INDICATOR USAGE:
     H*
     H*    06         DON'T PRINT COMPANY ADDRESS 2
     H*    11         FIRST PASS
     H*    23         ACCIDENT DATE < 1/1/94
     H*    24         LOST TIME FLAG = 'Y'
     H*    50,51,60   FILE CHAINS & LOOKUP
     H*    70         DWC-4 FORM SELECTED
     H*    71         DWC-3 FORM SELECTED
     H*    72         DWC-13 FORM SELECTED
     H*    73         DWC-12 FORM SELECTED
     H*
     H*  FOR DWC-12:
     H*    25         XXRSCD = 0
     H*
     H*  FOR DWC-13:
     H*    20         INITIAL REPORT SELECTED
     H*    21         FINAL REPORT SELECTED
     H*    22         ANNUAL REPORT SELECTED
     H*    26         XXFSDT = 0
     H*    27         XXTPDT = 0
     H*    28         XXPTDT = 0
     H*    29         XXMSDT = 0
     H*    30         XXWODT = 0
     H*    33         ADVANCES
     H*    38         FULL SALARY FLAG = 'Y'
     H*
     H*  FOR DWC-4:
     H*    32         XXISTD = 0
     H*    34         XXISUD = 0
     H*    35         XXIRID = 0
     H*    36         XXIRCD = 0
     H*    37         XXRTWD = 0
     H*    39         XXAJDT = 0
     H*    40         XXMMID = 0
     H*    41         RELEASED TO RETURN TO WORK RESTRICTION FLAG IS BLANK
     H*    42         RELEASED TO RETURN TO WORK RESTRICTION FLAG IS 'Y'
     H*    43         RETURN TO WORK RESTRICTION FLAG IS BLANK
     H*    44         RETURN TO WORK RESTRICTION FLAG IS 'Y'
     H*    46         RETROACTIVE FLAG = YES
     H*   N86         RETROACTIVE FLAG = *BLANK
     H*    47         CORRECTION OF SOCIAL SECURITY #
     H*    48         XXDTHD = 0
     H*    49         XXPID4 = 0
     H*    74         XXPTD4 = 0
     H*    75         XXPTEF = 0
     H*    76         XXAEF1 = 0
     H*    77         XXAEF2 = 0
     H*    78         XXACN1 = 0
     H*    79         XXACN2 = 0
     H*    80         XXREEF = 0
     H*    81         XXNDA  = 0
     H*
     H*  FOR DWC-3:
     H*    61         WL FROM DATE 1 NOT EQUAL TO ZERO
     H*    62         WL FROM DATE 2 NOT EQUAL TO ZERO
     H*****************************************************************
     FWMUPL1    IF   E           K DISK
     F*** BEGIN DLT *** 06/01/98 **************************************
     F*****WDF1L   IF  E           K        DISK
     F*****WMRSL   IF  E           K        DISK
     F*****WDELL   IF  E           K        DISK
     F*** END DLT ***** 06/01/98 **************************************
     FWALTL1    UF   E           K DISK
     FWAL1L     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWDPAL5    IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
     FWDPAL     IF   E           K DISK
     F                                     RENAME(PPA01:PPA03)
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     FWDRBL     IF   E           K DISK
     FWSFRL     IF   E           K DISK
     F*** BEGIN DEL *** 04/26/00 **************************************
     F********SDCML   O   E           K        DISK
     F********    PCM01                             KRENAMEPCM02
     F********SDCMLA  IF  E           K        DISK
     F********    PCM01                             KRENAMEPCM03
     F*** END DEL *** 04/26/00 ****************************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SSY01)
     F*                                                                *********
     F*** BEGIN DEL *** 04/26/00 **************************************
     F****BEGIN ADD *** 02/23/98 ***********************************************
     F********STCTL1  IF  E           K        DISK
     F****END*  ADD *** 02/23/98 ***********************************************
     F*** END DEL *** 04/26/00 ****************************************
     F*                                                                *********
     F* DWC-12
     FQSYSPRT1  O    F   85        PRINTER
     F* DWC-3
     FQSYSPRT2  O    F   85        PRINTER
     F* DWC-13
     FQSYSPRT3  O    F  127        PRINTER
     F* DWC-4
     FQSYSPRT5  O    F   85        PRINTER

     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D* ARRAY TO REMOVE ANY TITLE IN CLAIMS EXAMINER NAME
     D* RSV CODES, AMT RESERVED, & AMT PAID ON EACH CLAIM (WMCMP)
     D* ALL PMT VAL 1 CODES & CORRESPONDING PLACEMENT VALUE
     D TP              S              2    DIM(999)                             PMT VAL 1 CDES
     D PL              S              1    DIM(999)                             PLACEMENT
     D* COMPILE TIME ARRAY HOLDING ALL PMT VAL 1 PLACEMENTS
      **** BEG CHG **** 08/29/02 *****************************************
     D******************* PLC    27  27  1
     D PLC             S              1    DIM(28) CTDATA PERRCD(28)
      **** END CHG **** 08/29/02 *****************************************
     D WK              S              3  0 DIM(27)                              WEEKS
     D DY              S              1  0 DIM(27)                              DAYS
     D* AMT PAID BY CARRIER AS OF 12/31/--
     D PRA             S              9  2 DIM(25)                              PRIOR AMT PD
     D* AMT PAID BY CARRIER AS OF 12/31/--    (ROUNDED)
     D PRI             S              7  0 DIM(25)                              PRIOR AMT PD
     D* AMT PAID BY CARRIER TO DATE
     D CRA             S              9  2 DIM(28)                              CURRENT AMT PD
     D* AMT PAID BY CARRIER TO DATE           (ROUNDED)
     D CUR             S              7  0 DIM(25)                              CURRENT AMT PD
     D*
     D*
     D RMK             S             60    DIM(6) CTDATA PERRCD(1)              REMARKS
     D*** BEGIN DEL *** 04/26/00 **************************************
     D********PCM03
     D********      CMKEY                           C3KEY
     D********      CMCSID                          C3CSID
     D********      CMCOMM                          C3COMM
     D********      CMCDAT                          C3CDAT
     D********      CMCODE                          C3CODE
     D********      CMDCID                          C3DCID
     D********      CMPNAM                          C3PNAM
     D********      CMTRDT                          C3TRDT
     D********      CMTRTM                          C3TRTM
     D********      CMSEQ#                          C3SEQ#
     D********      CMSECL                          C3SECL
     D********      CMSBYL                          C3SBYL
     D********      CMSTAT                          C3STAT
     D********      CMUSER                          C3USER
     D********      CMPGM                           C3PGM
     D********      CMTIME                          C3TIME
     D********      CMDT                            C3DT
     D********      CMIUSR                          C3IUSR
     D********      CMFOLD                          C3FOLD
     D********      CMDCDT                          C3DCDT
     D********      CMDIFL                          C3DIFL
     D********      CMAPID                          C3APID
     D********      CMIDOC                          C3IDOC
     D********      CMIQUE                          C3IQUE
     D********      CMIMGF                          C3IMGF
     D********      CMIPTH                          C3IPTH
     D********      CMPIDX                          C3PIDX
     D********      CMQPTY                          C3QPTY
     D********      CMDCTP                          C3DCTP
     D********      CMIPGM                          C3IPGM
     D********      CMSMCD                          C3SMCD
     D********      CMFL01                          C3FL01
     D********      CMFL02                          C3FL02
     D********      CMFL03                          C3FL03
     D********      CMFL04                          C3FL04
     D********      CMFL05                          C3FL05
     D********      CMRMCD                          C3RMCD
     D********      CMIDT                           C3IDT
     D*** END DEL *** 04/26/00 ****************************************
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D                 DS
     D  XXAL04                 1     10
     D  XXDC03                 1      5
     D  XXDC04                 6     10
     D                 DS
     D  XXAL05                 1     10
     D  XXDC12                 1      5
     D  XXDC13                 6     10
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D                 DS
     D  CX                     1     30
     D                                     DIM(30) ASCEND
     D  XXCLEX                 1     30
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)                              RSV CODES
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)                              REMAINING RSV
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)                              TOTAL PAID
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  XXWLFR                 1      6  0
     D  WLFRMM                 1      2  0
     D  WLFRDD                 3      4  0
     D  WLFRYY                 5      6  0
     D                 DS
     D  XXWLTO                 1      6  0
     D  WLTOMM                 1      2  0
     D  WLTODD                 3      4  0
     D  WLTOYY                 5      6  0
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND                       COMPANY NAME W/ BLAN
     D  CONAME                 1     40
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    CMCLEX
     C*
     C*   KEY LIST FOR THE COMPANY MASTER FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LTFND
     C*
     C*   KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C     F2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LTFND
     C*
     C*** BEGIN DLT *** 06/01/98 **************************************
     C*   KEY LIST FOR THE FUND DETAIL FILE
     C**********
     C********** F1KEY     KLIST
     C**********           KFLD           LTCO#
     C**********           KFLD           LTFND
     C**********           KFLD           LTFYR
     C*** END DLT ***** 06/01/98 **************************************
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LTFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*** BEGIN DLT *** 06/01/98 **************************************
     C*  EMPLOYER DETAIL FILE
     C**********
     C********** ELKY      KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           LTFND
     C**********           KFLD           CMEMP#
     C**********           KFLD           CMDIV
     C**********           KFLD           LTFYR
     C**********
     C*  RATE SET CODE FILE
     C**********
     C********** RSKY      KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXRTCD  3
     C*** END DLT ***** 06/01/98 **************************************
     C*
     C*  STATE COMP BENEFIT FILE
     C*
     C     RBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LTFND
     C                   KFLD                    XXSTE             2
     C                   KFLD                    XXEFDT
     C*
     C     RBKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LTFND
     C                   KFLD                    XXSTE             2
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LTFND
     C                   KFLD                    LTFYR
     C                   KFLD                    LTCASE
     C*
     C*  CLAIM HISTORY FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LTFND
     C                   KFLD                    LTFYR
     C                   KFLD                    LTCASE
     C*
     C*  WORK FILE FOR MULT CLAIM FORMS - HOLDS DWC-13 DATES
     C*
     C     L1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LTFND
     C                   KFLD                    LTFYR
     C                   KFLD                    LTCASE
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR THE WORK FILE
     C*
     C     LTKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C     TBKY2         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  PAYMENT FILE
     C*
     C     PAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LTFND
     C                   KFLD                    LTFYR
     C                   KFLD                    LTCASE
     C*
     C     PAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LTFND
     C                   KFLD                    LTFYR
     C                   KFLD                    LTCASE
     C                   KFLD                    XXSEQ#
     C*
     C*  KEY LIST FOR DWC13 SUMMARY FILE
     C*
     C     FRKY          KLIST
     C                   KFLD                    XXCO#                          ADD 01/05/94
     C                   KFLD                    LTFND
     C                   KFLD                    LTFYR
     C                   KFLD                    LTCASE
     C*
     C     FRKY1         KLIST
     C                   KFLD                    XXCO#                          ADD 01/05/94
     C                   KFLD                    LTFND
     C                   KFLD                    LTFYR
     C                   KFLD                    LTCASE
     C                   KFLD                    XXTYPE            1
     C*** BEGIN DEL *** 04/26/00 **************************************
     C********
     C***DARTS FILE
     C********
     C********   C3KY      KLIST
     C********             KFLD           QCKEY
     C********             KFLD           ##SEQ#  50
     C*** END DEL *** 04/26/00 ****************************************
     C*
     C*  W/C BASE CONTROL FILE
     C     WCKY          KLIST
     C                   KFLD                    XWKEY             2
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
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
     C     DT6MDY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANK        SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** BEGIN ADD *** 04/26/00 **************************************
     C*
     C*          DA700R - API to write a DARTS Remark PLIST
     C*          ------------------------------------------------------
     C     DARTPL        PLIST
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C*** END ADD *** 04/26/00 ****************************************
     C*
     C     *LIKE         DEFINE    LTCO#         XXCO#
     C     *LIKE         DEFINE    CMACDT        XXEFDT
     C     *LIKE         DEFINE    PASEQ#        XXSEQ#
     C     *LIKE         DEFINE    LTDT          XXDATE
     C     *LIKE         DEFINE    F2IAUD        XXIAUD
     C     *LIKE         DEFINE    LTDT          XXSYDT
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQDATE        XXDT8             8 0
     C*
     C*** BEGIN DLT *** 10/19/99 **************************************
     C******************** Z-ADDXXDT8     SRDTI
     C******************** CALL 'SRCVTDAT'DT6MDY
     C******************** MOVELSRDTO     XXDSDT  60
     C*** END DLT ***** 10/19/99 **************************************
     C*
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C*** BEGIN ADD *** 10/19/99 **************************************
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDSDT            6 0
     C*** END ADD ***** 10/19/99 **************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C** GET THE DWC REMARK CODES FROM THE WORK COMP BASE CONTROL FILE
     C*
     C                   MOVE      *BLANKS       XWKEY
     C                   MOVEL     'WC'          XWKEY
     C     WCKY          CHAIN     SWC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      WCAL04        XXAL04
     C                   MOVE      WCAL05        XXAL05
     C                   END                                                    END
     C*
     C*   START READING THE WORK FILE - READ < OR = TO ENTERED DATE
     C*
     C     LTKEY         CHAIN     PLT01                              60
     C     *IN60         DOWEQ     '0'
 B1  C     LTDT          ANDLE     XXDATE
     C*
 B2  C     LTDWC4        IFEQ      'X'
     C     LTDW12        OREQ      'X'
     C     LTDW13        OREQ      'X'
     C     LTDWC3        OREQ      'X'
     C*
     C     LTDWC4        COMP      'X'                                    70
     C     LTDWC3        COMP      'X'                                    71
     C     LTDW13        COMP      'X'                                    72
     C     LTDW12        COMP      'X'                                    73
     C*** BEGIN DLT *** 10/19/99 **************************************
     C*** BEGIN ADD *** 10/19/98 **************************************
     C********** LTDW13    IFEQ 'X'
     C**********           Z-ADDLTFSDT    SRDTI
     C**********           CALL 'SRCVTDAT'DT6MDY
     C**********           MOVELSRDTO     XXDSDT
     C**********           ENDIF
     C*** END ADD *** 10/19/98 ****************************************
     C*** END DLT ***** 10/19/99 **************************************
     C*
     C*   GET COMPANY NAME & REMOVE LEADING BLANKS
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*   REMOVE LEADING BLANKS FROM COMPANY NAME
     C*
     C                   Z-ADD     1             Y                 2 0
     C     *BLANK        LOOKUP    CONM(Y)                            60
     C                   MOVEA     CONM(Y)       XXCONM           30
     C*
     C*   GET COMPANY ADDRESS FIELDS
     C*
     C                   MOVEL     COADR1        XCADR1           30
     C                   MOVEL     COADR2        XCADR2           30
     C                   MOVEL     COCTY         XCCTY            15
     C                   MOVEL     COST          XCST              2
     C                   Z-ADD     COZIP         XCZIP             9 0
     C*
     C     XCADR2        IFEQ      *BLANK
     C                   MOVE      '1'           *IN06
     C                   ELSE
     C                   MOVE      '0'           *IN06
     C                   END
     C*
     C*   LOAD PMT VAL 1 CODES INTO ARRAY TP, & PLACEMENT CODE INTO PL
     C*   ONE TIME ONLY
     C*
     C     *IN11         IFEQ      '0'
     C                   Z-ADD     1             X                 4 0
     C     TBKEY         CHAIN     PB201                              50
     C     *IN50         DOWEQ     '0'
     C                   MOVEL     B2KEY         XXCODE            2
     C                   MOVE      XXCODE        TP(X)
     C                   MOVE      B2PLCD        PL(X)
     C                   ADD       1             X
     C     TBKEY         READE     PB201                                  50
     C                   MOVE      '1'           *IN11
     C                   END
     C                   END
     C*
     C*   GET FUND MASTER RECORD - (NEED SECOND PHONE#)
     C*
     C     FNKEY         CHAIN     PFN01                              50
     C*** BEGIN ADD *** 10/19/98 **************************************
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        XXCONM
     C                   MOVEL     FNAAD1        XCADR1
     C                   MOVEL     FNAAD2        XCADR2
     C                   MOVEL     FNACTY        XCCTY
     C                   MOVEL     FNAST         XCST
     C                   Z-ADD     FNAZIP        XCZIP
     C*
     C     XCADR2        IFEQ      *BLANK
     C                   MOVE      '1'           *IN06
     C                   ELSE
     C                   MOVE      '0'           *IN06
     C                   END
     C*
     C                   ENDIF
     C*** END ADD *** 10/19/98 ****************************************
     C*
     C*   LOAD CLAIMANT NAME
     C*
     C     CMKY          CHAIN     PCM01                              50
     C     CDKEY         CHAIN     PCD01                              50
     C                   MOVEL     CDLSN         XXLSN            30
     C                   MOVEL     CDFSN         XXFSN            15
     C                   EXSR      SRFLNM
     C                   MOVE      *BLANKS       XXCLNM           30
     C                   MOVE      *BLANKS       XXCL25           25
     C                   MOVEL     XXNAME        XXCLNM
     C                   MOVEL     XXNAME        XXCL25
     C*
     C*  SETON *IN23 IF THE ACCIDENT DATE IS LESS THAN 1/1/94.
     C*
     C     CMACDT        COMP      940101                               23
     C*
     C*  SETON *IN24 IF THIS IS A LOST TIME CLAIM.
     C*
     C*** BEGIN DEL *** 05/13/97 **************************************
     C********** CMLTFL    COMP 'Y'                      24
     C*** END   DEL *** 05/13/97 **************************************
     C*** BEGIN ADD *** 05/13/97 **************************************
     C     CMLTFL        COMP      'I'                                    24
     C*** END   ADD *** 05/13/97 **************************************
     C*
     C*   LOAD CLAIMANT ADDRESS
     C*
     C                   MOVE      *BLANKS       XXCA25
     C                   MOVEL     CDADR1        XXCA25           25
     C                   MOVE      CDST          XXSTAT            2
     C                   Z-ADD     CDZIP         XXZIP
     C                   MOVEL     CDCTY         XXCTY            25
     C                   EXSR      SRADDR
     C                   MOVE      *BLANKS       XXCZ25
     C                   MOVEL     XXCSZ         XXCZ25           25
     C*
     C*   GET EMPLOYER FILE LOAD NAME
     C*
     C     EMKEY         CHAIN     PEM01                              50
     C                   MOVE      *BLANKS       XXENAM
     C                   MOVEL     EMNAME        XXENAM           25
     C*
     C*   LOAD EMPLOYERS ADDRESS & PHONE
     C*
     C                   MOVE      *BLANKS       XXEAD1
     C                   MOVEL     EMMAD1        XXEAD1           25
     C                   MOVE      EMMST         XXSTAT            2
     C                   Z-ADD     EMMZIP        XXZIP
     C                   MOVEL     EMMCTY        XXCTY            25
     C                   EXSR      SRADDR
     C                   MOVE      *BLANKS       XXECSZ
     C                   MOVEL     XXCSZ         XXECSZ           25
     C*
     C*   LOAD CLAIM EXAMINERS NAME
     C*
     C                   MOVE      *BLANKS       XXCLEX           30
     C                   MOVE      *BLANKS       XXTITL           30
     C                   MOVE      *BLANKS       XXTL17           17
     C                   MOVE      *BLANKS       XXADJN           35            ADJUSTER NAME
     C     UPKEY         CHAIN     PUP01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXCLEX
     C                   MOVEL     UPTITL        XXTITL
     C                   MOVEL     UPTITL        XXTL17
     C                   MOVEL     UPUSNM        XXADJN
     C                   Z-ADD     1             X
     C     ','           LOOKUP    CX(X)                                  50
     C     *IN50         IFEQ      '1'
     C     X             DOWLT     31
     C                   MOVE      *BLANK        CX(X)
     C                   ADD       1             X
     C                   END
     C                   END
     C                   END
     C*
     C*   LOAD ACCIDENT DATE & SOCIAL SEC #
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C***  Begin Chg  ***  06/09/20  *******************************
     C*                  MOVE      CMSS#         XXSS3             4 0
     C*                  MOVEL     CMSS#         XXSS1             3 0
     C*                  MOVEL     CMSS#         XXTEMP            5 0
     C*                  MOVE      XXTEMP        XXSS2             2 0
     C*
     C                   MOVE      CMSS#         XXSS3             4
     C                   MOVEL     CMSS#         XXSS1             3
     C                   MOVEL     CMSS#         XXTEMP            5
     C                   MOVE      XXTEMP        XXSS2             2
     C***  End Chg  ***  06/09/20  *******************************
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C*  GET INTERNAL AUDIT FIELD
     C*
     C     F2KEY         CHAIN     PF201                              50
     C                   Z-ADD     F2IAUD        XXIAUD
     C*
     C*   LOAD THE ADJUSTER'S PHONE #.
     C*
     C                   MOVEL     F2SPH#        XXSPH1            3 0
     C                   MOVE      F2SPH#        XXSPH2            4 0
     C                   Z-ADD     UPEXT#        XXEXT#            4 0
     C*
     C*   LOAD DWC-4 INFORMATION, IF SELECTED
     C*
     C     LTDWC4        IFEQ      'X'
     C                   EXSR      SRDWC4
     C                   END
     C*
     C*  IF DWC-3 SELECTED LOAD AWW FACTOR ACCORDING TO ACCIDENT DATE:
     C*  CLAIMS BEFORE 7/1/90 GET 85%; AFTER 7/1/90 GET 80.
     C*
     C     LTDWC3        IFEQ      'X'
     C     CMACDT        IFLT      900701
     C                   Z-ADD     .85           XXAWW             3 2
     C                   Z-ADD     .95           XXFACT            3 2
     C                   Z-ADD     95            XXPCT             3 0
     C                   ELSE
     C                   Z-ADD     .80           XXAWW
     C                   Z-ADD     .80           XXFACT
     C                   Z-ADD     80            XXPCT
     C                   END
     C                   END
     C*
     C*   FLIP DATES FOR DWC-12 & DWC-3
     C*
     C                   Z-ADD     LTRSCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXRSCD            6 0
     C                   Z-ADD     LTMMI3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXMMI3            6 0
     C     LTWLF1        IFNE      *ZEROS
     C                   MOVE      '1'           *IN61
     C                   Z-ADD     LTWLF1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXWLF1            6 0
     C                   Z-ADD     LTWLT1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXWLT1            6 0
     C                   ELSE
     C                   MOVE      '0'           *IN61
     C                   END
     C*
     C     LTWLF2        IFNE      *ZEROS
     C                   MOVE      '1'           *IN62
     C                   Z-ADD     LTWLF2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXWLF2            6 0
     C                   Z-ADD     LTWLT2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXWLT2            6 0
     C                   ELSE
     C                   MOVE      '0'           *IN62
     C                   END
     C*
     C                   MOVEL     XXCLEX        XXEX25           25            DWC-3
     C     XXRSCD        COMP      *ZEROS                             2525
     C*
     C*   LOAD DWC-13 INFORMATION, IF SELECTED
     C*
     C     LTDW13        IFEQ      'X'
     C                   EXSR      SRDW13
     C                   END
     C*
     C*   PRINT SELECTED CLAIM FORMS
     C*
      *
     C     CMSIC         IFEQ      *BLANKS
     C                   MOVE      *ON           *IN15
      *
     C                   ELSE
     C                   MOVE      *OFF          *IN15
      *
     C                   ENDIF                                                  END IFEQ *BLANK
     C*** BEGIN ADD *** 10/19/98 **************************************
     C     XXDSDT        COMP      *ZEROS                             8282
     C     XXWLF1        COMP      *ZEROS                             8383
     C     XXWLT1        COMP      *ZEROS                             8787
     C     XXWLF2        COMP      *ZEROS                             8888
     C     XXWLT2        COMP      *ZEROS                             8989
     C     XXMMI3        COMP      *ZEROS                             9090
     C*** END ADD *** 10/19/98 ****************************************
      *
     C                   EXCEPT    DET
     C*
     C*   WRITE DARTS RECORD TO SHOW THAT THE DWC FORMS HAVE BEEN PRINTED
     C*
     C                   EXSR      SRDART
     C*
     C*   DELETE THE RECORD FROM THE WORK FILE
     C*
     C                   DELETE    PLT01
     C*
E2   C                   END
     C*
     C     LTKEY         READE     PLT01                                  60
E1   C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
     C****************************************************
     C*   SUBROUTINE TO LOAD THE CITY STATE & ZIP        *
     C****************************************************
     C     SRADDR        BEGSR
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     XXSTAT        XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ            40
     C                   ENDSR
     C*
     C*
     C****************************************************
     C*   SUBROUTINE TO DWC-4 OUTPUT                     *
     C****************************************************
     C     SRDWC4        BEGSR
     C*
     C*   LOAD AJUSTER BUSINESS PHONE
     C*
     C                   Z-ADD     *ZEROS        XDAREA            3 0
     C                   Z-ADD     *ZEROS        XDPHNP            3 0
     C                   Z-ADD     *ZEROS        XDPHN#            4 0
     C     FNKEY         CHAIN     PFN01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     FNAREA        XDAREA
     C                   MOVEL     FNPH2         XDPHNP
     C                   MOVE      FNPH2         XDPHN#
     C                   END
     C*   CONVERT DATES
     C                   Z-ADD     LTMMID        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXMMID            6 0
     C                   Z-ADD     LTRTWD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXRTWD            6 0
     C                   Z-ADD     LTDTHD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDTHD            6 0
     C                   Z-ADD     LTREEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXREEF            6 0
     C                   Z-ADD     LTAJDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXAJDT            6 0
     C                   Z-ADD     LTPID4        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPID4            6 0
     C                   Z-ADD     LTPTD4        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPTD4            6 0
     C                   Z-ADD     LTPTEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPTEF            6 0
     C                   Z-ADD     LTISTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXISTD            6 0
     C                   Z-ADD     LTISUD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXISUD            6 0
     C                   Z-ADD     LTIRID        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXIRID            6 0
     C                   Z-ADD     LTIRCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXIRCD            6 0
     C                   Z-ADD     LTAEF1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXAEF1            6 0
     C                   Z-ADD     LTAEF2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXAEF2            6 0
     C                   Z-ADD     LTACN1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACN1            6 0
     C                   Z-ADD     LTACN2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACN2            6 0
     C                   Z-ADD     LTNDA         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXNDA             6 0
     C*
     C*   INDICATE PROPER BOX TO MARK FLAGS
     C*
     C     LTREDA        IFEQ      *BLANK
     C                   MOVE      '0'           *IN86                          N86 = RETRO = *BLANK
     C                   ELSE
     C                   MOVE      '1'           *IN86                          86 = RETRO IS Y OR N
     C                   END
     C                   MOVE      '0'           *IN46                           N46 = NO RETRO
     C     LTREDA        COMP      'Y'                                    46     RETRO = YES
     C                   MOVE      *ZEROS        XNSS1
     C                   MOVE      *ZEROS        XNSS2
     C                   MOVE      *ZEROS        XNSS3
     C     LTCSS#        IFEQ      'X'                                           CHANGE SS#
     C                   MOVE      '1'           *IN47
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'LTNSS#'      QQNAME           25
     C     LTNSS#        PARM      LTNSS#        QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C***  Begin Chg  ***  06/09/20  *******************************
     C*                  MOVE      LTNSS#        XNSS3             4 0           LOAD CORRECT SS#
     C*                  MOVEL     LTNSS#        XNSS1             3 0
     C*                  MOVEL     LTNSS#        XXTEMP
     C*                  MOVE      XXTEMP        XNSS2             2 0
     C                   MOVE      LTNSS#        XNSS3             4             LOAD CORRECT SS#
     C                   MOVEL     LTNSS#        XNSS1             3
     C                   MOVEL     LTNSS#        XXTEMP
     C                   MOVE      XXTEMP        XNSS2             2
     C***  End Chg  ***  06/09/20  *******************************
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'LTNSS#'      QQNAME           25
     C     LTNSS#        PARM      LTNSS#        QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   ELSE
     C                   MOVE      '0'           *IN47
     C                   END
     C*  SET INDICATORS FOR RETURN TO WORK RESTRICTION FLAGS.
     C*
     C     LTIRCT        COMP      *BLANKS                                41
     C     LTIRCT        COMP      'Y'                                    42
     C     LTRTWF        COMP      *BLANKS                                43
     C     LTRTWF        COMP      'Y'                                    44
     C*
     C*  DO NOT PRINT DATES IF ZERO.
     C*
     C     XXISTD        COMP      *ZEROS                             3232
     C     XXISUD        COMP      *ZEROS                             3434
     C     XXIRID        COMP      *ZEROS                             3535
     C     XXIRCD        COMP      *ZEROS                             3636
     C     XXRTWD        COMP      *ZEROS                             3737
     C     XXAJDT        COMP      *ZEROS                             3939
     C     XXMMID        COMP      *ZEROS                             4040
     C     XXDTHD        COMP      *ZEROS                             4848
     C     XXPID4        COMP      *ZEROS                             4949
     C     XXPTD4        COMP      *ZEROS                             7474
     C     XXPTEF        COMP      *ZEROS                             7575
     C     XXAEF1        COMP      *ZEROS                             7676
     C     XXAEF2        COMP      *ZEROS                             7777
     C     XXACN1        COMP      *ZEROS                             7878
     C     XXACN2        COMP      *ZEROS                             7979
     C     XXREEF        COMP      *ZEROS                             8080
     C     XXNDA         COMP      *ZEROS                             8181
     C*** BEGIN ADD *** 10/19/98 **************************************
     C     XXDSDT        COMP      *ZEROS                             8282
     C     XXWLF1        COMP      *ZEROS                             8383
     C     XXWLT1        COMP      *ZEROS                             8787
     C     XXWLF2        COMP      *ZEROS                             8888
     C     XXWLT2        COMP      *ZEROS                             8989
     C     XXMMI3        COMP      *ZEROS                             9090
     C*** END ADD *** 10/19/98 ****************************************
     C*** BEGIN ADD *** 07/07/00 **************************************
     C*  Determine if a decimal was entered in the weeks of entitlement
     C*    field.
     C*
     C     LTWKEN        COMP      LTWK#                              91
     C*** END ADD ***** 07/07/00 **************************************
     C*
     C                   ENDSR
     C*
     C****************************************************
     C*   SUBROUTINE FOR DWC-13 OUTPUT (OLD DWC-2)       *
     C****************************************************
     C     SRDW13        BEGSR
     C*
     C*   INITIALIZE FIELDS
     C*
     C                   Z-ADD     0             WK
     C                   Z-ADD     0             DY
     C                   Z-ADD     0             CRA
     C                   Z-ADD     0             XXFSDT
     C                   Z-ADD     0             XXPTDT            6 0
     C                   Z-ADD     0             XXTPDT
     C                   Z-ADD     0             XXWODT            6 0
     C                   Z-ADD     0             XXMSDT            6 0
     C                   Z-ADD     0             XXMSAM            9 2
     C*
     C*   CHECK TYPE OF REPORT
     C*
     C     LTRPTP        COMP      'I'                                    20
     C     LTRPTP        COMP      'F'                                    21
     C     LTRPTP        COMP      'A'                                    22
     C*
     C*   CALCULATE THE COMP RATE
     C*
     C*** BEGIN DLT *** 06/01/98 **************************************
     C********** F1KEY     CHAINPF101                50
     C********** ELKY      CHAINPEL01                50
     C**********           Z-ADDCMACDT    XXEFDT
     C**********           MOVE ELRTCD    XXRTCD
     C********** RSKY      CHAINPRS01                50
     C**********           MOVE RSJRST    XXSTE
     C*** END DLT ***** 06/01/98 **************************************
     C**** BEGIN ADD **** 11/11/98 ************************************
     C     CMCRT         IFNE      *ZEROS
     C                   Z-ADD     CMCRT         XXRATE
     C                   ELSE
     C**** END   ADD **** 11/11/98 ************************************
     C*** BEGIN ADD *** 06/01/98 **************************************
     C                   Z-ADD     CMACDT        XXEFDT
     C                   MOVE      CMJRST        XXSTE
     C*** END ADD ***** 06/01/98 **************************************
     C     RBKY          SETGT     PRB01
     C     RBKY2         READPE    PRB01                                  50
     C*
     C     *IN50         IFEQ      *OFF
      *** BEG CHG *** 06/02/04 ****************************************
     C********** RBREPC    DIV  100       XXPCNT  54
     C     RBCMPR        DIV       100           XXPCNT            7 4
      *** END CHG *** 06/02/04 ****************************************
     C*** BEGIN CHG *** 11/10/98 **************************************
      *** BEG CHG *** 10/09/97 ****************************************
     C***        CMAWW     MULT XXPCNT    XXWK    72H
     C     CMAWW         MULT(H)   XXPCNT        XXWK              7 2
     C**********           Z-ADDCMCRT     XXWK    72
      *** END CHG *** 10/09/97 ****************************************
     C*** END CHG *** 11/10/98 ****************************************
     C*
     C     XXWK          IFGT      RBPACP
     C                   Z-ADD     RBPACP        XXRATE            7 2
     C                   ELSE
     C                   Z-ADD     XXWK          XXRATE
     C                   ENDIF                                                  XXWK IFGT
     C*
     C     RBMNRT        IFNE      *ZERO
     C     XXWK          IFLT      RBMNRT
     C                   Z-ADD     RBMNRT        XXRATE
     C                   ELSE
     C                   Z-ADD     XXWK          XXRATE
     C                   ENDIF                                                  RBMNRT IFNE
     C                   ENDIF                                                  XXWK   IFLT
     C*
     C                   ELSE
     C                   Z-ADD     0             XXRATE
     C                   ENDIF                                                  *IN50 IFEQ
     C**** BEGIN ADD **** 11/11/98 ************************************
     C                   ENDIF                                                  CMCRT IFEQ 0
     C**** END   ADD **** 11/11/98 ************************************
     C*
     C*
     C*   START READING PAYMENT FILE
     C*
     C     PAKY1         CHAIN     PPA02                              50
B1   C     *IN50         DOWEQ     '0'
     C*** BEGIN ADD *** 10/19/98 **************************************
     C     PATRDT        IFLE      LTFSDT
     C*** END ADD *** 10/19/98 ****************************************
     C*
     C*  IF REVERSE, REFUND, STOP PAY, OR VOID FIND CORRESPONDING PMT
     C*
     C     PATYPE        IFEQ      'Z'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   Z-ADD     PASEQ#        SVSEQ#            5 0
     C                   Z-ADD     PABASQ        XXSEQ#
     C     PAKY2         CHAIN     PPA03                              50
     C                   Z-ADD     SVSEQ#        XXSEQ#
     C     PAKY2         CHAIN     PPA03                              50
     C                   END
     C*
B2   C     PABR2         IFNE      *BLANKS
     C*
     C*  FIND CORRESPONDING AMOUNT ARRAY INDEX
     C*
     C                   Z-ADD     1             X
     C     PABR2         LOOKUP    TP(X)                                  50
B3   C     *IN50         IFEQ      '1'
     C                   Z-ADD     1             I                 3 0
     C     PL(X)         LOOKUP    PLC(I)                                 50
B4   C     *IN50         IFEQ      '1'
     C*
      **** BEG ADD **** 08/29/02 *****************************************
     C     I             IFEQ      28
     C                   Z-ADD     21            I
     C                   END
      **** END ADD **** 08/29/02 *****************************************
     C*
     C*  ONLY ACCUMULATE MISCELLANEOUS MEDICAL IF THE RESERVE TYPE = 'M'.
     C*
     C     PL(X)         IFEQ      'K'
     C     PARETP        ANDNE     'M'
     C                   GOTO      NXTPMT
     C                   END
     C*
B5   C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'A'
     C     PATYPE        OREQ      'Z'
     C*
     C     PL(X)         IFEQ      'L'
     C     PARETP        ANDEQ     'M'
      **** BEG ADD **** 08/29/02 *****************************************
     C     PL(X)         OREQ      'U'
     C     PARETP        ANDEQ     'M'
      **** END ADD **** 08/29/02 *****************************************
     C                   ADD       PAAMT         XXMSAM
     C                   ELSE
     C                   ADD       PAAMT         CRA(I)
     C                   END
     C*
 5   C                   ELSE
     C*
     C     PL(X)         IFEQ      'L'
     C     PARETP        ANDEQ     'M'
      **** BEG ADD **** 08/29/02 *****************************************
     C     PL(X)         OREQ      'U'
     C     PARETP        ANDEQ     'M'
      **** END ADD **** 08/29/02 *****************************************
     C                   SUB       PAAMT         XXMSAM
     C                   ELSE
     C                   SUB       PAAMT         CRA(I)
     C                   END
     C*
E5   C                   END
     C*
     C*  CALCULATE THE PERMANENT TOTAL DATE
     C*
     C     PL(X)         IFEQ      '4'
     C     XXPTDT        ANDEQ     0
     C                   Z-ADD     PAFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPTDT            6 0
     C                   END
     C*
     C*  CALCULATE THE COMPENSATION SETTLEMENT DATE
     C*
     C     PL(X)         IFEQ      'L'
     C     PARETP        ANDEQ     'C'
      **** BEG ADD **** 08/29/02 *****************************************
     C     PL(X)         OREQ      'U'
     C     PARETP        ANDEQ     'C'
      **** END ADD **** 08/29/02 *****************************************
     C                   Z-ADD     PAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXWODT            6 0
     C                   END
     C*
     C*  CALCULATE THE MEDICAL SETTLEMENT DATE
     C*
     C     PL(X)         IFEQ      'L'
     C     PARETP        ANDEQ     'M'
      **** BEG ADD **** 08/29/02 *****************************************
     C     PL(X)         OREQ      'U'
     C     PARETP        ANDEQ     'M'
      **** END ADD **** 08/29/02 *****************************************
     C                   Z-ADD     PAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXMSDT            6 0
     C                   END
     C                   EXSR      SRWKDY
     C*
E4   C                   END                                                    LOKUP PLC
     C*
E3   C                   END                                                    LOKUP TP
     C*
E2   C                   END                                                    PABR2
     C*** BEGIN ADD *** 10/19/98 **************************************
     C                   ENDIF                                                  END LTFSDT
     C*** END ADD *** 10/19/98 ****************************************
     C*
     C     NXTPMT        TAG
     C     PAKY1         READE     PPA02                                  50
E1   C                   END
     C*
     C*  USE THE DATA IN WSFRP IF IT EXISTS
     C*
     C     FRKY          CHAIN     PFR01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      SRWSFR
     C                   END
     C*
     C*   GET WORK FILE WHICH HOLDS ALL DATES
     C*
     C     L1KEY         CHAIN     PL101                              50
     C     *IN50         IFEQ      '0'
     C*
     C                   Z-ADD     L1TPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTPDT            6 0
     C*
     C                   END
     C*
     C*
     C*   CHECK FOR ADVANCES
     C*
     C                   MOVE      '0'           *IN33
     C                   Z-ADD     1             X
     C     'B'           LOOKUP    PLC(X)                                 50
     C     *IN50         IFEQ      '1'
     C     CRA(X)        IFNE      0
     C                   MOVE      '1'           *IN33
     C                   END
     C                   END
     C*
     C*    FLIP FULL SALARY DATE & MARK PROPER BOX
     C*
     C*** BEGIN CHG *** 10/19/98 **************************************
     C**********           Z-ADDLTFSDT    SRDTI
     C                   Z-ADD     CMFSDT        SRDTI
     C*** END CHG *** 10/19/98 ****************************************
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFSDT            6 0
     C*** BEGIN CHG *** 10/19/98 **************************************
     C********** LTFSAL    IFEQ 'Y'
     C     CMFSAL        IFEQ      'Y'
     C*** END CHG *** 10/19/98 ****************************************
     C                   MOVE      '1'           *IN38                          MARK 'YES'
     C                   ELSE
     C                   MOVE      '0'           *IN38                          MARK 'NO'
     C                   END
     C*
     C*  CLEAR LINE ITEM DATES IF THE AMOUNT = 0.
     C*
     C     CRA(4)        IFEQ      0
     C                   Z-ADD     0             XXPTDT                         PERMANENT TOTAL
     C                   END
     C*
     C     CRA(7)        IFEQ      0
     C                   Z-ADD     0             XXTPDT                         TEMPORARY PARTIAL
     C                   END
     C*
     C     CRA(21)       IFEQ      0
     C                   Z-ADD     0             XXWODT                         COMP SETTLEMENT
     C                   END
     C*
     C     XXMSAM        IFEQ      0
     C                   Z-ADD     0             XXMSDT                         MED SETTLEMENT
     C                   END
     C*
     C*  DISPLAY RECOVERY TOTALS AS POSITIVE
     C*
     C                   MULT      -1            CRA(25)                        FLIP SIGN
     C                   MULT      -1            CRA(26)                        FLIP SIGN
     C                   MULT      -1            CRA(27)                        FLIP SIGN
     C*
     C*  FIND TOTAL OF ALL PAYMENTS
     C*
     C                   Z-ADD     0             XXCRTL
     C                   Z-ADD     1             Z                 2 0
     C                   DO        24            Z
     C                   ADD       CRA(Z)        XXCRTL            9 2
     C                   END
     C                   ADD       XXMSAM        XXCRTL            9 2
     C                   SUB       CRA(22)       XXCRTL
     C                   SUB       CRA(23)       XXCRTL
     C*
     C*  DO NOT PRINT DATES IF ZERO.
     C*
     C     XXFSDT        COMP      *ZEROS                             2626
     C     XXTPDT        COMP      *ZEROS                             2727
     C     XXPTDT        COMP      *ZEROS                             2828
     C     XXMSDT        COMP      *ZEROS                             2929
     C     XXWODT        COMP      *ZEROS                             3030
     C*** BEGIN ADD *** 10/19/98 **************************************
     C     XXDSDT        COMP      *ZEROS                             8282
     C     XXWLF1        COMP      *ZEROS                             8383
     C     XXWLT1        COMP      *ZEROS                             8787
     C     XXWLF2        COMP      *ZEROS                             8888
     C     XXWLT2        COMP      *ZEROS                             8989
     C     XXMMI3        COMP      *ZEROS                             9090
     C*** END ADD *** 10/19/98 ****************************************
     C*
     C                   ENDSR
     C*
     C****************************************************
     C*   SUBROUTINE TO LOAD FIRST LAST NAME             *
     C****************************************************
     C     SRFLNM        BEGSR
     C                   MOVE      *BLANKS       XXNAME
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXFSN         XXFRST
     C                   MOVEL     XXLSN         XXLAST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXNAME           40
     C                   ENDSR
     C*
     C*
     C****************************************************
     C*   SUBROUTINE TO LOAD NAME                        *
     C****************************************************
     C     SRNAME        BEGSR
     C                   MOVE      *BLANKS       XXNAME
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXFSN         XXFRST
     C                   MOVEL     XXLSN         XXLAST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXNAME           40
     C                   ENDSR
     C*
     C****************************************************
     C*   SUBROUTINE CALCULATE WEEKS AND DAYS            *
     C****************************************************
     C     SRWKDY        BEGSR
     C*
     C*        CALCULATE WEEKS & DAYS
     C*
     C     PATYPE        IFEQ      'P'
     C     PAAMT         ANDGE     0
     C     PATYPE        OREQ      'Z'
     C     PAAMT         ANDGE     0
     C     PATYPE        OREQ      'A'
     C     PAAMT         ANDGE     0
     C     PATYPE        OREQ      'M'
     C     PAAMT         ANDGE     0
     C*** BEG ADD *** 07/17/03 **************************************
     C     PATYPE        OREQ      'R'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'S'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'V'
     C     PAAMT         ANDLT     0
     C*** END ADD *** 07/17/03 **************************************
     C*
     C                   ADD       PAWKS         WK(I)
     C     DY(I)         ADD       PADAYS        XXDAYS            2 0
     C     XXDAYS        IFGE      7
     C     XXDAYS        DIV       7             XXWEEK            1 0
     C                   ADD       XXWEEK        WK(I)
     C                   MULT      7             XXWEEK
     C     XXDAYS        SUB       XXWEEK        DY(I)
     C                   ELSE
     C                   Z-ADD     XXDAYS        DY(I)
     C                   END
     C*
     C                   ELSE
     C*
     C                   SUB       PAWKS         WK(I)
     C     DY(I)         SUB       PADAYS        XXDAYS
     C     XXDAYS        IFLT      0
     C                   SUB       1             WK(I)
     C     7             ADD       XXDAYS        DY(I)
     C                   ELSE
     C                   Z-ADD     XXDAYS        DY(I)
     C                   END
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C****************************************************
     C*   SUBROUTINE TO USE WSFRP FILE DATA FOR DWC13    *
     C****************************************************
     C     SRWSFR        BEGSR
     C***
     C*  GET TO DATE TOTALS
     C***
     C                   MOVE      'T'           XXTYPE
     C     FRKY1         CHAIN     PFR01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     FRTTWK        WK(1)
     C                   Z-ADD     FRTTDY        DY(1)
     C                   Z-ADD     FRTT          CRA(1)
     C                   Z-ADD     FRTRWK        WK(2)
     C                   Z-ADD     FRTRDY        DY(2)
     C                   Z-ADD     FRTR          CRA(2)
     C                   Z-ADD     FRCAWK        WK(3)
     C                   Z-ADD     FRCADY        DY(3)
     C                   Z-ADD     FRCA          CRA(3)
     C                   Z-ADD     FRPTWK        WK(4)
     C                   Z-ADD     FRPTDY        DY(4)
     C                   Z-ADD     FRPT          CRA(4)
     C                   Z-ADD     FRPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPTDT
     C                   Z-ADD     FRPSWK        WK(5)
     C                   Z-ADD     FRPSDY        DY(5)
     C                   Z-ADD     FRPS          CRA(5)
     C                   Z-ADD     FRDBWK        WK(6)
     C                   Z-ADD     FRDBDY        DY(6)
     C                   Z-ADD     FRDB          CRA(6)
     C                   Z-ADD     FRTP          CRA(7)
     C                   Z-ADD     FRWL          CRA(8)
     C                   Z-ADD     FRPI          CRA(9)
     C                   Z-ADD     FRPE          CRA(10)
     C                   Z-ADD     FRAV          CRA(11)
     C                   Z-ADD     FRMD          CRA(12)
     C                   Z-ADD     FRHP          CRA(13)
     C                   Z-ADD     FRMT          CRA(14)
     C                   Z-ADD     FRDG          CRA(15)
     C                   Z-ADD     FRFR          CRA(16)
     C                   Z-ADD     FRAC          CRA(17)
     C                   Z-ADD     FRRH          CRA(18)
     C                   Z-ADD     FRNS          CRA(19)
     C                   Z-ADD     FRMC          CRA(20)
     C                   Z-ADD     FRWODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXWODT
     C                   Z-ADD     FRWO          CRA(21)
     C     FRTH          MULT      -1            CRA(25)
     C     FRSD          MULT      -1            CRA(26)
     C                   Z-ADD     FRTPWK        WK(7)
     C                   Z-ADD     FRTPDY        DY(7)
     C                   Z-ADD     FRPIWK        WK(9)
     C                   Z-ADD     FRPIDY        DY(9)
     C                   Z-ADD     FRWLWK        WK(8)
     C                   Z-ADD     FRWLDY        DY(8)
     C                   Z-ADD     FRMS          XXMSAM
     C                   Z-ADD     FRMSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXMSDT
     C     FROR          MULT      -1            CRA(27)
     C                   Z-ADD     FRINT         CRA(24)
     C*
     C*  START READING PAYMENT FILE
     C*
     C     PAKY1         CHAIN     PPA02                              60
B1   C     *IN60         DOWEQ     '0'
     C*
     C*  IF PAYMENT DATE IS > DWC13 SUMMARY DATE, ADD PAYMENT TO TOTAL
     C*
B2   C     PATRDT        IFGT      FRDATE
     C*
B3   C     PABR2         IFNE      *BLANKS
     C*
     C*  FIND CORRESPONDING AMOUNT ARRAY INDEX
     C*
     C                   Z-ADD     1             X
     C     PABR2         LOOKUP    TP(X)                                  60
B4   C     *IN60         IFEQ      '1'
     C                   Z-ADD     1             I                 3 0
     C     PL(X)         LOOKUP    PLC(I)                                 60
B5   C     *IN60         IFEQ      '1'
     C*
      **** BEG ADD **** 08/29/02 *****************************************
     C     I             IFEQ      28
     C                   Z-ADD     21            I
     C                   END
      **** END ADD **** 08/29/02 *****************************************
     C*
B6   C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'A'
     C     PATYPE        OREQ      'Z'
     C*
     C     PL(X)         IFEQ      'L'
     C     PARETP        ANDEQ     'M'
      **** BEG ADD **** 08/29/02 *****************************************
     C     PL(X)         OREQ      'U'
     C     PARETP        ANDEQ     'M'
      **** END ADD **** 08/29/02 *****************************************
     C                   ADD       PAAMT         XXMSAM
     C                   ELSE
     C                   ADD       PAAMT         CRA(I)
     C                   END
     C*
 6   C                   ELSE
     C*
     C     PL(X)         IFEQ      'L'
     C     PARETP        ANDEQ     'M'
      **** BEG ADD **** 08/29/02 *****************************************
     C     PL(X)         OREQ      'U'
     C     PARETP        ANDEQ     'M'
      **** END ADD **** 08/29/02 *****************************************
     C                   SUB       PAAMT         XXMSAM
     C                   ELSE
     C                   SUB       PAAMT         CRA(I)
     C                   END
     C*
E6   C                   END
     C*
     C*  CALCULATE THE PERMANENT TOTAL DATE
     C*
     C     PL(X)         IFEQ      '4'
     C     XXPTDT        ANDEQ     0
     C                   Z-ADD     PAFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPTDT
     C                   END
     C*
     C*  CALCULATE THE COMPENSATION SETTLEMENT DATE
     C*
     C     PL(X)         IFEQ      'L'
     C     PARETP        ANDEQ     'C'
      **** BEG ADD **** 08/29/02 *****************************************
     C     PL(X)         OREQ      'U'
     C     PARETP        ANDEQ     'C'
      **** END ADD **** 08/29/02 *****************************************
     C                   Z-ADD     PAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXWODT
     C                   END
     C*
     C*  CALCULATE THE MEDICAL SETTLEMENT DATE
     C*
     C     PL(X)         IFEQ      'L'
     C     PARETP        ANDEQ     'M'
      **** BEG ADD **** 08/29/02 *****************************************
     C     PL(X)         OREQ      'U'
     C     PARETP        ANDEQ     'M'
      **** END ADD **** 08/29/02 *****************************************
     C                   Z-ADD     PAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXMSDT
     C                   END
     C*
     C*  CALCULATE WEEKS AND DAYS
     C*
     C                   EXSR      SRWKDY
     C*
E5   C                   END                                                    LOKUP PLC
     C*
E4   C                   END                                                    LOKUP TP
     C*
E3   C                   END                                                    PABR2
     C*
E2   C                   END
     C*
     C     PAKY1         READE     PPA02                                  60
E1   C                   END
     C*
     C                   END
     C*
     C*
     C                   ENDSR
     C****************************************************************
     C*  SRDART - SUBROUTINE TO WRITE A DARTS RECORD
     C****************************************************************
     C*
     C     SRDART        BEGSR
     C*** BEGIN DEL *** 04/26/00 **************************************
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     XXSYDT
     C********
     C****ESTABLISH KEY
     C********             MOVE *BLANKS   CMKEY
     C********             MOVE *BLANKS   QCKEY
     C********             MOVE XXCO#     QCCO#
     C********             MOVE LTFND     QCFND
     C********             MOVE LTFYR     QCFYR
     C********             MOVE LTCASE    QCCASE
     C********
     C****LOAD WORK FIELDS TO DARTS FILE FIELDS
     C********
     C********             MOVELQCKEY     CMKEY
     C********             MOVE *BLANKS   CMCSID
     C********             MOVE *BLANKS   CMCOMM
     C********             Z-ADDXXSYDT    CMCDAT
     C********             MOVEL*BLANKS   CMCODE
     C********             MOVE *BLANKS   CMDCID
     C********                                                         *****
     C****BEGIN CHG *** 02/23/98 *******************************************
     C**********           MOVEL'W4102R'  CMPNAM
     C********             MOVEL'W0070R'  CMPNAM
     C****END*  CHG *** 02/23/98 *******************************************
     C********                                                         *****
     C********             Z-ADDXXSYDT    CMTRDT
     C********             Z-ADDDSTIME    CMTRTM
     C********
     C********             MOVE *ZEROS    CMSECL
     C********             MOVE 'N'       CMSBYL
     C********             MOVE *BLANKS   CMSTAT
     C********             MOVE *BLANKS   CMUSER
     C********             MOVELQ1USER    CMUSER
     C********             MOVE *BLANKS   CMPGM
     C********             MOVELQ1PGM     CMPGM
     C********             Z-ADDDSTIME    CMTIME
     C********             Z-ADDXXSYDT    CMDT
     C********             MOVE *BLANK    CMIUSR
     C********             MOVELQ1USER    CMIUSR
     C********             MOVE *BLANKS   CMFOLD
     C********             MOVE *ZEROS    CMDCDT
     C********             MOVE 'N'       CMDIFL
     C********             MOVE *BLANKS   CMAPID           IMAGE FIELDS
     C********             MOVE *BLANKS   CMIDOC           |
     C********             MOVE *BLANKS   CMIQUE           |
     C********             MOVE *BLANKS   CMIMGF           |
     C********             MOVE *BLANKS   CMIPTH           |
     C********             MOVE *ZEROS    CMPIDX           |
     C********             MOVE *BLANKS   CMQPTY           |
     C********             MOVE *BLANKS   CMDCTP           |
     C********             MOVE *BLANKS   CMIPGM           |
     C********                                                         *********
     C****BEGIN DEL *** 02/23/98 ***********************************************
     C**********           MOVE *BLANKS   CMSMCD
     C****END*  DEL *** 02/23/98 ***********************************************
     C********                                                         *********
     C********             MOVE *BLANKS   CMFL01
     C********             MOVE *BLANKS   CMFL02
     C********             MOVE *BLANKS   CMFL03
     C********             MOVE *BLANKS   CMFL04
     C********             MOVE *BLANKS   CMFL05
     C********             Z-ADDXXSYDT    CMIDT
     C****BEGIN ADD *** 03/31/00 **************************************
     C********             MOVELQ1PGM     CMIPGM
     C****END*ADD *** 03/31/00 ****************************************
     C********
     C****GET*LAST SEQUENCE USED
     C********             MOVE *HIVAL    ##SEQ#
     C********   C3KY      SETGTPCM03
     C********             READPPCM03                    50
     C********   *IN50     IFEQ '0'
     C********   QCKEY     ANDEQC3KEY
     C********   C3SEQ#    ADD  1         CMSEQ#
     C********             ELSE
     C********             Z-ADD1         CMSEQ#
     C********             END
     C********
     C***WRITE DARTS RECORD
     C********
     C**DWC-04
     C********
     C********   *IN70     IFEQ '1'
     C********   XXDC04    ANDNE*BLANKS
     C********             MOVE *BLANKS   CMRMCD
     C********             MOVELXXDC04    CMRMCD
     C********                                                         *********
     C****BEGIN ADD *** 02/23/98 ***********************************************
     C********                                                         *********
     C********   CMRMCD    CHAINPCT01                60
     C********                                                         *********
     C********   *IN60     IFEQ *OFF
     C********             MOVE CTSMCD    CMSMCD
     C********             ENDIF                           *IN60=OFF
     C********                                                         *********
     C****END*  ADD *** 02/23/98 ***********************************************
     C********
     C********             MOVE *BLANKS   CMCOMM
     C********             MOVELRMK,1     CMCOMM
     C********             WRITEPCM02
     C******BEGIN ADD **** 07/01/99 ***********************************
     C********             FEOD SDCML
     C******END   ADD **** 07/01/99 ***********************************
     C********             ADD  1         CMSEQ#
     C********             END                             DWC4 FORM
     C********
     C**DWC-03
     C********
     C********   *IN71     IFEQ '1'
     C********   XXDC03    ANDNE*BLANKS
     C********             MOVE *BLANKS   CMRMCD
     C********             MOVELXXDC03    CMRMCD
     C********                                                         *********
     C****BEGIN ADD *** 02/23/98 ***********************************************
     C********                                                         *********
     C********   CMRMCD    CHAINPCT01                60
     C********                                                         *********
     C********   *IN60     IFEQ *OFF
     C********             MOVE CTSMCD    CMSMCD
     C********             ENDIF                           *IN60=OFF
     C********                                                         *********
     C****END*  ADD *** 02/23/98 ***********************************************
     C********
     C********             MOVE *BLANKS   CMCOMM
     C********             MOVELRMK,2     CMCOMM
     C********             WRITEPCM02
     C******BEGIN ADD **** 07/01/99 ***********************************
     C********             FEOD SDCML
     C******END   ADD **** 07/01/99 ***********************************
     C********             ADD  1         CMSEQ#
     C********             END                             DWC3 FORM
     C********
     C**DWC-13
     C********
     C********   *IN72     IFEQ '1'
     C********   XXDC13    ANDNE*BLANKS
     C********             MOVE *BLANKS   CMRMCD
     C********             MOVELXXDC13    CMRMCD
     C********                                                         *********
     C****BEGIN ADD *** 02/23/98 ***********************************************
     C********                                                         *********
     C********   CMRMCD    CHAINPCT01                60
     C********                                                         *********
     C********   *IN60     IFEQ *OFF
     C********             MOVE CTSMCD    CMSMCD
     C********             ENDIF                           *IN60=OFF
     C********                                                         *********
     C****END*  ADD *** 02/23/98 ***********************************************
     C********
     C********             MOVE *BLANKS   CMCOMM
     C********
     C********   *IN20     IFEQ '1'                        INITIAL DWC-13
     C********             MOVELRMK,3     CMCOMM
     C********             END
     C********
     C********   *IN21     IFEQ '1'                        FINAL DWC-13
     C********             MOVELRMK,4     CMCOMM
     C********             END
     C********
     C********   *IN22     IFEQ '1'                        ANNUAL DWC-13
     C********             MOVELRMK,5     CMCOMM
     C********             END
     C********
     C********             WRITEPCM02
     C******BEGIN ADD **** 07/01/99 ***********************************
     C********             FEOD SDCML
     C******END   ADD **** 07/01/99 ***********************************
     C********             ADD  1         CMSEQ#
     C********             END                             DWC-13 FORM
     C********
     C**DWC-12
     C********
     C********   *IN73     IFEQ '1'
     C********   XXDC12    ANDNE*BLANKS
     C********             MOVE *BLANKS   CMRMCD
     C********             MOVELXXDC12    CMRMCD
     C********                                                         *********
     C****BEGIN ADD *** 02/23/98 ***********************************************
     C********                                                         *********
     C********   CMRMCD    CHAINPCT01                60
     C********                                                         *********
     C********   *IN60     IFEQ *OFF
     C********             MOVE CTSMCD    CMSMCD
     C********             ENDIF                           *IN60=OFF
     C********                                                         *********
     C****END*  ADD *** 02/23/98 ***********************************************
     C********
     C********             MOVE *BLANKS   CMCOMM
     C********             MOVELRMK,6     CMCOMM
     C********             WRITEPCM02
     C******BEGIN ADD **** 07/01/99 ***********************************
     C********             FEOD SDCML
     C******END   ADD **** 07/01/99 ***********************************
     C********             ADD  1         CMSEQ#
     C********             END
     C*** END DEL *** 04/26/00 ****************************************
     C*** BEGIN ADD *** 04/26/00 **************************************
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      LTFND         QCFND
     C                   MOVE      LTFYR         QCFYR
     C                   MOVE      LTCASE        QCCASE
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  *BLANKS       Q#DCID           12
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   MOVEL(P)  'N'           Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANKS       Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANKS       Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C*
     C*          DWC-04  DARTS REMARK
     C*          ------------------------------------------------------
     C     *IN70         IFEQ      *ON
     C     XXDC04        ANDNE     *BLANKS
     C                   MOVEL(P)  XXDC04        Q#RMCD            5
     C                   MOVEL(P)  RMK(1)        Q#COMM          256
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   CALL      'DA700R'      DARTPL
     C                   ENDIF                                                  *IN70 = On
     C*
     C*          DWC-03  DARTS REMARK
     C*          ------------------------------------------------------
     C     *IN71         IFEQ      *ON
     C     XXDC03        ANDNE     *BLANKS
     C                   MOVEL     XXDC03        Q#RMCD
     C                   MOVEL(P)  RMK(2)        Q#COMM          256
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   CALL      'DA700R'      DARTPL
     C                   ENDIF                                                  *IN71 = On
     C*
     C*          DWC-13  DARTS REMARK
     C*          ------------------------------------------------------
     C     *IN72         IFEQ      *ON
     C     XXDC13        ANDNE     *BLANKS
     C                   MOVEL     XXDC13        Q#RMCD
     C     *IN20         IFEQ      *ON                                          INITIAL DWC-13
     C                   MOVEL(P)  RMK(3)        Q#COMM
     C                   ENDIF
     C     *IN21         IFEQ      '1'                                          FINAL DWC-13
     C                   MOVEL(P)  RMK(4)        Q#COMM
     C                   ENDIF
     C     *IN22         IFEQ      '1'                                          ANNUAL DWC-13
     C                   MOVEL(P)  RMK(5)        Q#COMM
     C                   ENDIF
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   CALL      'DA700R'      DARTPL
     C                   ENDIF                                                  *IN72 = On
     C*
     C*          DWC-12  DARTS REMARK
     C*          ------------------------------------------------------
     C     *IN73         IFEQ      '1'
     C     XXDC12        ANDNE     *BLANKS
     C                   MOVEL     XXDC12        Q#RMCD
     C                   MOVEL(P)  RMK(6)        Q#COMM
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   CALL      'DA700R'      DARTPL
     C                   ENDIF                                                  *IN73 = On
     C*** END ADD *** 04/26/00 ****************************************
     C*
     C                   ENDSR
     O***  DWC-12 NOTICE OF DENIAL
     OQSYSPRT1  E       73   DET              15
     O                       XXCL25              32
     O                       XXSS1               38
     O                                           39 '-'
     O                       XXSS2               41
     O                                           42 '-'
     O                       XXSS3               46
     O                       XXACDT              73 '0  /  /  '
     O          E       73   DET              18
     O                       XXCA25              32
     O                       XXENAM              60
     O          E       73   DET              19
     O                       XXCZ25              32
     O          E       73   DET              25
     O               24                          48 'X'
     O              N24                          65 'X'
     O          E       73   DET              26
     O                       LTDEN1              72
     O          E       73   DET              27
     O                       LTDEN2              72
     O          E       73   DET              28
     O                       LTDEN3              72
     O          E       73   DET              29
     O                       LTDEN4              72
     O          E       73   DET              30
     O                       LTDEN5              72
     O          E       73   DET              33
     O                       LTWHY0              72
     O          E       73   DET              34
     O                       LTWHY1              72
     O          E       73   DET              35
     O                       LTWHY2              72
     O          E       73   DET              36
     O                       LTWHY3              72
     O          E       73   DET              37
     O                       LTWHY4              72
     O          E       73   DET              38
     O                       LTWHY5              72
     O          E       73   DET              39
     O                       LTWHY6              72
     O          E       73   DET              40
     O                       LTWHY7              72
     O          E       73   DET              41
     O                       LTWHY8              72
     O          E       73   DET              42
     O                       LTWHY9              72
     O          E       73   DET              44
     O               25      XXRSCD              29 '0 /  /  '
     O          E       73   DET              47
     O                       LTDCC1              72
     O          E       73   DET              48
     O                       LTDCC2              72
     O          E       73   DET              49
     O                       LTDCC3              72
     O          E       73   DET              50
     O                       LTDCC4              72
     O          E       73   DET              53
     O                       XXEX25              31
     O                       F2AREA              47
     O                       XXSPH1              53
     O                       XXSPH2              61
     O                       XXEXT#              74
     O          E       73   DET              56
     O                       XXIAUD              12
     O               82      XXDSDT              29 '0  /  /  '
     O                       XXCONM              76
     O          E       73   DET              57
     O                       XCADR1              76
     O          E       73   DET              58
     O                       XCCTY               61
     O                       XCST                65
     O                       XCZIP               76 '     -    '
     O          E       73   DET              60
     O                       F2SCTC              12
     O                       LTCO#               24
     O                                           25 '-'
     O                       LTFND               28
     O                                           29 '-'
     O                       LTFYR               32
     O                                           33 '-'
     O                       LTCASE              40
     O          E       73   DET              64
     O                       LTUSER              79
     O***  DWC-3 REQUEST FOR LOSS OF WAGES/BENEFIT
     OQSYSPRT2  E       71   DET            2 06
     O                       XXIAUD              75
     O          E       71   DET              18
     O                       XXCL25              32
     O                       XXENAM              60
     O                       XXSS1               68
     O                                           69 '-'
     O                       XXSS2               71
     O                                           72 '-'
     O                       XXSS3               76
     O          E       71   DET              19
     O                       XXCA25              32
     O                       XXEAD1              60
     O          E       71   DET              20
     O                       XXCZ25              32
     O                       XXECSZ              60
     O          E       71   DET              23
     O                                           36 '('
     O                       EMAREA              39
     O                                           40 ')'
     O                       EMPHN#              49 '   -    '
     O                       XXACDT        Y     73
     O          E       71   DET              29
     O               83 61   XXWLF1        Y     46
     O               87 61   XXWLT1        Y     56
     O               88 62   XXWLF2        Y     69
     O               89 62   XXWLT2        Y     79
     O          E       71   DET              43
     O                       LTFND               57
     O                                           58 '-'
     O                       LTFYR               61
     O                       LTCASE              77
     O          E       71   DET              44
     O                       LTWL                 7
     O               90      XXMMI3        Y     30
     O                       LTWLR3        K     40
     O                       LTTP                42
     O                       LTCV                57
     O          E       71   DET              47
     O                       XXAWW         K     31
     O                       XXAWW         K     69
     O          E       71   DET              51
     O                       XXFACT        K     40
     O                       XXFACT        K     77
     O          E       71   DET              57
     O                       XXEX25              31
     O               82      XXDSDT              42 '0 /  /  '
     O          E       71   DET              64
     O                       LTUSER              79
     O***  DWC-13 PROGRESS/FINAL REPORT
     OQSYSPRT3  E       72   DET              21
     O                       XXCLNM              39
     O                       XXSS1               54
     O                                           55 '-'
     O                       XXSS2               57
     O                                           58 '-'
     O                       XXSS3               62
     O                       XXACDT              95 '0  /  /  '
     O          E       72   DET              24
     O               20                          11 'X'
     O          E       72   DET              25
     O               22                          11 'X'
     O                       CMAWW         J     96
     O                       XXRATE        J    115
     O          E       72   DET              27
     O               21                          11 'X'
     O          E       72   DET              28
     O               38                          60 'X'
     O               26      XXFSDT              95 '0  /  /  '
     O*
     O          E       72   DET              34
     O               27      XXTPDT              33 '0  /  /  '
     O                       WK(7)         4     43
     O                       DY(7)         4     52
     O                       CRA(7)        K     71
     O                       CRA(12)       K    115
     O          E       72   DET              37
     O                       WK(1)         4     43
     O                       DY(1)         4     52
     O                       CRA(1)        K     71
     O                       CRA(13)       K    115
     O          E       72   DET              39
     O                       WK(3)         4     43
     O                       DY(3)         4     52
     O                       CRA(3)        K     71
     O                       CRA(14)       K    115
     O          E       72   DET              42
     O                       WK(2)         4     43
     O                       DY(2)         4     52
     O                       CRA(2)        K     71
     O                       CRA(15)       K    115
     O          E       72   DET              44
     O              N23      WK(9)         4     43
     O              N23      DY(9)         4     52
     O              N23      CRA(9)        K     71
     O                       CRA(17)       K    115
     O          E       72   DET              47
     O               23      CRA(9)        K     71
     O                       CRA(19)       K    115
     O          E       72   DET              49
     O               23      CRA(8)        K     71
     O                       CRA(20)       K    115
     O          E       72   DET              52
     O              N23      WK(8)         4     43
     O              N23      DY(8)         4     52
     O              N23      CRA(8)        K     71
     O                       CRA(18)       K    115
     O          E       72   DET              55
     O               28      XXPTDT              33 '0  /  /  '
     O                       WK(4)         4     43
     O                       DY(4)         4     52
     O                       CRA(4)        K     71
     O               29      XXMSDT              92 '0  /  /  '
     O                       XXMSAM        K    115
     O          E       72   DET              57
     O                       WK(5)         4     43
     O                       DY(5)         4     52
     O                       CRA(5)        K     71
     O          E       72   DET              59
     O                       XXCRTL        K    115
     O          E       72   DET              60
     O                       WK(6)         4     43
     O                       DY(6)         4     52
     O                       CRA(6)        K     71
     O          E       72   DET              62
     O                       CRA(16)       K     71
     O          E       72   DET              63
     O                       CRA(25)       K    115
     O          E       72   DET              65
     O               30      XXWODT              40 '0  /  /  '
     O                       CRA(21)       K     71
     O          E       72   DET              66
     O                       CRA(26)       K    115
     O          E       72   DET              67
     O                       CRA(10)       K     71
     O          E       72   DET              69
     O                       CRA(24)       K     71
     O                       CRA(27)       K    115
     O          E       72   DET              71
     O               33                          16 'ADVANCES'
     O               33      CRA(11)       K     71
     O          E       72   DET              74
     O                       XXIAUD              18
     O               82      XXDSDT              40 '0  /  /  '
     O                       XXCONM             100
     O          E       72   DET              75
     O                       XCADR1             100
     O          E       72   DET              76
     O                       XCCTY               85
     O                       XCST                89
     O                       XCZIP              100 '     -    '
     O          E       72   DET              77
     O                                           71 '('
     O                       F2AREA              75
     O                                           77 ')'
     O                       XXSPH1              81
     O                                           83 '-'
     O                       XXSPH2              88
     O          E       72   DET              79
     O                       F2SCTC              18
     O                       LTCO#               34
     O                                           35 '-'
     O                       LTFND               38
     O                                           39 '-'
     O                       LTFYR               42
     O                                           43 '-'
     O                       LTCASE              50
     O          E       72   DET              84
     O                       LTUSER             120
     O*
     O***  DWC-4 -- NOTICE OF ACTION/CHANGE
     OQSYSPRT5  E       70   DET              14
     O                       XXCLNM              35
     O                       XXSS1               43
     O                                           44 '-'
     O                       XXSS2               46
     O                                           47 '-'
     O                       XXSS3               51
     O                       XXACDT              73 '0  /  /  '
     O          E       70   DET              17
     O               32      XXISTD              50 '0  /  /  '
     O                       LTISTT              73
     O          E       70   DET              18
     O               34      XXISUD              50 '0  /  /  '
     O                       LTISUT              73
     O          E       70   DET              19
     O               35      XXIRID              50 '0  /  /  '
     O                       LTIRIT              73
     O          E       70   DET              21
     O               36      XXIRCD              40 '0  /  /  '
     O                 N41 42                    64 'X'
     O                 N41N42                    70 'X'
     O          E       70   DET              22
     O               37      XXRTWD              40 '0  /  /  '
     O                 N43 44                    64 'X'
     O                 N43N44                    70 'X'
     O          E       70   DET              23
     O               39      XXAJDT              40 '0  /  /  '
     O          E       70   DET              24
     O               40      XXMMID              40 '0  /  /  '
     O                       LTPIRT        K     57
     O               48      XXDTHD              79 '0  /  /  '
     O          E    23 70   DET              26
     O               49      XXPID4              58 '0  /  /  '
     O          E   N23 70   DET              27
     O               49      XXPID4              58 '0  /  /  '
     O                       LTRCUP        M     79
     O          E   N23 70   DET              28
     O*** BEGIN DLT *** 07/07/00 **************************************
     O************************ LTWK# Z   79
     O*** END DLT ***** 07/07/00 **************************************
     O*** BEGIN ADD *** 07/07/00 **************************************
     O                 N91   LTWK#         Z     79
     O                  91   LTWKEN        4     79
     O*** END ADD ***** 07/07/00 **************************************
     O          E       70   DET              30
     O               74      XXPTD4              45 '0  /  /  '
     O          E       70   DET              31
     O                       LTPTRT        K     46
     O                       LTPAWW        K     79
     O          E       70   DET              32
     O               75      XXPTEF              45 '0  /  /  '
     O                       LTPCRT        K     79
     O          E       70   DET              33
     O                       LTAAWW        K     79
     O          E       70   DET              34
     O                       LTACRT        K     79
     O          E       70   DET              35
     O                       LTATY1              24
     O                       LTATY2              45
     O          E       70   DET              36
     O                       LTUNPT        K     25
     O                       LTOVPT        K     46
     O                  46 86                    64 'X'
     O                 N46 86                    70 'X'
     O          E       70   DET              37
     O               76      XXAEF1              24 '0  /  /  '
     O               77      XXAEF2              45 '0  /  /  '
     O          E       70   DET              38
     O               78      XXACN1              24 '0  /  /  '
     O               79      XXACN2              45 '0  /  /  '
     O               80      XXREEF              71 '0  /  /  '
     O          E       70   DET              41
     O                       LTCSS#               6
     O                  47   XNSS1               33
     O                  47                       34 '-'
     O                  47   XNSS2               36
     O                  47                       37 '-'
     O                  47   XNSS3               41
     O                       CMCLAS        Z     74
     O          E       70   DET              42
     O                       LTCDA                6
     O               81      XXNDA               41 '0  /  /  '
     O          E       70   DET              43
     O                       LTCNAM               6
     O                       LTNNAM              63
     O          E       70   DET              44
     O                       LTSAGT               6
     O                       LTNAGT              63
     O                  15   EMSIC               74
     O                 N15   CMSIC               74
     O          E       70   DET              46
     O                       LTRMK1              71
     O          E       70   DET              47
     O                       LTRMK2              71
     O          E       70   DET              48
     O                       LTRMK3              71
     O          E       70   DET              49
     O                       LTRMK4              71
     O          E       70   DET              51
     O                       LTCC1               71
     O          E       70   DET              52
     O                       LTCC2               71
     O          E       70   DET              53
     O                       LTCC3               71
     O          E       70   DET              54
     O                       LTCC4               71
     O          E       70   DET              56
     O                       XXIAUD              13
     O               82      XXDSDT              27 '0  /  /  '
     O                       XXCONM              78
     O          E       70   DET              57
     O                       XCADR1              78
     O          E       70   DET              58
     O                       XCCTY               63
     O                       XCST                66
     O                       XCZIP               78 '     -    '
     O          E       70   DET              59
     O                       F2SCTC              13
     O                       LTCO#               22
     O                                           23 '-'
     O                       LTFND               26
     O                                           27 '-'
     O                       LTFYR               30
     O                                           31 '-'
     O                       LTCASE              38
     O                                           49 '('
     O                       F2AREA              53
     O                                           55 ')'
     O                       XXSPH1              59
     O                                           61 '-'
     O                       XXSPH2              66
     O          E       70   DET              64
     O                       LTUSER              79
     O*
**  TABLE PLC VALUES
123456789ABCDEFGHIJKLMNSOPRU
**  TABLE RMK
DWC-4 form processed
DWC-3 form processed
Initial DWC-13 form processed
Final DWC-13 form processed
Annual DWC-13 form processed
DWC-12 form processed
