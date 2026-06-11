     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
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
     H* 05/05/09  C:190 Project WC1699(Priss#2)                       *
     H*                 Changed processing of 139WJE so that it is    *
     H*                 not client flagged.                           *
     H*                                                               *
     H* 07/24/08 C:190 PROJECT 139WJE(Priss#13)                       *
     H*                 Add Claims office phone #.  Print Claims      *
     H*                  office phone # on the DWC forms if not zero. *
     H*
     H* 05/30/08 C:190 PROJECT 139WJE                                 *
     H*                CHECK FOR THE EXISTENCE OF A CLAIM OFFICE      *
     H*                IN FILE WMOFL.                                 *
     H*                                                               *
     H* 11/14/07 C:180 SUPPORT 60917                                  *
     H*                Calculate Compensation Settlement              *
     H*                                                               *
     H* 09/21/05 C:132 Project: WC044 - 48764                         *
     H*                On DWC-13 Output (QSYSPRT3) add the "Payments  *
     H*                as of date" (LTFSDT) on line next to the       *
     H*                user ID (LTUSER).                              *
     H*                                                               *
     H* 09/20/05 C:61 SUPPORT WA135 48968                             *
     H*               Print Company second address line DWC-4.        *
     H*                                                               *
     H* 06/24/05 C:15 Project: 44WDL                                  *
     H*               Client mod to use FNANAM for the insured name.  *
     H*                                                               *
     H* 04/01/05 C:15 Project: WC1276                                 *
     H*               Re-aligned output for new DWC forms.            *
     H*                                                               *
     H* 03/08/05 C:53 Support: WC051/45997                            *
     H*               Print PI rating.                                *
     H*                                                               *
     H* 06/30/04 C:15 Support: WC126/43040                            *
     H*               Changed to alignment on the DWC4 for usage with *
     H*               the laser form, instead of pre-printed form.    *
     H*                                                               *
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
     F*** Begin add *** 04/01/05 **************************************
     FWMLLL     IF   E           K DISK
     F*** End add ***** 04/01/05 **************************************
     F*** BEG ADD *** 05/30/08 ****************************************
     FWMOFL     IF   E           K DISK
     F*** END ADD *** 05/30/08 ****************************************
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
     FQSYSPRT1  O    F  127        PRINTER
     F* DWC-3
     FQSYSPRT2  O    F  127        PRINTER
     F* DWC-13
     FQSYSPRT3  O    F  127        PRINTER
     F* DWC-4
     FQSYSPRT5  O    F  127        PRINTER
     F*** Begin add *** 04/01/05 **************************************
     F* DWC-19
     FQSYSPRT6  O    F  127        PRINTER
     F*** End add ***** 04/01/05 **************************************

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
     D*** BEG CHG *** 09/26/05 ****************************************
     D RMK             S             60    DIM(7) CTDATA PERRCD(1)              REMARKS
     D*** END CHG *** 09/26/05 ****************************************
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
     D*** BEG ADD *** 09/26/05 ****************************************
     D                 DS
     D  XXAL03                 1     10
     D  XXDC00                 1      5
     D  XXDC19                 6     10
     D*** END ADD *** 09/26/05 ****************************************
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
     C*** BEG ADD *** 05/30/08 ****************************************
     C*
     C*  OFFICE MASTER FILE
     C     OFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMOFF#
     C*** END ADD *** 05/30/08 ****************************************
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
     C*** BEG ADD *** 05/30/08 ****************************************
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       XXCLT#            3
     C                   READ      POF01                                  60
     C*** END ADD *** 05/30/08 ****************************************
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
     C*** Begin add *** 04/01/05 ***************************************
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
     C*** End add ***** 04/01/05 ***************************************
     C*
     C     *LIKE         DEFINE    LTCO#         XXCO#
     C     *LIKE         DEFINE    CMACDT        XXEFDT
     C     *LIKE         DEFINE    PASEQ#        XXSEQ#
     C     *LIKE         DEFINE    LTDT          XXDATE
     C*** Begin dlt *** 04/01/05 ***************************************
     C********   *LIKE     DEFN F2IAUD    XXIAUD
     C*** End dlt ***** 04/01/05 ***************************************
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
     C*** BEG ADD *** 09/26/05 ****************************************
     C                   MOVE      WCAL03        XXAL03
     C*** END ADD *** 09/26/05 ****************************************
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
     C*** Begin add *** 04/01/05 **************************************
     C     LTDW19        OREQ      'X'
     C*** End add ***** 04/01/05 **************************************
     C*
     C     LTDWC4        COMP      'X'                                    70
     C     LTDWC3        COMP      'X'                                    71
     C     LTDW13        COMP      'X'                                    72
     C     LTDW12        COMP      'X'                                    73
     C*** Begin add *** 04/01/05 **************************************
     C     LTDW19        COMP      'X'                                    69
     C*** End add ***** 04/01/05 **************************************
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
     C*** BEG ADD ***** 05/30/08 **************************************
     C*
     C*   CHECK FOR EXISTENCE OF CLIAM OFFICE IN FILE WMOFL
     C*
     C     CMKY          CHAIN     PCM01                              50
     C     OFKY          CHAIN     POF01                              60
     C     *IN60         IFEQ      *OFF
     C*** BEG DLT ***** 05/30/08 **************************************
     C********   XXCLT#    ANDEQ'139'
     C*** END DLT ***** 05/30/08 **************************************
     C                   MOVEL     OFCNAM        YXCONM           30
     C                   MOVEL     OFCAD1        YCADR1           30
     C                   MOVEL     OFCAD2        YCADR2           30
     C                   MOVEL     OFCCTY        YCCTY            15
     C                   MOVEL     OFCST         YCST              2
     C                   Z-ADD     OFCZP         YCZIP             9 0
     C*** BEG ADD ***** 07/24/08 **************************************
     C                   Z-ADD     OFCARA        YCARA             3 0
     C                   Z-ADD     OFCPH1        YCPH1             3 0
     C                   Z-ADD     OFCPH2        YCPH2             4 0
     C     YCARA         IFNE      *ZEROS
     C     YCPH1         ANDNE     *ZEROS
     C                   MOVE      '1'           *IN54
     C                   ELSE
     C                   MOVE      '0'           *IN54
     C                   ENDIF                                                  YCARA=*BLANKS
     C*** END ADD ***** 07/24/08 **************************************
     C                   MOVE      *ON           *IN55
     C     YCADR2        IFEQ      *BLANK
     C                   MOVE      '1'           *IN07
     C                   ELSE
     C                   MOVE      '0'           *IN07
     C                   ENDIF                                                  YCADR2=*BLANKS
     C                   ELSE
     C*** END ADD ***** 05/30/08 **************************************
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
     C*** BEG ADD ***** 05/30/08 **************************************
     C                   ENDIF                                                  OFKY=*IN60
     C*** END ADD ***** 05/30/08 **************************************
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
     C*** Begin dlt *** 04/01/05 **************************************
     C********             MOVELFNANAM    XXCONM
     C********             MOVELFNAAD1    XCADR1
     C********             MOVELFNAAD2    XCADR2
     C********             MOVELFNACTY    XCCTY
     C********             MOVELFNAST     XCST
     C********             Z-ADDFNAZIP    XCZIP
     C*
     C********   XCADR2    IFEQ *BLANK
     C********             MOVE '1'       *IN06
     C********             ELSE
     C********             MOVE '0'       *IN06
     C********             END
     C*** End dlt ***** 04/01/05 **************************************
     C*
     C                   ENDIF
     C*** END ADD *** 10/19/98 ****************************************
     C*** Begin add *** 04/01/05 **************************************
     C                   MOVEL     FNNAME        XXFNAM           40
     C*** End add ***** 04/01/05 **************************************
     C*** Begin add *** 06/24/05 ***************************************
     C     @@CLT#        IFEQ      '044'
     C                   MOVEL     FNANAM        XXFNAM
     C                   ENDIF
     C*** End add ***** 06/24/05 ***************************************
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
     C                   MOVE      CDST          XXSTAT            2
     C                   Z-ADD     CDZIP         XXZIP
     C                   MOVEL     CDCTY         XXCTY            25
     C                   EXSR      SRADDR
     C                   MOVE      *BLANKS       XXCCSZ
     C                   MOVEL     XXCSZ         XXCCSZ           40
     C*
     C*   GET EMPLOYER FILE LOAD NAME
     C*
     C     EMKEY         CHAIN     PEM01                              50
     C                   MOVE      *BLANKS       XXENAM
     C                   MOVEL     EMNAME        XXENAM           25
     C*
     C*   LOAD EMPLOYERS ADDRESS & PHONE
     C*
     C                   MOVE      EMMST         XXSTAT            2
     C                   Z-ADD     EMMZIP        XXZIP
     C                   MOVEL     EMMCTY        XXCTY            25
     C                   EXSR      SRADDR
     C                   MOVE      *BLANKS       XXECSZ
     C                   MOVEL     XXCSZ         XXECSZ           40
     C*
     C*   LOAD CLAIM EXAMINERS NAME
     C*
     C                   MOVE      *BLANKS       XXCLEX           30
     C                   MOVE      *BLANKS       XXTITL           30
     C                   MOVE      *BLANKS       XXTL17           17
     C     UPKEY         CHAIN     PUP01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXCLEX
     C                   MOVEL     UPTITL        XXTITL
     C                   MOVEL     UPTITL        XXTL17
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
     C*
     C*  GET INTERNAL AUDIT FIELD
     C*
     C     F2KEY         CHAIN     PF201                              50
     C*** Begin dlt *** 04/01/05 ***************************************
     C****************     Z-ADDF2IAUD    XXIAUD
     C*** End dlt ***** 04/01/05 ***************************************
     C*** Begin add *** 04/01/05 ***************************************
     C                   MOVEL(P)  F2IAUD        XXIAUD           15
     C     @@CLT#        IFEQ      '051'
     C                   MOVEL(P)  EMSEMP        XXIAUD
     C                   ENDIF
     C*** End add ***** 04/01/05 ***************************************
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
     C     XXRSCD        COMP      *ZEROS                             2525
     C*
     C*   LOAD DWC-13 INFORMATION, IF SELECTED
     C*
     C     LTDW13        IFEQ      'X'
     C                   EXSR      SRDW13
     C                   END
     C*
     C*** Begin add *** 04/01/05 ***************************************
     C*   LOAD DWC-19 INFORMATION, IF SELECTED
     C*
     C     LTDW19        IFEQ      'X'
     C                   EXSR      SRDW19
     C                   END
     C*** End add ***** 04/01/05 ***************************************
     C*
     C*   PRINT SELECTED CLAIM FORMS
     C*
      *
     C     CMSIC         IFEQ      *BLANKS
     C                   MOVE      *ON           *IN15
     C                   ELSE
     C                   MOVE      *OFF          *IN15
     C                   ENDIF                                                  END IFEQ *BLANK
      *
     C*** BEGIN ADD *** 10/19/98 **************************************
     C     XXDSDT        COMP      *ZEROS                             8282
     C     XXWLF1        COMP      *ZEROS                             8383
     C     XXWLT1        COMP      *ZEROS                             8787
     C     XXWLF2        COMP      *ZEROS                             8888
     C     XXWLT2        COMP      *ZEROS                             8989
     C     XXMMI3        COMP      *ZEROS                             9090
     C*** END ADD *** 10/19/98 ****************************************
      *
      ***** BEG ADD ***** 09/21/05 ************************************
     C                   Z-ADD     LTFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZFSDT            8 0
      ***** END ADD ***** 09/21/05 ************************************
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
     C*
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
     C*** Begin add *** 04/01/05 **************************************
     C     CMKY          CHAIN     PLL01                              60
     C   60              Z-ADD     0             LLISCM
     C   60              Z-ADD     0             LLMSCM
     C*
     C                   Z-ADD     LLISCM        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXWODT            6 0
     C                   Z-ADD     LLMSCM        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXMSDT            6 0
     C*
     C     LTFSDT        IFEQ      0
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTFSDT
     C                   END
     C*** End add ***** 04/01/05 **************************************
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
      ******** BEGIN CHANGE ******** 11/14/07 *************************
     C*****      PL,X      IFEQ 'L'
     C*****      PARETP    ANDEQ'M'
      **** BEG ADD **** 08/29/02 *****************************************
     C*****      PL,X      OREQ 'U'
     C*****      PARETP    ANDEQ'M'
      **** END ADD **** 08/29/02 *****************************************
     C*****                ADD  PAAMT     XXMSAM
     C*****                ELSE
     C*****                ADD  PAAMT     CRA,I
     C*****                END
      *
     C                   SELECT
     C     PL(X)         WHENEQ    'L'
     C     PARETP        ANDEQ     'M'
     C                   ADD       PAAMT         XXMSAM
     C     PL(X)         WHENEQ    'U'
     C     PARETP        ANDEQ     'M'
     C                   ADD       PAAMT         XXMSAM
     C     PL(X)         WHENEQ    'L'
     C     PARETP        ANDEQ     'C'
     C                   ADD       PAAMT         CRA(21)
     C     PL(X)         WHENEQ    'U'
     C     PARETP        ANDEQ     'C'
     C                   ADD       PAAMT         CRA(21)
     C                   OTHER
     C                   ADD       PAAMT         CRA(I)
     C                   ENDSL
      ******** END CHANGE ********** 11/14/07 *************************
     C*
 5   C                   ELSE
     C*
      ******** BEGIN CHANGE ******** 11/14/07 *************************
     C*****      PL,X      IFEQ 'L'
     C*****      PARETP    ANDEQ'M'
      **** BEG ADD **** 08/29/02 *****************************************
     C*****      PL,X      OREQ 'U'
     C*****      PARETP    ANDEQ'M'
      **** END ADD **** 08/29/02 *****************************************
     C*****                SUB  PAAMT     XXMSAM
     C*****                ELSE
     C*****                SUB  PAAMT     CRA,I
     C*****                END
      *
     C                   SELECT
     C     PL(X)         WHENEQ    'L'
     C     PARETP        ANDEQ     'M'
     C                   SUB       PAAMT         XXMSAM
     C     PL(X)         WHENEQ    'U'
     C     PARETP        ANDEQ     'M'
     C                   SUB       PAAMT         XXMSAM
     C     PL(X)         WHENEQ    'L'
     C     PARETP        ANDEQ     'C'
     C                   SUB       PAAMT         CRA(21)
     C     PL(X)         WHENEQ    'U'
     C     PARETP        ANDEQ     'C'
     C                   SUB       PAAMT         CRA(21)
     C                   OTHER
     C                   SUB       PAAMT         CRA(I)
     C                   ENDSL
      ******** END CHANGE ********** 11/14/07 *************************
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
     C*** Begin dlt *** 04/01/05 **************************************
     C************         Z-ADDPAPODT    SRDTI
     C************         CALL 'SRCVTDAT'DTMDY
     C************         MOVELSRDTO     XXWODT  60
     C*** End dlt ***** 04/01/05 **************************************
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
     C*** Begin dlt *** 04/01/05 **************************************
     C************         Z-ADDPAPODT    SRDTI
     C************         CALL 'SRCVTDAT'DTMDY
     C************         MOVELSRDTO     XXMSDT  60
     C*** End dlt ***** 04/01/05 **************************************
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
     C*                                   BASKETS AND BEYOND
     C                   ENDSR
     C*
     C*** Begin add *** 04/01/05 ***************************************
     C****************************************************
     C*   SUBROUTINE FOR DWC-19 OUTPUT                   *
     C****************************************************
     C     SRDW19        BEGSR
     C*
     C                   MOVEL     CDADR1        XXCAD1           30
     C     CDADR2        IFNE      *BLANKS
     C                   MOVEL     CDADR2        XXCAD2           30
     C                   MOVEL     XXCCSZ        XXCAD3           30
     C                   ELSE
     C                   MOVEL     XXCCSZ        XXCAD2
     C                   MOVE      *BLANKS       XXCAD3
     C                   ENDIF
     C*
     C     EMMAD1        IFNE      *BLANKS
     C*
     C                   MOVEL     EMMAD1        XXEAD1           30
     C     EMMAD2        IFNE      *BLANKS
     C                   MOVEL     EMMAD2        XXEAD2           30
     C                   MOVEL     XXECSZ        XXEAD3           30
     C                   ELSE
     C                   MOVEL     XXECSZ        XXEAD2
     C                   MOVE      *BLANKS       XXEAD3
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL     EMPAD1        XXEAD1
     C                   MOVE      EMPZIP        XXPZIP            9
     C     EMPAD2        IFNE      *BLANKS
     C                   MOVEL     EMPAD2        XXEAD2
     C                   MOVEL(P)  EMPCTY        XXEAD3
     C                   CAT       ',':0         XXEAD3
     C                   CAT       EMPST:1       XXEAD3
     C                   CAT       XXPZIP:1      XXEAD3
     C                   ELSE
     C                   MOVEL(P)  EMPCTY        XXEAD2
     C                   CAT       ',':0         XXEAD2
     C                   CAT       EMPST:1       XXEAD2
     C                   CAT       XXPZIP:1      XXEAD2
     C                   MOVE      *BLANKS       XXEAD3
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     LTEEFR        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEEFR            6 0
     C                   Z-ADD     LTEETO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEETO            6 0
     C*
     C     XXEEFR        COMP      *ZEROS                             66
     C     XXEETO        COMP      *ZEROS                             67
     C*
     C                   ENDSR
     C*** End add ***** 04/01/05 ***************************************
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
     C*** Begin dlt *** 04/01/05 **************************************
     C************         Z-ADDFRWODT    SRDTI
     C************         CALL 'SRCVTDAT'DTMDY
     C************         MOVELSRDTO     XXWODT
     C*** End dlt ***** 04/01/05 **************************************
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
     C*** Begin dlt *** 04/01/05 **************************************
     C************         Z-ADDFRMSDT    SRDTI
     C************         CALL 'SRCVTDAT'DTMDY
     C************         MOVELSRDTO     XXMSDT
     C*** End dlt ***** 04/01/05 **************************************
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
      **** BEGIN CHANGE *****   11/14/07 ******************************
     C*****      PL,X      IFEQ 'L'
     C*****      PARETP    ANDEQ'M'
      **** BEG ADD **** 08/29/02 *****************************************
     C*****      PL,X      OREQ 'U'
     C*****      PARETP    ANDEQ'M'
      **** END ADD **** 08/29/02 *****************************************
     C*****                ADD  PAAMT     XXMSAM
     C******               ELSE
     C******               ADD  PAAMT     CRA,I
     C*****                END
      *
     C                   SELECT
     C     PL(X)         WHENEQ    'L'
     C     PARETP        ANDEQ     'M'
     C                   ADD       PAAMT         XXMSAM
     C     PL(X)         WHENEQ    'U'
     C     PARETP        ANDEQ     'M'
     C                   ADD       PAAMT         XXMSAM
     C     PL(X)         WHENEQ    'L'
     C     PARETP        ANDEQ     'C'
     C                   ADD       PAAMT         CRA(21)
     C     PL(X)         WHENEQ    'U'
     C     PARETP        ANDEQ     'C'
     C                   ADD       PAAMT         CRA(21)
     C                   OTHER
     C                   ADD       PAAMT         CRA(I)
     C                   ENDSL
      **** END CHANGE *******   11/14/07 ******************************
     C*
 6   C                   ELSE
     C*
      **** BEGIN CHANGE *****   11/14/07 ******************************
     C*****      PL,X      IFEQ 'L'
     C*****      PARETP    ANDEQ'M'
      **** BEG ADD **** 08/29/02 *****************************************
     C*****      PL,X      OREQ 'U'
     C*****      PARETP    ANDEQ'M'
      **** END ADD **** 08/29/02 *****************************************
     C****                 SUB  PAAMT     XXMSAM
     C****                 ELSE
     C****                 SUB  PAAMT     CRA,I
     C****                 END
     C*
      *
     C                   SELECT
     C     PL(X)         WHENEQ    'L'
     C     PARETP        ANDEQ     'M'
     C                   SUB       PAAMT         XXMSAM
     C     PL(X)         WHENEQ    'U'
     C     PARETP        ANDEQ     'M'
     C                   SUB       PAAMT         XXMSAM
     C     PL(X)         WHENEQ    'L'
     C     PARETP        ANDEQ     'C'
     C                   SUB       PAAMT         CRA(21)
     C     PL(X)         WHENEQ    'U'
     C     PARETP        ANDEQ     'C'
     C                   SUB       PAAMT         CRA(21)
     C                   OTHER
     C                   SUB       PAAMT         CRA(I)
     C                   ENDSL
      **** END CHANGE *******   11/14/07 ******************************
      *
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
     C*** Begin dlt *** 04/01/05 **************************************
     C************         Z-ADDPAPODT    SRDTI
     C************         CALL 'SRCVTDAT'DTMDY
     C************         MOVELSRDTO     XXWODT
     C*** End dlt ***** 04/01/05 **************************************
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
     C*** Begin dlt *** 04/01/05 **************************************
     C************         Z-ADDPAPODT    SRDTI
     C************         CALL 'SRCVTDAT'DTMDY
     C************         MOVELSRDTO     XXMSDT
     C*** End dlt ***** 04/01/05 **************************************
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
     C*
     C*** BEG ADD *** 09/26/05 ****************************************
     C*          DWC-19  DARTS REMARK
     C*          ------------------------------------------------------
     C     *IN69         IFEQ      *ON
     C     XXDC19        ANDNE     *BLANKS
     C                   MOVEL(P)  XXDC19        Q#RMCD            5
     C                   MOVEL(P)  RMK(7)        Q#COMM          256
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   CALL      'DA700R'      DARTPL
     C                   ENDIF                                                  *IN70 = On
     C*** END ADD *** 09/26/05 ****************************************
     C*
     C                   ENDSR
     O*=========================================================================
     O***  DWC-12 NOTICE OF DENIAL
     OQSYSPRT1  E       73   DET              16
     O                       XXSS1                7
     O                                            8 '-'
     O                       XXSS2               10
     O                                           11 '-'
     O                       XXSS3               15
     O                       XXCLNM              82
     O                       XXACDT              95 '0  /  /  '
     O          E       73   DET              19
     O                       CDADR1              44
     O                       EMNAME              94
     O          E       73   DET              20
     O                       XXCCSZ              44
     O*       E   24      73   DET
     O*                24                48 'X'
     O*               N24                65 'X'
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
     O          E       73   DET              40
     O                       LTWHY0              72
     O          E       73   DET              41
     O                       LTWHY1              72
     O          E       73   DET              42
     O                       LTWHY2              72
     O          E       73   DET              43
     O                       LTWHY3              72
     O          E       73   DET              44
     O                       LTWHY4              72
     O          E       73   DET              45
     O                       LTWHY5              72
     O          E       73   DET              46
     O                       LTWHY6              72
     O          E       73   DET              47
     O                       LTWHY7              72
     O          E       73   DET              48
     O                       LTWHY8              72
     O          E       73   DET              49
     O                       LTWHY9              72
     O          E       73   DET              52
     O               25      XXRSCD              45 '0 /  /  '
     O          E       73   DET              62
     O                       LTDCC1              72
     O          E       73   DET              63
     O                       LTDCC2              72
     O          E       73   DET              64
     O                       LTDCC3              72
     O          E       73   DET              65
     O                       LTDCC4              72
     O          E       73   DET              67
     O                       F2AREA              66
     O                       XXSPH1              76
     O                       XXSPH2              92
     O                       XXEXT#             111
     O          E       73   DET              68
     O                       XXCLEX              41
     O          E       73   DET              71
     O                       XXFNAM             110
     O          E       73   DET              72
     O                       XXIAUD              25
     O               82      XXDSDT              45 '0  /  /  '
     O          E       73   DET              74
     O*** BEG CHG *** 05/30/08 ****************************************
     O               55      YXCONM             100
     O              N55      XXCONM             100
     O**************           XXCONM   100
     O*** END CHG *** 05/30/08 ****************************************
     O          E       73   DET              75
     O*** BEG CHG *** 05/30/08 ****************************************
     O               55      YCADR1             100
     O              N55      XCADR1             100
     O*************            XCADR1   100
     O*** END CHG *** 05/30/08 ****************************************
     O          E       73   DET              76
     O                       F2SCTC              10
     O                       LTCO#               40
     O                                           41 '-'
     O                       LTFND               44
     O                                           45 '-'
     O                       LTFYR               48
     O                                           49 '-'
     O                       LTCASE              56
     O*** BEG CHG *** 05/30/08 ****************************************
     O               55      YCCTY               85
     O               55      YCST                89
     O               55      YCZIP              100 '     -    '
     O              N55      XCCTY               85
     O              N55      XCST                89
     O              N55      XCZIP              100 '     -    '
     O*********                XCCTY     85
     O*********                XCST      89
     O*********                XCZIP    100 '     -    '
     O*** END CHG *** 05/30/08 ****************************************
     O          E       73   DET              82
     O                       LTUSER             115
     O*-------------------------------------------------------------------------
     O*
     O***  DWC-3 REQUEST FOR LOSS OF WAGES/BENEFIT
     OQSYSPRT2  E       71   DET              01
     O          E       71   DET              14
     O                       XXCL25              30
     O                       XXENAM              70
     O                       XXSS1               90
     O                                           91 '-'
     O                       XXSS2               93
     O                                           94 '-'
     O                       XXSS3               98
     O          E       71   DET              15
     O                       CDADR1              45
     O                       EMMAD1              85
     O          E       71   DET              16
     O                       XXCCSZ              45
     O                       XXECSZ              85
     O          E       71   DET              20
     O                                            6 '('
     O                       CDAREA               9
     O                                           10 ')'
     O                       CDPHN#              19 '   -    '
     O                                           46 '('
     O                       EMAREA              49
     O                                           50 ')'
     O                       EMPHN#              59 '   -    '
     O                       XXACDT        Y    100
     O          E       71   DET              50
     O                       LTWL                 5
     O               90      XXMMI3        Y     38
     O                       LTWLR3        K     55
     O                       LTTP                60
     O                       LTCV                82
     O          E       71   DET              51
     O               83 61   XXWLF1        Y     28
     O               87 61   XXWLT1        Y     45
     O               88 62   XXWLF2        Y     84
     O               89 62   XXWLT2        Y    101
     O          E       71   DET              54
     O                       XXAWW         K     23
     O                       XXAWW         K     80
     O*       E   55      71   DET
     O*                        XXFACTK   40
     O*                        XXFACTK   77
     O          E       71   DET              69
     O                       XXFNAM             110
     O          E       71   DET              70
     O                       XXCLEX              35
     O               82      XXDSDT              55 '0 /  /  '
     O          E       71   DET              72
     O                       XXCONM             100
     O          E       71   DET              73
     O                       XCADR1             100
     O          E       71   DET              74
     O                       XCCTY               85
     O                       XCST                89
     O                       XCZIP              100 '     -    '
     O          E       71   DET              79
     O                       LTFND               77
     O                                           78 '-'
     O                       LTFYR               81
     O                                           82 '-'
     O                       LTCASE              89
     O                       LTUSER             115
     O*  The next line is executed to create a second page so the
     O*  backside overlay prints.
     O          E       71   DET              06
     O*-------------------------------------------------------------------------
     O*
     O***  DWC-13 PROGRESS/FINAL REPORT
     OQSYSPRT3  E       72   DET              15
     O                       XXSS1                6
     O                                            7 '-'
     O                       XXSS2                9
     O                                           10 '-'
     O                       XXSS3               14
     O                       XXCLNM              74
     O                       XXACDT              95 '0  /  /  '
     O          E       72   DET              18
     O               20                           4 'X'
     O          E       72   DET              20
     O               22                           4 'X'
     O                       CMAWW         J     90
     O                       XXRATE        J    111
     O          E       72   DET              22
     O               21                           4 'X'
     O          E       72   DET              24
     O               38                          63 'X'
     O               26      XXFSDT             110 '0  /  /  '
     O*
     O          E       72   DET              29
     O               27      XXTPDT              42 '0  /  /  '
     O                       WK(7)         4     50
     O                       DY(7)         4     58
     O                       CRA(7)        K     75
     O                       CRA(12)       K    115
     O          E       72   DET              32
     O                       WK(1)         4     50
     O                       DY(1)         4     58
     O                       CRA(1)        K     75
     O                       CRA(13)       K    115
     O          E       72   DET              35
     O                       WK(3)         4     50
     O                       DY(3)         4     58
     O                       CRA(3)        K     75
     O                       CRA(14)       K    115
     O          E       72   DET              37
     O                       WK(2)         4     50
     O                       DY(2)         4     58
     O                       CRA(2)        K     75
     O                       CRA(15)       K    115
     O          E       72   DET              40
     O              N23      WK(9)         4     50
     O              N23      DY(9)         4     58
     O              N23      CRA(9)        K     75
     O                       CRA(17)       K    115
     O          E       72   DET              42
     O               23      CRA(9)        K     75
     O                       CRA(19)       K    115
     O          E       72   DET              45
     O               23      CRA(8)        K     75
     O                       CRA(20)       K    115
     O          E       72   DET              47
     O              N23      WK(8)         4     50
     O              N23      DY(8)         4     58
     O              N23      CRA(8)        K     75
     O                       CRA(18)       K    115
     O          E       72   DET              49
     O               28      XXPTDT              33 '0  /  /  '
     O          E       72   DET              50
     O                       WK(4)         4     50
     O                       DY(4)         4     58
     O                       CRA(4)        K     75
     O               29      XXMSDT              90 '0  /  /  '
     O                       XXMSAM        K    115
     O          E       72   DET              52
     O                       WK(5)         4     50
     O                       DY(5)         4     58
     O                       CRA(5)        K     75
     O          E       72   DET              54
     O                       XXCRTL        K    115
     O          E       72   DET              55
     O                       WK(6)         4     50
     O                       DY(6)         4     58
     O                       CRA(6)        K     75
     O          E       72   DET              58
     O                       CRA(16)       K     75
     O          E       72   DET              59
     O                       CRA(25)       K    115
     O          E       72   DET              60
     O               30      XXWODT              40 '0  /  /  '
     O                       CRA(21)       K     75
     O          E       72   DET              61
     O                       CRA(26)       K    115
     O          E       72   DET              63
     O                       CRA(10)       K     75
     O          E       72   DET              64
     O                       CRA(27)       K    115
     O          E       72   DET              65
     O                       CRA(24)       K     75
     O          E       72   DET              66
     O               33                          16 'ADVANCES'
     O               33      CRA(11)       K     75
     O          E       72   DET              68
     O                       XXFNAM             110
     O          E       72   DET              69
     O                       XXIAUD              25
     O               82      XXDSDT              45 '0  /  /  '
     O          E       72   DET              72
     O*** BEG CHG *** 05/30/08 ****************************************
     O               55      YXCONM             100
     O              N55      XXCONM             100
     O***************          XXCONM   100
     O*** END CHG *** 05/30/08 ****************************************
     O          E       72   DET              73
     O*** BEG CHG *** 05/30/08 ****************************************
     O               55      YCADR1             100
     O              N55      XCADR1             100
     O***************          XCADR1   100
     O*** END CHG *** 05/30/08 ****************************************
     O          E       72   DET              74
     O                       F2SCTC              10
     O                       LTCO#               40
     O                                           41 '-'
     O                       LTFND               44
     O                                           45 '-'
     O                       LTFYR               48
     O                                           49 '-'
     O                       LTCASE              56
     O*** BEG CHG *** 05/30/08 ****************************************
     O               55      YCCTY               85
     O               55      YCST                89
     O               55      YCZIP              100 '     -    '
     O              N55      XCCTY               85
     O              N55      XCST                89
     O              N55      XCZIP              100 '     -    '
     O***************          XCCTY     85
     O***************          XCST      89
     O***************          XCZIP    100 '     -    '
     O*** END CHG *** 05/30/08 ****************************************
     O          E       72   DET              75
     O*** BEG CHG *** 07/24/08 ****************************************
     O               54 55                       71 '('
     O               54 55   YCARA               75
     O               54 55                       77 ')'
     O               54 55   YCPH1               81
     O               54 55                       83 '-'
     O               54 55   YCPH2               88
     O              N55                          71 '('
     O              N55      F2AREA              75
     O              N55                          77 ')'
     O              N55      XXSPH1              81
     O              N55                          83 '-'
     O              N55      XXSPH2              88
     O************                       71 '('
     O************             F2AREA    75
     O************                       77 ')'
     O************             XXSPH1    81
     O************                       83 '-'
     O************             XXSPH2    88
     O*** END CHG *** 07/24/08 ****************************************
     O          E       72   DET              78
      ***** BEG ADD ***** 09/21/05 ************************************
     O                                           89 'Payments As Of Date:'
     O                       ZZFSDT        Y    100
      ***** END ADD ***** 09/21/05 ************************************
     O                       LTUSER             115
     O*-------------------------------------------------------------------------
     O*
     O***  DWC-4 -- NOTICE OF ACTION/CHANGE
     OQSYSPRT5  E       70   DET              01
     O          E       70   DET              16
     O                       XXSS1                7
     O                                            8 '-'
     O                       XXSS2               10
     O                                           11 '-'
     O                       XXSS3               15
     O                       XXCLNM              73
     O                       XXACDT              95 '0  /  /  '
     O***     E   14      70   DET
     O***              32      XXISTD    49 '0  /  /  '
     O***                      LTISTT    73
     O          E       70   DET              19
     O               34      XXISUD              65 '0  /  /  '
     O                       LTISUT             110
     O          E       70   DET              20
     O               35      XXIRID              65 '0  /  /  '
     O                       LTIRIT             110
     O          E       70   DET              23
     O               36      XXIRCD              47 '0  /  /  '
     O                 N41 42                    77 'X'
     O                 N41N42                    86 'X'
     O          E       70   DET              24
     O               37      XXRTWD              47 '0  /  /  '
     O          E       70   DET              25
     O                 N43 44                    77 'X'
     O                 N43N44                    86 'X'
     O          E       70   DET              26
     O               39      XXAJDT              47 '0  /  /  '
     O          E       70   DET              27
     O               40      XXMMID              47 '0  /  /  '
     O                       LTPIRA              75
     O               48      XXDTHD             108 '0  /  /  '
     O          E    23 70   DET              30
     O               49      XXPID4              80 '0  /  /  '
     O          E   N23 70   DET              31
     O               49      XXPID4              80 '0  /  /  '
     O                       LTRCUP        M    115
     O          E   N23 70   DET              33
     O                 N91   LTWK#         Z    115
     O                  91   LTWKEN        4    115
     O          E       70   DET              35
     O               74      XXPTD4              62 '0  /  /  '
     O          E       70   DET              36
     O                       LTPTRT        K     62
     O                       LTPAWW        K    115
     O          E       70   DET              38
     O               75      XXPTEF              62 '0  /  /  '
     O                       LTPCRT        K    115
     O          E       70   DET              40
     O                       LTAAWW        K    115
     O          E       70   DET              41
     O                       LTBAJ1              31
     O                       LTBAJ2              62
     O                       LTACRT        K    115
     O          E       70   DET              43
     O                       LTATY1              31
     O                       LTATY2              62
     O                  46 86                    95 'X'
     O                 N46 86                   105 'X'
     O          E       70   DET              44
     O               80      XXREEF             110 '0  /  /  '
     O          E       70   DET              45
     O                       LTUNPT        K     32
     O                       LTOVPT        K     63
     O          E       70   DET              47
     O               76      XXAEF1              31 '0  /  /  '
     O               77      XXAEF2              62 '0  /  /  '
     O          E       70   DET              49
     O               78      XXACN1              31 '0  /  /  '
     O               79      XXACN2              62 '0  /  /  '
     O          E       70   DET              52
     O                  47   XNSS1               50
     O                  47                       51 '-'
     O                  47   XNSS2               53
     O                  47                       54 '-'
     O                  47   XNSS3               58
     O          E       70   DET              53
     O                       LTCSS#               5
     O                       CMCLAS        Z    100
     O          E       70   DET              54
     O               81      XXNDA               58 '0  /  /  '
     O          E       70   DET              55
     O                       LTCDA                5
     O          E       70   DET              56
     O                       LTCNAM               5
     O                       LTNNAM              83
     O          E       70   DET              57
     O                       LTNAGT              83
     O                       EMNAIC             100
     O          E       70   DET              58
     O                       LTSAGT               5
     O          E       70   DET              60
     O                       LTRMK1              75
     O          E       70   DET              61
     O                       LTRMK2              75
     O          E       70   DET              62
     O                       LTRMK3              75
     O          E       70   DET              63
     O                       LTRMK4              75
     O          E       70   DET              66
     O                       LTCC1               75
     O          E       70   DET              67
     O                       LTCC2               75
     O          E       70   DET              68
     O                       LTCC3               75
     O          E       70   DET              69
     O                       LTCC4               75
     O                       XXFNAM             110
     O          E       70   DET              72
      ***** BEG CHG ******* 05/30/08 **********************************
     O               55      YXCONM             100
     O              N55      XXCONM             100
     O************             XXCONM   100
      ***** END CHG ******* 05/30/08 **********************************
     O          E       70   DET              73
     O                       XXIAUD              20
     O               82      XXDSDT              45 '0  /  /  '
      ***** BEG CHG ******* 05/30/08 **********************************
     O               55      YCADR1             100
     O              N55      XCADR1             100
     O*************            XCADR1   100
      ***** END CHG ******* 05/30/08 **********************************
     O          E       70   DET              74
      ***** BEG DEL ******* 09/20/05 **********************************
     O*********                XCCTY     85
     O*********                XCST      89
     O*********                XCZIP    100 '     -    '
     O******* E   75      70   DET
     O*******                            71 '('
     O*******                  F2AREA    75
     O*******                            77 ')'
     O*******                  XXSPH1    81
     O*******                            83 '-'
     O*******                  XXSPH2    88
      ***** END DEL ******* 09/20/05 **********************************
      ***** BEG CHG ******* 05/30/08 **********************************
      ***** BEG ADD ******* 09/20/05 **********************************
      ***** BEG CHG ******* 05/30/08 **********************************
     O              N07 55   YCADR2             100
     O               07 55   YCCTY               85
     O               07 55   YCST                89
     O               07 55   YCZIP              100 '     -    '
      *
     O          E       70   DET              75
     O              N07 55   YCCTY               85
     O              N07 55   YCST                89
     O              N07 55   YCZIP              100 '     -    '
     O              N06N55   XCADR2             100
     O               06N55   XCCTY               85
     O               06N55   XCST                89
     O               06N55   XCZIP              100 '     -    '
     O*******         N06      XCADR2   100
     O*******          06      XCCTY     85
     O*******          06      XCST      89
     O*******          06      XCZIP    100 '     -    '
      *
     O          E       70   DET              75
     O              N07 55   YCCTY               85
     O              N07 55   YCST                89
     O              N07 55   YCZIP              100 '     -    '
     O              N06N55   XCCTY               85
     O              N06N55   XCST                89
     O              N06N55   XCZIP              100 '     -    '
     O*******         N06      XCCTY     85
     O*******         N06      XCST      89
     O*******         N06      XCZIP    100 '     -    '
      ***** END CHG ******* 05/30/08 **********************************
      *
     O          E       70   DET              76
      ***** BEG CHG ******* 07/24/08 **********************************
     O               54 55                       71 '('
     O               54 55   YCARA               75
     O               54 55                       77 ')'
     O               54 55   YCPH1               81
     O               54 55                       83 '-'
     O               54 55   YCPH2               88
     O              N55                          71 '('
     O              N55      F2AREA              75
     O              N55                          77 ')'
     O              N55      XXSPH1              81
     O              N55                          83 '-'
     O              N55      XXSPH2              88
     O*******                            71 '('
     O*******                  F2AREA    75
     O*******                            77 ')'
     O*******                  XXSPH1    81
     O*******                            83 '-'
     O*******                  XXSPH2    88
      ***** END CHG ******* 07/24/08 **********************************
      ***** END ADD ******* 09/20/05 **********************************
      *
     O          E       70   DET              77
     O                       F2SCTC              10
     O                       LTCO#               33
     O                                           34 '-'
     O                       LTFND               37
     O                                           38 '-'
     O                       LTFYR               41
     O                                           42 '-'
     O                       LTCASE              49
     O          E       70   DET              81
     O                       LTUSER             110
     O*  The next line is executed to create a second page so the
     O*  backside overlay prints.
     O          E       70   DET              06
     O*-------------------------------------------------------------------------
     O*** Begin add *** 04/01/05 **************************************
     OQSYSPRT6  E       69   DET              16
     O                       XXSS1               12
     O                                           13 '-'
     O                       XXSS2               15
     O                                           16 '-'
     O                       XXSS3               20
     O                       XXCLNM              74
     O                       XXACDT        Y     95
     O          E       69   DET              19
     O                       XXCAD1              39
     O                       XXENAM              69
     O                       XXCONM             110
     O          E       69   DET              20
     O                       XXCAD2              39
     O                       XXEAD1              74
     O                       XCADR1             110
     O          E       69   DET              21
     O                       XXCAD3              37
     O                       XXEAD2              74
     O              N06      XCADR2             110
     O               06      XCCTY               95
     O               06      XCST                99
     O               06      XCZIP              110 '     -    '
     O          E       69   DET              22
     O                       XXEAD3              74
     O              N06      XCCTY               85
     O              N06      XCST                89
     O              N06      XCZIP              100 '     -    '
     O          E       69   DET              28
     O               66      XXEEFR        Y     25
     O               67      XXEETO        Y     50
     O          E       69   DET              69
     O                       XXCLEX              37
     O                       XXCONM             100
     O          E       69   DET              70
     O                       XCADR1             100
     O          E       69   DET              71
     O              N06      XCADR2             100
     O               06      XCCTY               85
     O               06      XCST                89
     O               06      XCZIP              100 '     -    '
     O          E       69   DET              72
     O              N06      XCCTY               85
     O              N06      XCST                89
     O              N06      XCZIP              100 '     -    '
     O               06                          71 '('
     O               06      F2AREA              75
     O               06                          77 ')'
     O               06      XXSPH1              81
     O               06                          83 '-'
     O               06      XXSPH2              88
     O          E       69   DET              73
     O                       XXTITL              37
     O                       XXDSDT        Y     60
     O              N06                          71 '('
     O              N06      F2AREA              75
     O              N06                          77 ')'
     O              N06      XXSPH1              81
     O              N06                          83 '-'
     O              N06      XXSPH2              88
     O*** End add ***** 04/01/05 **************************************
     O*-------------------------------------------------------------------------
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
DWC-19 form processed
