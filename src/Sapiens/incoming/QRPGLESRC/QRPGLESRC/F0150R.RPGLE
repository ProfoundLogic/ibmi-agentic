     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
      * *****************************************************************
     H*****************************************************************
     H*    PROGRAM: F0150R                                            *
     H*    DESCRIPTION: DELETE EOMB FROM SYSTEM                       *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/09/20 C:212 Project : SY420                               *
     H*                 Mask SSN                                      *
     H*                                                               *
     H*  04/08/99 C:56 Support: MC095 #17568                          *
     H*               1. Add CO#/SUB#/FND TO CALL OF F0575R           *
     H*                                                               *
     H*  10/14/98 C:53  REQ/PROJ: MC145                               *
     H*                 SWITCH FDCAL2 FOR FDCALL
     H*                                                               *
     H*  06/24/98 C:53  REQ/PROJ: MC145                               *
     H*                 SWITCH OLD NETWORK/AR FLAG TO NEW A/R FLAG
     H*                                                               *
     H* 06/05/92 KRR       - IF DETAIL CHARGES ARE TO BE DELETED THEN *
     H*                      UPDATE FDHCP FILE.                       *
     H*                                                               *
     H* 06/11/92 KRR       - WHEN EOMB IS TO BE DELETED UPDATE THE    *
     H*                      POSSIBLE DUPLICATE CLAIMS FILE.          *
     H*                                                               *
     H* 07/21/92 KRR       - ADDED SUB# TO ALL PROCESSING             *
     H*                                                               *
     H*  11/11/92 C:20  PROJ:                                         *
     H*                 1. ADDED INITIALIZATION OF DCCERT AND DCEXT   *
     H*                                                               *
     H*  12/07/92 C:20  PROJ: 99AAK                                   *
     H*                 1. CHANGED TO NOT ALLOW DELETE OF EOMB IF     *
     H*                    EOFLG4 IS EQUAL TO 'Y'                     *
     H*                                                               *
     H*  01/12/93 C:20  PROJ: 99AAN                                   *
     H*                 1. IF EOFLG4 = 'N' AND EOFLG5 = 'Y' THEN      *
     H*                    DELETE ASSOCIATED NDTMP RECORDS            *
     H*                                                               *
     H*  02/18/93 C:20  PROJ:                                         *
     H*                 1. ADDED CODE TO CHECK NOT ALLOW DELETION OF  *
     H*                    INVOICED EOMB                              *
     H*                                                               *
     H*  02/24/93 C:20  PROJ:99AAE                                    *
     H*                 1. ADDED CODE TO CORRECTLY INITALIZE DCZIPA   *
     H*                    AND DCPRTY                                 *
     H*                                                               *
     H*  02/26/93 C:20  PROJ:                                         *
     H*                 1. CHANGED CODE TO NOT ALLOW DELETION OF      *
     H*                    INVOICED EOMB ONLY IF NETWORK IS INSTALLED *
     H*                                                               *
     H*  03/26/93 C:20  PROJ:79/MC039                                 *
     H*                 1. ADDED CODE TO WRITE TO EOMB DELETE         *
     H*                    AUXILLARY FILE IF EOMB IS DELETED.         *
     H*                                                               *
     H*  04/19/93 C:20  PROJ:FS088                                    *
     H*                 1. ADDED CODE TO UPDATE DCBLTY IF DETAIL      *
     H*                    CHARGES ARE KEPT WHEN EOMB IS DELETED      *
     H*                                                               *
     H*  07/14/93 C:20  PROJ: FS121                                   *
     H*                 1. CHANGED TOTAL PAID AMOUNT TO REFLECT ANY   *
     H*                    NETWORK DISCOUNT TAKEN                     *
     H*                                                               *
     H*  09/02/93 C:20  REQ/PROJ: 123/MC100                           *
     H*                 1. ADDED CALL TO F0151F/R EOMB INVOICE/CHECK  *
     H*                    REQUEST INFORMATION.  THIS WILL HAPPEN     *
     H*                    BEFORE GOING TO SCR02                      *
     H*                 2. ADD F6-EOMB INV/CHK INFO TO SCR02.  THIS   *
     H*                    CALL NEW PROGRAM F0151R TO DISPLAY INFO    *
     H*                 3. IF WCMS CHECK/REQUEST HAS BEEN PROCESSED   *
     H*                    FOR THIS EOMB ONLY ALLOW DELETION IF UPLVL *
     H*                    IS GREATER THAN THE VALUE FOUND IN THE     *
     H*                    SYSTEM CONTROL FILE SFBPF/FBDT02.  IF THE  *
     H*                    USERS LEVEL IS NOT GREAT ENOUGH TO DELETE  *
     H*                    GIVE MESSAGE                               *
     H*                                                               *
     H*  10/11/93 C:20  REQ/PROJ: FSMNT                               *
     H*                 1. ONLY CHAIN TO FMF1P TO LOOK FOR OUTSIDE OR *
\    H*                    INSIDE ACCOUNT FOR WCMS CHECK REQUEST IF   *
     H*                    WCMS INSTALLED(I.E. *IN71 = 1)             *
     H*                                                               *
     H*  03/18/94 C:20  REQ/PROJ: 98/NW001                            *
     H*                 1. IF EOMB IS DELETED RE-CREATE FADCP RECORD  *
     H*                    WITH NETWORK PPO CODE INCLUDED.            *
     H*                                                               *
     H*  05/25/94 C:20  REQ/PROJ: MCMNT                               *
     H*                 1. IF EOMB IS DELETED RE-CREATE FADCP RECORD  *
     H*                    WITH BILL SUB-TYPE INCLUDED.               *
     H*                                                               *
     H*  12/04/96 C:59   UPDATE FROM FS060
     H*  01/25/95 C:20   REQ/PROJ: 12/60MAL                           *
     H*                  1. IF EOMB IS DELETE AND A BILLING AMOUNT    *
     H*                     IS FOUND IN EOBAMT A CREDIT MEMO WILL BE  *
     H*                     PRINTED FOR THIS BILLIGN AMOUNT.          *
     H*                                                               *
     H*  12/04/96 C:59  UPDATE FROM FS060
     H*  02/27/95 C:20  REQ/PROJ: 152/MC119                           *
     H*                 1. SCR02 - ADDED F14 TO CALL DARTS.  WE WILL  *
     H*                    STILL HAVE SAME DARTS PROCESSING FOR WCMS  *
     H*                    VS. WCMC KEY AND THE KEY WILL STILL BE THE *
     H*                    CASE KEY.                                  *
     H*                                                               *
     H*  03/16/95 C:20   REQ/PROJ: 12/60MAL                           *
     H*                  1. DO NOT ISSUE A REFUND MEMO IF THE CHECK   *
     H*                     IS STILL A REQUEST IN WARPP.  THE REFUND  *
     H*                     MEMO WILL ONLY BE ISSUED IF CHECK IS CUT  *
     H*                     ALREADY.                                  *
     H*                                                               *
     H*                                                               *
     H*  03/06/95 C:20   REQ/PROJ: 149/MC116                          *
     H*                  1. CHANGED LOGICAL USED TO CHAIN TO WDPAP    *
     H*                     FROM WDPAL4 TO WDPAL8.  THE NEW LOGICAL   *
     H*                     INCLUDES EOB# AT THE END.                 *
     H*                                                               *
     H*  06/15/95 C:33   REQ/PROJ:    /FL006                          *
     H*                  1. IF FADCP IS WRITTEN, COPY E17A01-04 TO    *
     H*                     DC7A01-04                                 *
     H*                                                               *
     H*  08/01/95 C:05   REQ/PROJ:    /FL006                          *
     H*                  1. Review of FAEOP/FAE1P to FADCP, revising  *
     H*                     program to update all necessary fields    *
     H*                     in FADCP (see doc. WCMC: EOE1_DC)         *
     H*                                                               *
     H*  12/04/96   C:59   UPDATE FROM FS060
     H*  05/21/96   C:53   REQ/PROJ: MC060 # 3937                     *
     H*                       GET CORRECT FEIN/MOD                    *
     H*                                                               *
     H*  03/18/97   C:43   NEW FILE FIELDS FADCP, FAE1P
     H*                                                               *
     H*  05/20/97   C:15   PROJECT: 87MBN                             *
     H*                    ADDED A CALL TO F000R6 WHEN AN EOMB IS     *
     H*                    DELETED.  THIS PGM WILL DELETE THE EDI     *
     H*                    837 LOG RECORDS FOR THE EOMB.              *
     H*                                                               *
     H*  06/25/97   C:32   PROJECT: 87MBN                             *
     H*                    CHGD CALL PARMS TO INCLUDE CO#,SUB#,FND,   *
     H*                    EOB#. CHG PGM TO SKIP SCR01. THIS PGM      *
     H*                    IS NOW CALLED FROM F0350R.                 *
     H*                                                               *
     H*  08/21/97   C:32   PROJECT: 87MCJ                             *
     H*                    ADDED PREVIOUS EOMB# TO FADCP.             *
     H*                    ADDED DELETE CRITERIA BASED ON EDI FMLGP   *
     H*                    STATUS CODE.                               *
     H*                                                               *
     H*  11/21/97   C:53   PROJECT: MC145                             *
     H*                    CONDITION EDI PROCESS ON INSTALL FLAG      *
     H*****************************************************************
     H*  INDICATOR USAGE                                              *
     H*  SCR02:                                                       *
     H*** BEGIN ADD *** TAPE SUBMISSION ***************************   *
     H*  30   NO DIAGNOSIS CODES IN EOB RECORD - CAN'T KEEP CHARGES   *
     H*  31   CURRENT CHARGES EXIST ALREADY - CAN'T KEEP CHARGES      *
     H*** END ADD ***** TAPE SUBMISSION ***************************   *
     H*****************************************************************
     FF0150F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMCML1    UF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFAEOL3    UF   E           K DISK
     F*** BEGIN ADD *** 03/26/93 **************************************
     FFAE1L     UF   E           K DISK
     F*** END ADD *** 03/26/93 ****************************************
     F*** BEG CHG *** 10/14/98 ****************************************
     F***FDCAL2  UF  E           K        DISK
     FFDCALL    UF   E           K DISK
     F*** BEG CHG *** 10/14/98 ****************************************
     FFDEOP     O    E           K DISK
     F                                     RENAME(PEO01:PDE01)
     F*** BEGIN ADD *** 03/26/93 **************************************
     FFDE1P     O    E           K DISK
     F                                     RENAME(PE101:PE102)
     F*** END ADD *** 03/26/93 ****************************************
     F*** ADD NEXT LINE *** TAPE SUBMISSION ***************************
     FFADCL     UF A E           K DISK
     FFDHCL     UF   E           K DISK
     F*** BEGIN ADD *** 06/11/92 **************************************
     FFADUL     UF   E           K DISK
     FFADUL1    UF   E           K DISK
     F                                     RENAME(PDU01:PDU02)
     F*** END ADD *** 06/11/92 ****************************************
     F*** BEGIN ADD *** 07/21/92 **************************************
     FFMSML     IF   E           K DISK
     F*** END ADD *** 07/21/92 ****************************************
     F*** BEGIN ADD *** 11/10/92 **************************************
     FFAHBL     UF   E           K DISK
     FNATRL     UF   E           K DISK
     F*** END ADD *** 11/10/92 ****************************************
     F*** BEGIN ADD *** 01/12/93 **************************************
     FNDTML1    UF   E           K DISK
     F*** END ADD *** 01/12/93 ****************************************
     F*** BEGIN ADD *** 01/12/93 **************************************
     FSSYLF     IF   E           K DISK
     F*** END ADD *** 01/12/93 ****************************************
     F*** BEGIN ADD *** 09/02/93 **************************************
     FWARPL     IF   E           K DISK    USROPN
     F*** BEGIN CHG *** 03/06/95 ***********************************************
     F********WDPAL4  IF  E           K        DISK                           UC
     FWDPAL8    IF   E           K DISK    USROPN
     F*** END CHG *** 03/06/95 *************************************************
     FFMF1L     IF   E           K DISK
     F*** END ADD *** 09/02/93 ****************************************
     F*** BEGIN ADD *** 08/21/97 **************************************
     FFMLGL     IF   E           K DISK
     FFXT1L     UF   E           K DISK
     FFXT2L     UF   E           K DISK
     F*** END ADD *** 08/21/97 ****************************************
     F*** BEGIN ADD *** 01/25/95 ***********************************************
     FFAERP     O    E           K DISK
     FFAERL1    IF   E           K DISK
     F                                     RENAME(PER01:PER02)
     F*** END ADD *** 01/25/95 *************************************************
     F*** BEGIN ADD 05/21/96 ***********************************
     FWMNRL1    IF   E           K DISK
     F*** END   ADD 05/21/96 ***********************************
     F*** BEGIN ADD *** 04/18/95 ***********************************************
     FWDPAL     UF A E           K DISK    USROPN
     F                                     RENAME(PPA01:PPA02)
     FWMCML1    UF   E           K DISK    USROPN
     F                                     RENAME(PCM01:PCM02)

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D*
     D                 DS
     D  CD                     1     20
     D                                     DIM(10)
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
     D  PA                     1     90  2
     D                                     DIM(10)
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
     D  RR                     1     90  2
     D                                     DIM(10)
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
     D*** END ADD *** 04/18/95 *************************************************
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D  DSYR                  11     12  0
     D*** BEGIN ADD *** 05/25/94 ***********************************************
     D                 DS
     D  XC10A1                 1     10
     D  XCNWCD                 1      5
     D  XCTYPE                 6      8
     D  XCDIHR                 9     10
     D*** END ADD *** 05/25/94 *************************************************
     D*** BEGIN ADD *** 08/01/95 ***********************************************
     D                 DS
     D  XC10A2                 1     10
     D  XCNMGR                 1      1
     D  XCNMGP                 2      6
     D  XCTYBL                 7      9
     D  XCPCMT                10     10
     D                 DS
     D  XC10A3                 1     10
     D*** BEG CHG *** 03/18/97 ***
     D************************                1   3 XCHTSV
     D  XCHTSV                 1      2
     D  XSCTOB                 3      3
     D  XCFILL                 4     10
     D*** END CHG *** 03/18/97 ***
     D*** END ADD ***** 08/01/95 ***********************************************
     D*                                                                *********
     D*** BEGIN ADD *** 03/02/95 ***********************************************
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCSUB#                 4      8  0
     D  QCFND                  9     11  0
     D  QCFYR                 12     14  0
     D  QCCASE                15     21  0
     D                 DS
     D  QWKEY                  1     40
     D  QWCO#                  1      3  0
     D  QWFND                  4      6  0
     D  QWFYR                  7      9  0
     D  QWCASE                10     16  0
     D*** END ADD *** 03/02/95 *************************************************
     D*** BEGIN ADD *** 04/19/95 ***********************************************
     D*
     D DESC            C                   CONST('ADJUSTMENT FOR RECON-
     D                                     SIDERATION')
     D*** END ADD *** 04/19/95 *************************************************
     D*** BEGIN ADD *** 03/18/97 ***********************************************
     D                 DS
     D  E1AMT4                 1      9  2
     D  X1AMT4                 1      9
     D*** END  ADD *** 03/18/97 ***********
     D*** BEGIN ADD *** 06/15/95 ***********************************************
     D                 DS
     D  XXE120                 1     20
     D  XXE11                  1      3
     D  XXE12                 15     17
     D  XXE13                 18     19
     D  XXE14                 20     20
     D                 DS
     D  XXDC10                 1     20
     D  XXDC1                  1      3
     D  XXDC2                  7      9
     D  XXDC3                  9     10
     D  XXDC4                 10     10
     D*** END   ADD *** 06/15/95 ***********************************************
     D*** BEGIN ADD *** 08/21/97 ***********************************************
     D                 DS
     D  Q1ACLC                 1     25
     D  Q1CO#                  1      3  0
     D  Q1SUB#                 4      8  0
     D  Q1FND                  9     11  0
     D  Q1FYR                 12     14  0
     D  Q1CASE                15     21  0
     D*** END   ADD *** 08/21/97 ***********************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     IPCM02
     I              CMOSCL                      WCOSCL

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C*** BEGIN ADD *** 07/21/92 **************************************
     C                   KFLD                    XXSUB#
     C*** END ADD *** 07/21/92 ****************************************
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE PAYEE MASTER FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C*
     C*  KEY LIST FOR THE EOB WORK FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    SCEOB#
     C*
     C*  KEY LIST FOR THE CASE DETAIL FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    XXCO#
     C*** BEGIN ADD *** 07/21/92 **************************************
     C                   KFLD                    XXSUB#
     C*** END ADD *** 07/21/92 ****************************************
     C                   KFLD                    XXFND
     C                   KFLD                    EOFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C******               KFLD           XXTRCD           DEL 10/14/98
     C                   KFLD                    SCEOB#
     C*** BEGIN ADD *** 01/12/93 **************************************
     C*
     C*  KEY LIST FOR THE T/M DETAIL CHARGE FILE
     C*
     C     TMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    EOFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCEOB#
     C*** END ADD *** 01/12/93 ****************************************
     C*
     C*** BEGIN ADD *** 03/26/93 **************************************
     C*
     C*  KEY LIST FOR THE EOB AUXILLARY FILE
     C*
     C     E1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    EOFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCEOB#
     C*** END ADD *** 03/26/93 ****************************************
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C*** BEGIN ADD *** 07/21/92 **************************************
     C                   KFLD                    XXSUB#
     C*** END ADD *** 07/21/92 ****************************************
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    SCCASE
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    SCSS#
     C*
     C*** BEGIN ADD *** TAPE SUBMISSION ***************************
     C*  KEY LIST FOR THE DIAG CODE FILE
     C*
     C     DCKEY         KLIST
     C                   KFLD                    EOCO#
     C*** BEGIN ADD *** 07/21/92 **************************************
     C                   KFLD                    EOSUB#
     C*** END ADD *** 07/21/92 ****************************************
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C                   KFLD                    XXDCS#
     C*
     C     DCKEY1        KLIST
     C                   KFLD                    EOCO#
     C*** BEGIN ADD *** 07/21/92 **************************************
     C                   KFLD                    EOSUB#
     C*** END ADD *** 07/21/92 ****************************************
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C*** END ADD ***** TAPE SUBMISSION ***************************
     C*
     C*  HOSPITAL DETAIL CHARGE FILE
     C     HCKY          KLIST
     C                   KFLD                    CACO#
     C*** BEGIN ADD *** 07/21/92 **************************************
     C                   KFLD                    CASUB#
     C*** END ADD *** 07/21/92 ****************************************
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CADSEQ
     C*** BEGIN ADD *** 06/11/92 **************************************
     C     DUKY          KLIST
     C                   KFLD                    CACO#
     C*** BEGIN ADD *** 07/21/92 **************************************
     C                   KFLD                    CASUB#
     C*** END ADD *** 07/21/92 ****************************************
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    CAFDAT
     C                   KFLD                    CACPT4                         CHG 04/28/92
     C                   KFLD                    CAPMOD
     C                   KFLD                    CADSEQ
     C*
     C     DUKY1         KLIST
     C                   KFLD                    CACO#
     C*** BEGIN ADD *** 07/21/92 **************************************
     C                   KFLD                    CASUB#
     C*** END ADD *** 07/21/92 ****************************************
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    CAFDAT
     C                   KFLD                    CACPT4                         CHG 04/28/92
     C                   KFLD                    CAPMOD
     C*
     C     DUKY2         KLIST
     C                   KFLD                    CACO#
     C*** BEGIN ADD *** 07/21/92 **************************************
     C                   KFLD                    CASUB#
     C*** END ADD *** 07/21/92 ****************************************
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    CAFDAT
     C                   KFLD                    CASVCD
     C                   KFLD                    CACPT4
     C                   KFLD                    CAUNTS
     C                   KFLD                    CADSEQ
     C*
     C     DUKY2A        KLIST
     C                   KFLD                    CACO#
     C*** BEGIN ADD *** 07/21/92 **************************************
     C                   KFLD                    CASUB#
     C*** END ADD *** 07/21/92 ****************************************
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    CAFDAT
     C                   KFLD                    CASVCD
     C                   KFLD                    CACPT4
     C                   KFLD                    CAUNTS
     C*** END CHG *** 06/11/92 ****************************************
     C*** BEG ADD *** 08/21/97 ****************************************
     C*
     C*  KEY LIST FOR THE EDI HISTORY FILE
     C*
     C     LGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCEOB#
     C*
     C*  KEY LIST FOR THE EDI 837 FILES (PRIMARY/DETAIL)
     C*
     C     T1KEY         KLIST
     C                   KFLD                    Q1ACLC
     C*** END ADD *** 08/21/97 ****************************************
     C*** BEGIN ADD *** 07/21/92 **************************************
     C*
     C*   KEY LIST FOR THE SUBSCRIBER
     C*
     C     SMKEY         KLIST
     C**********           KFLD           UPCO#            DEL 06/25/97
     C                   KFLD                    XXCO#                          ADD 06/25/97
     C                   KFLD                    XXSUB#
     C*** END ADD *** 07/21/92 ****************************************
     C*
     C*** BEGIN ADD *** 11/10/92 **************************************
     C*                                                    ## KK 8/3/89
     C*  KEY LIST FOR THE NETWORK TRANSACTION RECORD FILE  ##
     C*                                                    ##
     C     TRKEY         KLIST                                                  ##
     C                   KFLD                    XXCO#                          ##
     C                   KFLD                    XXSUB#                         ##
     C                   KFLD                    XXFND
     C                   KFLD                    EOFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCEOB#                         ##
     C*
     C*  KEY LIST FOR THE HBA TRANS FILE
     C*
     C     HBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    EOFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCEOB#
     C*** END ADD *** 11/10/92 ****************************************
     C*** BEGIN ADD *** 02/26/93 **************************************
     C*
     C*  FS BASE CONTROL FILE
     C*
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
     C*** END ADD *** 02/26/93 ****************************************
     C*** BEGIN ADD *** 09/02/93 **************************************
     C*
     C* THE WCMS REQUEST FOR PAYMENT FILE
     C*
     C     RPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWCF#
     C                   KFLD                    EOFYR
     C                   KFLD                    SCCASE
     C*
     C* THE WCMS PAYMENT DETAIL FILE
     C*
     C     PAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWCF#
     C                   KFLD                    XXCHK#
     C*** BEGIN ADD *** 03/06/95 ***********************************************
     C                   KFLD                    SCEOB#
     C*** END ADD *** 03/06/95 *************************************************
     C*** BEGIN ADD *** 04/18/95 ***********************************************
     C*
     C     PAKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWCF#
     C                   KFLD                    EOFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    XXSEQ#
     C*
     C* KEY LIST FOR THE WCMS CLAIM MASTER FILE
     C*
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWCF#
     C                   KFLD                    EOFYR
     C                   KFLD                    SCCASE
     C*** END ADD *** 04/18/95 *************************************************
     C*
     C*   KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*** END ADD *** 09/02/93 ****************************************
     C*** BEGIN ADD *** 01/25/95 ***********************************************
     C*
     C*   KEY LIST FOR THE RECON EOMB TRANSACTION FILE
     C*
     C     ERKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    EOFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCEOB#
     C                   KFLD                    X1SEQ#
     C*** END ADD *** 01/25/95 *************************************************
     C*** BEGIN ADD 05/21/96 ***********************************
     C*
     C*   KEY LIST FOR GROUP PROVIDER MASTER
     C*
     C     NRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXGRTY            1
     C*
     C*** END   ADD 05/21/96 ***********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C*** BEGIN ADD *** 06/25/97 **************************************
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@EOB#            9
     C                   PARM                    @@RTN             1
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
     C*** SET INDIC FOR BYPASS OF SCR01
     C     QQCALL        IFEQ      '4'
     C                   MOVE      '1'           *IN35
     C                   ENDIF
     C*** END   ADD *** 06/25/97 **************************************
     C*
     C* INITIALIZE WORK FIELDS
     C     *LIKE         DEFINE    EOFYR         XXFYR                          ADD 08/21/97
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C*** BEGIN ADD *** 07/21/92 **************************************
     C     *LIKE         DEFINE    UPSUB#        XXSUB#
     C*** END ADD *** 07/21/92 ****************************************
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    UPFYR         XUFYR
     C     *LIKE         DEFINE    CAFYR         QQFYR
     C     *LIKE         DEFINE    CACASE        QQCASE
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMACDT        XXACDT
     C     *LIKE         DEFINE    SCACDT        QQACDT
     C     *LIKE         DEFINE    DCDCS#        XXDCS#
     C*** BEGIN ADD *** 09/02/93 **************************************
     C     *LIKE         DEFINE    FNWCF#        XXWCF#
     C     *LIKE         DEFINE    UPLVL         XXDLVL
     C     *LIKE         DEFINE    EOWCCK        XXCHK#
     C*** END ADD *** /02/93   ****************************************
     C*** BEGIN ADD 05/21/96 ***********************************
     C     *LIKE         DEFINE    NRFEIN        XXFEIN
     C     *LIKE         DEFINE    NRMOD         XXMOD
     C*** END   ADD 05/21/96 ***********************************
     C*** BEGIN ADD *** 01/25/95 ***********************************************
     C                   MOVE      *ZEROS        X1SEQ#            5 0
     C                   MOVE      *ZEROS        XXSEQ#            5 0
     C*** END ADD *** 01/25/95 *************************************************
     C*** BEGIN ADD *** 04/18/95 ***********************************************
     C     *LIKE         DEFINE    PABR2         XXBR2
     C     *LIKE         DEFINE    PABR7         XXBR7
     C*** END ADD *** 04/18/95 *************************************************
     C*** BEGIN ADD *** 03/02/95 ***********************************************
     C                   Z-ADD     *ZEROS        X                 5 0
     C*** END ADD *** 03/02/95 *************************************************
     C                   Z-ADD     *ZEROS        X1                5 0          ADD 04/18/95
     C*
     C                   MOVE      'I'           XXTRTP            1
     C*** BEGIN ADD *** 02/26/93 **************************************
     C*
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              60
     C     *IN60         IFEQ      '0'
     C                   SUBST     FBAL04:5      XXAR              1            ADD 06/24/98
     C     XXAR          IFEQ      'Y'                                          ADD 06/24/98
     C********** FBFL06    IFEQ 'Y'                        DEL 06/24/98
     C                   MOVE      '1'           *IN74
     C                   ELSE
     C                   MOVE      '0'           *IN74
     C                   END                                                    END FBFL01
     C*** BEGIN ADD *** 09/02/93 **************************************
     C*
     C* CHECK TO SEE IF WCMS INTERFACE IS INSTALLED
     C*
     C     FBFL01        IFEQ      'Y'
     C                   OPEN      WARPL
     C*** BEGIN CHG *** 03/06/95 ***********************************************
     C**********           OPEN WDPAL4
     C                   OPEN      WDPAL8
     C*** END CHG *** 03/06/95 *************************************************
     C*** BEGIN CHG *** 04/18/95 ***********************************************
     C                   OPEN      WDPAL
     C                   OPEN      WMCML1
     C*** END CHG *** 04/18/95 *************************************************
     C                   MOVE      '1'           *IN71
     C                   ELSE
     C                   MOVE      '0'           *IN71
     C                   END
     C*
     C                   Z-ADD     FBDT02        XXDLVL
     C*** END ADD *** 09/02/93 ****************************************
     C                   END                                                    END *IN60
     C*** END ADD *** 02/25/93 ****************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C*** BEGIN ADD *** 07/21/92 **************************************
     C                   MOVE      *ZERO         UPSUB#
     C*** END ADD *** 07/21/92 ****************************************
     C                   MOVE      *ZERO         UPFND
     C                   END
     C*** BEGIN ADD *** 06/25/97 **************************************
     C     *IN35         IFEQ      '1'
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@SUB#        XXSUB#
     C                   MOVE      @@FND         XXFND
     C                   MOVE      @@CO#         UPCO#
     C                   MOVE      @@SUB#        UPSUB#
     C                   MOVE      @@FND         UPFND
     C                   ELSE
     C*** END   ADD *** 06/25/97 **************************************
     C                   MOVE      UPCO#         XXCO#
     C*** BEGIN ADD *** 07/21/92 **************************************
     C                   MOVE      UPSUB#        XXSUB#
     C*** END ADD *** 07/21/92 ****************************************
     C                   MOVE      UPFND         XXFND
     C                   ENDIF                                                   ADD 06/25/9
     C                   Z-ADD     UPFYR         XUFYR
     C*** BEGIN ADD *** 03/02/95 ***********************************************
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              60
     C     *IN60         IFEQ      '0'
     C     FBFL01        IFEQ      'Y'
     C     F1KEY         CHAIN     PF101                              50
     C     *IN50         IFEQ      '0'
     C     F1OUTA        IFEQ      'N'
     C                   Z-ADD     4             X
     C     1             SUBST     FBAL01:X      XDKEY             1    65
     C     XDKEY         IFEQ      'Y'
     C                   MOVE      'Y'           XWCKEY            1
     C                   ELSE
     C                   MOVE      'N'           XWCKEY
     C                   END                                                    END XDKEY
     C                   ELSE                                                   ELSE F1OUTA EQ 'N'
     C                   MOVE      'N'           XWCKEY
     C                   END                                                    END F1OUTA EQ 'N'
     C                   END                                                    END *IN50 EQ '0'
     C                   ELSE                                                   ELSE FBFL01 EQ 'Y'
     C                   MOVE      'N'           XWCKEY
     C                   END                                                    END FBFL01 EQ 'Y'
     C                   END                                                    END *IN60 FBKY
     C*** END ADD *** 03/02/95 *************************************************
     C*
     C*  GET THE COMPANY AND FUND RECORDS
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*** BEGIN ADD *** 07/21/92 **************************************
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C                   MOVE      UPSUB#        SCSUB#
     C                   MOVE      SMNAME        SCSBNM
     C*** END ADD *** 07/21/92 ****************************************
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      UPFND         SCFND
     C                   MOVE      FNNAME        SCNAME
     C*** BEGIN ADD *** 09/02/93 **************************************
     C                   Z-ADD     FNWCF#        XXWCF#
     C*** END ADD *** 09/02/93 ****************************************
     C*** BEGIN ADD 05/21/96 ***********************************
     C* CHAIN TO THE GROUP PROVIDER MASTER FILE
     C*
     C                   MOVE      'B'           XXGRTY
     C     NRKEY         CHAIN     PNR01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     NRFEIN        XXFEIN
     C                   Z-ADD     NRMOD         XXMOD
     C                   END
     C*** END   ADD 05/21/96 ***********************************
     C*** BEG ADD *** 11/07/97 ***
     C*** ACCESS EDI FLAG
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*
     C*** END ADD *** 11/07/97 ***
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*** BEGIN ADD *** 06/25/97 **************************************
     C     *IN35         IFEQ      '1'
     C                   MOVE      @@EOB#        SCEOB#
     C                   WRITE     SCR01
     C                   GOTO      SCR01T
     C                   ENDIF
     C*** END   ADD *** 06/25/97 **************************************
     C*****************************************************************
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C************************************
     C*   COMMAND 03 = END OF JOB        *
     C************************************
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
     C************************************
     C*   F6                             *
     C************************************
     C     *INKF         IFEQ      '1'
     C************************************
     C*   F6=EOMB SEARCH - SCR01         *
     C************************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'F0650R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCEOB#
     C                   PARM                    QQRTCD            1
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCEOB#        OREQ      *ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   GOTO      STRT                                         ADD 09/02/93
     C**********           GOTO SCR01T                     DEL 09/02/93
     C                   END
     C                   END
     C*** BEGIN ADD *** 09/02/93 **************************************
     C************************************
     C*   F6=EOMB INV/CHK INFO - SCR02   *
     C************************************
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C* CALL EOMB INVOICE/CHECK REQUEST INFORMATION WINDOW
     C*
     C                   MOVE      SCSUB#        QQSUB#            5
     C                   MOVE      SCEOB#        QQEOB#            9
     C                   CALL      'F0151R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM                    QQSUB#
     C                   PARM                    QQEOB#
     C                   PARM                    QQRTCD
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C                   GOTO      STRT
     C*
     C                   END                                                    END SCR02
     C*** END ADD *** 09/02/93 ****************************************
     C*
     C                   END                                                    END *INKF
     C************************************
     C*   COMMAND 07 = CASE # SEARCH     *
     C************************************
     C     *INKG         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'F0651R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCEOB#
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCEOB#        OREQ      *ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   GOTO      STRT                                         ADD 09/02/93
     C**********           GOTO SCR01T                     DEL 09/02/93
     C                   END
     C                   END
     C*
     C                   END
     C************************************
     C*   COMMAND 12 = PRIOR SCREEN      *
     C************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      TOP
     C                   END
     C*** BEGIN ADD *** 03/02/95 ***********************************************
     C***************************************
     C*  F14 = DARTS                        *
     C***************************************
     C     *INKN         IFEQ      '1'
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      *BLANKS       QWKEY
     C     XWCKEY        IFEQ      'Y'
     C                   MOVE      UPCO#         QWCO#
     C                   MOVE      XXWCF#        QWFND
     C                   Z-ADD     SCFYR         QWFYR
     C                   MOVE      SCCASE        QWCASE
     C*
     C*** CALL DARTS ENTRY PROGRAM
     C                   CALL      'SC010R'                             99
     C                   PARM                    QWKEY
     C                   PARM                    Q1PGM
     C                   ELSE                                                   END XWCKEY
     C*
     C                   MOVE      UPCO#         QCCO#
     C                   MOVE      UPSUB#        QCSUB#
     C                   MOVE      UPFND         QCFND
     C                   Z-ADD     SCFYR         QCFYR
     C                   MOVE      SCCASE        QCCASE
     C*
     C*** CALL DARTS ENTRY PROGRAM
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM                    Q1PGM
     C                   END                                                    END XWCKEY
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END                                                    END *INKM EQ '1'
     C*** END ADD *** 03/02/95 *************************************************
     C*****************************************************
     C*  CMD 16 - SEARCH DISPLAY PREVIOUSLY PAID CHARGES  *
     C*****************************************************
     C     *INKQ         IFEQ      '1'
     C*
     C                   MOVEL     Q2RECF        XXSCRN            5
     C     Q2RECF        IFEQ      'SCR02'
     C                   Z-ADD     SCFYR         QQFYR
     C                   Z-ADD     SCCASE        QQCASE
     C                   Z-ADD     SCACDT        QQACDT
     C                   GOTO      STRT12
     C                   END
     C*
     C     EOKEY         CHAIN     PEO01                              30
     C     *IN30         CABEQ     '1'           STRT
     C*
     C                   Z-ADD     EOFYR         QQFYR
     C                   Z-ADD     EOCASE        QQCASE
     C*
     C     STRT12        TAG
     C     *IN60         CABEQ     '1'           STRT
     C*
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   MOVE(P)   XXCO#         Q2CO#
     C                   MOVE(P)   XXSUB#        Q2SUB#
     C                   MOVE(P)   XXFND         Q2FND
     C*** END ADD *** 04/08/99 ****************************************
     C                   CALL      'F0575R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    XXSS#
     C*********************PARM           XXACDT
     C                   PARM                    QQACDT
     C                   PARM                    QQRTCD
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   PARM                    Q2CO#             3 0
     C                   PARM                    Q2SUB#            5 0
     C                   PARM                    Q2FND             3 0
     C*** END ADD *** 04/08/99 ****************************************
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     XXSCRN        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     XXSCRN        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C************************************
     C*   COMMAND 13 - CHG USER PARMS    *
     C************************************
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C*** BEGIN ADD *** 07/21/92 **************************************
     C                   MOVE      *ZERO         UPSUB#
     C*** END ADD *** 07/21/92 ****************************************
     C                   MOVE      *ZERO         UPFND
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C                   END                                                    END CMDKEYS
     C*****************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*****************************************************************
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*** VALIDATE EOMB #
     C*
     C     SCEOB#        CABEQ     *ZERO         STRT                     20
     C*
     C     EOKEY         CHAIN     PEO01                              40
     C*
     C     *IN40         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*** BEGIN ADD *** 09/02/93 **************************************
     C*
     C* CALL EOMB INVOICE/CHECK REQUEST INFORMATION WINDOW
     C*
     C                   MOVE      SCSUB#        QQSUB#            5
     C                   MOVE      SCEOB#        QQEOB#            9
     C                   CALL      'F0151R'
     C                   PARM                    QQPRDS
     C                   PARM      ' '           QQCALL
     C                   PARM                    QQSUB#
     C                   PARM                    QQEOB#
     C                   PARM                    QQRTCD
     C*** END ADD *** 09/02/93 ****************************************
     C*
     C*** SEE IF EOB HAS BEEN PRINTED
     C*
     C     EOSTAT        IFEQ      'P'
     C                   MOVE      '1'           *IN22
     C                   END
     C*** BEGIN ADD *** 12/07/92 **************************************
     C*
     C* IF EOFLG4 IS EQUAL TO 'Y' DO NOT ALLOW DELETE AND SEND ERROR
     C* MESSAGE
     C*
     C     EOFLG4        IFEQ      'Y'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*** END ADD *** 12/07/92 ****************************************
     C*
     C                   Z-ADD     EOFEIN        SCFEIN
     C                   MOVE      EOFYR         SCFYR
     C                   MOVE      EOFYR         XXFYR                          ADD 08/21/97
     C                   Z-ADD     EOCASE        SCCASE
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFRDT
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
     C**42591*TAA*****     Z-ADDEOAMT     SCTOTL
     C                   MOVE      'N'           SCDELF
     C*
     C*** BEGIN ADD *** 4/25/91 *TAA**********************************
     C*********************Z-ADDEOAMT     SCTOTP           DEL 07/14/93
     C     EOAMT         SUB       EONDIS        SCTOTP                         ADD 07/14/93
     C                   Z-ADD     EOAMTC        SCTOTC
     C*** END ADD ***** 4/25/91 *TAA**********************************
     C*** BEGIN ADD *** 07/21/92 **************************************
     C*
     C*  GET SUBSCRIBER INFORMATION
     C*
     C                   MOVE      EOSUB#        XXSUB#
     C                   MOVE      EOSUB#        SCSUB#
     C     SMKEY         CHAIN     PSM01                              60
     C                   MOVE      SMNAME        SCSBNM
     C*** END ADD *** 07/21/92 ****************************************
     C*
     C*  GET FUND INFORMATION
     C*
     C                   MOVE      EOFND         XXFND
     C                   MOVE      EOFND         SCFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      FNNAME        SCNAME
     C*** BEGIN ADD *** 09/02/93 **************************************
     C                   Z-ADD     FNWCF#        XXWCF#
     C*** END ADD *** 09/02/93 ****************************************
     C*
     C*** GET PROVIDER INFORMATION
     C     PDKEY         CHAIN     PPD01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     PDNAME        QQLAST           56
     C                   MOVEL     PDFSN         QQFRST           15
     C                   MOVEL     '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCPNAM
     C                   END
     C*
     C*** GET CLAIM INFORMATION
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C                   MOVE      CMSS#         SCSS#
     C*** 3/16/89 ADD NEXT LINE (##)
     C                   Z-ADD     CMACDT        XXACDT
     C                   EXCEPT    ECM01
     C                   END
     C*
     C*** GET CLAIMAINT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              70
     C     *IN70         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVEL     '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCCNAM
     C                   END
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       scSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*****************************************************************
     C*   PROCESS INFORMATION SCREEN
     C*
     C     SCR02T        TAG
     C*** BEGIN ADD *** 11/10/92 *************************************
     C*  IF DELETE IS REQUESTED, CHECK FOR NATRP RECORD
     C*  IF NATRP RECORD HAS BEEN INVOICED (I.E. TRFLG4 NE *BLANK)
     C*  THEN THE DELETE IS NOT ALLOWED, AND THE USER IS INSTRUCTED
     C*  TO DELETE THE ASSOCIATED NETWORK INVOICE THRU REBILL BEFORE
     C*  HE CAN CONTINUE WITH THIS DELETE.
     C*
     C     TRKEY         CHAIN     PTR01                              60
     C     *IN60         IFEQ      '0'
     C     TRINV#        IFNE      *ZEROS                                        03/02/90 ADD
     C                   MOVE      '1'           *IN23                          CANNOT DELETE
     C                   GOTO      STRT
     C                   END                                                    END EQ 'Y'
     C                   END                                                    END 0 *IN60
     C*** BEGIN ADD *** 02/18/93 **************************************
     C*
     C     *IN74         IFEQ      '1'                                          ADD 02/26/93
     C     EOINV#        IFNE      *ZEROS
     C                   MOVE      '1'           *IN24                          CANNOT DELETE
     C                   GOTO      STRT
     C                   END                                                    END 0 *IN60
     C                   END                                                    END *IN74
     C*** END ADD *** 02/18/93 ****************************************
     C*** BEGIN ADD *** 09/02/93 **************************************
     C     *IN71         IFEQ      '1'                                          ADD 10/11/93
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      '0'
     C     F1OUTA        IFEQ      'N'
     C                   MOVE      '0'           *IN99
     C*
     C* CHAIN TO WCMS REQUEST FOR PAYMENT FILE TO SEE IF EOMB HAS PAYMENT
     C* REQUEST PROCESSED AGAINST IT
     C*
     C     RPKEY         CHAIN     PRP01                              50
     C     *IN50         DOWEQ     '0'
     C     RPEOB#        IFEQ      SCEOB#
     C                   MOVE      '1'           *IN99
     C                   GOTO      LVLCHK
     C                   END                                                    END RPEOB#
     C     RPKEY         READE     PRP01                                  50
     C                   END                                                    END *IN50
     C*
     C* CHAIN TO WCMS PAYMENT DETAIL FILE TO SEE IF EOMB HAS PAYMENT
     C* PROCESSED AGAINST IT
     C*
     C                   Z-ADD     EOWCCK        XXCHK#
     C     XXCHK#        IFNE      *ZEROS
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN99
     C                   GOTO      LVLCHK
     C                   END                                                    END *IN60
     C                   END                                                    END XXCHK#
     C*
     C* CHAIN TO EOMB AUXILIARY FILE TO SEE IF A OUTSIDE PPO OR BILL REVIEW
     C* CHECK HAS BEEN PROCESSED AGAINST THIS EOMB
     C*
     C     E1KEY         CHAIN     PE101                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    RE101
     C*
     C     E110A1        IFNE      *BLANKS
     C                   MOVEL     E110A1        XXCHK#
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN99
     C                   GOTO      LVLCHK
     C                   END                                                    END *IN60
     C                   END                                                    END E110A1
     C*
     C     E110A2        IFNE      *BLANKS
     C                   MOVEL     E110A2        XXCHK#
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN99
     C                   GOTO      LVLCHK
     C                   END                                                    END *IN60
     C                   END                                                    END E110A2
     C*
     C                   END                                                    END *IN60 E1
     C*
     C                   END                                                    END F1OUTA
     C                   END                                                    END *IN60
     C*
     C* IF WCMS CHECK REQUEST HAS BEEN PROCESSED USERS SECURITY LEVEL
     C* MUST BE GREATER THAN THE SECURITY LEVEL FOUND IN THE SYSTEM
     C* CONTROL FILE.  IF USER LEVEL IS LESS THAN THE SYSTEM CONTROL
     C* LEVEL SEND MESSAGE AND DO NOT ALLOW DELETION
     C*
     C     LVLCHK        TAG
     C     *IN99         IFEQ      '1'
     C     UPLVL         IFLT      XXDLVL
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   END                                                    END *IN71
     C*** END ADD *** 09/02/93 ****************************************
     C*
     C* CHECK TO SEE IF THIS HAS AN HBA RECORD
     C*
     C     HBKEY         CHAIN     PHB01                              79        ##LJD 10/6
     C     *IN79         IFEQ      '0'                                          ##
     C**********           MOVE HB1AAM    EOBAMT           DEL 01/25/95
     C     HBKEY         DELETE    PHB01                              85
     C                   ELSE                                                   ##LJD 10/6
     C*--- DELETED CODE #5
     C                   END                                                    ## END NE 4
     C*** BEGIN ADD *** 08/21/97 *************************************
     C     ##EDI         IFEQ      'Y'                                          ADD 11/21/97
     C*** CHECK STATUS IN EDI LOG FILE - IF ANY RECS FOR THIS EOMB#
     C***       HAVE SN,TA, OR TE STATUS - DO NOT ALLOW DELETE
     C                   Z-ADD     *ZEROS        XXDLOG            5 0
     C     LGKEY         CHAIN     PLG01                              60
     C     *IN60         DOWEQ     '0'
     C     LGSTS         IFEQ      'SN'
     C     LGSTS         OREQ      'TA'
     C     LGSTS         OREQ      'TE'
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   ELSE
     C     LGSTS         IFEQ      'EX'
     C     LGSTS         OREQ      'TR'
     C                   ADD       1             XXDLOG                         OK TO DEL ST
     C                   ENDIF                                                  LGSTS = EX
     C                   ENDIF                                                  LGSTS = SN
     C     LGKEY         READE     PLG01                                  60
     C                   ENDDO                                                  *IN60
     C                   ENDIF                                                  ADD 11/21/97
     C*** END   ADD *** 08/21/97 *************************************
     C*** BEGIN ADD *** 08/16/89 *************************************
     C*  VALIDATE DELETE DETAIL RECORDS FLAG
     C*
     C     SCDELF        IFNE      'Y'
     C     SCDELF        ANDNE     'N'
     C                   MOVE      '1'           *IN20                          Y/N REQD
     C                   GOTO      STRT
     C                   END
     C*** END ADD ***** 08/16/89 *************************************
     C*
     C*  CHECK DELETE DETAIL RECORDS FLAG
     C*
     C     SCDELF        IFEQ      'Y'
     C*
     C*  DELETE DETAIL RECORDS IN CASE DETAIL FILE
     C*********            MOVE *BLANKS   XXTRCD  2        DEL 10/14/98
     C     CAKEY         CHAIN     PCA01                              80
     C     *IN80         DOWEQ     '0'
     C     HCKY          CHAIN     PHC01                              81
     C     *IN81         IFEQ      '0'
     C                   DELETE    PHC01
     C                   END
     C*** BEGIN ADD *** 06/11/92 **************************************
     C     CAPRTY        IFEQ      '4'
     C     DUKY2         DELETE    PDU02                              82
     C     DUKY2A        CHAIN     PDU02                              82
     C     *IN82         IFEQ      '0'
     C     DUKY2A        READE     PDU02                                  83
     C     *IN83         IFEQ      '1'
     C     DUKY2A        DELETE    PDU02                              82
     C                   ELSE
     C                   EXCEPT    RDU02
     C                   END
     C                   END
     C                   ELSE
     C     DUKY          DELETE    PDU01                              82
     C     DUKY1         CHAIN     PDU01                              82
     C     *IN82         IFEQ      '0'
     C     DUKY1         READE     PDU01                                  83
     C     *IN83         IFEQ      '1'
     C     DUKY1         DELETE    PDU01                              82
     C                   ELSE
     C                   EXCEPT    RDU01
     C                   END
     C                   END
     C                   END
     C*** END ADD *** 06/11/92 ****************************************
     C                   DELETE    PCA01
     C     CAKEY         READE     PCA01                                  80
     C                   END                                                    ENDDOW *IN80
     C*
     C                   ELSE
     C*
     C*** BEGIN ADD *** TAPE SUBMISSION ******************************
     C*  IF 'N', MAKE SURE THAT THERE ARE DIAG CODES IN EOB RECORD
     C*
     C     EODIA1        IFEQ      *BLANK
     C     EODIA2        ANDEQ     *BLANK
     C     EODIA3        ANDEQ     *BLANK
     C     EODIA4        ANDEQ     *BLANK
     C                   MOVE      '1'           *IN30                          NEED DC CODES
     C                   GOTO      STRT
     C                   END                                                    ENDEQ *BLANK
     C*
     C*  IF 'N', MAKE SURE THAT THERE ARE NOT CURRENT CHARGES
     C*  ALREADY IN FILE (I.E. CHECK FADCP RECORD BY PROV/MOD
     C*  FOR DCSTAT = BLANK)
     C*
     C     DCKEY1        CHAIN     PDC01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*** SINCE PDC01 RECORDS ARE READ IN AS UPDATE, MUST NOW RELEASE THE
     C*** RECORD WE JUST READ TO PREVENT REC LOCKS...
     C*
     C                   EXCEPT    EDC01
     C*
     C     DCSTAT        IFEQ      ' '
     C                   MOVE      '1'           *IN31                          CURR CHRGS EXIST
     C                   GOTO      STRT
     C                   END
     C     DCKEY1        READE     PDC01                                  60
     C                   END                                                    DOW *IN60
     C*
     C                   MOVE      99999         XXDCS#
     C     DCKEY         SETGT     PDC01
     C                   READP     PDC01                                  82
     C*
     C*** SINCE PDC01 RECORDS ARE READ IN AS UPDATE, MUST NOW RELEASE THE
     C*** RECORD WE JUST READ TO RETRIEVE THE SEQ # FROM.(PREVENT REC LOCKS)
     C*
     C  N82              EXCEPT    EDC01
     C*
     C     *IN82         IFEQ      '0'
     C     EOCO#         ANDEQ     DCCO#
     C*** BEGIN ADD **8 07/21/92 **************************************
     C     EOSUB#        ANDEQ     DCSUB#
     C*** END ADD *** 07/21/92 ****************************************
     C     EOFND         ANDEQ     DCFND
     C     EOFYR         ANDEQ     DCFYR
     C     EOCASE        ANDEQ     DCCASE
     C     EOFEIN        ANDEQ     DCFEIN
     C     EOMOD         ANDEQ     DCMOD
     C                   ADD       1             DCDCS#
     C*
     C                   ELSE
     C                   Z-ADD     1             DCDCS#
     C                   END
     C*
     C*  UPDATE DETAIL CHARGE RECORDS WITH DIAGNOSIS CODE SEQUENCE #
     C*  AND ZERO OUT EOMB FIELDS:
     C*
     C**********           MOVE *BLANKS   XXTRCD  2        DEL 10/14/98
     C                   Z-ADD     *ZEROS        DCFRDT
     C                   Z-ADD     *ZEROS        DCTODT
     C     CAKEY         CHAIN     PCA01                              80
     C     *IN80         DOWEQ     '0'
     C*** BEGIN ADD *** 06/11/92 **************************************
     C     CAPRTY        IFEQ      '4'
     C     DUKY2         DELETE    PDU02                              82
     C     DUKY2A        CHAIN     PDU02                              82
     C     *IN82         IFEQ      '0'
     C     DUKY2A        READE     PDU02                                  83
     C     *IN83         IFEQ      '1'
     C     DUKY2A        DELETE    PDU02                              82
     C                   ELSE
     C                   EXCEPT    RDU02
     C                   END
     C                   END
     C                   ELSE
     C     DUKY          DELETE    PDU01                              82
     C     DUKY1         CHAIN     PDU01                              82
     C     *IN82         IFEQ      '0'
     C     DUKY1         READE     PDU01                                  83
     C     *IN83         IFEQ      '1'
     C     DUKY1         DELETE    PDU01                              82
     C                   ELSE
     C                   EXCEPT    RDU01
     C                   END
     C                   END
     C                   END
     C*** END ADD *** 06/11/92 ****************************************
     C*
     C*** BEGIN ADD *** 02/24/93 **************************************
     C     CAPRTY        IFEQ      '1'
     C                   MOVE      'N'           DCPRTY
     C                   ELSE
     C                   MOVE      'H'           DCPRTY
     C                   END
     C*** END ADD *** 02/24/93 ****************************************
     C                   MOVE      DCDCS#        CADCS#
     C                   Z-ADD     *ZERO         CACHK#
     C                   Z-ADD     *ZERO         CAEOB#
     C                   MOVE      Q1USER        CAUSER
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   Z-ADD     DSTIME        CATIME
     C                   UPDATE    PCA01
     C     CAKEY         READE     PCA01                                  80
     C                   END                                                    END DOW
     C*
     C*  CREATE DIAGNOSIS CODES RECORD
     C*
     C                   MOVE      EOCO#         DCCO#
     C*** BEGIN ADD *** 07/21/92 **************************************
     C                   MOVE      EOSUB#        DCSUB#
     C*** END ADD *** 07/21/92 ****************************************
     C                   MOVE      EOFND         DCFND
     C                   MOVE      EOFYR         DCFYR
     C                   MOVE      EOCASE        DCCASE
     C                   MOVE      EOFEIN        DCFEIN
     C                   MOVE      EOMOD         DCMOD
     C                   MOVE      EODIA1        DCDIA1
     C                   MOVE      EODIA2        DCDIA2
     C                   MOVE      EODIA3        DCDIA3
     C                   MOVE      EODIA4        DCDIA4
     C*
     C* DETERMINE FROM/TO SERVICES DATES FOR FADCP RECORD
     C     CAFDAT        IFLT      DCFRDT
     C     DCFRDT        OREQ      *ZEROS
     C                   Z-ADD     CAFDAT        DCFRDT
     C                   END
     C     CAFDAT        IFGT      DCTODT
     C     DCTODT        OREQ      *ZEROS
     C                   Z-ADD     CAFDAT        DCTODT
     C                   END
     C*
     C*** BEGIN ADD *** 02/01/93 **************************************
     C                   MOVE      EOAST         DCAST
     C*** END ADD *** 02/01/93 ****************************************
     C                   MOVE      EOUSER        DCIUSR
     C                   MOVE      EODT          DCIDT
     C                   MOVE      *BLANKS       DCSTAT
     C*** BEGIN ADD *** 11/10/92 **************************************
     C                   MOVE      EOSYS         DCSYS
     C                   MOVE      *BLANKS       DCCERT
     C                   Z-ADD     *ZEROS        DCEXT
     C*** END ADD *** 11/10/92 ****************************************
     C                   MOVE      EOZIPA        DCZIPA                         ADD 02/24/93
     C*** BEGIN ADD *** 04/19/93 **************************************
     C                   MOVE      EOFLG6        DCBLTY
     C                   MOVE      *BLANKS       DCFL01
     C                   MOVE      *BLANKS       DCFL02
     C                   MOVE      *BLANKS       DCFL03
     C                   MOVE      *BLANKS       DCFL04
     C                   MOVE      *BLANKS       DCFL05
     C                   MOVE      *BLANKS       DCFL06
     C                   MOVE      *BLANKS       DCFL07
     C                   MOVE      *BLANKS       DCFL08
     C                   MOVE      *BLANKS       DCFL09
     C                   MOVE      *BLANKS       DCFL10
     C*** BEGIN CHG *** 05/25/94 ***********************************************
     C*** BEGIN ADD *** 03/18/94 *************************************
     C     E1KEY         CHAIN     PE101                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             T                 2 0
     C********** 5         SUBSTE120A1:T  DC10A1       65
     C     5             SUBST     E120A1:T      XCNWCD                 65
     C                   Z-ADD     6             T                 2 0
     C     3             SUBST     E120A1:T      XCTYPE                 65
     C*** BEGIN ADD *** 08/01/95 **************************************
     C                   Z-ADD     18            T                 2 0
     C     2             SUBST     E120A1:T      XCDIHR                 65
     C*** END ADD ***** 08/01/95 **************************************
     C                   MOVE      XC10A1        DC10A1
     C*** BEGIN ADD *** 08/01/95 **************************************
     C                   Z-ADD     1             T                 2 0
     C     1             SUBST     E120A1:T      XCNMGR                 65
     C                   Z-ADD     2             T                 2 0
     C     5             SUBST     E120A1:T      XCNMGP                 65
     C                   Z-ADD     15            T                 2 0
     C     3             SUBST     E120A1:T      XCTYBL                 65
     C                   Z-ADD     20            T                 2 0
     C     1             SUBST     E120A1:T      XCPCMT                 65
     C                   MOVE      XC10A2        DC10A2
     C*
     C*** BEG CHG *** 03/18/97 ***
     C**********           Z-ADD7         T       20
     C********** 3         SUBSTE120A1:T  XCHTSV       65
     C**********           MOVE *BLANKS   XCFILL
     C                   CLEAR                   XC10A3
     C     2             SUBST     E120A2:7      XCHTSV
     C     1             SUBST     E120A2:9      XSCTOB
     C*** END CHG *** 03/18/97 ***
     C                   MOVE      XC10A3        DC10A3
     C*** END ADD ***** 08/01/95 **************************************
     C*** BEGIN ADD  ***  06/15/95  **********************************
     C                   MOVE      E17A01        DC7A01
     C                   MOVE      E17A02        DC7A02
     C                   MOVE      E17A03        DC7A03
     C                   MOVE      E17A04        DC7A04
     C                   Z-ADD     E1DT6         DCDT06
     C                   Z-ADD     E1DT7         DCDT07
     C                   Z-ADD     E1DT8         DCDT08
     C                   Z-ADD     E1DT9         DCDT09
     C                   Z-ADD     E1DT10        DCDT10
     C                   Z-ADD     E1DT11        DCDT11
     C                   Z-ADD     E1DT12        DCDT12
     C                   Z-ADD     E1DT13        DCDT13
     C                   Z-ADD     E1DT14        DCDT14
     C                   Z-ADD     E1DT15        DCDT15
     C*** END   ADD  ***  06/15/95  **********************************
     C*** BEG ADD *** 03/18/97 ***
     C                   EXSR      DCE1IN
     C*** END ADD *** 03/18/97 ***
     C                   ELSE                                                   ELSE *IN60 EQ '0'
     C*** BEG ADD *** 03/18/97 ***
     C                   EXSR      DCINIT
     C*** END ADD *** 03/18/97 ***
     C                   MOVE      *BLANKS       DC10A1
     C                   MOVE      *BLANKS       DC10A2                         ADD 08/01/95
     C                   MOVE      *BLANKS       DC10A3                         ADD 08/01/95
     C*** BEGIN ADD  ***  06/15/95  **********************************
     C                   MOVE      *BLANKS       DC7A01
     C                   MOVE      *BLANKS       DC7A02
     C                   MOVE      *BLANKS       DC7A03
     C                   MOVE      *BLANKS       DC7A04
     C                   Z-ADD     *ZEROS        DCDT06
     C                   Z-ADD     *ZEROS        DCDT07
     C                   Z-ADD     *ZEROS        DCDT08
     C                   Z-ADD     *ZEROS        DCDT09
     C                   Z-ADD     *ZEROS        DCDT10
     C                   Z-ADD     *ZEROS        DCDT11
     C                   Z-ADD     *ZEROS        DCDT12
     C                   Z-ADD     *ZEROS        DCDT13
     C                   Z-ADD     *ZEROS        DCDT14
     C                   Z-ADD     *ZEROS        DCDT15
     C*** END ADD ******  06/15/95  **********************************
     C                   END                                                    END *IN60 EQ '0'
     C*** END ADD *** 03/18/94 ***************************************
     C*** END CHG *** 05/25/94 *************************************************
     C*** BEG DEL *** 03/18/94 ***
     C****************     MOVE *BLANKS   DC10A2           DEL 08/01/95
     C****************     MOVE *BLANKS   DC10A3           DEL 08/01/95
     C****************     MOVE *BLANKS   DC10A4
     C*** END DEL *** 03/18/94 ****
     C                   MOVE      *BLANKS       DC10A5
     C*** END ADD *** 04/19/93 ****************************************
     C*** BEGIN ADD *** 08/21/97 **************************************
     C*** SAVE EOB# IN PREV EOB# FIELD
     C     SCDELF        IFEQ      'N'
     C                   MOVE      SCEOB#        DCPEOB
     C                   ELSE
     C                   MOVE      *ZEROS        DCPEOB
     C                   END
     C*** END   ADD *** 08/21/97 **************************************
     C                   MOVE      Q1USER        DCUSER
     C                   MOVE      Q1PGM         DCPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DCDT
     C                   Z-ADD     DSTIME        DCTIME
     C*** BEGIN ADD *** 06/24/92 **************************************
     C                   Z-ADD     *ZEROS        DCLTR#
     C*** END ADD *** 06/24/92 ****************************************
     C*
     C                   WRITE     PDC01
     C*** END ADD ***** TAPE SUBMISSION ******************************
     C*
     C                   END                                                     END IF 'Y'
     C*
     C*  WRITE RECORD TO EOB DELETE FILE FOR EOB RECORD
     C*
     C     SCDELF        IFEQ      'Y'
     C                   MOVE      'Y'           EOFLG8
     C                   END
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     Q1USER        EOIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOIDT
     C                   WRITE     PDE01
     C*** BEGIN ADD *** 11/10/92 *************************************
     C*  IF THERE IS AN ASSOCIATED NETWORK TRANSACTION RECORD -
     C*  WRITE A RECORD TO THE EOB DELETE FILE FOR IT
     C*  THEN DELETE ANY ASSOCIATED NETWORK TRANSACTION RECORD
     C*
     C     TRKEY         CHAIN     PTR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      'N'           EOSYS
     C                   Z-ADD     0             EOAMT
     C                   Z-ADD     0             EOAMTC
     C                   Z-ADD     0             EO#BLL
     C                   Z-ADD     0             EOOTHR
     C                   Z-ADD     0             EONDIS
     C                   Z-ADD     TRBAMT        EOBAMT
     C                   WRITE     PDE01
     C                   DELETE    PTR01
     C                   END
     C*** END ADD ***** 11/10/92 *************************************
     C*** BEGIN ADD *** 01/12/93 **************************************
     C     EOFLG5        IFEQ      'Y'
     C     TMKEY         CHAIN     PTM01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PTM01
     C     TMKEY         READE     PTM01                                  60
     C                   END
     C                   END
     C*** END ADD *** 01/12/93 ****************************************
     C*** BEGIN ADD *** 03/26/93 **************************************
     C     E1KEY         CHAIN     PE101                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     Q1USER        E1USER
     C                   MOVEL     Q1PGM         E1PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E1DT
     C                   Z-ADD     DSTIME        E1TIME
     C                   MOVEL     Q1USER        E1IUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E1IDT
     C                   WRITE     PE102
     C                   DELETE    PE101
     C                   END
     C*** END ADD *** 03/26/93 ****************************************
     C*
     C*  DELETE EOB RECORD FROM EOB FILE
     C     EOKEY         DELETE    PEO01                              90
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   SUB       EOAMT         CMAR01
     C                   UPDATE    PCM01
     C                   END                                                    END 0 *IN60
     C*
     C*** BEGIN ADD *** 03/16/95 ***********************************************
     C*
     C* ONLY ISSUE CREDIT MEMO IF BILL REVIEW CHECK HAS BEEN PROCESSED
     C*
     C     E110A2        IFNE      *BLANKS
     C*** END ADD *** 03/16/95 *************************************************
     C*** BEGIN ADD *** 01/25/95 ***********************************************
     C     EOBAMT        IFGT      *ZEROS
     C*
     C                   MOVE      *HIVAL        X1SEQ#
     C     ERKEY         SETGT     PER02
     C                   READP     PER02                                  60
     C     *IN60         IFEQ      '0'
     C     ERCO#         ANDEQ     XXCO#
     C     ERSUB#        ANDEQ     XXSUB#
     C     ERFND         ANDEQ     XXFND
     C     ERFYR         ANDEQ     EOFYR
     C     ERCASE        ANDEQ     SCCASE
     C     EROEOB        ANDEQ     SCEOB#
     C     ERSEQ#        ADD       1             XXSEQ#
     C                   ELSE
     C                   Z-ADD     1             XXSEQ#
     C                   END
     C*
     C                   Z-ADD     XXCO#         ERCO#
     C                   Z-ADD     XXSUB#        ERSUB#
     C                   Z-ADD     XXFND         ERFND
     C                   Z-ADD     EOFYR         ERFYR
     C                   Z-ADD     SCCASE        ERCASE
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       scSS# = UnMaskData('SSN':'SCSS#':SCSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   MOVE      SCSS#         ERSS#
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       scSS# = MaskData('SSN':'SCSS#':SCSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ERACDT
     C                   MOVE      SCEOB#        ERREOB
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ERTRDT
     C                   MOVE      SCEOB#        EROEOB
     C                   Z-ADD     XXSEQ#        ERSEQ#
     C                   Z-ADD     *ZEROS        EROINV
     C                   Z-ADD     *ZEROS        ERRECA
     C                   Z-ADD     EOBAMT        ERREFA
     C                   MOVE      EOSYS         ERSYS
     C*
     C                   MOVE      *BLANKS       ERFL01
     C                   MOVE      'Y'           ERFL02
     C                   MOVE      *BLANKS       ERFL03
     C                   MOVE      *BLANKS       ERFL04
     C                   MOVE      *BLANKS       ERFL05
     C                   MOVE      *BLANKS       ERFL06
     C                   MOVE      *BLANKS       ERFL07
     C                   MOVE      *BLANKS       ERFL08
     C                   MOVE      *BLANKS       ERFL09
     C                   MOVE      *BLANKS       ERFL10
     C                   Z-ADD     *ZEROS        ER92N1
     C                   Z-ADD     *ZEROS        ER92N2
     C                   Z-ADD     *ZEROS        ER92N3
     C                   Z-ADD     *ZEROS        ER92N4
     C                   Z-ADD     *ZEROS        ER92N5
     C                   MOVE      *BLANKS       ER10A1
     C                   MOVE      *BLANKS       ER10A2
     C                   MOVE      *BLANKS       ER10A3
     C                   MOVEL     Q1USER        ERUSER
     C                   MOVEL     Q1PGM         ERPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ERDT
     C                   Z-ADD     DSTIME        ERTIME
     C                   MOVEL     Q1USER        ERIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ERIDT
     C                   WRITE     PER01
     C*
     C*** BEGIN ADD *** 04/18/95 ***********************************************
     C*
     C* GET PAYMENT VALUE 1 AND 2 FROM THE OLD CHECK THAT WAS CREATED
     C*
     C                   MOVE      *BLANKS       XXBR2
     C                   MOVE      *BLANKS       XXBR7
     C     E110A2        IFNE      *BLANKS
     C                   MOVEL     E110A2        XXCHK#
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PABR2         XXBR2
     C                   MOVE      PABR7         XXBR7
     C                   ELSE
     C                   MOVE      *BLANKS       XXBR2
     C                   MOVE      *BLANKS       XXBR7
     C                   END
     C                   END
     C*
     C* BECAUSE THE CHECK HAS ALREADY BEEN ISSUED WE MUST ISSUE A MEDICAL REFUND
     C* GET THE CORRECT SEQUENCE NUMBER FOR THE REFUND ADJUSTEMENT
     C*
     C                   Z-ADD     99999         XXSEQ#
     C     PAKEY1        SETGT     PPA02
     C                   READP     PPA02                                  60
     C  N60              EXCEPT    PPA02E
     C  N60XXCO#         COMP      PACO#                              6060
     C  N60XXWCF#        COMP      PAFND                              6060
     C  N60SCFYR         COMP      PAFYR                              6060
     C  N60SCCASE        COMP      PACASE                             6060
     C   60              Z-ADD     *ZERO         PASEQ#
     C                   ADD       1             PASEQ#
     C*
     C                   Z-ADD     XXCO#         PACO#
     C                   Z-ADD     XXWCF#        PAFND
     C                   Z-ADD     EOFYR         PAFYR
     C                   Z-ADD     SCCASE        PACASE
     C                   MOVEL     E110A2        PACHK#
     C                   Z-ADD     *ZEROS        PABASQ
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATRDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAPODT
     C                   Z-ADD     *ZEROS        PAWKS
     C                   Z-ADD     *ZEROS        PADAYS
     C*** BEGIN DEL 05/21/96 ***********************************
     C******************** Z-ADDEOFEIN    PAFEIN
     C******************** Z-ADDEOMOD     PAMOD
     C*** END   DEL 05/21/96 ***********************************
     C*** BEGIN ADD 05/21/96 ***********************************
     C                   Z-ADD     XXFEIN        PAFEIN
     C                   Z-ADD     XXMOD         PAMOD
     C*** END   ADD 05/21/96 ***********************************
     C                   Z-ADD     EOBAMT        PAAMT
     C                   MOVE      XXBR2         PABR2
     C                   MOVE      XXBR7         PABR7
     C                   Z-ADD     EOFRDT        PAFRDT
     C                   Z-ADD     EOTODT        PATODT
     C                   MOVE      'UR'          PATRCD
     C                   MOVE      'R'           PATYPE
     C                   MOVE      'ME'          PARECD
     C                   MOVE      'M'           PARETP
     C                   MOVE      *BLANKS       PADESC
     C                   MOVEL     DESC          PADESC
     C*
     C                   MOVE      *BLANKS       PAADST
     C                   MOVE      *BLANKS       PASTAT
     C                   MOVEL     Q1USER        PAUSER
     C                   MOVEL     Q1PGM         PAPGM
     C                   Z-ADD     DSTIME        PATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADT
     C                   Z-ADD     *ZEROS        PABUL1
     C                   MOVE      *BLANKS       PAPGTP
     C                   Z-ADD     SCEOB#        PAEOB#
     C                   WRITE     PPA02
     C*
     C* THE RESERVES FOR THE CLAIM SHOULD BE UPDATED
     C*
     C     CMKY1         CHAIN     PCM02                              60
     C     *IN60         IFEQ      '0'
     C*
     C* FIND THE MEDICAL RESERVE IN THE CLAIM FILE
     C*
     C                   Z-ADD     1             X1
     C     'ME'          LOOKUP    CD(X1)                                 70
     C     *IN70         IFEQ      '1'
     C*
     C* ADD THE AMOUNT TO BE REFUNDED TO THE REMAINING RESERVES
     C*
     C     RR(X1)        ADD       EOBAMT        RR(X1)
     C*
     C* SUB THE AMOUNT TO BE REFUNDED TO THE PAYMENTS AGAINST RESERVES
     C*
     C     PA(X1)        SUB       EOBAMT        PA(X1)
     C*
     C                   MOVEL     Q1PGM         CMPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C                   MOVEL     Q1USER        CMUSER
     C*
     C                   UPDATE    PCM02
     C*
     C                   END                                                    END *IN70 EQ '1'
     C*
     C                   END                                                    END *IN60 EQ '0'
     C*** END ADD *** 04/18/95 *************************************************
     C*
     C                   END                                                    END EOBAMT GT *ZEROS
     C*
     C*** BEGIN ADD *** 03/16/95 ***********************************************
     C                   END                                                    END E110A2 NE *BLANK
     C*** END ADD *** 03/16/95 *************************************************
     C*** END ADD *** 01/25/95 *************************************************
     C*
     C*** BEGIN ADD *** 05/20/97 ***********************************************
     C*  CALL THE 837 LOG API TO DELETE THE LOG RECORDS ASSOCIATED WITH
     C*    THE DELETED EOMB.
     C*** BEG ADD *** 11/07/97 ***
     C*** EDI INSTALLED ?
     C     ##EDI         IFEQ      'Y'
     C*** END ADD *** 11/07/97 ***
     C*
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      XXSUB#        QQSUB#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      SCFYR         QXFYR
     C                   MOVE      SCCASE        QXCASE
     C                   MOVE      SCEOB#        QQEOB#
     C                   CALL      'F0070R6'
     C                   PARM                    QQCO#             3            COMPANY
     C                   PARM                    QQSUB#                         SUBSCRIBER
     C                   PARM                    QQFND             3            FUND ID
     C                   PARM                    QXFYR             3            FUND YR
     C                   PARM                    QXCASE            7            CASE/CLAIM
     C                   PARM                    QQEOB#                         EOB #
     C                   PARM      'D'           QQACTF            1            ACTION FLAG
     C                   PARM      *BLANKS       QQMGID            7            MSG ID
     C                   PARM      ' '           QQRTCD            1
     C*** END ADD ***** 05/20/97 ***********************************************
     C*** BEGIN ADD *** 08/21/97 **************************************
     C*** IF EDI LOG STATUS IS 'EX' OR 'TR', DELETE 837 WORKFILE
     C*
     C     XXDLOG        IFGT      *ZEROS
     C                   MOVE      *BLANKS       Q1ACLC
     C                   MOVE      XXCO#         Q1CO#
     C                   MOVE      XXSUB#        Q1SUB#
     C                   MOVE      XXFND         Q1FND
     C                   Z-ADD     SCFYR         Q1FYR
     C                   MOVE      SCCASE        Q1CASE
     C                   MOVE      '0'           *IN82
     C*
     C     *IN82         DOWEQ     '0'
     C     Q1ACLC        DELETE    PT101                              82
     C                   ENDDO
     C                   MOVE      '0'           *IN82
     C     *IN82         DOWEQ     '0'
     C     Q1ACLC        DELETE    PT201                              82
     C                   ENDDO
     C                   ENDIF                                                  XXDLOG
     C                   ENDIF                                                  ADD 11/21/97
     C*** END   ADD *** 08/21/97 **************************************
     C*
     C     *IN35         IFEQ      '0'                                          ADD 06/25/97
     C                   Z-ADD     *ZERO         SCEOB#
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ELSE                                                   ADD 06/25/97
     C                   MOVE      '*'           @@RTN                          ADD 06/25/97
     C                   ENDIF                                                  ADD 06/25/97
     C*****************************************************************
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*** BEG ADD *** 03/18/97
     C*******************************************************************
     C*  LOAD FADCP WITH THE NEW FILE FIELDS                           IL.
     C*******************************************************************
     C     DCE1IN        BEGSR
      * THE FOLLOWING ARE NEW FILE FIELDS
     C*** HOSPITAL DAILY ACCOMODATION RATE: FROM PACKED TO ZONED
     C*** TO CHARACTER
     C                   MOVE      *ZEROS        DC10A4                         E1AMT4
     C                   MOVE      X1AMT4        DC10A4                         E1AMT4
      ***
     C                   MOVE      E1ADSC        DCADSC
     C                   MOVE      E1ADTY        DCADTY
     C                   MOVE      E1ADDI        DCADDI
     C                   MOVE      E1ADLT        DCADLT
     C                   MOVE      E1ADHR        DCADHR
     C                   MOVE      E1DYCV        DCDYCV
     C                   MOVE      E1NOCV        DCNOCV
     C                   MOVE      E1OCD1        DCOCD1
     C                   MOVE      E1ODT1        DCODT1
     C                   MOVE      E1OCD2        DCOCD2
     C                   MOVE      E1ODT2        DCODT2
     C                   MOVE      E1OCD3        DCOCD3
     C                   MOVE      E1ODT3        DCODT3
     C                   MOVE      E1OCD4        DCOCD4
     C                   MOVE      E1ODT4        DCODT4
     C                   MOVE      E1OSCD        DCOSCD
     C                   MOVE      E1OSFD        DCOSFD
     C                   MOVE      E1OSTD        DCOSTD
     C                   MOVE      E1PATS        DCPATS
     C                   MOVE      E1VC01        DCVC01
     C                   MOVE      E1VA01        DCVA01
     C                   CLEAR                   DCVC02
     C                   CLEAR                   DCVA02
     C                   CLEAR                   DCVC03
     C                   CLEAR                   DCVA03
     C                   CLEAR                   DCVC04
     C                   CLEAR                   DCVA04
     C                   CLEAR                   DCVC05
     C                   CLEAR                   DCVA05
     C                   CLEAR                   DCVC06
     C                   CLEAR                   DCVA06
     C                   CLEAR                   DCVC07
     C                   CLEAR                   DCVA07
     C                   CLEAR                   DCVC08
     C                   CLEAR                   DCVA08
     C                   CLEAR                   DCVC09
     C                   CLEAR                   DCVA09
     C                   CLEAR                   DCVC10
     C                   CLEAR                   DCVA10
     C                   CLEAR                   DCVC11
     C                   CLEAR                   DCVA11
     C                   CLEAR                   DCVC12
     C                   CLEAR                   DCVA12
     C                   CLEAR                   DCSPCD
     C                   MOVE      E1CRDT        DCCRDT
     C                   MOVE      E1IWRI        DCIWRI
     C                   MOVE      E1ACCT        DCACCT
     C                   MOVE      E1PHTY        DCPHTY
     C                   MOVE      E1SFEI        DCSFEI
     C                   MOVE      E1SMOD        DCSMOD
     C                   MOVE      E1BFEI        DCBFEI
     C                   MOVE      E1BMOD        DCBMOD
     C                   MOVE      E1TFEI        DCTFEI
     C                   MOVE      E1TMOD        DCTMOD
     C                   MOVE      E1OFEI        DCOFEI
     C                   MOVE      E1OMOD        DCOMOD
     C                   CLEAR                   DCCOC1
     C                   CLEAR                   DCCOC2
     C                   CLEAR                   DCCOC3
     C                   CLEAR                   DCCOC4
     C                   CLEAR                   DCCOC5
     C                   CLEAR                   DCCOC6
     C                   CLEAR                   DCCOC7
     C                   MOVE      E1MRC#        DCMRC#
     C                   CLEAR                   DCOAMT
     C                   MOVE      E1DIA5        DCDIA5
     C                   MOVE      E1DIA6        DCDIA6
     C                   MOVE      E1DIA7        DCDIA7
     C                   MOVE      E1DIA8        DCDIA8
     C                   MOVE      E1DIA9        DCDIA9
     C                   MOVE      E17A06        DC7A06
     C                   MOVE      E17A05        DC7A05
     C                   MOVE      E1DT16        DCDT16
     C                   MOVE      E1DT17        DCDT17
     C                   MOVE      E1DT18        DCDT18
     C                   MOVE      E1PMRI        DCPMRI
     C                   CLEAR                   DCTOTN
     C                   MOVE      EOEMPL        DCEMPL                         EO
     C                   MOVE      EODFC         DCDFC                          EO
     C                   MOVE      EOFLIC        DCLIC#                         EO
     C                   MOVE      E1AMOD        DCAMOD
      *
     C                   ENDSR
     C*******************************************************************
     C*  CLEAR NEW FILE FIELDS                                         IL.
     C*******************************************************************
     C     DCINIT        BEGSR
      * THE FOLLOWING ARE NEW FILE FIELDS
     C                   CLEAR                   DCADSC
     C                   CLEAR                   DCADTY
     C                   CLEAR                   DCADDI
     C                   CLEAR                   DCADLT
     C                   CLEAR                   DCADHR
     C                   CLEAR                   DCDYCV
     C                   CLEAR                   DCNOCV
     C                   CLEAR                   DCOCD1
     C                   CLEAR                   DCODT1
     C                   CLEAR                   DCOCD2
     C                   CLEAR                   DCODT2
     C                   CLEAR                   DCOCD3
     C                   CLEAR                   DCODT3
     C                   CLEAR                   DCOCD4
     C                   CLEAR                   DCODT4
     C                   CLEAR                   DCOSCD
     C                   CLEAR                   DCOSFD
     C                   CLEAR                   DCOSTD
     C                   CLEAR                   DCPATS
     C                   CLEAR                   DCVC01
     C                   CLEAR                   DCVA01
     C                   CLEAR                   DCVC02
     C                   CLEAR                   DCVA02
     C                   CLEAR                   DCVC03
     C                   CLEAR                   DCVA03
     C                   CLEAR                   DCVC04
     C                   CLEAR                   DCVA04
     C                   CLEAR                   DCVC05
     C                   CLEAR                   DCVA05
     C                   CLEAR                   DCVC06
     C                   CLEAR                   DCVA06
     C                   CLEAR                   DCVC07
     C                   CLEAR                   DCVA07
     C                   CLEAR                   DCVC08
     C                   CLEAR                   DCVA08
     C                   CLEAR                   DCVC09
     C                   CLEAR                   DCVA09
     C                   CLEAR                   DCVC10
     C                   CLEAR                   DCVA10
     C                   CLEAR                   DCVC11
     C                   CLEAR                   DCVA11
     C                   CLEAR                   DCVC12
     C                   CLEAR                   DCVA12
     C                   CLEAR                   DCSPCD
     C                   CLEAR                   DCCRDT
     C                   CLEAR                   DCIWRI
     C                   CLEAR                   DCACCT
     C                   CLEAR                   DCPHTY
     C                   CLEAR                   DCSFEI
     C                   CLEAR                   DCSMOD
     C                   CLEAR                   DCBFEI
     C                   CLEAR                   DCBMOD
     C                   CLEAR                   DCTFEI
     C                   CLEAR                   DCTMOD
     C                   CLEAR                   DCOFEI
     C                   CLEAR                   DCOMOD
     C                   CLEAR                   DCCOC1
     C                   CLEAR                   DCCOC2
     C                   CLEAR                   DCCOC3
     C                   CLEAR                   DCCOC4
     C                   CLEAR                   DCCOC5
     C                   CLEAR                   DCCOC6
     C                   CLEAR                   DCCOC7
     C                   CLEAR                   DCMRC#
     C                   CLEAR                   DCOAMT
     C                   CLEAR                   DCDIA5
     C                   CLEAR                   DCDIA6
     C                   CLEAR                   DCDIA7
     C                   CLEAR                   DCDIA8
     C                   CLEAR                   DCDIA9
     C                   CLEAR                   DC7A06
     C                   CLEAR                   DCDT16
     C                   CLEAR                   DCDT17
     C                   CLEAR                   DCDT18
     C                   CLEAR                   DCPMRI
     C                   CLEAR                   DCTOTN
     C                   CLEAR                   DCAMOD
      *
     C                   ENDSR
     C*
     O*** ADD NEXT LINE *** TAPE SUBMISSION ***************************
     OPDC01     E            EDC01
     OPCM01     E            ECM01
     OPDU01     E            RDU01
     OPDU02     E            RDU02
     OPE101     E            RE101
     OPPA02     E            PPA02E
      *====================================================================*
