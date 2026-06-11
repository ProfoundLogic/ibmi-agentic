     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H/TITLE SRMIL  -GENERATE INCREASED LIMITS MINIMUM BASED ON ALL DIV
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                       *
     H**                                                              *
     H**      THIS PROGRAM IS CALLED BY VARIOUS BILLING PROGRAMS IN   *
     H**      PREPARATION OF GENERATING INC LMT OVER ALL DIVS.        *
     H**                                                              *
     H**                                                              *
     H* CALLED FROM:                                                  *
     H*   W8002C  - ORIGINAL BILL SUBMISSION                          *
     H*   W7710C  - FINAL AUDIT BILL SUBMISSION                       *
     H* CALLS TO:                                                     *
     H*                                                               *
     H*                                                               *
     H* PROGRAMMER: JOHN HOLSOMBACH                                   *
     H* CREATED:    07/14/98            PROJECT: WC705                *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H* 61  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/01/25 C:65  Support: 119398   (Sunz)                       *
     H*                If running a mass audit check WABLP beforehand *
     H*                and create WABLP records for any summarized    *
     H*                policies that don't have a record for the      *
     H*                state reporting division.                      *
     H*                                                               *
     H* 08/08/18 C:62  Project: 150WCS                                *
     H*                Add All division processing.                   *
     H*                                                               *
     H* 07/29/15 C:65  Support: Phase#36318 (CareWest)                *
     H*                Update activiation group from QILE to *CALLER  *
     H*                as this program was not picking up file        *
     H*                overrides from the revised bill.               *
     H*                                                               *
     H* 03/27/11 C:255 SUPPORT 71062  WA099                           *
     H*                ADDED CHANGE TO POPULATE BLRFLG(WABLP) WITH    *
     H*                NW/RNW FLAG VALUE.                             *
     H*                                                               *
     H* 10/11/07 C:183 PROJECT: 146WBC                                *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP.                                *
     H*                                                               *
     H*  02/15/06 C:136 SUPPORT: WA129 50227                          *
     H*                 SAVE OFF BLFLG3 VALUE AND RESTORE IT AFTER    *
     H*                 THE CALL TO W8010R SO THAT THE PRINT BILL     *
     H*                 PROGRAM CAN DELETE THE WABLP RECORD PROPERLY  *
     H*                 IF MASS BILLING IS CALLED TO CALCULATE AND    *
     H*                 UPDATE ONLY.                                  *
     H*                                                               *
     H*  06/20/05 C:65 PROJECT: WC1327                                *
     H*                ALLOW PROGRAM TO BE CALLED IN SINGLE DIVISION  *
     H*                MODE.  THIS MODE WILL ALLOW A BILL TO BE RUN   *
     H*                ON A SINGLE DIVISION ON A POLICY.              *
     H*                                                               *
     H* 09/03/03 C:65 PROJECT: SUPPORT#38838                          *
     H*               SWITCH LOGICAL BEING USED TO RUN THROUGH FILE   *
     H*               IN ORDER TO PREVENT A MULTI DIVISION POLICY     *
     H*               FROM RUNNING EVERY DIVISION AFTER EACH FYR      *
     H*               WHEN MORE THAN ONE FYR IS PRESENT IN WABLP.     *
     H*                                                               *
     H*  03/03/03 C:65 PROJECT: SUPP#36250                            *
     H*                CHANGE THE SIZE OF DATE FIELD TO MATCH CALLS.  *
     H*                                                               *
     H*  03/08/02 C:65 PROJECT: WC1030                                *
     H*                IF THE EMPLOYER HAS A PREVIOUS EMPLR# THEN     *
     H*                CONSIDER THE POLICY AS A RENEWAL.              *
     H*                                                               *
     H* 12/07/00 C:65 PROJECT: SUPPORT#25300                          *
     H*               PROCESS A EMP#/FYR CHECK INSTEAD OF DOING A     *
     H*               SETGT IN ORDER TO AVOID SKIPPING RECORDS IN     *
     H*               WABLP.                                          *
     H*                                                               *
     H*****************************************************************
     F*** BEG   DEL ***  03/27/11  ************************************
     F******WDELL   IF  E           K        DISK
     F******      PEL01                             KRENAMEPEL03
     F*** END   DEL ***  03/27/11  ************************************
     FWDELL1    IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWDILL     UF   E           K DISK
     F*** BEGIN CHG ***  09/03/03  ************************************
     F***WABLL   UF  E           K        DISK                      A
     F*********   PBL01                             KRENAMEPBL03
     F***WABLL1  IF  E           K        DISK
     FWABLL     IF   E           K DISK
     FWABLL1    UF A E           K DISK
     F                                     RENAME(PBL01:PBL03)
     F*** END CHG *****  09/03/03  ************************************
     FWABLL3    IF   E           K DISK
     F                                     RENAME(PBL01:PBL02)
     F*** begin add ***  08/01/25  ************************************
     FWABLL9    IF   E           K DISK
     F                                     RENAME(PBL01:PBL09)
     FWMEML     IF   E           K DISK
     FWABLP     O  A E           K DISK    PREFIX(B2:2)
     F                                     RENAME(PBL01:PBL0P)
     F*** end add *****  08/01/25  ************************************
     F*** BEGIN ADD ***  03/08/02  ************************************
     F*** BEG   DEL ***  03/27/11  ************************************
     F*******WZEXL   IF  E           K        DISK
     F*** END   DEL ***  03/27/11  ************************************
     F*** END ADD *****  03/08/02  ************************************

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     d*** Begin Add *** 08/08/18 *************************************
     D  qqAllDiv       s              1a
     D  q1AllDiv       s              1a   Inz(' ')
     d*** End   Add *** 08/08/18 *************************************
     d*** begin add ***  08/01/25  ************************************
     D  polARSum       s              1a
     D  xxDate         s              7  0
     D  xxTime         s              6  0
      /copy wPrEmplr
      /copy wPrAgent
     d*** end add *****  08/01/25  ************************************

     C************************************************************
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C     ELKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  BILLING WORK FILE
     C*
     C     BLKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     BLKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** BEGIN DEL ***  09/03/03  ************************************
     C*********            KFLD           XXFYR
     C*** END DEL *****  09/03/03  ************************************
     C                   KFLD                    XXEMP#
     C                   KFLD                    ELDIV
     C*** BEGIN ADD ***  09/03/03  ************************************
     C                   KFLD                    XXFYR
     C*** END ADD *****  09/03/03  ************************************
     C*
     C     BLKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXRNB
     C*
     C     BLKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C     BLKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXRNB
     C                   KFLD                    XXEMP#
     C*** BEGIN ADD ***  03/08/02  ************************************
     C*** BEG   DEL ***  03/27/11  ************************************
     C*********  ZEXKY     KLIST
     C*********            KFLD           XXCO#
     C*********            KFLD           XXFND
     C*********            KFLD           XXEMP#
     C*********            KFLD           XXDIV
     C*** END   DEL ***  03/27/11  ************************************
     C*** END ADD *****  03/08/02  ************************************
     c*** begin add ***  08/01/25  ************************************
     C     BLKY9         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXRNB
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     c*** end add *****  08/01/25  ************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQRNB             1
     C*** Begin Add *** 08/08/18 ************************************
     C                   PARM                    qqAllDiv
     C*** End   Add *** 08/08/18 ************************************
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
     C     DTCYM2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** BEGIN ADD ***  03/03/03  ************************************
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** END ADD *****  03/03/03  ************************************
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELEMP#        SVEMP#
     C     *LIKE         DEFINE    ELDIV         SVDIV
     C     *LIKE         DEFINE    QQCO#         Q1CO#
     C     *LIKE         DEFINE    QQFND         Q1FND
     C     *LIKE         DEFINE    QQFYR         Q1FYR
     C     *LIKE         DEFINE    QQEMP#        Q1EMP#
     C     *LIKE         DEFINE    ELDIV         X1DIV
     C*
     C*    SET UP AND INITALIZE
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQEMP#        SVEMP#
     C                   MOVE      QQDIV         SVDIV
     C                   MOVE      QQDIV         XXDIV
     C*** Begin Add *** 08/08/18 ************************************
     C*
     C* Set All division indicator
     C                   If        %Parms() > 7
     C                   Eval      q1AllDiv = qqAllDiv
     C                   EndIf
     C*** End   Add *** 08/08/18 ************************************
     C*** BEGIN ADD ***  06/20/05  ************************************
     C     QQRNB         IFEQ      '#'
     C                   MOVE      *BLANK        XXRNB
     C                   MOVE      *ON           *IN71
     C                   ELSE
     C                   MOVE      *OFF          *IN71
     C*** END ADD *****  06/20/05  ************************************
     C                   MOVE      QQRNB         XXRNB             1
     C*** BEGIN ADD ***  06/20/05  ************************************
     C                   ENDIF
     C*** END ADD *****  06/20/05  ************************************
     C                   TIME                    DSDTTM
     C*** begin add ***  08/01/25  ************************************
     C* Create WABLP records for state reporting divisions
     C                   Exsr      SRGenReq
     C*** end add *****  08/01/25  ************************************
     C*
     C                   EXSR      SRCALL
     C*
     C                   MOVE      '1'           *INLR
     C***************************************************************
     C*                   SUBROUTINE SRCALL
     C**                                                              *
     C**      THIS SUBRTN  IS CALLED BEFORE THE EXECUTION OF ORIGINAL *
     C**      AND AUDIT BILLS IN ORDER TO PREPARE BILLINGS FOR        *
     C**      ACCUMULATING OF PREMIUM ACROSS ALL DIVISIONS.           *
     C**                                                              *
     C**      THEREFORE, BEFORE EACH BILL IS CALCULATED, WE NEED TO   *
     C**      KNOW THE INC LIMIT PREMIUM OF EACH DIVISION.            *
     C**                                                              *
     C**      THIS SUBRTN WILL CALL W8010R OR W7720R FOR EACH EMPR/DIV*
     C**      WITHOUT UPDATING A/R.  AT THIS POINT, CALCULATIONS WILL *
     C**      ONLY BE CORRECT FOR MANUAL PREMIUM.                     *
     C**                                                              *
     C**      W8010R OR W7720R IS CALLED AGAIN THRU THE NORMAL CALL   *
     C**      BUT THIS TIME IT WILL HAVE ALL INC LMT PREM IN WDILP TO *
     C**      ARRIVE AT THE CORRECT INCR LIMITS' AMOUNTS              *
     C***************************************************************
     C     SRCALL        BEGSR
     C*
     C     SVEMP#        IFEQ      *ZERO
     C*
     C*** BEGIN CHG ***  06/20/05  ************************************
     C*********  QQRNB     IFEQ 'B'
     C     XXRNB         IFEQ      'B'
     C*** END CHG *****  06/20/05  ************************************
     C     BLKY          CHAIN     PBL01                              60
     C                   ELSE
     C     BLKY2         CHAIN     PBL02                              60
     C                   ENDIF
     C*
     C*** BEGIN DEL ***  12/07/00  ************************************
     C*********            Z-ADDBLEMP#    XXEMP#
     C*********            Z-ADDBLFYR     XXFYR
     C*** END DEL *****  12/07/00  ************************************
     C*
     C                   ELSE
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
     C*
     C* MAIN LOOP
     C*
     C     *IN60         DOWEQ     *OFF
     C*** BEGIN ADD ***  12/07/00  ************************************
     C     XXEMP#        IFNE      BLEMP#
     C     XXFYR         ORNE      BLFYR
     C     SVEMP#        ORNE      *ZERO
     C*
     C     SVEMP#        IFEQ      *ZERO
     C                   Z-ADD     BLEMP#        XXEMP#
     C                   Z-ADD     BLFYR         XXFYR
     C*** BEG ADD *****  02/15/06  ************************************
     C                   MOVE      BLFLG3        SVFLG3            1
     C*** END ADD *****  02/15/06  ************************************
     C                   ENDIF
     C*** END ADD *****  12/07/00  ************************************
     C*
     C* CLEAR MULTIPLE PREMIUM FILE FIRST
     C*
     C     QQTYPE        IFEQ      'E'
     C*
     C*** BEGIN CHG ***  06/20/05  ************************************
     C*********  ELKY1     CHAINPIL01                60
     C  N71ELKY1         CHAIN     PIL01                              60
     C   71ELKY          CHAIN     PIL01                              60
     C*** END CHG *****  06/20/05  ************************************
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PIL01
     C*** BEGIN CHG ***  06/20/05  ************************************
     C*********  ELKY1     READEPIL01                    60
     C  N71ELKY1         READE     PIL01                                  60
     C   71ELKY          READE     PIL01                                  60
     C*** END CHG *****  06/20/05  ************************************
     C                   ENDDO
     C*
     C                   ENDIF                                                  QQTYPE IFEQ
     C*
     C* IF NOT A REVISED BILL, CALL W8010R OR W7720R FOR EACH DIV
     C*
     C                   MOVE      XXCO#         Q1CO#
     C                   MOVE      XXFND         Q1FND
     C                   MOVE      XXFYR         Q1FYR
     C*** BEGIN CHG ***  03/03/03  ************************************
     C*********            MOVE DSDATE    Q1DATE  6
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C*****              MOVEL     SRDTO         Q1DATE            8
     C                   MOVEL     SRDTO         Q1DATE8           8
     C*** END CHG *****  03/03/03  ************************************
     C                   MOVE      *BLANKS       Q1ESCD            2
     C                   MOVE      *BLANKS       Q1OVR             1
     C                   MOVE      *BLANKS       Q1SRCD            2
     C                   MOVE      *BLANKS       Q1PRT             1
     C                   MOVE      *BLANKS       Q1AJCD            2
     C                   MOVE      'W'           Q1QTE             1
     C                   MOVE      *BLANKS       Q1AR              2
     C*
     C* EMPLOYER DIVISION LOOP
     C*
     C*** BEGIN CHG ***  06/20/05  ************************************
     C*********  ELKY1     CHAINPEL01                60
     C  N71ELKY1         CHAIN     PEL01                              60
     C   71ELKY          CHAIN     PEL01                              60
     C*** END CHG *****  06/20/05  ************************************
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      ELEMP#        Q1EMP#
     C                   MOVE      ELDIV         Q1DIV             5
     C*
     C*  IF ESTIMATING TYPE, CALL W8010R FOR EACH DIV, NO A/R UPDATE
     C*  ELSE CALL W7720R
     C*
     C                   MOVE      'Y'           XXPROC            1
     C     QQTYPE        IFEQ      'E'
     C*
     C     ELFAF         IFNE      'A'
     C*
     C                   CALL      'W8010R'
     C                   PARM                    Q1CO#
     C                   PARM                    Q1FND
     C                   PARM                    Q1FYR
     C                   PARM                    Q1EMP#
     C                   PARM                    Q1DIV
     C*****              PARM                    Q1DATE
     C                   PARM                    Q1DATE8
     C                   PARM                    Q1ESCD
     C                   PARM                    Q1OVR
     C                   PARM                    Q1SRCD
     C                   PARM                    Q1PRT
     C                   PARM                    Q1AJCD
     C                   PARM                    Q1QTE
     C                   PARM      'I'           Q1RNB             1
     C*
     C                   ELSE
     C                   MOVE      'N'           XXPROC
     C                   END
     C*
     C                   ELSE
     C*
     C* CHECK BILLING FLAG WHEN RUNNING MASS AUDITS.
     C*
     C     SVEMP#        IFEQ      *ZERO
     C     BLKY1         CHAIN(N)  PBL03                              60
     C*
     C     *IN60         IFEQ      *ON
     C     BLFLG2        OREQ      *BLANK
     C                   MOVE      'N'           XXPROC
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  SVEMP# IFEQ
     C*
     C     XXPROC        IFEQ      'Y'
     C*
     C                   CALL      'W7720R'
     C                   PARM                    Q1CO#
     C                   PARM                    Q1FND
     C                   PARM                    Q1FYR
     C                   PARM                    Q1EMP#
     C                   PARM                    Q1DIV
     C*****              PARM                    Q1DATE
     C                   PARM                    Q1DATE8
     C                   PARM                    Q1AR
     C                   PARM      'I'           QQREV             1
     C*** Begin Add *** 08/08/18 *************************************
     C                   PARM                    q1AllDiv
     C*** End   Add *** 08/08/18 *************************************
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* UPDATE BILLING FLAGS IN MASS RUNS
     C*
     C     SVEMP#        IFEQ      *ZERO
     C     XXPROC        ANDEQ     'Y'
     C*
     C     BLKY1         CHAIN     PBL03                              60
     C     QQTYPE        IFEQ      'E'
     C                   MOVE      'Y'           BLFLG1
     C                   MOVE      *BLANK        BLFLG2
      *** BEG CHG ***** 02/15/06 **********************************
     C*****************    MOVE 'Y'       BLFLG3
     C                   MOVE      SVFLG3        BLFLG3
      *** END CHG ***** 02/15/06 **********************************
     C                   ELSE
     C                   MOVE      *BLANK        BLFLG1
     C                   MOVE      'Y'           BLFLG2
     C                   MOVE      *BLANK        BLFLG3
     C                   ENDIF
     C*** BEG ADD ***** 03/27/11 *************************************
     C                   SELECT
     C     ELNWRW        WHENNE    ' '
     C                   MOVE      ELNWRW        BLRFLG
     C                   OTHER
     C                   CALL      'WA069R'
     C                   PARM      ELCO#         #1CO#             3 0
     C                   PARM      ELFND         #1FND             3 0
     C                   PARM      ELEMP#        #1EMP#            9 0
     C                   PARM      ELFYR         #1FYR             3 0
     C                   PARM      *BLANKS       #1NWRW            1
     C*
     C                   MOVE      #1NWRW        BLRFLG
     C                   ENDSL
     C*** END ADD ***** 03/27/11 *************************************
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      XXCO#         BLCO#
     C                   MOVE      XXFND         BLFND
     C                   MOVE      XXFYR         BLFYR
     C                   MOVE      XXEMP#        BLEMP#
     C                   MOVE      ELDIV         BLDIV
     C                   MOVE      Q1USER        BLUSER
     C                   MOVE      Q1PGM         BLPGM
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         BLDT
     C                   MOVE      DSTIME        BLTIME
     C                   MOVE      ELPFDT        BLPFDT
     C                   MOVE      ELDIV         XXDIV
     C*** BEG DEL ***** 03/27/11 *************************************
     C********** ELKY2     CHAINPEL03                60
     C********** ELKY3     REDPEPEL03                    60
     C*** BEGIN ADD ***  03/08/02  ************************************
     C***60***** ZEXKY     CHAINPEX01                60
     C*** END ADD *****  03/08/02  ************************************
     C**********
     C********** *IN60     IFEQ *OFF
     C**********           MOVE 'R'       BLRFLG
     C**********           ELSE
     C**********           MOVE 'N'       BLRFLG
     C**********           ENDIF                           *IN60 EQ 0
     C*** END DEL ***** 03/27/11 *************************************
     C*
     C                   Z-ADD     *ZERO         BLAGT#
     C     AHKY          SETGT     PAH01
     C                   READP     PAH01                                  50
     C     *IN50         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  50
     C                   ENDDO                                                  *IN50 EQ 0
     C*
     C     *IN50         IFEQ      '0'
     C     XXCO#         ANDEQ     AHCO#
     C     XXFND         ANDEQ     AHFND
     C     XXEMP#        ANDEQ     AHEMP#
     C     XXDIV         ANDEQ     AHDIV
     C     XXFYR         ANDEQ     AHFYR
     C                   MOVEL     AHAGT#        BLAGT#
     C                   ENDIF                                                  *IN50 EQ 0
     C*
     C                   WRITE     PBL03
     C*
     C                   ELSE
     C*
     C                   UPDATE    PBL03
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  SVEMP# IFEQ
     C*
     C*  READ NEXT DIVISION
     C*
     C*** BEGIN CHG ***  06/20/05  ************************************
     C*********  ELKY1     READEPEL01                    60
     C  N71ELKY1         READE     PEL01                                  60
     C   71ELKY          READE     PEL01                                  60
     C*** END CHG *****  06/20/05  ************************************
     C*
     C                   ENDDO
     C*** BEGIN ADD ***  12/07/00  ************************************
     C                   ENDIF                                                  XXEMP# IFNE
     C*** END ADD *****  12/07/00  ************************************
     C*
     C* READ NEXT EMPLOYER FOR MASS
     C*
     C     SVEMP#        IFEQ      *ZERO
     C*
     C*** BEGIN CHG ***  06/20/05  ************************************
     C*********  QQRNB     IFEQ 'B'
     C     XXRNB         IFEQ      'B'
     C*** END CHG *****  06/20/05  ************************************
     C*** BEGIN DEL ***  12/07/00  ************************************
     C*********  BLKY3     SETGTPBL01
     C*** END DEL *****  12/07/00  ************************************
     C     BLKY          READE     PBL01                                  60
     C                   ELSE
     C*** BEGIN DEL ***  12/07/00  ************************************
     C*********  BLKY4     SETGTPBL02
     C*** END DEL *****  12/07/00  ************************************
     C     BLKY2         READE     PBL02                                  60
     C                   ENDIF
     C*
     C*** BEGIN DEL ***  12/07/00  ************************************
     C*********            Z-ADDBLEMP#    XXEMP#
     C*********            Z-ADDBLFYR     XXFYR
     C*** END DEL *****  12/07/00  ************************************
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  SVEMP# IFEQ
     C*
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDSR
     C*** begin add ***  08/01/25  ************************************
     C***************************************************************
     C*                   SUBROUTINE SRGenReq
     C* Create WABLP records for state reporting divisions for policies
     C* which are summarized but state reporting division isnt processing
     C* during mass audits.
     C*
     C     SRGenReq      Begsr
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         xxDate
     C                   MOVE      DSTIME        xxTime
     C*
           If not (svemp# = 0 and QQType = 'A');
              Leavesr;
           Endif;

           If xxRNB = 'B';
              Setll blky pbl01;
              Reade blky pbl01;
           Else;
              Setll blky2 pbl09;
              Reade blky2 pbl09;
           Endif;

           polARSum = ' ';

           //* Loop over all records in billing request file.
           Dow not %eof;

              xxEmp# = BLEmp#;
              xxFyr = BLFyr;
              xxDiv = BLDiv;
              Chain elky3 pem01;
              Chain elky  pel01;

              Select;
              //* Policy has record for state rpt div.
              When EMStrp = 'Y';
                 polARSum = 'N';
                 If xxRNB = 'B';
                    Setgt elky1 pbl01;
                 Else;
                    Setgt blky9 pbl09;
                 Endif;
              //* Policy is summarized but no state rpt div found yet.
              When EMStrp <> 'Y' and ELArsm = 'Y' and polARSum <> 'N';
                 polARSum = 'Y';
              Endsl;

              If xxRNB = 'B';
                 Reade blky pbl01;
              Else;
                 Reade blky2 pbl09;
              Endif;

              //* Advancing to next policy or done.
              If BLEmp# <> xxEmp# or BLFyr <> xxFyr or %eof;
                 //* Policy is summary, needs state rpt div record written.
                 If polARSum = 'Y';

                    xxDiv = GetStRptDv(xxCo#:xxFnd:xxEmp#);
                    Chain elky  pel01;

                    Clear pbl0p;
                    B2Co# = xxCo#;
                    B2Fnd = xxFnd;
                    B2Emp# = xxEmp#;
                    B2Fyr = xxFyr;
                    B2Div = xxDiv;
                    B2User = Q1User;
                    B2Pgm  = Q1Pgm;
                    B2User = Q1User;
                    B2time = xxTime;
                    B2dt   = xxDate;
                    B2Pfdt = ELPfdt;
                    B2Flg2 = 'Y';
                    B2Rflg = ELNwrw;
                    B2Agt# = GetEmpAgt(xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr);
                    Write pbl0p;

                 Endif;
                 polARSum = ' ';
              Endif;
           Enddo;

           Clear xxEmp#;
           Clear xxFyr;
           Clear xxDiv;
     C*
     C                   Endsr
     C*** end add *****  08/01/25  ************************************
