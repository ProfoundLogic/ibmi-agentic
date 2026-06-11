     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE SRMDIV - GENERATE PREMIUM ON ALL DIVISIONS
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                       *
     H**                                                              *
     H**      THIS PROGRAM IS CALLED BY VARIOUS BILLING PROGRAMS IN   *
     H**      PREPARATION OF GENERATING PREMIUM OVER ALL DIVS.        *
     H**      SUBROUTINE PARM:  '1' RETURN PREMIUM                    *
     H**                        '2' PREMIUM SET UP                    *
     H**                        '3' REVISED BILL OVERRIDE LOAD        *
     H**                                                              *
     H**      RETURN: QQPREM - TOTAL OF ALL NON-STATE RPT DIV'S.      *
     H**                                                              *
     H**                                                              *
     H* CALLED FROM:                                                  *
     H*   W8002C  - ORIGINAL BILL SUBMISSION      SBRT = 2            *
     H*   W7710C  - FINAL AUDIT BILL SUBMISSION   SBRT = 2            *
     H*   W0090R  - REVISED BILLING CALC          SBRT = 3            *
     H* CALLS TO:                                                     *
     H*                                                               *
     H*                                                               *
     H* PROGRAMMER: JOHN HOLSOMBACH                                   *
     H* CREATED:    01/21/98            PROJECT: WC705                *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H* 61  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/14/24 C:291 Project: WC2974                                *
     H*                Recompile over WDMPP File changes.             *
     H*                                                               *
     H* 06/24/24 C:333 Project: 146WKR                                *
     H*                Recompile                                      *
     H*                                                               *
     H* 03/13/24 C:062 Project: 146WKQ                                *
     H*                Recompile                                      *
     H*                                                               *
     H* 03/04/24 C:079 Project: WC2951                                *
     H*                Recompile over WAELP File changes              *
     H*                                                               *
     H* 11/09/23 C:65  PROJECT: WC2810                                *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDMPP.                                *
     H*                                                               *
     H* 07/30/20 C:65  PROJECT: Phw#90082 (Sunz)                      *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDMPP.                                *
     H*                                                               *
     H* 04/22/19  C:65  PROJECT: WC2610                               *
     H*                 RECOMPILED FOR NEW FIELD in WDILP.            *
     H*                                                               *
     H* 05/01/19 C:291 Project: WC2206                                *
     H*                Recompile: New field added to WDELP.           *
     H*                                                               *
     H* 08/08/18 C:62  Project: 150WCS                                *
     H*                Add All division processing.                   *
     H*                                                               *
     H* 07/17/17 C:65  PROJECT: 150WAP                                *
     H*                ADD CODING TO RECOGNIZE THE 'C' VALUE FOR PEO. *
     H*                                                               *
     H* 08/25/15 C:302 Project: WC2385                                *
     H*                Perform a check at the end of the revised bill *
     H*                +when a user attempts to submit the revised    *
     H*                bill to check if the live files have been      *
     H*                updated since the revised bill started.        *
     H*                Clear time stamp before coping record.         *
     H*                                                               *
     H* 09/03/14 C:136 Support: WA147 86538                           *
     H*                Fix logic for copying to WALDP                 *
     H*                (Large Deductible).                            *
     H*                                                               *
     H* 05/29/13 C:283 Support: 82425 WA146                           *
     H*                Do not write to WALDP file in case if the      *
     H*                LD Module is not installed.                    *
     H*                                                               *
     H* 08/30/12 C:136 SUPPORT: WA025 80122                           *
     H*               -POPULATE WALDP (LARGE DEDUCTIBLE) IN           *
     H*                REVISED BILLING                                *
     H*               -CHANGE Q1DATE TO Q2DATE FOR COMPATIBILITY      *
     H*                WITH NEW FORMAT OF SZQ1P                       *
     H*                                                               *
     H* 03/27/11 C:255 SUPPORT 71062  WA099                           *
     H*                ADDED CHANGE TO POPULATE BLRFLG(WABLP) WITH    *
     H*                NW/RNW FLAG VALUE.                             *
     H*                                                               *
     H* 01/06/11 C:65  Project: 139WPL                                *
     H*                CHANGE OVERRIDE TO USE QCMDEXC INSTEAD.        *
     H*                                                               *
     H* 11/10/10 C:33  Project: 139WPL                                *
     H*                Include HCN Program information.  POPULATE THE *
     H*                CREDIT INTO WDMPP WHEN WRITING A NEW RECORD.   *
     H*                                                               *
     H* 07/28/10 C:255 PROJECT: 132WOJ                                *
     H*                RECOMPILED TO MAKE IT COMPATIBLE WITH WDELP AS*
     H*                NEW FIELDS ADDED FOR FLEX INFORMATION
     H*                                                               *
     H*                                                               *
     H* 06/21/10 C:255 PROJECT: 114WVY                                *
     H*                MODIFY PROGRAM TO RECOGNIZE NEW FIELDS ADDED IN*
     H*                FILES WDMPP,WDELP OF 2ND PERIOD DEVIATIONS FOR*
     H*                SPLIT DEVIATIONS.                              *
     H*                                                               *
     H* 09/22/09 C:234 PROJECT: 93WIB                                 *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP.                                *
     H*                                                               *
     H* 01/07/09 C:65  PROJECT: WC1769                                *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP.                                *
     H*                                                               *
     H* 01/22/08  C:65  PROJECT: WC1627                               *
     H*                 POPULATE NEW FIELDS IN WDMPP FOR NET RATE.    *
     H*                                                               *
     H* 10/11/07 C:183 PROJECT: 146WBC                                *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP.                                *
     H*                                                               *
     H* 09/18/07 C:65  PROJECT: SUPP# 60174                           *
     H*                ONLY RETURN FLAG FOR MINIMUM PREMIUM IF ELEMNP *
     H*                IS SET TO 'Y' FOR STATE REPORTING DIVISION.    *
     H*                                                               *
     H* 08/22/07 C:65  PROJECT: 114WMF                                *
     H*                ADD CODING TO RECOGNIZE THE 'Y' VALUE FOR PEO  *
     H*                FLAG FOR CSI FOR SPREAD DISCOUNT.              *
     H*                                                               *
     H*  10/17/06 C:65 PROJECT: 114WGH/WC1298                         *
     H*                ADD OVERRIDE FOR PREMIUM PERIODS FILE WDPPP.   *
     H*                                                               *
     H*  05/26/06 C:65 PROJECT: SUPP# 52933/WC1297                    *
     H*                IF A GROUP IS A PEO GROUP AND SET TO SPREAD    *
     H*                THE DISCOUNT ACROSS THE ENTIRE GROUP, ENSURE   *
     H*                THE FILE WADSP IS LOADED WITH THE ENTIRE       *
     H*                GROUP'S INFORMATION.                           *
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
     H* 04/07/05      PROJECT: 126WDK                                 *
     H*          C:33 PROJECT: 126WCD                                 *
     H*               MULTIPLE DEDUCTIBLE DISCOUNTS PER YEAR.         *
     H*                                                               *
     H* 05/18/04 C:65 PROJECT: SUPPORT                                *
     H*               ONLY CLEAR THE PREMIUM  FILE FOR ESTIMATED      *
     H*               BILLS.  AUDIT BILLS NEED TO RETAIN EST INFO.    *
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
     H* 03/14/02 C:65 PROJECT WC1031                                  *
     H*               ADD STANDARD PREMIUM TO THE CAPTURED INFO.      *
     H*                                                               *
     H*  03/08/02 C:65 PROJECT: WC1030                                *
     H*                IF THE EMPLOYER HAS A PREVIOUS EMPLR# THEN     *
     H*                CONSIDER THE POLICY AS A RENEWAL.              *
     H*                                                               *
     H* 01/28/02 C:65 PROJECT WC1031                                  *
     H*               CAPTURE PREMIUM INFO BEFORE MIN PREM IN WDMPP.  *
     H*                                                               *
     H* 10/01/01 C:65 PROJECT: WC1045                                 *
     H*               ADD FIELDS TO HOLD ANNUAL PREMIUM BEFORE LOSS   *
     H*               CONSTANT APPLICATION. SET THE RATING STATE ALSO.*
     H*               IF A POLICY HAS A STATE LEVEL LOSS CONSTANT     *
     H*               DEFINITION, THEN RUN THE CALCULATIONS TWICE.    *
     H*               THE FIRST TIME SET RNB FLAG TO 'L'.  THE SECOND *
     H*               RUN THE FLAG WILL BE SET TO 'Z'.  IF THERE ARE  *
     H*               NO LOSS CONSTANT DEFINITIONS, RUN CALCULTIONS   *
     H*               ONCE WITH A FLAG VALUE OF 'Z'(NORMAL).          *
     H*               THE FIRST RUN THROUGH SETS THE ANNUAL PREMIUM   *
     H*               BEFORE LOSS CONSTANT.  THE SECOND TIME THROUGH  *
     H*               SETS THE ANNUAL PREMIUM WITH LOSS CONSTANT.     *
     H*                                                               *
     H* 12/07/00 C:65 PROJECT: SUPPORT#25300                          *
     H*               PROCESS A EMP#/FYR CHECK INSTEAD OF DOING A     *
     H*               SETGT IN ORDER TO AVOID SKIPPING RECORDS IN     *
     H*               WABLP.                                          *
     H*                                                               *
     H* 11/15/99 C:65  PROJECT: WC936                                 *
     H*                LOAD THE FILES WAILP, WADSP, AND WAMPP FOR THE *
     H*                REVISED BILL OVERRIDE CALL.                    *
     H*                                                               *
     H*****************************************************************
     F*** BEG   DEL ***  03/27/11  ************************************
     F********WDELL   IF  E           K        DISK
     F********    PEL01                             KRENAMEPEL03
     F*** END   DEL ***  03/27/11  ************************************
     FWDELL1    IF   E           K DISK
     FWAELL     UF A E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWMEPL1    IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDMPL     UF   E           K DISK
     FWAEPL1    UF A E           K DISK
     F                                     RENAME(PEP01:PEP02)
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
     F*** BEGIN ADD ***  11/15/99  ************************************
     FWDILL     IF   E           K DISK
     FWAILL     IF A E           K DISK
     F                                     RENAME(PIL01:PIL02)
     FWAMPL     IF A E           K DISK
     F                                     RENAME(PMP01:PMP02)
     FWDDSL     IF   E           K DISK
     FWADSL     IF A E           K DISK
     F                                     RENAME(PDS01:PDS02)
     F*** END ADD *****  11/15/99  ************************************
     F*** BEGIN ADD ***  10/01/01  ************************************
     FWMRSL     IF   E           K DISK
     FWDSCL     IF   E           K DISK
     F*** END ADD *****  10/01/01  ************************************
     F*** BEGIN ADD ***  03/08/02  ************************************
     F*** BEG   DEL ***  03/27/11  ************************************
     F*********WZEXL   IF  E           K        DISK
     F*** END   DEL ***  03/27/11  ************************************
     F*** END ADD *****  03/08/02  ************************************
     F*** BEGIN ADD ***  05/26/06  ************************************
     FWMFNL     IF   E           K DISK
     F*** END ADD *****  05/26/06  ************************************
     F*** BEGIN ADD ***  10/17/06  ************************************
     FWDPPL     UF   E           K DISK
     FWAPPL     IF A E           K DISK
     F                                     RENAME(PPP01:PPP02)
     F*** END ADD *****  10/17/06  ************************************
     F***  Begin Add  11/10/10  *******************************************
     FWDHCL     IF   E           K DISK
     F                                     RENAME(PHC01:PHC00)
     FWDHCLR    IF A E           K DISK    USROPN
     F                                     RENAME(PHC01:PHC0R)
     F*** BEG ADD ***** 08/30/12 **************************************
     FWMLDL1    IF   E           K DISK
     FWALDL1    IF A E           K DISK
     F                                     RENAME(PLD01:PLD01X)
     F*** END ADD ***** 08/30/12 **************************************
     F*** BEG ADD ***** 05/29/13 **************************************
     FSSYLF     IF   E           K DISK
     F                                     ignore(SSY01)
     F                                     ignore(SPC01)
     F                                     ignore(SFB01)
     F                                     ignore(SRC01)
     F                                     ignore(SDR01)
     F                                     ignore(SBC01)
     F                                     ignore(SCS01)
     F                                     ignore(SGL01)
     F                                     ignore(SP201)
     F*** END ADD ***** 05/29/13 **************************************

     D*
     D OVRHC           C                   CONST('OVRDBF FILE(WDHCLR) -
     D                                     MBR(REVISED) -
     D                                     OVRSCOPE(*JOB)')
     D PFHC            C                   CONST('ADDPFM FILE(WDHCP) -
     D                                     MBR(REVISED)')
     D LFHC            C                   CONST('ADDLFMBRS -
     D                                     PHYFILE(WDHCP)')
     D***  End   Add  11/10/10  *******************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     d*** Begin Add *** 08/08/18 *************************************
     D  qqAllDiv       s              1a
     D  q1AllDiv       s              1a   Inz(' ')
     d*** End   Add *** 08/08/18 *************************************

     D*** BEG ADD *****  05/29/13  ************************************
     D xsKey           s              2A
     D noLDModule      s              1A
     D*** END ADD *****  05/29/13  ************************************
     C************************************************************
     C*** BEGIN ADD ***  09/18/07  ************************************
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    ELDIV
     C*** END ADD *****  09/18/07  ************************************
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
     C*** BEGIN ADD ***  05/26/06  ************************************
     C*
     C     ELKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C* GROUP MASTER FILE.
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C* SPREAD DISCOUNT FILE.
     C*
     C     DSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*** END ADD *****  05/26/06  ************************************
     C*** BEGIN ADD ***  10/01/01  ************************************
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
     C*
     C*  LOSS CONSTANT FILE
     C*
     C     SCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELPFDT
     C*
     C     SCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C*** END ADD *****  10/01/01  ************************************
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
     C*
     C*  EMPLOYER PAYROLL FILE
     C*
     C     EPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     EPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C***  Begin Add  ***  11/10/10  **************************************
     C     HCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C***  End   Add  ***  11/10/10  **************************************
     C*
     C*** BEGIN ADD ***  03/08/02  ************************************
     C*** BEG   DEL ***  03/27/11  ************************************
     C*********  ZEXKY     KLIST
     C*********            KFLD           XXCO#
     C*********            KFLD           XXFND
     C*********            KFLD           XXEMP#
     C*********            KFLD           XXDIV
     C*** END   DEL ***  03/27/11  ************************************
     C*** END ADD *****  03/08/02  ************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQPREM           11 2
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQCALL            1
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
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELEMP#        SVEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELDIV         SVDIV
     C     *LIKE         DEFINE    QQCO#         Q1CO#
     C     *LIKE         DEFINE    QQFND         Q1FND
     C     *LIKE         DEFINE    QQFYR         Q1FYR
     C     *LIKE         DEFINE    QQEMP#        Q1EMP#
     C     *LIKE         DEFINE    ELDIV         X1DIV
     C*
     C***  Begin Add  ***  11/10/10  **************************************
     C* Ensure REVISED member exists for WDHCP/L*
     C                   CALL      'SRCHKMBR'                           99
     C                   PARM      'WDHCP'       ##OBJ            10
     C                   PARM      '*FILE'       ##TYPE            7
     C                   PARM      'REVISED'     ##MBR            10
     C                   PARM                    ##ERR             1
     C*
     C     ##ERR         IFEQ      *ON
     C                   CALL      'QCAEXEC'                            99
     C                   PARM      PFHC          QCA              55
     C                   PARM      55            LENGTH           15 5
     C                   CALL      'QCAEXEC'                            99
     C                   PARM      LFHC          QCA              55
     C                   PARM      55            LENGTH           15 5
     C                   ENDIF
     C*
     C* Override WDHCLR to REVISED member
     C***  BEGIN CHG ****  01/06/11  **************************************
     C**********           CALL 'QCAEXEC'              99
     C                   CALL      'QCMDEXC'                            99
     C***  END CHG ******  01/06/11  **************************************
     C                   PARM      OVRHC         QCA              55
     C                   PARM      55            LENGTH           15 5
     C*
     C                   OPEN      WDHCLR
     C***  End   Add  ***  11/10/10  **************************************
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
     C*** Begin Add *** 08/08/18 ************************************
     C*
     C* Set All division indicator
     C                   If        %Parms() > 9
     C                   Eval      q1AllDiv = qqAllDiv
     C                   EndIf
     C*** End   Add *** 08/08/18 ************************************
     C                   TIME                    DSDTTM
     C*** BEGIN ADD ***  05/26/06  ************************************
     C     FNKY          CHAIN     PFN01                              60
     C*** END ADD *****  05/26/06  ************************************
     C*
     C*  RETURN PREMIUM.
     C*
     C     QQCALL        IFEQ      '1'
     C                   EXSR      SRRET
     C                   END
     C*
     C*  IF SUBROUTINE '2', NEED TO CALL W8010R OR W7720R FOR
     C*  EMPLOYERS SPREADING DISCOUNTS.
     C*
     C     QQCALL        IFEQ      '2'
     C                   EXSR      SRCALL
     C                   END
     C*
     C*  IF SUBROUTINE '3', NEED TO CALL LOAD OVERRIDE FILES FOR
     C*  EACH DIVISION FOR EMPLOYERS SPREADING DISCOUNT.
     C*
     C     QQCALL        IFEQ      '3'
     C                   EXSR      SRLOAD
     C                   END
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
     C**      KNOW THE ESTIMATED PREMIUM OF EACH DIVISION.            *
     C**                                                              *
     C**      THIS SUBRTN WILL CALL W8010R OR W7720R FOR EACH EMPR/DIV*
     C**      WITHOUT UPDATING A/R.  AT THIS POINT, CALCULATIONS WILL *
     C**      ONLY BE CORRECT UP TO THE STANDARD PREMIUM.             *
     C**                                                              *
     C**      W8010R OR W7720R IS CALLED AGAIN THRU THE NORMAL CALL   *
     C**      BUT THIS TIME IT WILL HAVE ALL STD PREMIUMS IN WDDSP TO *
     C**      ARRIVE AT THE CORRECT TOTAL PREMIUM.                    *
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
     C*** BEGIN ADD ***  05/18/04  ************************************
     C     QQTYPE        IFEQ      'E'
     C*** END ADD *****  05/18/04  ************************************
     C*** BEGIN CHG ***  06/20/05  ************************************
     C*********  ELKY1     CHAINPMP01                60
     C  N71ELKY1         CHAIN     PMP01                              60
     C   71ELKY          CHAIN     PMP01                              60
     C*** END CHG *****  06/20/05  ************************************
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PMP01
     C*** BEGIN CHG ***  06/20/05  ************************************
     C*********  ELKY1     READEPMP01                    60
     C  N71ELKY1         READE     PMP01                                  60
     C   71ELKY          READE     PMP01                                  60
     C*** END CHG *****  06/20/05  ************************************
     C                   ENDDO
     C*** BEGIN ADD ***  05/18/04  ************************************
     C                   ENDIF
     C*** END ADD *****  05/18/04  ************************************
     C*** BEGIN ADD ***  10/17/06  ************************************
     C     QQTYPE        IFEQ      'E'
     C  N71ELKY1         CHAIN     PPP01                              60
     C   71ELKY          CHAIN     PPP01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PPP01
     C  N71ELKY1         READE     PPP01                                  60
     C   71ELKY          READE     PPP01                                  60
     C                   ENDDO
     C                   ENDIF
     C*** END ADD *****  10/17/06  ************************************
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
     C*** BEG CHG *****  08/30/12  ************************************
     C***********        MOVEL     SRDTO         Q1DATE            8
     C                   MOVEL     SRDTO         Q2DATE            8
     C*** END CHG *****  08/30/12  ************************************
     C*** END CHG *****  03/03/03  ************************************
     C                   MOVE      *BLANKS       Q1ESCD            2
     C                   MOVE      *BLANKS       Q1OVR             1
     C                   MOVE      *BLANKS       Q1SRCD            2
     C                   MOVE      *BLANKS       Q1PRT             1
     C                   MOVE      *BLANKS       Q1AJCD            2
     C                   MOVE      'W'           Q1QTE             1
     C                   MOVE      *BLANKS       Q1AR              2
     C*** BEGIN ADD ***  10/01/01  ************************************
     C                   MOVE      *BLANK        XXLOSS            1
     C                   EXSR      CHKLC
     C*
     C                   MOVE      *OFF          *IN95
     C     *IN95         DOWEQ     *OFF
     C*** END ADD *****  10/01/01  ************************************
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
     C*** BEGIN ADD ***  10/01/01  ************************************
     C     XXLOSS        IFEQ      'Y'
     C                   MOVE      'L'           Q1RNB
     C                   ELSE
     C                   MOVE      'Z'           Q1RNB
     C                   ENDIF
     C*** END ADD *****  10/01/01  ************************************
     C                   CALL      'W8010R'
     C                   PARM                    Q1CO#
     C                   PARM                    Q1FND
     C                   PARM                    Q1FYR
     C                   PARM                    Q1EMP#
     C                   PARM                    Q1DIV
     C*** BEG CHG ***** 08/30/12 **************************************
     C***********        PARM                    Q1DATE
     C                   PARM                    Q2DATE
     C*** END CHG ***** 08/30/12 **************************************
     C                   PARM                    Q1ESCD
     C                   PARM                    Q1OVR
     C                   PARM                    Q1SRCD
     C                   PARM                    Q1PRT
     C                   PARM                    Q1AJCD
     C                   PARM                    Q1QTE
     C*** BEGIN CHG ***  10/01/01  ************************************
     C*********            PARM 'Z'       Q1RNB   1
     C                   PARM                    Q1RNB             1
     C*** END CHG *****  10/01/01  ************************************
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
     C*** BEGIN ADD ***  10/01/01  ************************************
     C     XXLOSS        IFEQ      'Y'
     C                   MOVE      'L'           QQREV
     C                   ELSE
     C                   MOVE      'Z'           QQREV
     C                   ENDIF
     C*** END ADD *****  10/01/01  ************************************
     C                   CALL      'W7720R'
     C                   PARM                    Q1CO#
     C                   PARM                    Q1FND
     C                   PARM                    Q1FYR
     C                   PARM                    Q1EMP#
     C                   PARM                    Q1DIV
     C*** BEG CHG ***** 08/30/12 **************************************
     C***********        PARM                    Q1DATE
     C                   PARM                    Q2DATE
     C*** END CHG ***** 08/30/12 **************************************
     C                   PARM                    Q1AR
     C*** BEGIN CHG ***  10/01/01  ************************************
     C*********            PARM 'Z'       QQREV   1
     C                   PARM                    QQREV             1
     C*** END CHG *****  10/01/01  ************************************
     C*** Begin Add *** 08/08/18 **************************************
     C                   PARM                    q1AllDiv
     C*** End   Add *** 08/08/18 **************************************
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
     C*
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
     C*********  ELKY2     CHAINPEL03                60
     C*********  ELKY3     REDPEPEL03                    60
     C*** BEGIN ADD ***  03/08/02  ************************************
     C***60****  ZEXKY     CHAINPEX01                60
     C*** END ADD *****  03/08/02  ************************************
     C*********
     C*********  *IN60     IFEQ *OFF
     C*********            MOVE 'R'       BLRFLG
     C*********            ELSE
     C*********            MOVE 'N'       BLRFLG
     C*********            ENDIF                           *IN60 EQ 0
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
     C*** BEGIN ADD ***  10/01/01  ************************************
     C     XXLOSS        IFEQ      'Y'
     C                   MOVE      *BLANK        XXLOSS
     C                   ELSE
     C                   MOVE      *ON           *IN95
     C                   ENDIF
     C*
     C                   ENDDO                                                  *IN95 DOWEQ
     C*** END ADD *****  10/01/01  ************************************
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
     C***************************************************************
     C***************************************************************
     C*                   SUBROUTINE SRLOAD
     C*   THIS SUBROUTINE WILL LOAD THE OVERRIDE FILES FOR ALL
     C*   DIVISIONS IF DISCOUNT IS SPREAD
     C***************************************************************
     C     SRLOAD        BEGSR
     C*
     C* READ ALL DIVISIONS IN WDELP
     C*
     C     ELKY1         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     ELDIV         XXDIV
     C*
     C*  CHAIN TO EMP DETAIL OVERRIDE FILE (WAELP) AND WRITE IF NO HIT
     C*
     C     ELKY2         CHAIN     PEL02                              60
     C  N60              EXCEPT    PEL02E
     C     *IN60         IFEQ      '1'
     C     ELKY          CHAIN     PEL01                              61
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
      ******* Beg add * 08/25/15  ************************************
     C                   Z-Add     ELDT          ELRDT
     C                   Z-Add     ELTIME        ELRTM
      ******* End add * 08/25/15  ************************************
     C  N61              WRITE     PEL02
     C                   ENDIF
     C*** BEGIN ADD ***  11/15/99  ************************************
     C     ELKY          CHAIN     PIL02                              60
     C     *IN60         IFEQ      '1'
     C     ELKY          CHAIN     PIL01                              61
     C     *IN61         IFEQ      '1'
     C                   Z-ADD     XXCO#         ILCO#
     C                   Z-ADD     XXFND         ILFND
     C                   Z-ADD     XXFYR         ILFYR
     C                   Z-ADD     XXEMP#        ILEMP#
     C                   Z-ADD     XXDIV         ILDIV
     C                   Z-ADD     0             ILEIN1
     C                   Z-ADD     0             ILAIN1
     C                   Z-ADD     0             ILEIN2
     C                   Z-ADD     0             ILAIN2
     C                   MOVE      *BLANK        ILSTAT
     C                   ENDIF                                                  *IN61 IFEQ
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        ILUSER
     C                   MOVEL     Q1PGM         ILPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         ILDT
     C                   Z-ADD     DSTIME        ILTIME
     C                   WRITE     PIL02
     C                   ENDIF
     C*
     C     ELKY          CHAIN     PDS02                              60
     C     *IN60         IFEQ      '1'
     C     ELKY          CHAIN     PDS01                              61
     C     *IN61         IFEQ      '1'
     C                   Z-ADD     XXCO#         DSCO#
     C                   Z-ADD     XXFND         DSFND
     C                   Z-ADD     XXFYR         DSFYR
     C                   Z-ADD     XXEMP#        DSEMP#
     C                   Z-ADD     XXDIV         DSDIV
     C                   Z-ADD     ELSTPR        DSSTPR
     C                   Z-ADD     0             DSEDIS
     C                   Z-ADD     0             DSTEPR
     C                   Z-ADD     0             DSTEDS
     C                   Z-ADD     0             DSEPCT
     C                   Z-ADD     0             DSANSP
     C                   Z-ADD     0             DSACSP
     C                   Z-ADD     0             DSADIS
     C                   Z-ADD     0             DSTANP
     C                   Z-ADD     0             DSTACP
     C                   Z-ADD     0             DSAPCT
     C                   MOVE      *BLANK        DSSTAT
     C                   ENDIF                                                  *IN61 IFEQ
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        DSUSER
     C                   MOVEL     Q1PGM         DSPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         DSDT
     C                   Z-ADD     DSTIME        DSTIME
     C                   WRITE     PDS02
     C                   ENDIF
     C*
     C     ELKY          CHAIN     PMP02                              60
     C     *IN60         IFEQ      '1'
     C     ELKY          CHAIN     PMP01                              61
     C  N61              EXCEPT    PMP01E
     C     *IN61         IFEQ      '1'
     C                   Z-ADD     XXCO#         MPCO#
     C                   Z-ADD     XXFND         MPFND
     C                   Z-ADD     XXFYR         MPFYR
     C                   Z-ADD     XXEMP#        MPEMP#
     C                   Z-ADD     XXDIV         MPDIV
     C*** BEGIN ADD ***  01/28/02  ************************************
     C                   Z-ADD     ELEDC1        MPEDC1
     C                   Z-ADD     ELEDC2        MPEDC2
     C                   Z-ADD     ELEDC3        MPEDC3
     C                   Z-ADD     ELEDCT        MPEDCT
     C***  BEG   ADD  ***  06/21/10 *************************************
     C                   Z-ADD     ELD1E2        MPD1E2
     C                   Z-ADD     ELD2E2        MPD2E2
     C***  END   ADD  ***  06/21/10 *************************************
     C***  BEGIN ADD  ***  04/07/05 *************************************
     C                   Z-ADD     ELEDD2        MPEDD2
     C***  END   ADD  ***  04/07/05 *************************************
     C***  BEGIN ADD  ***  11/10/10 *************************************
     C                   Z-ADD     ELEHCN        MPEHCN
     C***  END ADD  *****  11/10/10 *************************************
     C                   Z-ADD     ELEDCM        MPEDCM
     C                   Z-ADD     ELEDCS        MPEDCS
     C                   Z-ADD     ELEDC         MPEDC
     C                   Z-ADD     ELEDV2        MPEDV2
     C                   Z-ADD     ELEWAV        MPEWAV
     C                   Z-ADD     ELETCA        MPETCA
     C                   Z-ADD     ELPRTD        MPPRTD
     C*** BEGIN ADD ***  03/14/02  ************************************
     C                   Z-ADD     ELSTPR        MPSTPR
     C*** END ADD *****  03/14/02  ************************************
     C*** BEGIN ADD ***  01/22/08  ************************************
     C                   Z-ADD     ELMAPR        MPMAPR
     C                   Z-ADD     ELENMP        MPENMP
     C*** END ADD *****  01/22/08  ************************************
     C*** END ADD *****  01/28/02  ************************************
     C                   Z-ADD     ELANPR        MPANPR
     C                   Z-ADD     ELAAPR        MPAAPR
     C*** BEGIN ADD ***  10/01/01  ************************************
     C                   Z-ADD     ELANPR        MPANPL
     C                   Z-ADD     ELAAPR        MPAAPL
     C                   SUB       ELPRTD        MPANPL
     C                   SUB       ELPRTD        MPAAPL
     C                   SUB       ELLOSS        MPANPL
     C                   SUB       ELLOSS        MPAAPL
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        MPST
     C                   ELSE
     C                   MOVE      *BLANKS       MPST
     C                   ENDIF
     C*** END ADD *****  10/01/01  ************************************
     C                   MOVE      *BLANK        MPSTAT
     C                   ENDIF                                                  *IN61 IFEQ
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        MPUSER
     C                   MOVEL     Q1PGM         MPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         MPDT
     C                   Z-ADD     DSTIME        MPTIME
     C                   WRITE     PMP02
     C                   ENDIF
     C*** END ADD *****  11/15/99  ************************************
     C*** BEGIN ADD ***  10/17/06  ************************************
     C*
     C     ELKY          CHAIN     PPP02                              60
     C     *IN60         IFEQ      '1'
     C*
     C     ELKY          CHAIN(N)  PPP01                              61
     C     *IN61         DOWEQ     *OFF
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        PPUSER
     C                   MOVEL     Q1PGM         PPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         PPDT
     C                   Z-ADD     DSTIME        PPTIME
     C                   WRITE     PPP02
     C     ELKY          READE(N)  PPP01                                  61
     C                   ENDDO
     C*
     C                   ENDIF                                                  *IN60 IEFQ
     C*** END ADD *****  10/17/06  ************************************
     C*
     C*  CHAIN TO EMP PAYROLL OVERRIDE FILE (WAEPP) AND WRITE IF NO HIT
     C*
     C     EPKY1         CHAIN     PEP02                              60
     C  N60              EXCEPT    PEP02E
     C     *IN60         IFEQ      '1'
     C     EPKY1         CHAIN     PEP01                              61
     C     *IN61         DOWEQ     '0'
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EPDT
     C                   Z-ADD     DSTIME        EPTIME
     C  N61              WRITE     PEP02
     C     EPKY1         READE     PEP01                                  61
     C                   ENDDO
     C                   ENDIF
     C***  Begin Add  ***  11/10/10  **************************************
     C*
     C*  Chain to HCN Program File (WDHCLR) and write, if NO hit
     C*
     C     HCKY1         CHAIN(N)  PHC0R                              60
     C     *IN60         IFEQ      *ON
     C     HCKY1         CHAIN     PHC00                              61
     C     *IN61         DOWEQ     '0'
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        HCUSER
     C                   MOVEL     Q1PGM         HCPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         HCDT
     C                   Z-ADD     DSTIME        HCTIME
     C  N61              WRITE     PHC0R
     C     HCKY1         READE     PHC00                                  61
     C                   ENDDO
     C                   ENDIF
     C***  Begin Add  ***  11/10/10  **************************************
     C*** BEG ADD ***** 08/30/12 ******************************************
     C*
     C*  CHAIN TO LARGE DEDUCTIBLE CHG FILE AND WRITE IT IF NO HIT
     C*
     C*** BEG ADD ***** 05/29/13 ******************************************
      /Free
       xsKey = 'WC';
       Chain xsKey SWC01;

       noLDModule = '0';
       If WCFL43 <> 'Y';
          noLDModule = '1';
       Endif;

       If noLDModule = '0';

      /End-Free
     C*** End ADD ***** 05/29/13 ******************************************
     C     ELKY2         CHAIN(N)  PLD01X                             60
     C     *IN60         IFEQ      *ON
     C     ELKY2         CHAIN     PLD01                              61
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        LDUSER
     C                   MOVEL     Q1PGM         LDPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         LDDT
     C                   Z-ADD     DSTIME        LDTIME
     C*** beg chg ***** 09/03/14 ******************************************
     C******IN61         IFEQ      *ON
     C     *IN61         IFEQ      *OFF
     C*** end chg ***** 09/03/14 ******************************************
     C                   Z-ADD     XXCO#         LDCO#
     C                   Z-ADD     XXFND         LDFND
     C                   Z-ADD     XXEMP#        LDEMP#
     C                   Z-ADD     XXDIV         LDDIV
     C                   Z-ADD     XXFYR         LDFYR
     C                   MOVEL     LDUSER        LDIUSR
     C                   MOVEL     LDPGM         LDIPGM
     C                   Z-ADD     LDTIME        LDITIM
     C                   Z-ADD     LDDT          LDIDT
     C*** beg chg ***** 09/03/14 ******************************************
     C***********        ENDIF
     C***********        WRITE     PLD01X
     C                   WRITE     PLD01X
     C                   ENDIF
     C*** end chg ***** 09/03/14 ******************************************
     C                   ENDIF
     C*** BEG ADD ***** 05/29/13 ******************************************
      /Free
       Endif;
      /End-Free
     C*** END ADD ***** 05/29/13 ******************************************
     C*** END ADD ***** 08/30/12 ******************************************
     C*
     C*  READ NEXT DIVISION
     C*
     C     ELKY1         READE     PEL01                                  60
     C                   ENDDO
     C*** BEGIN ADD ***  05/26/06  ************************************
     C*
     C* IF DISCOUNT IS SPREAD ACROSS ENTIRE GROUP, LOAD ENTIRE GROUP
     C* INTO WADSP.
     C*
     C     FNPEOF        IFEQ      'M'
     C     FNPEOA        ANDEQ     'Y'
     C*** BEGIN ADD *** 08/22/07 **************************************
     C     FNPEOF        OREQ      'Y'
     C*** END ADD ***** 08/22/07 **************************************
     C*** BEGIN ADD *** 07/17/17 **************************************
     C     FNPEOF        OREQ      'C'
     C     FNPEOA        ANDEQ     'Y'
     C*** END ADD ***** 07/17/17 **************************************
     C*
     C     ELKY4         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     DSKY          CHAIN     PDS02                              60
     C     *IN60         IFEQ      '1'
     C     DSKY          CHAIN     PDS01                              61
     C     *IN61         IFEQ      '1'
     C                   Z-ADD     XXCO#         DSCO#
     C                   Z-ADD     XXFND         DSFND
     C                   Z-ADD     XXFYR         DSFYR
     C                   Z-ADD     ELEMP#        DSEMP#
     C                   Z-ADD     ELDIV         DSDIV
     C                   Z-ADD     ELSTPR        DSSTPR
     C                   Z-ADD     0             DSEDIS
     C                   Z-ADD     0             DSTEPR
     C                   Z-ADD     0             DSTEDS
     C                   Z-ADD     0             DSEPCT
     C                   Z-ADD     0             DSANSP
     C                   Z-ADD     0             DSACSP
     C                   Z-ADD     0             DSADIS
     C                   Z-ADD     0             DSTANP
     C                   Z-ADD     0             DSTACP
     C                   Z-ADD     0             DSAPCT
     C                   MOVE      *BLANK        DSSTAT
     C                   ENDIF                                                  *IN61 IFEQ
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        DSUSER
     C                   MOVEL     Q1PGM         DSPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         DSDT
     C                   Z-ADD     DSTIME        DSTIME
     C                   WRITE     PDS02
     C                   ENDIF
     C*
     C     ELKY4         READE     PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  FNPEOF IFEQ
     C*** END ADD *****  05/26/06  ************************************
     C*
     C                   ENDSR
     C*****************************************************************
     C*
     C* SRRET - RETURN PREMIUM
     C*
     C*****************************************************************
     C*
     C     SRRET         BEGSR
     C*
     C     ELKY3         CHAIN     PEM01                              60
     C     EMSTRP        IFNE      'Y'
     C*
     C     ELKY1         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     *OFF
     C     QQCALL        ANDNE     'Y'
     C*
     C*** BEGIN ADD ***  09/18/07  ************************************
     C     EMKY          CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     EMSTRP        ANDEQ     'Y'
     C*
     C*** END ADD *****  09/18/07  ************************************
     C     ELEMNP        IFEQ      'Y'
     C     QQTYPE        ANDEQ     'E'
     C     ELAMNP        OREQ      'Y'
     C     QQTYPE        ANDEQ     'A'
     C                   MOVE      'Y'           QQCALL
     C                   ENDIF                                                  ELEMNP IFEQ
     C*** BEGIN ADD ***  09/18/07  ************************************
     C                   ENDIF                                                  *IN60 IFEQ
     C*** END ADD *****  09/18/07  ************************************
     C*
     C     ELKY1         READE     PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  EMSTRP IFNE
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*** BEGIN ADD ***  10/01/01  ************************************
     C*****************************************************************
     C*
     C* CHECK IF POLICY NEEDS TO RUN CALCULATIONS TWICE.
     C*
     C*****************************************************************
     C*
     C     CHKLC         BEGSR
     C*
     C     ELKY1         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     SCKY          SETGT     PSC01
     C     SCKY1         READPE    PSC01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     SCPRBS        ANDGT     *ZERO
     C*
     C                   MOVE      'Y'           XXLOSS
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     ELKY1         READE     PEL01                                  60
     C*
     C                   ENDDO
     C*
     C                   ENDSR                                                  CHKLC SUBR
     C*
     C*****************************************************************
     C*** END ADD *****  10/01/01  ************************************
     C*****************************************************************
     OPEL02     E            PEL02E
     OPEP02     E            PEP02E
     O*** BEGIN ADD ***  11/15/99  ************************************
     OPMP01     E            PMP01E
     O*** END ADD *****  11/15/99  ************************************
