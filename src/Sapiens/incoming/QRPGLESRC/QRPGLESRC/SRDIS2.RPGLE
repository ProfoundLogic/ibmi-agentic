     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE SRDIS2 - DISCOUNT SPREAD ON ALL DIVISIONS
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                       *
     H**                                                              *
     H**      THIS PROGRAM IS CALLED BY VARIOUS BILLING PROGRAMS IN   *
     H**      PREPARATION OF DISCOUNT SPREADING                       *
     H**      SUBROUTINE PARM:  '1' ONLY CHECK FOR SPREAD             *
     H**                        '2' DISCOUNT SET UP                   *
     H**                        '3' REVISED BILL OVERRIDE LOAD        *
     H**                                                              *
     H**      RETURN CODE:      'Y' MULT DIV EXIST AND DISCOUNT IS    *
     H**                            SPREAD.                           *
     H**                        'N' NO DISCOUNT SPREAD                *
     H**                                                              *
     H**                                                              *
     H* CALLED FROM:                                                  *
     H*   W8002C  - ORIGINAL BILL SUBMISSION      SBRT = 2            *
     H*   W7710C  - FINAL AUDIT BILL SUBMISSION   SBRT = 2            *
     H*   W0090R  - REVISED BILLING CALC          SBRT = 3            *
     H* CALLS TO:                                                     *
     H*                                                               *
     H*                                                               *
     H* PROGRAMMER: JACKIE JANOWIAK                                   *
     H* CREATED:    01/19/92            PROJECT: WC111                *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H* 61  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/27/25 C:300 Support: 118149 (SUNZ)                         *
     H*                When Runnig Estimated Bill, Do not update      *
     H*                Billing Status (ELFAF) for Div 0 in W8010R.    *
     H*                Fix - Add QQDIV Entry parameter and call W8010R*
     H*                for the passed division, not for Div 0.        *
     H*                                                               *
     H* 04/22/19 C:65  PROJECT: WC2610                                *
     H*                Perform calculations if gradient scoring. To   *
     H*                accomplish accept quote parameter from CL.     *
     H*                                                               *
     H* 08/08/18 C:62  Project: 150WCS                                *
     H*                Add All division processing.                   *
     H*                                                               *
     H* 08/03/17 C:65  PROJECT: 150WAR                                *
     H*                Perform calculations if % of manual premium    *
     H*                has been set in order to calculate large       *
     H*                deductible percentage for later calls to use.  *
     H*                                                               *
     H* 07/17/17 C:65  PROJECT: 150WAP                                *
     H*                ADD CODING TO RECOGNIZE THE 'C' VALUE FOR PEO. *
     H*                                                               *
     H* 07/29/15 C:65  Support: Phase#36318 (CareWest)                *
     H*                Update activiation group from QILE to *CALLER  *
     H*                as this program was not picking up file        *
     H*                overrides from the revised bill.               *
     H*                                                               *
     H* 08/30/12 C:136 SUPPORT: WA025 80122                           *
     H*               -POPULATE WALDP (LARGE DEDUCTIBLE) IN           *
     H*                REVISED BILLING                                *
     H*                                                               *
     H* 03/27/11 C:255 SUPPORT 71062  WA099                           *
     H*                ADDED CHANGE TO POPULATE BLRFLG(WABLP) WITH    *
     H*                NW/RNW FLAG VALUE.                             *
     H*                                                               *
     H* 12/31/10 C:255 PROJECT: WC1918                                *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP FOR POLICY PAGE, ITEM 3C.       *
     H*                                                               *
     H* 11/10/10 C:33  Project: 139WPL                                *
     H*                Add HCN Program file.                          *
     H*                                                               *
     H* 07/28/10 C:255 PROJECT: 132WOJ                                *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELDS IN WDELP.                               *
     H*                                                               *
     H* 06/21/10 C:255 PROJECT: 114WVY                                *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP.                                *
     H*                                                               *
     H* 01/04/10 C:190 PROJECT: 25WRE                                 *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP.                                *
     H*                                                               *
     H* 09/22/09 C:234 PROJECT: 93WIB                                 *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP.                                *
     H*                                                               *
     H* 02/28/08 C:136 SUPPORT: WA132 62410                           *
     H*                SET THE SPREAD DISC FLAG IF MULTIPLE DIVS      *
     H*                EXIST AND ONE OF THE DIVISIONS HAS A DISC      *
     H*                CODE.                                          *
     H*                                                               *
     H* 10/11/07 C:183 PROJECT: 146WBC                                *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP.                                *
     H*                                                               *
     H* 08/22/07 C:65  PROJECT: 114WMF                                *
     H*                ADD CODING TO RECOGNIZE THE 'Y' VALUE FOR PEO  *
     H*                FLAG FOR CSI FOR SPREAD DISCOUNT.              *
     H*                                                               *
     H*  03/16/06 C:65 PROJECT: WC1411                                *
     H*                RUN THE CALCULATION FOR SPREAD DISCOUNT IF     *
     H*                THE GROUP IS A MULTIPLE PEO GROUP AND SET TO   *
     H*                SPREAD DISCOUNT FOR ALL EMPLOYERS.             *
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
     H* 12/13/01 C:65 PROJECT: SUPPORT#30217                          *
     H*               ONLY CLEAR THE DISCOUNT FILE FOR ESTIMATED      *
     H*               BILLS.  AUDIT BILLS NEED TO RETAIN EST INFO.    *
     H*                                                               *
     H* 11/27/01 C:65 PROJECT: WC1059                                 *
     H*               RUN THE CALCULATIONS FOR ANYONE WITH A REDUCED  *
     H*               EXPENSE CONSTANT AS WELL AS STOCK DISCOUNT.     *
     H*                                                               *
     H* 06/08/01 C:65 PROJECT: SUPPORT#27692                          *
     H*               CLEAR OUT THE DISCOUNT FILE FOR EACH EMPLOYER   *
     H*               BEFORE CALCULATING RECORDS.                     *
     H*                                                               *
     H* 01/30/01 C:65 PROJECT: SUPPORT#25641                          *
     H*               FIND FIRST EXISTING DIVISION RATHER THAN DIV 0  *
     H*               FOR SPREADING DISC.                             *
     H*                                                               *
     H* 12/07/00 C:65 PROJECT: SUPPORT#25300                          *
     H*               PROCESS A EMP#/FYR CHECK INSTEAD OF DOING A     *
     H*               SETGT IN ORDER TO AVOID SKIPPING RECORDS IN     *
     H*               WABLP.                                          *
     H*                                                               *
     H* 11/15/99 C:65 PROJECT: WC936                                  *
     H*               SEND A UNIQUE VALUE TO EACH CALC PROGRAM TO     *
     H*               REDUCE REDUNDENT CALCULATIONS.                  *
     H*                                                               *
     H* 06/10/98 C:65 PROJECT: WC705                                  *
     H*               FIX QQEMP# CHECKS TO SVEMP#.                    *
     H*                                                               *
     H* 02/02/98 C:65 PROJECT: WC705                                  *
     H*               CHANGED Q1DIV FROM 3 TO 5.                      *
     H*                                                               *
     H* 11/25/97 C:65 PROJECT: V3                                     *
     H*               ADDED REVISED PARM TO CALL TO W7720R.           *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SSY01)
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
     FWMAHL     IF   E           K DISK
     F*** BEG   DEL ***  03/27/11  ************************************
     F*******WDELL   IF  E           K        DISK
     F*******     PEL01                             KRENAMEPEL03
     F*** END   DEL ***  03/27/11  ************************************
     FWDELL1    IF   E           K DISK
     FWAELL     UF A E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWMEPL1    IF   E           K DISK
     FWAEPL1    UF A E           K DISK
     F                                     RENAME(PEP01:PEP02)
     F***  Begin Add  ***  11/10/10  **************************************
     FWDHCLR    IF   E           K DISK
     F                                     RENAME(PHC01:PHC0R)
     FWDHCL3    UF A E           K DISK
     F                                     RENAME(PHC01:PHC03)
     F***  End   Add  ***  11/10/10  **************************************
     FWDDSL     UF A E           K DISK
     FWADSL     UF A E           K DISK
     F                                     RENAME(PDS01:PDS02)
     F*** BEGIN ADD ***  03/08/02  ************************************
     F*** BEG   DEL ***  03/27/11  ************************************
     F*******WZEXL   IF  E           K        DISK
     F*** END   DEL ***  03/27/11  ************************************
     F*** END ADD *****  03/08/02  ************************************
     F*** BEGIN ADD ***  03/16/06  ************************************
     FWMFNL     IF   E           K DISK
     F*** END ADD *****  03/16/06  ************************************
     F*** BEG ADD ***** 08/30/12 **************************************
     FWMLDL1    IF   E           K DISK
     FWALDL1    IF A E           K DISK
     F                                      RENAME(PLD01:PLD01X)
     F*** END ADD ***** 08/30/12 **************************************

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     d*** Begin Add *** 08/08/18 *************************************
     D  qqallDiv       s              1a
     D  q1allDiv       s              1a   Inz(' ')
     d*** End   Add *** 08/08/18 *************************************

     C************************************************************
     C*
     C*  WORK COMP CONTROL FILE
     C*
     C     WCKY          KLIST
     C                   KFLD                    XCKEY
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
     C*** BEGIN CHG ***  06/20/05  ************************************
     C*********            KFLD           XXRFLG
     C                   KFLD                    XXRNB
     C*** END CHG *****  06/20/05  ************************************
     C*
     C     BLKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C     BLKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** BEGIN CHG ***  06/20/05  ************************************
     C*********            KFLD           XXRFLG
     C                   KFLD                    XXRNB
     C*** END CHG *****  06/20/05  ************************************
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
     C*  Copy HCN Program file
     C*
     C     HCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C***  End   Add  ***  11/10/10  **************************************
     C*
     C*  EMPLOYER DISCOUNT FILE
     C*
     C     DSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     DSKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     DSKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    X1DIV
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
     C                   PARM                    QQSBRT            1
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQSPRD            1
     C                   PARM                    QQRNB             1
     C*** Begin Add *** 08/08/18 ************************************
     C                   PARM                    qqAllDiv
     C*** End   Add *** 08/08/18 ************************************
     C*** begin add *** 04/22/19 ************************************
     C                   PARM                    QQQTE             1
     C*** end add ***** 04/22/19 ************************************
     C*** Begin Add *** 03/27/25 ************************************
     C                   PARM                    QQDIV             5
     C*** End   Add *** 03/27/25 ************************************
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
     C     *LIKE         DEFINE    WCKEY         XCKEY
     C     *LIKE         DEFINE    QQCO#         Q1CO#
     C     *LIKE         DEFINE    QQFND         Q1FND
     C     *LIKE         DEFINE    QQFYR         Q1FYR
     C*** BEGIN ADD ***  06/10/98  ************************************
     C     *LIKE         DEFINE    ELEMP#        SVEMP#
     C*** END ADD *****  06/10/98  ************************************
     C     *LIKE         DEFINE    QQEMP#        Q1EMP#
     C     *LIKE         DEFINE    ELDIV         X1DIV
     C*
     C*    SET UP AND INITALIZE
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEMP#        XXEMP#
     C*** BEGIN ADD ***  06/10/98  ************************************
     C                   MOVE      QQEMP#        SVEMP#
     C*** END ADD *****  06/10/98  ************************************
     C*** Begin Add *** 03/27/25 ************************************
     C                   MOVE      QQDIV         XXDIV
     C*** End   Add *** 03/27/25 ************************************
     C*** BEGIN ADD ***  06/20/05  ************************************
     C     QQRNB         IFEQ      '#'
     C                   MOVE      *BLANK        XXRNB
     C                   MOVE      *ON           *IN71
     C                   ELSE
     C                   MOVE      *OFF          *IN71
     C                   MOVE      QQRNB         XXRNB             1
     C                   ENDIF
     C*** END ADD *****  06/20/05  ************************************
     C*** Begin Add *** 08/08/18 ************************************
     C*
     C* Set All division indicator
     C                   If        %Parms() > 8
     C                   Eval      q1AllDiv = qqAllDiv
     C                   EndIf
     C*** End   Add *** 08/08/18 ************************************
     C*** begin add *** 04/22/19 ************************************
     C*
     C* Set quote mode if passed in.
     C                   If        %Parms() > 9
     C                   Move      QQQte         xxQte             1
     C                   Else
     C                   Clear                   xxQte
     C                   EndIf
     C*** end add ***** 04/22/19 ************************************
     C                   MOVE      'N'           QQSPRD
     C                   TIME                    DSDTTM
     C*
     C*  GET SYSTEM VALUE FOR SPREAD DISCOUNT FLAG & CONTINUE IF 'Y'
     C*
     C                   MOVE      *BLANKS       XCKEY
     C                   MOVEL     'WC'          XCKEY
     C     WCKY          CHAIN     SWC01                              60
     C     *IN60         IFEQ      '0'
     C*** BEGIN DEL ***  11/27/01  ************************************
     C*********  WCFL02    ANDEQ'Y'
     C*** END DEL *****  11/27/01  ************************************
     C*
     C*  IF SUBROUTINE '1', WE ARE ONLY CHECKING FOR THE SPREAD
     C*
     C     QQSBRT        IFEQ      '1'
     C                   EXSR      SRCHK
     C                   END
     C*
     C*  IF SUBROUTINE '2', NEED TO CALL W8010R OR W7720R FOR
     C*  EMPLOYERS SPREADING DISCOUNTS.
     C*
     C     QQSBRT        IFEQ      '2'
     C                   EXSR      SRCALL
     C                   END
     C*
     C*  IF SUBROUTINE '3', NEED TO CALL LOAD OVERRIDE FILES FOR
     C*  EACH DIVISION FOR EMPLOYERS SPREADING DISCOUNT.
     C*
     C     QQSBRT        IFEQ      '3'
     C                   EXSR      SRLOAD
     C                   END
     C*
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C***************************************************************
     C*                   SUBROUTINE SRCHK
     C*   THIS SUBROUTINE WILL DETERMINE IF THE DISCOUNT IS TO BE
     C*   SPREAD.   QQSPRD = 'Y' OR 'N'
     C***************************************************************
     C     SRCHK         BEGSR
      *** BEG CHG ***** 02/28/08 **************************************
     C**********
     C***GET*DISCOUNT VALUE FOR THIS EMPLR, DIV = 0
     C**********
     C**********           Z-ADD0         XXDIV
     C****BEGIN*CHG ***  01/30/01  ************************************
     C********** ELKY      CHAINPEL01                60
     C********** ELKY1     CHAINPEL01                60
     C****END*CHG *****  01/30/01  ************************************
     C********** *IN60     IFEQ '0'
     C********** ELDISC    ANDNE*BLANK
     C**********
     C***SEE*IF*ANOTHER DIVISION EXISTS
     C**********
     C********** ELKY1     READEPEL01                    60
     C********** *IN60     IFEQ '0'
     C****BEGIN*ADD ***  11/27/01  ************************************
     C********** WCFL02    ANDEQ'Y'
     C****END*ADD *****  11/27/01  ************************************
     C**********           MOVE 'Y'       QQSPRD
     C**********           END
     C**********
     C**********           END
     C*
     C*  SET SPREAD DISCOUNT FLAG IF ANY ONE OF THE DIVISIONS HAS A DISCOUNT
     C*  CODE AND THE SYSTEM FLAG IS SET TO SPREAD DISCOUNTS
     C*
     C                   Z-ADD     1             CNT               7 0
     C                   MOVE      *OFF          SDFL              1
     C*** begin add ***  04/22/19  *****************************************
     C                   Clear                   xxGrad            1
     C*** end add *****  04/22/19  *****************************************
     C     ELKY1         SETLL     PEL01
     C     ELKY1         READE     PEL01                                  60
     C     *IN60         DOWEQ     *OFF
     C                   ADD       1             CNT
     C     ELDISC        IFNE      *BLANK
     C     WCFL02        ANDEQ     'Y'
     C                   MOVE      *ON           XXSDFL            1
     C                   ENDIF
     C     XXSDFL        IFEQ      *ON
     C     CNT           ANDGT     1
     C                   MOVE      'Y'           QQSPRD
     C                   LEAVE
     C                   ENDIF
     C*** begin add ***  08/03/17  *****************************************
     C                   If        ELMANP <> 0
     C                   MOVE      'Y'           QQSPRD
     C                   LEAVE
     C                   Endif
     C*** end add *****  08/03/17  *****************************************
     C*** begin add ***  04/22/19  *****************************************
     C                   If        ELScore <> 0 and ELScoreBp <> 'Y'
     C                             and xxQte = 'Q'
     C                   MOVE      'Y'           QQSPRD
     C                   MOVE      'Y'           xxGrad
     C                   LEAVE
     C                   Endif
     C*** end add *****  04/22/19  *****************************************
     C     ELKY1         READE     PEL01                                  60
     C                   ENDDO
      *** END CHG ***** 02/28/08 **************************************
     C*
     C                   ENDSR
     C***************************************************************
     C***************************************************************
     C*                   SUBROUTINE SRCALL
     C**                                                              *
     C**      THIS SUBRTN  IS CALLED BEFORE THE EXECUTION OF ORIGINAL *
     C**      AND AUDIT BILLS IN ORDER TO PREPARE BILLINGS FOR        *
     C**      SPREADING THE DISCOUNT BASED ON THE STD PREM OF ALL     *
     C**      DIVISIONS.                                              *
     C**                                                              *
     C**      IF THE SYSTEM FLAG TELLS US TO SPREAD THE DISCOUNT AND  *
     C**      DISCOUNT TABLE IS USED, WE NEED TO ACCUMULATE THE STD   *
     C**      PREM OF ALL DIVISIONS AND PRORATE THE DISCOUNT BASED    *
     C**      ON EACH DIVISIONS SHARE OF THE TOTAL STD PREMIUM.       *
     C**                                                              *
     C**      THEREFORE, BEFORE EACH BILL IS CALCULATED, WE NEED TO   *
     C**      KNOW THE STANDARD PREMIUM OF EACH DIVSION.              *
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
     C* IF EMPLR = 0, NEED TO READ THE BILLING WORK FILE, WHICH IS
     C* OVERRIDDEN BY WXBLP
     C*
     C*** BEGIN ADD ***  06/10/98  ************************************
     C*****      QQEMP#    CABNE'00000'   READEM
     C     SVEMP#        CABNE     0             READEM
     C*** END ADD *****  06/10/98  ************************************
     C*
     C*** BEGIN CHG ***  06/20/05  ************************************
     C*********            MOVE QQRNB     XXRFLG  1
     C*********  QQRNB     IFEQ 'B'
     C     XXRNB         IFEQ      'B'
     C*** END CHG *****  06/20/05  ************************************
     C     BLKY          SETLL     PBL01
     C                   ELSE
     C     BLKY2         SETLL     PBL02
     C                   ENDIF                                                  QQRNB EQ B
     C*
     C     READE         TAG
     C*
     C*** BEGIN CHG ***  06/20/05  ************************************
     C*********  QQRNB     IFEQ 'B'
     C     XXRNB         IFEQ      'B'
     C*** END CHG *****  06/20/05  ************************************
     C     BLKY          READE     PBL01                                  60
     C                   ELSE
     C     BLKY2         READE     PBL02                                  60
     C                   ENDIF                                                  QQRNB EQ B
     C     *IN60         CABEQ     '1'           END
     C*
     C*  CHECK THE CORRECT FIELDS DEPENDING ON TYPE OF BILL BEING
     C*  CALLED
     C*
     C     QQTYPE        IFEQ      'E'
     C     BLFLG1        IFEQ      ' '
     C     BLFLG3        ANDEQ     ' '
     C                   GOTO      READE
     C                   END
     C                   ELSE
     C     BLFLG2        CABNE     'Y'           READE
     C                   END
     C*
     C*** BEGIN ADD ***  12/07/00  ************************************
     C     XXEMP#        IFNE      BLEMP#
     C     XXFYR         ORNE      BLFYR
     C*
     C*** END ADD *****  12/07/00  ************************************
     C                   Z-ADD     BLEMP#        XXEMP#
     C                   Z-ADD     BLFYR         XXFYR
     C*** BEG ADD *****  02/15/06  ************************************
     C                   MOVE      BLFLG3        SVFLG3            1
     C*** END ADD *****  02/15/06  ************************************
     C*
     C*  READ THE EMPLOYER RECORDS
     C*
     C     READEM        TAG
     C*** BEGIN ADD ***  06/08/01  ************************************
     C*
     C* CLEAR DISCOUNT FILE FIRST
     C*
     C*** BEGIN ADD ***  12/13/01  ************************************
     C     QQTYPE        IFEQ      'E'
     C*** END ADD *****  12/13/01  ************************************
     C*** BEGIN CHG ***  06/20/05  ************************************
     C*********  ELKY1     CHAINPDS01                60
     C  N71ELKY1         CHAIN     PDS01                              60
     C   71ELKY          CHAIN     PDS01                              60
     C*** END CHG *****  06/20/05  ************************************
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PDS01
     C*** BEGIN CHG ***  06/20/05  ************************************
     C*********  ELKY1     READEPDS01                    60
     C  N71ELKY1         READE     PDS01                                  60
     C   71ELKY          READE     PDS01                                  60
     C*** END CHG *****  06/20/05  ************************************
     C                   ENDDO
     C*** BEGIN ADD ***  12/13/01  ************************************
     C                   ENDIF
     C*** END ADD *****  12/13/01  ************************************
     C*** END ADD *****  06/08/01  ************************************
     C*
     C* CHECK FOR DISCOUNT SPREAD & CONTINUE IF 'Y'
     C*
     C                   EXSR      SRCHK
     C*** BEGIN ADD ***  11/27/01  ************************************
     C                   EXSR      SREXCO
     C*** END ADD *****  11/27/01  ************************************
     C*** BEGIN ADD ***  03/16/06  ************************************
     C     BLKY          CHAIN     PFN01                              60
     C     FNPEOF        IFEQ      'M'
     C     FNPEOA        ANDEQ     'Y'
     C     *IN60         ANDEQ     *OFF
     C*** BEGIN ADD *** 08/22/07 **************************************
     C     FNPEOF        OREQ      'Y'
     C     *IN60         ANDEQ     *OFF
     C*** END ADD ***** 08/22/07 **************************************
     C*** BEGIN ADD *** 07/17/17 **************************************
     C     FNPEOF        OREQ      'C'
     C     FNPEOA        ANDEQ     'Y'
     C     *IN60         ANDEQ     *OFF
     C*** END ADD ***** 07/17/17 **************************************
     C                   MOVE      'Y'           QQSPRD
     C                   ENDIF
     C*** END ADD *****  03/16/06  ************************************
     C*
     C     QQSPRD        IFEQ      'Y'
     C*
     C* IF REVISED BILL OVERRIDE EXISTS, LOAD WADSP TO WDDSP INSTEAD
     C* OF CALLING W8010R TO CALC
     C*
     C     DSKY1         CHAIN     PDS02                              60
     C     *IN60         IFEQ      '0'
     C     QQTYPE        ANDEQ     'E'
     C*
     C     *IN60         DOWEQ     '0'
     C                   EXCEPT    PDS02E
     C                   Z-ADD     DSDIV         X1DIV
     C     DSKY2         CHAIN     PDS01                              61        WDDSL
     C     DSKY2         CHAIN     PDS02                              60        WADSL
     C                   EXCEPT    PDS02E
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        DSUSER
     C                   MOVEL     Q1PGM         DSPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DSDT
     C                   Z-ADD     DSTIME        DSTIME
     C  N61              UPDATE    PDS01
     C   61              WRITE     PDS01
     C     DSKY1         READE     PDS02                                  60
     C                   END                                                    *IN60
     C*
     C                   ELSE
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
     C*** BEG CHG ***** 08/30/12 **************************************
     C***********        MOVEL     SRDTO         Q1DATE            8
     C                   MOVEL     SRDTO         Q2DATE            8
     C*** END CHG ***** 08/30/12 **************************************
     C*** END CHG *****  03/03/03  ************************************
     C                   MOVE      *BLANKS       Q1ESCD            2
     C                   MOVE      *BLANKS       Q1OVR             1
     C                   MOVE      *BLANKS       Q1SRCD            2
     C                   MOVE      *BLANKS       Q1PRT             1
     C                   MOVE      *BLANKS       Q1AJCD            2
     C                   MOVE      'W'           Q1QTE             1
     C*** begin add *** 04/22/19 ************************************
     C                   If        xxQte = 'Q'
     C                   Move      xxQte         Q1QTE
     C                   Endif
     C*** end add ***** 04/22/19 ************************************
     C                   MOVE      *BLANKS       Q1AR              2
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
     C                   MOVE      'Y'           XXPROC            1            JJJ 8/11/92
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
     C*** BEGIN CHG ***  11/15/99  ************************************
     C************         PARM           QQRNB
     C                   PARM      'S'           Q1RNB             1
     C*** END CHG *****  11/15/99  ************************************
     C*
     C*** begin add *** 04/22/19 ************************************
     C                   If        xxGrad = 'Y'
     C                   CALL      'W8010R'
     C                   PARM                    Q1CO#
     C                   PARM                    Q1FND
     C                   PARM                    Q1FYR
     C                   PARM                    Q1EMP#
     C                   PARM                    Q1DIV
     C                   PARM                    Q2DATE
     C                   PARM                    Q1ESCD
     C                   PARM                    Q1OVR
     C                   PARM                    Q1SRCD
     C                   PARM                    Q1PRT
     C                   PARM                    Q1AJCD
     C                   PARM                    Q1QTE
     C                   PARM      'S'           Q1RNB             1
     C                   Endif
     C*** end add ***** 04/22/19 ************************************
     C                   ELSE
     C                   MOVE      'N'           XXPROC
     C                   END
     C*
     C                   ELSE
     C*** BEGIN ADD *** JJJ *** 8/11/92
     C*
     C* IF RUNNING AUDITS FOR ALL EMPLOYERS, DO NOT PROCESS DIVISIONS
     C* WHICH DONT HAVE AN ENTRY IN WABLP WITH BLFLG2 = 'Y'
     C*
     C*** BEGIN ADD ***  06/10/98  ************************************
     C*****      QQEMP#    IFEQ '00000'
     C     SVEMP#        IFEQ      0
     C*** END ADD *****  06/10/98  ************************************
     C     BLKY1         CHAIN     PBL03                              60
     C  N60              EXCEPT    PBL03E
     C     *IN60         IFEQ      '1'
     C     BLFLG2        OREQ      *BLANKS
     C                   MOVE      'N'           XXPROC
     C                   END
     C                   END
     C*
     C     XXPROC        IFEQ      'Y'
     C*
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
     C*** BEGIN ADD ***  11/25/97  ************************************
     C*** BEGIN CHG ***  11/15/99  ************************************
     C**************       PARM ' '       QQREV   1
     C                   PARM      'S'           QQREV             1
     C*** END CHG *****  11/15/99  ************************************
     d*** Begin Add *** 08/08/18 *************************************
     C                   PARM                    q1AllDiv
     d*** End   Add *** 08/08/18 *************************************
     C*** END ADD *****  11/25/97  ************************************
     C*
     C                   END                                                    JJJ 8/11/92
     C*
     C                   END
     C*
     C*  IF READING WABLP, NEED TO UPDATE FLAGS FOR SECOND PASS
     C*
     C*** BEGIN ADD ***  06/10/98  ************************************
     C*****      QQEMP#    IFEQ '00000'
     C     SVEMP#        IFEQ      0
     C*** END ADD *****  06/10/98  ************************************
     C     XXPROC        ANDEQ     'Y'                                          JJJ 8/11/92
     C*
     C     BLKY1         CHAIN     PBL03                              60
     C     QQTYPE        IFEQ      'E'
     C                   MOVE      'Y'           BLFLG1
     C                   MOVE      *BLANKS       BLFLG2
      *** BEG CHG ***** 02/15/06 **********************************
     C*****************    MOVE 'Y'       BLFLG3
     C                   MOVE      SVFLG3        BLFLG3
      *** END CHG ***** 02/15/06 **********************************
     C                   ELSE
     C                   MOVE      *BLANKS       BLFLG1
     C                   MOVE      'Y'           BLFLG2
     C                   MOVE      *BLANKS       BLFLG3
     C                   END
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
     C     *IN60         IFEQ      '1'
     C                   MOVE      XXCO#         BLCO#
     C                   MOVE      XXFND         BLFND
     C                   Z-ADD     XXFYR         BLFYR
     C                   MOVE      XXEMP#        BLEMP#
     C                   MOVE      ELDIV         BLDIV
     C                   MOVE      Q1USER        BLUSER
     C                   MOVE      Q1PGM         BLPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BLDT
     C                   Z-ADD     DSTIME        BLTIME
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
     C                   END
     C*
     C                   END
     C*
     C*  READ NEXT DIVISION
     C*
     C*** BEGIN CHG ***  06/20/05  ************************************
     C*********  ELKY1     READEPEL01                    60
     C  N71ELKY1         READE     PEL01                                  60
     C   71ELKY          READE     PEL01                                  60
     C*** END CHG *****  06/20/05  ************************************
     C                   END
     C*
     C                   END                                                    WADSP CHAIN
     C*
     C                   END                                                    Q1SPRD = Y
     C*** BEGIN ADD ***  12/07/00  ************************************
     C                   ENDIF                                                  XXEMP# IFNE
     C*** END ADD *****  12/07/00  ************************************
     C*
     C*  READ NEXT EMPLOYER, IF QQEMP = 0
     C*
     C*** BEGIN ADD ***  06/10/98  ************************************
     C*****      QQEMP#    IFEQ '00000'
     C     SVEMP#        IFEQ      0
     C*** END ADD *****  06/10/98  ************************************
     C*** BEGIN DEL ***  12/07/00  ************************************
     C********   QQRNB     IFEQ 'B'
     C********   BLKY3     SETGTPBL01
     C********             ELSE
     C********   BLKY4     SETGTPBL02
     C********             ENDIF                           QQRNB EQ B
     C*** END DEL *****  12/07/00  ************************************
     C                   GOTO      READE
     C                   END
     C*
     C     END           TAG
     C                   ENDSR
     C***************************************************************
     C***************************************************************
     C*                   SUBROUTINE SRLOAD
     C*   THIS SUBROUTINE WILL LOAD THE OVERRIDE FILES FOR ALL
     C*   DIVISIONS IF DISCOUNT IS SPREAD
     C***************************************************************
     C     SRLOAD        BEGSR
     C*
     C* CHECK FOR DISCOUNT SPREAD & CONTINUE IF 'Y'
     C*
     C                   EXSR      SRCHK
     C*** BEGIN ADD ***  11/27/01  ************************************
     C                   EXSR      SREXCO
     C*** END ADD *****  11/27/01  ************************************
     C     QQSPRD        IFEQ      'Y'
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
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C  N61              WRITE     PEL02
     C                   END
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
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C                   Z-ADD     DSTIME        EPTIME
     C  N61              WRITE     PEP02
     C     EPKY1         READE     PEP01                                  61
     C                   END
     C                   END
     C*
     C*  CHAIN TO EMP DISCNT OVERRIDE FILE (WADSP) AND WRITE IF NO HIT
     C*
     C     DSKY          CHAIN     PDS02                              60
     C  N60              EXCEPT    PDS02E
     C     *IN60         IFEQ      '1'
     C     DSKY          CHAIN     PDS01                              61
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
     C                   MOVE      *BLANKS       DSSTAT
     C                   END
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        DSUSER
     C                   MOVEL     Q1PGM         DSPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DSDT
     C                   Z-ADD     DSTIME        DSTIME
     C                   WRITE     PDS02
     C                   END
     C*
     C***  Begin Add  ***  11/10/10  **************************************
     C*  Copy HCN Program file information to temporary file
     C*
     C     HCKY          CHAIN(N)  PHC03                              60
     C     *IN60         IFEQ      *ON
     C     HCKY          CHAIN     PHC0R                              61
     C     *IN61         DOWEQ     '0'
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        HCUSER
     C                   MOVEL     Q1PGM         HCPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HCDT
     C                   Z-ADD     DSTIME        HCTIME
     C                   WRITE     PHC03
     C     HCKY          READE     PHC0R                                  61
     C                   ENDDO
     C                   ENDIF
     C***  End   Add  ***  11/10/10  **************************************
     C*** BEG ADD ***** 08/30/12 ******************************************
     C*
     C*  CHAIN TO LARGE DEDUCTIBLE CHG FILE AND WRITE IT IF NO HIT
     C*
     C     HCKY          CHAIN(N)  PLD01X                             60
     C     *IN60         IFEQ      *ON
     C     HCKY          CHAIN     PLD01                              60
     C     *IN60         IFEQ      *OFF
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        LDUSER
     C                   MOVEL     Q1PGM         LDPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LDDT
     C                   Z-ADD     DSTIME        LDTIME
     C                   WRITE     PLD01X
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*ON
     C*** END ADD ***** 08/30/12 ******************************************
     C*
     C*  READ NEXT DIVISION
     C*
     C     ELKY1         READE     PEL01                                  60
     C                   END
     C*
     C                   END                                                    QQSPRD = Y
     C*
     C                   ENDSR
     C*** BEGIN ADD ***  11/27/01  ************************************
     C     SREXCO        BEGSR
     C*
     C     QQSPRD        IFNE      'Y'
     C*
     C                   MOVE      XXCO#         Q1CO#
     C                   MOVE      XXFND         Q1FND
     C                   MOVE      XXFYR         Q1FYR
     C                   MOVE      XXEMP#        Q1EMP#
     C                   MOVE      *BLANKS       QQSPRD
     C*
     C                   CALL      'SREXCO'
     C                   PARM                    Q1CO#
     C                   PARM                    Q1FND
     C                   PARM                    Q1FYR
     C                   PARM                    Q1EMP#
     C                   PARM                    QQSPRD
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD *****  11/27/01  ************************************
     OPBL03     E            PBL03E
     OPEL02     E            PEL02E
     OPEP02     E            PEP02E
     OPDS02     E            PDS02E
