     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W8001R - PROMPT FOR PREMIUM GENERATION
     H*****************************************************************
     H* CALLED FROM:                                                  *
     H*    MENU                                                       *
     H*    W0051R - WORK WITH EMPLOYER POLICY YEARS                   *
     H* CALLS TO:                                                     *
     H*    W8001C                                                     *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/08/18 C:062 PROJECT: 150WCS                                *
     H*                Added call to WTPOLESTBL.                      *
     H*                                                               *
     H* 02/08/13 C:190 PROJECT: 140WAU                                *
     H*                Added parameter QQAR2 to call W8001C           *
     H*                                                               *
     H* 12/21/10 C:191 Support: SEQ73572                              *
     H*                Fix support code                               *
     H*                                                               *
     H* 10/22/10 C:255 Support: SEQ73572                              *
     H*                Checked object WA073R in library list instead  *
     H*                Call Stack.                                    *
     H*                                                               *
     H* 07/27/10 C:257 SUPPORT: SEQ73572
     H*                IF API WA073R EXIST IN CALL STACK THEN USE
     H*                FILE WMEFL6 INSTEAD OF WMDBL3
     H*                                                               *
     H* 04/14/10 C:235 PROJECT: 132WNU                                *
     H*                Posting date 10 days from Effective date       *
     H*                                                               *
     H* 12/02/09 C:198 Support: 70835                                 *
     H*                SCR02 should not comeup when QQCALL = ' ' or   *
     H*                bill option is = 1 (Print Only)                *
     H*                                                               *
     H* 11/05/09 C:189 Project: 71WKH                                 *
     H*                Print only bills allow SELECTION/RESOLVE       *
     H*                                                               *
     H*                                                               *
     H* 02/03/09 C:33  Project: 146WBW (3rd Party Billing)            *
     H*                Do not default DARTS print if running for 3rd  *
     H*                party billing.  Additionally, allow the entry *
     H*                of A/R codes when in quote for 3rd Party       *
     H*                Billing.                                       *
     H*                                                               *
     H* 03/28/08 C:65  PROJECT: CI019                                 *
     H*                DO NOT ALLOW AN ESTIMATED BILL IF ONLINE       *
     H*                PROCESSING AND NO VALID ACH INFORMATION.       *
     H*                                                               *
     H*  09/27/07 C:183 PROJECT: 71WIA                                *
     H*                 Added edits for new Retro Credit A/R Batch    *
     H*                 # field.                                      *
     H*                                                               *
     H*  09/06/07 C:136 SUPPORT: WA045 59923                          *
     H*                 DEFAULT 'INTERIM DAYS FOR DEPOSIT DUE' DATE   *
     H*                 FOR SELF-REPORTING  POLICIES                 *
     H*                                                               *
     H*  08/09/07  C:65  PROJECT:  SUPP# 59377                        *
     H*                  ADD "TRANSACTION IS INVALID AT THIS TIME"    *
     H*                  ERROR MESSAGE TO EMPLOYER FYR/EMP# ENTRY     *
     H*                  IN CASE BILLS ARE CALLED FROM MENU.          *
     H*                                                               *
     H*  10/11/06  C:33  Seq#: WA099/41801                            *
     H*                  Use SC305R instead of SC580R.                *
     H*                                                               *
     H*  02/27/06 C:65 PROJECT: WC1327                                *
     H*                DO NOT ALLOW SINGLE DIVISION OPTION TO BE RUN  *
     H*                IF THE POLICY HAS ALREADY RUN A BILLING.       *
     H*                                                               *
     H*  06/20/05 C:65 PROJECT: WC1327                                *
     H*                ALLOW PROGRAM TO BE CALLED IN SINGLE DIVISION  *
     H*                MODE.  THIS MODE WILL ALLOW A BILL TO BE RUN   *
     H*                ON A SINGLE DIVISION ON A POLICY.  IF CALLED   *
     H*                FOR A SINGLE DIVISION, CHECK FOR A/R IN A      *
     H*                PREVIOUS MONTH ON THE DIVISION AND ON THE      *
     H*                STATE REPORTING DIVISION AS WELL IF SUMMARIZED.*
     H*                                                               *
     H*  11/30/04 C:65 PROJECT: 132WBW                                *
     H*                USE ACCOUNT CURRENT INTERIM DAYS FOR ACCOUNT   *
     H*                CURRENT POLICIES.                              *
     H*                                                               *
     H*  01/13/04 C:136 PROJECT: WC1116                               *
     H*                 Use expiration date in Direct Debit Info      *
     H*                 instead of status to find out if policy       *
     H*                 is an active ACH account.                     *
     H*                                                               *
     H*  11/13/03 C:65 PROJECT: WC1167                                *
     H*                ADD THREE FREE FORM COMMENT FIELDS.  THESE     *
     H*                FIELDS WILL BE MUTUALLY EXCLUSIVE WITH THE     *
     H*                COMMENT CODE FIELDS.                           *
     H*                                                               *
     H* 10/10/03 C:33  PROJECT: WC1017                                *
     H*                Allow for mass run in quote.                   *
     H*                                                               *
     H*  06/27/03 C:136 PROJECT: WC577                                *
     H*                 CHECK FOR DBSTAT = 'A' WHEN VALIDATING SCDATE *
     H*                 FOR ACH EMPLOYER.                             *
     H*                                                               *
     H*  05/30/03 C:65 PROJECT: WC577                                 *
     H*                GIVE INFORMATIONAL MESSAGE IF POSTING DATE IS  *
     H*                LESS THAN CURRENT DAY AND ACH ACCOUNT.         *
     H*                                                               *
     H*  05/27/03 C:65 PROJECT: SUPPORT#37047                         *
     H*                CHANGE PARM FIELD FOR A/R SEARCH.              *
     H*                                                               *
     H*  12/21/00 C:65 PROJECT: SUPPORT#25313                         *
     H*                ALSO CHECK FOR 'H' TYPE A/R BEFORE RUNNING BILL*
     H*                                                               *
     H*  04/27/00 C:65 PROJECT: SUPPORT#22696                         *
     H*                CORRECT THE PRIOR A/R CHECK IN OTHER DIVISIONS.*
     H*                                                               *
     H*  02/23/00 C:65 PROJECT: SUPPORT                               *
     H*                ALLOW DARTS FOR ESTIMATED CALC & PRT OPTION.   *
     H*                                                               *
     H*  11/15/99 C:65 PROJECT: WC936                                 *
     H*                WHEN RUNNING A BILL FOR A SINGLE POLICY WITH   *
     H*                MULTIPLE DIVISIONS, GIVE THE MESSAGE THAT THE  *
     H*                BILL WILL BE RUN FOR ALL DIVISIONS.            *
     H*                                                               *
     H*  11/12/99 C:65 PROJECT: WC577                                 *
     H*                ALLOW BOTH HARDCOPY AND DARTS TO BE 'N' IN     *
     H*                THE EVENT AN EMPLOYER DOES NOT WANT ANY OUTPUT *
     H*                                                               *
     H*  10/01/99 C:65 PROJECT: WC901                                 *
     H*                IF PGM CALLED FROM QUOTE AND IS RUN AS A CALC  *
     H*                & PRINT, THEN DEFAULT THE A/R CODES SO THE     *
     H*                BILL WILL WRITE A/R FOR THE INSTALLMENT SUMM.  *
     H*                BYPASS THE PREVIOUS A/R CHECK AS WELL.        *
     H*                                                               *
     H*  04/22/99 C:90 PROJECT: WC887                                 *
     H*                IF PGM CALLED FROM QUOTE, DEFAULT DARTS RMK    *
     H*                TO WMEXP/EXQTE.                                *
     H*                                                               *
     H*  07/29/98 C:78 PROJECT: WC577                                 *
     H*                IF DEFAULT DARTS = *BLANK, THEN RESOLVE FLAG   *
     H*                SHOULD BE 'N'.                                 *
     H*                                                               *
     H*  02/25/98 C:65 PROJECT: WC705                                 *
     H*                CHANGE NO A/R IN PRIOR MONTH TO CHECK TRANS-   *
     H*                ACTION DATE AS WELL AS POSTING DATE.           *
     H*                                                               *
     H*  02/25/98 C:65 PROJECT: WC705                                 *
     H*                CHANGE POSTING DATE TO BE THE GREATER OF BILL- *
     H*                ING START DATE OR SYSTEM DATE + INTERIM DAYS   *
     H*                                                               *
     H*  02/19/98 C:65 PROJECT: WC705                                 *
     H*                CHANGE SPREAD DISC MESSAGE FOR MULTIPLE DIVS.  *
     H*                                                               *
     H*  02/17/98 C:65 PROJECT: WC705                                 *
     H*                CHANGE DUE DATE TO INTERIM DAYS FOR DEP DUE.   *
     H*                                                               *
     H*  01/02/98 C:85 PROJECT: WC577                                 *
     H*                CHANGED DEFAULT FOR SCDEP TO "Y".              *
     H*                                                               *
     H*  12/26/97 C:85 PROJECT: WC577                                 *
     H*                CHANGED SEARCHES FOR SCYR, SCEMP# & SCDIV.     *
     H*                                                               *
     H*  11/20/97 C:65 PROJECT: V3                                    *
     H*                WHEN RUNNING FROM THE MENU(MASS RUNS) REMOVE   *
     H*                THE POSTING DATE PROMPT.                       *
     H*                WHEN RUNNING FROM EMPLOYER MASTER(SINGLE RUNS) *
     H*                REMOVE THE 2ND COPY IN AGENT ORDER.            *
     H*                DEFAULT THE HARDCOPY FLAG TO Y.                *
     H*                                                               *
     H*  11/19/97 C:65 PROJECT: V3                                    *
     H*                DO NOT SET A/R CODES WHEN NOT IN MODE 3 OR 4.  *
     H*                CHANGED FYR ON PROMPT TO POLICY YEAR.          *
     H*                DO NOT USE USER PARMS FOR FYR/EMP#/DIV ANYMORE.*
     H*                                                               *
     H*  11/04/97 C:85 PROJECT: WC764                                 *
     H*                VERSION 3 REPORT STANDARDS,                    *
     H*                ADDED DIVISION COUNT TO SCR01.                 *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     FW8001F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWDELL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     FSTCTL1    IF   E           K DISK
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL02)
     F*** BEG CHG *****  09/27/07  ************************************
     F**********WDELL1  IF  E           K        DISK
     F**********  PEL01                             KRENAMEPEL03
     FWDELL1    UF   E           K DISK
     F                                     RENAME(PEL01:PEL03)
     F*** END CHG *****  09/27/07  ************************************
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PSY01)
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SWC01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     FWMEML     IF   E           K DISK
     F                                     INFDS(DBFDS3)
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEAL6    IF   E           K DISK
     FWMEXL     IF   E           K DISK
     F*** BEGIN ADD ***  05/30/03  ************************************
     F*** BEGIN ADD ***  01/13/04  ************************************
     F*********** WMDBL   IF  E           K        DISK
     FWMDBL3    IF   E           K DISK
     F*** END   ADD ***  01/13/04  ************************************
     F*** END ADD *****  05/30/03  ************************************
     F***** BEG ADD ******* 07/27/10 **********************************
     FWMEFL6    IF   E           K DISK
     F***** END ADD ******* 07/27/10 **********************************
     F*** BEGIN ADD ***  02/27/06  ************************************
     FWDEHL     IF   E           K DISK
     F*** END ADD *****  02/27/06  ************************************
     F*** BEG ADD *****  09/27/07  ************************************
     FWDEAL4    IF   E           K DISK
     F                                     RENAME(PEA01:PEA04)
     FWABLL     UF   E           K DISK
     FWABLL3    UF   E           K DISK
     F                                     RENAME(PBL01:PBL02)
     F*** END ADD *****  09/27/07  ************************************
      *** Begin Add *** 08/08/18 *****************************************************
      * Estimated Billing
     d WtPolEstBl      pr                  ExtPgm('WTPOLESTBL')
     d inCall                         1A
     d inQte                          1A
     d inKC                           1A   options(*nopass)
     d inKL                           1A   options(*nopass)
     d inRrn                          9A   options(*nopass)

     d inCall          s              1A
     d inQte           s              1A
     d inKC            s              1A
     d inKL            s              1A
     d inRRn           s              9A
      *** End   Add *** 08/08/18 *****************************************************

     D*
     D*
     D OPTNM0          C                   CONST('Print Bills')
     D OPTNM2          C                   CONST('Calculate and Print -
     D                                     Bills')
     D OPTNM3          C                   CONST('Update Accounts Rece-
     D                                     ivable from Billing')
     D OPTNM4          C                   CONST('Calculate Update an-
     D                                     d Print Bills')
     D OPTNQ0          C                   CONST('Print Bills-Quote')
     D OPTNQ2          C                   CONST('Calculate and Print -
     D                                     Bills-Quote')
     D OPTNQ3          C                   CONST('Update A/R from Bill-
     D                                     ing-Quote')
     D OPTNQ4          C                   CONST('Calculate Update an-
     D                                     d Print Bills-Quote')
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXPODT                 1      7  0
     D  XXPOYM                 1      5  0
     D  XXPODA                 6      7  0
     D*** BEGIN ADD ***  02/26/98  ************************************
     D                 DS
     D  XXTRDT                 1      7  0
     D  XXTRYM                 1      5  0
     D  XXTRDA                 6      7  0
     D*** END ADD *****  02/26/98  ************************************
     D                 DS
     D  XXSYDT                 1      7  0
     D  XXSYYM                 1      5  0
     D  XXSYDA                 6      7  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*** BEGIN ADD ***  06/20/05  ************************************
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*** BEGIN ADD ***  06/20/05  ************************************
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE (TO COUNT DIVISIONS)
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C*  EMPLOYER A/R FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*** BEGIN CHG ***  04/27/00  ************************************
     C*********            KFLD           XXDIV
     C                   KFLD                    ELDIV
     C*** END CHG *****  04/27/00  ************************************
     C                   KFLD                    XXFYR
     C*
     F*** BEGIN ADD ***  01/13/04  ************************************
     C     DBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXDATE
     C*** END   ADD ***  01/13/04  ************************************
     C*
     C*** BEG ADD *****  09/27/07  ************************************
     C     EAKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCBAT#
     C*
     C     BLKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     BLKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCRNB
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*** END ADD *****  09/27/07  ************************************
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C*
     C*  DEFINE DIVISION COUNT
     C*
     C                   Z-ADD     0             SCDIVC            3 0          COUNT DIVISIONS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCALL            1
     C                   PARM                    QQQTE             1
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@RRN             9 0
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** BEGIN ADD ***  04/14/10  ************************************
     C     DTMDY3        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** BEGIN ADD ***  04/14/10  ************************************
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
     C*** Begin Add *** 08/08/18 *****************************************
      /free
        // Call the new Estimated Billing Prompt
        inCall = qqCall;
        inQte  = qqQte;

        If %Parms() > 2;
           inKC   = @@kc;
           inKL   = @@kl;
           inRrn  = %EditC(@@rrn:'X');
           WtPolEstBl(inCall:inQte:inKC:inKL:inRrn);
        Else;
           WtPolEstBl(inCall:inQte);
        EndIf;

        Return;
      /end-free
     C*** End   Add *** 08/08/18 *****************************************
     C*
     C*** BEG ADD ***** 07/27/10 ************************************
     C                   MOVEL(P)  'WA073R'      XXPGM
     C*** BEG CHG ***** 10/22/10 ************************************
     C*******              CALL 'SRCHKPGM'             99
     C*******              PARM           XXPGM  10
     C*******              PARM           XXNACH  1
     C*
     C                   MOVEL     '*PGM'        XXTYP             7
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    XXPGM            10
     C                   PARM                    XXTYP
     C                   PARM                    XXNACH            1
     C*** END CHG ***** 10/22/10 ************************************
     C*** END ADD ***** 07/27/10 ************************************
     C*** BEGIN ADD ***  06/20/05  ************************************
     C                   MOVE      *OFF          *IN71
     C     QQCALL        IFGT      '4'
     C                   MOVE      *ON           *IN71
     C                   MOVE      QQCALL        XXCALL            1 0
     C                   SUB       4             XXCALL
     C                   MOVE      XXCALL        QQCALL
     C                   ENDIF                                                  QQCALL IFGT
     C*** END ADD *****  06/20/05  ************************************
     C                   MOVE      *ON           *IN70
     C*** BEG ADD ****** 09/27/07  ************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C     XXCLT#        IFEQ      '071'
     C                   SETON                                        72
     C                   ELSE
     C                   SETOFF                                       72
     C                   ENDIF                                                  XXCLT# IFEQ 71
     C*** END ADD ****** 09/27/07  ************************************
     C*
     C*  CHECK IF OFFICE IS INSTALLED.
     C                   MOVEL     'SY'          XXSYK
     C     SYKY          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *ON
     C     SYOFIF        ORNE      'Y'
     C     QQCALL        ORNE      '4'
     C     QQQTE         ANDNE     'Q'
     C*** BEGIN ADD ***  02/23/00  ************************************
     C     QQCALL        ANDNE     '2'
     C*** END ADD *****  02/23/00  ************************************
     C     QQCALL        ORNE      '2'
     C     QQQTE         ANDEQ     'Q'
     C                   MOVE      *ON           *IN58
     C                   MOVE      'N'           SCDART
     C                   MOVE      'Y'           SCHARD
     C                   MOVE      'N'           SCAGTO
     C                   ELSE                                                   *IN60 EQ 1
     C*  DEFAULT RESOLVE TO DARTS AND HARDCOPY FLAGS TO 'Y' INITIALLY
     C                   MOVE      'Y'           SCDART
     C                   MOVE      'Y'           SCHARD
     C                   MOVE      'N'           SCAGTO
     C                   ENDIF                                                  *IN60 EQ 1
     C*
     C*** BEGIN ADD ***  11/05/09  ************************************
     C     QQCALL        IFEQ      ' '
     C                   MOVE      *OFF          *IN58
     C                   ENDIF                                                  QQCALL EQ ' '
     C*
     C*** END  ADD ***  11/05/09  ************************************
     C     QQCALL        IFEQ      '3'
     C                   MOVE      *ON           *IN62
     C                   MOVE      'N'           SCHARD
     C                   MOVE      'N'           SCAGTO
     C                   ENDIF                                                  QQCALL NE 3
     C*
     C     QQQTE         IFEQ      'Q'
     C                   MOVE      *ON           *IN68
     C                   ENDIF                                                  QQQTE EQ Q
     C                   MOVE      'N'           SCFQT
     C*** BEGIN ADD ***  11/30/04  ************************************
     C                   MOVE      *BLANK        XXACCT            1
     C*** END ADD *****  11/30/04  ************************************
     C*
     C     Q1PRMS        IFGT      2
     C     @@RRN         CHAIN     PEL02                              60
     C                   MOVE      ELCO#         XXCO#
     C                   MOVE      ELFND         XXFND
     C                   MOVE      ELEMP#        XXEMP#
     C                   MOVE      ELDIV         XXDIV
     C                   MOVE      ELFYR         XXFYR
     C                   MOVE      ELFND         SCFND
     C                   MOVE      ELEMP#        SCEMP#
     C                   MOVE      ELDIV         SCDIV
     C                   MOVE      ELFYR         SCFYR
     C                   Z-ADD     ELFYR         SCYR
     C*** BEGIN ADD ***  11/30/04  ************************************
     C                   Z-ADD     XXCO#         @@CO#
     C                   Z-ADD     XXFND         @@FND
     C                   Z-ADD     XXEMP#        @@EMP#
     C                   Z-ADD     XXDIV         @@DIV
     C                   Z-ADD     XXFYR         @@FYR
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         @@DATE
     C                   MOVEL     '*'           @@PLAN
      *
     C                   CALL      'WA002R'
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@AGT#            5 0
     C                   PARM                    @@ANAM           60
     C                   PARM                    @@PROD            5 0
     C                   PARM                    @@PNAM           60
     C                   PARM                    @@AREA            3 0
     C                   PARM                    @@PHON            7 0
     C                   PARM                    @@PLAN            1
     C                   PARM                    @@DATE            8 0
     C*
     C     @@PLAN        IFEQ      'A'
     C                   MOVE      'Y'           XXACCT
     C                   ENDIF
     C*** END ADD *****  11/30/04  ************************************
     C*
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              60
     C     BSSEFL        IFEQ      'S'
     C                   MOVE      *ON           *IN70
     C                   ELSE
     C                   MOVE      *OFF          *IN70
     C                   ENDIF                                                  BSSEFL IFEQ
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM                    XXEMP#
     C                   PARM                    XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*
     C                   MOVE      *ON           *IN49
     C                   ELSE
     C                   MOVE      *OFF          *IN49
     C                   ENDIF                                                  Q1PRMS IFGT
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
     C                   TIME                    DSDTTM
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '0'
     C     *IN49         ANDEQ     '0'
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPFND         XXFND
     C                   MOVE      UPFND         SCFND
     C                   ENDIF
     C*
     C*** BEGIN ADD ***  11/19/97  ************************************
     C     QQCALL        IFEQ      '3'
     C     QQCALL        OREQ      '4'
     C*** END ADD *****  11/19/97  ************************************
      *** BEG ADD ***** 02/03/09 ****************************************
     C     QQCALL        OREQ      '2'
     C     QQQTE         ANDEQ     'Q'
      *** END ADD ***** 02/03/09 ****************************************
     C     FNKEY         CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EXESCD        SCESCD
     C                   MOVE      EXSRCD        SCSRCD
     C                   MOVE      EXDACD        SCAJCD
     C                   MOVE      EXORG         SCRMCD
     C                   ELSE
     C                   MOVE      *BLANKS       SCESCD
     C                   MOVE      *BLANKS       SCSRCD
     C                   MOVE      *BLANKS       SCAJCD
     C                   MOVE      *BLANKS       SCRMCD
     C                   ENDIF                                                  *IN60 IFEQ
     C***** BEGIN CHG ******* 07/29/98 ********************************
     C*  IF DEFAULT DARTS IS BLANK THEN DEFAULT RESOLVE TO DARTS FLAG
     C*  MUST BE 'N'.
     C     SCRMCD        IFEQ      *BLANK
     C                   MOVE      'N'           SCDART
     C                   ENDIF                                                  SCRMCD
     C***** END   CHG ******* 07/29/98 ********************************
     C*** BEGIN ADD ***  11/19/97  ************************************
     C                   ENDIF                                                  QQCALL IFEQ
     C*** END ADD *****  11/19/97  ************************************
     C*** BEGIN ADD ***  02/23/00  ************************************
     C     QQCALL        IFEQ      '2'
     C     QQQTE         ANDNE     'Q'
     C*** BEGIN ADD ***  11/05/09  ************************************
     C     QQCALL        OREQ      ' '
     C*** END   ADD ***  11/05/09  ************************************
     C*
     C     FNKEY         CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EXORG         SCRMCD
     C                   ELSE
     C                   MOVE      *BLANKS       SCRMCD
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     SCRMCD        IFEQ      *BLANK
     C                   MOVE      'N'           SCDART
     C                   ENDIF                                                  SCRMCD
     C*
     C                   ENDIF                                                  QQCALL IFEQ
     C*** END ADD *****  02/23/00  ************************************
      **** BEGIN ADD **** 04/22/99 ************************************
     C     FNKEY         CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C     QQQTE         ANDEQ     'Q'
     C                   MOVE      EXQTE         SCRMCD
     C                   ENDIF                                                  QQCALL IFEQ
      **** END   ADD **** 04/22/99 ************************************
     C*
     C***  Begin Add  ***  02/03/09  **************************************
     C     EX3RDB        IFEQ      'Y'
     C                   CLEAR                   SCRMCD
     C                   MOVE      'N'           SCDART
     C                   ENDIF
     C***  End   Add  ***  02/03/09  **************************************
     C*
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      FNNAME        SCNAME
     C*** BEGIN CHG ***  11/19/97  ************************************
     C*****      *IN49     IFEQ *OFF
     C*****                Z-ADDUPEMP#    SCEMP#
     C*****                Z-ADDUPFYR     SCFYR
     C*****                Z-ADDUPDIV     SCDIV
     C*****                ENDIF                           *IN49 IFEQ
     C                   Z-ADD     0             UPFYR
     C*** END CHG *****  11/19/97  ************************************
     C*
     C* DEFAULT POSTING DATE TO BILLING START DATE IF CALLED FROM
     C* WORK-WITH, ELSE DEFAULT TO CURRENT SYSTEM DATE.
     C*
     C*** BEGIN ADD ***  02/25/98  ************************************
     C*
     C                   Z-ADD     0             SCDT
     C*
     C     FNKEY         CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*** BEGIN ADD ***  11/30/04  ************************************
     C     XXACCT        IFEQ      'Y'
     C                   Z-ADD     EXAOID        XXDAYS            5 0
     C                   ELSE
     C                   Z-ADD     EXRVID        XXDAYS
     C                   ENDIF
     C*** END ADD *****  11/30/04  ************************************
     C*** BEG ADD ***** 09/06/07 **************************************
     C     *IN70         IFEQ      *ON
     C                   Z-ADD     EXRVID        SCDUE
     C                   ENDIF
     C*** END ADD ***** 09/06/07 **************************************
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C*** BEGIN ADD ***  11/30/04  ************************************
     C*********            PARM EXRVID    QQDAYS  50
     C                   PARM      XXDAYS        QQDAYS            5 0
     C*** END ADD *****  11/30/04  ************************************
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SCDT              8 0
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*** END ADD *****  02/25/98  ************************************
     C     Q1PRMS        IFGT      2
     C*** BEG ADD *** 04/14/10 ****************************************
     C     XXCLT#        IFEQ      '132'
     C                   Z-ADD     ELBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY3
     C                   MOVEL     SRDTO         XXDT1             6 0
     C                   CALL      'SRFUTR8'
     C                   PARM      XXDT1         QQDT              8 0
     C                   PARM      XXDAYS        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SCDATE            8 0
     C                   ENDIF
     C                   ELSE
     C*** END ADD *** 04/14/10 ****************************************
     C                   Z-ADD     ELBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDATE
     C*** BEG ADD *** 04/14/10 ****************************************
     C                   ENDIF
     C*** END ADD *** 04/14/10 ****************************************
     C                   ELSE
     C*** BEGIN ADD ***  02/25/98  ************************************
     C     SCDT          IFNE      *ZERO
     C                   Z-ADD     SCDT          SCDATE
     C                   ELSE
     C*** END ADD *****  02/25/98  ************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         SCDATE
     C*** BEGIN ADD ***  02/25/98  ************************************
     C                   ENDIF                                                  SCDT IFNE
     C*** END ADD *****  02/25/98  ************************************
     C                   ENDIF                                                  Q1PRMS IFGT
     C*
     C*** BEGIN ADD ***  02/25/98  ************************************
     C     Q1PRMS        IFGT      2
     C*
     C     SCDT          IFNE      *ZERO
     C                   Z-ADD     SCDT          SRDTI
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         SCDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   ENDIF                                                  SCDT IFNE
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C*
     C*** BEG ADD *** 04/14/10*****************************************
     C     XXCLT#        IFEQ      '132'
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT2             7 0
     C*
     C     XXDT          IFGT      XXDT2
     C                   Z-ADD     SCDT          SCDATE
     C                   ENDIF                                                  XXDT IFGT
     C                   ELSE                                                   XXCLT#
     C*** END ADD *** 04/14/10*****************************************
     C     XXDT          IFGT      ELBSDT
     C                   Z-ADD     SCDT          SCDATE
     C                   ENDIF                                                  XXDT IFGT
     C*** BEG ADD *** 04/14/10*****************************************
     C                   ENDIF                                                  XXDT IFGT
     C*** END ADD *** 04/14/10*****************************************
     C*
     C                   ENDIF                                                  Q1PRMS IFGT
     C*** END ADD *****  02/25/98  ************************************
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   SELECT
     C     QQCALL        WHENEQ    ' '
     C     QQQTE         ANDEQ     'Q'
     C                   MOVEL(P)  OPTNQ0        OPTNM            40
     C     QQCALL        WHENEQ    '2'
     C     QQQTE         ANDEQ     'Q'
     C                   MOVEL(P)  OPTNQ2        OPTNM
     C     QQCALL        WHENEQ    '3'
     C     QQQTE         ANDEQ     'Q'
     C                   MOVEL(P)  OPTNQ3        OPTNM
     C     QQCALL        WHENEQ    '4'
     C     QQQTE         ANDEQ     'Q'
     C                   MOVEL(P)  OPTNQ4        OPTNM
     C     QQCALL        WHENEQ    ' '
     C                   MOVEL(P)  OPTNM0        OPTNM
     C     QQCALL        WHENEQ    '2'
     C                   MOVEL(P)  OPTNM2        OPTNM
     C     QQCALL        WHENEQ    '3'
     C                   MOVEL(P)  OPTNM3        OPTNM            40
     C     QQCALL        WHENEQ    '4'
     C                   MOVEL(P)  OPTNM4        OPTNM            40
     C                   OTHER
     C                   MOVEL(P)  'BILLS'       OPTNM
     C                   ENDSL
     C*
     C                   MOVEL(P)  OPTNM         SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C     QQCALL        IFNE      '3'
     C     QQCALL        ANDNE     '4'
     C                   MOVE      '1'           *IN91
     C                   END
      *
      *** BEG ADD ***** 02/03/09 ****************************************
     C     QQCALL        IFEQ      '2'
     C     QQQTE         ANDEQ     'Q'
     C     EX3RDB        ANDEQ     'Y'
     C                   MOVE      *OFF          *IN91
     C                   ENDIF
      *** END ADD ***** 02/03/09 ****************************************
      *
     C     *IN49         IFEQ      *OFF
     C     UPFYR         IFEQ      *ZERO
     C                   MOVE      '1'           *IN38
     C                   ELSE
     C                   MOVE      '1'           *IN41
     C                   ENDIF                                                  UPFYR EQ 0
     C                   ELSE
     C                   MOVE      *ON           *IN42
     C                   ENDIF                                                  *IN49 IFEQ
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C*
     C*  BEGIN CALCULATION FOR DIVISION COUNT
     C*
     C     *IN49         IFEQ      *ON
     C*
     C                   Z-ADD     0             SCDIVC                         COUNT DIVISIONS
     C*
     C*** BEG CHG *****  09/27/07  ************************************
     C********** ELKY1     CHAINPEL03                60
     C     ELKY1         CHAIN(N)  PEL03                              60
     C*** END CHG *****  09/27/07  ************************************
     C*
     C     *IN60         DOWEQ     *OFF
     C     ELPFDT        IFNE      ELPTDT
     C                   ADD       1             SCDIVC
     C                   ENDIF                                                  ELPFDT<>ELPTDT
     C*** BEG CHG *****  09/27/07  ************************************
     C********** ELKY1     READEPEL03                    60
     C     ELKY1         READE(N)  PEL03                                  60
     C*** END CHG *****  09/27/07  ************************************
     C                   ENDDO                                                  *IN60 = OFF
     C*
     C                   ENDIF                                                  *IN49 = ON
     C*** BEGIN ADD ***  11/15/99  ************************************
     C     SCDIVC        IFGT      1
     C*** BEGIN ADD ***  06/20/05  ************************************
     C     *IN71         ANDEQ     *OFF
     C*** END ADD *****  06/20/05  ************************************
     C*** BEG ADD *** 12/02/09 ****************************************
     C     QQCALL        ANDNE     ' '
     C*** END ADD *** 12/02/09 ****************************************
     C                   MOVEL     'SCR02'       Q2RECF
     C                   ENDIF                                                  SCDIVC IFGT
     C*** END ADD *****  11/15/99  ************************************
     C*
     C*  END CALCULATION FOR DIVISION COUNT
     C*
     C*** BEGIN CHG ***  01/02/98  ************************************
     C*
     C*  CHANGE DEFAULT FOR SCDEP TO "Y"
     C*
     C                   MOVE      'Y'           SCDEP
     C*** END   CHG ***  01/02/98  ************************************
     C*
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
     C*
     C*  PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  F3=EXIT
     C*
     C     *INKC         IFEQ      '1'
     C     Q1PRMS        IFGT      2
     C                   MOVE      *INKC         @@KC
     C                   ENDIF                                                  Q1PRMS IFGT
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  F12=CANCEL
     C*
     C     *INKL         IFEQ      '1'
     C*** BEGIN DEL ***  11/15/99  ************************************
     C*******    Q2RECF    IFEQ 'SCR01'
     C*** END DEL *****  11/15/99  ************************************
     C     Q1PRMS        IFGT      2
     C                   MOVE      *INKL         @@KL
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  Q1PRMS IFGT
     C*** BEGIN DEL ***  11/15/99  ************************************
     C************         ELSE
     C************         MOVEL'SCR01'   Q2RECF
     C************         GOTO STRT
     C************         ENDIF                           Q2RECF IFEQ
     C*** END DEL *****  11/15/99  ************************************
     C                   END
     C*
     C*  CMD 10 - TABLE FILE SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C*  CMD 10 - TABLE FILE SEARCH - COMMENT 1,2,3
     C*
     C     HFLD          IFEQ      'SCCOM1'
     C     HFLD          OREQ      'SCCOM2'
     C     HFLD          OREQ      'SCCOM3'
     C*
     C                   MOVEL(P)  *BLANKS       QQKEY
     C*
     C                   CALL      'W2191R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQKEY            10
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCCOM1'
     C*
     C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C                   SETON                                        79
     C                   ELSE
     C                   SETON                                        80
     C                   MOVEL     QQKEY         SCCOM1
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCCOM2'
     C*
     C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C                   SETON                                        80
     C                   ELSE
     C                   MOVEL     QQKEY         SCCOM2
     C                   SETON                                        81
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCCOM3'
     C*
     C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C                   SETON                                        81
     C                   ELSE
     C                   MOVEL     QQKEY         SCCOM3
     C                   SETON                                        82
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C*
     C*  CMD 10 - TABLE FILE SEARCH - A/R TRANSACTION CODES
     C*
     C*
     C*
     C     HFLD          IFEQ      'SCESCD'
     C     HFLD          OREQ      'SCSRCD'
     C     HFLD          OREQ      'SCAJCD'
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C*
     C                   CALL      'W2161R'
     C*** BEGIN CHG ***  05/27/03  ************************************
     C*********            PARM '2'       QQCALL  1
     C                   PARM      '2'           ##CALL            1
     C*** END CHG *****  05/27/03  ************************************
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCESCD'
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C                   SETON                                        30
     C                   ELSE
     C                   MOVEL     SCKEY         SCESCD
     C                   SETON                                        66
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCSRCD'
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C                   SETON                                        66
     C                   ELSE
     C                   MOVEL     SCKEY         SCSRCD
     C                   SETON                                        67
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCAJCD'
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C                   SETON                                        67
     C                   ELSE
     C                   MOVEL     SCKEY         SCAJCD
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C*
     C*  CMD 10 - TABLE FILE SEARCH - DARTS REMARK CODE
     C*
     C*
     C     HFLD          IFEQ      'SCRMCD'
     C*
     C***  Begin Chg  ***  10/11/06  **************************************
     C**********           CALL 'SC580R'
     C**********           PARM           SCRMCD
     C**********           PARM           QQRTC   1
     C**********
     C**********           MOVE *ON       *IN83
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN83
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCRMCD
     C                   OTHER
     C                   MOVE      *ON           *IN83
     C                   ENDSL
     C***  End   Chg  ***  10/11/06  **************************************
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C*** BEGIN DEL *** 12/26/97 **************************************
     C**********
     C**********
     C**********
     C***CMD*10*- TABLE FILE SEARCH - EMPLOYER FYR
     C**********
     C**********
     C********** HFLD      IFEQ 'SCFYR'
     C**********
     C********** SCEMP#    IFNE *ZERO
     C**********           Z-ADDSCEMP#    XXEMP#
     C**********           Z-ADDSCDIV     XXDIV
     C********** EMKEY     CHAINPEM01                60
     C********** *IN60     IFEQ *OFF
     C**********           MOVE '2'       @@CALL
     C**********           Z-ADDXXFND     @1RRN
     C**********           Z-ADDDBRRN3    @1RRN2
     C**********
     C**********           CALL 'W0051R'
     C**********           PARM           @1KC    1
     C**********           PARM           @1KL    1
     C**********           PARM           @@CALL  1
     C**********           PARM           @1RRN   90
     C**********           PARM           @1RRN2  90
     C**********           PARM           QQQTE   1
     C**********
     C********** @1RRN2    IFNE *ZERO
     C********** @1RRN2    CHAINPEL02                60
     C**********           MOVE ELEMP#    SCEMP#
     C**********           MOVE ELDIV     SCDIV
     C**********           MOVE ELFND     SCFND
     C**********           MOVE ELFYR     SCFYR
     C**********           ELSE
     C********** @1RRN     CHAINPEM02                60
     C**********           MOVE EMEMP#    SCEMP#
     C**********           MOVE EMDIV     SCDIV
     C**********           MOVE EMFND     SCFND
     C**********           ENDIF                           @1RRN  IFNE
     C**********
     C**********           ELSE
     C**********
     C**********           MOVE *ON       *IN21
     C**********
     C**********           ENDIF                           *IN60 IFEQ
     C**********
     C**********           ELSE
     C**********
     C**********           MOVE 'Y'       @@CALL
     C**********           Z-ADDXXFND     @1RRN
     C**********           Z-ADD0         @1RRN2
     C**********
     C**********           CALL 'W0050R'
     C**********           PARM           @1KC    1
     C**********           PARM           @1KL    1
     C**********           PARM           @@CALL  1
     C**********           PARM           @1RRN   90
     C**********           PARM           @1RRN2  90
     C**********           PARM           QQQTE   1
     C**********           PARM 2         @@VIEW  10
     C**********
     C********** @1RRN2    IFNE *ZERO
     C********** @1RRN2    CHAINPEL02                60
     C**********           MOVE ELEMP#    SCEMP#
     C**********           MOVE ELDIV     SCDIV
     C**********           MOVE ELFND     SCFND
     C**********           MOVE ELFYR     SCFYR
     C**********           ELSE
     C********** @1RRN     CHAINPEM02                60
     C**********           MOVE EMEMP#    SCEMP#
     C**********           MOVE EMDIV     SCDIV
     C**********           MOVE EMFND     SCFND
     C**********           ENDIF                           @1RRN  IFNE
     C**********
     C**********           ENDIF                           SCEMP# IFNE
     C**********
     C**********           MOVE *ON       *IN38
     C**********
     C**********           ENDIF                           HFLD IFEQ
     C**********
     C**********
     C**F10*-*TABLE FILE SEARCH - EMPLOYER NUMBER OR DIVISION
     C**********
     C**********
     C********** HFLD      IFEQ 'SCEMP#'
     C********** HFLD      OREQ 'SCDIV'
     C**********
     C********** SCFYR     IFNE *ZERO
     C**********           MOVE '2'       @@CALL
     C**********           ELSE
     C**********           MOVE 'Y'       @@CALL
     C**********           ENDIF
     C**********           Z-ADDXXFND     @1RRN
     C**********           Z-ADD0         @1RRN2
     C**********
     C**********           CALL 'W0050R'
     C**********           PARM           @1KC    1
     C**********           PARM           @1KL    1
     C**********           PARM           @@CALL  1
     C**********           PARM           @1RRN   90
     C**********           PARM           @1RRN2  90
     C**********           PARM           QQQTE   1
     C**********
     C********** @1RRN2    IFNE *ZERO
     C********** @1RRN2    CHAINPEL02                60
     C**********           MOVE ELEMP#    SCEMP#
     C**********           MOVE ELDIV     SCDIV
     C**********           MOVE ELFND     SCFND
     C**********           MOVE ELFYR     SCFYR
     C**********           ELSE
     C********** @1RRN     CHAINPEM02                60
     C**********           MOVE EMEMP#    SCEMP#
     C**********           MOVE EMDIV     SCDIV
     C**********           MOVE EMFND     SCFND
     C**********           ENDIF                           @1RRN  IFNE
     C**********
     C**********           MOVE *ON       *IN41
     C**********
     C**********           ENDIF                           HFLD IFEQ
     C**********
     C*** END   DEL *** 12/26/97 **************************************
     C*
     C*** BEGIN ADD *** 12/26/97 **************************************
     C*
     C*  F10 - SEARCH
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C                   MOVE      *OFF          *IN38                          SCFYR POSITION
     C                   MOVE      *OFF          *IN41                          SCEMP# POSITION
     C                   MOVE      *OFF          *IN42                          SCRNB POSITION
     C*
     C*  F10 - TABLE FILE SEARCH - POLICY YEAR
     C*
     C     HFLD          IFEQ      'SCFYR'
     C*
     C                   MOVE      *ON           *IN38                          SCFYR POSITION
     C*
     C*  EMPLOYER NUMBER EQUAL TO ZERO
     C*
     C     SCEMP#        IFEQ      *ZEROS
     C*
     C                   MOVE      *ZEROS        @1RRN
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @1KC              1
     C     *INKL         PARM                    @1KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @1RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @1RRN         ANDNE     *ZEROS
     C*
     C     @1RRN         CHAIN     PEM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKL=OFF,@1RRN
     C*
     C                   ENDIF                                                  SCEMP#=0
     C*
     C*  EMPLOYER NUMBER NOT EQUAL TO ZERO
     C*
     C     SCEMP#        IFNE      *ZEROS
     C*
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '2'           @@CALL
     C                   Z-ADD     XXFND         @1RRN
     C                   Z-ADD     DBRRN3        @1RRN2
     C*
     C                   CALL      'W0051R'
     C                   PARM                    @1KC              1
     C                   PARM                    @1KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @1RRN             9 0
     C                   PARM                    @1RRN2            9 0
     C                   PARM                    QQQTE             1
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     @1RRN         IFNE      *ZEROS
     C*
     C     @1RRN         CHAIN     PEL02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELFYR         SCFYR
     C*
     C                   MOVE      *OFF          *IN38                          SCFYR POSITION
     C                   MOVE      *ON           *IN42                          SCRNB POSITION
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  @1RRN<>0
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  SCEMP#<>0
     C*
     C                   ENDIF                                                  HFLD=SCFYR
     C*
     C*  F10 - TABLE FILE SEARCH - EMPLOYER NUMBER OR DIVISION
     C*
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   MOVE      *ON           *IN41                          SCEMP# POSITION
     C*
     C                   MOVE      *ZEROS        @1RRN
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @1KC              1
     C     *INKL         PARM                    @1KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @1RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @1RRN         ANDNE     *ZEROS
     C*
     C     @1RRN         CHAIN     PEM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C*
     C                   MOVE      *OFF          *IN41                          SCEMP# POSITION
     C                   MOVE      *ON           *IN42                          SCRNB POSITION
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKL=OFF,@1RRN
     C*
     C                   ENDIF                                                  HFLD=SCEMP#
     C*
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C*** END   ADD *** 12/26/97 **************************************
     C*
     C*
     C* ENSURE DATES DISPLAY CORRECTLY
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKJ IFEQ
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C*
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C     *IN49         ANDEQ     '0'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZEROS        UPCO#
     C                   MOVE      *ZEROS        UPFND
     C                   ENDIF
     C                   GOTO      TOP
     C                   ENDIF
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCDIV         XXDIV
     C                   Z-ADD     SCFYR         XXFYR
     C*
     C*  FUND YEAR MUST BE GREATER THAN ZERO
     C*
     C     SCFYR         IFLE      *ZERO
     C     SCRNB         ANDEQ     *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  EMPLOYER# MUST BE GREATER THAN ZERO
     C*
     C     SCEMP#        IFLE      *ZERO
     C     SCRNB         ANDEQ     *BLANK
     C                   MOVE      *ON           *IN54
     C                   GOTO      STRT
     C                   ENDIF                                                  SCEMP# LE 0
     C***  Begin Del  ***  10/10/03  **************************************
     C**********
     C***IF*CALLED FROM QUOTE, EMP# MUST BE GREATER THAN ZERO.
     C**********
     C********** SCEMP#    IFLE *ZERO
     C********** QQQTE     ANDEQ'Q'
     C**********           MOVE *ON       *IN54
     C**********           GOTO STRT
     C**********           ENDIF                           SCEMP# LE 0
     C***  End   Del  ***  10/10/03  **************************************
     C*
     C*  VALIDATE EMPLOYER/DIVISION IF ENTERED
     C*
     C     SCEMP#        IFNE      *ZERO
     C     EMKEY         CHAIN     PEM01                              21
     C     *IN21         CABEQ     '1'           STRT
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C     ELKEY         CHAIN     PEL01                              40
     C     *IN40         CABEQ     '1'           STRT
     C                   END
     C*
     C*  FYR/EMP#/DIV AND RENEWAL/NEW/BOTH FLAG CANNOT BOTH BE SET.
     C*
     C     SCRNB         IFNE      *BLANK
     C     SCFYR         IFNE      *ZERO
     C     SCEMP#        ORNE      *ZERO
     C     SCDIV         ORNE      *ZERO
     C                   MOVE      *ON           *IN55
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFYR NE 0
     C                   ENDIF                                                  SCRNB IFNE
     C*** BEGIN ADD ***  02/27/06  ************************************
     C*
     C* DO NOT ALLOW SINGLE DIVISION ORIGINAL BILL IF A BILLING HAS
     C* ALREADY BEEN RUN.
     C*
     C     SCEMP#        IFNE      *ZERO
     C     *IN71         ANDEQ     *ON
     C*
     C     ELKEY         CHAIN     PEH01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN44
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  SCFYR IFNE
     C*** END ADD *****  02/27/06  ************************************
     C*
     C*** BEGIN ADD ***  03/28/08  ************************************
     C*
     C* DO NOT ALLOW A NON-PRINT ONLY BILL OUTSIDE OF QUOTE IF NO VALID
     C* ACH INFORMATION FOR A POLICY WITH ONLINE S/R PROCESSING.
     C*
     C     QQQTE         IFNE      'Q'
     C     QQCALL        ANDNE     '1'
     C     SCEMP#        ANDNE     *ZERO
     C*
     C                   MOVE      ELCO#         Q3CO#
     C                   MOVE      ELFND         Q3FND
     C                   MOVE      ELEMP#        Q3EMP#
     C                   MOVE      ELDIV         Q3DIV
     C                   MOVE      ELFYR         Q3FYR
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         Q3EFDT
     C                   MOVE      '1'           Q3MODE
     C                   MOVE      *BLANK        Q3ERR
     C                   MOVE      *BLANKS       Q3DATA
     C*
     C                   CALL      'SRONLI'
     C                   PARM                    Q3CO#             3
     C                   PARM                    Q3FND             3
     C                   PARM                    Q3EMP#            9
     C                   PARM                    Q3DIV             5
     C                   PARM                    Q3FYR             3
     C                   PARM                    Q3EFDT            8
     C                   PARM                    Q3MODE            1
     C                   PARM                    Q3ERR             1
     C                   PARM                    Q3DATA          100
     C*
     C     Q3ERR         IFEQ      '1'
     C                   MOVE      *ON           *IN89
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF                                                  QQQTE IFNE
     C*** END ADD *****  03/28/08  ************************************
     C*
     C* IF SCRNB IS ENTERED, IT MUST BE AN R,N, OR B.
     C*
     C     SCRNB         IFNE      *BLANK
     C     SCRNB         ANDNE     'R'
     C     SCRNB         ANDNE     'N'
     C     SCRNB         ANDNE     'B'
     C                   MOVE      *ON           *IN56
     C                   GOTO      STRT
     C                   ENDIF                                                  SCRNB NE ' '
     C*
     C*  USER MUST HAVE PROPER SECURITY FOR RUNNING A MASS BILLING.
     C*
     C     SCRNB         IFNE      *BLANK
     C     FNKEY         CHAIN     PEX01                              60
     C     *IN60         IFEQ      *ON
     C     COKEY         CHAIN     PEX01                              60
     C                   ENDIF                                                  *IN60 IFEQ
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EXSUBL        XCLVL             3
     C                   MOVE      EXSUBT        XCTYP             1
     C                   MOVE      UPLVL         XULVL             3
     C                   MOVE      UPTYPE        XUTYP             1
     C                   MOVE      '1'           QQEXEC            1
     C                   MOVE      '1'           QQRES             1
     C                   CALL      'SRCCSC'
     C                   PARM                    XUTYP
     C                   PARM                    XULVL
     C                   PARM                    XCTYP
     C                   PARM                    XCLVL
     C                   PARM                    QQEXEC
     C                   PARM                    QQRES
     C*
     C                   MOVE      QQRES         *IN78
     C     *IN78         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN81 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  SCRNB NE ' '
     C*
     C     SCRNB         IFNE      *BLANKS
     C     *IN86         ANDNE     *ON
     C                   MOVE      *ON           *IN86
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      *OFF          *IN86
     C                   ENDIF
     C*
     C*  REDO DEPOSIT MUST BE Y OR N
     C*
     C     SCDEP         IFNE      'Y'
     C     SCDEP         ANDNE     'N'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE ENTERED DATE
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE PAYMENT DUE DATE
     C*
     C     *IN70         IFEQ      *ON
     C*
     C     SCDUE         IFLT      *ZERO
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   ENDIF                                                  SCDUE IFLT
     C*
     C                   ENDIF                                                  *IN70 IFEQ
     C*
     C*  VALIDATE THE BILLING COMMENTS
     C*
     C     SCCOM1        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCOM1        XXKEY
     C     TBKEY         CHAIN     PBC01                              25
     C  N25BCSTAT        COMP      'I'                                    25
     C     *IN25         CABEQ     '1'           STRT
     C                   END
     C*
     C     SCCOM2        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCOM2        XXKEY
     C     TBKEY         CHAIN     PBC01                              26
     C  N26BCSTAT        COMP      'I'                                    26
     C     *IN26         CABEQ     '1'           STRT
     C                   END
     C*
     C     SCCOM3        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCOM3        XXKEY
     C     TBKEY         CHAIN     PBC01                              27
     C  N27BCSTAT        COMP      'I'                                    27
     C     *IN27         CABEQ     '1'           STRT
     C                   END
     C*** BEGIN ADD ***  11/13/03  ************************************
     C     SCCOM1        IFNE      *BLANKS
     C     SCCOM2        ORNE      *BLANKS
     C     SCCOM3        ORNE      *BLANKS
     C*
     C     SCFCM1        IFNE      *BLANKS
     C     SCFCM2        ORNE      *BLANKS
     C     SCFCM3        ORNE      *BLANKS
     C                   MOVE      *ON           *IN43
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFCM1 IFNE
     C*
     C                   ENDIF                                                  SCCOM1 IFNE
     C*** END ADD *****  11/13/03  ************************************
     C*
     C*  VALIDATE THE RESOLVE TO DARTS FLAG.
     C*
     C     SCDART        IFNE      'Y'
     C     SCDART        ANDNE     'N'
     C                   MOVE      *ON           *IN57
     C                   GOTO      STRT
     C                   ENDIF                                                  SCDART NE Y
     C*
     C     SCDART        IFEQ      'Y'
     C     SCRMCD        CHAIN     PCT01                              84
     C*
     C     *IN84         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     CTSTAT        IFEQ      'I'
     C                   MOVE      *ON           *IN85
     C                   GOTO      STRT
     C                   ENDIF                                                  CTSTAT IFEQ
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       SCRMCD
     C*
     C                   ENDIF                                                  SCDART IFEQ
     C*
     C*  VALIDATE THE HARDCOPY FLAG
     C*
     C     SCHARD        IFNE      'Y'
     C     SCHARD        ANDNE     'N'
     C                   MOVE      *ON           *IN59
     C                   GOTO      STRT
     C                   ENDIF                                                  SCHARD NE Y
     C*
     C*  VALIDATE THE 2ND AGENT ORDER FLAG
     C*
     C     SCAGTO        IFNE      'Y'
     C     SCAGTO        ANDNE     'N'
     C                   MOVE      *ON           *IN61
     C                   GOTO      STRT
     C                   ENDIF                                                  SCAGTO NE Y
     C*
     C*  VALIDATE THE FINAL QUOTE FLAG
     C*
     C     SCFQT         IFNE      'Y'
     C     SCFQT         ANDNE     'N'
     C                   MOVE      *ON           *IN69
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFQT NE Y
     C*
     C*  RESOLVE AND HARDCOPY FLAGS CANNOT BOTH BE 'N'
     C*
     C*** BEGIN DEL ***  11/12/99  ************************************
     C******     SCDART    IFEQ 'N'
     C******     SCHARD    ANDEQ'N'
     C******     QQCALL    ANDNE'3'
     C******               MOVE *ON       *IN63
     C******               GOTO STRT
     C******               ENDIF                           SCDART EQ N
     C*** END DEL *****  11/12/99  ************************************
     C*
     C*  IF HARDCOPY IS 'Y' THEN 2ND ORDER FLAG MUST BE 'Y'
     C*
     C     SCHARD        IFEQ      'N'
     C     SCAGTO        ANDEQ     'Y'
     C                   MOVE      *ON           *IN64
     C                   GOTO      STRT
     C                   ENDIF                                                  SCDARD EQ Y
     C*
     C*  VALIDATE A/R TRANSACTION CODE IF ENTERED
     C*
     C     *IN91         IFEQ      '0'
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCESCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              35
     C  N35TTTYPE        COMP      'B'                                2323
     C  N23
     CANN35TTSTAT        COMP      'I'                                    35
     C     *IN23         CABEQ     '1'           STRT
     C     *IN35         CABEQ     '1'           STRT
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCSRCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              34
     C  N34TTTYPE        COMP      'H'                                3232
     C  N32
     CANN34TTSTAT        COMP      'I'                                    34
     C     *IN34         CABEQ     '1'           STRT
     C     *IN32         CABEQ     '1'           STRT
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCAJCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              37
     C  N37TTTYPE        COMP      'H'                                3636
     C  N36
     CANN37TTSTAT        COMP      'I'                                    37
     C     *IN37         CABEQ     '1'           STRT
     C     *IN36         CABEQ     '1'           STRT
     C                   END
     C*
     C*
     C*  DO NOT ALLOW PREMIUM OR A/R UPDATE REQUEST IF THERE ARE 'B'
     C*  OR 'A' TRANSACTIONS IN A/R WITH A POSTING DATE LESS THAN
     C*  THE CURRENT SYSTEM MONTH
     C*
     C     QQQTE         IFNE      'Q'
     C     QQCALL        ANDNE     *BLANKS
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSYDT
     C                   Z-ADD     SCFYR         XXFYR
     C*** BEGIN ADD ***  06/20/05  ************************************
     C     *IN71         IFEQ      *OFF
     C*** END ADD *****  06/20/05  ************************************
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*** BEG CHG *****  09/27/07  ************************************
     C********** ELKY1     CHAINPEL03                60
     C     ELKY1         CHAIN(N)  PEL03                              60
     C*** END CHG *****  09/27/07  ************************************
     C     *IN60         DOWEQ     *OFF
     C*** END ADD *****  11/15/99  ************************************
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C     EAARTY        IFEQ      'A'
     C     EAARTY        OREQ      'B'
     C*** BEGIN ADD ***  12/21/00  ************************************
     C     EAARTY        OREQ      'H'
     C*** END ADD *****  12/21/00  ************************************
     C                   Z-ADD     EAPODT        XXPODT
     C*** BEGIN ADD ***  02/26/98  ************************************
     C                   Z-ADD     EATRDT        XXTRDT
     C*** END ADD *****  02/26/98  ************************************
     C     XXPOYM        IFLT      XXSYYM
     C*** BEGIN ADD ***  02/26/98  ************************************
     C     XXTRYM        ANDLT     XXSYYM
     C*** END ADD *****  02/26/98  ************************************
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END
     C                   END
     C     EAKEY         READE     PEA01                                  60
     C                   END
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*** BEG CHG *****  09/27/07  ************************************
     C********** ELKY1     READEPEL03                    60
     C     ELKY1         READE(N)  PEL03                                  60
     C*** END CHG *****  09/27/07  ************************************
     C                   ENDDO                                                  *IN60 = OFF
     C*** END ADD *****  11/15/99  ************************************
     C*** BEGIN ADD ***  06/20/05  ************************************
     C                   ELSE
     C*
     C* VALIDATE A/R ON DIVISION ONLY FOR SINGLE DIVISION CALL, AND/OR
     C* A/R POSTED FOR STATE REPORTING DIVISION IF A/R SUMMARIZED.
     C*
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     EAARTY        IFEQ      'A'
     C     EAARTY        OREQ      'B'
     C     EAARTY        OREQ      'H'
     C                   Z-ADD     EAPODT        XXPODT
     C                   Z-ADD     EATRDT        XXTRDT
     C*
     C     XXPOYM        IFLT      XXSYYM
     C     XXTRYM        ANDLT     XXSYYM
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   ENDIF                                                  XXPOYM IFLT
     C*
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C     EAKEY         READE     PEA01                                  60
     C                   ENDDO
     C*
     C     ELARSM        IFEQ      'Y'
     C*
     C* CHECK STATE REPORTING DIVISION FOR A/R SUMMARIZED DIVISIONS.
     C*
     C                   Z-ADD     XXDIV         SVDIV             5 0
     C     EMKY1         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C     EMSTRP        ANDNE     'Y'
     C     EMKY1         READE     PEM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   Z-ADD     EMDIV         XXDIV
     C     ELKEY         CHAIN     PEL01                              60
     C*
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     EARDIV        IFEQ      SVDIV
     C*
     C     EAARTY        IFEQ      'A'
     C     EAARTY        OREQ      'B'
     C     EAARTY        OREQ      'H'
     C                   Z-ADD     EAPODT        XXPODT
     C                   Z-ADD     EATRDT        XXTRDT
     C*
     C     XXPOYM        IFLT      XXSYYM
     C     XXTRYM        ANDLT     XXSYYM
     C                   MOVE      '1'           *IN39
     C                   Z-ADD     SVDIV         XXDIV
     C     EMKEY         CHAIN     PEM01                              60
     C     ELKEY         CHAIN     PEL01                              60
     C                   GOTO      STRT
     C                   ENDIF                                                  XXPOYM IFLT
     C*
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C                   ENDIF                                                  EARDIV IFEQ
     C*
     C     EAKEY         READE     PEA01                                  60
     C                   ENDDO
     C*
     C                   Z-ADD     SVDIV         XXDIV
     C     EMKEY         CHAIN     PEM01                              60
     C     ELKEY         CHAIN     PEL01                              60
     C*
     C                   ENDIF                                                  ELARSM IFEQ
     C*
     C                   ENDIF                                                  *IN71 IFEQ
     C*** END ADD *****  06/20/05  ************************************
     C                   END                                                    NOT QUOTE
     C*                                                    NOT PRINT BILL
     C*  GET EMPLOYER DETAIL FOR BILLING SCHEDULE
     C*
     C                   MOVE      *BLANKS       XXSTYL            1
     C                   Z-ADD     SCFYR         XXFYR
     C     SCEMP#        IFNE      *ZERO
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              60
     C  N60              MOVEL     BSSEFL        XXSTYL
     C                   END
     C                   END
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      XXFND         QQFND             3
     C                   MOVE      XXFYR         QQFYR             3
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      *BLANK        QQSPRD            1
     C                   CALL      'SRDISC'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQSPRD
     C*
     C*** BEGIN CHG ***  02/19/98  ************************************
     C*********  QQSPRD    IFEQ 'Y'
     C                   MOVE      ' '           QQMCHK            1
     C                   CALL      'SRMCHK'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQMCHK
     C     QQMCHK        IFEQ      'Y'
     C*** END CHG *****  02/19/98  ************************************
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*** BEGIN DEL ***  11/15/99  ************************************
     C**************       MOVEL'SCR02'   Q2RECF
     C**************       GOTO STRT
     C*** END DEL *****  11/15/99  ************************************
     C                   END
     C*** BEGIN ADD ***  05/30/03  ************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT2            7 0
     C*
     C*** BEGIN CHG ***  01/13/04  ************************************
     C*********  EMKEY     CHAINPDB01                60
     C*********  *IN60     IFEQ *OFF
     C*********  XXDAT2    ANDLTXXDATE
      *** BEG ADD ***** 06/27/03 ***********************************************
     C*********  DBSTAT    ANDEQ'A'
      *** END ADD ***** 06/27/03 ***********************************************
     C*** BEG CHG ***** 07/27/10 ************************************
     C*
     C* CHECK IF NEW ACH MODULE IS INSTALLED ('WC1806')
     C*
     C**** BEG   CHG **** 10/22/10 *********************************************
     C*******    XXNACH    IFEQ *ON
     C     XXNACH        IFEQ      *OFF
     C**** END   CHG **** 10/22/10 *********************************************
     C*** BEGIN ADD ****** 12/21/10 ************************************
     C* Set EFT valid flag
     C                   MOVE      '0'           EFTFLG            1
     C*** END   ADD ****** 12/21/10 ************************************
     C     EMKEY         CHAIN     PEF01                              62
     C     *IN62         DOWEQ     *OFF
     C     XXDATE        IFLT      EFEXDT
     C     XXDATE        ANDGE     EFEFDT
     C*** BEGIN ADD ****** 12/21/10 ************************************
     C     EFEXDT        ANDNE     *ZERO
     C     XXDATE        ORGE      EFEFDT
     C     EFEXDT        ANDEQ     *ZERO
     C                   MOVE      '1'           EFTFLG            1
     C                   LEAVE
     C                   ENDIF                                                  XXDATE < EFEXDT
     C     EMKEY         READE     PEF01                                  62
     C                   ENDDO
     C*** END   ADD ****** 12/21/10 ************************************
     C*
     C*** BEGIN DEL ****** 12/21/10 ************************************
     C* FOLLOWING CODE IS RE-WRITTEN BELOW FOR CLARITY
     C*********  *IN87     ANDEQ*OFF
     C*********  Q1PRMS    ANDGT2
     C*********  EFEXDT    OREQ *ZERO
     C*********  XXDATE    ANDGEEFEFDT
     C*********  XXDAT2    ANDLTXXDATE
     C*********  *IN87     ANDEQ*OFF
     C*********  Q1PRMS    ANDGT2
     C*********
     C*********            MOVE *ON       *IN87
     C*********            GOTO STRT
     C*********            LEAVE
     C*********            ENDIF
     C*********  EMKEY     READEPEF01                    62
     C*********            ENDDO
     C*********  *IN87     IFNE *ON
     C*********            MOVE *OFF      *IN87
     C*********            ENDIF
     C*** END   DEL ****** 12/21/10 ************************************
     C* Use EFT valid flag in further edit
     C     EFTFLG        IFEQ      '1'
     C     *IN87         ANDEQ     *OFF
     C     XXDAT2        ANDLT     XXDATE
     C     Q1PRMS        ANDGT     2
     C                   MOVE      *ON           *IN87
     C                   GOTO      STRT
     C                   ELSE                                                   EFTFLG='1'
     C                   MOVE      *OFF          *IN87
     C                   ENDIF                                                  EFTFLG='1'
     C*** BEG CHG ***** 10/22/10 ************************************
     C*******              ENDIF
     C*******    XXNACH    IFNE *ON
     C                   ELSE                                                   XXNACH=*ON
     C*** END CHG ***** 10/22/10 ************************************
     C*** END CHG ***** 07/27/10 ************************************
     C     DBKY          SETGT     PDB01
     C     EMKEY         READPE    PDB01                                  60
     C     *IN60         IFEQ      *OFF
     C     DBEXDT        ANDEQ     *ZERO
     C     XXDAT2        ANDLT     XXDATE
     C     *IN87         ANDEQ     *OFF
     C     Q1PRMS        ANDGT     2
     C     *IN60         OREQ      *OFF
     C     DBEXDT        ANDNE     *ZERO
     C     XXDATE        ANDLT     DBEXDT
     C*** BEGIN ADD ****** 12/21/10 ************************************
     C     XXDAT2        ANDLT     XXDATE
     C*** END   ADD ****** 12/21/10 ************************************
     C*** END   CHG ***  01/13/04  ************************************
     C     *IN87         ANDEQ     *OFF
     C     Q1PRMS        ANDGT     2
     C                   MOVE      *ON           *IN87
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      *OFF          *IN87
     C                   ENDIF
     C*** END ADD *****  05/30/03  ************************************
     C*** BEG CHG ***** 07/27/10 ************************************
     C                   ENDIF
     C*** END CHG ***** 07/27/10 ************************************
     C*** BEG ADD *****  09/27/07  ************************************
     C     *IN72         IFEQ      *ON
     C                   SELECT
     C     SCBAT#        WHENLT    0
     C                   MOVE      *ON           *IN18
     C                   GOTO      STRT
     C     SCBAT#        WHENGT    0
     C     EAKY4         CHAIN     PEA04                              99
     C     *IN99         IFEQ      *ON
     C                   MOVE      *ON           *IN19
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   OTHER
     C                   MOVE      *OFF          *IN18
     C                   MOVE      *OFF          *IN19
     C                   ENDSL
     C                   ENDIF                                                  *IN72 IFEQ *ON
     C*
     C     SCBAT#        IFNE      0
     C     XXCLT#        ANDEQ     '071'
     C                   EXSR      SRBAT
     C                   ENDIF                                                  SCBAT# IFNE 0
     C*** END ADD *****  09/27/07  ************************************
     C*** BEGIN ADD ***  11/15/99  ************************************
     C                   GOTO      SUBMIT
     C*** END ADD *****  11/15/99  ************************************
     C*
     C*   PROCESS REQUEST SCREEN 2
     C*
     C     SCR02T        TAG
     C*** BEGIN ADD ***  11/15/99  ************************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C     SUBMIT        TAG
     C*** END ADD *****  11/15/99  ************************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C*** BEGIN ADD ***  10/01/99  ************************************
     C     QQCALL        IFEQ      '2'
     C     QQQTE         ANDEQ     'Q'
     C     FNKEY         CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EXESCD        SCESCD
     C                   MOVE      EXSRCD        SCSRCD
     C                   MOVE      EXDACD        SCAJCD
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  QQCALL IFEQ
     C*** END ADD *****  10/01/99  ************************************
     C                   MOVE      SCDIV         QQDIV             5
     C                   MOVE      SCDATE        QQDATE            8
     C                   MOVE      SCDUE         XXDUE             8 0
     C                   MOVE      XXDUE         QQDUE             8
     C                   MOVE      *BLANKS       QQOVR             1
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C*** BEGIN ADD ***  11/13/03  ************************************
     C                   MOVEL     SCFCM1        QQFCM1
     C                   MOVEL     SCFCM2        QQFCM2
     C                   MOVEL     SCFCM3        QQFCM3
     C*** END ADD *****  11/13/03  ************************************
     C*** BEGIN ADD ***  06/20/05  ************************************
     C     *IN71         IFEQ      *ON
     C                   MOVE      '#'           SCRNB
     C                   ENDIF
     C*** END ADD *****  06/20/05  ************************************
     C                   CALL      'W8001C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQDATE
     C                   PARM                    SCESCD
     C                   PARM                    SCCOM1
     C                   PARM                    SCCOM2
     C                   PARM                    SCCOM3
     C                   PARM                    QQDUE
     C                   PARM                    SCDEP
     C                   PARM                    XXSTYL
     C                   PARM                    QQCALL
     C                   PARM                    QQOVR
     C                   PARM                    SCSRCD
     C                   PARM                    SCAJCD
     C                   PARM                    QQQTE
     C                   PARM                    QQOUTQ
     C                   PARM                    SCRNB
     C                   PARM                    SCDART
     C                   PARM                    SCHARD
     C                   PARM                    SCAGTO
     C                   PARM                    SCFQT
     C                   PARM                    SCRMCD
     C*** BEGIN ADD ***  11/15/99  ************************************
     C                   PARM      '  '          QQAR              2
     C                   PARM      '00000000'    QQEFDT            8
     C*** END ADD *****  11/15/99  ************************************
     C*** BEGIN ADD ***  11/13/03  ************************************
     C                   PARM                    QQFCM1           30
     C                   PARM                    QQFCM2           30
     C                   PARM                    QQFCM3           30
     C*** END ADD *****  11/13/03  ************************************
     C********* Beg Add ********** 02/08/13 **********************************
     C                   PARM      '  '          QQAR2             2
     C********* End Add ********** 02/08/13 **********************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*** BEG ADD *****  09/27/07  ************************************
      *
      *===============================================================
      * SRBAT Subroutine
      *===============================================================
      *
     C     SRBAT         BEGSR
      *
     C     SCEMP#        IFEQ      0
      *
     C     SCRNB         IFEQ      'B'
     C     BLKY          CHAIN     PBL01                              99
     C                   ELSE
     C     BLKY2         CHAIN     PBL02                              99
     C                   ENDIF                                                  SCRNB IFEQ 'B'
      *
     C     *IN99         DOWEQ     *OFF
      *
     C     BLFLG1        IFNE      *BLANK
     C     BLFLG3        ORNE      *BLANK
     C                   Z-ADD     SCBAT#        BLARQ#
      *
     C     SCRNB         IFEQ      'B'
     C                   UPDATE    PBL01
     C                   ELSE
     C                   UPDATE    PBL02
     C                   ENDIF                                                  SCRNB IFEQ 'B'
      *
     C                   ENDIF                                                  BLFLG1 IFNE *BLANK
      *
     C     SCRNB         IFEQ      'B'
     C     BLKY          READE     PBL01                                  99
     C                   ELSE
     C     BLKY2         READE     PBL02                                  99
     C                   ENDIF                                                  SCRNB IFEQ 'B'
      *
     C                   ENDDO                                                  *IN99 DOWEQ *OFF
      *
     C                   ELSE
      *
     C     *IN71         IFEQ      *ON
     C     ELKY2         CHAIN     PEL03                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     SCBAT#        ELAMIN
     C                   UPDATE    PEL03
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C                   ELSE
     C     ELKY1         CHAIN     PEL03                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     SCBAT#        ELAMIN
     C                   UPDATE    PEL03
     C     ELKY1         READE     PEL03                                  99
     C                   ENDDO                                                  *IN99 DOWEQ *OFF
     C                   ENDIF                                                  *IN71 IFEQ *ON
     C                   ENDIF                                                  SCEMP# IFEQ 0
      *
     C                   ENDSR
     C*** END ADD *****  09/27/07  ************************************
