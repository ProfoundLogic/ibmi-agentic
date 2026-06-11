     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7710R - PROMPT FOR FINAL AUDIT
     H*****************************************************************
     H*  CALLED FROM:                                                 *
     H*      MENU                                                     *
     H*      W0051R - WORK WITH EMPLOYER POLICY YEARS                 *
     H*  CALLS TO:                                                    *
     H*      W7710C                                                   *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/31/12 C:65  Project : WC2116                               *
     H*                Default checkbox to N to avoid error message.  *
     H*                                                               *
     H* 06/16/10 C:255 Project :C71WKR - For client id 71             *
     H*                         Default 'Run audit for single divisi- *
     H*                         on only' to'Y' and Default 'Revised - *
     H*                         Audit' to 'N'                         *
     H*                                                               *
     H* 03/06/10 C:180 FURTHER QUALIFY EDIT - APPLICABLE TO SINGLE DIV.
     H*                SUPPORT 71996
     H*                                                               *
     H* 02/25/10 C:33  Project: 25WRC  Priss #13                      *
     H*                Process Retro independently of Large Deductible*
     H*                                                               *
     H* 02/18/10 C:191 PROJECT: WC1838  PRISS                         *
     H*                MOVE WITH PADDING TO XXKEY - IT IS REATINING   *
     H*                OLD VALUE WHEN USING FOR A/R CODE VALIDTION    *
     H*                                                               *
     H* 01/13/10 C:136 PROJECT: WC1838                                *
     H*                EDIT TO ENSURE THAT USER HAS AUTHORITY TO RUN  *
     H*                A BILL ON A SINGLE POLICY WITH LARGE           *
     H*                DECUCTIBLE                                     *
     H*                                                               *
     H* 06/01/09 C:180 SUPPORT 67409
     H*                Do not allow audit bill for an unexpired term
     H*                when employer # is entered.
     H*
     H* 03/11/09 C:61  PROJECT WC1630                                 *
     H*                Add prompt: "Include deductible balance (Y/ )" *
     H*                                                               *
     H* 02/03/09 C:33  Project: 146WBW                                *
     H*                Do not default DARTS print if running for 3rd  *
     H*                 party billing.                                *
     H*                                                               *
     H*  05/28/08 C:136 SUPPORT: WA139 63131                          *
     H*                 ENABLE SINGLE DIVISION BILLING FOR GIC        *
     H*                                                               *
     H*  09/27/07 C:183 PROJECT: 71WIA                                *
     H*                 Modify Audit Bill prompt to always provide    *
     H*                 input of DARTS code and resolve flag regard   *
     H*                 less of the type of Audit Bill run.           *
     H*                                                               *
     H*  08/13/07 C:65  PROJECT: 71WHL                                *
     H*                 ADD CHECK(ER) TO SCFAFL FIELD TO ENSURE AUTO  *
     H*                 POPULATE WORKS WITHOUT HITTING ENTER.         *
     H*                                                               *
     H*  08/01/07 C:65  PROJECT: 71WHL                                *
     H*                 REMOVE THE CLIENT ID CHECK FOR THIS PROJECT   *
     H*                 EXCEPT FOR THE GROUP 5 PRINT ONLY DARTS CHG.  *
     H*                                                               *
     H*  05/04/07 C:136 SUPPORT: WA071 57986                          *
     H*                 PREVENT CODE IN 71WHL FROM AFFECTING THE      *
     H*                 DEFAULT PRINTER.                              *
     H*                                                               *
     H*  11/27/06 C:136 PROJECT: 71WHL                                *
     H*                 ADD PROMPTS FOR AUDIT DARTS AND STAFF CODE    *
     H*                 ADD CLIENT SPECIFIC CODE TO ENABLE USER       *
     H*                 TO RESOLVE 'PRINT ONLY' DOC TO DARTS          *
     H*                                                               *
     H*  11/15/06  C:132 Project: 71WHM Mass Audit-Same options as    *
     H*                  Single Audit.                                *
     H*                - Add menu options to call W7710R in correct   *
     H*                  mode: Print, Calc & Print, Calc,Print,Update *
     H*                - Change check on Q1PARMS=0 to Check relative  *
     H*                  Record# = 0, in multiple places.             *
     H*                                                               *
     H*  10/11/06  C:33  Seq#: WA099/41801                            *
     H*                  Use SC305R instead of SC580R.                *
     H*                                                               *
     H*  09/20/06 C:136 PROJECT: 71WHE (BACK TO BASE PHASE 1)         *
     H*                 REMOVE PREMIUM NOTICE FLAG FROM PROMPT, THIS  *
     H*                 WILL BE FLAGGED IN BILLING OPTIONS            *
     H*                                                               *
     H*  09/11/06 C:65 PROJECT: SUPP# 54436                           *
     H*                ADD ERROR MESSAGE TO PREVENT AN AUDIT FROM     *
     H*                RUNNING WHEN A/R SUMMARY FLAG IS SET ON A CX'D *
     H*                DIVISION AND CX DATE DOES NOT MATCH STATE      *
     H*                REPORTING DIVISION.                            *
     H*                                                               *
     H*  01/24/06 C:65 PROJECT: 114WHO                                *
     H*                ADD A MESSAGE WINDOW FOR RUNNING AN AUDIT ON   *
     H*                A POLICY WHICH HAS ALREADY RUN A SETTLEMENT.   *
     H*                                                               *
     H*  01/18/06 C:65 PROJECT: SUPP# 50990                           *
     H*                DEFAULT EXPIRATION DATE FOR FLAT CX AUDITS     *
     H*                WHEN DOING THE AUDIT PAYROLL CHECK.            *
     H*                                                               *
     H*  01/13/06 C:65 PROJECT: 71WGH/WC1327                          *
     H*                ADD FIELD FOR CALLING AUDIT FOR A SINGLE       *
     H*                DIVISION ONLY.  IF CALLED FOR A SINGLE         *
     H*                DIVISION THEN CHECK FOR AUDITED PAYROLLS ON    *
     H*                THE DIVISION ONLY INSTEAD OF ALL DIVISIONS.    *
     H*                THIS WILL BE DONE FOR CLIENT 71 ONLY.          *
     H*                                                               *
     H*  01/12/06 C:65 PROJECT: 71WGH                                 *
     H*                ADD FIELD FOR PRINTING CONTRIBUTION NOTICES.   *
     H*                THIS WILL BE DONE FOR CLIENT 71 ONLY.          *
     H*                                                               *
     H*  02/15/05 C:65 PROJECT: WC1167                                *
     H*                ADD THREE FREE FORM COMMENT FIELDS.  THESE     *
     H*                FIELDS WILL BE MUTUALLY EXCLUSIVE WITH THE     *
     H*                COMMENT CODE FIELDS.                           *
     H*                                                               *
     H*  11/30/04 C:65 PROJECT: 132WBW                                *
     H*                USE ACCOUNT CURRENT INTERIM DAYS FOR ACCOUNT   *
     H*                CURRENT POLICIES.                              *
     H*                                                               *
     H*  12/19/00 C:65 PROJECT: SUPPORT                               *
     H*                DO NOT ALLOW POSTING DATES IN PRIOR MONTHS.    *
     H*                                                               *
     H*  06/08/00 C:65 PROJECT: SUPPORT                               *
     H*                CORRECT CANCELLATION DATE CHECK.               *
     H*                                                               *
     H*  05/26/00 C:65 PROJECT: WC936                                 *
     H*                CORRECT PAYROLL CHECK EDIT FOR MENU PROMPT.    *
     H*                                                               *
     H*  11/15/99 C:65 PROJECT: WC936                                 *
     H*                WHEN RUNNING A BILL FOR A SINGLE POLICY WITH   *
     H*                MULTIPLE DIVISIONS, GIVE THE MESSAGE THAT THE  *
     H*                BILL WILL BE RUN FOR ALL DIVISIONS.            *
     H*                                                               *
     H*  05/26/99 C:65 PROJECT: SUPPORT# 18244                        *
     H*                ONLY DEFAULT THE A/R CODE IN UPDATE MODE.      *
     H*                                                               *
     H*  04/22/99 C:90 PROJECT: WC886                                 *
     H*                DEFAULT A/R CODE TO WMEXP/EXAUCD.              *
     H*                                                               *
     H*  11/23/98 C:65 PROJECT: WC705                                 *
     H*                ALLOW DARTS REMARK CODE TO BE OPTIONAL.        *
     H*                                                               *
     H*  09/14/98 C:65 PROJECT: WC705                                 *
     H*                MADE SLIGHT ALTERATIONS TO WORK WITH CHECK(ER) *
     H*                                                               *
     H*  07/31/98 C:65 PROJECT: WC705                                 *
     H*                ONLY HAVE A DARTS CODE IF OFFICE IS INSTALLED  *
     H*                AND WE ARE IN CALC,PRT,& UPD MODE.             *
     H*                                                               *
     H*  04/30/98 C:87 PROJECT: WC630                                 *
     H*                ADD CODE TO CHECK THE VALUE OF THE 'REVISED    *
     H*                AUDIT FLAG' BEFORE THE USER PRESSES ENTER. IF  *
     H*                A 'Y' IS ENTERED THEN CHANGE THE DARTS CODE TO *
     H*                THE 'REVISED FINAL AUDIT' REMARK CODE INSTEAD  *
     H*                OF THE 'FINAL AUDIT' REMARK CODE CURRENTLY     *
     H*                DISPLAYED FROM ADMIN/DARTS FILE.               *
     H*                                                               *
     H*  03/31/98 C:65 PROJECT: WC705                                 *
     H*                ADD PRINT ONLY AND CALC & PRINT ONLY VERSIONS. *
     H*                MODE = 1 PRINT ONLY                            *
     H*                MODE = 2 CALC & PRINT                          *
     H*                MODE = 3 CALC, UPDATE, & PRINT                 *
     H*                ONLY ALLOW DARTS AND A/R CODE ON MODE 3.       *
     H*                                                               *
     H*  03/23/98 C:65 PROJECT: WC705                                 *
     H*                ADD SECURITY CHECK FOR SUBMISSION OF MASS BILLS*
     H*                                                               *
     H*  02/19/98 C:65 PROJECT: WC705                                 *
     H*                CHANGE SPREAD DISC MESSAGE FOR MULTIPLE DIVS.  *
     H*                                                               *
     H*  12/11/97 C:85 PROJECT: WC577                                 *
     H*                CHANGED SEARCHES TO NOT BLANK VALUE IN FIELD   *
     H*                IF CANCEL (F12) IS PRESSED ON SEARCH SCREEN.   *
     H*                ALSO, CHANGED SEARCHES TO LEAVE FIELD BLANK    *
     H*                IF THE FIELD WAS BLANK BEFORE BEGINNING SEARCH,*
     H*                AND TO POSITION CURSOR TO APPROPRIATE FIELDS.  *
     H*                                                               *
     H*  11/07/97 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 SCREEN STANDARDS.                    *
     H*                CHANGED POSTING DATE TO ADD INTERIM DAYS.      *
     H*                ADDED EMPLOYER SEARCH.                         *
     H*                DELETED REFERENCES TO SCR99.                   *
     H*                                                               *
     H*  11/05/97 C:85 PROJECT: WC764                                 *
     H*                ADDED DIVISION COUNT.                          *
     H*                                                               *
     H*  05/29/96 C:33 PROJECT WC420                                  *
     H*                IF THIS REPORT IS CALLED ON DEMAND IT SHOULD   *
     H*                BE ROUTED TO THE USER PARM OUTQ INSTEAD OF THE *
     H*                OVERRIDE OUTQ.                                 *
     H*                                                               *
     H*  01/31/94 C:09 PROJECT WC303                                  *
     H*                CORRECT EDIT THAT CHECKS TO SEE THAT ALL       *
     H*                AUDITED PAYROLLS HAVE BEEN ENTERED BEFORE      *
     H*                ALLOWING AN AUDIT ON THE REQUESTED EMPLOYER.   *
     H*                                                               *
     H*  08/04/92 KWK  PROJECT WC158                                  *
     H*                MAKE SURE ALL AUDITED PAYROLLS HAVE BEEN       *
     H*                ENTERED BEFORE ALLOWING THE AUDIT TO BE        *
     H*                REQUESTED IF AUDIT IS BEING RUN FOR ONE        *
     H*                EMPLOYER.                                      *
     H*                                                               *
     H*  01/20/92 LPK  PROJECT WC111                                  *
     H*                ADDED AN INFORMATIONAL SCREEN AND A FLAG TO    *
     H*                SCR01 TO CHECK IF EMPLOYER USES SPREAD DISCOUNT*
     H*                CALCULATIONS.                                  *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     FW7710F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWDELP     IF   E             DISK
     F                                     INFDS(DBFDS)
     F*
     F*** BEGIN ADD *** 11/05/97 **************************************
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL03)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     F*** END   ADD *** 11/05/97 **************************************
     F*
     FWTABL     IF   E           K DISK
     F*** BEGIN ADD ***  07/31/98  ************************************
     F                                     IGNORE(PSY01)
     F*** END ADD *****  07/31/98  ************************************
     FSTCTL1    IF   E           K DISK
     FWMEML     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F*
     F*** BEGIN ADD *** 11/07/97 **************************************
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     F*** END   ADD *** 11/07/97 **************************************
     F*
     FSMCOL     IF   E           K DISK
     F*** BEGIN ADD ***  07/31/98  ************************************
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
     F*** END ADD *****  07/31/98  ************************************
     FWMUPL     IF   E           K DISK
     FWMEPL1    IF   E           K DISK
     FWMEXL     IF   E           K DISK
      *** BEG ADD ***** 11/27/06 *******************************
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)
     FWMAXL     IF   E           K DISK
      *** END ADD ***** 11/27/06 *******************************
      ***** BEG ADD ******* 03/11/09 **********************************
      *
      * Group Detail
     FWDF1L     IF   E           K DISK
      ***** END ADD ******* 03/11/09 **********************************
     F*** BEG ADD ***** 01/13/10 ****************************************
     FWMLDL1    IF   E           K DISK
     F*** END ADD ***** 01/13/10 ****************************************

     D*** BEGIN ADD ***  11/15/99  ************************************
     D*** END ADD *****  11/15/99  ************************************
     D*
     D OPTNM           C                   CONST('Audit Bills')
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D*** BEGIN ADD *** 11/07/97 **************************************
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D*** END   ADD *** 11/07/97 **************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*** BEGIN ADD ***  06/08/00  ************************************
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECTIVE DATES
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATES
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D*** END ADD *****  06/08/00  ************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      ***** BEG ADD ******* 03/11/09 **********************************
      *
      *  Group Detail
      *

     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
      ***** END ADD ******* 03/11/09 **********************************
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*** BEGIN ADD ***  07/31/98  ************************************
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*** END ADD *****  07/31/98  ************************************
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  EMPLOYER PAYROLL FILE
     C*
     C     EPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCEMP#
     C*** BEGIN CHG ***  11/15/99  ************************************
     C***********          KFLD           SCDIV
     C                   KFLD                    X1DIV
     C*** END CHG *****  11/15/99  ************************************
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*** BEGIN ADD ***  11/15/99  ************************************
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    X1DIV
     C*** END ADD *****  11/15/99  ************************************
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCFYR
     C*
     C*** BEGIN ADD *** 11/05/97 **************************************
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE (TO COUNT DIVISIONS)
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
     C*** END   ADD *** 11/05/97 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
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
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
      *** BEG ADD ***** 11/27/06 **********************************
     C     UPRMPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN1            9 0
     C                   PARM      3             ##VIEW            1 0
      *** END ADD ***** 11/27/06 **********************************
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    UPFYR         XXFYR
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    BCKEY         XXKEY
     C*
     C*** BEGIN ADD *** 01/12/06 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C                   MOVE      *OFF          *IN71
     C     CLTID         IFEQ      '071'
      *** BEG ADD ***** 05/28/08 **************************************
     C     CLTID         OREQ      '139'
      *** END ADD ***** 05/28/08 **************************************
     C                   MOVE      *ON           *IN71
     C                   ENDIF
     C*** END ADD ***** 01/12/06 **************************************
     C*
     C*** begin add *** 10/31/12 **************************************
     C                   MOVE      'N'           SCREV
     C*** end add ***** 10/31/12 **************************************
     C*** BEG ADD ***** 06/16/10 **************************************
     C     CLTID         IFEQ      '071'
     C                   MOVEL     'Y'           SCSING
     C                   MOVE      'N'           SCREV
     C                   ENDIF
     C*** BEG ADD ***** 06/16/10 **************************************
     C*
     C*** BEGIN ADD ***  03/31/98  ************************************
     C     Q1PRMS        IFGT      *ZERO
     C                   MOVE      @@MODE        QQMODE            1
      ***** BEG ADD ***** 11/15/06 ************************************
     C                   Z-ADD     @@RRN         XXRRN             9 0
      ***** END ADD ***** 11/15/06 ************************************
     C                   ELSE
      ***** BEG ADD ***** 11/15/06 ************************************
     C                   Z-ADD     *ZEROS        XXRRN             9 0
      ***** END ADD ***** 11/15/06 ************************************
     C                   MOVE      '3'           QQMODE
     C                   ENDIF
     C*
     C     QQMODE        IFEQ      '3'
     C                   MOVE      *ON           *IN98
     C                   ELSE
     C                   MOVE      *OFF          *IN98
     C                   ENDIF                                                  @@MODE IFEQ
     C*** END ADD *****  03/31/98  ************************************
     C*
     C*** BEG ADD *****  11/27/06  ************************************
     C*** BEGIN DEL ***  08/01/07  ************************************
     C*********  CLTID     IFEQ '071'
     C*** END DEL *****  08/01/07  ************************************
     C     QQMODE        IFEQ      '2'
     C                   MOVE      *ON           *IN97
     C                   ELSE
     C                   MOVE      *OFF          *IN97
     C                   ENDIF                                                  @@MODE IFEQ
     C*** BEGIN DEL ***  08/01/07  ************************************
     C*********            ENDIF                           CLTID = '071'
     C*** END DEL *****  08/01/07  ************************************
     C*** END ADD *****  11/27/06  ************************************
     C*** BEGIN ADD ***  07/31/98  ************************************
     C*
     C*  CHECK IF OFFICE IS INSTALLED.
     C                   MOVEL     'SY'          XXSYK
     C     SYKY          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *ON
     C     SYOFIF        ORNE      'Y'
     C*** BEG DEL *****  09/27/07  ************************************
     C********** QQMODE    ORNE '3'
     C*** END DEL *****  09/27/07  ************************************
     C                   MOVE      *ON           *IN58
     C                   ELSE
     C                   MOVE      *OFF          *IN58
     C                   ENDIF
     C*** END ADD *****  07/31/98  ************************************
     C*** BEGIN ADD *** 11/05/97 **************************************
     C*
     C*  DEFINE DIVISION COUNT
     C*
     C                   Z-ADD     0             SCDIVC            3 0          COUNT DIVISIONS
     C*** END   ADD *** 11/05/97 **************************************
     C*
     C*** BEGIN ADD *** 12/11/97 **************************************
     C*
     C*  DEFINE DARTS REMARK CODE PARM
     C*
     C                   MOVE      *BLANKS       QQRMCD            5            REMARK CODE PAR
     C*** END   ADD *** 12/11/97 **************************************
     C*
     C     STRT01        TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C                   Z-ADD     UPCO#         XXCO#
     C                   Z-ADD     UPFND         XXFND
     C                   Z-ADD     UPFYR         SCFYR
     C                   Z-ADD     UPEMP#        SCEMP#
     C                   Z-ADD     UPDIV         SCDIV
     C*** BEGIN ADD ***  11/30/04  ************************************
     C                   MOVE      *BLANK        XXACCT            1
     C*** END ADD *****  11/30/04  ************************************
     C*** BEG ADD ***** 05/04/07 **************************************
     C                   MOVEL     UPOUTQ        XXOUTQ           10
     C                   MOVEL     UPLVL         XXLVL             3
     C                   MOVEL     UPTYPE        XXTYPE            1
     C*** END ADD ***** 05/04/07 **************************************
     C*
     C     Q1PRMS        IFGT      *ZERO
      ***** BEG ADD ***** 11/15/06 ************************************
     C     XXRRN         ANDGT     *ZERO
      ***** END ADD ***** 11/15/06 ************************************
      *
      ***** BEG ADD ***** 11/15/06 ************************************
      ***** END ADD ***** 11/15/06 ************************************
     C*
     C     @@RRN         CHAIN     WDELP                              60
     C*
     C                   Z-ADD     ELCO#         XXCO#
     C                   Z-ADD     ELFND         XXFND
     C                   Z-ADD     ELFYR         XXFYR
     C                   Z-ADD     ELFYR         SCYR
     C                   Z-ADD     ELFYR         SCFYR
     C                   Z-ADD     ELEMP#        SCEMP#
     C                   Z-ADD     ELDIV         SCDIV
     C*** BEGIN ADD ***  11/15/99  ************************************
     C                   Z-ADD     ELPTDT        SVPTDT            7 0
     C*** END ADD *****  11/15/99  ************************************
      *** BEG ADD **** 11/27/06 ***************************************
     C*** BEG DEL **** 09/27/07 ***************************************
     C********** CLTID     IFEQ '071'
     C********** QQMODE    ANDEQ'1'
     C********** XXFND     ANDEQ5
     C**********           MOVE *ON       *IN98
     C**********           MOVE *OFF      *IN58
     C**********           ENDIF
     C*** END DEL **** 09/27/07 ***************************************
      *** END ADD **** 11/27/06 ***************************************
     C*** BEGIN ADD ***  11/30/04  ************************************
     C                   Z-ADD     XXCO#         @@CO#
     C                   Z-ADD     XXFND         @@FND
     C                   Z-ADD     SCEMP#        @@EMP#
     C                   Z-ADD     SCDIV         @@DIV
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
     C     EMKY          CHAIN     PEM01                              60
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
     C*
     C                   ENDIF                                                  Q1PRMS>*ZERO
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C*
     C* DEFAULT POSTING DATE TO BILLING START DATE IF CALLED FROM
     C* WORK-WITH, ELSE DEFAULT TO CURRENT SYSTEM DATE.
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*** BEGIN ADD ***  11/30/04  ************************************
     C     XXACCT        IFEQ      'Y'
     C                   Z-ADD     EXAAID        XXDAYS            5 0
     C                   ELSE
     C                   Z-ADD     EXAUID        XXDAYS
     C                   ENDIF
     C*** END ADD *****  11/30/04  ************************************
     C*
     C*** BEGIN ADD *** 11/07/97 **************************************
     C*
     C* POSTING DATE (AUDIT INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C*** BEGIN ADD ***  11/30/04  ************************************
     C*********            PARM EXAUID    QQDAYS  50
     C                   PARM      XXDAYS        QQDAYS            5 0
     C*** END ADD *****  11/30/04  ************************************
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SCDATE
     C                   ENDIF
     C*** END   ADD *** 11/07/97 **************************************
     C*
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         SCDATE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     FNKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C*** BEGIN ADD ***  03/31/98  ************************************
     C*** BEG DEL *****  09/27/07  ************************************
     C********** *IN98     ANDEQ*ON
     C*** END DEL *****  09/27/07  ************************************
     C*** END ADD *****  03/31/98  ************************************
     C*** BEGIN ADD ***  07/31/98  ************************************
     C     *IN58         ANDEQ     *OFF
     C*** END ADD *****  07/31/98  ************************************
     C                   MOVE      EXAUD         SCRMCD
     C                   ELSE
     C                   MOVE      *BLANKS       SCRMCD
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C***  Begin Add  ***  02/03/09  **************************************
     C     EX3RDB        IFEQ      'Y'
     C                   CLEAR                   SCRMCD
     C                   ENDIF
     C***  End   Add  ***  02/03/09  **************************************
     C*
      **** BEGIN ADD **** 04/22/99 ************************************
     C     *IN60         IFEQ      *OFF
     C*** BEGIN ADD ***  05/26/99  ************************************
     C     *IN98         ANDEQ     *ON
     C*** END ADD *****  05/26/99  ************************************
     C                   MOVE      EXAUCD        SCCODE
     C                   ENDIF                                                  *IN60 IFEQ
      **** END   ADD **** 04/22/99 ************************************
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   MOVEL(P)  OPTNM         SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C*
      ***** BEG CHG ***** 11/15/06 ************************************
     C********   Q1PRMS    COMP *ZERO                99
     C     XXRRN         COMP      *ZERO                              99
      ***** END CHG ***** 11/15/06 ************************************
     C*
     C*** BEGIN ADD *** 11/05/97 **************************************
     C*
     C*  POSITION CURSOR AT FIRST FIELD
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   SETON                                        91
     C*
     C*  BEGIN CALCULATION FOR DIVISION COUNT
     C*
     C                   Z-ADD     0             SCDIVC                         COUNT DIVISIONS
     C*
     C     ELKY1         CHAIN     PEL03                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ELPFDT        IFNE      ELPTDT
     C                   ADD       1             SCDIVC
     C                   ENDIF                                                  ELPFDT<>ELPTDT
     C     ELKY1         READE     PEL03                                  60
     C                   ENDDO                                                  *IN60 = OFF
     C*** BEGIN ADD ***  05/26/00  ************************************
     C     ELKY          CHAIN     PEL02                              60
     C*** END ADD *****  05/26/00  ************************************
     C*
     C*  END CALCULATION FOR DIVISION COUNT
     C*
     C                   ELSE
     C*
     C                   SETON                                        92
     C*
     C                   ENDIF                                                  *IN99 = ON
     C*
     C*** END   ADD *** 11/05/97 **************************************
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*
     C                   MOVE      *BLANK        MULTI             1
     C*
     C     SCDIVC        IFGT      1
     C                   MOVE      'Y'           MULTI
     C                   ENDIF
     C*
     C     SCDIVC        IFGT      1
     C*
     C                   Z-ADD     1             XXX               1 0
     C                   Z-ADD     0             SVCNDT            7 0
     C                   DO        5
     C     CD(XXX)       IFGE      ELPFDT
     C     CD(XXX)       ANDLE     ELPTDT
     C     ED(XXX)       ANDNE     0
     C                   MOVE      CD(XXX)       SVCNDT
     C                   LEAVE
     C                   ENDIF
     C                   ADD       1             XXX
     C                   ENDDO
     C*
     C     SVCNDT        IFNE      *ZERO
     C                   Z-ADD     SVCNDT        SVPTDT
     C                   ENDIF
     C*
     C*** BEGIN ADD ***  01/18/06  ************************************
     C*
     C* DEFAULT POLICY TO DATE FOR FLAT CANCELLATIONS.
     C*
     C     SVCNDT        IFEQ      *ZERO
     C*
     C                   Z-ADD     1             XXX               1 0
     C                   DO        5
     C     CD(XXX)       IFLT      ELPFDT
     C     CD(XXX)       ANDNE     0
     C     ED(XXX)       ANDNE     0
     C                   Z-ADD     ELPFDT        SVPTDT
     C                   LEAVE
     C                   ENDIF
     C                   ADD       1             XXX
     C                   ENDDO
     C*
     C                   ENDIF                                                  SVCNDT IFEQ
     C*** END ADD *****  01/18/06  ************************************
     C*
     C                   ENDIF                                                  SCDIVC
     C*** END ADD *****  11/15/99  ************************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C*** BEGIN ADD ***  11/15/99  ************************************
     C     MULTI         IFEQ      'Y'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   ELSE
     C*** END ADD *****  11/15/99  ************************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*** BEGIN ADD ***  11/15/99  ************************************
     C                   ENDIF                                                  SINGLE IFEQ
     C*** END ADD *****  11/15/99  ************************************
     C*** BEG ADD *****  11/27/06  ************************************
     C                   MOVE      *BLANKS       SVFAFL            1
     C*** END ADD *****  11/27/06  ************************************
      ***** BEG ADD ******* 03/11/09 **********************************
     C     F1KY          CHAIN     PF101                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      F1IDBD        SCIDBD
     C                   ENDIF
      *
      ***** END ADD ******* 03/11/09 **********************************
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C*** BEGIN ADD *** 04/30/98 *************************************
     C                   MOVE      SCREV         XXREV             1
     C*** END   ADD *** 04/30/98 *************************************
     C*
     C*** BEGIN ADD *** 11/07/97 **************************************
     C*
     C*  FORMAT POSTING DATE IF CHANGED BY USER
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         SCDATE
     C*** END   ADD *** 11/07/97 **************************************
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   ENDIF                                                  Q2RECF=SCR02
     C*
     C*  PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*  F3=EXIT
     C*
     C     *INKC         IFEQ      *ON
     C   99              MOVE      *INKC         @@KC
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC=*ON
     C*
     C*  F12=CANCEL
     C*
     C     *INKL         IFEQ      *ON
     C*
     C*** BEGIN DEL ***  11/15/99  ************************************
     C*******              SELEC
     C*******    Q2RECF    WHEQ 'SCR01'
     C*** END DEL *****  11/15/99  ************************************
     C   99              MOVE      *INKL         @@KL
     C                   GOTO      TERM
     C*** BEGIN DEL ***  11/15/99  ************************************
     C*******    Q2RECF    WHEQ 'SCR02'
     C*******              MOVEL'SCR01'   Q2RECF    P
     C*******              GOTO STRT
     C*******              ENDSL
     C*** END DEL *****  11/15/99  ************************************
     C*
     C                   ENDIF                                                  *INKL=*ON
     C*
     C*  F10=SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C*** BEGIN ADD *** 11/07/97 **************************************
     C*
     C* EMPLOYER # SEARCH
     C*
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##MODE            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      DBRRNF        ##RRN2            9 0
     C                   PARM      'W'           ##QTE             1
     C                   PARM      1             ##VIEW            1 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     ##RRN         CHAIN     PEM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C*
     C                   MOVE      *ON           *IN91
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN93
     C*
     C                   ENDIF                                                  *INKL IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C*** END   ADD *** 11/07/97 **************************************
     C*
     C*  F10 - TABLE FILE SEARCH - COMMENT 1
     C*
     C     HFLD          IFEQ      'SCCOM1'
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C*
     C                   CALL      'W2191R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY            10
     C*
     C*** BEGIN DEL *** 12/11/97 **************************************
     C**********           MOVELSCKEY     SCCOM1
     C*** END   DEL *** 12/11/97 **************************************
     C                   MOVE      '1'           *IN28
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C*
     C                   SETON                                        34
     C*
     C                   ELSE
     C*
     C*** BEGIN ADD *** 12/11/97 **************************************
     C                   MOVEL     SCKEY         SCCOM1
     C*** END   ADD *** 12/11/97 **************************************
     C                   SETON                                        35
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  F10 - TABLE FILE SEARCH - COMMENT 2
     C*
     C     HFLD          IFEQ      'SCCOM2'
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C*
     C                   CALL      'W2191R'
     C                   PARM      '2'           @@CALL
     C     *INKC         PARM                    QQKC
     C     *INKL         PARM                    QQKL
     C                   PARM                    SCKEY
     C*
     C*** BEGIN DEL *** 12/11/97 **************************************
     C**********           MOVELSCKEY     SCCOM2
     C*** END   DEL *** 12/11/97 **************************************
     C                   MOVE      '1'           *IN29
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C*
     C                   SETON                                        35
     C*
     C                   ELSE
     C*
     C*** BEGIN ADD *** 12/11/97 **************************************
     C                   MOVEL     SCKEY         SCCOM2
     C*** END   ADD *** 12/11/97 **************************************
     C                   SETON                                        36
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  F10 - TABLE FILE SEARCH - COMMENT 3
     C*
     C     HFLD          IFEQ      'SCCOM3'
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C*
     C                   CALL      'W2191R'
     C                   PARM      '2'           @@CALL
     C     *INKC         PARM                    QQKC
     C     *INKL         PARM                    QQKL
     C                   PARM                    SCKEY
     C*
     C*** BEGIN DEL *** 12/11/97 **************************************
     C**********           MOVELSCKEY     SCCOM3
     C*** END   DEL *** 12/11/97 **************************************
     C                   MOVE      '1'           *IN30
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C*
     C                   SETON                                        36
     C*
     C                   ELSE
     C*
     C*** BEGIN ADD *** 12/11/97 **************************************
     C                   MOVEL     SCKEY         SCCOM3
     C*** END   ADD *** 12/11/97 **************************************
     C                   SETON                                        37
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  F10 - TABLE FILE SEARCH - A/R TRANSACTION CODE
     C*
     C     HFLD          IFEQ      'SCCODE'
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C*
     C                   CALL      'W2161R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY            10
     C*
     C*** BEGIN DEL *** 12/11/97 **************************************
     C**********           MOVELSCKEY     SCCODE
     C*** END   DEL *** 12/11/97 **************************************
     C*
     C*** BEGIN ADD *** 12/11/97 **************************************
     C                   MOVE      *ON           *IN82
     C*** END   ADD *** 12/11/97 **************************************
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C*
     C                   SETON                                        37
     C*
     C                   ELSE
     C*** BEGIN ADD *** 12/11/97 **************************************
     C                   MOVEL     SCKEY         SCCODE
     C*** END   ADD *** 12/11/97 **************************************
     C*
     C*** BEGIN ADD *** 11/05/97 **************************************
     C                   SETON                                        83
     C*** END   ADD *** 11/05/97 **************************************
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  F10 - TABLE FILE SEARCH - DEFAULT DARTS REMARK CODE
     C*
     C     HFLD          IFEQ      'SCRMCD'
     C*
     C*** BEGIN CHG *** 12/11/97 **************************************
     C**********           MOVEL*BLANKS   SCRMCD
     C                   MOVEL     *BLANKS       QQRMCD
     C*** END   CHG *** 12/11/97 **************************************
     C***  Begin Chg  ***  10/11/06  **************************************
     C**********           CALL 'SC580R'
     C****BEGIN*CHG *** 12/11/97 **************************************
     C**********           PARM           SCRMCD
     C**********           PARM           QQRMCD
     C****END***CHG *** 12/11/97 **************************************
     C**********           PARM           QQRTC   1
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C     QQRMCD        PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C***  End   Chg  ***  10/11/06  **************************************
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C*
     C*** BEGIN CHG *** 12/11/97 **************************************
     C********** SCRMCD    OREQ *BLANKS
     C     QQRMCD        OREQ      *BLANKS
     C*** END   CHG *** 12/11/97 **************************************
     C*
     C                   SETON                                        83
     C*
     C*** BEGIN ADD *** 12/11/97 **************************************
     C                   ELSE
     C                   MOVE      QQRMCD        SCRMCD
     C*** END   ADD *** 12/11/97 **************************************
     C*** BEGIN ADD *** 01/12/06 **************************************
     C   71              SETON                                        86
     C*** END ADD ***** 01/12/06 **************************************
     C*
     C                   ENDIF                                                  INKL=ON
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
      *** BEG ADD ***** 11/27/06 *************************************
     C     HFLD          IFEQ      'SCDADC'
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
     C                   MOVEL     QQRMCD        SCDADC
     C                   MOVE      *ON           *IN62
     C                   ELSE
     C                   MOVE      *ON           *IN61
     C                   ENDIF
      *
     C                   ENDIF                                                  SCDADC
      *
     C     HFLD          IFEQ      'SCDASC'
      *
     C                   CALL      'S0005R'      UPRMPL
     C     ##RRN1        IFNE      *ZEROS
     C     ##RRN1        CHAIN     PUP03                              60
     C                   MOVEL     UPSTAF        SCDASC
     C                   ELSE
     C                   MOVE      *ON           *IN62
     C                   ENDIF
      *
     C                   ENDIF                                                  SCDASC
     C*** END ADD ***** 11/27/06 **************************************
     C*
     C     ENDF10        TAG
     C*
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*  PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C*
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C*
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*  F13=PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C                   GOTO      STRT01
     C*
     C                   END
     C*
     C                   END
     C*
     C*  GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*** BEGIN DEL ***  09/14/98  ************************************
     C*** BEGIN ADD ***  07/31/98  ************************************
     C*****      *IN58     IFEQ *OFF
     C*** END ADD *****  07/31/98  ************************************
     C*** END DEL *****  09/14/98  ************************************
     C*
     C*** BEGIN ADD *** 04/30/98 *************************************
     C*  CHECK IF SCREV WAS CHANGED
     C     SCREV         IFNE      XXREV
     C     SCREV         IFEQ      'Y'
     C*** BEGIN CHG ***  09/14/98  ************************************
     C*****                MOVE EXRFAF    SCRMCD
     C  N58              MOVE      EXRFAF        SCRMCD
     C*** END CHG *****  09/14/98  ************************************
     C                   ELSE
     C*** BEGIN CHG ***  09/14/98  ************************************
     C*****                MOVE EXAUD     SCRMCD
     C  N58              MOVE      EXAUD         SCRMCD
     C*** END CHG *****  09/14/98  ************************************
     C                   ENDIF
      *** BEG ADD ***** 02/03/09 **************************************
     C     EX3RDB        IFEQ      'Y'
     C                   CLEAR                   SCRMCD
     C                   ENDIF
      *** END ADD ***** 02/03/09 **************************************
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C                   ENDIF
     C*** END   ADD *** 04/30/98 *************************************
     C*
     C*** BEGIN DEL ***  09/14/98  ************************************
     C*** BEGIN ADD ***  07/31/98  ************************************
     C*****                ENDIF                           *IN58 IFEQ
     C*** END ADD *****  07/31/98  ************************************
     C*** END DEL *****  09/14/98  ************************************
     C*
     C*  POLICY YEAR MUST BE GREATER THAN OR EQUAL TO ZERO
     C*
     C*** BEGIN ADD *** 11/07/97 **************************************
     C     SCFYR         IFLE      *ZERO
     C     SCEMP#        ANDNE     *ZERO
     C     SCFYR         ORLE      *ZERO
     C     SCDIV         ANDNE     *ZERO
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN92
     C*** END   ADD *** 11/07/97 **************************************
     C*
     C                   GOTO      STRT
     C                   END
     C                   Z-ADD     SCFYR         XXFYR
     C*
     C*  VALIDATE EMPLOYER/DIVISION IF NOT EQUAL TO 0
     C*
     C     SCEMP#        IFNE      0
     C     SCDIV         ORNE      0
     C*
     C     EMKY          CHAIN     PEM01                              21
     C*
     C*** BEGIN ADD *** 11/07/97 **************************************
     C     *IN21         IFEQ      *ON
     C                   MOVE      *ON           *IN93
     C                   END
     C     *IN21         CABEQ     '1'           STRT
     C*
     C     ELKY          CHAIN     PEL02                              21
     C     *IN21         IFEQ      *ON
     C                   MOVE      *ON           *IN93
     C                   END
     C     *IN21         CABEQ     '1'           STRT
     C*** END   ADD *** 11/07/97 **************************************
     C*** BEG ADD ***** 01/13/10 ****************************************
     C                   MOVE      *BLANK        XXLRGD            1
     C                   MOVE      *OFF          *IN48
     C*
     C*   VERIFY IF POLICY IS LARGE DEDUCTIBLE BASED ON BILLING SCHEDULE CODE
     C*
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C*
     C     *IN60         IFEQ      *ZERO
     C     BSLPGM        ANDEQ     'L'
     C*
     C     ELKY          CHAIN     PLD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'Y'           XXLRGD
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     XXLRGD        IFEQ      'Y'
     C                   MOVE      UPLVL         ##ULVL
     C                   MOVE      UPTYPE        ##UTYP
     C                   MOVE      EXLDAL        ##CLVL
     C                   MOVE      EXLDAT        ##CTYP
     C*
     C                   CALL      'SRCCSC'
     C                   PARM                    ##UTYP            1
     C                   PARM                    ##ULVL            3
     C                   PARM                    ##CTYP            1
     C                   PARM                    ##CLVL            3
     C                   PARM      '1'           ##EXEC            1
     C                   PARM      ' '           ##RES             1
      *
     C     ##RES         IFEQ      *ON
     C                   MOVE      *ON           *IN48
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD ***** 01/13/10 ****************************************
     C*** BEGIN ADD ****** 02/18/10 ************************************
     C                   MOVE      *BLANKS       XXKEY
     C*** END   ADD ****** 02/18/10 ************************************
     C*
     C                   END
     C*** BEGIN ADD *** 01/13/06 **************************************
     C*
     C*  VALIDATE SINGLE DIVISION MODE FLAG.
     C*
     C     *IN71         IFEQ      *ON
     C*
     C     SCSING        IFNE      'Y'
     C     SCSING        ANDNE     ' '
     C                   MOVE      *ON           *IN88
     C                   GOTO      STRT
     C                   ENDIF                                                  SCSING IFNE
     C*
     C     SCSING        IFEQ      'Y'
     C     SCEMP#        ANDEQ     *ZERO
     C                   MOVE      *ON           *IN89
     C                   GOTO      STRT
     C                   ENDIF                                                  SCSING IFNE
     C*
     C                   ENDIF                                                  *IN71 IFEQ
     C*** END ADD ***** 01/13/06 **************************************
     C*** BEGIN ADD ***  09/11/06  ************************************
     C                   Z-ADD     0             XXCXDT            7 0
     C                   Z-ADD     0             XSCXDT            7 0
     C                   MOVE      *BLANK        SVARSM            1
     C*** END ADD *****  09/11/06  ************************************
     C*
     C*  MAKE SURE ALL AUDITED PAYROLLS HAVE BEEN ENTERED FOR THE
     C*  SPECIFIED EMPLOYER
     C*
     C     SCEMP#        IFNE      *ZEROS
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*** BEGIN ADD ***  05/26/00  ************************************
     C*
     C                   Z-ADD     ELPTDT        SVPTDT            7 0
     C                   Z-ADD     0             XXDIVC            5 0
     C     ELKY1         CHAIN     PEL03                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ELPFDT        IFNE      ELPTDT
     C                   ADD       1             XXDIVC
     C                   ENDIF                                                  ELPFDT<>ELPTDT
     C     ELKY1         READE     PEL03                                  60
     C                   ENDDO                                                  *IN60 = OFF
     C*
     C     ELKY          CHAIN     PEL02                              60
     C*
     C                   MOVE      *BLANK        MULTI             1
     C*
     C     XXDIVC        IFGT      1
     C                   MOVE      'Y'           MULTI
     C                   ENDIF
     C*** BEGIN ADD *** 01/13/06 **************************************
     C     SCSING        IFEQ      'Y'
     C                   MOVE      *BLANK        MULTI             1
     C                   ENDIF
     C*** END ADD ***** 01/13/06 **************************************
     C*
     C     XXDIVC        IFGT      1
     C*
     C                   Z-ADD     1             XXX               1 0
     C                   Z-ADD     0             SVCNDT            7 0
     C                   DO        5
     C     CD(XXX)       IFGE      ELPFDT
     C     CD(XXX)       ANDLE     ELPTDT
     C     ED(XXX)       ANDNE     0
     C                   MOVE      CD(XXX)       SVCNDT
     C                   LEAVE
     C                   ENDIF
     C                   ADD       1             XXX
     C                   ENDDO
     C*
     C     SVCNDT        IFNE      *ZERO
     C                   Z-ADD     SVCNDT        SVPTDT
     C                   ENDIF
     C*
     C*** BEGIN ADD ***  09/11/06  ************************************
     C                   Z-ADD     SVCNDT        XXCXDT
     C                   MOVE      ELARSM        SVARSM
     C*** END ADD *****  09/11/06  ************************************
     C*** BEGIN ADD ***  01/18/06  ************************************
     C*
     C* DEFAULT POLICY TO DATE FOR FLAT CANCELLATIONS.
     C*
     C     SVCNDT        IFEQ      *ZERO
     C*
     C                   Z-ADD     1             XXX               1 0
     C                   DO        5
     C     CD(XXX)       IFLT      ELPFDT
     C     CD(XXX)       ANDNE     0
     C     ED(XXX)       ANDNE     0
     C                   Z-ADD     ELPFDT        SVPTDT
     C                   LEAVE
     C                   ENDIF
     C                   ADD       1             XXX
     C                   ENDDO
     C*
     C                   ENDIF                                                  SVCNDT IFEQ
     C*** END ADD *****  01/18/06  ************************************
     C*
     C                   ENDIF                                                  XXDIVC
     C*** END ADD *****  05/26/00  ************************************
     C*
     C     ELKY1         CHAIN     PEL03                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     MULTI         IFEQ      'Y'
     C                   MOVE      ELDIV         X1DIV             5 0
     C                   ELSE
     C                   MOVE      SCDIV         X1DIV
     C                   ENDIF
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C                   Z-ADD     1             XXX               1 0
     C                   Z-ADD     0             SVCNDT            7 0
     C                   DO        5
     C     CD(XXX)       IFGE      ELPFDT
     C     CD(XXX)       ANDLE     ELPTDT
     C     ED(XXX)       ANDNE     0
     C                   MOVE      CD(XXX)       SVCNDT
     C                   LEAVE
     C                   ENDIF
     C                   ADD       1             XXX
     C                   ENDDO
     C*
     C     SVCNDT        IFNE      *ZERO
     C                   Z-ADD     SVCNDT        XXPTDT            7 0
     C                   ELSE
     C                   Z-ADD     ELPTDT        XXPTDT
     C                   ENDIF
     C*
     C*** BEGIN ADD ***  09/11/06  ************************************
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     SVCNDT        XSCXDT
     C                   ENDIF                                                  EMSTRP IFEQ
     C*** END ADD *****  09/11/06  ************************************
     C*** BEGIN ADD ***  01/18/06  ************************************
     C*
     C* DEFAULT POLICY TO DATE FOR FLAT CANCELLATIONS.
     C*
     C     SVCNDT        IFEQ      *ZERO
     C*
     C                   Z-ADD     1             XXX               1 0
     C                   DO        5
     C     CD(XXX)       IFLT      ELPFDT
     C     CD(XXX)       ANDNE     0
     C     ED(XXX)       ANDNE     0
     C                   Z-ADD     ELPFDT        XXPTDT
     C                   LEAVE
     C                   ENDIF
     C                   ADD       1             XXX
     C                   ENDDO
     C*
     C                   ENDIF                                                  SVCNDT IFEQ
     C*** END ADD *****  01/18/06  ************************************
     C*
     C     XXPTDT        IFEQ      SVPTDT
     C*
     C*** END ADD *****  11/15/99  ************************************
     C*
     C     EPKY          CHAIN     PEP01                              60
     C     *IN60         DOWEQ     '0'
     C     EPSTAT        IFEQ      'A'
     C     EPSAFL        IFNE      'A'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C                   END                                                    EPSTAT='A'
     C     EPKY          READE     PEP01                                  60
     C                   END
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*
     C                   ENDIF                                                  XXPTDT IFEQ
     C*
     C     MULTI         IFEQ      'Y'
     C     ELKY1         READE     PEL03                                  60
     C                   ELSE
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C*
     C                   ENDDO
     C*** END ADD *****  11/15/99  ************************************
     C**** BEGIN ADD **** 06/01/09 *********************************************
     C**** BEGIN ADD **** 03/06/10 ************************************
     C     MULTI         IFEQ      *BLANKS
     C**** END ADD   **** 03/06/10 ************************************
     C                   Z-ADD     0             I1                3 0
     C                   Z-ADD     0             SCCX              7 0
     C                   MOVE      *OFF          *IN47
      *
     C     SCEMP#        IFGT      *ZERO
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C                   ENDIF
     C*
     C                   Z-ADD     0             SCCX
     C*
     C                   Z-ADD     1             I1
     C*
     C     *ZERO         LOOKUP    ED(I1)                                 81
     C*
     C     *IN81         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
     C*
     C     I1            IFNE      0
     C                   Z-ADD     CD(I1)        SCCX
     C                   ENDIF                                                  I1 IFNE 0
      *
     C     SCCX          IFEQ      *ZERO
     C     XXDT          ANDLT     ELPTDT
     C     ELPFDT        ANDNE     ELPTDT
     C                   MOVE      *ON           *IN47
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C**** END   ADD **** 06/01/09 *********************************************
     C**** BEGIN ADD **** 03/06/10 ************************************
     C                   ENDIF
     C**** END   ADD **** 03/06/10 *********************************************
     C*** BEGIN ADD ***  09/11/06  ************************************
     C     XXCXDT        IFNE      *ZERO
     C     XXCXDT        ANDNE     XSCXDT
     C     SVARSM        ANDEQ     'Y'
     C                   MOVE      *ON           *IN46
     C                   GOTO      STRT
     C                   ENDIF                                                  XXCXDT IFNE
     C*** END ADD *****  09/11/06  ************************************
     C                   END
     C*
     C*  IF RUNNING MASS AUDIT, NO FYR CAN BE SELECTED.
     C*
     C*** BEGIN ADD *** 11/07/97 **************************************
     C     SCEMP#        IFEQ      *ZERO
     C     SCDIV         ANDEQ     *ZERO
     C     SCFYR         ANDNE     *ZERO
     C*** END   ADD *** 11/07/97 **************************************
     C*
     C                   MOVE      *ON           *IN44
     C                   GOTO      STRT
     C                   ENDIF
     C*** BEGIN ADD *** 03/23/98 **************************************
     C*
     C*  USER MUST HAVE PROPER SECURITY FOR RUNNING A MASS BILLING.
     C*
     C     SCEMP#        IFEQ      *ZERO
     C     FNKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *ON
     C     COKY          CHAIN     PEX01                              60
     C                   ENDIF                                                  *IN60 IFEQ
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EXSUBL        XCLVL             3
     C                   MOVE      EXSUBT        XCTYP             1
      *** BEG CHG ***** 05/04/07 **********************************
     C***********          MOVE UPLVL     XULVL   3
     C***********          MOVE UPTYPE    XUTYP   1
     C                   MOVE      XXLVL         XULVL             3
     C                   MOVE      XXTYPE        XUTYP             1
      *** END CHG ***** 05/04/07 **********************************
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
     C                   MOVE      QQRES         *IN45
     C     *IN45         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN45 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  SCEMP# IFEQ
     C*
     C*** END ADD ***** 03/23/98 **************************************
     C*
     C*  VALIDATE THE ENTERED DATE
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C*
     C*** BEGIN ADD *** 11/07/97 **************************************
     C                   MOVE      *ON           *IN91
     C*** END   ADD *** 11/07/97 **************************************
     C*
     C                   GOTO      STRT
     C                   END
      ***** BEG ADD ******* 03/11/09 **********************************
      *
      *  "INCLUDE DEDUCTIBLE BALANCE (Y/ )"
      *
     C                   MOVE      *OFF          *IN72
      *
     C     SCIDBD        IFNE      'Y'
     C     SCIDBD        ANDNE     *BLANK
     C                   MOVE      *ON           *IN72
     C                   GOTO      STRT
     C                   ENDIF
      *
      ***** END ADD ******* 03/11/09 **********************************
     C*** BEGIN ADD ***  12/19/00  ************************************
     C*
     C*  POSTING DATE CANNOT BE IN A PRIOR MONTH.
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD            5 0
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPOSD            5 0
     C*
     C     XXPOSD        IFLT      XXCURD
     C                   MOVE      *ON           *IN38
     C                   GOTO      STRT
     C                   ENDIF                                                  XXPOSD IFLT
     C*** END ADD *****  12/19/00  ************************************
     C*
     C*  REVISED BILL MUST BE Y OR N
     C*
     C     SCREV         IFNE      'Y'
     C     SCREV         ANDNE     'N'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE BILLING COMMENTS
     C*
     C     SCCOM1        IFNE      *BLANKS
     C                   MOVEL     SCCOM1        XXKEY
     C     TBKY          CHAIN     PBC01                              25
     C  N25BCSTAT        COMP      'I'                                    25
     C     *IN25         CABEQ     '1'           STRT
     C                   END
     C*
     C     SCCOM2        IFNE      *BLANKS
     C                   MOVEL     SCCOM2        XXKEY
     C     TBKY          CHAIN     PBC01                              26
     C  N26BCSTAT        COMP      'I'                                    26
     C     *IN25         CABEQ     '1'           STRT
     C                   END
     C*
     C     SCCOM3        IFNE      *BLANKS
     C                   MOVEL     SCCOM3        XXKEY
     C     TBKY          CHAIN     PBC01                              27
     C  N27BCSTAT        COMP      'I'                                    27
     C     *IN25         CABEQ     '1'           STRT
     C                   END
     C*** BEGIN ADD ***  02/15/05  ************************************
     C     SCCOM1        IFNE      *BLANKS
     C     SCCOM2        ORNE      *BLANKS
     C     SCCOM3        ORNE      *BLANKS
     C*
     C     SCFCM1        IFNE      *BLANKS
     C     SCFCM2        ORNE      *BLANKS
     C     SCFCM3        ORNE      *BLANKS
     C                   MOVE      *ON           *IN39
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFCM1 IFNE
     C*
     C                   ENDIF                                                  SCCOM1 IFNE
     C*** END ADD *****  02/15/05  ************************************
     C*
     C*  VALIDATE A/R TRANSACTION CODE IF ENTERED
     C*
     C*** BEGIN ADD ***  03/31/98  ************************************
     C     *IN98         IFEQ      *ON
     C*** END ADD *****  03/31/98  ************************************
     C*** BEGIN CHG ****** 01/14/10 ************************************
     C************         MOVELSCCODE    XXKEY
     C                   MOVEL(P)  SCCODE        XXKEY
     C*** END   CHG ****** 01/14/10 ************************************
     C     TBKY          CHAIN     PTT01                              23
     C  N23TTSTAT        COMP      'I'                                    23
     C     *IN23         CABEQ     '1'           STRT
     C     TTTYPE        COMP      'D'                                3232
     C     *IN32         CABEQ     '1'           STRT
     C*
     C* VALIDATE DARTS CODE IF PRESENT
     C*
     C*** BEGIN ADD ***  07/31/98  ************************************
     C     *IN58         IFEQ      *OFF
     C*** END ADD *****  07/31/98  ************************************
     C*** BEGIN ADD ***  11/23/98  ************************************
     C     SCRMCD        ANDNE     *BLANKS
     C*** END ADD *****  11/23/98  ************************************
     C     SCRMCD        CHAIN     PCT01                              84
     C*
     C     *IN84         IFEQ      *ON
     C*
     C*** BEGIN ADD *** 11/07/97 **************************************
     C                   MOVE      *ON           *IN83
     C*** END   ADD *** 11/07/97 **************************************
     C*
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     CTSTAT        IFEQ      'I'
     C                   MOVE      *ON           *IN85
     C                   GOTO      STRT
     C                   ENDIF                                                  CTSTAT IFEQ
     C*** BEGIN ADD ***  07/31/98  ************************************
     C                   ENDIF                                                  *IN58 IFEQ
     C*** END ADD *****  07/31/98  ************************************
     C*** BEGIN ADD ***  03/31/98  ************************************
     C                   ENDIF                                                  *IN98 IFEQ
     C*** END ADD *****  03/31/98  ************************************
     C*
     C*** BEGIN ADD *** 01/12/06 **************************************
     C*
     C*  VALIDATE CONTRIBUTION NOTICE FLAG IF ENTERED AND AVAILABLE.
     C*
      *** BEG DEL ***** 09/20/06 *************************************
     C********** *IN71     IFEQ *ON
     C********** SCCONT    IFNE 'Y'
     C********** SCCONT    ANDNE' '
     C**********           MOVE *ON       *IN87
     C**********           GOTO STRT
     C**********           ENDIF                           SCCONT IFNE
     C**********           ENDIF                           *IN71 IFEQ
      *** END DEL ***** 09/20/06 *************************************
     C*** END ADD ***** 01/12/06 **************************************
     C*** BEGIN ADD *** 01/24/06 **************************************
     C*
     C*  DISPLAY INFORMATION WINDOW IF RETRO SETTLEMENT HAS BEEN RUN.
     C*
     C*
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C                   CLEAR                   XXKEY
     C*
     C***  Begin Chg  ***  02/25/10  **************************************
     C********** *IN60     IFEQ *OFF
     C********** BSLPGM    ANDNE*BLANKS
     C                   MOVE      *OFF          XXRTRO            1
     C     *IN60         IFEQ      *OFF
     C     BSLPGM        IFEQ      'P'
     C     BSLPGM        OREQ      'I'
     C                   MOVE      *ON           XXRTRO
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXRTRO        IFEQ      *ON
     C***  End   Chg  ***  02/25/10  **************************************
     C     ELSTAM        ANDNE     *ZERO
     C*
     C                   EXFMT     WDWMSG
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVE      *OFF          *INKL
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*** END ADD ***** 01/24/06 **************************************
      *
      ***** BEG ADD ***** 11/15/06 ************************************
     C     XXRRN         IFGT      *ZEROS
      ***** END ADD ***** 11/15/06 ************************************
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      XXFND         QQFND             3
     C                   MOVE      XXFYR         QQFYR             3
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      *BLANK        QQSPRD            1
     C*
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
     C*** BEGIN ADD ***  03/31/98  ************************************
     C     QQMODE        ANDNE     '1'
     C*** END ADD *****  03/31/98  ************************************
     C*** END CHG *****  02/19/98  ************************************
     C*
     C     EMKY          CHAIN     PEM01                              60
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
     C*
     C*** BEGIN DEL ***  11/15/99  ************************************
     C*********            MOVEL'SCR02'   Q2RECF
     C*********            GOTO STRT
     C*** END DEL *****  11/15/99  ************************************
     C                   END
      *
      ***** BEG ADD ***** 11/15/06 ************************************
     C                   ENDIF                                                  XXRRN > ZEROS
      ***** END ADD ***** 11/15/06 ************************************
      *
     C*** BEG ADD *****  11/27/06  ************************************
      *
      *  VALIDATE FINAL AUDIT READY FLAG
      *
     C     SCFAFL        IFNE      'Y'
     C     SCFAFL        ANDNE     *BLANK
     C                   MOVE      *ON           *IN67
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  AUTO POPULATE DARTS REMARK CODE AND STAFF CODE IF THE FINAL AUDIT
      *  READY FLAG IS SWITCH TO 'Y' AND SUCH CODES EXIST IN AUDIT TRACKING
      *
     C     SCFAFL        IFEQ      *BLANKS
     C     SVFAFL        ANDEQ     'Y'
     C                   MOVE      SCFAFL        SVFAFL
     C*** BEGIN ADD ***  08/13/07  *****************************************
     C                   MOVE      *BLANKS       SCDASC
     C                   MOVE      *BLANKS       SCDADC
     C                   MOVE      *ON           *IN61
     C                   GOTO      STRT
     C*** END ADD *****  08/13/07  *****************************************
     C                   ENDIF
      *
     C     SVFAFL        IFEQ      *BLANKS
     C     SCFAFL        ANDEQ     'Y'
     C                   MOVE      SCFAFL        SVFAFL
     C     FNKY          CHAIN     PAX01                              60
     C     *IN60         IFEQ      *OFF
     C     SCDASC        IFEQ      *BLANKS
     C                   MOVEL     AXDASC        SCDASC
     C                   ENDIF
     C     SCDADC        IFEQ      *BLANKS
     C                   SELECT
     C     SCREV         WHENEQ    'Y'
     C                   MOVEL     AXRADC        SCDADC
     C     SCREV         WHENEQ    'N'
     C                   MOVEL     AXIADC        SCDADC
     C                   ENDSL
     C                   ENDIF                                                  SCDADC
     C*** BEGIN ADD ***  08/13/07  *****************************************
     C                   MOVE      *ON           *IN61
     C*** END ADD *****  08/13/07  *****************************************
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60
     C                   ENDIF                                                  SVFAFL
      *
      *  VALIDATE THAT GENERATE FINAL AUDIT READY FLAG SYNCS WITH THE DARTS
      *  REMARK AND STAFF CODE
      *
     C     SCFAFL        IFEQ      'Y'
     C     SCDADC        ANDEQ     *BLANKS
     C     SCFAFL        OREQ      'Y'
     C     SCDASC        ANDEQ     *BLANKS
     C     SCFAFL        OREQ      ' '
     C     SCDADC        ANDNE     *BLANKS
     C     SCFAFL        OREQ      ' '
     C     SCDASC        ANDNE     *BLANKS
     C                   MOVE      *ON           *IN63
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* VALIDATE  AUDIT DARTS REMARK CODE
     C*
     C     SCDADC        IFNE      *BLANKS
     C     SCDADC        CHAIN     PCT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN65
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C                   ENDIF                                                  SCDADC NE BLANK
     C*
     C* VALIDATE AUDIT DARTS STAFF CODE
     C*
     C     SCDASC        IFNE      *BLANKS
     C     SCDASC        CHAIN     PUP02                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN66
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C                   ENDIF                                                  SCDADC NE BLANK
     C*** END ADD *****  11/27/06  ************************************
      *
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
     C*
     C     SUBMIT        TAG
     C*** END ADD *****  11/15/99  ************************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      XXFND         QQFND             3
     C                   MOVE      XXFYR         QQFYR             3
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      SCDIV         QQDIV             5
     C                   MOVE      SCDATE        QQDATE            8
     C*** BEG CHG ***** 05/04/07 **************************************
     C*****************    MOVE UPOUTQ    QQOUTQ 10
     C                   MOVE      XXOUTQ        QQOUTQ           10
     C*** END CHG ***** 05/04/07 **************************************
     C*** BEGIN ADD ***  03/31/98  ************************************
     C                   MOVE      QQMODE        XXMODE            1 0
     C     SCREV         IFEQ      'Y'
     C                   ADD       3             XXMODE
     C                   ENDIF                                                  SCREV IFEQ
     C                   MOVE      XXMODE        SCREV
     C*** END ADD *****  03/31/98  ************************************
     C*** BEGIN ADD ***  01/13/06  ************************************
     C     SCSING        IFEQ      'Y'
     C*
     C                   SELECT
     C     XXMODE        WHENEQ    1
     C                   MOVE      'A'           SCREV
     C     XXMODE        WHENEQ    2
     C                   MOVE      'B'           SCREV
     C     XXMODE        WHENEQ    3
     C                   MOVE      'C'           SCREV
     C     XXMODE        WHENEQ    4
     C                   MOVE      'D'           SCREV
     C     XXMODE        WHENEQ    5
     C                   MOVE      'E'           SCREV
     C     XXMODE        WHENEQ    6
     C                   MOVE      'F'           SCREV
     C                   ENDSL
     C*
     C                   ENDIF                                                  SCSING IFEQ
     C*** END ADD *****  01/13/06  ************************************
     C*** BEGIN ADD ***  02/15/05  ************************************
     C                   MOVEL     SCFCM1        QQFCM1
     C                   MOVEL     SCFCM2        QQFCM2
     C                   MOVEL     SCFCM3        QQFCM3
     C*** END ADD *****  02/15/05  ************************************
     C*
     C                   CALL      'W7710C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQDATE
     C                   PARM                    SCCODE
     C                   PARM                    SCCOM1
     C                   PARM                    SCCOM2
     C                   PARM                    SCCOM3
     C                   PARM                    SCREV
     C                   PARM                    QQOUTQ
     C                   PARM                    SCRMCD
     C*** BEGIN ADD ***  02/15/05  ************************************
     C                   PARM                    QQFCM1           30
     C                   PARM                    QQFCM2           30
     C                   PARM                    QQFCM3           30
     C*** END ADD *****  02/15/05  ************************************
     C*** BEGIN ADD *** 01/12/06 **************************************
     C*** BEG CHG ***** 09/20/06 **************************************
     C***********          PARM           SCCONT
     C                   PARM      *BLANK        QQCONT            1
     C*** END CHG ***** 09/20/06 **************************************
     C*** BEG ADD ***** 11/27/06 **************************************
     C                   PARM                    SCDADC
     C                   PARM                    SCDASC
     C*** END ADD ***** 11/27/06 **************************************
      ***** BEG ADD ******* 03/11/09 **********************************
     C                   PARM      SCIDBD        QQIDBD            1
      ***** END ADD ******* 03/11/09 **********************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
