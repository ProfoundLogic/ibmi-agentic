     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7570R - PROMPT FOR SELF-REPORTING LATE NOTICES
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS IS THE PROMPT PROGRAM FOR SELF-REPORTING LATE NOTICES.   *
     H* VALUES PROMPTED FOR ARE RUN DATE, REPORTING MONTH/YEAR,       *
     H* PAST DUE DATE, NEW DUE DATE, SORT SEQUENCE (EMP NBR, EMP,     *
     H* NAME, AGENT NBR), EMPLOYER NAME RANGE, AGENT NBR RANGE,       *
     H* MAXIMUM AMT, COMMENT CODES 1/2/3, & DARTS REMARK CODE.        *
     H* WHEN ALL SCREEN EDITING IN SCR01 IS PASSED, A CALL IS MADE    *
     H* TO THE BATCH SUBMITTER W7570C.                                *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   MENU                                                        *
     H* CALLS TO:                                                     *
     H*   M0006C                                                      *
     H*   W2191R                                                      *
     H*   SC580R                                                      *
     H*   W0006R                                                      *
     H*   SRDCK                                                       *
     H*                                                               *
     H* INDICATORS:                                                   *
     H*                                                               *
     H*   10  HELP KEY WAS PRESSED                                    *
     H*   11  COMMAND KEY WAS PRESSED                                 *
     H*   20  RUN DATE INVALID                                        *
     H*   21  CHAIN TO PFN01                                          *
     H*   21  REPORT DATE MONTH INVALID                               *
     H*   22  PAST DUE DATE INVALID                                   *
     H*   23  REPORT BY DATE INVALID                                  *
     H*   24  PRIMARY SORT SEQUENCE VALID                             *
     H*   25  EMPLYR FROM NAME GREATER THAN EMPLYR TO NAME            *
     H*   26  AGENT FROM NBR GREATER THAN AGENT TO NBR                *
     H*   27  MAXIMUM AMOUNT < ZERO                                   *
     H*   28  SCCOM1 INVALID                                          *
     H*   29  SCCOM2 INVALID                                          *
     H*   30  SCCOM3 INVALID                                          *
     H*   31  SCCOM4 INVALID                                          *
     H*   32  SCCOM5 INVALID                                          *
     H*   33  DARTS REMARK CODE NOT FOUND                             *
     H*   35  EMPLYR ALPHA AND NUMBER RANGES ENTERED                  *
     H*   39, 17  SECONDARY SORT SEQUENCE VALID                       *
     H*   43  'GENERATE REPORT' IS NE Y OR BLANK
     H*   47  AGENT ALPHA AND NUMBER RANGES ENTERED                   *
     H*   60  GENERAL CHAIN AND LOKUP INDICATOR                       *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                     *
     H*                                                                *
     H* 05/18/21 C:65  Support: WC2760                                 *
     H*                If running generate report only then clear out  *
     H*                the resolve to iDarts and remark code fields.   *
     H*                                                                *
     H*  01/28/20 C:062 PROJECT: WC2760                                *
     H*                 Add scriptura popup window.                    *
     H*                                                                *
     H*  03/21/09 C:189 PROJECT: 102WEW                                *
     H*                 SR Lt Notice-Resolve to Darts/Cert Ltrs        *
     H*                                                                *
     H*                                                                *
     H*  12/17/08 C:136 SUPPORT: 102WEP                                *
     H*                 CHANGE EDIT ON ADDITIONAL DAYS FIELD TO ALLOW  *
     H*                 VALUE OF ZERO PER GW                           *
     H*                                                                *
     H*  08/21/08  C:191 PROJECT: 102WEP                              *
     H*                  BACK TO BASE :                               *
     H*                                                               *
     H*  05/24/07 C:136 SUPPORT: WA081 58498                          *
     H*                 CORRECT EDIT, PREVENT AN ENTRY OF BLANK INTO  *
     H*                 FREQUENCY FIELD WHEN VENDOR PROCESSING IS     *
     H*                 ENABLED.                                      *
     H*                                                               *
     H*  10/11/06  C:33  Seq#: WA099/41801                            *
     H*                  Use SC305R instead of SC580R.                *
     H*                                                                *
     H*  10/27/03  C:33  Project: WC1132                               *
     H*                  Revamp Dunning Process                        *
     H*                                                               *
     H* 02/18/03 C:136 PROJECT: WC1115                                *
     H*                ALLOW FOR WEEKLY PROCESSING IF VENDOR          *
     H*                PROCESSING IS ENABLED.                         *
     H*                                                               *
     H* 11/17/00  C:35   PROJECT: WC577                                *
     H*                  ALLOW FOR INCLUDE/EXCLUDE OF UP TO 5 ACTIVITY *
     H*                  STATUS CODES                                  *
     H*                                                                *
     H* 03/03/00 C:118 PROJECT: WC974                                 *
     H*                Add 'Generate report only (Y/ )' prompt to     *
     H*                display file. Pass flag to CL programs.        *
     H*                                                               *
     H* 03/16/99  C:87  PROJECT: SUPP # 17123                         *
     H*                 QQSTDT and QQENDT were not being loaded corr- *
     H*                 ectly.  Chgd XXDATE to 8,0 and chgd the way   *
     H*                 that date for february is built.              *
     H*                                                               *
     H*  01/23/98 C:87 PROJECT CM034                                  *
     H*                ADD SECONDARY SORT AND CODE TO HANDLE IT       *
     H*                                                               *
     H*  06/25/97 C:79 PROJECT WC711                                  *
     H*               -VR3 CHANGES                                    *
     H*                                                               *
     H*  05/19/97 C:57 PROJECT CM015                                  *
     H*               -WHEN PROMPTING DARTS, REDISPLAY SCR01          *
     H*****************************************************************
     FW7570F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSTCTL1    IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     FWMEXL     IF   E           K DISK
     FWMEMP     IF   E             DISK
     FWMAGP     IF   E             DISK
     FWMAAP     IF   E             DISK
      **** BEGIN ADD **** 11/17/00 *********************************************
     FWTAVL     IF   E           K DISK
     FWTAVP     IF   E             DISK
     F                                     RENAME(PAV01:PAV02)
      **** END   ADD **** 11/17/00 *********************************************
     F*** BEG ADD ****** 08/21/08 ************************************
     FSTSTL1    IF   E           K DISK
     F*** END ADD ****** 08/21/08 ************************************
      **** BEG   ADD **** 02/18/03 *********************************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
      **** END   ADD **** 02/18/03 *********************************************

     D*
     D SRCH            S             10    DIM(6) CTDATA PERRCD(1)
      **** BEGIN ADD **** 11/17/00 *********************************************
     D ASE             S             10    DIM(5) CTDATA PERRCD(1) ASCEND
      **** END   ADD **** 11/17/00 *********************************************
     D*
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXMCY                  1      6  0
     D  XXMCYM                 1      2  0
     D  XXMCYY                 3      6  0
      *
     D***  BEGIN CHG  ***  03/16/99  **********************************
     D*********** DS ************************
     D*************************************** 1   60XXDATE
     D*************************************** 1   20XXMM
     D*************************************** 3   40XXDD
     D*************************************** 5   60XXYY
     D*
     D                 DS
     D  XXDATE                 1      8  0
     D  XXMM                   1      2  0
     D  XXDD                   3      4  0
     D  XXYY                   5      8  0
     D***  END   CHG  ***  03/16/99  **********************************
      **** BEGIN ADD **** 11/17/00 *********************************************
     D                 DS
     D  AS                     1     10
     D                                     DIM(5) ASCEND                        ACTIVITY STATUS
     D  SCAST1                 1      2
     D  SCAST2                 3      4
     D  SCAST3                 5      6
     D  SCAST4                 7      8
     D  SCAST5                 9     10
      **** END   ADD **** 11/17/00 *********************************************
     D***  Begin Add  ***  10/27/03  **************************************
     D QQCOM           DS
     D  QQCOM1                 1      2
     D  QQCOM2                 3      4
     D  QQCOM3                 5      6
     D  QQCOM4                 7      8
     D  QQCOM5                 9     10
     D***  End   Add  ***  10/27/03  **************************************
     D*
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D***  Begin Add  ***  10/27/03  **************************************
     D OPTNM           C                   CONST('Late Notices and -
     D                                     Report - S/R')
     D***  End   Add  ***  10/27/03  **************************************
     d*** Begin Add 01/28/21 **********************************************
     d isUseScript     s              1n   inz
     D isUseSpoolOut   s              1n   inz
     D isUseXML        S              1n   inz
     d xxSubmit        s              1n
     d xxMass          s              1
     d massFlag        s              1
     d processType     c                   const('SR Late Notice')
     d xxProcType      s            100

     d sVLDA           DS          1024    DTAARA(*LDA)
     d xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)

     d/copy sPrAtrium
     d/copy sPrXML
     d/copy sPrGenPop
     d/copy sPrLetter
     d*** End   Add 01/28/21 **********************************************
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     **
     ** FIELD DEFINITIONS
     **

     C     *LIKE         DEFINE    FNFND         XXFND
     C     *LIKE         DEFINE    BCKEY         XXKEY
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    AGMOD         XXMOD
     C     *LIKE         DEFINE    AGFEIN        XXFEIN
     **
     ** KEY LIST FOR THE COMPANY MASTER FILE
     **
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     **
     ** KEY LIST FOR THE USER PARAMETER MASTER FILE
     **
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     **
     ** KEY LIST FOR TABLE FILE
     **
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY
     **
     ** KEY LIST FOR THE FUND MASTER FILE
     **
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     **
     **  KEY LIST FOR AGENT MASTER FILE
     **
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     **
     **  CLIENT CONTROL FILE
     **
     C     EXKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
      **** BEG   ADD **** 02/18/03 *********************************************
     **
     **  SYSTEM CONTROL FILE
     **
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      **** END   ADD **** 02/18/03 *********************************************
     C*** BEG ADD ****** 08/21/08 ************************************
     C*  SYSTEM TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*** END ADD ****** 08/21/08 ************************************
     **
     ** ENTRY PARAMETERS
     **
     C     *ENTRY        PLIST
     C***  Begin Chg  ***  10/27/03  **************************************
     C**********           PARM           QQPRDS 40
     C**********           PARM           QQCALL  1
     C                   PARM                    @@PRDS           40
     C                   PARM                    @@CALL            1
     C***  End   Chg  ***  10/27/03  **************************************
     **
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     **
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     **
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      **** BEGIN ADD **** 11/17/00 *********************************************
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
      **** END   ADD **** 11/17/00 *********************************************
      /free
       // Are we using Scriptura?
       isUseScript = UseScriptura(processType);
       isScriptEn = *Off;
       If isUseScript;
          isScriptEn = *On;
       Endif;
     **
     **  Get The User Parameter File
     **
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     **
     C                   MOVE      UPCO#         XXCO#
     C                   Z-ADD     UPFND         SCFND
     **
     **  Get The Fund Record, Load Fund Name For Screen
     **
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              21
     C                   MOVEL     FNNAME        SCNAME
     **
     C                   TIME                    DSDTTM
     **
     ** DEFAULT THE BALANCE DATE TO BE THE SYSTEM DATE
     **
     C                   Z-ADD     DSDATE        SCDTE
     **
      *** BEG ADD **** 02/18/03 **************************************
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL23        IFEQ      'Y'
     C                   SETON                                        41
     C                   ELSE
     C                   SETOFF                                       41
     C                   ENDIF                                                  WCFL23 IFEQ
      *** END ADD **** 02/18/03 **************************************
     C*** BEG ADD ****** 08/21/08 ************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** END ADD ****** 08/21/08 ************************************
     **
     C     STRT01        TAG
     **
     **  Get the Company Record
     **
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C***  Begin Del  ***  10/27/03  **************************************
     C********** ' '       CHEKRSCCONM    X       20
     C********** 40        SUB  X         X
     C**********           DIV  2         X
     C********** ' '       CAT  SCCONM:X  SCCONM
     C***  End   Del  ***  10/27/03  **************************************
     **
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
      *
     C***  Begin Chg  ***  10/27/03  **************************************
     C**********           MOVELQQPRDS    SRDFLD    P
     C     Q1PRMS        IFGE      1
     C     @@PRDS        ANDNE     *BLANKS
     C                   MOVEL(P)  @@PRDS        SRDFLD
     C                   ELSE
     C                   MOVEL(P)  OPTNM         SRDFLD
     C                   ENDIF
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCPRDS
     C***  End   Chg  ***  10/27/03  **************************************
     **
     **   Get Default DARTS Remark Code From Admin/DARTS control maint.
     **
     C     EXKY          CHAIN     PEX01                              60
     **
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANK        SCDRMK
      **** BEGIN ADD **** 03/21/09 *********************************************
     C                   MOVEL     *BLANK        SCDTMK
      **** END   ADD **** 03/21/09 *********************************************
     **
     C                   ELSE
     C                   MOVEL     EXLNRM        SCDRMK
      **** BEGIN ADD **** 03/21/09 *********************************************
     C                   MOVEL     EXLNRM        SCDTMK
      **** END   ADD **** 03/21/09 *********************************************
     **
     C                   ENDIF
     **
     **   Load the Record Format Info For First Time
     **
     C                   MOVEL(P)  'SCR01'       Q2RECF
     **
     **   Display All Formats
     **
     C     STRT          TAG
     **
     ** CONVERT THE BALANCE DATE ENTERED
     **
     C                   Z-ADD     SCDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     **
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         SCDTE
     C                   ENDIF
     **
     ** CONVERT DATE ENTERED AS MMYY TO MMCCYY.
     **
     C                   Z-ADD     SCRMCY        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     **
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         SCRMCY
     C                   ENDIF                                                  SPERR IFEQ '0'
     C***  Begin Del  ***  10/27/03  **************************************
     ***********
     ****VALIDATE PAST DUE DATE
     ***********
     C**********           Z-ADDSCPDDT    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     ***********
     C********** SRERR     IFEQ '0'
     C**********           MOVELSRDTO     SCPDDT
     C**********           ENDIF
     ***********
     ****VALIDATE REPORT BY DATE
     ***********
     C**********           Z-ADDSCRBDT    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     ***********
     C********** SRERR     IFEQ '0'
     C**********           MOVELSRDTO     SCRBDT
     C**********           ENDIF
     C***  End   Del  ***  10/27/03  **************************************
     **
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
      **** BEGIN ADD **** 11/17/00 *********************************************
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   ENDIF                                                  Q2RECF IFEQ 'SCR03'
      **** END   ADD **** 11/17/00 *********************************************
     **
     **   Process All Command Keys
     **
     C     *IN11         IFEQ      *ON
     **
     **   COMMAND 03 = EXIT
     **
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF
      **** BEGIN ADD **** 11/17/00 *********************************************
     C     *INKL         IFEQ      *ON
     C     Q2RECF        ANDEQ     'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL IFEQ *ON
      **** END   ADD **** 11/17/00 *********************************************
     **
     **   Process Help Requests
     **
     C     *IN10         IFEQ      *ON
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL(P)  'QHLP'        FILNAM           10
     C                   MOVEL(P)  '*LIB'        LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     **
     **  COMMAND 10 - TABLE FILE SEARCH - DARTS REMARK FOR EACH PMT
     **
     C     *INKJ         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     **
     **  VALIDATE SEARCH FIELDS
     **
     C                   Z-ADD     1             X                 2 0
     **
     C     HFLD          LOOKUP    SRCH(X)                                60
     **
     C     *IN60         IFEQ      *ON
     **
     C                   ADD       11            X
     C                   MOVE      *ON           *IN(X)
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  *IN60 = OFF
     **
     **  TABLE SEARCH - COMMENT CODE 1
     **
     C     HFLD          IFEQ      'SCCOM1'
     **
     C                   CALL      'W2191R'
     C                   PARM      '2'           QQCALL
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      *BLANKS       SCKEY            10
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     **
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN49
     C                   ENDIF
     **
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCCOM1
     C                   MOVE      *ON           *IN50
     **
     C                   ELSE
     C                   MOVE      *ON           *IN49
     **
     C                   ENDIF                                                  SCKEY IFNE
     **
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  HFLD IFEQ SC
     **
     **  TABLE SEARCH - COMMENT CODE 2
     **
     C     HFLD          IFEQ      'SCCOM2'
     **
     C                   CALL      'W2191R'
     C                   PARM      '2'           QQCALL
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      *BLANKS       SCKEY            10
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     **
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN50
     C                   ENDIF
     **
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCCOM2
     C                   MOVE      *ON           *IN51
     **
     C                   ELSE
     C                   MOVE      *ON           *IN50
     **
     C                   ENDIF                                                  SCKEY IFNE
     **
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  HFLD IFEQ SC
     **
     **  TABLE SEARCH - COMMENT CODE 3
     **
     C     HFLD          IFEQ      'SCCOM3'
     **
     C                   CALL      'W2191R'
     C                   PARM      '2'           QQCALL
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      *BLANKS       SCKEY            10
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     **
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN51
     C                   ENDIF
     **
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCCOM3
     C                   MOVE      *ON           *IN52
     **
     C                   ELSE
     C                   MOVE      *ON           *IN51
     **
     C                   ENDIF                                                  SCKEY IFNE
     **
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  HFLD IFEQ SC
     **
     **  TABLE SEARCH - COMMENT CODE 4
     **
     C     HFLD          IFEQ      'SCCOM4'
     **
     C                   CALL      'W2191R'
     C                   PARM      '2'           QQCALL
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      *BLANKS       SCKEY            10
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     **
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN52
     C                   ENDIF
     **
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCCOM4
     C                   MOVE      *ON           *IN53
     **
     C                   ELSE
     C                   MOVE      *ON           *IN52
     **
     C                   ENDIF                                                  SCKEY IFNE
     **
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  HFLD IFEQ SC
     **
     **  TABLE SEARCH - COMMENT CODE 5
     **
     C     HFLD          IFEQ      'SCCOM5'
     **
     C                   CALL      'W2191R'
     C                   PARM      '2'           QQCALL
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      *BLANKS       SCKEY            10
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     **
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN53
     C                   ENDIF
     **
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCCOM5
     C                   MOVE      *ON           *IN54
     **
     C                   ELSE
     C                   MOVE      *ON           *IN53
     **
     C                   ENDIF                                                  SCKEY IFNE
     **
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  HFLD IFEQ SC
     **
     **  TABLE SEARCH - DARTS REMARK CODE
     **
     C     HFLD          IFEQ      'SCDRMK'
     **
     C***  Begin Chg  ***  10/11/06  **************************************
     C**********           CALL 'SC580R'
     C**********           PARM *BLANKS   XXDRMK  5
     C**********           PARM           QQRTCD  1
     ***********
     C********** QQRTCD    IFEQ 'A'
     C**********           GOTO TERM
     C**********           ENDIF
     ***********
     C********** QQRTCD    IFEQ 'B'
     C**********           MOVE *ON       *IN54
     C**********           ENDIF
     ***********
     C********** XXDRMK    IFNE *BLANKS
     C**********           MOVE XXDRMK    SCDRMK
     C**********           MOVE *ON       *IN55
     ***********
     C**********           ELSE
     C**********           MOVE *ON       *IN54
     ***********
     C**********           ENDIF
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN54
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL(P)  ##RMCD        SCDRMK
     C                   MOVE      *ON           *IN55
     C                   OTHER
     C                   MOVE      *ON           *IN54
     C                   ENDSL
     C***  End   Chg  ***  10/11/06  **************************************
     **
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  HFLD IFEQ
     **
     **  TABLE SEARCH - FROM EMPLOYER NAME
     **
     C     HFLD          IFEQ      'SCFREA'
     **
     C                   CALL      'W0050R'
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM      DBRRNF        QQRRN2            9 0
     C                   PARM                    QQQTE             1
     C                   PARM      1             QQVIEW            1 0
     C                   PARM                    @@RRN3            9 0
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     **
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN56
     C                   ENDIF
     **
     ** LOAD EMPLOYER NAME
     **
     C     QQRRN         IFNE      *ZERO
     **
     C     QQRRN         CHAIN     WMEMP                              60
     **
     C     *IN60         IFEQ      *ZERO
     C                   MOVE      *ON           *IN57
     C                   MOVEL     EMNAME        SCFREA
     C                   ENDIF                                                  *IN60 IFEQ
     **
     C                   ELSE
     C                   MOVE      *ON           *IN56
     **
     C                   ENDIF                                                  QQRRN IFNE
     **
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  HFLD IFEQ
     **
     **  TABLE SEARCH - TO EMPLOYER NAME
     **
     C     HFLD          IFEQ      'SCTOEA'
     **
     C                   CALL      'W0050R'
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM      DBRRNF        QQRRN2            9 0
     C                   PARM                    QQQTE             1
     C                   PARM      1             QQVIEW            1 0
     C                   PARM                    @@RRN3            9 0
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     **
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN57
     C                   ENDIF
     **
     **  LOAD EMPLOYER NAME
     **
     C     QQRRN         IFNE      *ZERO
     **
     C     QQRRN         CHAIN     WMEMP                              60
     **
     C     *IN60         IFEQ      *ZERO
     C                   MOVE      *ON           *IN58
     C                   MOVEL     EMNAME        SCTOEA
     C                   ENDIF
     **
     C                   ELSE
     C                   MOVE      *ON           *IN57
     **
     C                   ENDIF                                                  QQRRN IFNE
     **
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  HFLD IFEQ
     **
     **  TABLE SEARCH - FROM EMPLOYER NUMBER
     **
     C     HFLD          IFEQ      'SCFREN'
     **
     C                   CALL      'W0050R'
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM      DBRRNF        QQRRN2            9 0
     C                   PARM                    QQQTE             1
     C                   PARM      2             QQVIEW            1 0
     C                   PARM                    @@RRN3            9 0
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     **
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN58
     C                   ENDIF
     **
     ** LOAD EMPLOYER NUMBER
     **
     C     QQRRN         IFNE      *ZERO
     **
     C     QQRRN         CHAIN     WMEMP                              60
     **
     C     *IN60         IFEQ      *ZERO
     C                   MOVE      *ON           *IN59
     C                   MOVEL     EMEMP#        SCFREN
     C                   ENDIF                                                  *IN60 IFEQ
     **
     C                   ELSE
     C                   MOVE      *ON           *IN58
     C                   ENDIF                                                  QQRRN IFNE
     **
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  HFLD IFEQ
     **
     **  TABLE SEARCH - TO EMPLOYER NUMBER
     **
     C     HFLD          IFEQ      'SCTOEN'
     **
     C                   CALL      'W0050R'
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM      DBRRNF        QQRRN2            9 0
     C                   PARM                    QQQTE             1
     C                   PARM      2             QQVIEW            1 0
     C                   PARM                    @@RRN3            9 0
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     **
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN59
     C                   ENDIF
     **
     **  LOAD EMPLOYER NUMBER
     **
     C     QQRRN         IFNE      *ZERO
     **
     C     QQRRN         CHAIN     WMEMP                              60
     **
     C     *IN60         IFEQ      *ZERO
     C                   MOVE      *ON           *IN61
     C                   MOVEL     EMEMP#        SCTOEN
     C                   ENDIF                                                  *IN60 IFEQ
     **
     C                   ELSE
     C                   MOVE      *ON           *IN59
     **
     C                   ENDIF                                                  QQRRN IFNE
     **
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  HFLD IFEQ
     **
     **  TABLE SEARCH - FROM AGENT NAME
     **
     C     HFLD          IFEQ      'SCFRAA'
     **
     C                   CALL      'W0040R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      1             ##VIEW            1 0
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     **
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     **
     C                   MOVE      *ON           *IN61
     **
     C                   ELSE
     **
     C     ##RRN         CHAIN     PAG01                              99
     **
     **
     C                   MOVE      *ON           *IN62
     C                   MOVEL     AGNAME        SCFRAA
     **
     C                   ENDIF                                                  *INKL=*ON
     **
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  HFLD IFEQ
     **
     **  TABLE SEARCH - TO AGENT NAME
     **
     C     HFLD          IFEQ      'SCTOAA'
     **
     C                   CALL      'W0040R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      1             ##VIEW            1 0
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     **
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     **
     C                   MOVE      *ON           *IN62
     **
     C                   ELSE
     **
     C     ##RRN         CHAIN     PAG01                              99
     **
     C                   MOVEL     AGNAME        SCTOAA
     C                   MOVE      *ON           *IN63
     **
     C                   ENDIF
     **
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  HFLD IFEQ
     **
     **  TABLE SEARCH - FROM AGENT NUMBER
     **
     C     HFLD          IFEQ      'SCFRAN'
     **
     C                   CALL      'W0040R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRNF        ##RRN             9 0
     C                   PARM      2             ##VIEW            1 0
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     **
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     **
     C                   MOVE      *ON           *IN63
     **
     C                   ELSE
     **
     C     ##RRN         CHAIN     PAA01                              99
     **
     C                   MOVE      *ON           *IN64
     C                   MOVEL     AAAGT#        SCFRAN
     **
     C                   ENDIF
     **
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  HFLD IFEQ
     **
     **  TABLE SEARCH - TO AGENT NUMBER
     **
     C     HFLD          IFEQ      'SCTOAN'
     **
     C                   CALL      'W0040R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRNF        ##RRN             9 0
     C                   PARM      2             ##VIEW            1 0
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     **
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     **
     C                   MOVE      *ON           *IN64
     **
     C                   ELSE
     **
     C     ##RRN         CHAIN     PAA01                              99
     **
     C                   MOVE      AAAGT#        SCTOAN
     **
     C                   ENDIF                                                  QQAGT# IFNE
     **
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  HFLD IFEQ  '
     **
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  Q2RECF = 'SCR01'
     **
      **** BEGIN ADD **** 11/17/00 *********************************************
     ******************************************************************
     **  PROCESS SCR02 SEARCH FIELDS
     ******************************************************************
     C     Q2RECF        IFEQ      'SCR02'
      **** BEGIN ADD **** 03/21/09 *********************************************
     C     HFLD          IFEQ      'SCDTMK'
     C     SCDFLG        ANDEQ     'Y'
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN68
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL(P)  ##RMCD        SCDTMK
     C                   MOVE      *ON           *IN55
     C                   OTHER
     C                   MOVE      *ON           *IN68
     C                   ENDSL
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      **** END ADD **** 03/21/09 *********************************************
      *
      * * * * * * * * * * * * * * * * * * * * * * * * *
      *  SEARCH A/R CODES
      * * * * * * * * * * * * * * * * * * * * * * * * *
      *
      *
      *  IS THE CURSOR ON A FIELD IN TABLE 'ASE,
      *
     C                   Z-ADD     1             X
     C     HFLD          LOOKUP    ASE(X)                                 60
      *
     C     *IN60         IFEQ      *ON
      *
     C                   CLEAR                   ##RRN
     C                   CALL      'W2164R'      ACDPL
      *
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC IFEQ *ON
      *
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      0
      *
     C     X             ADD       26            X1                2 0
     C                   MOVE      *ON           *IN(X1)
     C                   GOTO      STRT
      *
      *  WAS A VALUE SELECTED OR ENTER PRESSED
      *
     C                   ELSE
      *
     C     X             ADD       27            X1                2 0
     C     ##RRN         CHAIN     WTAVP                              60
     C                   MOVEL(P)  AVKEY         AS(X)
      *
     C                   ENDIF                                                  *INKL IFEQ *ON
      *
     C     X1            IFNE      31
     C                   MOVE      *ON           *IN(X1)
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   ENDIF                                                  X1 IFNE 31
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  Q2RECF='SCR03'
      **** END   ADD **** 11/17/00 *********************************************
     C                   ENDIF                                                  *INKJ = *ON
     **
     **   Process Change of User Parameters
     **
     C     *INKM         IFEQ      *ON
     C                   CALL      'W0006R'
     **
     **  Get the User Parameter File
     **
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPCO#         XXCO#
     **
     **  Get the Fund Record, Load Fund Name for Screen
     **
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              21
     C                   MOVEL     FNNAME        SCNAME
     **
     C                   GOTO      STRT01
     **
     C                   ENDIF                                                  *INKM = ON
     **
     C                   ENDIF
      **** BEGIN ADD **** 11/17/00 *********************************************
      *
      *   GOTO THE APPROPRIATE SCREEN
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   GOTO      SCR01T
     C                   ENDIF                                                  Q2RECF IFEQ 'SCR01'
     C     Q2RECF        IFEQ      'SCR02'
     C                   GOTO      SCR02T
     C                   ENDIF                                                  Q2RECF IFEQ 'SCR02'
      **** END   ADD **** 11/17/00 *********************************************
     **
     **   Process Request Screen
     **
     C     SCR01T        TAG
     **
     ** Validate the Run Date Entered
     **
     C                   Z-ADD     SCDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
     **
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     **
     ** VALIDATE THE REPORT DATE
     **
     C                   Z-ADD     SCRMCY        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C                   MOVEL     SRDTO         SCRMCY
     **
      *** BEG CHG **** 03/05/03 ************************************************
     C********** SRERR     IFEQ '1'
     C  N41SRERR         IFEQ      '1'
      *** END CHG **** 03/05/03 ************************************************
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ELSE
     **
     ** LOAD SCREEN DATE TO WORK FIELD
     **
     C                   MOVE      SCRMCY        XXRPMY            6 0
     **
     C                   ENDIF                                                  SPERR IFEQ '0'
      *** BEG ADD **** 02/18/03 ************************************************
     **
     ** VALIDATE THE REPORT DATE
     **
     C                   Z-ADD     SCRPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     **
     C   41SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ELSE
     **
     ** LOAD SCREEN DATE TO WORK FIELD
     **
     C                   MOVEL     SRDTO         SCRPDT
     **
     C                   ENDIF                                                  SPERR IFEQ '0'
     **
     ** ENSURE THE REPORT DATE IS A MONTH END DATE IF REPORT FREQ IS 'M' OR 'B'
     **
     C                   CALL      'SRFUTR8'
     C                   PARM      SCRPDT        ##RPDT            8 0
     C                   PARM      1             ###DAY            5 0
     C                   PARM                    ##ERR             1
     **
     C                   MOVEL     ##RPDT        XXRPDT            4
     C     2             SUBST     XXRPDT:3      XX1STD            2
     C   41SCFREQ        IFEQ      'M'
     C     SCFREQ        OREQ      'B'
     **
     C     XX1STD        IFNE      '01'
     C                   SETON                                        66
     C                   GOTO      STRT
     C                   ENDIF                                                  XX1STD IFNE 1'
     **
     C                   ENDIF                                                  SCFREQ = 'M'
     **
     C   41SCFREQ        IFNE      'M'
     C     SCFREQ        ANDNE     'W'
     C     SCFREQ        ANDNE     'B'
     C                   SETON                                        67
      *** BEG ADD ***** 05/24/07 **********************************
     C                   GOTO      STRT
      *** END ADD ***** 05/24/07 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFREQ = 'M'
      *** END ADD **** 02/18/03 ************************************************
     C***  Begin Chg  ***  10/27/03  **************************************
     ***********
     ****VALIDATE PAST DUE DATE
     ***********
     C**********           Z-ADDSCPDDT    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     SCPDDT
     C********** SRERR     IFEQ '1'
     C**********           MOVE *ON       *IN22
     C**********           GOTO STRT
     C**********           ENDIF
     ***********
     ****VALIDATE REPORT BY DATE
     ***********
     C**********           Z-ADDSCRBDT    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     SCRBDT
     C********** SRERR     IFEQ '1'
     C**********           MOVE *ON       *IN23
     C**********           GOTO STRT
     C**********           ENDIF
     C*
     C*  Validate Add'l Days
     C*
      *** BEG CHG ***** 12/17/08 *************************************
     C********** SCADDL    IFLE *ZERO
     C     SCADDL        IFLT      *ZERO
      *** END CHG ***** 12/17/08 *************************************
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   ENDIF
     C***  End   Chg  ***  10/27/03  **************************************
     C*** BEG ADD ****** 08/21/08 ************************************
     **
     ** VALIDATE THE RATING STATE ENTERED
     **
     C     SCJRST        IFNE      *BLANK
     C                   MOVEL     SCJRST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN42
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ****** 08/21/08 ************************************
     **
      ***  BEGIN CHG  ***  01/23/98  *********************************
     *****SORT*SELECTION MUST BE "EM OR "EN' OR "AG" OR "AN"
     ***********
     C********** SCSRT     IFEQ '  '
     C**********           MOVE *ON       *IN39
     C**********           GOTO STRT
     ***********
     C**********           ELSE
     ***********
     C********** SCSRT     IFNE 'EA'
     C********** SCSRT     ANDNE'EN'
     C********** SCSRT     ANDNE'AA'
     C********** SCSRT     ANDNE'AN'
     ***********
     C**********           MOVE *ON       *IN24
     C**********           GOTO STRT
     ***********
     C**********           ENDIF                           SCSRT NE EA   '
     ***********
     C**********           ENDIF                           SCSRT EQ ' ''
     **
      * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *   VALIDATE PRIMARY SORT
      * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *
      *   SORT1 MUST BE 'EA' 'EN' 'AA' OR 'AN'
      *
     C                   MOVE      *OFF          *IN24
      *
     C     SCSRT1        IFNE      'EA'
     C     SCSRT1        ANDNE     'EN'
     C     SCSRT1        ANDNE     'AA'
     C     SCSRT1        ANDNE     'AN'
      *
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCSRT1 IFNE
      *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *   VALIDATE SECONDARY SORT
      * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *
      *   SORT2 MUST BE BLANK IF SORT1 IS 'EA' OR 'EN'
      *
     C     SCSRT1        IFEQ      'EA'
     C     SCSRT1        OREQ      'EN'
      *
     C     SCSRT2        IFNE      '  '
     C                   MOVE      *ON           *IN39
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ELSE
      *
      *   SORT2 MUST BE 'EA', 'EN', OR BLANK IF SORT 1 IS 'AA' OR 'AN'
      *
     C     SCSRT2        IFNE      '  '
     C     SCSRT2        ANDNE     'EA'
     C     SCSRT2        ANDNE     'EN'
      *
     C                   MOVE      *ON           *IN17
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCSRT2 IFNE
      *
     C                   ENDIF                                                  SCSRT1 IFEQ
      ***  END   CHG  ***  01/23/98  *********************************
     **
     **  VALIDATE THE EMPLOYER SORT RANGES
     **
     C     SCTOEA        IFNE      *BLANK
     **
     **    CONVERT TO UPPER CASE
     **
     C     LO:UP         XLATE(P)  SCFREA        SCFREA
     C     LO:UP         XLATE(P)  SCTOEA        SCTOEA
     **
     C     SCTOEN        IFNE      *ZERO
     C                   MOVE      *ON           *IN35
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     **
     **  VALIDATE THE AGENT SORT RANGES
     **
     C     SCTOAA        IFNE      *BLANK
     **
     **    CONVERT TO UPPER CASE
     **
     C     LO:UP         XLATE(P)  SCFRAA        SCFRAA
     C     LO:UP         XLATE(P)  SCTOAA        SCTOAA
     **
     C     SCTOAN        IFNE      *ZERO
     C                   MOVE      *ON           *IN47
     C                   GOTO      STRT
     C                   ENDIF
     **
     C                   ENDIF                                                  SCTOAA IFNE
     **
     **  FROM EMPLOYER ALPHA CANT BE GREATER THAN TO EMPLOYER ALPHA
     **
     C     SCFREA        IFGT      SCTOEA
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFREA IFGT
     **
     **
     ** FROM EMPLOYER NUMBER CANT BE GREATER THAN TO EMPLOYER NUMBER
     **
     C     SCFREN        IFGT      SCTOEN
     C                   MOVE      *ON           *IN37
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFREN IFGT
     **
     ** FROM AGENT NAME CANT BE GREATER THAN TO AGENT NAME
     **
     C     SCFRAA        IFGT      SCTOAA
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF                                                   SCFRAA IFGT
     **
     ** FROM AGENT NUMBER CANT BE GREATER THAN TO AGENT NUMBER
     **
     C     SCFRAN        IFGT      SCTOAN
     C                   MOVE      *ON           *IN38
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFRAA IFGT
     **
     ** AMOUNT ENTERED MUST NOT BE LESS THAN ZERO
     **
     C***  Begin Chg  ***  10/27/03  **************************************
     C********** SCAMTD    IFLE *ZERO
     C     SCAMTD        IFLT      *ZERO
     C***  End   Chg  ***  10/27/03  **************************************
     C                   MOVE      *ON           *IN27
     C                   MOVE      *ON           *IN55
     C                   GOTO      STRT
     C                   ENDIF
      *
      * VALIDATE THE COMMENT CODE ENTRIES
      *
     C     SCCOM1        IFNE      *BLANKS
     C                   MOVEL(P)  SCCOM1        XXKEY
     C     TBKEY         CHAIN     PBC01                              28
     C     *IN28         IFEQ      *ON
     C     BCSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCCOM2        IFNE      *BLANKS
     C                   MOVEL(P)  SCCOM2        XXKEY
     C     TBKEY         CHAIN     PBC01                              29
     C     *IN29         IFEQ      *ON
     C     BCSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN29
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCCOM3        IFNE      *BLANKS
     C                   MOVEL(P)  SCCOM3        XXKEY
     C     TBKEY         CHAIN     PBC01                              30
     C     *IN30         IFEQ      *ON
     C     BCSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN30
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCCOM4        IFNE      *BLANKS
     C                   MOVEL(P)  SCCOM4        XXKEY
     C     TBKEY         CHAIN     PBC01                              31
     C     *IN31         IFEQ      *ON
     C     BCSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN31
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     **
     C     SCCOM5        IFNE      *BLANKS
     C                   MOVEL(P)  SCCOM5        XXKEY
     **
     C     TBKEY         CHAIN     PBC01                              32
     **
     C     *IN32         IFEQ      *ON
     C     BCSTAT        OREQ      'I'
     **
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  *IN32 IFEQ
     **
     C                   ENDIF                                                  SCCOM5 IFNE
      *
      * VALIDATE THE DARTS REMARK CODE
      *
     C     SCDRMK        IFNE      *BLANKS
      *
     C     SCDRMK        CHAIN     PCT01                              33
      *
     C     *IN33         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDIF                                                  SCDRMK IFNE
      *
      *** BEG ADD ***** 02/18/03 *********************************************
     C     XXRPMY        IFNE      *ZEROS
      *** END ADD ***** 02/18/03 *********************************************
     C                   MOVE      XXRPMY        XXMCY
     C                   MOVE      XXMCYM        XXMM
     C                   MOVE      XXMCYY        XXYY
     C                   Z-ADD     1             XXDD
     C                   MOVE      XXDATE        QQSTDT            8
      *
     C     XXMM          IFEQ      4
     C     XXMM          OREQ      6
     C     XXMM          OREQ      9
     C     XXMM          OREQ      11
     C                   Z-ADD     30            XXDD
     C                   ELSE
     C     XXMM          IFEQ      2
     C                   Z-ADD     29            XXDD
      *
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C***  BEGIN DEL  ***  03/16/99  **********************************
     C*******************  MOVELSRDTO     XXDATE
     C***  END   DEL  ***  03/16/99  **********************************
      *
     C     SRERR         IFEQ      '1'
     C                   Z-ADD     28            XXDD
     C                   ENDIF                                                  SRERR IFEQ
     C                   ELSE
     C                   Z-ADD     31            XXDD
     C                   ENDIF                                                  XXMM IFEQ
     C                   ENDIF                                                  XXMM IFEQ
     C***** BEGIN ADD **** 03/03/00 ***********************************
      *** BEG ADD ***** 02/18/03 *********************************************
     C                   ENDIF                                                  XXRPMY<>0
      *** END ADD ***** 02/18/03 *********************************************
     C*
     C*  VALIDATE THAT 'GENERATE REPORT' FLAG IS Y OR BLANK
     C*
     C     SCRPTF        IFNE      'Y'
     C     SCRPTF        ANDNE     *BLANK
     C                   MOVE      *ON           *IN43
     C                   GOTO      STRT
     C                   ENDIF                                                  SCRPTF<>Y
     C*
     C***** END   ADD **** 03/03/00 ***********************************
      **** BEGIN ADD **** 11/17/00 *********************************************
     C***  Begin Add  ***  10/27/03  **************************************
     C                   MOVE      'Y'           SCPRBL
     C***  End   Add  ***  10/27/03  **************************************
      *
      *  ALL EDITS HAVE PASSED, LOAD SCREEN 2
      *
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
      *
     C     SCR02T        TAG
      *
      *  VALIDATE 'INCLUDE OR EXCLUDE ACTIVITY STATUS CODES
      *
     C     SCIEFL        IFNE      'I'
     C     SCIEFL        ANDNE     'E'
     C     SCIEFL        ANDNE     ' '
      *
     C                   MOVE      *ON           *IN90
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCIEFL IFNE 'I'
      *
     C                   MOVE      *OFF          *IN94
      *
     C     1             DO        5             X
      *
     C     AS(X)         IFNE      *BLANK
     C                   MOVE      *ON           *IN94
     C                   ENDIF                                                  AS,X IFNE *BLANK
      *
     C                   ENDDO                                                  1 DO 5 X
      *
     C     SCIEFL        IFNE      *BLANK
     C     *IN94         ANDEQ     *OFF
     C                   MOVE      *ON           *IN92
     C                   GOTO      STRT
     C                   ENDIF                                                  SCIEFL IFNE *BLANK
      *
     C     SCIEFL        IFEQ      *BLANK
     C     *IN94         ANDEQ     *ON
     C                   MOVE      *ON           *IN93
     C                   GOTO      STRT
     C                   ENDIF                                                  SCIEFL IFEQ *BLANK
      *
      *  VALIDATE THE ACTIVITY STATUS CODES
      *
     C                   Z-ADD     6             X3                2 0
     C                   Z-ADD     65            XI                2 0
     C                   Z-ADD     21            XN                2 0
      *
     C     1             DO        5             X
      *
     C                   ADD       1             XN
     C     X3            SUB       X             X4                2 0
      *
     C     X4            IFEQ      *ZERO
      *
     C                   ADD       5             X3
     C                   ADD       1             XI
     C                   MOVE      22            XN
      *
     C                   ENDIF                                                  X4 IFEQ *ZERO
      *
     C     AS(X)         IFNE      *BLANKS
      *
     C                   MOVEL(P)  AS(X)         XXKEY
     C     TBKEY         CHAIN     PAV01                              99
      *
     C     *IN99         IFEQ      *ON
     C     AVSTAT        OREQ      'I'
      *
     C     X             ADD       26            X2                2 0
     C                   MOVE      *ON           *IN(X2)
     C                   MOVE      *ON           *IN(XI)
     C                   MOVE      *ON           *IN(XN)
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *IN99 IFEQ *ON
      *
     C                   ENDIF                                                  AS,X IFNE *BLANKS
      *
     C                   ENDDO                                                  1 DO 5 X
      **** END   ADD **** 11/17/00 *********************************************
     C***  Begin Add  ***  10/27/03  **************************************
     C*
     C*  Validate 'Print Balance Info'
     C*
     C     SCPRBL        IFNE      'Y'
     C     SCPRBL        ANDNE     'N'
     C                   MOVE      *ON           *IN71
     C                   GOTO      STRT
     C                   ENDIF
     C*
      **** BEGIN ADD **** 03/21/09 *********************************************
     C*
      * VALIDATE THE SR LATE NOTICE FLAG
     C*
     C     SCDFLG        IFNE      'Y'
     C     SCDFLG        ANDNE     ' '
     C                   MOVE      *ON           *IN70
     C                   GOTO      STRT
     C                   ENDIF                                                  SCDFLG IFNE
      *
      * VALIDATE THE SR LATE NOTICE DARTS REMARK CODE
      *
     C     SCDTMK        IFNE      *BLANKS
      *
     C     SCDTMK        CHAIN     PCT01                              69
      *
     C     *IN69         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDIF                                                  SCDTMK IFNE
     C*
      **** END   ADD **** 03/21/09 *********************************************
     C*  Calc Report by Date
     C*
     C                   Z-ADD     DSDATE        SCRBDT            8 0
     C                   CALL      'SRFUTR8'
     C                   PARM                    SCRBDT            8 0
     C                   PARM      SCADDL        ##DAYS            5 0
     C                   PARM                    ##ERR             1
     C*
     C*  Calc Due Date
     C*
     C     FNMIDY        IFLE      *ZERO
     C                   Z-ADD     1             FNMIDY
     C                   ENDIF
     C*
     C     *IN41         IFEQ      *OFF
     C*
     C     *LIKE         DEFINE    SCRMCY        X6RMCY
     C*
     C     SCRMCY        ADD       10000         X6RMCY
     C                   MOVEL     X6RMCY        MONTH             2 0
     C*
     C     MONTH         IFGT      12
     C                   ADD       1             X6RMCY
     C                   SUB       120000        X6RMCY
     C                   ENDIF
     C*
     C                   Z-ADD     X6RMCY        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         X5RMCY            5 0
     C*
     C                   Z-ADD     FNMIDY        X7PDDT            7 0
     C                   MOVEL     X5RMCY        X7PDDT
     C*
     C                   ELSE
     C*
     C     *LIKE         DEFINE    SCRPDT        X8RPDT
     C*
     C                   CALL      'SRFMTH8'
     C                   PARM      SCRPDT        X8RPDT
     C                   PARM      1             #OFMTH            3 0
     C                   PARM                    SRERR
     C*
     C                   Z-ADD     X8RPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7RPDT            7 0
     C*
     C                   Z-ADD     X7RPDT        X7PDDT
     C                   MOVE      FNMIDY        X7PDDT
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     X7PDDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPDDT            8 0
     C***  End   Add  ***  10/27/03  **************************************
     c*** Begin Add *** 01/28/21 ******************************************
       // Bring Up Scriptura Print Options Window
       Exsr OutControl;
       If xxSubmit = *on;
     c*** End   Add *** 01/28/21 ******************************************
     C*
     ** Call the CLP to Submit the List
     **
      **** BEG ADD ****** 02/18/03 *********************************************
     C     SCRPDT        IFNE      *ZEROS
     C                   MOVE      SCRPDT        QQSTDT
     C                   MOVE      SCRPDT        QQENDT
     C                   ELSE
      **** END ADD ****** 02/18/03 *********************************************
     C                   MOVE      XXDATE        QQENDT            8
      **** BEG ADD ****** 02/18/03 *********************************************
     C                   ENDIF                                                  SCRPDT
      **** END ADD ****** 02/18/03 *********************************************
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND1            3
     C                   MOVE      SCDTE         QQDTE             8
     C***** BEGIN ADD **** 03/03/00 ***********************************
     C                   MOVE      SCRPTF        QQRPTF            1
     C***** END   ADD **** 03/03/00 ***********************************
      *** BEGIN CHG *** 01/23/98 *************************************
     C******************** MOVE SCSRT     QQSRT   2
     C                   MOVE      SCSRT1        QQSRT1            2
     C                   MOVE      SCSRT2        QQSRT2            2
      *** END   CHG *** 01/23/98 *************************************
     C                   MOVE      SCFRAA        QQFRAA            2
     C                   MOVE      SCTOAA        QQTOAA            2
     C                   MOVE      SCFRAN        QQFRAN            5
     C                   MOVE      SCTOAN        QQTOAN            5
     C                   MOVE      SCFREA        QQFREA            2
     C                   MOVE      SCTOEA        QQTOEA            2
     C                   MOVE      SCFREN        QQFREN            9
     C                   MOVE      SCTOEN        QQTOEN            9
     C                   MOVE      SCAMTD        QQAMTD            9
     C                   MOVE      SCRMCY        QQRMCY            6
     C                   MOVE      SCPDDT        QQPDDT            8
     C                   MOVE      SCRBDT        QQRBDT            8
     C*** BEG ADD ****** 08/21/08 ************************************
     C     CLTID         IFEQ      '102'
     C                   MOVE      *ZEROS        QQRBDT            8
     C                   ENDIF                                                  CLTID = 102
     C*** END ADD ****** 08/21/08 ************************************
     C                   MOVE      SCFREA        QQENFR            2
     C                   MOVE      SCTOEA        QQENTO            2
     C***  Begin Chg  ***  10/27/03  **************************************
     C**********           MOVE SCCOM1    QQCOM1  2
     C**********           MOVE SCCOM2    QQCOM2  2
     C**********           MOVE SCCOM3    QQCOM3  2
     C**********           MOVE SCCOM4    QQCOM4  2
     C**********           MOVE SCCOM5    QQCOM5  2
     C                   MOVE      SCCOM1        QQCOM1
     C                   MOVE      SCCOM2        QQCOM2
     C                   MOVE      SCCOM3        QQCOM3
     C                   MOVE      SCCOM4        QQCOM4
     C                   MOVE      SCCOM5        QQCOM5
     C***  End   Chg  ***  10/27/03  **************************************
     C                   MOVE      SCDRMK        QQDRMK            5
      **** BEGIN ADD **** 11/17/00 *********************************************
     C                   MOVE      SCIEFL        QQIEFL            1
     C                   MOVEA     AS            QQASTC           10
      **** END   ADD **** 11/17/00 *********************************************
      **** BEG   ADD **** 02/18/03 *********************************************
     C                   MOVE      SCRPDT        QQRPDT            8
     C                   MOVE      SCFREQ        QQFREQ            1
      **** END   ADD **** 02/18/03 *********************************************
     **
     C                   CALL      'W7570C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND1
     C                   PARM                    QQDTE
     C***** BEGIN ADD **** 03/03/00 ***********************************
     C                   PARM                    QQRPTF
     C***** END   ADD **** 03/03/00 ***********************************
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
      *** BEGIN CHG *** 01/23/98 *************************************
     C******************** PARM           QQSRT
     C                   PARM                    QQSRT1
     C                   PARM                    QQSRT2
      *** BEGIN CHG *** 01/23/98 *************************************
     C                   PARM                    QQFREA
     C                   PARM                    QQTOEA
     C                   PARM                    QQFREN
     C                   PARM                    QQTOEN
     C                   PARM                    QQFRAA
     C                   PARM                    QQTOAA
     C                   PARM                    QQFRAN
     C                   PARM                    QQTOAN
     C                   PARM                    QQAMTD
     C                   PARM                    QQRMCY
     C                   PARM                    QQPDDT
     C                   PARM                    QQRBDT
     C                   PARM                    QQSTDT
     C                   PARM                    QQENDT
     C***  Begin Chg  ***  10/27/03  **************************************
     C**********           PARM           QQCOM1
     C**********           PARM           QQCOM2
     C**********           PARM           QQCOM3
     C**********           PARM           QQCOM4
     C**********           PARM           QQCOM5
     C                   PARM                    QQCOM
     C***  End   Chg  ***  10/27/03  **************************************
     C                   PARM                    QQDRMK
      **** BEGIN ADD **** 11/17/00 *********************************************
     C                   PARM                    QQIEFL
     C                   PARM                    QQASTC
      **** END   ADD **** 11/17/00 *********************************************
      **** BEG   ADD **** 02/18/03 *********************************************
     C                   PARM                    QQRPDT
     C                   PARM                    QQFREQ
      **** END   ADD **** 02/18/03 *********************************************
     C***  Begin Chg  ***  10/27/03  **************************************
     C                   PARM      SCPRBL        QQPRBL            1
     C***  End   Chg  ***  10/27/03  **************************************
     C*** BEG ADD ****** 08/21/08 ************************************
     C                   PARM      SCJRST        QQJRST            2
     C*** END ADD ****** 08/21/08 ************************************
     C*** BEG ADD ****** 03/21/09 ************************************
     C                   PARM      SCDFLG        QQDFLG            1
     C                   PARM      SCDTMK        QQDTMK            5
     C*** END ADD ****** 03/21/09 ************************************
     c*** Begin Add *** 01/28/21 ******************************************
       Else;
     c                   Goto      strt
       EndIf;
     c*** End   Add *** 01/28/21 ******************************************
     **
     **  END THE PROGRAM
     **
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
       //*** Begin Add *** 01/28/21 ****************************************
       //===================================================================
       // Write Output Control Screen
       //===================================================================
       Begsr OutControl;

           xxSubmit = *on;

           If isUseScript;

              xxProcType = processType;
              If (scFREN = scTOEN and scFREN <> 0);
                 xxMass = ' ';
              Else;
                 xxMass = 'Y';
              Endif;

              In *Lock *dtaara;

              Clear svLDA;

              Chain(n) (UPco#:xxFnd) pex01;
              If %found and EXLNRM <> ' ';
                 xxDart = 'Y';
                 xxRMCD = EXLNRM;
              Else;
                 xxDart = 'N';
                 xxRMCD = ' ';
              Endif;
          //*** begin add ***  05/18/21  ***********************************
             If scRptF = 'Y';
                xxDart = 'N';
                xxRMCD = ' ';
                xxCopy = 'Y';
             Endif;
          //*** end add *****  05/18/21  ***********************************

              Out *dtaara;

              xxSubmit = OutputWindow(xxProcType:xxMass);

           Endif;

       Endsr;
       //*** End   Add *** 01/28/21 ****************************************
**
SCDTE
SCRMCY
SCPDDT
SCRBDT
SCAMTD
SCSRT
**
SCAST1
SCAST2
SCAST3
SCAST4
SCAST5
