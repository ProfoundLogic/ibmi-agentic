     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H BNDDIR('WTBNDDIR')
     H****************************************************************
     H/TITLE W7290R - PROMPT FOR LATE NOTICE
     H****************************************************************
     H**    PROGRAM DESCRIPTION:
     H**       THIS PROGRAM PROMPTS FOR POLICY EFFECT DT RANGE, SORT
     H**       SEQUENCE, BALANCE DATE, PAST DUE DT, EMPLR NAME RANGE,
     H**       & A/R CODES. THIS PROGRAM PRODUCES ONE OR ALL AGENTS,
     H**       ONE OR ALL EMPLOYERS, SELECTED A/R CODES OR ALL CODES.
     H******************************************************************
     H* REVISIONS:                                                     *
     H*                                                                *
     H* 01/10/17  C:65  PROJECT: SY371                                 *
     H*                 Implement Scriptura output control window      *
     H*                 when process is flagged to generate Scriptura  *
     H*                 output.  The settings are stored/passed via    *
     H*                 a local data area which is picked up by        *
     H*                 API's and the printing program.                *
      *                                                                *
      * 10/25/16 C:300 Project: WC2456                                 *
      *                - Add new parameter                             *
     H*                                                                *
     H*  08/21/08  C:191 Project: 102WEP                               *
     H*                  BACK TO BASE: BASE OPTION                     *
     H*                   - SELECT CRITERIA BY STATE/LATE NOTICES      *
     H*                                                                *
     H*  01/10/07  C:65  Project: 71WHE                                *
     H*                  ADD PROMPT FOR ADDITIONAL DAYS FOR INSTALLMNT *
     H*                  DATE DUE, TO BASE.                            *
     H*                                                                *
     H*  10/11/06  C:33  Seq#: WA099/41801                             *
     H*                  Use SC305R instead of SC580R.                 *
     H*                                                                *
     H*  10/27/03  C:33  Project: WC1132                               *
     H*                  Revamp Dunning Process                        *
     H*                                                                *
     H* 11/17/00  C:35   PROJECT: WC577                                *
     H*                  ALLOW FOR INCLUDE/EXCLUDE OF UP TO 5 ACTIVITY *
     H*                  STATUS CODES                                  *
     H*                                                                *
     H*    03/20/00  C:118 PROJECT: WC911
     H*                    Add 'Exclude contested audits (Y/ )' prompt
     H*                    to display file. Pass flag to CL programs.
     H*
     H*    03/03/00  C:118 PROJECT: WC974
     H*                    Add 'Generate report only (Y/ )' prompt to
     H*                    display file. Pass flag to CL programs.
     H*                                                               *
     H**   12/11/97  C:87  PROJECT CM033
     H**                  -ALL MOD COMMENTS REMOVED - MAJOR REWRITE
     H**                  -REWORK SEARCHES TO COMBINE REDUNDANT CODE.
     H**                  -LATE NOTICES WILL SELECT ACCTS WITH INSTALL-
     H**                   MENTS WITHIN GIVEN DATE RANGE. ADD VARS:
     H**                   BLDT, PAYD.
     H**                  -PROMPT FOR UNIQUE # OF DAYS. VARS: FIDY,
     H**                   EBDY, ABDY, RADY.
     H**                  -ALLOW SELECTION OF SPECIFIC A/R CODES BASED
     H**                   ON INCLUDE OR EXCLUDE FLAG. VAR: ARIE.
     H**                  -CHECKED THAT LAST ENTRY OF A/R CODE ARRAY
     H**                   HAD NON-BLANKS AS ERR CHK WHEN PASSING PARMS
     H**
     H**   06/25/97  C:79  PROJECT WC711
     H**                  -LATE NOTICES. CHANGE REMARK CODE FILE
     H**
     H**   05/19/97  C:57  PROJECT CM015
     H**                  -WHEN PROMPTING DARTS, REDISPLAY SCR01
     H**
     H**   01/27/97  C:57  PROJECT CM015
     H**                  -ADDED AUDIT STATUS (SCAUST) TO SCR01 & PARMS
     H**                  -CHGED EDIT FOR SCAMTD TO BE > ZERO
     H**
     H**   09/19/96  C:57  PROJECT CM015
     H**                   NEW.  BASED ON PCA W7290R IN WSRCC
     H*****************************************************************
     F*
     FW7290F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     F***  Begin Add  ***  10/27/03  **************************************
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM00)
     F***  End   Add  ***  10/27/03  **************************************
     FWMEMP     IF   E             DISK
     FWMEXL     IF   E           K DISK
     FWMAAP     IF   E             DISK
      **** BEGIN ADD **** 11/17/00 *********************************************
     FWTAVL     IF   E           K DISK
     FWTAVP     IF   E             DISK
     F                                     RENAME(PAV01:PAV02)
     F*** BEG ADD ****** 08/21/08 ************************************
     FSTSTL1    IF   E           K DISK
     F*** END ADD ****** 08/21/08 ************************************
      **** END   ADD **** 11/17/00 *********************************************
      *** begin add ***  01/10/17  ************************************
     d/copy sPrLetter
     D/copy sPrXML
     D isUseScript     S              1n   inz
     d processType     c                   const('Estimated Late Notice')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d
     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxDart                         1    overlay(svlda:251)
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
      *** end add *****  01/10/17  ************************************

     D*
     D CDE             S             10    DIM(30) CTDATA PERRCD(1) ASCEND
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
     D  CD                     1     60
     D                                     DIM(30) ASCEND                       A/R CODES
     D  SCCD01                 1      2
     D  SCCD02                 3      4
     D  SCCD03                 5      6
     D  SCCD04                 7      8
     D  SCCD05                 9     10
     D  SCCD06                11     12
     D  SCCD07                13     14
     D  SCCD08                15     16
     D  SCCD09                17     18
     D  SCCD10                19     20
     D  SCCD11                21     22
     D  SCCD12                23     24
     D  SCCD13                25     26
     D  SCCD14                27     28
     D  SCCD15                29     30
     D  SCCD16                31     32
     D  SCCD17                33     34
     D  SCCD18                35     36
     D  SCCD19                37     38
     D  SCCD20                39     40
     D  SCCD21                41     42
     D  SCCD22                43     44
     D  SCCD23                45     46
     D  SCCD24                47     48
     D  SCCD25                49     50
     D  SCCD26                51     52
     D  SCCD27                53     54
     D  SCCD28                55     56
     D  SCCD29                57     58
     D  SCCD30                59     60
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
     D                 DS
     D  SCDIV                  1      5
     D  XXDIV1                 1      1
     D  XXDIV2                 2      2
     D  XXDIV3                 3      3
     D  XXDIV4                 4      4
     D  XXDIV5                 5      5
     D                 DS
     D  QQID                   1     16
     D  QQFRID                 1      8
     D  QQTOID                 9     16
     D                 DS
     D  QQEA                   1      4
     D  QQFREA                 1      2
     D  QQTOEA                 3      4
     D                 DS
     D  QQEN                   1     18
     D  QQFREN                 1      9
     D  QQTOEN                10     18
     D                 DS
     D  QQAA                   1      4
     D  QQFRAA                 1      2
     D  QQTOAA                 3      4
     D                 DS
     D  QQAN                   1     10
     D  QQFRAN                 1      5
     D  QQTOAN                 6     10
     D                 DS
     D  QQCOM                  1     10
     D  QQCOM1                 1      2
     D  QQCOM2                 3      4
     D  QQCOM3                 5      6
     D  QQCOM4                 7      8
     D  QQCOM5                 9     10
     D                 DS
     D  QQEF                   1     16
     D  QQFREF                 1      8
     D  QQTOEF                 9     16
     D***  End   Add  ***  10/27/03  **************************************
     D*
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D***  Begin Add  ***  10/27/03  **************************************
     D OPTNM           C                   CONST('Late Notices')
     D CHGNUM          C                   CONST('WA003R')
     D***  End   Add  ***  10/27/03  **************************************
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     **
     **   KEY LIST FOR THE COMPANY FILE
     **

     C     COKY          KLIST
     C                   KFLD                    UPCO#
     **
     **  KEY LIST FOR USER PARAMETER FILE
     **
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     **
     **  KEY LIST FOR REMARK TYPE TABLE FILE
     **
     C     CTKY          KLIST
     C                   KFLD                    SCDRMK
     **
     **  KEY LIST FOR TABLE FILE
     **
     C     TBKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     **
     C     TBKY1         KLIST
     C                   KFLD                    UPCO#
     **
     **  KEY LIST FOR FUND MASTER FILE
     **
     C     FNKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C***  Begin Add  ***  10/27/03  **************************************
     C*
     C*   Employer File
     C*
     C     EMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    XXDIV
     C*
     C     EMKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C***  End   Add  ***  10/27/03  **************************************
     **
     **  CLIENT CONTROL FILE
     **
     C     EXKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C*** BEG ADD ****** 08/21/08 ************************************
     C*  SYSTEM TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*** END ADD ****** 08/21/08 ************************************
     *******************************************************************
     **  ENTRY PARAMETERS
     *******************************************************************
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
     **
     ** DATE CONVERSION 6 TO 8 DIGITS
     **
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      **** BEGIN ADD **** 11/17/00 *********************************************
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
      **** END   ADD **** 11/17/00 *********************************************
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C***  End   Add  ***  10/27/03  **************************************
     **
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPCO#         @@CO#
     C     *LIKE         DEFINE    EMFND         @@FND
     C     *LIKE         DEFINE    EMEMP#        @@EMP#
     C     *LIKE         DEFINE    EMDIV         @@DIV
     C*
      *** begin add ***  01/10/17  ***********************************
      /Free
       isUseScript = UseScriptura(processType);
       isScript = isUseScript;
      /end-free
      *** end add *****  01/10/17  ***********************************
      *
     C                   Z-ADD     0             QQVIEW            1 0
     **
     **  GET THE USER PARAMETER FILE
     **
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C                   MOVE      UPCO#         XXCO#
     **
     **  GET THE COMPANY RECORD
     **
     C     STRT01        TAG
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C***  Begin Del  ***  10/27/03  **************************************
     C**********           MOVE UPFND     SCFND
     C***  End   Del  ***  10/27/03  **************************************
     **
     **  GET THE REQUESTED RECORD, LOAD INFORMATION
     **
     C***  Begin Del  ***  10/27/03  **************************************
     C**********           MOVE SCFND     XXFND
     C**********           MOVE UPCO#     XXCO#
     C********** FNKY      CHAINPFN01                60
     C**********           MOVELFNNAME    SCNAME
     C***  End   Del  ***  10/27/03  **************************************
     **
     C                   MOVE      *BLANK        CD
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
     C***  End   Chg  ***  10/27/03  **************************************
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
      *
     C***  Begin Del  ***  10/27/03  **************************************
     C**********           MOVE DSDATE    SCPAYD    P
     ***********
     *****Get*Default DARTS Remark Code From Admin/DARTS control maint.
     ***********
     C********** EXKY      CHAINPEX01                60
     ***********
     C********** *IN60     IFEQ *ON
     C**********           MOVEL*BLANK    SCDRMK
     ***********
     C**********           ELSE
     C**********           MOVELEXLNRM    SCDRMK
     ***********
     C**********           ENDIF
     C***  End   Del  ***  10/27/03  **************************************
     **
     **   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     **
     C                   MOVE      *ON           *IN62
     C                   MOVEL(P)  'SCR01'       Q2RECF
     **
     **   DISPLAY ALL FORMATS
     **
     C     STRT          TAG
     C***  Begin Del  ***  10/27/03  **************************************
      ***********
      **CONVERT*BILLINGS THROUGH DATE
      ***********
     C***********          Z-ADDSCBLDT    SRDTI
     C***********          CALL 'SRCVTDAT'DTMDY8
      ***********
     C***********SRERR     IFEQ '0'
     C***********          MOVELSRDTO     SCBLDT
     C***********          ENDIF
      ***********
      ***CONVERT*PAYMENTS THROUGH DATE
      ***********
     C***********          Z-ADDSCPAYD    SRDTI
     C***********          CALL 'SRCVTDAT'DATCHK
      ***********
     C***********SRERR     IFEQ '0'
     C***********          MOVELSRDTO     SCPAYD
     C***********          ENDIF
     C***  End   Del  ***  10/27/03  **************************************
      *
      *  CONVERT INSTALLMENT DATE RANGE 'FROM'
      *
     C                   Z-ADD     SCFRID        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         SCFRID
     C                   ENDIF
      *
      *  CONVERT INSTALLMENT DATE RANGE 'TO'
      *
     C                   Z-ADD     SCTOID        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         SCTOID
     C                   ENDIF
     C***  Begin Add  ***  10/27/03  **************************************
      *
      * Convert Policy Effective Date 'FROM'
      *
     C                   Z-ADD     SCFREF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
      *
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         SCFREF
     C                   ENDIF
      *
      * Convert Policy Effective Date 'TO'
      *
     C                   Z-ADD     SCTOEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
      *
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         SCTOEF
     C                   ENDIF
     C***  End   Add  ***  10/27/03  **************************************
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
     **
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   ENDIF
      **** BEGIN ADD **** 11/17/00 *********************************************
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   ENDIF                                                  Q2RECF IFEQ 'SCR03'
      **** END   ADD **** 11/17/00 *********************************************
     **
     **   PROCESS ALL COMMAND KEYS
     **
     C     *IN11         IFEQ      *ON
     **
     **   FUNCTION 3 = END OF JOB
     **
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF
     **
     **   FUNCTION 12= PRIOR SCREEN
     **
     C     *INKL         IFEQ      *ON
     C     Q2RECF        ANDEQ     'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      **** BEGIN ADD **** 11/17/00 *********************************************
     C     *INKL         IFEQ      *ON
     C     Q2RECF        ANDEQ     'SCR03'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL IFEQ *ON
      **** END   ADD **** 11/17/00 *********************************************
     **
     **  FUNCTION 10= TABLE FILE SEARCH
     **
     C     *INKJ         IFEQ      *ON
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C     *LIKE         DEFINE    Q2RECF        SVRECF
     C                   MOVE      Q2RECF        SVRECF
     C***  End   Add  ***  10/27/03  **************************************
     **
     C***  Begin Del  ***  10/27/03  **************************************
     C********** Q2RECF    IFEQ 'SCR01'
     C***  End   Del  ***  10/27/03  **************************************
     C***  Begin Add  ***  10/27/03  **************************************
     C     HFLD          IFEQ      'SCFND'
     C     HFLD          OREQ      'SCTFND'
     C                   EXSR      GRPSCH
     C                   GOTO      STRT
     C                   ENDIF
     C***  End   Add  ***  10/27/03  **************************************
      *
      * * * * * * * * * * * * * * * * * * * * * * * * *
      *  TABLE SEARCH - COMMENT CODES
      * * * * * * * * * * * * * * * * * * * * * * * * *
      *
     C     HFLD          IFEQ      'SCCOM1'
     C     HFLD          OREQ      'SCCOM2'
     C     HFLD          OREQ      'SCCOM3'
     C     HFLD          OREQ      'SCCOM4'
     C     HFLD          OREQ      'SCCOM5'
     C                   EXSR      SRPCMT
     C                   ENDIF                                                  SCCOM1 = '?'
     **
     **  TABLE SEARCH - DARTS REMARK CODE
     **
     C     HFLD          IFEQ      'SCDRMK'
     **
     C***  Begin Chg  ***  10/11/06  **************************************
     C**********           CALL 'SC580R'
     C**********           PARM           XXDRMK
     C**********           PARM           QQRTCD  1
     C**********
     C********** QQRTCD    IFEQ 'A'
     C**********           GOTO TERM
     C**********           ENDIF
     ***********
     C********** QQRTCD    IFEQ 'B'
     C*****Begin Add  ***  10/27/03  **************************************
     C********** XXDRMK    OREQ *BLANK
     C*****End** Add  ***  10/27/03  **************************************
     C**********           MOVE *ON       *IN59
     C**********           ENDIF
     ***********
     C********** QQRTCD    IFEQ '*'
     C*****Begin Add  ***  10/27/03  **************************************
     C********** XXDRMK    ANDNE*BLANK
     C*****End** Add  ***  10/27/03  **************************************
     C**********           MOVELXXDRMK    SCDRMK    P
     C*****Begin Chg  ***  10/27/03  **************************************
     C**********           MOVE *ON       *IN53
     C**********           MOVE *ON       *IN31
     C*****End** Chg  ***  10/27/03  **************************************
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
     C                   MOVE      *ON           *IN59
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL(P)  ##RMCD        SCDRMK
     C                   MOVE      *ON           *IN31
     C                   OTHER
     C                   MOVE      *ON           *IN59
     C                   ENDSL
     C***  End   Chg  ***  10/11/06  **************************************
     **
     C***  Begin Chg  ***  10/27/03  **************************************
     C**********           MOVEL'SCR01'   Q2RECF    P
     C                   MOVEL(P)  SVRECF        Q2RECF
     C***  End   Chg  ***  10/27/03  **************************************
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  HFLD = SCDRM
     **
     C***  Begin Del  ***  10/27/03  **************************************
     C**********           GOTO STRT
     ***********
     C**********           ENDIF                           Q2RECF = 'SCR01'
     ******************************************************************
     ****PROCESS SCR02 SEARCH FIELDS
     ******************************************************************
     C********** Q2RECF    IFEQ 'SCR02'
     C***  End   Del  ***  10/27/03  **************************************
      *
      * * * * * * * * * * * * * * * * * * * * * * * * *
      *  TABLE SEARCH - FROM/TO EMPLOYER NAME/NUMBER
      * * * * * * * * * * * * * * * * * * * * * * * * *
      *
     C     HFLD          IFEQ      'SCFREA'
     C     HFLD          OREQ      'SCTOEA'
     C     HFLD          OREQ      'SCFREN'
     C     HFLD          OREQ      'SCTOEN'
     C***  Begin Add  ***  10/27/03  **************************************
     C     HFLD          OREQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C     SCFND         IFEQ      *ZERO
     C     SCFND         ORNE      SCTFND
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     SCFND         XXFND
     C     FNKY          CHAIN     PFN01                              60
     C***  End   Add  ***  10/27/03  **************************************
      *
     C     HFLD          IFEQ      'SCFREA'
     C     HFLD          OREQ      'SCTOEA'
     C***  Begin Add  ***  10/27/03  **************************************
     C     HFLD          OREQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C***  End   Add  ***  10/27/03  **************************************
     C                   Z-ADD     1             QQVIEW
     C                   ENDIF
      *
     C     HFLD          IFEQ      'SCFREN'
     C     HFLD          OREQ      'SCTOEN'
     C                   Z-ADD     2             QQVIEW
     C                   ENDIF
      *
     C                   CALL      'W0050R'
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM      DBRRNF        QQRRN2            9 0
     C                   PARM                    QQQTE             1
     C                   PARM                    QQVIEW
      *
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     *INKL         IFEQ      *ON
     C     QQRRN         OREQ      *ZERO
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCFREA'
     C                   MOVE      *ON           *IN61
      *
     C     HFLD          WHENEQ    'SCTOEA'
     C                   MOVE      *ON           *IN87
      *
     C     HFLD          WHENEQ    'SCFREN'
     C                   MOVE      *ON           *IN79
      *
     C     HFLD          WHENEQ    'SCTOEN'
     C                   MOVE      *ON           *IN80
      *
     C***  Begin Add  ***  10/27/03  **************************************
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C                   MOVE      *ON           *IN31
     C***  End   Add  ***  10/27/03  **************************************
      *
     C                   ENDSL
      *
     C     *INKL         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDIF                                                  *INKL=*ON OR QQRRN
      *
      * LOAD EMPLOYER NAME/NUMBER
      *
     C     QQRRN         IFNE      *ZERO
      *
     C     QQRRN         CHAIN     WMEMP                              60
      *
     C     *IN60         IFEQ      *ZERO
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCFREA'
     C                   MOVEL     EMNAME        SCFREA
     C                   MOVE      *ON           *IN87
      *
     C     HFLD          WHENEQ    'SCTOEA'
     C                   MOVEL     EMNAME        SCTOEA
     C                   MOVE      *ON           *IN79
      *
     C     HFLD          WHENEQ    'SCFREN'
     C                   MOVEL     EMEMP#        SCFREN
     C                   MOVE      *ON           *IN80
      *
     C     HFLD          WHENEQ    'SCTOEN'
     C                   MOVEL     EMEMP#        SCTOEN
     C***  Begin Add  ***  10/27/03  **************************************
     C**********           MOVE *ON       *IN81
     C                   MOVE      *ON           *IN32
     C***  End   Add  ***  10/27/03  **************************************
      *
     C***  Begin Add  ***  10/27/03  **************************************
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVEL     EMDIV         XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C     XXNUM         IFNE      *BLANK
     C                   MOVEL     XXNUM         SCDIV
     C                   ELSE
     C                   MOVE      '0'           SCDIV
     C                   ENDIF
     C                   MOVE      *ON           *IN61
     C***  End   Add  ***  10/27/03  **************************************
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  QQRRN IFNE
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD=SCFREA
      *
      * * * * * * * * * * * * * * * * * * * * * * * *
      *  TABLE SEARCH - FROM/TO AGENT NAME/NUMBER
      * * * * * * * * * * * * * * * * * * * * * * * *
      *
     C     HFLD          IFEQ      'SCFRAN'
     C     HFLD          OREQ      'SCTOAN'
     C     HFLD          OREQ      'SCFRAA'
     C     HFLD          OREQ      'SCTOAA'
      *
     C     HFLD          IFEQ      'SCFRAN'
     C     HFLD          OREQ      'SCTOAN'
     C                   Z-ADD     2             QQVIEW
     C                   ENDIF
      *
     C     HFLD          IFEQ      'SCFRAA'
     C     HFLD          OREQ      'SCTOAA'
     C                   Z-ADD     1             QQVIEW
     C                   ENDIF
      *
     C                   CALL      'W0040R'
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      DBRRNF        QQRRN             9 0
     C                   PARM                    QQVIEW
      *
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
      *
     C     *INKL         IFEQ      *ON
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCFRAN'
     C                   MOVE      *ON           *IN85
      *
     C     HFLD          WHENEQ    'SCTOAN'
     C                   MOVE      *ON           *IN86
      *
     C     HFLD          WHENEQ    'SCFRAA'
     C                   MOVE      *ON           *IN81
      *
     C     HFLD          WHENEQ    'SCTOAA'
     C                   MOVE      *ON           *IN83
      *
     C                   ENDSL
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL = *ON
      *
     C     QQRRN         IFNE      *ZERO
      *
     C     QQRRN         CHAIN     WMAAP                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCFRAN'
     C                   MOVEL     AAAGT#        SCFRAN
     C                   MOVE      *ON           *IN86
      *
     C     HFLD          WHENEQ    'SCTOAN'
     C                   MOVEL     AAAGT#        SCTOAN
     C                   MOVE      *ON           *IN91
      *
     C     HFLD          WHENEQ    'SCFRAA'
     C                   MOVEL     AANAME        SCFRAA
     C                   MOVE      *ON           *IN83
      *
     C     HFLD          WHENEQ    'SCTOAA'
     C                   MOVEL     AANAME        SCTOAA
     C                   MOVE      *ON           *IN85
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  *IN60 = *ZERO
      *
     C                   ENDIF                                                  QQRRN NE *ZERO
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD IFEQ SCFRAN
      *
      * * * * * * * * * * * * * * * * * * * * * * * * *
      *  SEARCH A/R CODES
      * * * * * * * * * * * * * * * * * * * * * * * * *
      *
     C***  Begin Del  ***  10/27/03  **************************************
     C********** 1         DO   30        X       20
     C**********           MOVELCD,X      XX2     2
     C***  End   Del  ***  10/27/03  **************************************
     **
     **  IS THE CURSOR ON A FIELD IN TABLE 'CDE'
     **
     C***  Begin Add  ***  10/27/03  **************************************
     C                   Z-ADD     1             X                 3 0
     C***  End   Add  ***  10/27/03  **************************************
     C     HFLD          LOOKUP    CDE(X)                                 60
     **
     C     *IN60         IFEQ      *ON
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C     X             SUB       1             XR                3 0          ROW
     C                   DIV       5             XR
     C                   ADD       1             XR
     C     XR            SUB       1             XC                3 0          COLUMN
     C                   MULT      5             XC
     C     X             SUB       XC            XC
     C***  End   Add  ***  10/27/03  **************************************
     **
     C                   CALL      'W2161R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      *BLANKS       QQKEY            10
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     **
     C     *INKL         IFEQ      *ON
     **
     C***  Begin Chg  ***  10/27/03  **************************************
     C********** X         ADD  26        X1      20
     C**********           MOVE *ON       *IN,X1
     C                   ADD       20            XC
     C                   ADD       80            XR
     C                   MOVE      *ON           *IN(XR)
     C                   MOVE      *ON           *IN(XC)
     C***  End   Chg  ***  10/27/03  **************************************
     C                   GOTO      STRT
     **
     **
     **  WAS A VALUE SELECTED OR ENTER PRESSED
     **
     C                   ELSE
     **
     C***  Begin Chg  ***  10/27/03  **************************************
     C********** QQKEY     IFEQ *BLANKS
     C********** X         ADD  26        X1      20
     ***********
     C**********           ELSE
     C********** X         ADD  27        X1      20
     C**********           MOVELQQKEY     CD,X
     ***********
     C**********           ENDIF
     ***********
     C********** X1        IFNE 57
     C**********           MOVE *ON       *IN,X1
     ***********
     C**********           MOVEL'SCR02'   Q2RECF    P
     C**********           ENDIF
     ***********
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         CD(X)
     C                   ADD       1             XC
     C                   ENDIF
     C*
     C     XC            IFGT      5
     C                   SUB       5             XC
     C                   ADD       1             XR
     C                   ENDIF
     C*
     C                   ADD       20            XC
     C                   ADD       80            XR
     C                   MOVE      *ON           *IN(XR)
     C                   MOVE      *ON           *IN(XC)
     C***  End   Chg  ***  10/27/03  **************************************
     C*
     C                   GOTO      STRT
     C                   ENDIF
     **
     C                   ENDIF
     C***  Begin Del  ***  10/27/03  **************************************
     C**********           ENDDO
     C***  End   Del  ***  10/27/03  **************************************
     **
     C***  Begin Del  ***  10/27/03  **************************************
     C**********           GOTO STRT
     C**********           ENDIF                           Q2RECF='SCR02'
     ***********
      *****BEGIN ADD **** 11/17/00 *********************************************
     ******************************************************************
     ****PROCESS SCR03 SEARCH FIELDS
     ******************************************************************
     C********** Q2RECF    IFEQ 'SCR03'
     C***  End   Del  ***  10/27/03  **************************************
      *
      * * * * * * * * * * * * * * * * * * * * * * * * *
      *  SEARCH A/R CODES
      * * * * * * * * * * * * * * * * * * * * * * * * *
      *
      *
      *  IS THE CURSOR ON A FIELD IN TABLE 'ASE,
      *
     C***  Begin Chg  ***  10/27/03  **************************************
     C**********           Z-ADD1         X
     C                   Z-ADD     1             X                 3 0
     C***  End   Chg  ***  10/27/03  **************************************
     C     HFLD          LOOKUP    ASE(X)                                 60
      *
     C     *IN60         IFEQ      *ON
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C     X             SUB       1             XR                3 0          ROW
     C                   DIV       5             XR
     C                   ADD       1             XR
     C     XR            SUB       1             XC                3 0          COLUMN
     C                   MULT      5             XC
     C     X             SUB       XC            XC
     C***  End   Add  ***  10/27/03  **************************************
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
     C***  Begin Chg  ***  10/27/03  **************************************
     C********** X         ADD  26        X1      20
     C**********           MOVE *ON       *IN,X1
     C                   ADD       20            XC
     C                   ADD       95            XR
     C                   MOVE      *ON           *IN(XR)
     C                   MOVE      *ON           *IN(XC)
     C***  End   Chg  ***  10/27/03  **************************************
     C                   GOTO      STRT
      *
      *  WAS A VALUE SELECTED OR ENTER PRESSED
      *
     C                   ELSE
      *
     C***  Begin Del  ***  10/27/03  **************************************
     C********** X         ADD  27        X1      20
     C***  End   Del  ***  10/27/03  **************************************
     C     ##RRN         CHAIN     WTAVP                              60
     C                   MOVEL(P)  AVKEY         AS(X)
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C                   ADD       1             XC
     C*
     C     XC            IFGT      5
     C                   SUB       5             XC
     C                   ADD       1             XR
     C                   ENDIF
     C*
     C                   ADD       20            XC
     C                   ADD       95            XR
     C                   MOVE      *ON           *IN(XR)
     C                   MOVE      *ON           *IN(XC)
     C***  End   Add  ***  10/27/03  **************************************
      *
     C                   ENDIF                                                  *INKL IFEQ *ON
      *
     C***  Begin Chg  ***  10/27/03  **************************************
     C********** X1        IFNE 31
     C**********           MOVE *ON       *IN,X1
     C**********           MOVEL'SCR03'   Q2RECF    P
     C**********           ENDIF                           X1 IFNE 31
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C***  End   Chg  ***  10/27/03  **************************************
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *
     C                   GOTO      STRT
      *
     C***  Begin Del  ***  10/27/03  **************************************
     C**********           ENDIF                           Q2RECF='SCR03'
     C***  End   Del  ***  10/27/03  **************************************
      **** END   ADD **** 11/17/00 *********************************************
     C                   ENDIF                                                  *INKJ
     **
     **   PROCESS HELP REQUESTS
     **
     C     *IN10         IFEQ      *ON
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     **
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     **
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     **
     **   PROCESS CHANGE OF USER PARAMETERS
     **
     C     *INKM         IFEQ      *ON
     C     Q2RECF        IFEQ      'SCR01'
     **
     C                   CALL      'W0006R'
     **
     **  GET THE USER PARAMETER FILE
     **
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C                   MOVE      UPCO#         XXCO#
     C                   GOTO      STRT01
     C                   ENDIF                                                  Q2RECF = SCR01
     **
     C                   ENDIF                                                  *INKM = *ON
     C                   ENDIF                                                  *IN11 = *ON
     **
     **   GOTO THE APPROPRIATE SCREEN
     **
     C     Q2RECF        IFEQ      'SCR01'
     C                   GOTO      SCR01T
     C                   ENDIF
     C     Q2RECF        IFEQ      'SCR02'
     C                   GOTO      SCR02T
     C                   ENDIF
      **** BEGIN ADD 11/17/00 **************************************************
     C     Q2RECF        IFEQ      'SCR03'
     C                   GOTO      SCR03T
     C                   ENDIF                                                  Q2RECF IFEQ 'SCR03'
      **** END   ADD 11/17/00 **************************************************
     **
     **   PROCESS REQUEST SCREEN
     **
     C     SCR01T        TAG
     C***  Begin Del  ***  10/27/03  **************************************
      **********
      ***VALIDATE THE BILLINGS THROUGH DATE
      **********
     C**********           Z-ADDSCBLDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY8
     C**********           MOVELSRDTO     SCBLDT
      **********
     C********** SRERR     IFEQ '1'
     C**********           MOVE *ON       *IN21
     C**********           GOTO STRT
     C**********           ENDIF
      **********
      ***VALIDATE PAYMENTS THROUGH DATE
      **********
     C**********           Z-ADDSCPAYD    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     SCPAYD
      **********
     C********** SRERR     IFEQ '1'
     C**********           MOVE *ON       *IN23
     C**********           GOTO STRT
     C**********           ENDIF
     C***  End   Del  ***  10/27/03  **************************************
     C***  Begin Add  ***  10/27/03  **************************************
     C*
     C*  Validate groups
     C*
     C     SCFND         IFNE      *ZERO
     C                   MOVE      SCFND         XXFND
     C     FNKY          CHAIN     PFN01                              21
     C   21              GOTO      STRT
     C                   ENDIF
     C*
     C     SCTFND        IFNE      *ZERO
     C                   MOVE      SCTFND        XXFND
     C     FNKY          CHAIN     PFN01                              22
     C   22              GOTO      STRT
     C                   ENDIF
     C*
     C     SCTFND        IFLT      SCFND
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   ENDIF
     C***  End   Add  ***  10/27/03  **************************************
      *
      *  VALIDATE THE MINIMUM AMT IS NOT LESS THAN ZERO
      *
     C     SCAMTD        IFLE      *ZERO
     C                   MOVE      *ON           *IN24
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  VALIDATE INTERIM PERIOD FOR FIRST INSTALLMENT IS NOT < 0
      *
     C     SCFIDY        IFLT      *ZERO
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  VALIDATE INTERIM PERIOD FOR ESTIMATED BILLING IS NOT < 0
      *
     C     SCEBDY        IFLT      *ZERO
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  VALIDATE INTERIM PERIOD FOR AUDIT BILLING IS NOT < 0
      *
     C     SCABDY        IFLT      *ZERO
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  VALIDATE INTERIM PERIOD FOR REVISED ADT BILLING IS NOT < 0
      *
     C     SCRADY        IFLT      *ZERO
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  VALIDATE INSTALLMENT DATE RANGE 'FROM'
      *
     C                   Z-ADD     SCFRID        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRID
      *
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  VALIDATE INSTALLMENT DATE RANGE 'TO'
      *
     C                   Z-ADD     SCTOID        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTOID
      *
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  VALIDATE INSTALLMENT DATE RANGE 'FROM' < 'TO'
      *
     C                   Z-ADD     SCFRID        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT            7 0
     C                   Z-ADD     SCTOID        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT            7 0
      *
     C     XXFRDT        IFGT      XXTODT
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   ENDIF
     C***  Begin Del  ***  10/27/03  **************************************
      **********
      ***VALIDATE BILLING THROUGH DATE WITHIN INSTLMNT DATE RANGE
      **********
     C**********           Z-ADDSCBLDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXBLDT  70
      **********
     C********** XXBLDT    IFLT XXFRDT
     C********** XXBLDT    ORGT XXTODT
     C**********           MOVE *ON       *IN95
     C**********           GOTO STRT
     C**********           ELSE
     C**********           MOVE *OFF      *IN95
     C**********           ENDIF                           XXBLDT < XXFRDT
      **********
      ***VALIDATE PAYMENTS THROUGH DATE NOT < BILLING THROUGH DATE
      **********
     C**********           Z-ADDSCPAYD    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXPAYD  70
      **********
     C********** XXPAYD    IFLT XXBLDT
     C**********           MOVE *ON       *IN96
     C**********           GOTO STRT
     C**********           ELSE
     C**********           MOVE *OFF      *IN96
     C**********           ENDIF                           XXPAYD < XXBLDT
     C***  End   Del  ***  10/27/03  **************************************
     **
     C***** BEGIN ADD **** 03/03/00 ***********************************
     C*
     C*  VALIDATE THAT 'GENERATE REPORT' FLAG IS Y OR BLANK
     C*
     C     SCRPTF        IFNE      'Y'
     C     SCRPTF        ANDNE     *BLANK
     C                   MOVE      *ON           *IN94
     C                   GOTO      STRT
     C                   ENDIF                                                  SCRPTF<>Y
     C*
     C***** END   ADD **** 03/03/00 ***********************************
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C*
     C*  VALIDATE THAT 'EXCLUDE CONTESTED AUDITS' FLAG IS Y OR BLANK
     C*
     C     SCCTST        IFNE      'Y'
     C     SCCTST        ANDNE     *BLANK
     C                   MOVE      *ON           *IN97
     C                   GOTO      STRT
     C                   ENDIF                                                   SCCTST<>Y
     C*
     C***** END   ADD **** 03/20/00 ***********************************
     C***** BEGIN ADD **** 01/10/07 ***********************************
     C*
     C*  VALIDATE THAT ADDL DAYS FOR INST DATE DUE IS NOT LESS THAN ZERO.
     C*
     C     SCADDU        IFLT      *ZERO
     C                   MOVE      *ON           *IN46
     C                   GOTO      STRT
     C                   ENDIF                                                   SCADDU IFLT 0
     C*
     C***** END   ADD **** 01/10/07 ***********************************
     **
     **  VALIDATE ESTIMATED/SELF REPORTING FLAG
     **
     C     SCESSR        IFNE      'E'
     C     SCESSR        ANDNE     'S'
     C     SCESSR        ANDNE     'B'
     C                   MOVE      *ON           *IN29
     C                   GOTO      STRT
     C                   ENDIF
     **
     **   ACCOUNT STATUS MUST BE 'A', 'B', OR 'C'
     **
     C     SCACST        IFNE      'A'
     C     SCACST        ANDNE     'C'
     C     SCACST        ANDNE     'B'
     C                   MOVE      *ON           *IN30
     C                   GOTO      STRT
     C                   ENDIF
     **
     **   AUDIT STATUS MUST BE 'A', 'N', OR 'B'
     **
     C     SCAUST        IFNE      'A'                                          AUDIT
     C     SCAUST        ANDNE     'N'                                          NON AUDITS
     C     SCAUST        ANDNE     'B'                                          BOTH
     C                   MOVE      *ON           *IN45
     C                   GOTO      STRT
     C                   ENDIF
     C*** BEG ADD ****** 08/21/08 ************************************
     **
     ** VALIDATE THE RATING STATE ENTERED
     **
     C     SCJRST        IFNE      *BLANK
     C                   MOVEL     SCJRST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ****** 08/21/08 ************************************
     **
     ** VALIDATE THE COMMENT CODE ENTRIES
     **
     C     SCCOM1        IFNE      *BLANKS
     C                   MOVEL(P)  SCCOM1        XXKEY
     C     TBKY          CHAIN     PBC01                              38
     C     *IN38         IFEQ      *ON
     C     BCSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN38
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCCOM2        IFNE      *BLANKS
     C                   MOVEL(P)  SCCOM2        XXKEY
     C     TBKY          CHAIN     PBC01                              39
     C     *IN39         IFEQ      *ON
     C     BCSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN39
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCCOM3        IFNE      *BLANKS
     C                   MOVEL(P)  SCCOM3        XXKEY
     C     TBKY          CHAIN     PBC01                              40
     C     *IN40         IFEQ      *ON
     C     BCSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN40
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCCOM4        IFNE      *BLANKS
     C                   MOVEL(P)  SCCOM4        XXKEY
     C     TBKY          CHAIN     PBC01                              41
     C     *IN41         IFEQ      *ON
     C     BCSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN41
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCCOM5        IFNE      *BLANKS
     C                   MOVEL(P)  SCCOM5        XXKEY
     C     TBKY          CHAIN     PBC01                              42
     C     *IN42         IFEQ      *ON
     C     BCSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN42
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C***  Begin Del  ***  10/27/03  **************************************
      **********
      **VALIDATE THE DARTS REMARK CODE
      **********
     C********** SCDRMK    IFNE *BLANKS
     C********** SCDRMK    CHAINPCT01                43
     C********** *IN43     IFEQ *ON
     C**********           GOTO STRT
     C**********           ENDIF
     C**********           ENDIF
     C***  End   Del  ***  10/27/03  **************************************
     **
     **  ALL EDITS HAVE PASSED, LOAD SCREEN 2
     **
     C***  Begin Add  ***  10/27/03  **************************************
     C     SCFND         IFEQ      SCTFND
     C                   Z-ADD     SCFND         XXFND
     C                   ELSE
     C                   CLEAR                   XXFND
     C                   ENDIF
     **
     **   Get Default DARTS Remark Code From Admin/DARTS control maint.
     **
     C     EXKY          CHAIN     PEX01                              60
     **
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANK        SCDRMK
     **
     C                   ELSE
     C                   MOVEL     EXLNRM        SCDRMK
     **
     C                   ENDIF
     C*
     C                   MOVE      'N'           SCDART
     C***  End   Add  ***  10/27/03  **************************************
     C*
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     **
     C     SCR02T        TAG
     C***  Begin Add  ***  10/27/03  **************************************
      *
      *  Validate the DARTS Remark Code
     C*
     C     SCDART        IFNE      'Y'
     C     SCDART        ANDNE     'N'
     C                   MOVE      *ON           *IN44
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     SCDRMK        IFNE      *BLANKS
     C     SCDART        OREQ      'Y'
     C     SCDRMK        CHAIN     PCT01                              43
     C   43              GOTO      STRT
     C                   ENDIF
     C*
     C*  Validate Employer, if entered
     C*
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    SCDIV         XZDIV
     C     *LIKE         DEFINE    EMEMP#        XZEMP#
     C     *LIKE         DEFINE    *IN27         EMPCHG
     C*
     C     SCEMP#        IFNE      *ZERO
     C*
     C     SCFND         IFEQ      *ZERO
     C     SCFND         ORNE      SCTFND
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     SCDIV         IFNE      *BLANKS
     C*
     C*  BE TO LOAD THE CORRECT VALUE IF FIELD EXIT WAS NOT PRESSSED
     C*
     C     XXDIV5        DOWEQ     *BLANK
     C                   MOVE      XXDIV4        XXDIV5
     C                   MOVE      XXDIV3        XXDIV4
     C                   MOVE      XXDIV2        XXDIV3
     C                   MOVE      XXDIV1        XXDIV2
     C                   CLEAR                   XXDIV1
     C                   ENDDO                                                  XXDIV5=*BLANK
     C*
     C                   MOVE      SCDIV         XXDIV
     C*
     C     EMKY          CHAIN     PEM00                              26
     C*
     C                   ELSE
     C     EMKY2         CHAIN     PEM00                              26
     C                   ENDIF                                                  SCDIV<>*BLANKS
     C*
     C     *IN26         CABEQ     '1'           STRT
     C                   ENDIF
     C*
     C     SCEMP#        IFEQ      *ZEROS
     C     SCDIV         ANDNE     *BLANKS
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF                                                  SCEMP#=*ZEROS
     C*
     C*  Check for DIV=*BLANK first time thru and send message
     C*
     C     SCEMP#        IFNE      *ZEROS
     C*
     C     SCDIV         IFNE      XZDIV
     C     SCEMP#        ORNE      XZEMP#
     C                   MOVE      *ON           EMPCHG
     C                   MOVE      SCDIV         XZDIV
     C                   MOVE      SCEMP#        XZEMP#
     C                   ENDIF                                                  SCDIV<>XZDIV
     C*
     C     SCDIV         IFEQ      *BLANKS
     C     EMPCHG        ANDEQ     *ON
     C                   MOVE      *ON           *IN27
     C                   MOVE      *OFF          EMPCHG
     C                   MOVE      SCDIV         XZDIV
     C                   MOVE      SCEMP#        XZEMP#
     C                   GOTO      STRT
     C                   ENDIF                                                  SCDIV=*BLANKS
     C*
     C                   ENDIF                                                  SCEMP#<>*ZEROS
     C***  End   Add  ***  10/27/03  **************************************
     **
     C***  Begin Del  ***  10/27/03  **************************************
      *********** * * * * * * * * * * * * * * * * * * * * * *
      ****VALIDATE PRIMARY SORT
      *********** * * * * * * * * * * * * * * * * * * * * * *
      **********
     C**********           MOVE *OFF      *IN73
      **********
     C********** SCSRT1    IFNE 'EA'
     C********** SCSRT1    ANDNE'EN'
     C********** SCSRT1    ANDNE'AA'
     C********** SCSRT1    ANDNE'AN'
      **********
     C**********           MOVE *ON       *IN73
     C**********           GOTO STRT
      **********
     C**********           ENDIF                           SCSRT1 IFNE
      **********
      *********** * * * * * * * * * * * * * * * * * * * * * *
      ****VALIDATE SECONDARY SORT
      *********** * * * * * * * * * * * * * * * * * * * * * *
      **********
      ****SORT2*MUST BE BLANK IF SORT1 IS 'EA' OR 'EN'
      **********
     C********** SCSRT1    IFEQ 'EA'
     C********** SCSRT1    OREQ 'EN'
      **********
     C********** SCSRT2    IFNE '  '
     C**********           MOVE *ON       *IN75
     C**********           GOTO STRT
     C**********           ENDIF
      **********
     C**********           ELSE
      **********
      ****SORT2*MUST BE 'EA', 'EN', OR BLANK IF SORT 1 IS 'AA' OR 'AN'
      **********
     C********** SCSRT2    IFNE '  '
     C********** SCSRT2    ANDNE'EA'
     C********** SCSRT2    ANDNE'EN'
      **********
     C**********           MOVE *ON       *IN76
     C**********           GOTO STRT
      **********
     C**********           ENDIF                           SCSRT2 IFNE
      **********
     C**********           ENDIF                           SCSRT1 IFEQ
     C***  End   Del  ***  10/27/03  **************************************
      *
      * * * * * * * * * * * * * * * * * * * * * * * * *
      *  VALIDATE THE SORT RANGES
      * * * * * * * * * * * * * * * * * * * * * * * * *
      *
      *  CHOOSE EITHER EMP NAME OR EMP #
      *
     C     SCTOEA        IFNE      *BLANK
      *
      *    CONVERT TO UPPER CASE
      *
     C     LO:UP         XLATE(P)  SCFREA        SCFREA
     C     LO:UP         XLATE(P)  SCTOEA        SCTOEA
      *
     C     SCTOEN        IFNE      *ZERO
     C                   MOVE      *ON           *IN78
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C***  Begin Add  ***  10/27/03  **************************************
     C*
     C*  Validate policy effective dates
     C*
     C     SCFREF        IFNE      *ZERO
     C*
     C                   Z-ADD     SCFREF        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFREF
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     SCFREF        IFNE      *ZERO
     C     SCTOEF        ORNE      *ZERO
     C*
     C                   Z-ADD     SCTOEF        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTOEF
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN29
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   Z-ADD     SCFREF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C                   Z-ADD     SCTOEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C     XXFRDT        IFGT      XXTODT
     C                   MOVE      *ON           *IN30
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C***  End   Add  ***  10/27/03  **************************************
      *
      *  CHOOSE EITHER AGT NAME OR AGT #
      *
     C     SCTOAA        IFNE      *BLANK
      *
      *    CONVERT TO UPPER CASE
      *
     C     LO:UP         XLATE(P)  SCFRAA        SCFRAA
     C     LO:UP         XLATE(P)  SCTOAA        SCTOAA
      *
     C     SCTOAN        IFNE      *ZERO
     C                   MOVE      *ON           *IN82
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDIF                                                  SCTOAA IFNE
      *
      * VALIDATE TO RANGE NOT LESS THAN FROM RANGE
      *
     C     SCFREA        IFGT      SCTOEA
     C                   MOVE      *ON           *IN77
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFREA IFGT
      *
     C     SCFREN        IFGT      SCTOEN
     C                   MOVE      *ON           *IN72
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFREN IFGT
      *
     C     SCFRAA        IFGT      SCTOAA
     C                   MOVE      *ON           *IN71
     C                   GOTO      STRT
     C                   ENDIF                                                   SCFRAA IFGT
      *
     C     SCFRAN        IFGT      SCTOAN
     C                   MOVE      *ON           *IN84
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFRAA IFGT
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C     SCEMP#        IFNE      *ZEROS
     C     SCSRT1        ANDNE     *BLANKS
     C                   MOVE      *ON           *IN74
     C                   GOTO      STRT
     C                   ENDIF                                                  SCEMP#<>*ZEROS
     C*
      * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *   VALIDATE PRIMARY SORT
      * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *
     C                   MOVE      *OFF          *IN73
      *
     C     SCSRT1        IFNE      'EA'
     C     SCSRT1        ANDNE     'EN'
     C     SCSRT1        ANDNE     'AA'
     C     SCSRT1        ANDNE     'AN'
     C     SCEMP#        ANDEQ     *ZEROS
      *
     C                   MOVE      *ON           *IN73
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
     C                   MOVE      *ON           *IN75
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
     C                   MOVE      *ON           *IN76
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCSRT2 IFNE
      *
     C                   ENDIF                                                  SCSRT1 IFEQ
     C*
     C                   MOVE      'N'           SCINDP
     C***  End   Add  ***  10/27/03  **************************************
     C***  Begin Del  ***  10/27/03  **************************************
      **********
      ***VALIDATE 'INCLUDE OR EXCLUDE A/R CODES'
      **********
     C********** SCARIE    IFNE 'I'
     C********** SCARIE    ANDNE'E'
     C********** SCARIE    ANDNE' '
      **********
     C**********           MOVE *ON       *IN90
     C**********           GOTO STRT
      **********
     C**********           ENDIF                           SCARIE <> 'I'
      **********
     C**********           MOVE *OFF      *IN94
     C********** 1         DO   30        X
      **********
     C********** CD,X      IFNE *BLANK
     C**********           MOVE *ON       *IN94
     C**********           ENDIF
      **********
     C**********           ENDDO                           1 DO 30  X
      **********
     C********** SCARIE    IFNE *BLANK
     C********** *IN94     ANDEQ*OFF
     C**********           MOVE *ON       *IN92
     C**********           GOTO STRT
     C**********           ENDIF                           SCARIE <> *BLANK
      **********
     C********** SCARIE    IFEQ *BLANK
     C********** *IN94     ANDEQ*ON
     C**********           MOVE *ON       *IN93
     C**********           GOTO STRT
     C**********           ENDIF                           SCARIE <> *BLANK
      **********
     ****VALIDATE THE A/R CODES
     ***********
     C**********           Z-ADD6         X3      20
     C**********           Z-ADD65        XI      20
     C**********           Z-ADD21        XN      20
     ***********
     C********** 1         DO   30        X
     ***********
     C**********           ADD  1         XN
     C********** X3        SUB  X         X4      20
     ***********
     C********** X4        IFEQ *ZERO
     C**********           ADD  5         X3
     C**********           ADD  1         XI
     C**********           MOVE 22        XN
     C**********           ENDIF
     ***********
     C********** CD,X      IFNE *BLANKS
     C**********           MOVELCD,X      XXKEY     P
     ***********
     C********** TBKY      CHAINPTT01                99
     ***********
     C********** *IN99     IFEQ *ON
     ***********
     C********** X         ADD  26        X2      20
     C**********           MOVE *ON       *IN,X2
     C**********           MOVE *ON       *IN,XI
     C**********           MOVE *ON       *IN,XN
     C**********           GOTO STRT
     ***********
     C**********           ENDIF                           *IN40 IFEQ
     ***********
     C********** TTSTAT    IFEQ 'I'
     C********** X         ADD  26        X2
     C**********           MOVE *ON       *IN,X2
     C**********           MOVE *ON       *IN,XI
     C**********           MOVE *ON       *IN,XN
     C**********           GOTO STRT
     C**********           ENDIF
     ***********
     C**********           ENDIF
     ***********
     C**********           ENDDO
     C***  End   Del  ***  10/27/03  **************************************
      **** BEGIN ADD **** 11/17/00 *********************************************
      *
      *  ALL EDITS HAVE PASSED, LOAD SCREEN 3
      *
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C                   GOTO      STRT
      *
     C     SCR03T        TAG
      *
     C***  Begin Chg  ***  10/27/03  **************************************
      **********
      ***VALIDATE 'INCLUDE OR EXCLUDE ACTIVITY STATUS CODES
      **********
     C********** SCIEFL    IFNE 'I'
     C********** SCIEFL    ANDNE'E'
     C********** SCIEFL    ANDNE' '
      **********
     C**********           MOVE *ON       *IN90
     C**********           GOTO STRT
      **********
     C**********           ENDIF                           SCIEFL IFNE 'I'
      **********
     C**********           MOVE *OFF      *IN94
      **********
     C********** 1         DO   5         X
      **********
     C********** AS,X      IFNE *BLANK
     C**********           MOVE *ON       *IN94
     C**********           ENDIF                           AS,X IFNE *BLANK
      **********
     C**********           ENDDO                           1 DO 5 X
      **********
     C********** SCIEFL    IFNE *BLANK
     C********** *IN94     ANDEQ*OFF
     C**********           MOVE *ON       *IN92
     C**********           GOTO STRT
     C**********           ENDIF                           SCIEFL IFNE *BLANK
      **********
     C********** SCIEFL    IFEQ *BLANK
     C********** *IN94     ANDEQ*ON
     C**********           MOVE *ON       *IN93
     C**********           GOTO STRT
     C**********           ENDIF                           SCIEFL IFEQ *BLANK
      **********
      ***VALIDATE THE ACTIVITY STATUS CODES
      **********
     C**********           Z-ADD6         X3      20
     C**********           Z-ADD65        XI      20
     C**********           Z-ADD21        XN      20
      **********
     C********** 1         DO   5         X
      **********
     C**********           ADD  1         XN
     C********** X3        SUB  X         X4      20
      **********
     C********** X4        IFEQ *ZERO
      **********
     C**********           ADD  5         X3
     C**********           ADD  1         XI
     C**********           MOVE 22        XN
      **********
     C**********           ENDIF                           X4 IFEQ *ZERO
      **********
     C********** AS,X      IFNE *BLANKS
      **********
     C**********           MOVELAS,X      XXKEY     P
     C********** TBKY      CHAINPAV01                99
      **********
     C********** *IN99     IFEQ *ON
     C********** AVSTAT    OREQ 'I'
      **********
     C********** X         ADD  26        X2      20
     C**********           MOVE *ON       *IN,X2
     C**********           MOVE *ON       *IN,XI
     C**********           MOVE *ON       *IN,XN
     C**********           GOTO STRT
      **********
     C**********           ENDIF                           *IN99 IFEQ *ON
      **********
     C**********           ENDIF                           AS,X IFNE *BLANKS
      **********
     C**********           ENDDO                           1 DO 5 X
     C*
     C*  Include deposit transaction flag
     C*
     C     SCINDP        IFNE      'Y'
     C     SCINDP        ANDNE     'N'
     C                   MOVE      *ON           *IN31
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   EXSR      CHKAR
     C*
     C                   EXSR      CHKASC
     C***  End   Chg  ***  10/27/03  **************************************
      **** END   ADD **** 11/17/00 *********************************************
      *** begin add ***  01/10/17  ***********************************
      /free
       xxSubmit = *on;

       If isUseScript;

          xxProcType = processType;

          If scEmp# <> 0 and scDiv <> *blanks;
             xxMass = ' ';
          Else;
             xxMass = 'Y';
          Endif;

          In *Lock *dtaara;

          Clear svLDA;

          xxDart = 'N';
          xxRMCD = ' ';

          Setll (UPCo#:scFnd) pfn01;
          Reade(n) (UPCo#) pfn01;

          Dow not %eof;
             If FNFnd > scTFnd and scTFnd <> 0;
                Leave;
             Endif;
             Chain(n) (UPCo#:FNFnd) pex01;
             If %found and EXLNRM <> ' ';
                xxDart = 'Y';
                xxRMCD = EXLNRM;
                Leave;
             Endif;
             Reade(n) (UPCo#) pfn01;
          Enddo;

          Out *dtaara;

          xxSubmit = OutputWindow(xxProcType:xxMass);

       Endif;
      /end-free
     C                   If        xxSubmit = *off
     C                   Goto      Strt
     C                   Endif
      *** end add *****  01/10/17  ***********************************
      *
      *  CHECK IF A/R CODE ARRAY IS FULL. IF NOT LOAD '@@' INTO LAST
      *  POSITION. THIS IS TO AVOID POSSIBLE IBM ERROR WHEN PASSING
      *  PARAMETERS > 32 CHARS.
      *
     C     CD(30)        IFEQ      *BLANKS
     C                   MOVE      '@@'          CD(30)
     C                   ENDIF
     **
     **
     **   CALL THE CLP TO SUBMIT THE LIST.
     **
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND1            3
     C***  Begin Chg  ***  10/27/03  **************************************
     C**********           MOVE SCFRID    QQFRID  8
     C**********           MOVE SCTOID    QQTOID  8
     C**********           MOVE SCBLDT    QQBLDT  8
     C                   MOVE      SCFRID        QQFRID
     C                   MOVE      SCTOID        QQTOID
     C                   MOVE      SCTOID        QQBLDT            8
     C***  End   Chg  ***  10/27/03  **************************************
     C                   MOVE      SCAMTD        QQAMTD            9
     C***** BEGIN ADD **** 03/03/00 ***********************************
     C                   MOVE      SCRPTF        QQRPTF            1
     C***** END   ADD **** 03/03/00 ***********************************
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C                   MOVE      SCCTST        QQCTST            1
     C***** END   ADD **** 03/20/00 ***********************************
     C                   MOVEA     CD            QQCODE           60
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   MOVE      'D'           QQMODE            1
     C                   MOVE      SCFIDY        QQFIDY            2
     C                   MOVE      SCEBDY        QQEBDY            2
     C                   MOVE      SCABDY        QQABDY            2
     C                   MOVE      SCRADY        QQRADY            2
     C                   MOVE      SCSRT1        QQSRT1            2
     C                   MOVE      SCSRT2        QQSRT2            2
     C***  Begin Chg  ***  10/27/03  **************************************
     C**********           MOVE SCFRAA    QQFRAA  2
     C**********           MOVE SCTOAA    QQTOAA  2
     C**********           MOVE SCFRAN    QQFRAN  5
     C**********           MOVE SCTOAN    QQTOAN  5
     C**********           MOVE SCFREA    QQFREA  2
     C**********           MOVE SCTOEA    QQTOEA  2
     C**********           MOVE SCFREN    QQFREN  9
     C**********           MOVE SCTOEN    QQTOEN  9
     C                   MOVE      SCFRAA        QQFRAA
     C                   MOVE      SCTOAA        QQTOAA
     C                   MOVE      SCFRAN        QQFRAN
     C                   MOVE      SCTOAN        QQTOAN
     C                   MOVE      SCFREA        QQFREA
     C                   MOVE      SCTOEA        QQTOEA
     C                   MOVE      SCFREN        QQFREN
     C                   MOVE      SCTOEN        QQTOEN
     C***  End   Chg  ***  10/27/03  **************************************
     C                   MOVE      SCACST        QQACST            1
     C                   MOVE      SCESSR        QQESSR            1
     C***  Begin Chg  ***  10/27/03  **************************************
     C**********           MOVE SCPAYD    QQPAYD  8
     C**********           MOVE SCCOM1    QQCOM1  2
     C**********           MOVE SCCOM2    QQCOM2  2
     C**********           MOVE SCCOM3    QQCOM3  2
     C**********           MOVE SCCOM4    QQCOM4  2
     C**********           MOVE SCCOM5    QQCOM5  2
     C                   MOVE      *BLANKS       QQPAYD            8
     C                   MOVE      SCCOM1        QQCOM1
     C                   MOVE      SCCOM2        QQCOM2
     C                   MOVE      SCCOM3        QQCOM3
     C                   MOVE      SCCOM4        QQCOM4
     C                   MOVE      SCCOM5        QQCOM5
     C***  End   Chg  ***  10/27/03  **************************************
     C                   MOVE      SCAUST        QQAUST            1
     C                   MOVE      SCDRMK        QQDRMK            5
     C                   MOVE      SCARIE        QQARIE            1
      **** BEGIN ADD **** 11/17/00 *********************************************
     C                   MOVE      SCIEFL        QQIEFL            1
     C                   MOVEA     AS            QQASTC           10
      **** END   ADD **** 11/17/00 *********************************************
     C***  Begin Add  ***  10/27/03  **************************************
     C                   MOVE      SCTFND        QQTFND
     C                   MOVE      SCEMP#        QQEMP#
     C                   MOVE      SCFREF        QQFREF
     C                   MOVE      SCTOEF        QQTOEF
     C*
     C     SCTFND        IFEQ      *ZERO
     C                   MOVE      001           QQFND1
     C                   MOVE      999           QQTFND
     C                   ENDIF
     C*
     C     SCEMP#        IFNE      *ZEROS
     C                   MOVEL(P)  'EN'          QQSRT1
     C                   ENDIF                                                  SCEMP#<>*ZEROS
     C***  End   Add  ***  10/27/03  **************************************
     C***** BEGIN ADD **** 01/10/07 ***********************************
     C                   MOVE      SCADDU        QQADDU
     C***** END ADD ****** 01/10/07 ***********************************
     **
     C                   CALL      'W7290C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND1
     C***  Begin Chg  ***  10/27/03  **************************************
     C**********           PARM           QQFRID
     C**********           PARM           QQTOID
     C                   PARM                    QQID
     C***  End   Chg  ***  10/27/03  **************************************
     C                   PARM                    QQBLDT
     C                   PARM                    QQAMTD
     C***** BEGIN ADD **** 03/03/00 ***********************************
     C                   PARM                    QQRPTF
     C***** END   ADD **** 03/03/00 ***********************************
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C                   PARM                    QQCTST
     C***** END   ADD **** 03/20/00 ***********************************
     C                   PARM                    QQOUTQ
     C                   PARM                    QQMODE
     C                   PARM                    QQFIDY
     C                   PARM                    QQEBDY
     C                   PARM                    QQABDY
     C                   PARM                    QQRADY
     C                   PARM                    QQSRT1
     C                   PARM                    QQSRT2
     C***  Begin Chg  ***  10/27/03  **************************************
     C**********           PARM           QQFREA
     C**********           PARM           QQTOEA
     C**********           PARM           QQFREN
     C**********           PARM           QQTOEN
     C**********           PARM           QQFRAA
     C**********           PARM           QQTOAA
     C**********           PARM           QQFRAN
     C**********           PARM           QQTOAN
     C                   PARM                    QQEA
     C                   PARM                    QQEN
     C                   PARM                    QQAA
     C                   PARM                    QQAN
     C***  End   Chg  ***  10/27/03  **************************************
     C                   PARM                    QQACST
     C                   PARM                    QQESSR
     C                   PARM                    QQPAYD
     C***  Begin Chg  ***  10/27/03  **************************************
     C**********           PARM           QQCOM1
     C**********           PARM           QQCOM2
     C**********           PARM           QQCOM3
     C**********           PARM           QQCOM4
     C**********           PARM           QQCOM5
     C                   PARM                    QQCOM
     C***  End   Chg  ***  10/27/03  **************************************
     C                   PARM                    QQDRMK
     C                   PARM                    QQAUST
     C                   PARM                    QQCODE
     C                   PARM                    QQARIE
      **** BEGIN ADD **** 11/17/00 *********************************************
     C                   PARM                    QQIEFL
     C                   PARM                    QQASTC
      **** END   ADD **** 11/17/00 *********************************************
     C***  Begin Add  ***  10/27/03  **************************************
     C                   PARM                    QQTFND            3
     C                   PARM      SCDART        QQDART            1
     C                   PARM                    QQEMP#            9
     C                   PARM      SCDIV         QQDIV             5
     C                   PARM                    QQEF
     C                   PARM      SCINDP        QQINDP            1
     C***  End   Add  ***  10/27/03  **************************************
     C***** BEGIN ADD **** 01/10/07 ***********************************
     C                   PARM                    QQADDU            3
     C***** END ADD ****** 01/10/07 ***********************************
     C*** BEG ADD ****** 08/21/08 ************************************
     C                   PARM      SCJRST        QQJRST            2
     C*** END ADD ****** 08/21/08 ************************************
     C*** BEG ADD ****** 10/25/16 ************************************
     C                   PARM      'N'           qqGrpSlct         1
     C*** END ADD ****** 10/25/16 ************************************
     **
     **  END THE PROGRAM
     **
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     *********************************************************************
     **  SRPCMT - CALL PROMPT PGM FOR COMMENT FIELDS
     *********************************************************************
     C     SRPCMT        BEGSR
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2191R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     **
     C     *INKL         IFEQ      *ON
     **
     C                   SELECT
     **
     C     HFLD          WHENEQ    'SCCOM1'
     C                   MOVE      *ON           *IN54
     **
     C     HFLD          WHENEQ    'SCCOM2'
     C                   MOVE      *ON           *IN55
     **
     C     HFLD          WHENEQ    'SCCOM3'
     C                   MOVE      *ON           *IN56
     **
     C     HFLD          WHENEQ    'SCCOM4'
     C                   MOVE      *ON           *IN57
     **
     C     HFLD          WHENEQ    'SCCOM5'
     C                   MOVE      *ON           *IN58
     **
     C                   ENDSL
     **
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  *INKL IFEQ
     **
     C     SCKEY         IFNE      *BLANK
     **
     **
     C                   SELECT
     **
     C     HFLD          WHENEQ    'SCCOM1'
     C                   MOVEL     SCKEY         SCCOM1
     C                   MOVE      *ON           *IN55
     **
     C     HFLD          WHENEQ    'SCCOM2'
     C                   MOVEL     SCKEY         SCCOM2
     C                   MOVE      *ON           *IN56
     **
     C     HFLD          WHENEQ    'SCCOM3'
     C                   MOVEL     SCKEY         SCCOM3
     C                   MOVE      *ON           *IN57
     **
     C     HFLD          WHENEQ    'SCCOM4'
     C                   MOVEL     SCKEY         SCCOM4
     C                   MOVE      *ON           *IN58
     **
     C     HFLD          WHENEQ    'SCCOM5'
     C                   MOVEL     SCKEY         SCCOM5
     C                   MOVE      *ON           *IN59
     **
     C                   ENDSL
     **
     C                   ELSE
     **
     C                   SELECT
     **
     C     HFLD          WHENEQ    'SCCOM1'
     C                   MOVE      *ON           *IN54
     **
     C     HFLD          WHENEQ    'SCCOM2'
     C                   MOVE      *ON           *IN55
     **
     C     HFLD          WHENEQ    'SCCOM3'
     C                   MOVE      *ON           *IN56
     **
     C     HFLD          WHENEQ    'SCCOM4'
     C                   MOVE      *ON           *IN57
     **
     C     HFLD          WHENEQ    'SCCOM5'
     C                   MOVE      *ON           *IN58
     **
     C                   ENDSL
     **
     **
     C                   ENDIF                                                  SCKEY IFNE
     **
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     **
     C                   ENDSR
     C*
     C***  Begin Add ***  10/27/03  ***************************************
     C*====================================================================
     C* Check A/R codes
     C*====================================================================
     C     CHKAR         BEGSR
      *
      *  VALIDATE 'INCLUDE OR EXCLUDE A/R CODES'
      *
     C     SCARIE        IFNE      'I'
     C     SCARIE        ANDNE     'E'
     C     SCARIE        ANDNE     ' '
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C                   ENDIF                                                  SCARIE <> 'I'
      *
     C     *BLANK        LOOKUP    CD                                 32
     C     SCARIE        IFNE      *BLANK
     C     *IN32         ANDEQ     *OFF
     C     SCARIE        OREQ      *BLANK
     C     *IN32         ANDEQ     *ON
     C                   MOVE      *ON           *IN33
     C                   GOTO      STRT
     C                   ENDIF
     C                   MOVE      *OFF          *IN32
      *
      *  VALIDATE THE A/R CODES
      *
     C                   DO        30            X
      *
     C     CD(X)         IFNE      *BLANKS
      *
     C                   MOVEL(P)  CD(X)         XXKEY
      *
     C     TBKY          CHAIN     PTT01                              60
      *
     C     *IN60         IFEQ      *ON
     C     TTSTAT        OREQ      'I'
      *
     C     X             SUB       1             XR                3 0          ROW
     C                   DIV       5             XR
     C                   ADD       1             XR
     C     XR            SUB       1             XC                3 0          COLUMN
     C                   MULT      5             XC
     C     X             SUB       XC            XC
      *
     C                   ADD       20            XC
     C                   ADD       70            XR
     C                   MOVE      *ON           *IN(XR)
     C                   MOVE      *ON           *IN(XC)
      *
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   ENDSR
      *
     C*====================================================================
     C* Check Activity Status Codes
     C*====================================================================
     C     CHKASC        BEGSR
      *
      *  VALIDATE 'INCLUDE OR EXCLUDE ACTIVITY STATUS CODES
      *
     C     SCIEFL        IFNE      'I'
     C     SCIEFL        ANDNE     'E'
     C     SCIEFL        ANDNE     ' '
     C                   MOVE      *ON           *IN35
     C                   GOTO      STRT
     C                   ENDIF                                                  SCIEFL IFNE 'I'
      *
     C     *BLANK        LOOKUP    AS                                 36
     C     SCIEFL        IFNE      *BLANK
     C     *IN36         ANDEQ     *OFF
     C     SCIEFL        OREQ      *BLANK
     C     *IN36         ANDEQ     *ON
     C                   MOVE      *ON           *IN37
     C                   GOTO      STRT
     C                   ENDIF
     C                   MOVE      *OFF          *IN36
      *
      *  Validate the Activity Status Codes
      *
     C     1             DO        5             X
      *
     C     AS(X)         IFNE      *BLANKS
      *
     C                   MOVEL(P)  AS(X)         XXKEY
     C     TBKY          CHAIN     PAV01                              99
      *
     C     *IN99         IFEQ      *ON
     C     AVSTAT        OREQ      'I'
      *
     C     X             SUB       1             XR                3 0          ROW
     C                   DIV       5             XR
     C                   ADD       1             XR
     C     XR            SUB       1             XC                3 0          COLUMN
     C                   MULT      5             XC
     C     X             SUB       XC            XC
      *
     C                   ADD       20            XC
     C                   ADD       90            XR
     C                   MOVE      *ON           *IN(XR)
     C                   MOVE      *ON           *IN(XC)
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *IN99 IFEQ *ON
      *
     C                   ENDIF                                                  AS,X IFNE *BLANKS
      *
     C                   ENDDO                                                  1 DO 5 X
      *
     C                   ENDSR
      *
     C*====================================================================
     C* Search Groups
     C*====================================================================
     C     GRPSCH        BEGSR
     C*
     C                   MOVE      UPCO#         ##CO#
     C*
     C                   CALL      'W0010R'
     C     *INKC         PARM      *BLANK        ##KC              1
     C     *INKL         PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##CO#             3
     C                   PARM      *BLANKS       ##FND             3
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     ##FND         OREQ      *BLANK
     C                   SELECT
     C     HFLD          WHENEQ    'SCFND'
     C                   MOVE      *ON           *IN31
     C     HFLD          WHENEQ    'SCTFND'
     C                   MOVE      *ON           *IN32
     C                   ENDSL
     C                   ELSE
     C                   SELECT
     C     HFLD          WHENEQ    'SCFND'
     C                   MOVE      *ON           *IN32
     C                   MOVE      ##FND         SCFND
     C     HFLD          WHENEQ    'SCTFND'
     C                   MOVE      *ON           *IN33
     C                   MOVE      ##FND         SCTFND
     C                   ENDSL
     C                   ENDIF
     C*
     C                   ENDSR
     C***  End   Add ***  10/27/03  ***************************************
**
SCCD01
SCCD02
SCCD03
SCCD04
SCCD05
SCCD06
SCCD07
SCCD08
SCCD09
SCCD10
SCCD11
SCCD12
SCCD13
SCCD14
SCCD15
SCCD16
SCCD17
SCCD18
SCCD19
SCCD20
SCCD21
SCCD22
SCCD23
SCCD24
SCCD25
SCCD26
SCCD27
SCCD28
SCCD29
SCCD30
**
SCAST1
SCAST2
SCAST3
SCAST4
SCAST5
