     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7290R1 - PROMPT FOR AGING REPORT
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:                                                 *
     H*    This program prompts for policy effect dt range, balance   *
     H*    date, sort sequence, account type and status, audit status *
     H*    and A/R codes. It then passes this info on so as to        *
     H*    produce a corresponding aging report.                      *
     H*                                                               *
     H*    Aging report was established as a separate report, taken   *
     H*    from Employer Late Notice report.                          *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    Menu                                                       *
     H*  CALLS TO:                                                    *
     H*    W7290C1                                                    *
     H*                                                               *
     H*  CREATED: 02/04/98  PROGRAMMER- C:87  PROJECT: CM033          *
     H*                                                               *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H* 08/16/06  C:33   Project: WC1443                              *
     H*                  Aging Report Across Groups                   *
     H*                                                               *
     H* 08/17/05  C:062  Project: WA099 46392                         *
     H*                  If F12 hit from scr03 then clear @@ from     *
     H*                  the 30th code.                               *
     H*                                                               *
     H* 12/17/04  C:136  Project: WC1248                              *
     H*                  Age A/R records by posting date only.        *
     H*                  Add prompt in this program.                  *
     H*                                                               *
     H* 12/07/04  C:136  Project: 129WHU                              *
     H*                  Add FCC mods to base.                        *
     H*                  Add selection for billing type.              *
     H*                                                               *
     H* 05/15/02  C:65   PROJECT: WC1085                              *
     H*                  ADD SELECTION FOR INCLUDE/EXCLUDE A/R        *
     H*                  CATEGORIES TO THE PROMPT.                    *
     H*                                                               *
     H*  05/11/01  C:136 PROJECT: WC577                               *
     H*                  Display location of output file.             *
     H*                                                               *
     H* 11/16/00  C:35   PROJECT: WC577                               *
     H*                  ADD SELECTION FOR INCLUDE/EXCLUDE ACTIVITY   *
     H*                  STATUS CODES.                                *
     H*                                                               *
     H*  01/18/00  C:90  PROJECT: WC831                               *
     H*                  ADDED NEW FIELDS FOR MIN CREDIT AND DEBIT    *
     H*                  BALANCE AMOUNTS.                             *
     H*                                                               *
     H*  11/02/98  C:87  PROJECT: NONE                                *
     H*                  Found error with balance date format when    *
     H*                  F13=Chg usr parms is executed. Resolved.     *
     H*                                                               *
     H*****************************************************************
     F*
     FW7290F1   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     FWMEMP     IF   E             DISK
     FWMAAP     IF   E             DISK
      **** BEGIN ADD **** 11/16/00 *********************************************
     FWTAVL     IF   E           K DISK
     FWTAVP     IF   E             DISK
     F                                     RENAME(PAV01:PAV02)
      **** END   ADD **** 11/16/00 *********************************************
      **** BEGIN ADD **** 05/15/02 *********************************************
     FWTCTL     IF   E           K DISK
     FWTCTP     IF   E             DISK
     F                                     RENAME(PCT01:PCT02)
      **** END   ADD **** 05/15/02 *********************************************
      *** BEG ADD ***** 12/07/04 ***********************************************
     FSWCPF     IF   E             DISK
      *** BEG ADD ***** 12/07/04 ***********************************************

     D*
     D CDE             S             10    DIM(30) CTDATA PERRCD(1) ASCEND
      **** BEGIN ADD **** 11/16/00 *********************************************
     D ASE             S             10    DIM(5) CTDATA PERRCD(1) ASCEND
      **** END   ADD **** 11/16/00 *********************************************
     D*** BEGIN ADD ***  05/15/02  ************************************
     D CTE             S             10    DIM(10) CTDATA PERRCD(1) ASCEND
     D*** END ADD *****  05/15/02  ************************************
     D***  Begin Add  ***  08/16/06  **************************************
     D OPTNM           C                   CONST('Aging Report')
     D***  End   Add  ***  08/16/06  **************************************
     D*
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D***  BEGIN CHG  ***  11/02/98  **********************************
     D************DS*************************
     D*************************************** 1  120DSDTTM
     D*************************************** 1   60DSTIME
     D*************************************** 7  120DSDATE
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D***  END   CHG  ***  11/02/98  **********************************
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
      **** BEGIN ADD **** 11/16/00 *********************************************
     D                 DS
     D  AS                     1     10
     D                                     DIM(5) ASCEND                        ACTIVITY STATUS
     D  SCAST1                 1      2
     D  SCAST2                 3      4
     D  SCAST3                 5      6
     D  SCAST4                 7      8
     D  SCAST5                 9     10
      **** END   ADD **** 11/16/00 *********************************************
      **** BEGIN ADD **** 05/15/02 *********************************************
     D                 DS
     D  CT                     1     10
     D                                     DIM(10) ASCEND                       A/R CATEGORIES
     D  SCCT01                 1      1
     D  SCCT02                 2      2
     D  SCCT03                 3      3
     D  SCCT04                 4      4
     D  SCCT05                 5      5
     D  SCCT06                 6      6
     D  SCCT07                 7      7
     D  SCCT08                 8      8
     D  SCCT09                 9      9
     D  SCCT10                10     10
      **** END   ADD **** 05/15/02 *********************************************
     D*
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     **
     **   KEY LIST FOR THE COMPANY FILE
     **

     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     **
     **  KEY LIST FOR USER PARAMETER FILE
     **
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     **
     **  KEY LIST FOR TABLE FILE
     **
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     **
     **  KEY LIST FOR FUND MASTER FILE
     **
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
      **** BEGIN ADD **** 05/15/02 ************************************
     C     CTKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXCTYP            1
      **** END   ADD **** 05/15/02 ************************************
     **
     *******************************************************************
     **  ENTRY PARAMETERS
     *******************************************************************
     C     *ENTRY        PLIST
     C***  Begin Chg  ***  08/16/06  **************************************
     C**********           PARM           QQPRDS 40
     C**********           PARM           QQCALL  1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C***  End   Chg  ***  08/16/06  **************************************
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
      **** BEGIN ADD **** 11/16/00 *********************************************
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
      **** END   ADD **** 11/16/00 *********************************************
     **
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPCO#         @@CO#
     C     *LIKE         DEFINE    EMFND         @@FND
     C     *LIKE         DEFINE    EMEMP#        @@EMP#
     C     *LIKE         DEFINE    EMDIV         @@DIV
      *
     C***  Begin Add  ***  08/16/06  **************************************
     C                   MOVEL(P)  OPTNM         ##DATA
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
      *
     C                   MOVEL     ##DATA        SCPRDS
     C***  End   Add  ***  08/16/06  **************************************
      *
     C                   Z-ADD     0             QQVIEW            1 0
     **
     **  GET THE USER PARAMETER FILE
     **
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C                   MOVE      UPCO#         XXCO#
     **
     **  GET THE COMPANY RECORD
     **
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVE      UPFND         SCFND
     **
     **  GET THE REQUESTED RECORD, LOAD INFORMATION
     **
     C                   MOVE      SCFND         XXFND
     C                   MOVE      UPCO#         XXCO#
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     **
     C                   MOVE      *BLANK        CD
     **
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C***  Begin Del  ***  08/16/06  **************************************
     C**********           MOVE QQPRDS    SCPRDS
     C***  End   Del  ***  08/16/06  **************************************
     C***  BEGIN ADD  ***  11/02/98  **********************************
     C*******************  MOVE DSDATE    SCBLDT
     C                   MOVE(P)   DSDATE        SCBLDT
     C***  END   ADD  ***  11/02/98  **********************************
     **
     **   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     **
      *** BEG ADD *****12/07/04 ****************************************
     C                   READ      SWCPF                                  60
     C     WCFL23        IFEQ      'Y'
     C     WCFL24        OREQ      'Y'
     C                   MOVE      *ON           SYSFLG            1
     C                   ENDIF
      *** END ADD *****12/07/04 ****************************************
     **
     C                   MOVE      *ON           *IN62
     C                   MOVEL(P)  'SCR01'       Q2RECF
     **
     **   DISPLAY ALL FORMATS
     **
     C     STRT          TAG
     **
     ** CONVERT SYSTEM DATE(6 DIGITS) TO SCREEN DATE(8 DIGITS)
     **
     C                   Z-ADD     SCBLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     **
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         SCBLDT
     C                   ENDIF
     **
     **  CONVERT POLICY EFF DT RANGE : FROM DATE
     **
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     **
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         SCPFDT
     C                   ENDIF
     **
     **  CONVERT POLICY EFF DT RANGE : TO DATE
     **
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     **
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         SCPTDT
     C                   ENDIF
     **
     C*** BEG ADD ******** 05/11/01 ********************************************
     C**
     C**  DISPLAY LOCATION OF OUTPUT FILE
     C**
     C                   CALL      'SA030R'
     C                   PARM                    QQSUFX            3
     C     'PDSRPT'      CAT(P)    QQSUFX        XXRPTL           10
     C                   CAT(P)    '.':0         XXRPTL
     C                   MOVE      XXRPTL        SCRPTL
     C**
     C*** END ADD ******** 05/11/01 ********************************************
     C**
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
     **
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   ENDIF
      *
     C*** BEG ADD ***** 12/07/04 ***********************************************
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   ENDIF
     C*** END ADD ***** 12/07/04 ***********************************************
     *****************************************************************
     **   PROCESS ALL COMMAND KEYS
     *****************************************************************
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
     C***  Begin Add  ***  08/16/06  **************************************
     C     *INKL         IFEQ      *ON
     C     Q2RECF        ANDEQ     'SCR01'
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF
     C***  End   Add  ***  08/16/06  **************************************
      *
     C     *INKL         IFEQ      *ON
     C     Q2RECF        ANDEQ     'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
      *** BEG ADD ***** 12/07/04 ***************************
     C     *INKL         IFEQ      *ON
     C     Q2RECF        ANDEQ     'SCR03'
     C*** BEGIN ADD *** 08/17/05 **************************************
     C     CD(30)        IFEQ      '@@'
     C                   MOVE      '  '          CD(30)
     C                   ENDIF
     C*** END   ADD *** 08/17/05 **************************************
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *** END ADD ***** 12/07/04 ***************************
     **
     **  FUNCTION 10= TABLE FILE SEARCH
     **
     C     *INKJ         IFEQ      *ON
     **
     C     Q2RECF        IFEQ      'SCR01'
      *
      * * * * * * * * * * * * * * * * * * * * * * * * *
      *  TABLE SEARCH - FROM/TO EMPLOYER NAME/NUMBER
      * * * * * * * * * * * * * * * * * * * * * * * * *
      *
     C     HFLD          IFEQ      'SCFREA'
     C     HFLD          OREQ      'SCTOEA'
     C     HFLD          OREQ      'SCFREN'
     C     HFLD          OREQ      'SCTOEN'
      *
     C     HFLD          IFEQ      'SCFREA'
     C     HFLD          OREQ      'SCTOEA'
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
     C                   MOVE      *ON           *IN81
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
     C                   ENDIF                                                  Q2RECF IFEQ SCR01
      *
     ******************************************************************
     **  PROCESS SCR02 SEARCH FIELDS
     ******************************************************************
     C     Q2RECF        IFEQ      'SCR02'
     **
     **  SEARCH A/R CODES
     **
      **** BEGIN DEL **** 11/16/00 *********************************************
     C********** 1         DO   30        X       20
     C**********           MOVELCD,X      XX2     2
      **** END   DEL **** 11/16/00 *********************************************
     C*** BEGIN ADD ***  05/15/02  ************************************
     **
     **  IS THE CURSOR ON A FIELD IN TABLE 'CTE'
     **
     C                   Z-ADD     1             X                 2 0
     C     HFLD          LOOKUP    CTE(X)                                 60
     **
     C     *IN60         IFEQ      *ON
     **
     C                   CALL      'W2162R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    ##RRN             9 0
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     **
     C     *INKL         IFEQ      *ON
     **
     C     X             ADD       70            X1                2 0
     C                   MOVE      *ON           *IN(X1)
     C                   MOVE      *ON           *IN81
     C                   GOTO      STRT
     **
     **
     **  WAS A VALUE SELECTED OR ENTER PRESSED
     **
     C                   ELSE
     **
     C     ##RRN         IFNE      0
     C     ##RRN         CHAIN     PCT02                              60
     C                   ENDIF                                                  ##RRN IFNE 0
     C*
     C     *IN60         IFEQ      *OFF
     C     ##RRN         ANDNE     0
      *
     C                   MOVE      CTCTYP        CT(X)
     C     X             ADD       71            X1                2 0
     C                   MOVE      *ON           *IN(X1)
     C                   MOVE      *ON           *IN81
     C     X             IFEQ      10
     C                   MOVE      *OFF          *IN(X1)
     C                   MOVE      *OFF          *IN81
     C                   MOVE      *ON           *IN91
     C                   ENDIF
      *
     C                   ELSE
      *
     C     X             ADD       70            X1                2 0
     C                   MOVE      *ON           *IN(X1)
     C                   MOVE      *ON           *IN81
      *
     C                   ENDIF                                                  *IN60 IFEQ  OFF
     **
     C                   MOVEL(P)  'SCR02'       Q2RECF
     **
     C                   GOTO      STRT
     C                   ENDIF
     **
     C                   ENDIF
     C*** END ADD *****  05/15/02  ************************************
     **
     **  IS THE CURSOR ON A FIELD IN TABLE 'CDE'
     **
      **** BEGIN ADD **** 11/16/00 *********************************************
     C                   Z-ADD     1             X                 2 0
      **** END   ADD **** 11/16/00 *********************************************
     C     HFLD          LOOKUP    CDE(X)                                 60
     **
     C     *IN60         IFEQ      *ON
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
     C     X             ADD       26            X1                2 0
     C                   MOVE      *ON           *IN(X1)
     C                   GOTO      STRT
     **
     **
     **  WAS A VALUE SELECTED OR ENTER PRESSED
     **
     C                   ELSE
     **
     C     QQKEY         IFEQ      *BLANKS
     C     X             ADD       26            X1                2 0
     **
     C                   ELSE
     C     X             ADD       27            X1                2 0
     C                   MOVEL     QQKEY         CD(X)
     **
     C                   ENDIF
     **
      **** BEGIN DEL **** 11/16/00 *********************************************
     C********** X1        IFNE 57
      **** END   DEL **** 11/16/00 *********************************************
     C                   MOVE      *ON           *IN(X1)
     **
     C                   MOVEL(P)  'SCR02'       Q2RECF
      **** BEGIN DEL **** 11/16/00 *********************************************
     C******************** ENDIF
      **** END   DEL **** 11/16/00 *********************************************
     **
     C                   GOTO      STRT
     C                   ENDIF
     **
     C                   ENDIF
      **** BEGIN DEL **** 11/16/00 *********************************************
     C******************** ENDDO
      **** END   DEL **** 11/16/00 *********************************************
      **** BEGIN ADD **** 11/16/00 *********************************************
      *
      * SEARCH ACTIVITY STATUS CODES
      *
      *
      *  IS THE CURSOR ON A FIELD IN TABLE 'ASE'
      *
     C                   Z-ADD     1             X                 2 0
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
     C     X             ADD       16            X1                2 0
     C                   MOVE      *ON           *IN(X1)
     C                   GOTO      STRT
      *
      *
      *  WAS A VALUE SELECTED OR ENTER PRESSED
      *
     C                   ELSE
      *
     C     X             IFLT      5
     C     X             ADD       17            X1                2 0
     C                   ENDIF                                                  X IFLT 5
     **
     C                   MOVE      *ON           *IN(X1)
     C     ##RRN         CHAIN     WTAVP                              60
     C                   MOVEL(P)  AVKEY         AS(X)
     **
     C                   MOVEL(P)  'SCR02'       Q2RECF
     **
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL IFEQ *ON
     **
     C                   ENDIF                                                  *IN60 IFEQ *ON
      **** END   ADD **** 11/16/00 *********************************************
     **
     C                   GOTO      STRT
     C                   ENDIF                                                  Q2RECF='SCR02'
     **
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
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C                   MOVE      UPCO#         XXCO#
     C                   GOTO      STRT01
     C                   ENDIF                                                  Q2RECF = SCR01
     **
     C                   ENDIF                                                  *INKM = *ON
     **
     C                   ENDIF                                                  *IN11 = *ON
      *** END PROCESS ALL COMMAND KEYS
     **
     **   PROCESS THE APPROPRIATE SCREEN
     **
     C     Q2RECF        IFEQ      'SCR01'
     C                   GOTO      SCR01T
     C                   ENDIF
     C     Q2RECF        IFEQ      'SCR02'
     C                   GOTO      SCR02T
     C                   ENDIF
      *
      *** BEG ADD ***** 12/07/04 ***************************
     C     Q2RECF        IFEQ      'SCR03'
     C                   GOTO      SCR03T
     C                   ENDIF
      *** END ADD ***** 12/07/04 ***************************
     **
     **   SCREEN 1 PROCESSING
     **
     C     SCR01T        TAG
     **
     **  VALIDATE THE 'BALANCE AS OF' DATE
     **
     C                   Z-ADD     SCBLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCBLDT
     **
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF
     **
     **  VALIDATE POLICY EFF DT RANGE : FROM DATE
     **
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPFDT
     **
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF
     **
     **  VALIDATE POLICY EFF DT RANGE : TO DATE
     **
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPTDT
     **
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF
     **
     **  VALIDATE POLICY EFF RANGE FROM DT <= TO DT
     **
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT            6 0
     **
     C     XXFRDT        IFGT      XXTODT
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   ENDIF
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
     **
     **  VALIDATE CREDIT/DEBIT/OR BOTH FLAG
     **
     C     SCCRDB        IFNE      'C'
     C     SCCRDB        ANDNE     'D'
     C     SCCRDB        ANDNE     'B'
     C                   MOVE      *ON           *IN31
     C                   GOTO      STRT
     C                   ENDIF
     **
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
     C     SCSRT2        IFNE      *BLANK
      *
      *   SORT2 MUST BE BLANK IF SORT1 IS 'EA' OR 'EN'
      *
     C     SCSRT1        IFEQ      'EA'
     C     SCSRT1        OREQ      'EN'
     C                   MOVE      *ON           *IN75
     C                   GOTO      STRT
     C                   ELSE
      *
      *   SORT2 MUST BE 'EA', 'EN', OR BLANK IF SORT 1 IS 'AA' OR 'AN'
      *
     C     SCSRT2        IFNE      'EA'
     C     SCSRT2        ANDNE     'EN'
     C                   MOVE      *ON           *IN76
     C                   GOTO      STRT
     C                   ENDIF                                                  SCSRT2 IFNE 'EA'
      *
     C                   ENDIF                                                  SCSRT1 IFEQ 'EA'
      *
     C                   ENDIF                                                  SCSRT2 = *BLANK
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
      *
      *** BEG ADD ***** 12/17/04 ***************************
     C     SCSLAR        IFNE      *BLANK
     C     SCSLAR        ANDNE     'Y'
     C                   MOVE      *ON           *IN88
     C                   ENDIF
      *** END ADD ***** 12/17/04 ***************************
     **
     **  ALL EDITS HAVE PASSED, LOAD SCREEN 2
     **
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     **
     C     SCR02T        TAG
      **** BEGIN ADD 05/15/02 **************************************************
      *
      *  CANNOT SET BOTH CATEGORY AND A/R CODE FLAGS
      *
     C     SCCTIE        IFNE      *BLANK
     C     SCARIE        ANDNE     *BLANK
      *
     C                   MOVE      *ON           *IN84
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCCTFL IFNE '
      *
      *  VALIDATE 'INCLUDE OR EXCLUDE ACTIVITY STATUS CODES'
      *
     C     SCCTIE        IFNE      'I'
     C     SCCTIE        ANDNE     'E'
     C     SCCTIE        ANDNE     ' '
      *
     C                   MOVE      *ON           *IN85
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCIEFL <> 'I'
      *
      *  CHECK IF A/R CATEGORIES ARE INPUT
      *
     C                   MOVE      *OFF          *IN60
     C     1             DO        10            X
      *
     C     CT(X)         IFNE      *BLANK
     C                   MOVE      *ON           *IN60
     C                   ENDIF
      *
     C                   ENDDO                                                  1 DO 10  X
      *
     C     SCCTIE        IFNE      *BLANK
     C     *IN60         ANDEQ     *OFF
     C                   MOVE      *ON           *IN87
     C                   GOTO      STRT
     C                   ENDIF                                                  SCCTIE <> *BLANK
      *
     C     SCCTIE        IFEQ      *BLANK
     C     *IN60         ANDEQ     *ON
     C                   MOVE      *ON           *IN86
     C                   GOTO      STRT
     C                   ENDIF                                                  SCCTIE <> *BLANK
      *
      * VALIDATE THE CATEGORIES
      *
     C                   Z-ADD     70            XI                2 0
      *
     C     1             DO        10            X
      *
     C     CT(X)         IFNE      *BLANKS
      *
     C                   MOVE      CT(X)         XXCTYP
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      *ON
      *
     C                   ADD       X             XI
     C                   MOVE      *ON           *IN(XI)
     C                   MOVE      *ON           *IN82
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *IN60 IFEQ  ON
     **
     C                   ENDIF                                                  CT,X IFNE *BLANKS
     **
     C                   ENDDO                                                  1 DO 10 X
      **** END ADD * 05/15/02 **************************************************
     **
      *
      *  VALIDATE 'INCLUDE OR EXCLUDE A/R CODES'
      *
     C     SCARIE        IFNE      'I'
     C     SCARIE        ANDNE     'E'
     C     SCARIE        ANDNE     ' '
      *
     C                   MOVE      *ON           *IN90
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCARIE <> 'I'
      *
      *  CHECK IF A/R CODE WAS INPUT
      *
     C                   MOVE      *OFF          *IN94
     C     1             DO        30            X
      *
     C     CD(X)         IFNE      *BLANK
     C                   MOVE      *ON           *IN94
     C                   ENDIF
      *
     C                   ENDDO                                                  1 DO 30  X
      *
     C     SCARIE        IFNE      *BLANK
     C     *IN94         ANDEQ     *OFF
     C                   MOVE      *ON           *IN92
     C                   GOTO      STRT
     C                   ENDIF                                                  SCARIE <> *BLANK
      *
     C     SCARIE        IFEQ      *BLANK
     C     *IN94         ANDEQ     *ON
     C                   MOVE      *ON           *IN93
     C                   GOTO      STRT
     C                   ENDIF                                                  SCARIE <> *BLANK
      **
     **  VALIDATE THE A/R CODES
     **
     C                   Z-ADD     6             X3                2 0
     C                   Z-ADD     65            XI                2 0
     C                   Z-ADD     21            XN                2 0
     **
     C     1             DO        30            X
     **
     C                   ADD       1             XN
     C     X3            SUB       X             X4                2 0
     **
     C     X4            IFEQ      *ZERO
     C                   ADD       5             X3
     C                   ADD       1             XI
     C                   MOVE      22            XN
     C                   ENDIF
     **
     C     CD(X)         IFNE      *BLANKS
     C                   MOVEL(P)  CD(X)         XXKEY
     **
     C     TBKEY         CHAIN     PTT01                              99
     **
     C     *IN99         IFEQ      *ON
     **
     C     X             ADD       26            X2                2 0
     C                   MOVE      *ON           *IN(X2)
     C                   MOVE      *ON           *IN(XI)
     C                   MOVE      *ON           *IN(XN)
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  *IN40 IFEQ
     **
     C     TTSTAT        IFEQ      'I'
     C     X             ADD       26            X2
     C                   MOVE      *ON           *IN(X2)
     C                   MOVE      *ON           *IN(XI)
     C                   MOVE      *ON           *IN(XN)
     C                   GOTO      STRT
     C                   ENDIF
     **
     C                   ENDIF
     **
     C                   ENDDO
      *
      *  CHECK IF A/R CODE ARRAY IS FULL. IF NOT LOAD '@@' INTO LAST
      *  POSITION. THIS IS TO AVOID POSSIBLE IBM ERROR WHEN PASSING
      *  PARAMETERS > 32 CHARS.
      *
     C     CD(30)        IFEQ      *BLANKS
     C                   MOVE      '@@'          CD(30)
     C                   ENDIF
      **** BEGIN ADD 11/16/00 **************************************************
      *
      *  VALIDATE 'INCLUDE OR EXCLUDE ACTIVITY STATUS CODES'
      *
     C     SCIEFL        IFNE      'I'
     C     SCIEFL        ANDNE     'E'
     C     SCIEFL        ANDNE     ' '
      *
     C                   MOVE      *ON           *IN94
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCIEFL <> 'I'
      *
      *  CHECK IF ACTIVITY STATUS CODES WERE ENTERED
      *
      *
     C                   MOVE      *OFF          *IN58
     C     1             DO        5             X
      *
     C     AS(X)         IFNE      *BLANK
     C                   MOVE      *ON           *IN58
     C                   ENDIF                                                  AS,X IFNE *BLANK
      *
     C                   ENDDO                                                  1 DO 5 X
      *
     C     SCIEFL        IFNE      *BLANK
     C     *IN58         ANDEQ     *OFF
     C                   MOVE      *ON           *IN95
     C                   GOTO      STRT
     C                   ENDIF                                                  SCIEFL IFNE *BLANK
      *
     C     SCIEFL        IFEQ      *BLANK
     C     *IN58         ANDEQ     *ON
     C                   MOVE      *ON           *IN96
     C                   GOTO      STRT
     C                   ENDIF                                                  SCIEFL IFEQ *BLANK
      *
      * VALIDATE THE ACTIVITY STATUS CODES
      *
     C                   Z-ADD     11            XI                2 0
      *
     C     1             DO        5             X
      *
     C     AS(X)         IFNE      *BLANKS
      *
     C                   MOVEL(P)  AS(X)         XXKEY
      *
     C     TBKEY         CHAIN     PAV01                              99
      *
     C     *IN99         IFEQ      *ON
     C     AVSTAT        OREQ      'I'
      *
     C                   ADD       X             XI
     C                   MOVE      *ON           *IN(XI)
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *IN99 IFEQ *ON
     **
     C                   ENDIF                                                  AS,X IFNE *BLANKS
     **
     C                   ENDDO                                                  1 DO 5 X
      **** END   ADD **** 11/16/00 *********************************************
      *** BEG ADD ***** 12/07/04 ***************************
     **
     **  ALL EDITS HAVE PASSED, LOAD SCREEN 3
     **
     C     SYSFLG        IFEQ      *ON
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C                   GOTO      STRT
     **
     C     SCR03T        TAG
      *
     C     SCBTYP        IFNE      *BLANK
     C     SCBTYP        ANDNE     'A'
     C     SCBTYP        ANDNE     'D'
     C     SCBTYP        ANDNE     'P'
     C                   MOVE      *ON           *IN71
     C                   GOTO      STRT
     C                   ENDIF                                                  SCIEFL IFEQ *BLANK
      *
      *** BEG ADD ***** 12/17/04 ***************************
     C     SCSLAR        IFNE      *BLANK
     C     SCSLAR        ANDNE     'Y'
     C                   MOVE      *ON           *IN72
     C                   ENDIF
      *** END ADD ***** 12/17/04 ***************************
      *
     C                   ENDIF
      *** END ADD ***** 12/07/04 ***************************
     **
     **   CALL THE CLP TO SUBMIT THE LIST.
     **
     C                   MOVE      UPCO#         QQCO#             3
     C***  Begin Chg  ***  08/16/06  **************************************
     C**********           MOVE SCFND     QQFND1  3
     C                   MOVE      *ZEROS        QQFND1            3
     C***  Begin Chg  ***  08/16/06  **************************************
     C                   MOVE      SCPFDT        QQPFDT            8
     C                   MOVE      SCPTDT        QQPTDT            8
     C                   MOVE      SCBLDT        QQBLDT            8
     C                   MOVEA     CD            QQCODE           60
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   MOVE      'D'           QQMODE            1
     C                   MOVE      SCSRT1        QQSRT1            2
     C                   MOVE      SCSRT2        QQSRT2            2
     C                   MOVE      SCFRAA        QQFRAA            2
     C                   MOVE      SCTOAA        QQTOAA            2
     C                   MOVE      SCFRAN        QQFRAN            5
     C                   MOVE      SCTOAN        QQTOAN            5
     C                   MOVE      SCFREA        QQFREA            2
     C                   MOVE      SCTOEA        QQTOEA            2
     C                   MOVE      SCFREN        QQFREN            9
     C                   MOVE      SCTOEN        QQTOEN            9
     C                   MOVE      SCACST        QQACST            1
     C                   MOVE      SCESSR        QQESSR            1
     C                   MOVE      SCCRDB        QQCRDB            1
     C                   MOVE      SCAUST        QQAUST            1
     C                   MOVE      SCARIE        QQARIE            1
      **** BEGIN ADD **** 01/18/00 ************************************
     C                   MOVE      SCMINC        QQMINC            9
     C                   MOVE      SCMIND        QQMIND            9
      **** END   ADD **** 01/18/00 ************************************
      **** BEGIN ADD **** 11/16/00 *********************************************
     C                   MOVE      SCIEFL        QQIEFL            1
     C                   MOVEA     AS            QQASTC           10
      **** END   ADD **** 11/16/00 *********************************************
      **** BEGIN ADD **** 05/15/02 *********************************************
     C                   MOVE      SCCTIE        QQCTIE            1
     C                   MOVEA     CT            QQCTYP           10
      **** END   ADD **** 05/15/02 *********************************************
     **
     C                   CALL      'W7290C1'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND1
     C                   PARM                    QQPFDT
     C                   PARM                    QQPTDT
     C                   PARM                    QQBLDT
     C                   PARM                    QQOUTQ
     C                   PARM                    QQMODE
     C                   PARM                    QQSRT1
     C                   PARM                    QQSRT2
     C                   PARM                    QQFREA
     C                   PARM                    QQTOEA
     C                   PARM                    QQFREN
     C                   PARM                    QQTOEN
     C                   PARM                    QQFRAA
     C                   PARM                    QQTOAA
     C                   PARM                    QQFRAN
     C                   PARM                    QQTOAN
     C                   PARM                    QQACST
     C                   PARM                    QQESSR
     C                   PARM                    QQCRDB
     C                   PARM                    QQAUST
     C                   PARM                    QQCODE
     C                   PARM                    QQARIE
      **** BEGIN ADD **** 01/18/00 ************************************
     C                   PARM                    QQMINC
     C                   PARM                    QQMIND
      **** END   ADD **** 01/18/00 ************************************
      **** BEGIN ADD **** 11/16/00 *********************************************
     C                   PARM                    QQIEFL
     C                   PARM                    QQASTC
      **** END   ADD **** 11/16/00 *********************************************
      **** BEGIN ADD **** 05/15/02 *********************************************
     C                   PARM                    QQCTIE
     C                   PARM                    QQCTYP
      **** END   ADD **** 05/15/02 *********************************************
      *** BEG ADD ***** 12/07/04 ***************************
     C                   PARM      SCBTYP        QQBTYP            1
      *** END ADD ***** 12/07/04 ***************************
      *** BEG ADD ***** 12/17/04 ***************************
     C                   PARM      SCSLAR        QQSLAR            1
      *** END ADD ***** 12/17/04 ***************************
      *
     C***  Begin Add  ***  08/16/06  **************************************
     C                   MOVE      *ON           *INKC
     C***  End   Add  ***  08/16/06  **************************************
     **
     **  END THE PROGRAM
     **
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
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
**
SCCT01
SCCT02
SCCT03
SCCT04
SCCT05
SCCT06
SCCT07
SCCT08
SCCT09
SCCT10
