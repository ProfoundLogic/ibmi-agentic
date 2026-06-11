     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N7736R - CASH RECEIPT/ADJ JOURNAL - PROMPT SCRN PGM
      *****************************************************************
      ** REVISIONS:                                                  **
     H*                                                              **
     H*  12/18/24 C:342 Project : MC447 Phase 2                      **
     H*                 1.F10 Search disabled on the Group Field     **
     H*                 DSPF Changes:                                **
     H*                 1.Changed Existing Group field into Typeahead**
     H*                                                              **
     H*  04/28/98 C:90  Proj:MC181                                   **
     H*             SCR01 - CHANGED TO MEET VERSION 4 STANDARDS      **
     H*             COMBINED ALL SEARCHES INTO F10 SEARCH            **
     H*
     H*  12/04/97 C:86  Project: MC145
     H*             CHANGED CODE TO FIND OUT IF THE TO DATE WAS LESS
     H*             THAN THE FROM DATE AND REMOVED ALL CALLS TO SCREEN
     H*             99
     H*
      ** 07/05/95 C:35     - REQ/PROJ: NW008                         **
      **                     MOD TO ALLOW SYSTEM TYPE M              **
      *****************************************************************
      ** INDICATORS:                                                 **
      **  71          INVALID PRODUCT/SYSTEM CODE                    **
      **  72          INVALID SUBSCRIBER #                           **
      **  73          INVALID GROUP #                                **
      **  74          INVALID FROM DATE                              **
      **  75          INVALID TO DATE                                **
      **  76          DATES MUST BE IN ASCENDING ORDER               **
      **  77          INVALID REPORT TYPE                            **
      **  78          INVALID CASE                                   **
      **  79          POSITION CURSOR @ GROUP AFTER SUB SRCH         **
      **  80          POSITION CURSOR @ YR/CASE AFTER GROUP SRCH     **
      **  81          POSITION CURSOR @ FRDT AFTER CASE SRCH         **
      **  22-56       INVALID A/R CODES                              **
      **  65-70                                                      **
      *****************************************************************
     FN7736F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFTABL     IF   E           K DISK
      **** BEGIN ADD **** 04/28/98 ************************************
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     FFTACP     IF   E             DISK
     F                                     RENAME(PAC01:PAC02)
      **** END   ADD **** 04/28/98 ************************************

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
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
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*
     C*  CASE MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C*
     C*
     C*  KEY LIST FOR TABLE FILE
     C*
     C     TBKY1         KLIST
     C                   KFLD                    UPCO#
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   MOVE      *BLANK        CD
     C                   Z-ADD     1             X                 2 0
     C     TBKY1         CHAIN     PAC01                              60
     C     *IN60         DOWEQ     '0'
     C     ACSTAT        IFNE      'I'
     C                   MOVEL     ACKEY         XXCODE            2
     C                   MOVE      XXCODE        CD(X)
     C                   ADD       1             X
     C                   END
     C     X             IFEQ      31
     C                   MOVE      '1'           *IN60
     C                   ELSE
     C     TBKY1         READE     PAC01                                  60
     C                   END
     C                   END
     C*
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C**********************************
     C*   F3 = EXIT
     C**********************************
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END                                                    END *INKC
      **** BEGIN CHG **** 04/28/98 ************************************
      *
     C**********************************
     C*   F10 - SEARCH                 *
     C**********************************
     C     *INKJ         IFEQ      '1'
     C**********************************
     C*     PRODUCT SEARCH             *
     C**********************************
      *
     C     HFLD          IFEQ      'SCSYS'
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRNP            9 0
     C                   PARM      'SYS'         ##TBID           10
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         CABEQ     *ON           STRT                     82
     C     HFLD          IFEQ      *BLANKS
     C                   MOVE      *ON           *IN82
     C                   ENDIF                                                  IFEQ *BLANKS
      *
     C     ##RRNP        CHAIN     PGT02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCSYS
     C                   MOVE      *ON           *IN83
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   ENDIF                                                  HFLD SCSYS
      *
     C**********************************
     C**********   F6 - SUBSCRIBER SEARCH      *
     C**********************************
     C********** *INKF     IFEQ '1'
     C******************** CALL 'F0545R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCSUB#
     C******************** PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C********** QQRTC     IFEQ 'B'
     C********** SCSUB#    OREQ *ZERO
     C**************       WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C******************** END                             END EQ B/0
     C*
     C********** QQRTC     IFEQ '*'
     C**************       WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** MOVE '1'       *IN79            POS CURSOR @ GRP
     C******************** GOTO SCR01T
     C******************** END                             END EQ *
     C*
     C******************** END                             END *INKF
      *
     C**********************************
     C*   SUBSCRIBER SEARCH            *
     C**********************************
      *
     C     HFLD          IFEQ      'SCSUB#'
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRNS            9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         CABEQ     *ON           STRT                     83
      *
     C     ##RRNS        CHAIN     PSM02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SMSUB#        SCSUB#
     C                   MOVE      *ON           *IN79
     C                   ELSE
     C                   MOVE      *ON           *IN83
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   ENDIF                                                  HFLD SCSUB#
     C**********************************
     C**********   COMMAND  7 - GROUP SEARCH    *
     C**********************************
     C********** *INKG     IFEQ '1'
     C******************** CALL 'F0508R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCSUB#
     C******************** PARM           SCFND
     C******************** PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C********** QQRTC     IFEQ 'B'
     C********** SCFND     OREQ *ZERO
     C****************     WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C******************** END
     C*
     C********** QQRTC     IFEQ '*'
     C****************     WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** MOVE '1'       *IN80            POS CURSOR @ YR
     C******************** GOTO SCR01T
     C******************** END                             END EQ *
     C*
     C******************** END                             END *INKG
      *
     C**********************************
     C*        GROUP SEARCH            *
     C**********************************
      *
     C******** Beg Del ***** 12/18/24 ***************************
     C*****HFLD          IFEQ      'SCFND'
     C*****              MOVE      ##RRNS        ##RRNF
     C*****              CALL      'F0060R'
     C*****              PARM      '2'           ##CALL            1
     C******INKC         PARM                    ##KC              1
     C******INKL         PARM                    ##KL              1
     C*****              PARM                    ##RRNF            9 0
      *****
     C******INKC         CABEQ     *ON           TERM
      *****
     C******INKL         CABEQ     *ON           STRT                     79
      *****
     C*****##RRNF        CHAIN     PFN02                              60
     C******IN60         IFEQ      *OFF
     C*****              MOVE      FNFND         SCFND
     C*****              MOVE      *ON           *IN80
     C*****              ELSE
     C*****              MOVE      *ON           *IN79
     C*****              ENDIF                                                  IFEQ *IN60
      *****
     C*****              ENDIF                                                  HFLD SCFND
     C******** End Del ***** 12/18/24 ***************************
     C**********************************
     C**********   COMMAND  9 - CASE  SEARCH    *
     C**********************************
     C********** *INKI     IFEQ '1'
     C******************** CALL 'F0583R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCSUB#
     C******************** PARM           SCFND
     C******************** PARM           SCFYR
     C******************** PARM           SCCASE
     C******************** PARM           XXSS#   9
     C******************** PARM           QQRTCD  1
     C********** QQRTCD    CABEQ'A'       TERM
     C*
     C********** QQRTC     IFEQ 'B'
     C********** SCFYR     OREQ *ZERO
     C*****************    WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** GOTO STRT
     C******************** END                             END EQ B/0
     C*
     C********** QQRTC     IFEQ '*'
     C*****************    WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C******************** Z-ADDSCFYR     XXFYR
     C******************** MOVE '1'       *IN81            POS CURSOR @ FRDT
     C******************** GOTO SCR01T
     C******************** END                             END EQ *
     C******************** END                             END *INKI
      *
     C**********************************
     C*   YEAR/CASE SEARCH             *
     C**********************************
      *
     C     HFLD          IFEQ      'SCFYR'
     C     HFLD          OREQ      'SCCASE'
     C                   MOVE      SCSUB#        ##SUB#
     C                   MOVE      SCFND         ##FND
     C                   CALL      'F0269R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FND             3 0
     C                   PARM      0             ##FYR             3 0
     C                   PARM      0             ##CASE            7 0
     C                   PARM                    ##SS#             9
     C                   PARM                    ##ACDT            8 0
     C                   PARM                    ##RTCD            1
      *
     C     ##FYR         IFEQ      *ZEROS
     C     HFLD          CABEQ     'SCFYR'       STRT                     80    PC AT SCFYR
     C                   ENDIF                                                  IFEQ QQFYR
     C     ##CASE        IFEQ      *ZEROS
     C     HFLD          CABEQ     'SCCASE'      STRT                     85    PC AT SCCASE
     C                   ENDIF                                                  IFEQ QQCASE
      *
     C     ##RTCD        IFEQ      '*'
     C                   MOVE      *ON           *IN81
     C                   MOVE      ##SUB#        SCSUB#
     C                   MOVE      ##FND         SCFND
     C                   MOVE      ##FYR         SCFYR
     C                   MOVE      ##CASE        SCCASE
     C                   ENDIF                                                  IFEQ '*'
      *
     C                   ENDIF                                                  HFLD SCFYR
      *
     C**********************************
     C**********   COMMAND 10 -  TABLE SEARCH   *
     C**********************************
     C********** *INKJ     IFEQ '1'
     C********** Q2RECF    ANDEQ'SCR01'
     C******************** Z-ADD1         X       20
     C*
     C********** X         DOUEQ31
     C******************** MOVELCD,X      XX2     2
     C********** XX2       IFEQ '? '
     C*
     C******************** MOVE *BLANK    SCKEY  10
     C******************** CALL 'F2181R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           SCKEY
     C******************** PARM           QQRTC   1
     C******************** MOVELSCKEY     CD,X
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C***************      WRITESCR99
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTC     IFEQ 'B'
     C********** SCKEY     OREQ *BLANK
     C********** X         ADD  26        X1      20
     C******************** MOVE '1'       *IN,X1
     C******************** GOTO STRT
     C******************** END
     C*
     C********** QQRTC     IFEQ '*'
     C***********X******** ADD  27        X1      20
     C***********X1******* IFNE 57
     C******************** MOVE '1'       *IN,X1
     C******************** END
     C******************** GOTO STRT
     C******************** END
     C********************
     C******************** END
     C******************** ADD  1         X
     C******************** END
     C******************** GOTO STRT
     C******************** END
      *
     C*********************************
     C*       A/R CODE SEARCH         *
     C*********************************
      *
     C     HFLD          IFEQ      'SCCD01'
     C     HFLD          OREQ      'SCCD02'
     C     HFLD          OREQ      'SCCD03'
     C     HFLD          OREQ      'SCCD04'
     C     HFLD          OREQ      'SCCD05'
     C     HFLD          OREQ      'SCCD06'
     C     HFLD          OREQ      'SCCD07'
     C     HFLD          OREQ      'SCCD08'
     C     HFLD          OREQ      'SCCD09'
     C     HFLD          OREQ      'SCCD10'
     C     HFLD          OREQ      'SCCD11'
     C     HFLD          OREQ      'SCCD12'
     C     HFLD          OREQ      'SCCD13'
     C     HFLD          OREQ      'SCCD14'
     C     HFLD          OREQ      'SCCD15'
     C     HFLD          OREQ      'SCCD16'
     C     HFLD          OREQ      'SCCD17'
     C     HFLD          OREQ      'SCCD18'
     C     HFLD          OREQ      'SCCD19'
     C     HFLD          OREQ      'SCCD20'
     C     HFLD          OREQ      'SCCD21'
     C     HFLD          OREQ      'SCCD22'
     C     HFLD          OREQ      'SCCD23'
     C     HFLD          OREQ      'SCCD24'
     C     HFLD          OREQ      'SCCD25'
     C     HFLD          OREQ      'SCCD26'
     C     HFLD          OREQ      'SCCD27'
     C     HFLD          OREQ      'SCCD28'
     C     HFLD          OREQ      'SCCD29'
     C     HFLD          OREQ      'SCCD30'
     C                   CALL      'N2305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RRN             9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      0
     C     HFLD          CABEQ     'SCCD01'      STRT                     27
     C     HFLD          CABEQ     'SCCD02'      STRT                     28
     C     HFLD          CABEQ     'SCCD03'      STRT                     29
     C     HFLD          CABEQ     'SCCD04'      STRT                     30
     C     HFLD          CABEQ     'SCCD05'      STRT                     31
     C     HFLD          CABEQ     'SCCD06'      STRT                     32
     C     HFLD          CABEQ     'SCCD07'      STRT                     33
     C     HFLD          CABEQ     'SCCD08'      STRT                     34
     C     HFLD          CABEQ     'SCCD09'      STRT                     35
     C     HFLD          CABEQ     'SCCD10'      STRT                     36
     C     HFLD          CABEQ     'SCCD11'      STRT                     37
     C     HFLD          CABEQ     'SCCD12'      STRT                     38
     C     HFLD          CABEQ     'SCCD13'      STRT                     39
     C     HFLD          CABEQ     'SCCD14'      STRT                     40
     C     HFLD          CABEQ     'SCCD15'      STRT                     41
     C     HFLD          CABEQ     'SCCD16'      STRT                     42
     C     HFLD          CABEQ     'SCCD17'      STRT                     43
     C     HFLD          CABEQ     'SCCD18'      STRT                     44
     C     HFLD          CABEQ     'SCCD19'      STRT                     45
     C     HFLD          CABEQ     'SCCD20'      STRT                     46
     C     HFLD          CABEQ     'SCCD21'      STRT                     47
     C     HFLD          CABEQ     'SCCD22'      STRT                     48
     C     HFLD          CABEQ     'SCCD23'      STRT                     49
     C     HFLD          CABEQ     'SCCD24'      STRT                     50
     C     HFLD          CABEQ     'SCCD25'      STRT                     51
     C     HFLD          CABEQ     'SCCD26'      STRT                     52
     C     HFLD          CABEQ     'SCCD27'      STRT                     53
     C     HFLD          CABEQ     'SCCD28'      STRT                     54
     C     HFLD          CABEQ     'SCCD29'      STRT                     55
     C     HFLD          CABEQ     'SCCD30'      STRT                     56
     C                   ENDIF                                                  IFEQ INKL
      *
     C     ##RRN         CHAIN     PAC02                              60
     C     *IN60         IFEQ      *OFF
     C                   SELECT
     C     HFLD          WHENEQ    'SCCD01'
     C                   MOVEL     ACKEY         SCCD01
     C                   MOVE      *ON           *IN28
     C     HFLD          WHENEQ    'SCCD02'
     C                   MOVEL     ACKEY         SCCD02
     C                   MOVE      *ON           *IN29
     C     HFLD          WHENEQ    'SCCD03'
     C                   MOVEL     ACKEY         SCCD03
     C                   MOVE      *ON           *IN30
     C     HFLD          WHENEQ    'SCCD04'
     C                   MOVEL     ACKEY         SCCD04
     C                   MOVE      *ON           *IN31
     C     HFLD          WHENEQ    'SCCD05'
     C                   MOVEL     ACKEY         SCCD05
     C                   MOVE      *ON           *IN32
     C     HFLD          WHENEQ    'SCCD06'
     C                   MOVEL     ACKEY         SCCD06
     C                   MOVE      *ON           *IN33
     C     HFLD          WHENEQ    'SCCD07'
     C                   MOVEL     ACKEY         SCCD07
     C                   MOVE      *ON           *IN34
     C     HFLD          WHENEQ    'SCCD08'
     C                   MOVEL     ACKEY         SCCD08
     C                   MOVE      *ON           *IN35
     C     HFLD          WHENEQ    'SCCD09'
     C                   MOVEL     ACKEY         SCCD09
     C                   MOVE      *ON           *IN36
     C     HFLD          WHENEQ    'SCCD10'
     C                   MOVEL     ACKEY         SCCD10
     C                   MOVE      *ON           *IN37
     C     HFLD          WHENEQ    'SCCD11'
     C                   MOVEL     ACKEY         SCCD11
     C                   MOVE      *ON           *IN38
     C     HFLD          WHENEQ    'SCCD12'
     C                   MOVEL     ACKEY         SCCD12
     C                   MOVE      *ON           *IN39
     C     HFLD          WHENEQ    'SCCD13'
     C                   MOVEL     ACKEY         SCCD13
     C                   MOVE      *ON           *IN40
     C     HFLD          WHENEQ    'SCCD14'
     C                   MOVEL     ACKEY         SCCD14
     C                   MOVE      *ON           *IN41
     C     HFLD          WHENEQ    'SCCD15'
     C                   MOVEL     ACKEY         SCCD15
     C                   MOVE      *ON           *IN42
     C     HFLD          WHENEQ    'SCCD16'
     C                   MOVEL     ACKEY         SCCD16
     C                   MOVE      *ON           *IN43
     C     HFLD          WHENEQ    'SCCD17'
     C                   MOVEL     ACKEY         SCCD17
     C                   MOVE      *ON           *IN44
     C     HFLD          WHENEQ    'SCCD18'
     C                   MOVEL     ACKEY         SCCD18
     C                   MOVE      *ON           *IN45
     C     HFLD          WHENEQ    'SCCD19'
     C                   MOVEL     ACKEY         SCCD19
     C                   MOVE      *ON           *IN46
     C     HFLD          WHENEQ    'SCCD20'
     C                   MOVEL     ACKEY         SCCD20
     C                   MOVE      *ON           *IN47
     C     HFLD          WHENEQ    'SCCD21'
     C                   MOVEL     ACKEY         SCCD21
     C                   MOVE      *ON           *IN48
     C     HFLD          WHENEQ    'SCCD22'
     C                   MOVEL     ACKEY         SCCD22
     C                   MOVE      *ON           *IN49
     C     HFLD          WHENEQ    'SCCD23'
     C                   MOVEL     ACKEY         SCCD23
     C                   MOVE      *ON           *IN50
     C     HFLD          WHENEQ    'SCCD24'
     C                   MOVEL     ACKEY         SCCD24
     C                   MOVE      *ON           *IN51
     C     HFLD          WHENEQ    'SCCD25'
     C                   MOVEL     ACKEY         SCCD25
     C                   MOVE      *ON           *IN52
     C     HFLD          WHENEQ    'SCCD26'
     C                   MOVEL     ACKEY         SCCD26
     C                   MOVE      *ON           *IN53
     C     HFLD          WHENEQ    'SCCD27'
     C                   MOVEL     ACKEY         SCCD27
     C                   MOVE      *ON           *IN54
     C     HFLD          WHENEQ    'SCCD28'
     C                   MOVEL     ACKEY         SCCD28
     C                   MOVE      *ON           *IN55
     C     HFLD          WHENEQ    'SCCD29'
     C                   MOVEL     ACKEY         SCCD29
     C                   MOVE      *ON           *IN56
     C     HFLD          WHENEQ    'SCCD30'
     C                   MOVEL     ACKEY         SCCD30
     C                   MOVE      *ON           *IN27
     C                   ENDSL
     C                   GOTO      STRT
     C                   ENDIF                                                  IFEQ *IN60
      *
     C                   ENDIF                                                  IFEQ HFLD
      *
     C*                    MOVE *ON       *IN90
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  IFEQ INKJ
      **** END   CHG **** 04/28/98 ************************************
     C**********************************
     C*   PROCESS HELP REQUESTS        *
     C**********************************
     C     *IN10         IFEQ      '1'
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C*****************    WRITESCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *IN10
      **** BEGIN DEL **** 04/28/98 ************************************
     C**********************************
     C**********   COMMAND 13 - CHG USER PARMS  *
     C**********************************
     C********** *INKM     IFEQ '1'
     C*
     C******************** CALL 'F0006R'
     C*****************    WRITESCR99
     C*
     C**********  GET THE USER PARAMETER FILE
     C*
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C******************** MOVE *ZERO     UPCO#
     C******************** MOVE *ZERO     UPSUB#
     C******************** END
     C******************** GOTO STRT01
     C******************** END                             END *INKM
      **** END   DEL **** 04/28/98 ************************************
     C*
     C                   END                                                    END CMDKEYS
     C*****************************************************************
     C*   PROCESS REQUEST SCREEN
     C*****************************
     C     SCR01T        TAG
     C*****************************
     C* VALIDATE SELECTIONS:      *
     C*****************************
     C*
     C*   IF ENTERED, PRODUCT/SYSTEM MUST BE VALID
     C*
     C     SCSYS         IFNE      *BLANKS
     C     SCSYS         IFNE      'P'                                          PBR
     C     SCSYS         ANDNE     'H'                                          HBA
     C     SCSYS         ANDNE     'N'                                          NET
     C     SCSYS         ANDNE     'U'                                          UR
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C     SCSYS         ANDNE     'M'                                          MED MGMT
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C     SCSYS         ANDNE     'A'                                          OTHER
     C                   MOVE      '1'           *IN71                          INVALID SYSTEM
     C                   GOTO      STRT
     C                   END                                                    ENDNE P/H/N/A
     C                   END                                                    ENDNE *BLANKS
     C*
     C*   IF ENTERED, SUBSCRIBER MUST BE VALID
     C*
     C     SCSUB#        IFNE      *ZEROS
     C     SMKEY         CHAIN     PSM01                              72        INVALID SUBSCRIBER
     C     *IN72         CABEQ     '1'           STRT
     C                   END                                                    ENDNE *ZEROS
     C*
     C*   IF ENTERED, GROUP MUST BE VALID
     C*
     C     SCFND         IFNE      *ZEROS
     C     FNKEY         CHAIN     PFN01                              73        INVALID GROUP
     C     *IN73         CABEQ     '1'           STRT
     C                   END                                                    ENDNE *ZEROS
     C*
     C*   IF ENTERED, CASE MUST BE VALID
     C*
     C     SCCASE        IFNE      *ZEROS
     C                   MOVE      SCFYR         XXFYR
     C     CMKEY         CHAIN     PCM01                              78        INVALID CASE
     C     *IN78         CABEQ     '1'           STRT
     C                   END                                                    ENDNE *ZEROS
     C*
     C*   IF ENTERED, VALIDATE DATES & ASCENDING ORDER
     C*
     C     SCFRDT        IFNE      *ZEROS
     C     SCTODT        ORNE      *ZEROS
     C*
     C*   IF ENTERED, FROM DATE MUST BE VALID
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     74    INVALID FROM DATE
     C*
     C*   IF ENTERED, TO DATE MUST BE VALID
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         CABEQ     '1'           STRT                     75    INVALID TO DATE
     C*
     C*   IF ENTERED, DATES MUST BE IN ASCENDING ORDER
     C*********** BEGIN CHANGE *************** 12/04/97 ***************
     C**************       MOVE SCFRDT    SRFRDC  6
     C**************       MOVE SCTODT    SRTODC  6
     C**************
     C**************       CALL 'SRDDIF'
     C**************       PARM           SRFRDC
     C**************       PARM           SRTODC
     C**************       PARM           SRDAYS  50
     C**************       PARM ' '       SRERR   1
     C**************       PARM ' '       SRTERM  1
     C**************
     C********** SRERR     CABEQ'1'       STRT           25INVALID ORDER
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SRFRDC            7 0
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SRTODC            7 0
     C     SRFRDC        IFGT      SRTODC
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*********** END   CHANGE *************** 12/04/97 ***************
     C*
     C                   END                                                    ENDNE *ZEROS
     C*
     C*   REPORT TYPE MUST BE D, S OR B
     C*
     C     SCRTYP        IFNE      'D'                                          DETAIL
     C     SCRTYP        ANDNE     'S'                                          SUMMARY
     C     SCRTYP        ANDNE     'B'                                          BOTH
     C                   MOVE      '1'           *IN77                          INVALID REPORT TYPE
     C                   GOTO      STRT
     C                   END                                                    ENDNE D/S/B
     C*
     C*  VALIDATE THE A/R CODES
     C*
     C                   Z-ADD     1             X
     C     X             DOUEQ     31
     C*
     C     CD(X)         IFNE      *BLANKS
     C*
     C                   MOVEA     '0000'        *IN(65)
     C                   MOVE      '0'           *IN70
     C     X             DIV       5.1           XXROW             2 0
     C     XXROW         ADD       65            X2                2 0
     C                   MOVE      '1'           *IN(X2)
     C                   MULT      5             XXROW
     C     X             SUB       XXROW         XXCOL             2 0
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     CD(X)         XXKEY
     C     TBKEY         CHAIN     PAC01                              40
     C     *IN40         IFEQ      '1'
     C     XXCOL         ADD       21            X1                2 0
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C                   END
     C     ACSTAT        IFEQ      'I'
     C     XXCOL         ADD       21            X1                2 0
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C                   ADD       1             X
     C                   END
     C*
     C****************************************
     C*   CALL THE CLP TO SUBMIT THE LIST.   *
     C****************************************
     C                   MOVE      SCSYS         QQSYS             1
     C                   MOVE      UPCO#         QQCO#
     C                   MOVE      SCSUB#        QQSUB#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C******* BEGIN CHANGE ******** 12/04/97 **************************
     C****************     MOVE SCFRDT    QQFRDT  6
     C****************     MOVE SCTODT    QQTODT  6
     C                   MOVE      SCFRDT        QQFRDT            8
     C                   MOVE      SCTODT        QQTODT            8
     C******* END   CHANGE ******** 12/04/97 **************************
     C                   MOVEA     CD            QQCODE           60
     C                   MOVE      SCRTYP        QQRTYP            1
     C                   CALL      'N7736C'
     C                   PARM                    QQSYS             1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQCODE           60
     C                   PARM                    QQRTYP            1
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
     C*****************************
     C*  END THE PROGRAM          *
     C*****************************
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
