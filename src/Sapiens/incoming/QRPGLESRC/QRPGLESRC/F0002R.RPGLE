     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**********************************************************
     H* PROGRAM: F0002R                                        *
     H* DESCRIPTION: FUND AUXILIARY FILE MAINTENANCE           *
     H**********************************************************
     H* INDICATORS:                                            *
     H*  91          INVOICING INSTALLED                       *
     H* SCR01:                                                 *
     H*  20          FUND REQUIRED                             *
     H*  21          INVALID FUND                              *
     H* SCR02:                                                 *
     H*  09          INQUIRY MODE (QQCALL = 1)                 *
     H*  20 (NOT USED) CARRIER AUDIT # REQUIRED                *
     H*  21 (NOT USED) AGENT AUDIT # REQUIRED                  *
     H*  26          BILL REVIEW NOT (Y/N)                     *
     H*  94          NETWORK INSTALLED OR NOT                  *
     H* SCRST:                                                 *
     H*  09          INQUIRY MODE (QQCALL = 1)                 *
     H*  24          STATE CODE NOT VALID                      *
     H*  25          STATE CODE NOT INSTALLED                  *
     H* SCRFL:                                                 *
     H*  09          INQUIRY MODE (QQCALL = 1)                 *
     H*  20          CARRIER AUDIT # REQUIRED                  *
     H*  21          CARRIER FEIN # REQUIRED                   *
     H*  22          CARRIER ZIP CODE REQUIRED                 *
     H*  23          AGENT FEIN# REQUIRED,IF AGT AUDIT# ENTERED*
     H*  24          AGENT ZIP CODE RQRD, IF AGT AUDIT# ENTERED*
     H* SCRTX:                                                 *
     H*  09          INQUIRY MODE (QQCALL = 1)                 *
     H*  21          MASTER BUSINESS IDENTIFIER REQUIRED       *
     H**********************************************************
     H* REVISIONS:                                             *
     H*
     H* 10/07/98 C:94 PROJECT: 87MFX
     H*               CHANGE F12=PREVIOUS TO F12=CANCEL
     H*
     H**********************************************************
     H*   NEW COMMENTS START HERE
     H**********************************************************
     H*                                                        *
     H* 11/13/97 C:53  PROJECT: MC145                          *
     H*                CONDITION SCR04 ON NETWORK FLAG
     H*
     H* 01/22/91 LJD - DON'T REQUIRE AGENT AUDIT #             *
     H* 07/21/92 KRR   PROJ: 99AAC                             *
     H*                1.ADDED SUB# TO ALL PROCESSING          *
     H* 01/25/93 C:20  PROJ:                                   *
     H*                1.CHANGED F13 FROM W0006R TO F0006R     *
     H* 02/25/93 C:20  PROJ:                                   *
     H*                1.ADDED SCOUTA(OUTSIDE ACCOUNT) TO SCR02*
     H* 06/01/93 C:20  PROJ: 99AAE                             *
     H*                1.ADDED SCNTWK(OUTSIDE NETWORK) TO SCR02*
     H* 12/07/93 C:20  REQ/PROJ: FLMNT                         *
     H*                1.DO NOT REQUIRE CARRIER INTERNAL AUDIT#*
     H* 03/22/94 C:20  REQ/PROJ: 98/NW001                      *
     H*                1.REMOVED SCNTWK FLAG FROM SCREEN       *
     H*                2.ADDED NETWORK CODE AND FLAGS 1-10     *
     H* 03/31/94 C:20  REQ/PROJ: 98/NW001                      *
     H*                1.FOR CHAIN TO FMF1L USE 95 AS INDICATOR*
     H*                  WAS USING 40                          *
     H*                                                        *
     H* 06/15/95 C:26  PROJECT: AR012                          *
     H*                ADD EDIT OF NETWORK FLAG FOR 'I'        *
     H*                                                        *
     H* 06/15/95 C:33  PROJECT: FL006                          *
     H*                SCR02: REMOVE                           *
     H*                       CARRIER INTERNAL AUDIT#          *
     H*                       AGENT INTERNAL AUDIT#            *
     H*                SCRST: STATE PROMPT                     *
     H*                       CONDITION ON WHETHER SFBPF/      *
     H*                       FBTP01-TP10 HAS ANYTHING IN IT   *
     H*                SCRFL: FLORIDA STATE TAPE INFO SCREEN   *
     H*                       CARRIER INTERNAL AUDIT#: REQUIRED*
     H*                       CARRIER FEIN & ZIP: REQUIRED     *
     H*                       AGENT INTERNAL AUDIT#:           *
     H*                       AGENT FEIN & ZIP: REQUIRED IF    *
     H*                          AUDIT# ENTERED                *
     H*                                                        *
     H* 07/25/95 C:02  PROJECT: 25MAL                          *
     H*                CHANGE... ADD SCR04                     *
     H*                       MOVE FIELDS FROM SCR02 TO 04 AND *
     H*                       ADD NEW FIELDS FROM F1P
     H*                                                        *
     H* 07/28/95 C:28  PROJECT:25MAA                           *
     H*                ADDED ALTERNATE ADDRESS AT GROUP LEVEL  *
     H*                (SCR02).                                *
     H*                                                        *
     H* 08/14/95 C:33  PROJECT: TX002                          *
     H*                SCRTX: TEXAS STATE TAPE INFO SCREEN     *
     H*                       MASTER BUSINESS IDENTIFER:       *
     H*                       REQUIRED                         *
     H*                                                        *
     H* 11/27/96 C:32  REQ/PROJ: 12/60MAL                      *
     H*                1.ADD BILL REVIEW SERVICE PAID FLAG TO  *
     H*                  SCREEN  USING F1FLG2.                 *
     H*                                                        *
     H* 11/27/96 C:32  PROJECT: 60MAP                          *
     H*                ADD BRS % USING F1N61.                  *
     H*                                                        *
     H* 12/02/96 C:59 Req/Proj: 203/FS134                      *
     H*               Call to program CHKSTR to verify: if     *
     H*               State is installed.  Returns '1' If found*
     H*               and '0' if not found.                    *
     H*                                                        *
     H* 01/29/97 C:56  PROJECT: 87MBD                          *
     H*                Add WCMC Check Issuance Flag for SCR02  *
     H*                                                        *
     H* 02/06/97 C:56  PROJECT: 87MBE                          *
     H*                PREVENT USER FROM REMOVAL OF A NETWORK  *
     H*                CODE THAT IS ASSIGNED IN EMPLOYER       *
     H*                MASTER (SCR04)                          *
     H**********************************************************
     FF0002F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMF1L     UF A E           K DISK
     FFDF2L     UF A E           K DISK
     FSSYLF     IF   E           K DISK
     FNMNWL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMEML2    IF   E           K DISK

     D*
     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D ST              S              2    DIM(2) CTDATA PERRCD(1)
     D SNW             S             10    DIM(10) CTDATA PERRCD(1)
     D SNF             S             10    DIM(10) CTDATA PERRCD(1)
     D SN$             S             10    DIM(10) CTDATA PERRCD(1)
     D SNT             S             10    DIM(10) CTDATA PERRCD(1)
     D SNI             S             10    DIM(10) CTDATA PERRCD(1)
     D SNM             S             10    DIM(10) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  ST2                    1     20
     D                                     DIM(10)
     D  FBST01                 1      2
     D  FBST02                 3      4
     D  FBST03                 5      6
     D  FBST04                 7      8
     D  FBST05                 9     10
     D  FBST06                11     12
     D  FBST07                13     14
     D  FBST08                15     16
     D  FBST09                17     18
     D  FBST10                19     20
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  NW                     1     50
     D                                     DIM(10)                                NETWORK CODE
     D  SCNW01                 1      5
     D  SCNW02                 6     10
     D  SCNW03                11     15
     D  SCNW04                16     20
     D  SCNW05                21     25
     D  SCNW06                26     30
     D  SCNW07                31     35
     D  SCNW08                36     40
     D  SCNW09                41     45
     D  SCNW10                46     50
     D                 DS
     D  NF                     1     10
     D                                     DIM(10)                                NETWORK FLAGS
     D  SCNF01                 1      1
     D  SCNF02                 2      2
     D  SCNF03                 3      3
     D  SCNF04                 4      4
     D  SCNF05                 5      5
     D  SCNF06                 6      6
     D  SCNF07                 7      7
     D  SCNF08                 8      8
     D  SCNF09                 9      9
     D  SCNF10                10     10
     D                 DS
     D  N$                     1     90  2
     D                                     DIM(10)                                NETWORK FEE
     D  SCN$01                 1      9  2
     D  SCN$02                10     18  2
     D  SCN$03                19     27  2
     D  SCN$04                28     36  2
     D  SCN$05                37     45  2
     D  SCN$06                46     54  2
     D  SCN$07                55     63  2
     D  SCN$08                64     72  2
     D  SCN$09                73     81  2
     D  SCN$10                82     90  2
     D                 DS
     D  NT                     1     10
     D                                     DIM(10)                                NETWORK FEE TYPE
     D  SCNT01                 1      1
     D  SCNT02                 2      2
     D  SCNT03                 3      3
     D  SCNT04                 4      4
     D  SCNT05                 5      5
     D  SCNT06                 6      6
     D  SCNT07                 7      7
     D  SCNT08                 8      8
     D  SCNT09                 9      9
     D  SCNT10                10     10
     D                 DS
     D  NI                     1     90  0
     D                                     DIM(10)                                NETWORK FEIN
     D  SCNI01                 1      9  0
     D  SCNI02                10     18  0
     D  SCNI03                19     27  0
     D  SCNI04                28     36  0
     D  SCNI05                37     45  0
     D  SCNI06                46     54  0
     D  SCNI07                55     63  0
     D  SCNI08                64     72  0
     D  SCNI09                73     81  0
     D  SCNI10                82     90  0
     D                 DS
     D  SI                     1     90
     D                                     DIM(10)                                NETWORK FEIN
     D  SCSI01                 1      9
     D  SCSI02                10     18
     D  SCSI03                19     27
     D  SCSI04                28     36
     D  SCSI05                37     45
     D  SCSI06                46     54
     D  SCSI07                55     63
     D  SCSI08                64     72
     D  SCSI09                73     81
     D  SCSI10                82     90
     D                 DS
     D  NM                     1     30  0
     D                                     DIM(10)                                NETWORK MODIFIER
     D  SCNM01                 1      3  0
     D  SCNM02                 4      6  0
     D  SCNM03                 7      9  0
     D  SCNM04                10     12  0
     D  SCNM05                13     15  0
     D  SCNM06                16     18  0
     D  SCNM07                19     21  0
     D  SCNM08                22     24  0
     D  SCNM09                25     27  0
     D  SCNM10                28     30  0
     D                 DS
     D  NN                     1    250
     D                                     DIM(10)                                NETWORK NAME
     D  SCNN01                 1     25
     D  SCNN02                26     50
     D  SCNN03                51     75
     D  SCNN04                76    100
     D  SCNN05               101    125
     D  SCNN06               126    150
     D  SCNN07               151    175
     D  SCNN08               176    200
     D  SCNN09               201    225
     D  SCNN10               226    250
     D                 DS
     D  F$                     1     90  2
     D                                     DIM(10)                                NETWORK FEE
     D  F1N$01                 1      9  2
     D  F1N$02                10     18  2
     D  F1N$03                19     27  2
     D  F1N$04                28     36  2
     D  F1N$05                37     45  2
     D  F1N$06                46     54  2
     D  F1N$07                55     63  2
     D  F1N$08                64     72  2
     D  F1N$09                73     81  2
     D  F1N$10                82     90  2
     D                 DS
     D  FT                     1     10
     D                                     DIM(10)                                NETWORK FEE TYPE
     D  F1NT01                 1      1
     D  F1NT02                 2      2
     D  F1NT03                 3      3
     D  F1NT04                 4      4
     D  F1NT05                 5      5
     D  F1NT06                 6      6
     D  F1NT07                 7      7
     D  F1NT08                 8      8
     D  F1NT09                 9      9
     D  F1NT10                10     10
     D                 DS
     D  FI                     1     90  0
     D                                     DIM(10)                                NETWORK FEIN
     D  F1NI01                 1      9  0
     D  F1NI02                10     18  0
     D  F1NI03                19     27  0
     D  F1NI04                28     36  0
     D  F1NI05                37     45  0
     D  F1NI06                46     54  0
     D  F1NI07                55     63  0
     D  F1NI08                64     72  0
     D  F1NI09                73     81  0
     D  F1NI10                82     90  0
     D                 DS
     D  FM                     1     30  0
     D                                     DIM(10)                                NETWORK MODIFIER
     D  F1NM01                 1      3  0
     D  F1NM02                 4      6  0
     D  F1NM03                 7      9  0
     D  F1NM04                10     12  0
     D  F1NM05                13     15  0
     D  F1NM06                16     18  0
     D  F1NM07                19     21  0
     D  F1NM08                22     24  0
     D  F1NM09                25     27  0
     D  F1NM10                28     30  0
     D*
     D*** BEGIN ADD *** 02/06/97 **************************************
     D                 DS
     D  HLNW                   1     50
     D                                     DIM(10)                                HOLD NETWORK CODE
     D  HLNW01                 1      5
     D  HLNW02                 6     10
     D  HLNW03                11     15
     D  HLNW04                16     20
     D  HLNW05                21     25
     D  HLNW06                26     30
     D  HLNW07                31     35
     D  HLNW08                36     40
     D  HLNW09                41     45
     D  HLNW10                46     50

     I*** END ADD *** 02/06/97 ****************************************

     C*
     C*** BEGIN ADD *** 02/06/97 **************************************
     C     *LIKE         DEFINE    EMNWCD        XXNWCD
     C*** END ADD *** 02/06/97 ****************************************
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE SUBSCRIBER
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C*
     C*   KEY LIST FOR THE FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE GROUP STATE INFORMATION FILE
     C*
     C     F2KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXFND
     C*
     C     F2KY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    SCST
     C*
     C*  FS BASE CONTROL FILE
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
     C*
     C* PPO NETWORK MASTER FILE
     C*
     C     NWKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    NW(A)
     C*
     C* PROVIDER MASTER FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    NI(Q)
     C                   KFLD                    NM(Q)
     C*
     C*** BEGIN ADD *** 02/06/97 **************************************
     C*
     C*   KEY LIST FOR EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXNWCD
     C*** END ADD *** 02/06/97 ****************************************
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
     C                   MOVEL     SNW(1)        XX10A            10
     C                   MOVEL     SNF(1)        XX10A            10
     C                   MOVEL     SN$(1)        XX10A            10
     C                   MOVEL     SNT(1)        XX10A            10
     C                   MOVEL     SNI(1)        XX10A            10
     C                   MOVEL     SNM(1)        XX10A            10
     C* INITIALIZE WORK FIELDS
     C     *LIKE         DEFINE    FNFND         XXFND
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   END
     C*
     C** CHECK FOR INVOICE FUNCTION
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              60
     C     *IN60         IFEQ      '0'
     C     FBINVF        IFEQ      'Y'
     C                   MOVE      '1'           *IN91
     C                   ELSE                                                   ELSE FBFL02 = 'Y'
     C                   MOVE      '0'           *IN91
     C                   END                                                    END FBFL02 = 'Y'
     C     FBFL01        IFEQ      'Y'
     C                   MOVE      '1'           *IN71
     C                   ELSE                                                   ELSE FBFL01 = 'Y'
     C                   MOVE      '0'           *IN71
     C                   END                                                    END FBFL01 = 'Y'
     C     FBFL06        IFEQ      'Y'
     C                   MOVE      '0'           *IN94
     C                   ELSE                                                   ELSE FBFL01 = 'Y'
     C                   MOVE      '1'           *IN94
     C                   END                                                    END FBFL01 = 'Y'
     C*
     C     FBTP01        IFEQ      *BLANKS
     C     FBTP02        ANDEQ     *BLANKS
     C     FBTP03        ANDEQ     *BLANKS
     C     FBTP04        ANDEQ     *BLANKS
     C     FBTP05        ANDEQ     *BLANKS
     C     FBTP06        ANDEQ     *BLANKS
     C     FBTP07        ANDEQ     *BLANKS
     C     FBTP08        ANDEQ     *BLANKS
     C     FBTP09        ANDEQ     *BLANKS
     C     FBTP10        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN92
     C                   MOVE      '1'           *IN93
     C                   ELSE
     C                   MOVE      '0'           *IN92
     C                   MOVE      '0'           *IN93
     C                   END
     C                   END                                                    END *IN60 SFB01
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
     C* GET SUBSCRIBER NAME
     C*
     C                   MOVE      UPSUB#        SCSUB#
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         SCSUB#
     C                   MOVE      *BLANK        SCSBNM
     C                   ELSE
     C                   MOVEL     SMNAME        SCSBNM
     C                   END
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVEL     *BLANKS       SCFND
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C     Q2RECF        IFEQ      'SCRST'
     C                   EXFMT     SCRST
     C                   END
     C     Q2RECF        IFEQ      'SCRFL'
     C                   EXFMT     SCRFL
     C                   END
     C     Q2RECF        IFEQ      'SCRTX'
     C                   EXFMT     SCRTX
     C                   END
     C  N94Q2RECF        IFEQ      'SCR04'                                      CHG 11/13/97
     C                   EXFMT     SCR04
     C                   END
     C***********************************************************
     C*   PROCESS ALL COMMAND KEYS
     C     *IN11         IFEQ      '1'
     C**************************************
     C*   COMMAND 03 = END OF JOB          *
     C**************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C*************************
     C*  F6 = STATE SEARCH    *
     C*************************
     C     *INKF         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCRST'
     C                   CALL      'SY600R'
     C                   PARM                    SCST
     C                   PARM                    QQRTC             1
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCRST'       Q2RECF
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCST          OREQ      *BLANKS
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C                   END                                                    END *INKF
     C**************************************
     C*   COMMAND 12 = PRIOR SCREEN        *
     C**************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   ELSE
     C     Q2RECF        IFEQ      'SCRST'
     C  N94              MOVEL     'SCR04'       Q2RECF
     C   94              MOVEL     'SCR02'       Q2RECF
     C                   ELSE
     C     Q2RECF        IFEQ      'SCR04'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   ELSE
     C                   MOVEL     'SCRST'       Q2RECF
     C                   END
     C                   END
     C                   END
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C**************************************
     C*   FUNCTION 06                      *
     C**************************************
     C     *INKF         IFEQ      '1'
     C**************************************
     C*   COMMAND F6 = FUND SEARCH         *
     C**************************************
     C     Q2RECF        IFEQ      'SCR01'                                      ADD 03/22/94
     C                   CALL      'F0506R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCFND
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCFND         OREQ      *ZERO
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END EQ B/ZERO
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END                                                    END Q2RECF EQ 'SCR01
     C**************************************
     C*   COMMAND F6 = NWCD SEARCH         *
     C**************************************
     C     Q2RECF        IFEQ      'SCR04'                                      ADD 03/22/94
     C                   DO        10            A                 5 0
     C     A             ADD       39            A1                2 0
     C     A             ADD       39            A2                2 0
     C*
     C                   MOVEL     NW(A)         XX2               2
     C     XX2           IFEQ      '? '
     C     HFLD          OREQ      SNW(A)
     C*
     C                   MOVE      *BLANKS       QQNWCD            5
     C                   CALL      'N0505R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQNWCD
     C                   PARM                    QQRTC
     C                   MOVEL     QQNWCD        NW(A)
     C                   MOVE      *ON           *IN31
     C                   MOVE      *OFF          *IN30
     C                   MOVE      '1'           *IN(A1)                        POSIT CURSOR
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     NW(A)         OREQ      *ZERO
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   MOVE      *ON           *IN30
     C                   MOVE      *OFF          *IN31
     C                   MOVE      '1'           *IN(A2)                        POSIT CURSOR
     C                   GOTO      STRT
     C                   END                                                    END EQ B/ZERO
     C*
     C     QQRTC         IFEQ      '*'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   MOVE      *ON           *IN31
     C                   MOVE      *OFF          *IN30
     C                   MOVE      '1'           *IN(A1)                        POSIT CURSOR
     C                   GOTO      STRT
     C                   END                                                    END EQ *
     C                   END                                                    END ?
     C                   END                                                    END DO
     C                   END                                                    END Q2RECF EQ 'SCR02
     C*
     C                   END                                                    END *INKF
     C**************************************
     C*  COMMAND  9 - PROV ALPHA SEARCH    *
     C**************************************
     C     *INKI         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR04'                                      ADD 03/22/94
     C                   DO        10            A                 5 0
     C     A             ADD       39            A1                2 0
     C     A             ADD       39            A2                2 0
     C*
     C     HFLD          IFEQ      SNI(A)
     C*
     C                   Z-ADD     NI(A)         QQNIA
     C                   Z-ADD     NM(A)         QQNMA
     C                   CALL      'F0531R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQNIA             9 0
     C                   PARM                    QQNMA             3 0
     C                   PARM                    QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C     QQRTCD        IFEQ      'B'
     C     QQNIA         OREQ      *ZEROS
     C                   MOVE      QQNIA         SI(A)
     C                   Z-ADD     QQNMA         NM(A)
     C                   Z-ADD     QQNIA         NI(A)
     C                   MOVE      '1'           *IN34                          POS CURSOR PROV
     C                   MOVE      '1'           *IN(A1)
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   Z-ADD     A             Q
     C                   Z-ADD     QQNMA         NM(A)
     C                   Z-ADD     QQNIA         NI(A)
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PDNAME        NN(Q)
     C                   ELSE
     C                   MOVEL     *BLANKS       NN(Q)
     C                   ENDIF
     C                   MOVE      NI(A)         SI(A)
     C*
     C                   MOVE      '1'           *IN34                          POS CURSOR PRTY
     C                   MOVE      '1'           *IN(A1)
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDEQ '*'
     C*
     C                   ENDIF                                                  XX2 = ?
     C*
     C                   ENDDO                                                  DO 10
     C*
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  END EQ 'SCR04'
     C                   ENDIF                                                  END *INKI
     C*
     C*
     C**************************************
     C*  COMMAND 11 - PROVA SEARCH         *
     C**************************************
     C     *INKK         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR04'                                      ADD 03/22/94
     C                   DO        10            A                 5 0
     C     A             ADD       39            A1                2 0
     C     A             ADD       39            A2                2 0
     C*
     C     HFLD          IFEQ      SNI(A)
     C*
     C*
     C                   Z-ADD     NI(A)         QQNIA
     C                   Z-ADD     NM(A)         QQNMA
     C                   CALL      'F0533R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQNIA             9 0
     C                   PARM                    QQNMA             3 0
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C     QQRTCD        IFEQ      'B'
     C     QQNIA         OREQ      *ZEROS
     C                   Z-ADD     QQNMA         NM(A)
     C                   Z-ADD     QQNIA         NI(A)
     C                   MOVE      NI(A)         SI(A)
     C                   MOVE      '1'           *IN34                          POS CURSOR PROV
     C                   MOVE      '1'           *IN(A1)
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   Z-ADD     A             Q
     C                   Z-ADD     QQNMA         NM(A)
     C                   Z-ADD     QQNIA         NI(A)
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PDNAME        NN(Q)
     C                   ELSE
     C                   MOVEL     *BLANKS       NN(Q)
     C                   ENDIF
     C*
     C                   MOVE      NI(A)         SI(A)
     C                   MOVE      '1'           *IN34                          POS CURSOR PRTY
     C                   MOVE      '1'           *IN(A1)
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDEQ '*'
     C*
     C                   ENDIF                                                  XX2 = ?
     C*
     C                   ENDDO                                                  DO 10
     C*
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  END EQ 'SCR04'
     C                   ENDIF                                                  END *INKK
     C*
     C*
     C**************************************
     C*  COMMAND 13 - CHG USER PARAMETERS  *
     C**************************************
     C     *INKM         IFEQ      '1'
     C                   CALL      'F0006R'                                     ADD 01/25/93
     C                   WRITE     SCR99
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END                                                    END 1 *IN60
     C                   GOTO      STRT01
     C                   END                                                    END *INKM
     C*
     C                   GOTO      STRT
     C*
     C                   END                                                    END CMDKEYS
     C***********************************************************
     C*   GOTO THE APPROPRIATE SCREEN
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCRST'       SCRSTT
     C     Q2RECF        CABEQ     'SCRFL'       SCRFLT
     C     Q2RECF        CABEQ     'SCR04'       SCR04T
     C     Q2RECF        CABEQ     'SCRTX'       SCRTXT
     C*
     C                   GOTO      STRT
     C***************************************
     C*
     C***************************************
     C     SCR01T        TAG
     C*
     C*  FUND NUMBER MUST BE GREATER THAN ZERO
     C*
     C     SCFND         IFLE      *ZERO
     C                   MOVE      '1'           *IN20                          FUND REQ'D
     C                   GOTO      STRT
     C                   END
     C*
     C*  FIRST GOTO THE FUND MASTER FILE.
     C*  FUND NUMBER MUST BE A VALID NUMBER.
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              21        INVALID FUND
     C     *IN21         CABEQ     '1'           STRT
     C*
     C                   MOVEL     FNNAME        SCFNNM
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     F1KEY         CHAIN     PF101                              95
     C*
     C* UPDATE MODE
     C     *IN95         IFEQ      '0'
     C                   MOVE      UA(2)         SCMODE
     C                   Z-ADD     F25N01        SCCRA#
     C                   Z-ADD     F25N02        SCAGA#
     C                   MOVE      F29N01        SCMBI
     C                   MOVE      F1BRTC        SCBRTC
     C                   MOVE      F1OUTA        SCOUTA                         ADD 02/25/93
     C*** BEGIN ADD *** 01/29/97 **************************************
     C                   MOVE      F1FL04        SCFL04
     C*** END ADD *** 01/29/97 ****************************************
     C*
     C*** BEGIN DEL *** 02/06/97 **************************************
     C**
     C** THE FOLLOWING CODE WAS MOVED TO SCR02T TO ACCOMODATE
     C** 'F12=PREVIOUS' OPTION ON SCR04
     C**
     C**********           MOVE F1NW01    SCNW01
     C**********           MOVE F1NW02    SCNW02
     C**********           MOVE F1NW03    SCNW03
     C**********           MOVE F1NW04    SCNW04
     C**********           MOVE F1NW05    SCNW05
     C**********           MOVE F1NW06    SCNW06
     C**********           MOVE F1NW07    SCNW07
     C**********           MOVE F1NW08    SCNW08
     C**********           MOVE F1NW09    SCNW09
     C**********           MOVE F1NW10    SCNW10
     C**********           MOVE F1NF01    SCNF01
     C**********           MOVE F1NF02    SCNF02
     C**********           MOVE F1NF03    SCNF03
     C**********           MOVE F1NF04    SCNF04
     C**********           MOVE F1NF05    SCNF05
     C**********           MOVE F1NF06    SCNF06
     C**********           MOVE F1NF07    SCNF07
     C**********           MOVE F1NF08    SCNF08
     C**********           MOVE F1NF09    SCNF09
     C**********           MOVE F1NF10    SCNF10
     C*** END DEL *** 02/06/97 ****************************************
     C*
     C                   MOVE      F1FLG1        SCFLG1
     C                   MOVE      F1FLG2        SCFLG2
     C                   MOVE      F16N1         SCBRS
     C                   MOVEL     F1ANME        SCANAM
     C                   MOVEL     F1AAD1        SCAAD1
     C                   MOVEL     F1AAD2        SCAAD2
     C                   MOVEL     F1ACTY        SCACTY
     C                   MOVEL     F1AST         SCAST
     C                   MOVEL     F1AZIP        SCAZI1
     C                   MOVE      F1AZIP        SCAZI2
     C                   Z-ADD     F1AARA        SCAARE
     C                   Z-ADD     F1APHN        SCAPHN
     C*
     C*** BEGIN CHG *** 02/06/97 **************************************
     C                   MOVEA     F$            N$
     C                   MOVEA     FI            NI
     C                   MOVEA     FT            NT
     C                   MOVEA     FM            NM
     C*** END CHG *** 02/06/97 ****************************************
     C*
     C                   MOVE      FI            SI
     C*
     C*** GET THE PROV. NAMES FOR EACH PROVIDER TO DIAPLAY
     C                   DO        10            Q                 5 0
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PDNAME        NN(Q)
     C                   ELSE
     C                   MOVEL     *BLANKS       NN(Q)
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C* ADD MODE
     C                   MOVE      UA(1)         SCMODE
     C                   Z-ADD     *ZEROS        SCCRA#
     C                   Z-ADD     *ZEROS        SCAGA#
     C                   MOVE      *BLANKS       SCBRTC
     C                   MOVE      *BLANKS       SCOUTA                         ADD 02/25/93
     C*
     C*** BEGIN ADD *** 01/29/97 **************************************
     C                   MOVE      *BLANKS       SCFL04
     C*** END ADD *** 01/29/97 ****************************************
     C*
     C                   MOVE      *BLANKS       SCNW01
     C                   MOVE      *BLANKS       SCNW02
     C                   MOVE      *BLANKS       SCNW03
     C                   MOVE      *BLANKS       SCNW04
     C                   MOVE      *BLANKS       SCNW05
     C                   MOVE      *BLANKS       SCNW06
     C                   MOVE      *BLANKS       SCNW07
     C                   MOVE      *BLANKS       SCNW08
     C                   MOVE      *BLANKS       SCNW09
     C                   MOVE      *BLANKS       SCNW10
     C                   MOVE      *BLANKS       SCNF01
     C                   MOVE      *BLANKS       SCNF02
     C                   MOVE      *BLANKS       SCNF03
     C                   MOVE      *BLANKS       SCNF04
     C                   MOVE      *BLANKS       SCNF05
     C                   MOVE      *BLANKS       SCNF06
     C                   MOVE      *BLANKS       SCNF07
     C                   MOVE      *BLANKS       SCNF08
     C                   MOVE      *BLANKS       SCNF09
     C                   MOVE      *BLANKS       SCNF10
     C                   MOVE      *ZEROS        N$
     C                   MOVE      *ZEROS        NI
     C                   MOVE      *BLANKS       NT
     C                   MOVE      *ZEROS        NM
     C                   MOVE      *BLANKS       NN
     C                   MOVE      *ZEROS        SI
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C***************************************
     C* SCR02T - PROCESS RECORD             *
     C***************************************
     C     SCR02T        TAG
     C*
     C     *IN71         IFEQ      '1'
     C     SCOUTA        IFNE      'Y'
     C     SCOUTA        ANDNE     'N'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*** BEGIN ADD *** 01/29/97 **************************************
     C     SCFL04        IFNE      'Y'
     C     SCFL04        ANDNE     'N'
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*** END ADD *** 01/29/97 ****************************************
     C*
     C     SCFLG2        IFNE      'Y'
     C     SCFLG2        ANDNE     'N'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C     SCFLG1        IFNE      'Y'
     C     SCFLG1        ANDNE     'N'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C     SCFLG1        IFEQ      'Y'
     C     SCANAM        IFEQ      *BLANK
     C     SCAAD1        OREQ      *BLANK
     C     SCACTY        OREQ      *BLANK
     C                   WRITE     SCR02
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*** BEGIN ADD *** 02/06/97 **************************************
     C                   MOVE      F1NW01        SCNW01
     C                   MOVE      F1NW02        SCNW02
     C                   MOVE      F1NW03        SCNW03
     C                   MOVE      F1NW04        SCNW04
     C                   MOVE      F1NW05        SCNW05
     C                   MOVE      F1NW06        SCNW06
     C                   MOVE      F1NW07        SCNW07
     C                   MOVE      F1NW08        SCNW08
     C                   MOVE      F1NW09        SCNW09
     C                   MOVE      F1NW10        SCNW10
     C                   MOVE      *BLANKS       HLNW01
     C                   MOVE      *BLANKS       HLNW02
     C                   MOVE      *BLANKS       HLNW03
     C                   MOVE      *BLANKS       HLNW04
     C                   MOVE      *BLANKS       HLNW05
     C                   MOVE      *BLANKS       HLNW06
     C                   MOVE      *BLANKS       HLNW07
     C                   MOVE      *BLANKS       HLNW08
     C                   MOVE      *BLANKS       HLNW09
     C                   MOVE      *BLANKS       HLNW10
     C*
     C                   MOVE      F1NW01        HLNW01
     C                   MOVE      F1NW02        HLNW02
     C                   MOVE      F1NW03        HLNW03
     C                   MOVE      F1NW04        HLNW04
     C                   MOVE      F1NW05        HLNW05
     C                   MOVE      F1NW06        HLNW06
     C                   MOVE      F1NW07        HLNW07
     C                   MOVE      F1NW08        HLNW08
     C                   MOVE      F1NW09        HLNW09
     C                   MOVE      F1NW10        HLNW10
     C                   MOVE      F1NF01        SCNF01
     C                   MOVE      F1NF02        SCNF02
     C                   MOVE      F1NF03        SCNF03
     C                   MOVE      F1NF04        SCNF04
     C                   MOVE      F1NF05        SCNF05
     C                   MOVE      F1NF06        SCNF06
     C                   MOVE      F1NF07        SCNF07
     C                   MOVE      F1NF08        SCNF08
     C                   MOVE      F1NF09        SCNF09
     C                   MOVE      F1NF10        SCNF10
     C*** END ADD *** 02/06/97 ****************************************
     C  N94              MOVEL     'SCR04'       Q2RECF
     C   94              MOVEL     'SCRST'       Q2RECF
     C                   GOTO      STRT
     C*
     C***************************************
     C* SCRSTT - STATE PROMPT               *
     C***************************************
     C     SCRSTT        TAG
     C*
     C*  THE STATE MUST BE VALID
     C*
     C     SCST          IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C* INCREASING NUMBER OF STATES,  CALL CHKSTR IF EXISTS
     C*
     C                   MOVEL     SCST          QQSTT
     C                   CALL      'CHKSTR'                             99
     C                   PARM                    QQSTT             2
     C                   PARM                    QQSTRT            1
     C                   PARM                    QQFLRT            3
     C     *IN99         IFEQ      '0'
     C     QQSTRT        IFEQ      '0'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   ELSE
     C     SCST          LOOKUP    ST                                     60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   ENDIF                                                  END LOKUP ST
     C                   ENDIF                                                  END LOKUP ST
     C                   ELSE
     C*
     C     SCST          LOOKUP    ST2                                    60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   ELSE
     C     SCST          LOOKUP    ST                                     60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   ENDIF                                                  END *IN99
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C     'SCR'         CAT       SCST          Q2RECF
     C     F2KY1         CHAIN     PF201                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      F25N01        SCCRA#
     C                   MOVE      F29N01        SCCRFE
     C                   MOVE      F29N02        SCCRZP
     C                   MOVE      F25N02        SCAGA#
     C                   MOVE      F29N03        SCAGFE
     C                   MOVE      F29N04        SCAGZP
     C                   MOVE      F29N01        SCMBI
     C                   END
     C                   GOTO      STRT
     C*
     C***************************************
     C* SCRFLT - FL STATE TAPE INFO         *
     C***************************************
     C     SCRFLT        TAG
     C*
     C*  THE CARRIER AUDIT#, FEIN AND ZIP ARE REQUIRED
     C*
     C     SCCRA#        IFEQ      *ZERO
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C     SCCRFE        IFEQ      *ZERO
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C     SCCRZP        IFEQ      *ZERO
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF THE AGENT AUDIT# IS ENTERED, FEIN AND ZIP ARE REQUIRED
     C*
     C     SCAGA#        IFNE      *ZERO
     C     SCAGFE        IFEQ      *ZERO
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C     SCAGZP        IFEQ      *ZERO
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  NEXT SCREEN
     C*
     C   94              MOVE      Q1USER        F2IUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F2IDT
     C*
     C   94              Z-ADD     DSTIME        F2ITIM
     C                   MOVE      F1STAT        F2STAT
     C                   MOVE      Q1PGM         F2PGM
     C                   MOVE      Q1USER        F2USER
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F2DATE
     C*
     C                   Z-ADD     DSTIME        F2TIME
     C                   Z-ADD     UPCO#         F2CO#
     C                   Z-ADD     UPSUB#        F2SUB#
     C                   MOVEL     SCFND         F2FND
     C                   MOVEL     SCST          F2ST
     C                   Z-ADD     SCCRA#        F25N01
     C                   Z-ADD     SCCRFE        F29N01
     C                   Z-ADD     SCCRZP        F29N02
     C                   Z-ADD     SCAGA#        F25N02
     C                   Z-ADD     SCAGFE        F29N03
     C                   Z-ADD     SCAGZP        F29N04
     C     F2KY1         CHAIN     PF201                              94
     C  N94              UPDATE    PF201
     C   94              WRITE     PF201
     C*
     C                   MOVE      *BLANKS       SCST
     C                   MOVEL     'SCRST'       Q2RECF
     C                   GOTO      STRT
     C*
     C*
     C***************************************
     C* SCRTXT - TX STATE TAPE INFO         *
     C***************************************
     C     SCRTXT        TAG
     C*
     C*  THE MASTER BUSINESS IDENTIFIER IS REQUIRED
     C*
     C     SCMBI         IFEQ      *ZERO
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  NEXT SCREEN
     C     F2KY1         CHAIN     PF201                              94
     C   94              MOVE      Q1USER        F2IUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F2IDT
     C*
     C   94              Z-ADD     DSTIME        F2ITIM
     C                   MOVE      F1STAT        F2STAT
     C                   MOVE      Q1PGM         F2PGM
     C                   MOVE      Q1USER        F2USER
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F2DATE
     C*
     C                   Z-ADD     DSTIME        F2TIME
     C                   Z-ADD     UPCO#         F2CO#
     C                   Z-ADD     UPSUB#        F2SUB#
     C                   MOVEL     SCFND         F2FND
     C                   MOVEL     SCST          F2ST
     C                   MOVE      SCMBI         F29N01
     C  N94              UPDATE    PF201
     C   94              WRITE     PF201
     C*
     C                   MOVE      *BLANKS       SCST
     C                   MOVEL     'SCRST'       Q2RECF
     C                   GOTO      STRT
     C*
     C***************************************
     C* SCR04  - SCREEN 4                   *
     C***************************************
     C     SCR04T        TAG
     C     *IN94         IFEQ      '0'
     C*
     C* IF NETWORK CODE IS ENTERED NETWORK FLAG MUST BE ENTERED
     C*
     C                   DO        10            A                 5 0
     C     A             ADD       81            A1                2 0
     C     A             ADD       81            A2                2 0
     C*
     C     NW(A)         IFNE      *BLANKS
     C     NF(A)         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           *IN(A1)
     C                   GOTO      STRT
     C                   END                                                    END NF,A =BLANKS
     C                   END                                                    END NF,A =BLANKS
     C*
     C     NF(A)         IFNE      *BLANKS
     C     NF(A)         ANDNE     'N'
     C     NF(A)         ANDNE     'Y'
     C     NF(A)         ANDNE     'I'
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           *IN(A1)
     C                   GOTO      STRT
     C                   END                                                    END NF,A =BLANKS
     C*
     C     NF(A)         IFNE      *BLANKS
     C     NW(A)         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN25
     C                   MOVE      '1'           *IN(A2)
     C                   GOTO      STRT
     C                   END                                                    END NF,A =BLANKS
     C                   END                                                    END NF,A =BLANKS
     C*
     C     NW(A)         IFNE      *BLANKS
     C     NWKEY         CHAIN     PNW01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   MOVE      '1'           *IN(A2)
     C                   GOTO      STRT
     C                   END                                                    END *IN60 EQ '1'
     C                   END                                                    END NW,A =BLANKS
     C*** SCN$ ONLY ALLOWED IF NF = N OR Y
     C     NF(A)         IFNE      'N'
     C     NF(A)         ANDNE     'Y'
     C     N$(A)         IFNE      *ZEROS
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           *IN(A2)
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*** SCNT REQUIRED IF NF = N OR Y
     C     NF(A)         IFEQ      'N'
     C     NF(A)         OREQ      'Y'
     C     NT(A)         IFEQ      *BLANK
     C                   MOVE      '1'           *IN27
     C                   MOVE      '1'           *IN(A2)
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*** SCNT MUST BE VALID
     C     NT(A)         IFNE      *BLANK
     C     NT(A)         IFNE      'B'
     C     NT(A)         ANDNE     'N'
     C     NT(A)         ANDNE     'R'
     C     NT(A)         ANDNE     'S'
     C                   MOVE      '1'           *IN27
     C                   MOVE      '1'           *IN(A2)
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*** SCNT =B NF MUST BE 'N' AND N$ >0
     C     NT(A)         IFEQ      'B'
     C     NF(A)         IFNE      'N'
     C                   MOVE      '1'           *IN27
     C                   MOVE      '1'           *IN(A2)
     C                   GOTO      STRT
     C                   ENDIF
     C     N$(A)         IFLE      *ZERO
     C                   MOVE      '1'           *IN27
     C                   MOVE      '1'           *IN(A2)
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*** SCNT =N NF MUST BE 'N' AND N$ =0
     C     NT(A)         IFEQ      'N'
     C     NF(A)         IFNE      'N'
     C                   MOVE      '1'           *IN27
     C                   MOVE      '1'           *IN(A2)
     C                   GOTO      STRT
     C                   ENDIF
     C     N$(A)         IFNE      *ZERO
     C                   MOVE      '1'           *IN27
     C                   MOVE      '1'           *IN(A2)
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C*** SCNT =R NF MUST BE 'Y' AND N$ =0
     C     NT(A)         IFEQ      'R'
     C     NF(A)         IFNE      'Y'
     C                   MOVE      '1'           *IN27
     C                   MOVE      '1'           *IN(A2)
     C                   GOTO      STRT
     C                   ENDIF
     C     N$(A)         IFNE      *ZERO
     C                   MOVE      '1'           *IN27
     C                   MOVE      '1'           *IN(A2)
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*** SCNT =S NF MUST BE 'N' AND N$ >0
     C     NT(A)         IFEQ      'S'
     C     NF(A)         IFNE      'N'
     C                   MOVE      '1'           *IN27
     C                   MOVE      '1'           *IN(A2)
     C                   GOTO      STRT
     C                   ENDIF
     C     N$(A)         IFLE      *ZERO
     C                   MOVE      '1'           *IN27
     C                   MOVE      '1'           *IN(A2)
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C     NF(A)         IFEQ      'Y'
     C     NF(A)         OREQ      'N'
     C     NI(A)         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN28
     C                   MOVE      '1'           *IN(A2)
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*** SCNI MUST BE VALID, CHECK THE PDP FILE
     C     NI(A)         IFNE      *ZEROS
     C                   Z-ADD     A             Q
     C*
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN28
     C                   MOVEL     '1'           *IN(A2)
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   END                                                    END DO 10
     C*
     C                   END                                                    END *IN94 EQ '0'
     C*
     C*** BEGIN ADD *** 02/06/97 **************************************
     C*
     C*  PREVENT THE REMOVAL OF A NETWORK CODE THAT IS ASSIGNED
     C*  IN EMPLOYER MASTER
     C*
     C                   Z-ADD     0             X                 2 0
     C                   MOVE      '0'           FNFLG             1
     C*
     C     1             DO        10            X
     C                   Z-ADD     0             Y                 2 0
     C     HLNW(X)       IFNE      *BLANKS
     C*
     C     1             DO        10            Y
     C     HLNW(X)       IFEQ      NW(Y)
     C                   MOVE      '1'           FNFLG
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO                                                  Y DO LOOP
     C*
     C     FNFLG         IFEQ      '0'
     C                   MOVE      HLNW(X)       XXNWCD
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN92
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      '0'           *IN92
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  FNFLG IFEQ
     C*
     C                   MOVE      '0'           FNFLG
     C                   ENDIF                                                  HLNW,X IFNE
     C*
     C                   ENDDO                                                  X DO LOOP
     C*
     C*** END ADD *** 02/06/97 ****************************************
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C     F1KEY         CHAIN     PF101                              95
     C                   Z-ADD     UPCO#         F1CO#
     C                   Z-ADD     UPSUB#        F1SUB#
     C                   MOVEL     SCFND         F1FND
     C                   MOVE      SCBRTC        F1BRTC
     C                   MOVE      *BLANK        F1STAT
     C                   MOVE      SCOUTA        F1OUTA                         ADD 02/25/93
     C                   MOVE      *BLANKS       F1NTWK                         ADD 03/22/94
     C                   MOVE      *BLANKS       F1FLG1                         ADD 02/25/93
     C                   MOVE      *BLANKS       F1FLG2                         ADD 02/25/93
     C                   MOVE      *BLANKS       F1FLG3                         ADD 02/25/93
     C                   MOVE      *BLANKS       F1FLG4                         ADD 02/25/93
     C                   MOVE      *BLANKS       F1FLG5                         ADD 02/25/93
     C*** BEGIN ADD *** 01/29/97 **************************************
     C                   MOVE      SCFL04        F1FL04
     C*** END ADD *** 01/29/97 ****************************************
     C                   MOVE      SCNW01        F1NW01
     C                   MOVE      SCNW02        F1NW02
     C                   MOVE      SCNW03        F1NW03
     C                   MOVE      SCNW04        F1NW04
     C                   MOVE      SCNW05        F1NW05
     C                   MOVE      SCNW06        F1NW06
     C                   MOVE      SCNW07        F1NW07
     C                   MOVE      SCNW08        F1NW08
     C                   MOVE      SCNW09        F1NW09
     C                   MOVE      SCNW10        F1NW10
     C                   MOVE      SCNF01        F1NF01
     C                   MOVE      SCNF02        F1NF02
     C                   MOVE      SCNF03        F1NF03
     C                   MOVE      SCNF04        F1NF04
     C                   MOVE      SCNF05        F1NF05
     C                   MOVE      SCNF06        F1NF06
     C                   MOVE      SCNF07        F1NF07
     C                   MOVE      SCNF08        F1NF08
     C                   MOVE      SCNF09        F1NF09
     C                   MOVE      SCNF10        F1NF10
     C*
     C                   MOVEL     SCFLG1        F1FLG1
     C                   MOVE      SCFLG2        F1FLG2
     C                   MOVE      SCBRS         F16N1
     C                   MOVEL     SCANAM        F1ANME
     C                   MOVEL     SCAAD1        F1AAD1
     C                   MOVEL     SCAAD2        F1AAD2
     C                   MOVEL     SCACTY        F1ACTY
     C                   MOVEL     SCAST         F1AST
     C                   MOVEL     SCAZI1        F1AZIP
     C                   MOVE      SCAZI2        F1AZIP
     C                   Z-ADD     SCAARE        F1AARA
     C                   Z-ADD     SCAPHN        F1APHN
     C*
     C                   MOVEA     N$            F$
     C                   MOVEA     NI            FI
     C                   MOVEA     NT            FT
     C                   MOVEA     NM            FM
     C*
     C                   MOVEL     Q1USER        F1USER
     C                   MOVEL     Q1PGM         F1PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        F1TIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F1DT
     C*
     C     *IN95         IFEQ      '0'
     C                   UPDATE    PF101
     C                   ELSE
     C                   MOVEL     Q1USER        F1IUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F1IDT
     C*
     C                   WRITE     PF101
     C                   END
     C*
     *****  IF THERE ARE NO VALID STATES IN FBTP01-10/FILE SFBPF
     *****  RETURN TO SCR01 OHTERWISE GOTO SCRST
     C                   MOVE      *BLANKS       SCST
     C     *IN93         IFEQ      '1'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   ELSE
     C                   MOVEL     'SCRST'       Q2RECF
     C                   END
     C*
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *========================================================================*
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE ST
FL
TX
**
SCNW01
SCNW02
SCNW03
SCNW04
SCNW05
SCNW06
SCNW07
SCNW08
SCNW09
SCNW10
**
SCNF01
SCNF02
SCNF03
SCNF04
SCNF05
SCNF06
SCNF07
SCNF08
SCNF09
SCNF10
**
SCN$01
SCN$02
SCN$03
SCN$04
SCN$05
SCN$06
SCN$07
SCN$08
SCN$09
SCN$10
**
SCNT01
SCNT02
SCNT03
SCNT04
SCNT05
SCNT06
SCNT07
SCNT08
SCNT09
SCNT10
**
SCNI01
SCNI02
SCNI03
SCNI04
SCNI05
SCNI06
SCNI07
SCNI08
SCNI09
SCNI10
**
SCNM01
SCNM02
SCNM03
SCNM04
SCNM05
SCNM06
SCNM07
SCNM08
SCNM09
SCNM10
