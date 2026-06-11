     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W2269R3 - EDI 148/A49 EDIT MATRIX TABLE SEARCH
     H*
     H******************************************************************
     H* PROGRAM DESCIPTION:  THIS PROGRAM DISPLAYS THE EDIT MATRIX    *
     H*    LIST FOR THE SELECTED CO#/JST/DN#/SET/MTC WHEN CALLED BY   *
     H*    W2069R3                                                    *
     H*                                                               *
     H*  CREATED: 06/18/97  for Version 3 BASE
     H*                    C:58  PROJECT: WC468
     H*                                                               *
     H* ### SEE IMPORTANT INFO IN W2069R2 FOR OTHER CLIENT'S MODS ### *
     H*                                                               *
     H*****************************************************************
     H*  CALLED BY: W2069R3                                           *
     H*  CALLS TO: NONE                                               *
     H*                                                               *
     H*    PROGRAMMER: C:49                                           *
     H*    CREATED:    02/02/96                                       *
     H*    PROJECT#:   68AMN  EDI 148/A49 EDIT MATRIX TABLE           *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* 17 - ROLL DOWN                                                *
     H* 18 - ROLL UP                                                  *
     H* 32 - ROLL REQUEST BEYOND EOF                                  *
     H* 60 - GENERAL CHAIN INDICATOR                                  *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H* FILE USAGE:                                                   *
     H*                                                               *
     H*  W2269F3: WORKSTATION FILE                                    *
     H*  WTEML:  (PEM01) CO#/JST/DN#/SET/MTC/MSG#                     *
     H*  SMCOL:  (PCO01) CO#                                          *
     H*  WMUPL:  (PUP01) USR                                          *
     H*                                                               *
     H*****************************************************************
     FW2269F3   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F*
     FWTEML     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     I*   KEY LIST FOR THE EDIT MATRIX TABLE
     I*

     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXDN#
     C                   KFLD                    XXSET
     C                   KFLD                    XXMTC
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
     C*----------------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQJST             2
     C                   PARM                    QQDN#             5 0
     C                   PARM                    QQSET             3
     C                   PARM                    QQMTC             2
     C                   PARM                    QQRTCD            1
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
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMJST         XXJST
     C     *LIKE         DEFINE    EMDN#         XXDN#
     C     *LIKE         DEFINE    EMSET         XXSET
     C     *LIKE         DEFINE    EMMTC         XXMTC
     C     *LIKE         DEFINE    EMJST         SVJST
     C     *LIKE         DEFINE    EMDN#         SVDN#
     C     *LIKE         DEFINE    EMSET         SVSET
     C     *LIKE         DEFINE    EMMTC         SVMTC
     C*
     C*   GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         XXCO#
     C                   ENDIF
     C                   MOVE      UPCO#         XXCO#
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      QQCO#         XXCO#
     C                   MOVEL     QQJST         XXJST
     C                   MOVE      QQDN#         XXDN#
     C                   MOVE      QQSET         XXSET
     C                   MOVE      QQMTC         XXMTC
     C*
     C                   Z-ADD     0             RRN2              5 0
     C                   Z-ADD     0             SFRRN
     C*
     C*          =============
     C     TOP           TAG
     C*          =============
     C     EMKY          SETLL     PEM01                              60
     C*
     C                   MOVE      *ON           *IN71
     C                   MOVE      *ON           *IN72
     C                   WRITE     SCR01
     C                   WRITE     SCR01C
     C                   MOVE      *OFF          *IN71
     C                   MOVE      *OFF          *IN72
     C*
     C                   EXSR      LOAD
     C*          =============
     C     STRT          TAG
     C*          =============
     C                   MOVEL     Q1USER        SCUSER
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      *ON           *IN71
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      *OFF          *IN71
     C                   ENDIF
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*                                                                **
     C     *IN11         IFEQ      *ON
     C*
     C*   F3 = EXIT
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *ON           *INLR                          GOTO STRT
     C                   GOTO      TERM
     C                   ENDIF                                                  END *INKC
     C*
     C*   F12 = CANCEL
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*   *IN17 -  ROLLDOWN REQUEST
     C*
     C     *IN17         IFEQ      *ON                                          ROLLDOWN
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   Z-ADD     POS2          RRN2
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*   *IN18 -  ROLLUP REQUEST
     C*
     C     *IN18         IFEQ      *ON                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   Z-ADD     POS2          RRN2
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C*
     C     SCJST         IFNE      *BLANKS
     C     SCDN#         ORNE      *ZEROS
     C*
     C     SCJST         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXJST
     C                   MOVEL     SCJST         XXJST
     C                   MOVE      *BLANKS       SCJST
     C                   ENDIF                                                  END SCJST  NE BLKS
     C     SCDN#         IFNE      *ZEROS
     C                   Z-ADD     *ZEROS        XXDN#
     C                   MOVE      SCDN#         XXDN#
     C                   Z-ADD     *ZEROS        SCDN#
     C                   ENDIF                                                  END SCDN#  NE BLKS
     C*
     C                   Z-ADD     0             RRN2
     C                   GOTO      TOP
     C                   ENDIF                                                  END REPOSITION LKS
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C                   READC     SCR01S                                 50
     C     *IN50         DOWEQ     *OFF
     C     SFSEL         IFEQ      '1'
     C                   Z-ADD     SFRRN         RRN2
     C     RRN2          CHAIN     SCR01S                             50
     C     *IN50         IFEQ      *OFF
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANK        QQJST
     C                   MOVEL     SFJST         QQJST
     C                   MOVE      *BLANK        QQDN#
     C                   MOVE      SFDN#         QQDN#
     C                   MOVE      *BLANK        QQSET
     C                   MOVEL     SFSET         QQSET
     C                   MOVE      *BLANK        QQMTC
     C                   MOVEL     SFMTC         QQMTC
     C                   GOTO      TERM
     C                   ENDIF                                                  ELSE EQ '1'
     C*
     C                   MOVE      *BLANK        SFSEL
     C                   ENDIF
     C*
     C                   READC     SCR01S                                 50
     C                   ENDDO
     C*
     C*          =============
     C     TERM          TAG
     C*          =============
     C                   MOVE      '1'           *INLR
     C*
     C                   RETURN
     C*
     C*================================================================
     C*   LOAD SUBROUTINE
     C*================================================================
     C     LOAD          BEGSR
     C*
     C                   MOVE      *OFF          *IN32
     C*
     C                   DO        14            X                 5 0
     C*
     C                   READ      PEM01                                  60
     C*
     C     *IN60         IFEQ      *ON
     C     X             ANDEQ     1
     C                   MOVE      *ON           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      *OFF
     C                   ADD       1             RRN2
     C     X             IFEQ      1
     C                   Z-ADD     RRN2          POS2              5 0
     C                   MOVE      EMJST         SVJST
     C                   MOVE      EMDN#         SVDN#
     C                   MOVE      EMSET         SVSET
     C                   MOVEL     EMMTC         SVMTC
     C                   END
     C*
     C*  ONLY ONE RECORD PER UNIQUE KEY
     C     X             IFGT      1
     C                   EXSR      SRSORT
     C                   ENDIF
     C*
     C                   MOVE      EMJST         SFJST
     C                   MOVE      EMDN#         SFDN#
     C                   MOVE      EMSET         SFSET
     C                   MOVEL     EMMTC         SFMTC
     C                   Z-ADD     RRN2          SFRRN
     C                   MOVE      *ON           *IN71
     C                   WRITE     SCR01S
     C                   MOVE      *OFF          *IN71
     C                   ELSE
     C                   MOVE      *ON           *IN75
     C                   END
     C                   END
     C*
     C*     INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     *ON
     C                   MOVE      *BLANKS       SFJST
     C                   Z-ADD     *ZEROS        SFDN#
     C                   MOVE      *BLANKS       SFSET
     C                   MOVE      *BLANKS       SFMTC
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     1             POS2
     C                   Z-ADD     1             SFRRN
     C                   MOVE      *ON           *IN71
     C                   WRITE     SCR01S
     C                   MOVE      *OFF          *IN71
     C                   END
     C*
     C*   TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     POS2          #@$2
     C                   Z-ADD     RRN2          POS2
     C*
     C                   ENDSR
     C*================================================================
     C*   SRSORT -- SORT RECORDS AND DISPLAY ONLY ONE PER UNIQUE KEY
     C*================================================================
     C     SRSORT        BEGSR
     C*
     C                   MOVE      '00000'       *IN80
     C     *IN80         DOWEQ     *OFF
     C*
     C     EMJST         COMP      SVJST                                  81    JST = SVJST
     C     EMDN#         COMP      SVDN#                                  82    DN# = SVDN#
     C     EMSET         COMP      SVSET                                  83    SET = SVSET
     C     EMMTC         COMP      SVMTC                                  84    MTC = SVMTC
     C                   MOVEA     *IN(80)       CHECK             5
     C*
     C     CHECK         IFNE      '01111'
     C                   MOVE      EMJST         SVJST
     C                   MOVE      EMDN#         SVDN#
     C                   MOVE      EMSET         SVSET
     C                   MOVE      EMMTC         SVMTC
     C                   LEAVE
     C                   ENDIF
     C*
     C                   READ      PEM01                                  80
     C                   ENDDO
     C*
     C                   ENDSR
