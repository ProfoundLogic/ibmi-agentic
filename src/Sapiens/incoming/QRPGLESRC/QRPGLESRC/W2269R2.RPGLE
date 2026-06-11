     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*
     H*      W2269R2 - EDI 148/A49 ERROR MESSAGE TABLE SEARCH
     H*
     H******************************************************************
     H* PROGRAM DESCIPTION:  THIS PROGRAM DISPLAYS THE ERROR MESSAGE  *
     H*    LIST FOR THE SELECTED CO# WHEN CALLED BY W2069R2           *
     H*                                                               *
     H* ### SEE IMPORTANT INFO IN W2069R2 FOR OTHER CLIENT'S MODS ### *
     H*                                                               *
     H*****************************************************************
     H*  CALLED BY: W2069R2                                           *
     H*  CALLS TO: NONE                                               *
     H*                                                               *
     H*  CREATED: 06/18/97  for Version 3 BASE
     H*                    C:58  PROJECT: WC468
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  04/10/12 C:15  PROJECT: SY266D                               *
     H*                 Remove second subfile from screen.            *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* 09 - INQUIRY MODE(NO DELETE)                                  *
     H* 17 - ROLL DOWN                                                *
     H* 33 - Record selected for deletion                             *
     H* 60 - GENERAL CHAIN INDICATOR                                  *
     H*                                                               *
     H*****************************************************************
     H* FILE USAGE:                                                   *
     H*                                                               *
     H*  W2269F2: WORKSTATION FILE                                    *
     H*  WTERL:  (PER01) CO#/MSG#                                     *
     H*  SMCOL:  (PCO01) CO#                                          *
     H*  WMUPL:  (PUP01) USR                                          *
     H*                                                               *
     H*****************************************************************
     FW2269F2   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F*
     FWTERL     UF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     D OPTNM           C                   CONST('EDI Error Codes-
     D                                      Information')
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
     C*
     C*   KEY LIST FOR THE DATA ELEMENT CRITERIA TABLE
     C*
     C     ERKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXMSG#
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
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQMSG#            3 0
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
     C     *LIKE         DEFINE    ERCO#         XXCO#
     C     *LIKE         DEFINE    ERMSG#        XXMSG#
     C     *LIKE         DEFINE    ERSTAT        XXDLTF
     C     *LIKE         DEFINE    SCMODE        XXMODE
     C*
     C*   CHECK FOR INQUIRY MODE
     C*
     C     QQCALL        COMP      '1'                                    09
     C     *IN09         IFEQ      *ON
     C                   MOVE      *BLANKS       SCMODE
     C                   ENDIF
     C*
     C*   GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C   60              MOVE      *ZERO         XXCO#
     C                   MOVE      UPCO#         XXCO#
     C     COKEY         CHAIN     PCO01                              60
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQCO#         XXCO#
     C                   MOVEL     QQMSG#        XXMSG#
     C*
     C                   Z-ADD     1             RRN2              5 0
     C                   Z-ADD     0             SVRRN             5 0
     C                   Z-ADD     0             SFRRN
     C*
     C*          =============
     C     TOP           TAG
     C*          =============
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
     C*   F5 = REFRESH
     C*
     C     *INKE         IFEQ      *ON
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     0             XXMSG#
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   ENDIF
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
     C     *IN17         IFEQ      *ON                                          ROLLUP
     C     *IN75         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN11
     C*
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C*
     C     SCMSG#        IFNE      *ZEROS
     C                   Z-ADD     SCMSG#        XXMSG#
     C     ERKY          SETLL     PER01
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     *ZEROS        SCMSG#
     C                   GOTO      TOP
     C                   ENDIF                                                  END REPOSITION LKS
     C*
     C*================================================================*
     C*   Process changes in subfile
     C                   READC     SCR01S                                 50
     C     *IN50         DOWEQ     *OFF
     C*
     C*================================================================*
     C*   Option 1 -- SEARCH SELECTION
     C*
     C     SFSEL         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANK        QQMSG#
     C                   MOVEL     SFMSG#        QQMSG#
     C                   GOTO      TERM
     C                   ENDIF                                                  SEL  EQ '1'
     C*
     C*================================================================*
     C*   Option 4 -- DELETE REQUEST
     C*
     C     SFSEL         IFEQ      '4'
     C*
     C                   MOVE      *ON           *IN33                          Enter again
     C                   UPDATE    SCR01S
     C*
     C                   MOVE      *ON           *IN71
     C                   Z-ADD     SFRRN         SVRRN
     C                   Z-ADD     SFRRN         #@$2
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      *OFF          *IN71
     C*
     C     *INKE         IFEQ      *ON                                          Refresh
     C                   LEAVE
     C                   ENDIF
     C*
     C     SVRRN         CHAIN     SCR01S                             50
     C                   MOVE      *BLANK        SFSEL
     C*
     C     *INKL         IFEQ      *OFF                                         No cancel,
     C                   MOVEL     SFMSG#        XXMSG#
     C     ERKY          DELETE    PER01                              50        delete it
     C                   ENDIF
     C*
     C                   UPDATE    SCR01S
     C                   ENDIF                                                  e SFSEL=4
     C*
     C                   READC     SCR01S                                 50
     C                   ENDDO
     C*
     C                   Z-ADD     1             RRN2
     C                   EXSR      LOAD
     C                   GOTO      STRT
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
     C*     Rebuild subfile
     C     RRN2          IFEQ      1
     C*
     C*     Clear subfile
     C                   MOVE      *ON           *IN72
     C                   WRITE     SCR01C
     C                   MOVE      *OFF          *IN72
     C*
     C*     Position database
     C     ERKY          SETLL     PER01
     C                   Z-ADD     0             RRN2
     C                   MOVE      *OFF          *IN75
     C*
     C                   ELSE
     C     *IN60         CABEQ     *ON           ENLOAD
     C                   ENDIF
     C*
     C*     Load 24 records into subfile
     C                   DO        24            X                 5 0
     C*
     C                   READ      PER01                                  60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN75
     C                   LEAVE
     C                   ELSE
     C*
     C                   ADD       1             RRN2
     C     X             IFEQ      1
     C                   Z-ADD     RRN2          POS2              5 0
     C                   ENDIF
     C                   MOVE      ERMSG#        SFMSG#
     C                   MOVEL     ERDESC        SFDESC
     C                   Z-ADD     RRN2          SFRRN
     C                   WRITE     SCR01S
     C                   ENDIF
     C                   ENDDO
     C*
     C*     INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     *ON
     C                   MOVE      *BLANKS       SFMSG#
     C                   MOVE      *BLANKS       SFDESC
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
     C     ENLOAD        ENDSR
