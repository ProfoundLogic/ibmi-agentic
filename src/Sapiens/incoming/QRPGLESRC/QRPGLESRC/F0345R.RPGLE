     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE F0345R - EOMB Batch Print - Prompt Program
     H*****************************************************************
     H*                                                               *
     H* CALLED FROM: MENU                                             *
     H* CALLS TO:    F0345C - EOMB Batch Print - Submitter            *
     H*                                                               *
     H*****************************************************************
     H* Created on 09/10/01 by C:62 for Project:FS231                 *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* GLOBAL                                                        *
     H*  99    General Chain                                          *
     H*                                                               *
     H* SCR01                                                         *
     H*  71    MCR0500 - Invalid Batch Date                           *
     H*  72    MCR0513 - Invalid Table value                          *
     H*                                                               *
     H*****************************************************************
     FF0345F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*
     F* System company
     FSMCOL     IF   E           K DISK
     F*
     F* System user parm
     FSMUPL     IF   E           K DISK
     F*

     D OPTNM           C                   CONST('EOMB Batch Reprint')
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  Company Master File
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*  User Parameter File
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
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
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL(P)  OPTNM         SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS           40
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
     C                   ENDIF
     C**************************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*****************************************************************
     C     *IN11         IFEQ      '1'
     C***********************************
     C*   F3 = EXIT                     *
     C***********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  END *INKC
     C***********************************
     C*   *IN10 = PROCESS HELP REQUESTS *
     C***********************************
     C     *IN10         IFEQ      '1'
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
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  END *IN10
     C*
     C                   ENDIF                                                  END CMDKEYS
     C**************************************************************************
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C* Validate Batch date
     C*
     C                   Z-ADD     SCSDT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSDT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        71
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* Validate Spool file type
     C*
     C     SCTYPE        IFNE      '1'
     C     SCTYPE        ANDNE     '2'
     C     SCTYPE        ANDNE     '3'
     C     SCTYPE        ANDNE     'A'
     C                   SETON                                        72
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*   CALL THE CLP TO RUN THE LIST.
     C*
     C                   MOVE      UPCO#         ##CO#
     C                   Z-ADD     SCSDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         ##DATE
     C                   MOVEL     SCTYPE        ##TYPE
     C                   MOVEL     UPOUTQ        ##QUE
     C                   MOVE      'B'           ##MODE
     C*
     C                   CALL      'F0345C'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##DATE            8
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##QUE            10
     C                   PARM                    ##MODE            1
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
