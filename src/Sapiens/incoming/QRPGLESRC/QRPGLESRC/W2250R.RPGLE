     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W2250R - DEPARTMENT CODE MAINTENANCE
     H*****************************************************************
     H*  MODIFICATIONS:                                               *
     H*                                                               *
     H*  11/28/94  C:30  PROJECT:  WC376                              *
     H*                  ALLOW F23 TO DELETE DEPARTMENT CODE IF       *
     H*                  STATUS IS 'I'.                               *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  VARIABLE  USAGE:                                             *
     H*    QQRTCD - USED TO RETURN VALUE TO W4115R                    *
     H*           A = F3 (EXIT)                                       *
     H*           B = F12 (CANCEL)                                    *
     H*           * = NEW DEPT ADDED                                  *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  IND     FUNCTION DESCRIPTION                                 *
     H*   09     INQUIRY MODE                                         *
     H*   99     CALL FROM W4115F - ADD FOR DEPT LETTERS GENERATION   *
     H*  SCR01                                                        *
     H*   20     WCR0503 - DEPT CODE CAN'T BE BLANK                   *
     H*   21     WCR0505 - CAN'T INQUIRY ON A CODE THAT DOESN'T EXIST *
     H*   22     WCR0823 - IF CALL FROM W4115F, CAN'T ADD EXISTING DEPT
     H*  SCR02                                                        *
     H*   20     WCR0503 - DEPT NAME CAN'T BE BLANK                   *
     H*****************************************************************
     F*  REVISIONS:                                                   *
     F*                                                               *
     F*  09/13/91 KWK  PROJECT AHS                                    *
     F*                ADDED DEPARTMENT FIELD SO THAT THE DEPARTMENT  *
     F*                CODE MASTER FILE COULD BE USED FOR BOTH        *
     F*                DEPARTMENT LETTERS AND CLAIM DEPARTMENT        *
     F*                LETTERS.                                       *
     F*****************************************************************
     FW2250F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMDEL     UF A E           K DISK

     D UA              S             10    DIM(3) CTDATA PERRCD(1)                MODE MESSAGES
     D PTL             S             40    DIM(1) CTDATA PERRCD(1)                PGM TITLES
     D CMSG            S             18    DIM(1) CTDATA PERRCD(1)                CMD KEY MESSAGE
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  DEPARTMENT CODE FILE
     C*
     C     DEKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    SCDPCD
     C*
     C     *LIKE         DEFINE    DEDPCD        QQDPCD
     C     *LIKE         DEFINE    DEDEPT        XXDEPT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQDEPT            2
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
     C                   MOVE      '0'           *IN64
     C                   MOVE      *BLANKS       QQRTCD
     C                   MOVE      QQDEPT        XXDEPT
     C*
     C                   MOVEL     QQPRDS        SCPRDS
     C                   MOVE      *BLANKS       SCCMSG
     C     QQCALL        IFEQ      '1'
     C                   MOVE      '1'           *IN09
     C                   MOVE      UA(3)         SCMODE                         'INQUIRY'
     C                   ELSE                                                   ELSE EQ '1'
     C     QQCALL        IFEQ      'D'
     C                   MOVE      '1'           *IN99
     C                   MOVE      UA(1)         SCMODE                         'ADD'
     C                   MOVEL     CMSG(1)       SCCMSG
     C                   END                                                    END EQ 'D'
     C                   END                                                    END EQ '1'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCDPCD
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
     C************************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C********************************
     C*   F3 = EXIT                  *
     C********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C********************************
     C*   F12 = CANCEL(PRIOR SCREEN) *
     C********************************
     C     *INKL         IFEQ      '1'
     C*** SCR01 - IF CALL FROM W4115F, RETURN TO W4115F
     C     Q2RECF        IFEQ      'SCR01'
     C     *IN99         ANDEQ     '1'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*** SCR02 - RETURN TO SCR01
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C***********************************
     C*   F6 = DEPT CODE SEARCH         *
     C***********************************
     C     *INKF         IFEQ      '1'
     C                   MOVEL     PTL(1)        QQPRDS
     C                   CALL      'W2251R'
     C                   PARM                    SCDPCD
     C                   PARM                    QQPRDS
     C                   PARM                    QQRTC             1
     C                   PARM                    QQDEPT            2
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTC         IFEQ      'B'
     C     SCDPCD        OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C     *IN99         IFEQ      '1'
     C                   GOTO      STRT
     C                   ELSE
     C                   GOTO      SCR01T
     C                   END                                                    END 1 *IN99
     C                   END                                                    END EQ '*'
     C*
     C                   END                                                    END *INKF
     C***********************************
     C*   COMMAND 20 - CHANGE STATUS    *
     C***********************************
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   MOVE      '1'           *IN64
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      '0'           *IN64
     C                   END
     C                   GOTO      STRT
     C                   END                                                    END *INKU
     C*
     C*   F23=DELETE CODE
     C     *INKX         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C                   DELETE    PDE01
     C                   MOVE      '0'           *IN64
     C                   MOVE      *BLANKS       SCDPCD
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C***********************************
     C*   *IN10 - PROCESS HELP REQUESTS *
     C***********************************
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
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C***********************************
     C*   *INKM - CHG USER PARMS        *
     C***********************************
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END                                                    END *INKM
     C*****
     C                   END                                                    END CMDKEYS
     C*************************************************************************
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  THE DEPT CODE CANNOT BE EQUAL TO BLANKS
     C*
     C     SCDPCD        IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     DEKEY         CHAIN     PDE01                              40
     C     *IN40         IFEQ      '0'
     C*
     C*  IF CALLED FROM W4115F, CAN'T ADD AN EXISTING DEPARTMENT
     C     *IN99         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     DEDPNM        SCDPNM
     C                   MOVEL     DESTAT        SCSTAT
     C     SCSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN64                          ALLOW DELETEING
     C                   ELSE                                                   |
     C                   MOVE      '0'           *IN64                          NO DELETING ING
     C                   END                                                    |
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANK        SCDPNM
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      '0'           *IN64                          NO DELETING
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*  THE DEPARTMENT NAME CANNOT BE BLANK
     C*
     C     SCDPNM        IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     *IN09         IFEQ      '0'
     C     DEKEY         CHAIN     PDE01                              40
     C                   MOVE      UPCO#         DECO#
     C                   MOVEL     SCDPCD        DEDPCD
     C                   MOVE      SCDPNM        DEDPNM
     C                   MOVE      SCSTAT        DESTAT
     C                   MOVE      Q1USER        DEUSER
     C                   MOVE      Q1PGM         DEPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DEDT
     C*
     C                   Z-ADD     DSTIME        DETIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PDE01
     C                   ELSE
     C                   MOVE      Q1USER        DEIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DEIDT
     C*
     C                   MOVE      XXDEPT        DEDEPT
     C                   WRITE     PDE01
     C                   END
     C*
     C                   END
     C*
     C*** IF THIS PROGRAM WAS CALLED FROM W4115R, RETURN TO W4115R
     C     *IN99         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   MOVE      *BLANKS       SCDPCD
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCDPCD
     C                   GOTO      STRT
     C*
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  TABLE UA - MODE MESSAGE
-- ADD --
- UPDATE -
- INQUIRY-
**  TABLE PTL - PGM TITLES
         DEPARTMENT CODE SEARCH
**  TABLE CMSG - COMMAND KEY MESSAGE
F12=Cancel
