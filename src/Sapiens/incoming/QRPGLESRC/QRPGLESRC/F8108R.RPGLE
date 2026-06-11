     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE F8107R - PROMPT FOR DATA COPY
     H****************************************************************
     H*  PROGRAM DESCRIPTION:                                        *
     H*       THIS PROGRAM ALLOWS THE USER TO ENTER THE FUND TO BE   *
     H*       COPIED                                                 *
     H*                                                              *
     H*  CALLED BY: MENU                                             *
     H*                                                              *
     H*  CALLS TO:  F8108C                                           *
     H*                                                              *
     H*  PROGRAMMER: C:79                                            *
     H*  CREATED:    09/24/04    PROJECT: 48MFN                      *
     H****************************************************************
     H* REVISIONS:                                                   *
     H*                                                              *
     H****************************************************************
     H* INDICATOR USAGE:                                             *
     H*  SCR01                                                       *
     H*  21 - INVALID SUBSCRIBER NUMBER                              *
     H*  22 - INVALID FUND NUMBER                                    *
     H*                                                              *
     H****************************************************************
     FF8108F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FFMFNL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)

     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
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
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND MASTER FILE
     C*
     C     XXKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#            5 0
     C                   KFLD                    SCFND             3 0
     C*
     C     XXKEYP        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
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
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
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
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*   F10=SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C                   SELECT
     C*
     C*   SUBSCRIBER SEARCH
     C*
     C     HFLD          WHENEQ    'SCFND'
     C     XXKEYP        CHAIN     PSM01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN         ##RRN
     C                   ELSE
     C                   Z-ADD     *ZEROS        ##RRN
     C                   ENDIF
     C*
     C                   CALL      'F0060R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##RRN             9 0
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PFN02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     FNSUB#        SCSUB#
     C                   Z-ADD     FNFND         SCFND
     C                   MOVE      '1'           *IN28
     C*
     C                   ELSE
     C                   MOVE      '1'           *IN29
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   MOVE      '1'           *IN29
     C*
     C                   ENDIF                                                  ##RRN NE 0
     C*
     C*   GROUP SEARCH
     C*
     C     HFLD          WHENEQ    'SCSUB#'
     C*
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PSM02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     SMSUB#        SCSUB#
     C                   MOVE      '1'           *IN29
     C*
     C                   ELSE
     C                   MOVE      '1'           *IN28
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   MOVE      '1'           *IN28
     C*
     C                   ENDIF                                                  ##RRN NE 0
     C*
     C                   ENDSL                                                  HFLD WHEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  INKJ = *ON
     C*
     C                   ENDIF                                                  *IN11 IFEQ
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE SUBSCRIBER
     C*
     C     XXKEYP        CHAIN     PSM01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  IS GROUP BLANK ?
     C*
     C     SCFND         COMP      0                                      30
     C*
     C*  VALIDATE FUND AND GET THE FUND NAME
     C*
     C  N30XXKEY         CHAIN     PFN01                              60
     C   30XXKEYP        CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*   CALL THE CLP TO SUBMIT THE FUND MASTER.
     C*
     C                   MOVE      UPCO#         X@CO#
     C                   MOVE      SCSUB#        X@SUB
     C                   MOVE      SCFND         X@FND
     C                   CALL      'F8108C'
     C                   PARM                    X@CO#             3
     C                   PARM                    X@SUB             5
     C                   PARM                    X@FND             3
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
