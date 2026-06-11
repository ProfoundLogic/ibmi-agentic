     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H*
     H/TITLE ISDL01R - Read for deleted images and call ISDL01R
     H*                to delete image
     H*
     H*****************************************************************
     H* Created on 02/01/2008 by c:62 for Support 60091               *
     H*****************************************************************
     H* Revisions:
     H*
      *  10/17/11 C:279 Project: SY195
      *                 Modify program to pass/return UNC path field
      *                 and IFS flag to called/calling programs.
      *
     H*****************************************************************
     FIMDOL8    IF   E             DISK
     FIMDOL1    UF   E           K DISK
     F                                     RENAME(PDO01:PDO02)
     FIMDOLD    IF   E           K DISK
     F                                     RENAME(PDO01:PDO03)
     FIMIEL5    IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C     DTYMD         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*YMD'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   READ      PDO01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   EXSR      CHKQUE
     C*
     C     ACTIVE        IFEQ      *OFF
     C     DODCID        CHAIN     PDO03                              99
     C     *IN99         IFEQ      *OFF
     C     DODKEY        ANDEQ     *BLANKS
     C*
     C     DOFLR         CAT(P)    '/':0         ##FOLD
     C                   CAT       DOSFLR:0      ##FOLD
     C*
     C                   CALL      'ISDL01C'
     C                   PARM                    ##FOLD           80
     C                   PARM      DODCID        ##DCID           12
     C                   PARM      *BLANKS       ##RTCD            1
     C*** BEG ADD *** 10/17/11 ****************************************
     C                   PARM      DOUNCN        ##UNCN          255
     C                   PARM      DOIFSP        ##IFSP            1
     C*** END ADD *** 10/17/11 ****************************************
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C     DODCID        CHAIN     PDO02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'D'           DOFL02
     C                   MOVEL(P)  Q1USER        DOLUSR
     C                   MOVEL     Q1PGM         DOLPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DOLTIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYMD
     C                   MOVEL     SRDTO         DOLDT
     C                   UPDATE    PDO02
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   READ      PDO01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*================================================================
     C* CHKQUE - CHECK TO SEE IF QUEUE IS ACTIVE
     C*================================================================
     C     CHKQUE        BEGSR
     C*
     C                   MOVE      *OFF          ACTIVE            1
     C*
     C     DODCID        CHAIN     PIE01                              99
     C     *IN99         DOWEQ     *OFF
     C     IESTAT        IFEQ      'A'
     C     IESTAT        OREQ      'L'
     C                   MOVE      *ON           ACTIVE
     C                   LEAVE
     C                   ENDIF
     C     IEDSTS        IFNE      'DL'
     C                   MOVE      *ON           ACTIVE
     C                   LEAVE
     C                   ENDIF
     C     DODCID        READE     PIE01                                  99
     C                   ENDDO
     C*
     C                   ENDSR
