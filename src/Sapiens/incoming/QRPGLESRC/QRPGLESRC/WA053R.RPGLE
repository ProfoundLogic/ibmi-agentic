     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  WA053R - Adjust Researves                                    *
     H*                                                               *
     H*  Called from: F8062RBAK - Rx Backout Program                  *
     H*  Calls to   : None                                            *
     H*                                                               *
     H*****************************************************************
     H*  Created on 12/12/2003 by c:62 for Support 40348              *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*****************************************************************
     FWDELL     UF   E           K DISK
     FWMCML1    UF   E           K DISK

     D*
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 6     14  0
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*================================================================
     C* Parameter Lists
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@FND
     C                   PARM                    @@FYR
     C                   PARM                    @@CASE
     C                   PARM                    @@RECD            2
     C                   PARM                    @@TYPE            1
     C                   PARM                    @@AMT             9 2
     C                   PARM                    @@PTYP            1
     C                   PARM                    @@RTCD            1
     C*
     C     PRCCPL        PLIST
     C                   PARM      @@CO#         ##CO#
     C                   PARM      @@FND         ##FND
     C                   PARM      @@FYR         ##FYR
     C                   PARM      @@CASE        ##CASE
     C                   PARM      @@RECD        ##RECD            2
     C                   PARM      @@TYPE        ##TYPE            1
     C                   PARM      @@AMT         ##AMT             9 2
     C                   PARM      @@PTYP        ##PTYP            1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*================================================================
     C* Key Lists
     C*================================================================
     C*
     C     CMKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C*
     C     ELKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*================================================================
     C* Like Defines
     C*================================================================
     C     *LIKE         DEFINE    CMCO#         @@CO#
     C     *LIKE         DEFINE    CMFND         @@FND
     C     *LIKE         DEFINE    CMFYR         @@FYR
     C     *LIKE         DEFINE    CMCASE        @@CASE
     C     *LIKE         DEFINE    CMCO#         ##CO#
     C     *LIKE         DEFINE    CMFND         ##FND
     C     *LIKE         DEFINE    CMFYR         ##FYR
     C     *LIKE         DEFINE    CMCASE        ##CASE
     C*================================================================
     C*
     C                   SETOFF                                       16
     C     @@TYPE        IFEQ      'R'
     C     @@TYPE        OREQ      'Z'
     C     @@TYPE        OREQ      'C'
     C                   SETON                                        16
     C                   ENDIF
     C*
     C     CMKY          CHAIN(N)  PCM01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     CMSTAT        IFEQ      'C'
     C                   CALL      'SRPRCC'      PRCCPL
     C                   ENDIF
     C*
     C     @@PTYP        IFEQ      'P'
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     1             X                 3 0
     C     @@RECD        LOOKUP    RT(X)                                  99
     C*
     C     *IN16         IFEQ      *OFF
     C                   ADD       @@AMT         RR(X)
     C                   SUB       @@AMT         AR(X)
     C                   ELSE
     C                   SUB       @@AMT         RR(X)
     C                   ADD       @@AMT         AR(X)
     C                   ENDIF
     C*
     C                   MOVEL(P)  Q1USER        CMUSER
     C                   MOVEL(P)  Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C                   UPDATE    PCM01
     C                   ENDIF
     C*
     C     ELKY          CHAIN     PEL01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     *IN16         IFEQ      *OFF
     C                   SUB       @@AMT         ELTCLP
     C                   ADD       @@AMT         ELOUTS
     C                   ELSE
     C                   ADD       @@AMT         ELTCLP
     C                   SUB       @@AMT         ELOUTS
     C                   ENDIF
     C*
     C                   MOVEL(P)  Q1USER        ELUSER
     C                   MOVEL(P)  Q1PGM         ELPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C*
     C                   UPDATE    PEL01
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVEL     'F'           @@RTCD
     C                   ENDIF
     C*
     C                   SETON                                        LR
