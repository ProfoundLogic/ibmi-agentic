     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7462R - DIARY REPORT
     FWMDDL     UF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE DIARY DATE FILE
     C*
     C     DDKEY         KLIST
     C                   KFLD                    XXCO#
     C     DDKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    DDFND
     C                   KFLD                    DDFYR
     C                   KFLD                    DDCASE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQDATE        X1DATE            8 0
     C*
     C                   TIME                    DSDTTM
     C*
     C*   CHAIN TO THE DIARY DATE FILE AND BEGIN MAIN LOOP
     C*
     C     DDKEY         CHAIN     PDD01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C                   EXCEPT    PDD01E
     C*
     C                   Z-ADD     DDCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCDAT            8 0
     C*
     C                   Z-ADD     X1DATE        XXDATE            8 0
     C*
     C                   Z-ADD     DDMDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXMDAT            8 0
     C*
     C*   UPDATE COMP. & MED. DATE IF IT EQUALS RUN DATE
     C*
     C     XXCDAT        COMP      XXDATE                                 21
     C     XXMDAT        COMP      XXDATE                                 22
     C*
     C     *IN21         IFEQ      '1'
     C     *IN22         OREQ      '1'
     C*
     C*   CALCULATE 14 DAYS INTO FUTURE
     C*
     C                   MOVE      14            SRDAYS            5 0
     C                   MOVE      XXDATE        SRDTE             8 0
     C                   MOVE      *BLANKS       SRERR             1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDTE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C*
     C     DDKEY2        CHAIN     PDD01                              40
     C*
     C   21              MOVE      SRDTE         XXCDAT
     C   22              MOVE      SRDTE         XXMDAT
     C*
     C                   Z-ADD     XXCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDCDAT
     C*
     C                   Z-ADD     XXMDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDMDAT
     C*
     C                   MOVE      Q1USER        DDUSER
     C                   MOVE      Q1PGM         DDPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDDT
     C*
     C                   Z-ADD     DSTIME        DDTIME
     C*
     C                   UPDATE    PDD01
     C*
     C                   END
     C     DDKEY         READE     PDD01                                  50
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     OPDD01     E            PDD01E
