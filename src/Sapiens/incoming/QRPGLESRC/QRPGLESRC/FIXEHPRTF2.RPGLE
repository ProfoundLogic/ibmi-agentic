     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXEHPRTF - POPULATE EHPRTF WITH ANNUAL EXPENSE CONSTANT.
     H*****************************************************************
     H* MODIFIED VERSION OF PROGRAM TO CORRECT REDUCED EXPENSE CONST  *
     H* ON MA POLICIES.                                               *
     H*                                                               *
     H*****************************************************************
     FWDEHP     UF   E             DISK
     FWMRSL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSDATE                 7     12  0

     C*
     C     RSKY          KLIST
     C                   KFLD                    EHCO#
     C                   KFLD                    EHRTCD
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   MOVE      *ON           *IN66
     C                   TIME                    DSDTTM
     C*
     C                   EXCEPT    HEAD
     C*
     C                   READ      PEH01                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C     RSKY          CHAIN     PRS01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     RSJRST        ANDEQ     'MA'
     C     EHPRTD        ANDNE     *ZERO
     C     EHPRTD        ANDNE     EHPRTF
      *
     C     EHPROR        IFEQ      1
     C*
     C                   Z-ADD     EHPRTD        QQPREM
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   Z-ADD     EHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
     C                   EXCEPT    DET
     C*
     C     *IN66         IFEQ      *ON
     C                   Z-ADD     EHPRTD        EHPRTF
     C                   MOVEL(P)  Q1PGM         EHPGM
     C                   UPDATE    PEH01
     C                   ENDIF                                                  *IN66 IFEQ
     C*
     C                   ELSE
      *
     C                   MOVE      EHCO#         QQCO#
     C                   MOVE      EHFND         QQFND
     C                   MOVE      EHFYR         QQFYR
     C                   MOVE      EHEMP#        QQEMP#
     C                   MOVE      EHTRDT        QQTRDT
     C                   Z-ADD     0             QQPREM
     C                   CALL      'SRFEXP'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQTRDT            7 0
     C                   PARM                    QQPREM           11 2
     C*
     C     QQPREM        IFNE      *ZERO
     C     QQPREM        ANDNE     EHPRTF
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   Z-ADD     EHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
     C                   EXCEPT    DET
     C*
     C     *IN66         IFEQ      *ON
     C                   Z-ADD     QQPREM        EHPRTF
     C                   MOVEL(P)  Q1PGM         EHPGM
     C                   UPDATE    PEH01
     C                   ENDIF                                                  *IN66 IFEQ
      *
     C                   ENDIF                                                  QQPREM IFNE
      *
     C                   ENDIF                                                  EHPROR IFEQ ON
      *
     C                   ENDIF                                                  EHPRTF IFEQ ON
      *
     C                   READ      PEH01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C                   MOVE      *ON           *INLR
      *
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'PROGRAM NAME:'
     O                                           26 'FIXEHPRTF2'
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                                           24 'PGM TO CHECK IF EHPRTF'
     O                                           43 'IS NOT CORRECT   '
     O               66                          73 'UPDATE MODE IS ACTIVE'
     O          E            HEAD           2
     O                                            5 'CO#'
     O                                           11 'FUND'
     O                                           16 'FYR'
     O                                           27 'EMPLOYER#'
     O                                           34 'DIV'
     O                                           45 'PRORATE'
     O                                           59 'EHPRTF'
     O                                           73 'EHPRTD'
     O                                           90 'NORM PRM'
     O                                          104 'ANNL PRM'
     O                                          118 'NEW EHPRTF'
     O                                          130 'EHTRDT'
     O          E            DET            1
     O                       EHCO#         4      5
     O                       EHFND         4     11
     O                       EHFYR         4     16
     O                       EHEMP#        4     27
     O                       EHDIV         4     34
     O                       EHPROR        J     45
     O                       EHPRTF        J     59
     O                       EHPRTD        J     73
     O                       EHWNMP        J     90
     O                       EHWAPR        J    104
     O                       QQPREM        J    118
     O                       XXTRDT             130 '  /  /    '
