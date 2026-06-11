     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXINPR - FIX INFORCED PREMIUM
     H*****************************************************************
     FWDEHL     UF   E           K DISK
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWDECL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  EMPLOYER DETAIL HISTORY FILE
     C*
     C     EHKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EHFND
     C                   KFLD                    EHFYR
     C                   KFLD                    EHEMP#
      *
     C     ECKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EHFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
      *
     C     ECKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EHFND
     C                   KFLD                    XXST
     C*
     C     *LIKE         DEFINE    EHCO#         XXCO#
     C     *LIKE         DEFINE    ECEXCO        XXEXCO
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQUP              1
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
     C*  IF EMPLOYER NUMBER IS NOT BLANK, SET UP FOR SINGLE RECORD PROCESS
     C*
     C                   Z-ADD     101           XXCO#
     C*
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*
     C     EHKY          CHAIN     PEH01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     0             XXEXCO
     C*
     C     EHPRTD        IFNE      *ZERO
     C                   MOVE      EHPFDT        XXEFDT            7 0
     C*
     C     ELKY2         CHAIN     PEL02                              60
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVE      ELRTCD        XXRTCD            3
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST              2
     C     ECKY          SETGT     PEC01
     C     ECKY1         READPE    PEC01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     ECEXCO        ANDGT     XXEXCO
     C                   Z-ADD     ECEXCO        XXEXCO
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C     ELKY2         READE     PEL02                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF
     C*
     C     EHINMP        ADD       XXEXCO        XXAMT            11 2
     C     XXAMT         IFNE      EHIAPR
     C*
     C                   Z-ADD     EHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   EXCEPT    DET
     C*
     C                   Z-ADD     XXAMT         EHIAPR
     C     QQUP          IFEQ      'Y'
     C                   UPDATE    PEH01
     C                   ENDIF
     C                   ENDIF
     C*
     C     EHKY          READE     PEH01                                  60
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
     C*
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'CHKIAPR:  '
     O                                           36 'PGM TO CHECK IF EMPLR '
     O                                           57 'INFORCE ANNUAL IS BAD'
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                                            8 'UPD:'
     O                       QQUP                12
     O          E            HEAD           2
     O                                            5 'CO#'
     O                                           11 'FUND'
     O                                           16 'FYR'
     O                                           27 'EMPLOYER#'
     O                                           34 'DIV'
     O                                           42 'STATUS'
     O                                           55 'EHTRDT'
     O                                           72 'EXP CN'
     O                                           89 'NORMAL'
     O                                          106 'ANNUAL'
     O          E            DET            1
     O                       EHCO#         4      5
     O                       EHFND         4     11
     O                       EHFYR         4     16
     O                       EHEMP#        4     27
     O                       EHDIV         4     34
     O                       EHFAF               39
     O                       XXTRDT              55 '  /  /    '
     O                       XXEXCO        4     72
     O                       EHINMP        4     89
     O                       EHIAPR        4    106
