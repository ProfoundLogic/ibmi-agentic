     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWDEHP10 - RESET TRANSACTION DATE FOR WDEHP.
     H*****************************************************************
     H*  THIS PROGRAM IDENTIFIES ACCOUNTS THAT HAVE NO ESTIMATED OR   *
     H*  REVISED WDEHP RECORDS AND HAVE HAD MULTIPLE AUDITS RUN.      *
     H*                                                               *
     H*  PROGRAMMER: 65     SUPPORT#: 39437                           *
     H*  DATE: 10/14/03                                               *
     H*                                                               *
     H*****************************************************************
     FWDELL     IF   E             DISK
     FWDEHL     UF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSDATE                 7     12  0

     C*
     C     EHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXFAF
     C*
     C     *LIKE         DEFINE    EHCO#         XXCO#
     C     *LIKE         DEFINE    EHFND         XXFND
     C     *LIKE         DEFINE    EHFYR         XXFYR
     C     *LIKE         DEFINE    EHEMP#        XXEMP#
     C     *LIKE         DEFINE    EHDIV         XXDIV
     C     *LIKE         DEFINE    EHFAF         XXFAF
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
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQUPD             1
      *
     C     QQUPD         IFEQ      'Y'
     C                   MOVE      *ON           *IN66
     C                   ELSE
     C                   MOVE      *OFF          *IN66
     C                   ENDIF
      *
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
      *
     C                   READ      PEL01                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C                   Z-ADD     ELCO#         XXCO#
     C                   Z-ADD     ELFND         XXFND
     C                   Z-ADD     ELFYR         XXFYR
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   Z-ADD     ELDIV         XXDIV
      *
     C     ELFAF         IFEQ      'A'
     C*
     C                   MOVE      'E'           XXFAF
     C     EHKY          CHAIN     PEH01                              60
     C   60              MOVE      'R'           XXFAF
     C   60EHKY          CHAIN     PEH01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      'A'           XXFAF
     C     EHKY          CHAIN     PEH01                              60
     C     EHKY          READE     PEH01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     EHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
      *
     C                   Z-ADD     EHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8 0
      *
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
      *
     C                   EXCEPT    DET
      *
     C     *IN66         IFEQ      *ON
     C                   EXSR      FIX
     C                   ENDIF
     C*
     C     EHKY          READE     PEH01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ  EATRDT
     C*
     C                   ENDIF                                                  ELFAF IFEQ  EATRDT
      *
     C                   READ      PEL01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
     C                   MOVE      *ON           *INLR
     C*
     C*****************************************************************
     C*
     C     FIX           BEGSR
     C*
     C                   Z-ADD     0             EHTRDT
     C                   MOVEL(P)  Q1PGM         EHPGM
     C                   MOVEL(P)  Q1USER        EHUSER
     C*
     C                   UPDATE    PEH01
     C*
     C                   ENDSR
     C*
     C*****************************************************************
      *
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'PROGRAM NAME:'
     O                                           26 'FIXWDEHP10'
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                                           27 'PGM TO CHECK FOR AUDITS'
     O                                           47 'WITH NO E/R RECORDS'
     O               66                          73 'UPDATE MODE IS ACTIVE'
     O          E            HEAD           2
     O                                            5 'CO#'
     O                                           11 'FUND'
     O                                           16 'FYR'
     O                                           27 'EMPLOYER#'
     O                                           34 'DIV'
     O                                           46 'EHEFDT'
     O                                           58 'EHTRDT'
     O                                           70 'EHPGM'
     O          E            DET            1
     O                       EHCO#         4      5
     O                       EHFND         4     11
     O                       EHFYR         4     16
     O                       EHEMP#        4     27
     O                       EHDIV         4     34
     O                       XXEFDT              46 '  /  /    '
     O                       XXTRDT              58 '  /  /    '
     O                       EHPGM               70
