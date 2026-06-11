     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE CHKDUPSEQ# - CHECK PAYMENT DETAIL FOR DUP SEQ#
     H*****************************************************************
     FWDPAL     IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F                                     USROPN

     D QCMD            S             50    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  Override the Print file to 198 CPI.
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM      QCMD(1)       ##CMD            50
     C                   PARM      50            ##CMDL           15 5
     C*
     C                   OPEN      QSYSPRT
     C*
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*
     C     *LIKE         DEFINE    PACO#         XXCO#
     C     *LIKE         DEFINE    PAFND         XXFND
     C     *LIKE         DEFINE    PAFYR         XXFYR
     C     *LIKE         DEFINE    PACASE        XXCASE
     C     *LIKE         DEFINE    PASEQ#        XXSEQ#
     C     *LIKE         DEFINE    PACHK#        XXCHK#
     C     *LIKE         DEFINE    PABASQ        XXBASQ
     C     *LIKE         DEFINE    PATRDT        XXTRDT
     C     *LIKE         DEFINE    PAPODT        XXPODT
     C     *LIKE         DEFINE    PAFEIN        XXFEIN
     C     *LIKE         DEFINE    PAMOD         XXMOD
     C     *LIKE         DEFINE    PAAMT         XXAMT
     C     *LIKE         DEFINE    PATRCD        XXTRCD
     C     *LIKE         DEFINE    PATYPE        XXTYPE
     C     *LIKE         DEFINE    PARECD        XXRECD
     C     *LIKE         DEFINE    PARETP        XXRETP
     C     *LIKE         DEFINE    PAADST        XXADST
     C     *LIKE         DEFINE    PASTAT        XXSTAT
     C     *LIKE         DEFINE    PAUSER        XXUSER
     C     *LIKE         DEFINE    PAPGM         XXPGM
     C     *LIKE         DEFINE    PATIME        XXTIME
     C     *LIKE         DEFINE    PADT          XXDT
     C     *LIKE         DEFINE    PABUL1        XXBUL1
     C*
     C*  Start of main loop.
     C*
     C                   READ      PPA01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     PACO#         IFEQ      XXCO#
     C     PAFND         ANDEQ     XXFND
     C     PAFYR         ANDEQ     XXFYR
     C     PACASE        ANDEQ     XXCASE
     C     PASEQ#        ANDEQ     XXSEQ#
      *
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       1             XTCASE            9 0
     C                   ADD       XXAMT         XTAMT            11 2
     C                   ADD       PAAMT         PTAMT            11 2
      *
     C                   ELSE
      *
     C                   MOVE      PACO#         XXCO#
     C                   MOVE      PAFND         XXFND
     C                   MOVE      PAFYR         XXFYR
     C                   MOVE      PACASE        XXCASE
     C                   MOVE      PASEQ#        XXSEQ#
     C                   MOVE      PACHK#        XXCHK#
     C                   MOVE      PABASQ        XXBASQ
     C                   MOVE      PATRDT        XXTRDT
     C                   MOVE      PAPODT        XXPODT
     C                   MOVE      PAFEIN        XXFEIN
     C                   MOVE      PAMOD         XXMOD
     C                   MOVE      PAAMT         XXAMT
     C                   MOVE      PATRCD        XXTRCD
     C                   MOVE      PATYPE        XXTYPE
     C                   MOVE      PARECD        XXRECD
     C                   MOVE      PARETP        XXRETP
     C                   MOVE      PAADST        XXADST
     C                   MOVE      PASTAT        XXSTAT
     C                   MOVE      PAUSER        XXUSER
     C                   MOVE      PAPGM         XXPGM
     C                   MOVE      PATIME        XXTIME
     C                   MOVE      PADT          XXDT
     C                   MOVE      PABUL1        XXBUL1
     C                   END                                                    CO#/SEQ# EQUAL
     C*
     C                   READ      PPA01                                  60
     C                   END                                                    *IN60 OFF
     C*
     C                   EXCEPT    TOT
     C                   MOVE      '1'           *INLR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1  3
     O                       DSTIME               8 '  :  :  '
     O                                           55 'PAYMENT DETAIL RECORDS'
     O                                           74 'WITH DUPLICATE SEQ'
     O                                           82 'NUMBERS'
     O                       DSDATE        Y    198
     O          E            HEAD           2
     O                                           10 'CHKDUPSEQ#'
     O                                          190 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD           1
     O                                          103 'TRANS'
     O                                          110 'RESRV'
     O          E            HEAD           2
     O                                            3 'CO#'
     O                                            8 'FND'
     O                                           13 'FYR'
     O                                           21 'CASE'
     O                                           28 'SEQ#'
     O                                           37 'CHECK#'
     O                                           44 'BASQ#'
     O                                           55 'TRANS DT'
     O                                           66 'POST DT'
     O                                           75 'FED ID#'
     O                                           81 'MOD'
     O                                           95 'CHECK AMT'
     O                                          100 'CD'
     O                                          103 'TP'
     O                                          107 'CD'
     O                                          110 'TP'
     O                                          115 'STS'
     O                                          121 'USER'
     O                                          138 'MAINT PGM'
     O                                          149 'MAINT TM'
     O                                          160 'MAINT DT'
     O          E            DET            1
     O                       XXCO#         Z      3
     O                       XXFND         Z      8
     O                       XXFYR         Z     13
     O                       XXCASE        Z     21
     O                       XXSEQ#        Z     28
     O                       XXCHK#        Z     37
     O                       XXBASQ        Z     44
     O                       XXTRDT              55 '   /  /  '
     O                       XXPODT              66 '   /  /  '
     O                       XXFEIN        Z     77
     O                       XXMOD               81
     O                       XXAMT         J     96
     O                       XXTRCD             100
     O                       XXTYPE             103
     O                       XXRECD             107
     O                       XXRETP             110
     O                       XXSTAT             114
     O                       XXUSER             127
     O                       XXPGM              139
     O                       XXTIME             149 '  :  :  '
     O                       XXDT               160 '   /  /  '
     O          E            DET            2
     O                       PACO#         Z      3
     O                       PAFND         Z      8
     O                       PAFYR         Z     13
     O                       PACASE        Z     21
     O                       PASEQ#        Z     28
     O                       PACHK#        Z     37
     O                       PABASQ        Z     44
     O                       PATRDT              55 '   /  /  '
     O                       PAPODT              66 '   /  /  '
     O                       PAFEIN        Z     77
     O                       PAMOD               81
     O                       PAAMT         J     96
     O                       PATRCD             100
     O                       PATYPE             103
     O                       PARECD             107
     O                       PARETP             110
     O                       PASTAT             114
     O                       PAUSER             127
     O                       PAPGM              139
     O                       PATIME             149 '  :  :  '
     O                       PADT               160 '   /  /  '
      *
     O          E            TOT         2  1
     O                                            7 'TOTALS:'
     O                       XTCASE        Z     21
     O                       XTAMT         J     96
     O          E            TOT            1
     O                       PTAMT         J     96
** QCMDEXC
OVRPRTF FILE(QSYSPRT) PAGESIZE(66 198) CPI(15)
