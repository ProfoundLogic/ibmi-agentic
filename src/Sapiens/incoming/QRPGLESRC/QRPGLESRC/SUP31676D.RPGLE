     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE UPDATE RPSEQ#
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     FWDRPL     UF   E           K DISK    USROPN
     FWDPALB    IF   E           K DISK    USROPN
     F*WDPAL   IF  E           K        DISK                           UC
     F*           PPA01                             KRENAMEPPA02
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C**************************************************************************
     C*
     C     PAKY          KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C                   KFLD                    RPCHK#
     C*
     C*          P2KY      KLIST
     C*                    KFLD           RPCO#
     C*                    KFLD           RPFND
     C*                    KFLD           RPFYR
     C*                    KFLD           RPCASE
     C*
     C*  Determine if the claim system is installed.
     C*
     C                   CALL      'S6000R'
     C                   PARM                    @@WCMC            1
     C                   PARM                    @@CHK             1
     C                   PARM                    @@BAR             1
     C                   PARM                    @@DED             1
     C                   PARM                    @@ADMN            1
     C                   PARM                    @@CLM             1
     C                   PARM                    @@EDI             1
     C                   PARM                    @@MICR            1
     C*
     C     @@CLM         IFNE      'Y'
     C                   SETON                                        LR
     C                   ENDIF
     C*
     C*
     C                   Z-ADD     0             XXCNT             7 0
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   OPEN      WDRPL
     C                   OPEN      WDPALB
     C*----------------------------------------------------------------
     C*  Start of main loop
     C*
     C                   READ      PRP01                                  99
     C     *IN99         DOWEQ     '0'
     C     RPSEQ#        IFEQ      0
     C*
     C     PAKY          CHAIN     PPA01                              99
     C     *IN99         DOWEQ     *OFF
     C     PABUL1        ANDNE     RPBUL1
     C     PAKY          READE     PPA01                                  99
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     PASEQ#        RPSEQ#
     C                   UPDATE    PRP01
     C*
     C                   ELSE
     C*
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       1             XXCNT
     C                   ENDIF
     C*
     C                   ENDIF
     C                   READ      PRP01                                  99
     C                   ENDDO
     C*
     C                   EXCEPT    TOT
     C                   SETON                                        LR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                                           10 'SUP31676'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            3 'FND'
     O                                            8 'FYR'
     O                                           17 'CASE'
     O                                           27 'CHECK#'
     O                                           38 'TRANS DT'
     O                                           50 'BUL1'
     O                                           60 'RPPNAM'
     O          E            DET
     O                       RPFND         4      3
     O                       RPFYR         4      8
     O                       RPCASE        4     17
     O                       RPCHK#        4     27
     O                       RPTRDT              38
     O                       RPBUL1        4     50
     O                       RPPNAM              94
     O          E            TOT
     O                                           13 'TOTAL COUNT:'
     O                       XXCNT         4     21
