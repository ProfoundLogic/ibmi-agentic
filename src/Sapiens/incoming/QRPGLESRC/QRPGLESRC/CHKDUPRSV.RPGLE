     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE CHKDUPRSV - CHECK RESERVE HISTORY FOR DUP RECORDS
     H*****************************************************************
     FWMRHL     UF   E           K DISK
     FWMRHL5    IF   E           K DISK
     F                                     RENAME(PRH01:PRH02)
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     IPRH02
     I              RHSU01                      R2SU01
     I              RHSU02                      R2SU02
     I              RHSU03                      R2SU03
     I              RHSU04                      R2SU04
     I              RHSU05                      R2SU05
     I              RHSU06                      R2SU06
     I              RHSU07                      R2SU07
     I              RHSU08                      R2SU08
     I              RHSU09                      R2SU09
     I              RHSU10                      R2SU10
     I              RHRT01                      R2RT01
     I              RHRT02                      R2RT02
     I              RHRT03                      R2RT03
     I              RHRT04                      R2RT04
     I              RHRT05                      R2RT05
     I              RHRT06                      R2RT06
     I              RHRT07                      R2RT07
     I              RHRT08                      R2RT08
     I              RHRT09                      R2RT09
     I              RHRT10                      R2RT10
     I              RHEXPL                      R2EXPL
     I              RHCLST                      R2CLST
     I              RHSTAT                      R2STAT
     I              RHUSER                      R2USER
     I              RHPGM                       R2PGM
     I              RHTIME                      R2TIME
     I              RHDT                        R2DT

     C*----------------------------------------------------------------
     C*
     C     RHKY          KLIST
     C                   KFLD                    RHCO#
     C                   KFLD                    RHFND
     C                   KFLD                    RHFYR
     C                   KFLD                    RHCASE
     C                   KFLD                    RHEFDT
     C                   KFLD                    RHTIME
     C*
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   MOVE      'N'           XXUPDF            1
     C*
     C*  Start of main loop.
     C*
     C                   READ      PRH01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     RHKY          CHAIN     PRH02                              99
     C  N99RHKY          READE     PRH02                                  99
     C     *IN99         DOWEQ     '0'
     C     RHTIME        ANDEQ     R2TIME
     C                   ADD       1             RHTIME
     C                   MOVE      'Y'           XXUPDF
     C     RHKY          CHAIN     PRH02                              99
     C                   ENDDO
      *
     C     XXUPDF        IFEQ      'Y'
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   UPDATE    PRH01
     C                   ADD       1             XTCASE            9 0
     C                   MOVE      'N'           XXUPDF
     C                   END
     C*
     C                   READ      PRH01                                  60
     C                   END                                                    *IN60 OFF
     C*
     C                   EXCEPT    TOT
     C                   MOVE      '1'           *INLR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1  3
     O                       DSTIME               8 '  :  :  '
     O                                           55 'RESERVE HISTORY RECORDS'
     O                                           76 'WITH DUPLICATE KEYS'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                            9 'CHKDUPRSV'
     O                                          123 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           2
     O                                            3 'CO#'
     O                                            8 'FND'
     O                                           13 'FYR'
     O                                           21 'CASE'
     O                                           33 'EFF DT'
     O                                           45 'MAINT DT'
     O                                           56 'MAINT TIME'
     O                                           68 'MAINT PGM'
     O          E            DET            2
     O                       RHCO#         Z      3
     O                       RHFND         Z      8
     O                       RHFYR         Z     13
     O                       RHCASE        Z     21
     O                       RHEFDT              33 '   /  /  '
     O                       RHDT                45 '   /  /  '
     O                       RHTIME              56 '  :  :  '
     O                       RHPGM               69
      *
     O          E            TOT         2  1
     O                                            7 'TOTALS:'
     O                       XTCASE        Z     21
