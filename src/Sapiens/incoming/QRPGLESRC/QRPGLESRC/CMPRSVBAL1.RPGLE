     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PRTRSVBAL - PORT DEDUCTIBLE RECOVERY DISCREPANCIES
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     FWARPL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D RQ              S              9  2 DIM(10)                              Request Payment
     D DF              S              9  2 DIM(10)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)                              Reserve Type
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
     D  RR                     1     90  2
     D                                     DIM(10)                              Remaining Reserve
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
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C**************************************************************************
     C*  Key list for the Claim Master file
     C*
     C     RPKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   Z-ADD     1             X                 3 0
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C                   EXCEPT    HEAD
     C**************************************************************************
     C*  Loop through all Claim records.
     C*
     C                   READ      PCM01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C*  Accumulate the total of all released rqst pmt records.
     C*
     C                   CLEAR                   RQ
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C     RPSTAT        IFEQ      *BLANK
     C*
     C                   Z-ADD     1             X
     C     RPRECD        LOOKUP    RT(X)                                  60
     C   60              ADD       RPAMT         RQ(X)
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             X
     C     *BLANK        LOOKUP    RT(X)                                  60
     C   60              MOVEL     RPRECD        RT(X)
     C   60              ADD       RPAMT         RQ(X)
     C                   ENDIF
     C*
     C                   ENDIF
     C     RPKY          READE     PRP01                                  60
     C                   ENDDO
     C*
     C*  Determine if the reserves are sufficient.
     C*
     C                   CLEAR                   DF
     C                   DO        10            X
     C*
     C     RT(X)         IFNE      *BLANK
     C     RR(X)         ANDLT     RQ(X)
     C     RQ(X)         SUB       RR(X)         DF(X)
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*  Determine if the reserves are negative.
     C*
     C                   MOVE      'N'           XXUPD             1
     C                   CLEAR                   DF
     C                   DO        10            X
     C*
     C     RT(X)         IFNE      *BLANK
     C     RR(X)         ANDLT     0
     C     RR(X)         MULT      -1            DF(X)
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*  Get the next claim record.
     C*
     C                   READ      PCM01                                  50
     C                   ENDDO
     C*
     C*  End of Pgm
     C*
     C                   MOVE      '1'           *INLR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 02
     O                       DSTIME              08 '  :  :  '
     O                                           51 'This report prints claim'
     O                                           75 's with insufficient rese'
     O                                           99 'rves or negative reserve'
     O                                          101 's.'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           10 'PRTRSVBAL1'
     O                                          125 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           2
     O                                            3 'Grp'
     O                                           19 'FYR   Claim#'
     O                                           30 'Rsv Code'
     O                                           46 'Remaining Rsv'
     O                                           63 'Rqst Pmt Total'
     O          E            DET            1
     O                       CMFND         4      3
     O                       CMFYR         4      9
     O                       CMCASE        4     19
     O                       RT(X)               27
     O                       RR(X)         L     44
     O                       RQ(X)         L     60
