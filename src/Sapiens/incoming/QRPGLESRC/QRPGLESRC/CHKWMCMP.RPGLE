     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PROGRAM TO FIX PAID AMOUNTS IN CLAIM MASTER FILE
     H*
     H***********************************************************
     H* REVISIONS:
     H*   12-28-92   003   CHANGE PROGRAM TO ACCEPT ENTRY PARMS
     H*                    FOR CO#  AND FND#
     H***********************************************************
     H*
     FWMCML1    IF   E           K DISK
     FWMCHL     IF   E           K DISK
     FWDPAL7    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
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

     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXDATE
     C*
     C     PAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQDATE            7
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQDATE        XXDATE            7 0
     C                   EXCEPT    HEAD
     C*
     C*
     C     PAKY          SETLL     PPA01
     C     PAKY1         READE     PPA01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     PATYPE        IFEQ      'R'
     C     PAPGM         ANDNE     'W8021R1'
     C*
     C*  GET THE CLAIM RECORD
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     CMKY          SETGT     PCH01
     C     CMKY          READPE    PCH01                                  60
     C     *IN60         IFEQ      '0'
     C*
     C     CHOCST        IFEQ      'O'
     C     CHEFDT        ANDGT     PATRDT
     C     CMKY          READPE    PCH01                                  60
     C                   END
     C*
     C     CHOCST        IFEQ      'C'
     C     CHEFDT        ANDLT     PATRDT
     C*
     C                   Z-ADD     1             X                 3 0
     C     PARECD        LOOKUP    RT(X)                                  60
     C     *IN60         IFEQ      '1'
     C                   ADD       PAAMT         RR(X)
     C                   SUB       PAAMT         AR(X)
     C*******************  UPDATPCM01
     C*
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C     PAKY1         READE     PPA01                                  60
     C                   ENDDO
     C*
     C                   SETON                                        LR
     OQSYSPRT   E            HEAD           1 06
     O                                           56 'ALL CLAIMS    '
     O          E            HEAD           2
     O                                            4 'FND'
     O                                            8 'YEAR'
     O                                           20 'CASE NUMBER'
     O                                           34 'REFUND AMT'
     O                                           49 'REM RESERVES'
     O                                           64 'PAYMENT TOT'
     O          E            DET
     O                       CMFND         Z      4
     O                       CMFYR         Z      7
     O                       CMCASE              20
     O                       PAAMT               35 '         .  -'
     O                       RR(X)               50 '         .  -'
     O                       AR(X)               65 '         .  -'
     O                       PAPGM               77
     O                       PATRCD              81
     O                       PADESC             115
