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
     FWDPAL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
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
     D                 DS
     D  CD                     1     20
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

     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     PAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C*
     C                   MOVE      '1'           *INOF
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     0             XXPAY            11 2
     C*
     C*  GET ALL THE CLAIM DETAIL RECORDS
     C*
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         DOWEQ     '0'
     C     PATRDT        IFGE      960331
     C     PATRDT        ANDLE     960628
     C*
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'Z'
     C     PATYPE        OREQ      'A'
     C                   ELSE
     C     -1            MULT      PAAMT         PAAMT
     C                   END
     C*
     C                   ADD       PAAMT         XXPAY
     C                   END
     C     PAKEY         READE     PPA01                                  60
     C                   END
     C*
     C                   XFOOT     AR            XXAR             11 2
     C     XXPAY         COMP      XXAR                               6161
     C   OF              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   EXCEPT    DET
     C*
     C                   ADD       XXPAY         XTPAY            11 2
     C                   ADD       XXAR          XTAR             11 2
     C                   ADD       1             XTCASE            5 0
     C*
     C     CMKEY         READE     PCM01                                  60
     C                   END
     C                   EXCEPT    TOT
     C                   SETON                                        LR
     OQSYSPRT   E            HEAD           1 06
     O                                           56 'ALL CLAIMS    '
     O          E            HEAD           2
     O                                            4 'FND'
     O                                            8 'YEAR'
     O                                           20 'CASE NUMBER'
     O                                           35 'PAY DET'
     O                                           50 'CLAIM MAST'
     O                                           55 'NAT'
     O                                           60 'PAR'
     O                                           65 'CAU'
     O          E            DET
     O                       CMFND         Z      4
     O                       CMFYR         Z      7
     O                       CMCASE              20
     O                       XXPAY               35 '         .  -'
     O                       XXAR                50 '         .  -'
     O                       CMNATR              55
     O                       CMPART              60
     O                       CMCAUS              65
     O               61                          67 '*'
     O          E            TOT
     O                       XXFND         Z      4
     O                       XTCASE        Z     20
     O                       XTPAY               35 '         .  -'
     O                       XTAR                50 '         .  -'
