     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PROGRAM TO COMPARE PAID AMOUNTS IN CLAIM MASTER FILE
     H*****************************************************************
     H* Description:  This pgm prints a list of claims which have a   *
     H*   discrepancy between the total paid in the Claim file and    *
     H*   the total paid in the Pmt Detail file.                      *
     H*                                                               *
     H*****************************************************************
     FWMCML1    IF   E           K DISK
     FWDPAL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
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

     C**************************************************************************
     C*
     C*  Key list for the Pmt Detail file
     C*
     C     PAKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*  Key list for the Company file
     C*
     C     COKY          KLIST
     C                   KFLD                    CMCO#
     C*
     C                   TIME                    DSDTTM
     C*
     C*  Start of main loop
     C*
     C                   READ      PCM01                                  60
     C*
     C  N60COKY          CHAIN     PCO01                              60
     C                   EXCEPT    HEAD
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     0             XXPAY            11 2
     C*
     C*  GET ALL THE CLAIM DETAIL RECORDS
     C*
     C     PAKY          CHAIN     PPA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     PATYPE        IFNE      'P'
     C     PATYPE        ANDNE     'M'
     C     PATYPE        ANDNE     'Z'
     C     PATYPE        ANDNE     'A'
     C                   MULT      -1            PAAMT
     C                   END
     C*
     C                   ADD       PAAMT         XXPAY
     C*
     C     PAKY          READE     PPA01                                  60
     C                   END
     C*
     C                   XFOOT     AR            XXAR             11 2
     C     XXPAY         COMP      XXAR                               6161
     C*
     C     *IN61         IFEQ      *ON
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       XXPAY         XTPAY            11 2
     C                   ADD       XXAR          XTAR             11 2
     C                   ADD       1             XTCASE            5 0
     C                   END
     C*
     C                   READ      PCM01                                  60
     C                   END
     C*
     C                   EXCEPT    TOT
     C                   SETON                                        LR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              96
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           76 'COMPARE WMCMP & WDPAP'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            3 'FND'
     O                                            8 'FYR'
     O                                           17 'CASE'
     O                                           33 'PMT DET AMT'
     O                                           50 'CLM MST AMT'
     O                                           56 'NAT'
     O                                           61 'PAR'
     O                                           66 'CAU'
     O          E            DET
     O                       CMFND         4      3
     O                       CMFYR         4      8
     O                       CMCASE        4     17
     O                       XXPAY         J     34
     O                       XXAR          J     51
     O                       CMNATR              55
     O                       CMPART              60
     O                       CMCAUS              65
     O          E            TOT         2
     O                                            7 'TOTALS:'
     O                       XTCASE        3     17
     O                       XTPAY         J     34
     O                       XTAR          J     51
