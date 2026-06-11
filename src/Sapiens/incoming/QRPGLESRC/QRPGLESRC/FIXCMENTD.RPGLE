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
     FWMCML1    UF   E           K DISK
     FSMCOL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C**************************************************************************
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
     C  N60COKY          CHAIN     PCO01                              61
     C                   EXCEPT    HEAD
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     CMIDT         IFGT      980401
     C*
     C     CMLTFL        IFNE      'R'
     C     CMENTD        ANDEQ     0
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C                   Z-ADD     CMIDT         CMENTD
     C                   UPDATE    PCM01
     C                   END
     C*
     C                   END
     C*
     C                   READ      PCM01                                  60
     C                   END
     C*
     C                   SETON                                        LR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              96
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                                           10 'FIXCMENTD'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            3 'FND'
     O                                            8 'FYR'
     O                                           17 'CASE'
     O                                           34 'CMIDT'
     O                                           50 'CMENTD'
     O          E            DET
     O                       CMFND         4      3
     O                       CMFYR         4      8
     O                       CMCASE        4     17
     O                       CMIDT               34
     O                       CMENTD              50
