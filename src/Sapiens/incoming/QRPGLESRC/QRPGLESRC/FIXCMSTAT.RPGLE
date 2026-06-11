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
     FWMCHL     IF   E           K DISK
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
     C*  Key list for the Claim History file
     C*
     C     CHKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
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
     C*
     C     CHKY          SETGT     PCH01
     C     CHKY          READPE    PCH01                                  60
     C   60              MOVE      *BLANK        CHOCST
     C     CMSTAT        IFEQ      'A'
     C     CHOCST        ORNE      *BLANK
     C     CHOCST        ANDNE     CMSTAT
     C*
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C     CHOCST        IFEQ      'C'
     C                   MOVE      'C'           CMSTAT
     C                   ELSE
     C                   MOVE      'O'           CMSTAT
     C                   ENDIF
     C*
     C                   UPDATE    PCM01
     C*
     C                   ENDIF
     C*
     C                   READ      PCM01                                  60
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              96
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                                           09 'FIXCMSTAT'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                           34 'CLAIM'
     O                                           51 'HISTORY'
     O          E            HEAD           2
     O                                            3 'FND'
     O                                            8 'FYR'
     O                                           17 'CASE'
     O                                           35 'STATUS'
     O                                           50 'STATUS'
     O          E            DET            1
     O                       CMFND         4      3
     O                       CMFYR         4      8
     O                       CMCASE        4     17
     O                       CMSTAT              32
     O                       CHOCST              47
