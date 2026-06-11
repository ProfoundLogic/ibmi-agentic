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
     FWDRBL     IF   E           K DISK
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
     C*   KEY LIST FOR COMP REIMB FILE
     C*
     C     RBKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMJRST
     C                   KFLD                    CMACDT
     C*
     C     RBKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMJRST
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
     C     CMCRT         IFEQ      0
     C     CMLTFL        ANDEQ     'I'
     C     CMAWW         ANDGT     0
     C*
     C     RBKY          SETGT     PRB01
     C     RBKY1         READPE    PRB01                                  60
     C     *IN60         IFEQ      *OFF
     C*
     C     RBREPC        IFGT      *ZERO
     C     RBREPC        DIV(H)    100           XXREPC            5 4            CVT  COMP RATE
     C     CMAWW         MULT(H)   XXREPC        CMCRT                            CALC COMP RATE
      *
      * COMP RATE CAN NOT BE LESS THAN THE MINIMUM
      *
     C     CMCRT         IFLT      RBMNRT
     C                   Z-ADD     RBMNRT        CMCRT
     C                   END
      *
      * COMP RATE CAN NOT BE GREATER THAN THE MAXIMUM
      *
     C     CMCRT         IFGT      RBPACP
     C                   Z-ADD     RBPACP        CMCRT
     C                   END
     C*
     C                   UPDATE    PCM01
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
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
     O                                           10 'FIXCMENTD'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            3 'FND'
     O                                            8 'FYR'
     O                                           17 'CASE'
     O                                           26 'CMLTFL'
     O                                           40 'CMAWW'
     O                                           54 'CMCRT'
     O          E            DET
     O                       CMFND         4      3
     O                       CMFYR         4      8
     O                       CMCASE        4     17
     O                       CMLTFL              23
     O                       CMAWW         J     41
     O                       CMCRT         J     55
