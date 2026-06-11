     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PROGRAM TO COMPARE PAID AMOUNTS IN CLAIM MASTER FILE
     H*****************************************************************
     H* Description:  This pgm removes EDI log records which should   *
     H*   not exist.                                                  *
     H*                                                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMLGL     UF   E           K DISK
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
     C                   KFLD                    LGCO#
     C*
     C*  Key list for the Claim file
     C*
     C     CMKY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C*
     C                   TIME                    DSDTTM
     C*----------------------------------------------------------------
     C*  Start of main loop
     C*
     C                   READ      PLG01                                  60
     C*
     C  N60COKY          CHAIN     PCO01                              61
     C                   EXCEPT    HEAD
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     LGSTS         IFEQ      'EC'
     C     LGSTS         OREQ      'EI'
     C     LGSTS         OREQ      'EX'
     C*
     C*  Retrieve the claim record.
     C*    If the claim record does not exist or the log jurisdiction
     C*    state does not match the claim, delete the log record.
     C*
     C     CMKY          CHAIN     PCM01                              61
     C     *IN61         IFEQ      '1'
     C     LGJST         ORNE      CMJRST
     C     LGJST         OREQ      *BLANKS
     C*
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   DELETE    PLG01
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   READ      PLG01                                  60
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              96
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                                           10 'FIXEDILOG'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            3 'Fnd'
     O                                            8 'Fyr'
     O                                           17 'Case'
     O                                           28 'Trans Id'
     O                                           34 'Mtc'
     O                                           41 'Jrst'
     O                                           47 'Sts'
     O                                           59 'Init date'
     O          E            DET
     O                       LGFND         4      3
     O                       LGFYR         4      8
     O                       LGCASE        4     17
     O                       LGTSET              25
     O                       LGMTC               33
     O                       LGJST               40
     O                       LGSTS               46
     O                       LGIDT               59 '   /  /  '
