     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  CREATED BY: C:53 05/02/00
     H*
     H*  CVTWC836 - POPULATE CTTRDT FROM CTDT IF BLANK
     H*
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWACTP     UF   E             DISK
     F*
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOA)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     *ZEROS        COUNT             5 0
     C                   Z-ADD     *ZEROS        COUNTA            5 0
     C                   MOVE      *ON           *INOA
     C                   MOVE      'U'           @@MODE            1
     C     @@MODE        COMP      'U'                                    10
     C*
     C     *IN98         DOUEQ     *ON
     C                   READ      PCT01                                  98
     C   98              ITER
     C                   ADD       1             COUNTA
     C*
     C     CTTRDT        IFEQ      *ZEROS
     C                   Z-ADD     CTDT          CTTRDT
     C                   ADD       1             COUNT
     C   10              UPDATE    PCT01
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    TOT1
     C*
     C                   SETON                                        LR
     C*
     OQSYSPRT   E            HEAD           1 02
     O                       DSTIME              08 '  :  :  '
     O                       DSDATE        Y     79
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           45 'UPDATE CTTRDT FROM CTD'
     O                                         +  0 'T IF = ZEROS'
     O          E            TOT1        2
     O                                           22 'Total cases read:'
     O                       COUNTA        J     29
     O          E            TOT1        2
     O                                           22 'Total cases changed:'
     O                       COUNT         J     29
