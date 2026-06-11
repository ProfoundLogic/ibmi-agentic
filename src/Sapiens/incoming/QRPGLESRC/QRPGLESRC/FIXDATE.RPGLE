     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE PROGRAM TO LOAD CMIDT IF ZERO
     H*****************************************************************
     H* Description:  This pgm will load CMIDT, if zero, with CHEFDT  *
     H*   of the first history record.                                *
     H*****************************************************************
     FWMPDP     UF   E           K DISK
     FQSYSPRT   O    F   85        PRINTER OFLIND(*INOF)

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXDT                   1      7  0
     D  XXMM                   3      4  0
     D  XXDD                   5      6  0
     D                 DS
     D  XXDT1                  1      7  0
     D  XXYY1                  1      3  0
     D  XXMM1                  4      5  0
     D  XXDD1                  6      7  0

     C*=========================================================================
     C*
     C     *LIKE         DEFINE    PDDT          SVDT
     C                   MOVE      *ON           NOREC             1
     C                   Z-ADD     100           XXYY1
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*_______________________________________________________________
     C*
     C*  MAIN LOOP
     C*
     C                   READ      PPD01                                  99
     C     *IN99         DOWEQ     '0'
     C*
     C     PDDT          IFLT      20000
     C     PDDT          ANDNE     0
     C*
     C                   Z-ADD     PDDT          SVDT
     C                   Z-ADD     PDDT          XXDT
     C                   Z-ADD     XXMM          XXMM1
     C                   Z-ADD     XXDD          XXDD1
     C                   Z-ADD     XXDT1         PDDT
     C                   UPDATE    PPD01
     C*
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C     NOREC         IFEQ      *ON
     C                   MOVE      *OFF          NOREC
     C                   ENDIF
     C*
     C                   ENDIF                                                  PDDT > 20000
     C*
     C                   READ      PPD01                                  99
     C                   ENDDO                                                  *IN99=*OFF
     C*
     C     NOREC         IFEQ      *ON
     C                   EXCEPT    MSG
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C*
     C*_______________________________________________________________
     C*
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           54 'Updated Provider Records'
     O                                           74 'PAGE:'
     O                       PAGE          Z     80
     O          E            HEAD           2
     O                                            9 'Fed ID #'
     O                                           14 'Mod'
     O                                           26 'Old date'
     O                                           37 'New date'
     O          E            DET            1
     O                       PDFEIN               9
     O                       PDMOD               14
     O                       SVDT                26
     O                       PDDT                37
     O          E            MSG         2  1
     O                                           25 '*** No records found ***'
