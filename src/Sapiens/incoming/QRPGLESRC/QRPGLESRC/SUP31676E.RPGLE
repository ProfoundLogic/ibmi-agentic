     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE UPDATE RPSEQ#
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     FWDPAL     IF   E           K DISK
     FWDRPL3    IF   E           K DISK
     FWDRPL     UF A E           K DISK
     F                                     RENAME(PRP01:PRP02)
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C**************************************************************************
     C*
     C     RPKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    PASEQ#
     C*
     C     R2KY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    PACHK#
     C                   KFLD                    XXTRDT
     C*
     C     *LIKE         DEFINE    PATRDT        XXTRDT
     C                   Z-ADD     0             XXCNT             7 0
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*----------------------------------------------------------------
     C*  Start of main loop
     C*
     C                   READ      PPA01                                  99
     C     *IN99         DOWEQ     '0'
     C     PACHK#        IFNE      0
     C*
     C*  Determine if the View Check record exists.
     C*
     C     RPKY          CHAIN     PRP01                              98
     C     *IN98         IFEQ      *ON
     C*****
     C*  For payments and recoveries, update RPSEQ# if WDRPP record
     C*  is found using the CHK# and TRDT.
     C*
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'R'
     C*
     C                   Z-ADD     PATRDT        XXTRDT
     C*
     C     R2KY          CHAIN     PRP02                              97
     C     *IN97         IFEQ      *OFF
     C                   Z-ADD     PASEQ#        RPSEQ#
     C                   UPDATE    PRP02
     C                   ELSE
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       1             XXCNT
     C                   ENDIF
     C*****
     C                   ELSE
     C*
     C*  For adj, voids, stops, and reversals, write a WDRPP record if
     C*  the WDRPP is found for the original transaction.
     C*
     C                   Z-ADD     PAPODT        XXTRDT
     C*
     C     R2KY          CHAIN(N)  PRP02                              97
     C     *IN97         IFEQ      *OFF
     C                   Z-ADD     PASEQ#        RPSEQ#
     C                   WRITE     PRP02
     C                   ELSE
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       1             XXCNT
     C                   ENDIF
     C*
     C                   ENDIF                                                  PATYPE = P,M,R
     C*****
     C                   ENDIF                                                  *IN98 = *ON
     C*
     C                   ENDIF
     C                   READ      PPA01                                  99
     C                   ENDDO
     C*
     C                   EXCEPT    TOT
     C                   SETON                                        LR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                                           10 'SUP31676'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            3 'FND'
     O                                            8 'FYR'
     O                                           17 'CASE'
     O                                           27 'CHECK#'
     O                                           38 'TRANS DT'
     O                                           50 'SEQ#'
     O                                           62 'BUL1'
     O          E            DET
     O                       PAFND         4      3
     O                       PAFYR         4      8
     O                       PACASE        4     17
     O                       PACHK#        4     27
     O                       PATRDT              38
     O                       PASEQ#        4     50
     O                       PABUL1        4     62
     O          E            TOT
     O                                           13 'TOTAL COUNT:'
     O                       XXCNT         4     21
