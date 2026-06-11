     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PROGRAM TO CORRECT CLAIM FIELDS IN WDELP
     H*****************************************************************
     H* Description:  This pgm prints a list of employers with        *
     H*   discrepancies in the claim total fields.                    *
     H*                                                               *
     H*****************************************************************
     FWDPALB    IF   E           K DISK
     FWDRPL     UF   E           K DISK
     FSMCOL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C**************************************************************************
     C*
     C*  Key list for the Check Detail File
     C*
     C     RPKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    PACHK#
     C*
     C     *LIKE         DEFINE    PACO#         XXCO#
     C     *LIKE         DEFINE    PACHK#        SVCHK#
     C     *LIKE         DEFINE    PATRDT        SVTRDT
     C                   MOVE      'N'           XXPRT             1
     C                   MOVE      'N'           XXDUP             1
     C                   Z-ADD     1             X                 3 0
     C                   TIME                    DSDTTM
     C*
     C                   READ      PCO01                                  99
     C                   EXCEPT    HEAD
     C*-------------------------------------------------------------------------
     C*  Start of main loop
     C*
     C                   READ      PPA01                                  99
     C     *IN99         DOWEQ     '0'
     C*
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PACHK#        IFNE      0
     C*
     C*  If more than one record exists with the same check# but the
     C*    transaction date is different, print on the report.
     C*
     C                   MOVE      'N'           XXDUP
     C     PACHK#        IFEQ      SVCHK#
     C     PATRDT        ANDNE     SVTRDT
     C                   MOVE      'Y'           XXDUP
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   EXCEPT    DET1
     C                   ENDIF
     C                   Z-ADD     PACHK#        SVCHK#
     C                   Z-ADD     PATRDT        SVTRDT
     C*
     C*  Retrieve check detail record and update.
     C*
     C                   Z-ADD     1             X
     C     RPKY          CHAIN     PRP01                              98
     C     *IN98         DOWEQ     '0'
     C*
     C     PATRDT        IFNE      RPTRDT
     C*
     C     XXDUP         IFEQ      'N'
     C     X             ORGT      1
     C*
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C                   Z-ADD     PATRDT        RPTRDT
     C                   UPDATE    PRP01
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ADD       1             X
     C     RPKY          READE     PRP01                                  98
     C                   ENDDO
     C*
     C                   ENDIF                                                  PACHK# ne 0
     C                   ENDIF                                                  PATYPE = P/M
     C*
     C*  Get the next pmt record.
     C*
     C                   READ      PPA01                                  99
     C                   ENDDO
     C*
     C*  End of program
     C*
     C                   SETON                                        LR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              96
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           78 'Change WDRPP Trans Date'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                            3 'Grp'
     O                                            8 'FYR'
     O                                           17 '   Case'
     O                                           27 ' Check#'
     O                                           38 'Trans dt'
     O                                           48 ' RPTRDT'
     O                                           58 ' SVTRDT'
     O          E            DET         1
     O                       PAFND         4      3
     O                       PAFYR         4      8
     O                       PACASE        4     17
     O                       PACHK#        4     27
     O                       PATRDT              38
     O                       RPTRDT              48
     O                       SVTRDT              58
     O          E            DET1        0
     O                                           82 '**Duplicate check #'
