     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PROGRAM TO CORRECT V/S/Z IN THE CHECK REC FILE
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     FWDPALH    IF   E           K DISK
     FWDCRL7    IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D XCACCT          C                   CONST('2090000708174')

     C**************************************************************************
     C*
     C*  Key list for the Pmt Detail file
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTRDT
     C*
     C     PAKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  Key list for the Check Rec file
     C*
     C     CRKEY         KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PACHK#
     C*
     C     *LIKE         DEFINE    PACO#         XXCO#
     C     *LIKE         DEFINE    PATRDT        XXTRDT
     C                   Z-ADD     980401        XXTRDT
     C                   TIME                    DSDTTM
     C*-------------------------------------------------------------------------
     C*  Start of main loop
     C*
     C                   READ      PCO01                                  99
     C                   EXCEPT    HEAD
     C     *IN99         DOWEQ     '0'
     C                   Z-ADD     COCO#         XXCO#
     C*
     C     PAKY          SETLL     PPA01
     C     PAKY1         READE     PPA01                                  99
     C     *IN99         DOWEQ     '0'
     C*
     C     PATYPE        IFEQ      'S'
     C     PATYPE        OREQ      'V'
     C     PATYPE        OREQ      'Z'
     C*
     C*  Get the Check Rec record.
     C*
     C     CRKEY         CHAIN     PCR01                              99
     C     PATYPE        IFEQ      'Z'
     C     CRKEY         READE     PCR01                                  99
     C                   ENDIF
     C*
     C     *IN99         IFEQ      '1'
     C                   Z-ADD     0             CRCHK#
     C                   MOVE      *BLANK        CRACTP
     C                   Z-ADD     0             CRACDT
     C                   Z-ADD     0             CRRDT1
     C                   ENDIF
     C*
     C     *IN99         IFEQ      '1'
     C     CRACTP        ORNE      PATYPE
     C     CRCHK#        ORNE      PACHK#
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  Get the next pmt record.
     C*
     C     PAKY1         READE     PPA01                                  99
     C                   ENDDO
     C*
     C*  Get the next company record.
     C*
     C                   READ      PCO01                                  99
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
     O                                           63 'Pmt voids which did not'
     O                                           87 'transfer into Check Rec'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                            3 'Grp'
     O                                            8 'FYR'
     O                                           17 '   Case'
     O                                           23 'Type'
     O                                           32 'PATRDT'
     O                                           42 'PAPODT'
     O                                           52 'PACHK#'
     O                                           62 'CRCHK#'
     O                                           72 'CRACTP'
     O                                           82 'CRACDT'
     O                                           92 'CRRDT1'
     O                                          102 'PAACCT'
     O                                          130 'PAAMT'
     O          E            DET         1
     O                       PAFND         4      3
     O                       PAFYR         4      8
     O                       PACASE        4     17
     O                       PATYPE              21
     O                       PATRDT              32
     O                       PAPODT              42
     O                       PACHK#              52
     O                       CRCHK#              62
     O                       CRACTP              72
     O                       CRACDT              82
     O                       CRRDT1              92
     O               99                          94 '*'
     O                       PAACCT             116
     O                       PAAMT         J    131
