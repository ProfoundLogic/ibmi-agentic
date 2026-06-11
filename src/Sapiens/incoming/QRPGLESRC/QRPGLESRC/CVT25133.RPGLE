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
     FWACTL     IF   E           K DISK
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
     C     *ENTRY        PLIST
     C                   PARM                    QQUPD             1
     C*
     C*  Key list for the Check Detail File
     C*
     C     RPKY          KLIST
     C                   KFLD                    CTCO#
     C                   KFLD                    CTFND
     C                   KFLD                    CTXFYR
     C                   KFLD                    CTXCAS
     C*
     C                   TIME                    DSDTTM
     C*
     C                   READ      PCO01                                  99
     C                   EXCEPT    HEAD
     C*-------------------------------------------------------------------------
     C*  Start of main loop
     C*
     C                   READ      PCT01                                  99
     C     *IN99         DOWEQ     '0'
     C*
     C     CTXFYR        IFNE      CTFYR
     C     CTXCAS        ANDEQ     CTCASE
     C*
     C*  Read records in WDRPP using the old FYR and Case as the key
     C*    and update the records with the new FYR.
     C*
     C     RPKY          CHAIN     PRP01                              98
     C     *IN98         DOWEQ     *OFF
     C*
     C     QQUPD         IFEQ      'Y'
     C                   MOVE      CTFYR         RPFYR
     C                   MOVEL(P)  'CVT25133'    RPPGM
     C                   UPDATE    PRP01
     C                   ENDIF
     C*
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C     RPKY          READE     PRP01                                  98
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C*  Get the next pmt record.
     C*
     C                   READ      PCT01                                  99
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
     O                                           75 'Correct WDRPP FYR'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                            3 'Grp'
     O                                            8 'FYR'
     O                                           17 '   Case'
     O                                           28 'Old FYR'
     O                                           38 'Check #'
     O          E            DET         1
     O                       CTFND         4      3
     O                       CTFYR         4      8
     O                       CTCASE        4     17
     O                       CTXFYR        4     26
     O                       RPCHK#        4     38
