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
     FWACML     IF   E           K DISK
     FWDPAL     UF   E           K DISK
     FSMCOL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C**************************************************************************
     C*
     C*  Key list for the Claim Master File
     C*
     C     CMKY          KLIST
     C                   KFLD                    COCO#
     C*
     C*  Key list for the Check Detail File
     C*
     C     PAKY          KLIST
     C                   KFLD                    COCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C                   MOVE      'N'           XXPRT             1
     C                   MOVE      'N'           XXDUP             1
     C                   Z-ADD     1             X                 3 0
     C                   TIME                    DSDTTM
     C*
     C*-------------------------------------------------------------------------
     C                   READ      PCO01                                  99
     C                   EXCEPT    HEAD
     C     *IN99         DOWEQ     '0'
     C*
     C*  Start of main loop
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         DOWEQ     '0'
     C*
     C*  If any records exist in the pmt detail file, delete them.
     C*
     C     PAKY          CHAIN     PPA01                              99
     C     *IN99         DOWEQ     '0'
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   DELETE    PPA01
     C     PAKY          READE     PPA01                                  99
     C                   ENDDO
     C*
     C     CMKY          READE     PCM01                                  99
     C                   ENDDO
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
     O                                           70 'Pmts which exist on dele'
     O                                           81 'ted claims.'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                            3 'Grp'
     O                                            8 'FYR'
     O                                           17 '   Case'
     O                                           27 ' Check#'
     O                                           38 'Trans dt'
     O                                           48 'Post dt'
     O                                           63 'Amount'
     O          E            DET         1
     O                       PAFND         4      3
     O                       PAFYR         4      8
     O                       PACASE        4     17
     O                       PACHK#        4     27
     O                       PATRDT              38
     O                       PAPODT              48
     O                       PAAMT         J     63
