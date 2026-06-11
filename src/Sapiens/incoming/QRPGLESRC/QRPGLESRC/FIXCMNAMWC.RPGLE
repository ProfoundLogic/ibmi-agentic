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
     FWMCDL     IF   E           K DISK
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
     C*  Key list for the Claimant Demographics file
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQMODE            1
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *BLANKS       SVNAME           40
     C                   MOVE      *BLANKS       XXNAME           40
     C*
     C*  Start WCMS main loop
     C*
     C                   READ      PCM01                                  60
     C*
     C  N60COKY          CHAIN     PCO01                              61
     C                   EXCEPT    HEAD
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     CDKY          CHAIN     PCD01                              61
     C     *IN61         IFEQ      '0'
     C*
     C     CMLSN         IFNE      CDLSN
     C     CMFSN         ORNE      CDFSN
     C     CMMID         ORNE      CDMID
     C*
     C     CMLSN         CAT(P)    ',':0         SVNAME
     C                   CAT       CMFSN:1       SVNAME
     C                   CAT       CMMID:1       SVNAME
     C     CDLSN         CAT(P)    ',':0         XXNAME
     C                   CAT       CDFSN:1       XXNAME
     C                   CAT       CDMID:1       SVNAME
     C*
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C     QQMODE        IFEQ      'U'
     C                   MOVEL     CDLSN         CMLSN
     C                   MOVEL     CDFSN         CMFSN
     C                   MOVEL     CDMID         CMMID
     C                   UPDATE    PCM01
     C                   ENDIF
     C*
     C                   ENDIF
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
     O                       Q1PGM               10
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            3 'FND'
     O                                            8 'FYR'
     O                                           17 'Case'
     O                                           33 'Previous name'
     O                                           76 'Correct name'
     O          E            DET
     O                       CMFND         4      3
     O                       CMFYR         4      8
     O                       CMCASE        4     17
     O                       SVNAME              60
     O                       XXNAME             104
