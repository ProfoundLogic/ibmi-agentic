     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PROGRAM TO CORRECT CLAIM INITIAL ENTRY FIELDS
     H*****************************************************************
     H* Description:  This pgm corrects the claim initial entry fields*
     H*   which were changed inadvertantly.                           *
     H*                                                               *
     H*****************************************************************
     FFAEOL17   IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FWDPALA    IF   E           K DISK
     FWARPLE    IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C**************************************************************************
     C*
     C*  Key list for the EOB Master file
     C*
     C     EOKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  Key list for the Pmt Detail file
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C*
     C*  Key list for the Pmt Detail file
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C*  Key list for the Rqst Pmt file
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOFND         XXFND
     C     *LIKE         DEFINE    EOPAID        XXPAID
     C                   TIME                    DSDTTM
     C*-------------------------------------------------------------------------
     C*  Start of main loop
     C*
     C                   READ      PCO01                                  99
     C                   EXCEPT    HEAD
     C     *IN99         DOWEQ     '0'
     C                   Z-ADD     COCO#         XXCO#
     C*
     C     EOKY          CHAIN     PEO01                              99
     C     *IN99         DOWEQ     '0'
     C*
     C     EOPAID        IFNE      0
     C     EOBLST        ANDEQ     'C'
     C*
     C*  Retrieve the WCMS fund #.
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      '0'
     C                   Z-ADD     FNWCF#        XXFND
     C*
     C*  Accumulate the total paid to the provider.
     C*
     C                   Z-ADD     0             XXPAID
     C     PAKY          CHAIN     PPA01                              99
     C     *IN99         DOWEQ     '0'
     C*
     C     PAFEIN        IFEQ      EOFEIN
     C     PAMOD         ANDEQ     EOMOD
     C     PATYPE        IFEQ      'S'
     C     PATYPE        OREQ      'V'
     C     PATYPE        OREQ      'R'
     C                   SUB       PAAMT         XXPAID
     C                   ELSE
     C                   ADD       PAAMT         XXPAID
     C                   ENDIF
     C                   ENDIF
     C*
     C     PAKY          READE     PPA01                                  99
     C                   ENDDO
     C*
     C     RPKY          CHAIN     PRP01                              99
     C     *IN99         DOWEQ     '0'
     C*
     C     RPFEIN        IFEQ      EOFEIN
     C     RPMOD         ANDEQ     EOMOD
     C     RPSTAT        ANDNE     'D'
     C                   ADD       RPAMT         XXPAID
     C                   ENDIF
     C*
     C     RPKY          READE     PRP01                                  99
     C                   ENDDO
     C*
     C     XXPAID        IFGT      EOPAID
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  Get the next claim record.
     C*
     C     EOKY          READE     PEO01                                  99
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
     O                                           71 'EOMBs which have been'
     O                                           80 'overpaid'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                            3 'Grp'
     O                                            8 'FYR'
     O                                           17 '   Case'
     O                                           29 '     EOB#'
     O                                           44 '      EOPAID'
     O                                           59 '  Total paid'
     O                                           70 'Trans dt'
     O          E            DET         1
     O                       XXFND         4      3
     O                       EOFYR         4      8
     O                       EOCASE        4     17
     O                       EOEOB#              29
     O                       EOPAID        J     45
     O                       XXPAID        J     60
     O                       EOTRDT              70
