     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  FIXTPFR - FIX EOFRDT AND EOTODT.              C:62 03/13/00  *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FFAEOP     UF   E             DISK
     FFDCALR    IF   E           K DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDS           DS
     D  DBRRN                397    400B 0

     C*
     C     CAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     *LIKE         DEFINE    EOFRDT        LLFRDT
     C     *LIKE         DEFINE    EOTODT        HHTODT
     C*
     C                   READ      PEO01                                  99
     C     *IN99         DOWEQ     *OFF
     C     EOSYS         IFEQ      'P'
     C     EOSYS         OREQ      'H'
     C*
     C                   Z-ADD     *ZEROS        LLFRDT
     C                   Z-ADD     *ZEROS        HHTODT
     C*
     C     CAKY          CHAIN     PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     LLFRDT        IFEQ      *ZEROS
     C     CAFDAT        ORLT      LLFRDT
     C                   Z-ADD     CAFDAT        LLFRDT
     C                   ENDIF
     C*
     C     CAFDAT        IFGT      HHTODT
     C                   Z-ADD     CAFDAT        HHTODT
     C                   ENDIF
     C*
     C     CAKY          READE     PCA01                                  99
     C                   ENDDO
     C*
     C                   Z-ADD     LLFRDT        EOFRDT
     C                   Z-ADD     HHTODT        EOTODT
     C                   ENDIF
     C*
     C                   UPDATE    PEO01
     C                   READ      PEO01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
