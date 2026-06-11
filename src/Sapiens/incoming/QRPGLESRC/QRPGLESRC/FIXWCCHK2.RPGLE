     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  FIXWCCHK - FIX BRS CHECK INFO IN FAEAP        C:62 07/16/01  *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FFAEAL1    UF   E           K DISK
     FWARPP     IF   E             DISK
     FWMNRL     IF   E           K DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     NRKY          KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFEIN
     C                   KFLD                    RPMOD
     C*
     C     EAKY          KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C                   KFLD                    RPEOB#
     C                   KFLD                    KKRCN
     C*
     C     *LIKE         DEFINE    EASUB#        KKSUB#
     C     *LIKE         DEFINE    EARCN         KKRCN
     C*
     C                   Z-ADD     44            KKSUB#
     C*
     C                   READ      PRP01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     RPEOB#        IFNE      *ZEROS
     C*
     C                   Z-ADD     RPEOBS        KKRCN
     C*
     C     EAKY          CHAIN     PEA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     NRKY          CHAIN     PNR01                              99
     C     *IN99         IFEQ      *OFF
     C                   SELECT
     C     NRGRTY        WHENEQ    'B'
     C                   Z-ADD     RPAMT         EABAMT
     C     NRGRTY        WHENEQ    'N'
     C                   Z-ADD     RPAMT         EANAMT
     C                   ENDSL
     C                   ENDIF
     C*
     C                   UPDATE    PEA01
     C                   ENDIF
     C                   ENDIF
     C*
     C                   READ      PRP01                                  99
     C*
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*
