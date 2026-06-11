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
     FWDPALG    IF   E           K DISK
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
     C                   KFLD                    PACO#
     C                   KFLD                    PAFEIN
     C                   KFLD                    PAMOD
     C*
     C     PAKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    KKEOBS
     C*
     C     *LIKE         DEFINE    PAEOBS        KKEOBS
     C*
     C                   READ      PEA01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     *ZEROS        EAWCPC
     C                   Z-ADD     *ZEROS        EAWCPD
     C                   Z-ADD     *ZEROS        EAWCBC
     C                   Z-ADD     *ZEROS        EAWCBD
     C                   Z-ADD     *ZEROS        EAWCNC
     C                   Z-ADD     *ZEROS        EAWCND
     C*
     C                   Z-ADD     EARCN         KKEOBS
     C*
     C     PAKY          CHAIN     PPA01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     PATYPE        IFEQ      'P'
     C     PAADST        ANDEQ     ' '
     C*
     C     NRKY          CHAIN     PNR01                              99
     C     *IN99         IFEQ      *OFF
     C                   SELECT
     C     NRGRTY        WHENEQ    'B'
     C                   Z-ADD     PACHK#        EAWCBC
     C                   Z-ADD     PADT          SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         EAWCBD
     C                   Z-ADD     PAAMT         EABAMT
     C     NRGRTY        WHENEQ    'N'
     C                   Z-ADD     PACHK#        EAWCNC
     C                   Z-ADD     PADT          SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         EAWCND
     C                   Z-ADD     PAAMT         EANAMT
     C     NRGRTY        WHENEQ    'O'
     C                   Z-ADD     PACHK#        EAWCPC
     C                   Z-ADD     PADT          SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         EAWCPD
     C                   ENDSL
     C                   ELSE
     C     EAFEIN        IFEQ      PAFEIN
     C     EAMOD         ANDEQ     PAMOD
     C                   Z-ADD     PACHK#        EAWCPC
     C                   Z-ADD     PADT          SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         EAWCPD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     PAKY          READE     PPA01                                  99
     C*
     C                   ENDDO
     C*
     C                   UPDATE    PEA01
     C*
     C                   READ      PEA01                                  99
     C*
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*
