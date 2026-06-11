     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  CRTNMIMP- READ THROUGH NMARP AND CREATE       C:62 04/25/00  *
     H*            INVOICE MASTER RECORDS                             *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FNMIMP     O    E             DISK
     FNMIML2    IF   E           K DISK
     F                                     RENAME(PIM01:PIM02)
     FNMARP     IF   E             DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     IMKY          KLIST
     C                   KFLD                    ARCO#
     C                   KFLD                    ARSUB#
     C                   KFLD                    ARFND
     C                   KFLD                    ARFYR
     C                   KFLD                    ARCASE
     C                   KFLD                    ARINV#
     C*
     C                   READ      PAR01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     ARTRTP        IFEQ      'I'
     C*
     C     IMKY          CHAIN     PIM02                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   Z-ADD     ARCO#         IMCO#
     C                   Z-ADD     ARSUB#        IMSUB#
     C                   Z-ADD     ARFND         IMFND
     C                   Z-ADD     ARFYR         IMFYR
     C                   Z-ADD     ARCASE        IMCASE
     C                   Z-ADD     ARINV#        IMINV#
     C                   Z-ADD     ARTRDT        IMTRDT
     C                   MOVEL     'A'           IMSTAT
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IMDT
     C                   MOVEL     SRDTO         IMIDT
     C                   Z-ADD     DSTIME        IMTIME
     C                   Z-ADD     DSTIME        IMITIM
     C*
     C                   MOVEL     Q1USER        IMUSER
     C                   MOVEL     Q1USER        IMIUSR
     C                   MOVEL     Q1PGM         IMPGM
     C                   MOVEL     Q1PGM         IMIPGM
     C*
     C                   WRITE     PIM01
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   READ      PAR01                                  99
     C*
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*
