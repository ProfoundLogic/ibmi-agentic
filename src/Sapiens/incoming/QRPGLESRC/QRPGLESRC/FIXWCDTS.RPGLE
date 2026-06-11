     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  FIXWCDTS - FIX WCMS DATE FIELDS.              C:62 02/07/00  *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FFAEAL1    UF   E             DISK
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
     C     DTYYM2        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   READ      PEA01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     EAWCPD        IFNE      *ZEROS
     C*
     C                   Z-ADD     EAWCPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   Z-ADD     EAWCPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYM2
     C*
     C     SRERR         IFEQ      *OFF
     C*
     C                   MOVEL     SRDTO         EAWCPD
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL     SRDTO         EAWCPD
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     EAWCBD        IFNE      *ZEROS
     C*
     C                   Z-ADD     EAWCBD        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   Z-ADD     EAWCBD        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYM2
     C*
     C     SRERR         IFEQ      *OFF
     C*
     C                   MOVEL     SRDTO         EAWCBD
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL     SRDTO         EAWCBD
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     EAWCND        IFNE      *ZEROS
     C*
     C                   Z-ADD     EAWCND        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   Z-ADD     EAWCND        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYM2
     C*
     C     SRERR         IFEQ      *OFF
     C*
     C                   MOVEL     SRDTO         EAWCND
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL     SRDTO         EAWCND
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   UPDATE    PEA01
     C*
     C                   READ      PEA01                                  99
     C*
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*
