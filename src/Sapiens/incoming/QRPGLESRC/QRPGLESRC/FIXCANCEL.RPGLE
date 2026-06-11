     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWDF2P - INITIALIZES F2DT01 BASED ON HIGHEST ELPFDT
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWDCXL     UF   E           K DISK
     FWDXHL     UF   E           K DISK
     FWMSHL1    UF   E           K DISK
     FWMEML     UF   E           K DISK
      *

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     SHKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXFYR
     C                   KFLD                    XXTYPE
      *
     C     XHKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXFYR
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    CXREQ#
      *
     C     EMKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
      *
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    SHTYPE        XXTYPE
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C                   MOVEL     'C'           XXTYPE
     C                   READ      PCX01                                  60
     C     *IN60         DOWEQ     *OFF
     C     CXCDAT        IFEQ      980922
     C     XHKY          CHAIN     PXH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PXH01
     C     XHKY          READE     PXH01                                  60
     C                   ENDDO
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C     EMCAD5        IFNE      0
     C                   Z-ADD     0             EMCAD5
     C                   ELSE
     C     EMCAD4        IFNE      0
     C                   Z-ADD     0             EMCAD4
     C                   ELSE
     C     EMCAD3        IFNE      0
     C                   Z-ADD     0             EMCAD3
     C                   ELSE
     C     EMCAD2        IFNE      0
     C                   Z-ADD     0             EMCAD2
     C                   ELSE
     C                   Z-ADD     0             EMCAD1
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   UPDATE    PEM01
     C                   ENDIF
     C     SHKY          CHAIN     PSH01                              60
     C     *IN60         DOWEQ     *OFF
     C     SHTRDT        IFEQ      980922
     C                   DELETE    PSH01
     C                   ENDIF
     C     SHKY          READE     PSH01                                  60
     C                   ENDDO
     C                   DELETE    PCX01
     C                   ENDIF
     C                   READ      PCX01                                  60
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
