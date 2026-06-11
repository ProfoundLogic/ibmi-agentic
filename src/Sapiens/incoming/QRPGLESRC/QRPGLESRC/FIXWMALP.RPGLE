     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXWMALP - ENSURE LOCATION RECORDS (TYPE 'L') HAVE EMPLR INFO
     H*****************************************************************
     H*                                                               *
     H*  CREATED: 04/30/99  C:09  PROJECT SUPPORT(WA048 #17501)       *
     H*                                          (WA048 #17504)       *
     H*                                                               *
     H*  THIS PROGRAM WILL READ THRU ALL EMPLOYER RECORDS AND IF ANY  *
     H*  EMPLOYERS HAVE LOCATIONS (WMALP/ALELTP='L') ASSIGNED, THEY   *
     H*  ARE CHANGED TO ENSURE SELECT FIELDS ARE THE SAME AS EMPLOYER *
     H*  MASTER, WHICH IS APPROPRIATE FOR EMPLOYER LOCATIONS.         *
     H*                                                               *
     H*****************************************************************
     H*
     FWMEMP     IF   E           K DISK
     FWMALL     UF   E           K DISK

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     ALKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C     *LIKE         DEFINE    DSTIME        XXTIME
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        XXTIME
     C*
     C                   READ      PEM01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     ALKY          CHAIN     PAL01                              65
     C     *IN65         DOWEQ     '0'
     C*
     C     ALELTP        IFEQ      'L'
     C*
     C     EMNAME        IFNE      ALNAME
     C                   MOVEL     EMNAME        ALNAME
     C                   END
     C*
     C     EMFDNM        IFNE      ALFDNM
     C                   MOVEL     EMFDNM        ALFDNM
     C                   END
     C*
     C     EMFEIN        IFNE      ALFEIN
     C                   Z-ADD     EMFEIN        ALFEIN
     C                   END
     C*
     C     EMTPOB        IFNE      ALTPOB
     C                   MOVEL     EMTPOB        ALTPOB
     C                   END
     C*
     C                   MOVEL     Q1USER        ALUSER
     C                   MOVEL     Q1PGM         ALPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ALDT
     C                   ADD       1             XXTIME
     C                   Z-ADD     XXTIME        ALTIME
     C                   UPDATE    PAL01
     C                   ENDIF                                                  ALELTP="L"
     C*
     C     ALKY          READE     PAL01                                  65
     C*
     C                   ENDDO                                                  DOW *IN65
     C*
     C                   READ      PEM01                                  60
     C                   ENDDO                                                  DOW *IN60
     C*
     C                   MOVE      *ON           *INLR
