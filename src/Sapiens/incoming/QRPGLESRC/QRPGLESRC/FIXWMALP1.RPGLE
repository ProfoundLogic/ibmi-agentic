     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXWMALP1- ENSURE TYPE OF BUSINESS ON ANY LOCATION IS THE
     H*                 SAME AS THE EMPLOYER TYPE OF BUSINESS.
     H*****************************************************************
     H*                                                               *
     H*  CREATED: 03/12/00  C:118 PROJECT SUPPORT(WA048 #21972)       *
     H*                                                               *
     H*  THIS PROGRAM SHOULD ENSURE THAT THE TYPE OF BUSINESS ON ANY  *
     H*  LOCATION IS THE SAME AS THE EMPLOYER TYPE OF BUSINESS.  ALSO,*
     H*  THERE SHOULD BE NO INDIVIDUAL OWNER/OFFICER RECORDS FOR      *
     H*  LOCATIONS, OWNERS/OFFICERS FOR LOCATIONS ARE ASSUMED TO BE   *
     H*  THE SAME AS THOSE ASSIGNED TO THE EMPLOYER AND CANNOT BE     *
     H*  CHANGED.                                                     *
     H*****************************************************************
     H*
     FWMEMP     IF   E           K DISK
     FWMALL     UF   E           K DISK
     FWMOOL     UF   E           K DISK
     F*
     FQSYSPRT   O    F  128        PRINTER
     F*

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
     C     OOKY          KLIST
     C                   KFLD                    ALCO#
     C                   KFLD                    ALFND
     C                   KFLD                    ALEMP#
     C                   KFLD                    ALDIV
     C                   KFLD                    ALELID
     C*
     C     *LIKE         DEFINE    DSTIME        XXTIME
     C*
     C*  INITIALIZE COUNTERS
     C                   Z-ADD     0             CNTAL             7 0
     C                   Z-ADD     0             CNTOO             7 0
     C                   EXCEPT    HEAD
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
     C     EMTPOB        IFNE      ALTPOB
     C                   MOVEL     EMTPOB        ALTPOB
     C                   ADD       1             CNTAL
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
     C*
     C                   ENDIF                                                  EMTP<>ALTP
     C*
     C     OOKY          CHAIN     POO01                              70
     C     *IN70         DOWEQ     '0'
     C*
     C                   DELETE    POO01
     C                   ADD       1             CNTOO
     C*
     C     OOKY          READE     POO01                                  70
     C*
     C                   ENDDO                                                  *IN70=DOW0
     C*
     C                   ENDIF                                                  ALELTP="L"
     C*
     C     ALKY          READE     PAL01                                  65
     C*
     C                   ENDDO                                                  DOW *IN65
     C*
     C                   READ      PEM01                                  60
     C                   ENDDO                                                  DOW *IN60
     C*
     C                   EXCEPT    DETAIL
     C*
     C                   MOVE      *ON           *INLR
     C*
     OQSYSPRT   E            HEAD           2 06
     O                                           30 '# OF ALTPOB RECORDS UPD'
     O                                         +000 'ATED AND # OF DELETED '
     O                                         +000 'RECORDS FROM WMOOP'
     O          E            HEAD           1
     O                                           20 '# TPOB UPDATED'
     O                                           50 '# OWNER/OFFICER DELETES'
     O          E            DETAIL         1
     O                       CNTAL               20
     O                       CNTOO               40
