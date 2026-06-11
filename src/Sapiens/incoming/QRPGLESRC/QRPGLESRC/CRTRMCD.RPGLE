     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H* Create CERT remark code
     H*****************************************************************
     H* Created on 11/02/2005 by c:62 for Support VI129 49124
     H*****************************************************************
     H* Revisions:
     H*
     H*****************************************************************
     FSTCTL1    IF A E           K DISK
     F*

     D*
     D DESC            C                   CONST('CERTIFICATE OF -
     D                                     INSURANCE PRINT')
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*=====================================================================
     C*
     C                   TIME                    DSDTTM
     C*
     C     'CERTS'       CHAIN     PCT01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PCT01
     C                   MOVEL     'CI'          CTTYPE
     C                   MOVEL     DESC          CTDESC
     C                   MOVEL     'N'           CTFL01
     C                   MOVEL     'N'           CTFL03
     C                   MOVEL     'N'           CTFL04
     C                   MOVEL     'A'           CTSTAT
     C                   MOVEL     'CERTS'       CTRMCD
     C                   MOVEL     '000'         CTDSLV
     C                   MOVEL     '000'         CTDLSL
     C                   MOVEL     '000'         CTCMSL
     C                   MOVEL     '000'         CTCPSL
     C*
     C                   MOVEL(P)  Q1USER        CTUSER
     C                   MOVEL(P)  Q1PGM         CTPGM
     C                   Z-ADD     DSTIME        CTTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTDT
     C*
     C                   MOVEL(P)  CTUSER        CTIUSR
     C                   MOVEL(P)  CTPGM         CTIPGM
     C                   Z-ADD     CTTIME        CTITIM
     C                   Z-ADD     CTDT          CTIDT
     C*
     C                   WRITE     PCT01
     C                   ENDIF
     C*
     C                   SETON                                        LR
