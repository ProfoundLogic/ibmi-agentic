     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWDEHP3 - FIX WDEHP FOR FIRST RECORD ON EMPLR IS REVISED
     H*
     FWDEHL6    UF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSDATE                 7     12  0

     C*
     C     *LIKE         DEFINE    EHCO#         XXCO#
     C     *LIKE         DEFINE    EHFND         XXFND
     C     *LIKE         DEFINE    EHEMP#        XXEMP#
     C     *LIKE         DEFINE    EHDIV         XXDIV
     C     *LIKE         DEFINE    EHFYR         XXFYR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQUPD             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     QQUPD         IFEQ      'Y'
     C                   MOVE      *ON           *IN66
     C                   ELSE
     C                   MOVE      *OFF          *IN66
     C                   ENDIF
     C*
     C                   Z-ADD     0             XXCO#
     C                   Z-ADD     0             XXFND
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C                   Z-ADD     0             XXFYR
     C*
     C                   TIME                    DSDTTM
     C*
     C                   EXCEPT    HEAD
     C*
     C                   READ      PEH01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     EHCO#         IFNE      XXCO#
     C     EHFND         ORNE      XXFND
     C     EHEMP#        ORNE      XXEMP#
     C     EHDIV         ORNE      XXDIV
     C     EHFYR         ORNE      XXFYR
     C*
     C                   Z-ADD     EHCO#         XXCO#
     C                   Z-ADD     EHFND         XXFND
     C                   Z-ADD     EHEMP#        XXEMP#
     C                   Z-ADD     EHDIV         XXDIV
     C                   Z-ADD     EHFYR         XXFYR
     C*
     C     EHFAF         IFEQ      'R'
     C     EHXAPR        ANDEQ     *ZERO
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   Z-ADD     EHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C                   EXCEPT    DET
     C*
     C*  WRITTEN PREMIUM FIELDS
     C*
     C                   Z-ADD     EHWMPR        EHXMPR
     C                   Z-ADD     EHWSPR        EHXSPR
     C                   Z-ADD     EHWNMP        EHXNMP
     C                   Z-ADD     EHWAPR        EHXAPR
     C                   Z-ADD     EHWDC1        EHXDC1
     C                   Z-ADD     EHWDC2        EHXDC2
     C                   Z-ADD     EHWDC3        EHXDC3
     C                   Z-ADD     EHWXP1        EHXXP1
     C                   Z-ADD     EHWXP2        EHXXP2
     C                   Z-ADD     EHWXP3        EHXXP3
     C                   Z-ADD     EHWIN1        EHXIN1
     C                   Z-ADD     EHWIN2        EHXIN2
     C                   Z-ADD     EHWDCT        EHXDCT
     C                   Z-ADD     EHWDCM        EHXDCM
     C                   Z-ADD     EHWDCS        EHXDCS
     C                   Z-ADD     EHWDCR        EHXDCR
     C                   Z-ADD     EHWFRS        EHXFRS
     C                   Z-ADD     EHWDV2        EHXDV2
     C*
     C*  INFORCED PREMIUM FIELDS
     C*
     C                   Z-ADD     EHIMPR        EHJMPR
     C                   Z-ADD     EHISPR        EHJSPR
     C                   Z-ADD     EHINMP        EHJNMP
     C                   Z-ADD     EHIAPR        EHJAPR
     C                   Z-ADD     EHIDC1        EHJDC1
     C                   Z-ADD     EHIDC2        EHJDC2
     C                   Z-ADD     EHIDC3        EHJDC3
     C                   Z-ADD     EHIXP1        EHJXP1
     C                   Z-ADD     EHIXP2        EHJXP2
     C                   Z-ADD     EHIXP3        EHJXP3
     C                   Z-ADD     EHIIN1        EHJIN1
     C                   Z-ADD     EHIIN2        EHJIN2
     C                   Z-ADD     EHIDCT        EHJDCT
     C                   Z-ADD     EHIDCM        EHJDCM
     C                   Z-ADD     EHIDCS        EHJDCS
     C                   Z-ADD     EHIDCR        EHJDCR
     C                   Z-ADD     EHIFRS        EHJFRS
     C                   Z-ADD     EHIDV2        EHJDV2
     C*
     C     *IN66         IFEQ      *ON
     C                   UPDATE    PEH01
     C                   ENDIF
     C*
     C                   ENDIF                                                  EHFAF IFEQ
     C*
     C                   ENDIF                                                  EHCO# IFNE
     C*
     C                   READ      PEH01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'PROGRAM NAME:'
     O                                           25 'FIXWDEHP3'
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                                           24 'PGM TO CORRECT WDEHP '
     O                                           47 'WHEN 1ST RCD IS REVISED'
     O               66                          77 'UPDATE MODE IS SET'
     O          E            HEAD           2
     O                                            5 'CO#'
     O                                           11 'FUND'
     O                                           16 'FYR'
     O                                           27 'EMPLOYER#'
     O                                           34 'DIV'
     O                                           46 'EHTRDT'
     O          E            DET            1
     O                       EHCO#         4      5
     O                       EHFND         4     11
     O                       EHFYR         4     16
     O                       EHEMP#        4     27
     O                       EHDIV         4     34
     O                       XXTRDT              45 '  /  /    '
