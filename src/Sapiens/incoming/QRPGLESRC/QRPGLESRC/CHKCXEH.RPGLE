     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKCXEH - CHECK FOR WDCXP AFTER LAST AUDIT WDEHP RECORD.
     H*****************************************************************
     H*
     H*  PROGRAM TO CHECK FOR WDCXP RECORDS AFTER THE LAST AUDIT
     H*  WDEHP RECORD.
     H*
     H*****************************************************************
     FWDELL1    IF   E           K DISK
     FWDEHL6    IF   E           K DISK
     FWDCXL2    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSDATE                 7     12  0

     C*
     C     KEY1          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   TIME                    DSDTTM
     C*
     C                   EXCEPT    HEAD
     C*
     C                   READ      PEL01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     KEY1          SETGT     PCX01
     C     KEY1          READPE    PCX01                                  60
     C     *IN60         IFEQ      *OFF
     C*
     C     KEY1          SETGT     PEH01
     C     KEY1          READPE    PEH01                                  60
     C*
     C     *IN60         IFEQ      *ON
     C     *IN60         OREQ      *OFF
     C     EHFAF         ANDNE     'A'
     C     *IN60         OREQ      *OFF
     C     EHFAF         ANDEQ     'A'
     C     EHTRDT        ANDLT     CXCDAT
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANK        EHFAF
     C                   Z-ADD     0             EHTRDT
     C                   ENDIF
     C*
     C                   Z-ADD     EHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
     C                   Z-ADD     CXCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCDAT            8 0
     C*
     C                   EXCEPT    DET
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   READ      PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'PROGRAM NAME:'
     O                                           25 'CHKCXEH  '
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                                           24 'PGM TO CHECK FOR WDCXP'
     O                                           43 'AFTER AUDIT WDEHP.'
     O                                           75 'PAGE:'
     O                       PAGE                80
     O          E            HEAD           2
     O                                            5 'CO#'
     O                                           11 'FUND'
     O                                           16 'FYR'
     O                                           27 'EMPLOYER#'
     O                                           34 'DIV'
     O                                           41 'EHFAF'
     O                                           55 'EHTRDT'
     O                                           68 'CXCDAT'
     O          E            DET            1
     O                       ELCO#         4      5
     O                       ELFND         4     11
     O                       ELFYR         4     16
     O                       ELEMP#        4     27
     O                       ELDIV         4     34
     O                       EHFAF               39
     O                       XXTRDT              55 '  /  /    '
     O                       XXCDAT              68 '  /  /    '
