     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXAUDIT - WRITE 'D' AUDIT RECORD FOR AUDITS WITHOUT ONE.
     H*
     FWDELL     IF   E           K DISK
     FWMEAL4    IF A E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D AUDITD          C                   CONST('AUDIT TRANSACTION')
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     EAKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    XXTYPE            1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C* READ EMPLOYER HISTORY
     C*
     C                   READ      PEL01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     ELFAF         IFEQ      'A'
     C*
     C                   MOVE      'D'           XXTYPE
     C*
     C     EAKY          CHAIN     PEA01                              60
     C     *IN60         IFEQ      *ON
     C*
     C                   Z-ADD     ELPTDT        XXDT              7 0
     C                   MOVE      'B'           XXTYPE
     C*
     C     EAKY          SETGT     PEA01
     C     EAKY          READPE    PEA01                                  60
     C     *IN60         IFEQ      *OFF
     C     EAPODT        ANDGT     XXDT
     C     *IN60         OREQ      *OFF
     C     EATRDT        ANDGT     XXDT
     C*
     C     EAPODT        IFGT      XXDT
     C                   Z-ADD     EAPODT        XXDT
     C                   ENDIF
     C     EATRDT        IFGT      XXDT
     C                   Z-ADD     EATRDT        XXDT
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C                   MOVE      'A'           XXTYPE
     C*
     C     EAKY          SETGT     PEA01
     C     EAKY          READPE    PEA01                                  60
     C     *IN60         IFEQ      *OFF
     C     EAPODT        ANDGT     XXDT
     C     *IN60         OREQ      *OFF
     C     EATRDT        ANDGT     XXDT
     C*
     C     EAPODT        IFGT      XXDT
     C                   Z-ADD     EAPODT        XXDT
     C                   ENDIF
     C     EATRDT        IFGT      XXDT
     C                   Z-ADD     EATRDT        XXDT
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   TIME                    DSDTTM
     C*
     C                   CLEAR                   PEA01
     C                   Z-ADD     ELCO#         EACO#
     C                   Z-ADD     ELFND         EAFND
     C                   Z-ADD     ELEMP#        EAEMP#
     C                   Z-ADD     ELDIV         EADIV
     C                   Z-ADD     ELFYR         EAFYR
     C                   Z-ADD     0             EAAMT
     C                   MOVE      'D'           EAARTY
     C                   MOVE      'AT'          EAARCD
     C                   Z-ADD     XXDT          EATRDT
     C                   Z-ADD     XXDT          EAPODT
     C                   MOVEL     AUDITD        EADESC
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   Z-ADD     DSTIME        EATIME
     C                   MOVE      ' '           EAEDIT
     C                   Z-ADD     0             EABUL1
     C                   Z-ADD     0             EACHK#
     C                   MOVE      *BLANK        EALATE
     C*
     C                   WRITE     PEA01
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  ELFAF IFEQ
     C*
     C* READ NEXT EMPLOYER
     C*
     C                   READ      PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*
     C*****************************************************************
