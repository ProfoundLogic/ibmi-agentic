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
     FWDF2L     UF   E           K DISK
     FWDELL3    IF   E           K DISK
      *

     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
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
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
      *
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
      *
     C     XXFND         IFNE      0
     C     F2KY          CHAIN     PF201                              60
     C                   ELSE
     C                   READ      PF201                                  60
     C                   ENDIF                                                  XXFND IFNE 0
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVE      F2CO#         XXCO#
     C                   MOVE      F2FND         XXFND
      *
     C     ELKY          SETGT     PEL01
     C     ELKY          READPE    PEL01                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQDT
      *
     C                   Z-ADD     1             QQDAYS
      *
     C                   CALL      'SRFUTR8'
     C                   PARM                    QQDT              8 0
     C                   PARM                    QQDAYS            5 0
     C                   PARM                    QQERR             1
      *
     C     QQERR         IFNE      *ON
      *
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         F2DT01
      *
     C                   UPDATE    PF201
      *
     C                   ENDIF                                                  QQERR IFNE *ON
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C     QQFND         IFNE      '000'
     C                   MOVE      *ON           *IN60
     C                   ELSE
     C                   READ      PF201                                  60
     C                   ENDIF                                                  QQFND IFNE '000'
      *
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
