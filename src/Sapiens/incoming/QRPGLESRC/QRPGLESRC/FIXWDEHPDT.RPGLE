     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWDEHPDT
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     FWMEALT    IF   E           K DISK
     FWDEHL     UF   E           K DISK

     C*
     C     EHKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C                   KFLD                    EAFYR
     C                   KFLD                    XXFAF
     C                   KFLD                    XXPODT
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXARTY
     C                   KFLD                    XXTRDT
     C*
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXARTY
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQV3DT            7
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
      *
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQV3DT        XXTRDT            7 0
     C                   MOVE      'A'           XXFAF             1
     C                   MOVE      'D'           XXARTY            1
      *
     C     EAKY          SETLL     PEA01
     C     EAKY1         READE     PEA01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   Z-ADD     EAPODT        XXPODT            7 0
      *
     C     EHKY          CHAIN     PEH01                              60
      *
     C     *IN60         IFEQ      *ON
      *
     C                   Z-ADD     30            QQDAYS
     C                   Z-ADD     EAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQDT
     C                   CALL      'SRFUTR8'
     C                   PARM                    QQDT              8 0
     C                   PARM                    QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
      *
     C     EHKY          CHAIN     PEH01                              60
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *
     C     *IN60         IFEQ      *OFF
      *
     C     EHTRDT        IFNE      EATRDT
     C     EHEFDT        ORNE      EAPODT
     C                   Z-ADD     EATRDT        EHTRDT
     C                   Z-ADD     EAPODT        EHEFDT
     C                   UPDATE    PEH01
     C                   ENDIF                                                  EHTRDT IFNE EATRDT
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C     EAKY1         READE     PEA01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
     C                   MOVE      *ON           *INLR
