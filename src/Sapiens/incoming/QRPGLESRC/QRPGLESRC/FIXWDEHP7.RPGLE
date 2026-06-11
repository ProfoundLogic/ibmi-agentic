     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWDEHP7 - CORRECT TRANS DATE ON FIXED RECORDS
     H*****************************************************************
     H*                                                               *
     H* FINDS THE CORRECT TRANSACTION DATE FOR ANY RECORD WHICH WAS   *
     H* DESIGNATED WITH THE 1/1/2008 DATE.                            *
     H*                                                               *
     H*****************************************************************
     FWDEHP     UF   E             DISK

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
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQD               2
      *
     C                   MOVE      QQD           XXD               2 0
      *
     C                   READ      PEH01                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C     EHTRDT        IFEQ      1080101
     C*
     C                   Z-ADD     XXD           QQDAYS
     C                   Z-ADD     EHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQDT
     C                   CALL      'SRPAST8'
     C                   PARM                    QQDT              8 0
     C                   PARM                    QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHTRDT
     C                   MOVEL(P)  Q1PGM         EHPGM
      *
     C                   UPDATE    PEH01
      *
     C                   ENDIF                                                  EHTRDT IFEQ ON
      *
     C                   READ      PEH01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
     C                   MOVE      *ON           *INLR
