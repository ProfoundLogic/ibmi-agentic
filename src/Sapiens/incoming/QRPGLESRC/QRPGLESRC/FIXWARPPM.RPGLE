     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*
     H* REVERSE AR RECORDS FOR TASB'S BATCH # 5883 AND 5911
     H*
     H*****************************************************************
     FWARPPM    IF   E           K DISK
     FWARPP     O    E           K DISK
     F                                     RENAME(PRP01:PRP02)

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*================================================================
     C*
     C                   READ      PRP01                                  99
     C     *IN99         DOWEQ     '0'
     C*
     C     RPSTAT        IFEQ      'Y'
     C*
     C                   Z-ADD     0             RPCHK#
     C                   Z-ADD     0             RPBUL1
     C                   MOVE      *BLANK        RPSTAT
     C                   MOVE      *BLANKS       RPACCT
     C                   WRITE     PRP02
     C*
     C                   ENDIF
     C*
     C                   READ      PRP01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*
     C*****************************************************************
