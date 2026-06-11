     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE CVTKY010
     H*****************************************************************
     H* Description:  This pgm calls F6251R for all days within a     *
     H*   date range which is passed into the pgm.                    *
     H*                                                               *
     H* Programmer: 15     Date Created: 03/24/00                     *
     H*****************************************************************
      *

     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   Z-ADD     0             XXDATE            7 0
     C*
     C                   MOVE      QQTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT            7 0
     C*
     C*  Call F6251R for all days between the entered dates.
     C*
     C     XXDATE        DOUGT     XXTODT
     C*
     C                   CALL      'F6251R'
     C                   PARM                    QQCO#
     C                   PARM                    QQFRDT
     C                   PARM      'W'           QQCALL            1
     C*
     C                   MOVE      QQFRDT        SRDATE
     C                   Z-ADD     1             SRDAYS
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE            8 0
     C                   PARM                    SRDAYS            5 0
     C                   PARM      *BLANKS       SRERR             1
     C                   MOVE      SRDATE        QQFRDT
     C*
     C                   MOVE      QQFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   ENDDO
     C*
     C*  End of program
     C*
     C                   SETON                                        LR
     C*
