     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVCFLAST  - CONVERT CMAST TO FL IF BLANKS
     FFMCML1    UF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
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
     C                   TIME                    DSDTTM
     C*
     C*** READ CLAIM MASTER FILE
     C                   READ      PCM01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C*** IF CMAST IS EQUAL TO BLANKS UPDATE IT WITH 'FL'
     C     CMAST         IFEQ      *BLANKS
     C                   MOVEL     'FL'          CMAST
     C                   MOVEL     'PDSPGMR'     CMUSER
     C                   MOVEL     'CVCFLAST'    CMPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*
     C                   Z-ADD     DSTIME        CMTIME
     C                   UPDATE    PCM01
     C                   END                                                    END CMAST
     C*
     C*** READ NEXT RECORD
     C                   READ      PCM01                                  60
     C                   END                                                    END DOWEQ *IN60
     C*
     C                   MOVE      '1'           *INLR
