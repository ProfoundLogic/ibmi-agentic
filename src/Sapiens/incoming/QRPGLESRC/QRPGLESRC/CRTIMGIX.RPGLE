     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     FIAUSL     IF   E           K DISK
     FIMIQL     UF A E           K DISK
     F*

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  CMKEY                  1     40
     D  XXCO#                  1      3  0
     D  XXFND                  4      6  0
     D  XXFYR                  7      9  0
     D  XXCASE                10     16  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*** DARTS FILE
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
     C                   Z-ADD     0             XXDCDT            6 0
     C                   MOVE      *BLANKS       XXCOMM           40
     C                   TIME                    DSDTTM
     C*
     C                   READ      QLIDOBJD                               50
     C     *IN50         DOWEQ     '0'
     C     ODOBNM        CHAIN     PIQ01                              60
     C*
     C                   MOVEL     ODOBNM        XXUSRI           10
     C                   MOVEL     ODOBTX        XXDESC           40
     C                   MOVEL     XXUSRI        IQID
     C                   MOVEL     XXUSRI        IQMSGQ
     C                   MOVEL     XXDESC        IQDESC
     C                   MOVEL     *BLANK        IQSECV
     C                   MOVEL     'A'           IQSTAT
     C                   MOVEL     'CRTIMGIX'    IQIPGM
     C                   MOVEL     'PROGRAM'     IQIUSR
     C                   MOVEL     'CLMSC'       IQDOCT
     C                   MOVEL     'Y'           IQFL01
     C                   MOVEL     'N'           IQFL02
     C                   MOVEL     'N'           IQFL03
     C  N60              UPDATE    PIQ01
     C   60              WRITE     PIQ01
     C                   READ      QLIDOBJD                               50
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
     C**************************************************************************
