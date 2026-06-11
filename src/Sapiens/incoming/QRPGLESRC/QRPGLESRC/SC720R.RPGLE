     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* TITLE SC720R - WORKERS COMPENSATION EXTRACT CREATION          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
      *****************************************************************
     FSMHDP     IF   E             DISK
     FSMWCP     O    E             DISK
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*DATA STRUCTURE FOR W0050R INSTALLATION
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C     *ENTRY        PLIST
     C                   PARM                    QQCSID            2
     C                   PARM                    QQPGM            10
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
     C*  INITIALIZE WORK FIELDS
     C*
     C                   TIME                    DSDTTM
     C*
     C*** BEGIN READING HEADER RECORDS
     C*
     C                   READ      PHD01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C*  CHECK TO SEE THAT THE REMARK HEADER CSID IS THE SAME
     C*
     C     QQCSID        IFEQ      HDCSID
     C*
     C*  CHECK THE PROGRAM FOR THE DATA FIELD THAT NEED TO BE MOVED
     C*
     C     QQPGM         IFEQ      HDPNAM
     C*
     C                   MOVE      HDKEY         QCKEY
     C                   MOVE      QCCO#         HDCO#
     C                   MOVE      QCFND         HDFND
     C                   MOVE      QCEMP#        HDEMP#
     C                   MOVE      QCDIV         HDDIV
     C                   MOVE      *BLANK        HDFEIN
     C                   MOVE      *BLANK        HDMOD
     C                   MOVE      *BLANK        HDFYR
     C                   MOVE      *BLANK        HDRGN#
     C                   MOVE      *BLANK        HDSS#
     C                   MOVE      *BLANK        HDCASE
     C                   MOVE      *BLANK        HDAGT#
     C*
     C                   MOVE      Q1USER        HDUSER
     C                   MOVE      Q1PGM         HDPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HDDT
     C*
     C                   Z-ADD     DSTIME        HDTIME
     C                   WRITE     PWC01
     C                   END
     C*
     C                   END
     C                   READ      PHD01                                  50
     C                   END
     C                   MOVE      '1'           *INLR
