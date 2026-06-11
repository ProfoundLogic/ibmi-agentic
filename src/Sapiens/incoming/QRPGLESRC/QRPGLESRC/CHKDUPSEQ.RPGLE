     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     FSDCMLA    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
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
     C     CMKY          KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     C*
     C*=====================================================================
     C*
     C                   Z-ADD     0             XXDCDT            6 0
     C                   MOVE      *BLANKS       XXCOMM           40
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*
     C                   READ      PCM01                                  50
     C     *IN50         DOWEQ     '0'
     C     CMPNAM        IFEQ      'W0070R'
     C*
     C*  LOAD THE ORIGINAL RECORD INFO INTO SAVE FIELDS
     C*
     C                   MOVE      CMRMCD        SVRMCD            5
     C                   MOVE      CMDIFL        SVDIFL            1
     C*
     C                   Z-ADD     CMDCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         SVDCDT            6 0
     C*
     C                   MOVEL     CMCOMM        SVCOMM           40
     C*
     C     CMKY          READE     PCM01                                  60
     C  N60              EXCEPT    DET
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     CMDCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDCDT
     C*
     C                   MOVEL     CMCOMM        XXCOMM
     C                   EXCEPT    DET1
     C     CMKY          READE     PCM01                                  60
     C                   ENDDO
     C*
     C   OF              EXCEPT    HEAD
     C*
     C                   END
     C*
     C     CMKY          SETGT     PCM01
     C                   READ      PCM01                                  50
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
     C**************************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'PROGRAM NAME:'
     O                                           25 'CHKDUPSEQ'
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                                           20 'THE REPORT LISTS ALL'
     O                                           40 'DARTS RECORDS WITH'
     O                                           63 'DUPLICATE SEQ#'
     O          E            HEAD           1
     O                                           28 'REMARK'
     O                                           35 'DIARY'
     O                                           44 'DIARY'
     O          E            HEAD           1
     O                                            4 'CO#'
     O                                            8 'FND'
     O                                           12 'FYR'
     O                                           20 'CASE'
     O                                           27 'CODE'
     O                                           35 'FLAG'
     O                                           46 'CMPLT DT'
     O                                           56 'COMMENT'
     O                                          100 'SEQ#'
     O          E            DET         1  1
     O                       XXCO#         4      4
     O                       XXFND         4      8
     O                       XXFYR         4     12
     O                       XXCASE        4     20
     O                       SVRMCD              28
     O                       SVDIFL              33
     O                       SVDCDT              46 '  /  /  '
     O                       SVCOMM              89
     O                       CMSEQ#        4    100
     O          E            DET1           1
     O                       XXCO#         4      4
     O                       XXFND         4      8
     O                       XXFYR         4     12
     O                       XXCASE        4     20
     O                       CMRMCD              28
     O                       CMDIFL              33
     O                       XXDCDT              46 '  /  /  '
     O                       XXCOMM              89
     O                       CMSEQ#        4    100
