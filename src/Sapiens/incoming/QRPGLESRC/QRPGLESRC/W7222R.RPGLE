     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7222R - Build Work File for MONTHLY INTERIM PAYROLL RPT
     H*****************************************************************
     H** REVISIONS:                                                  **
     H*                                                               *
     H*  10/10/95 C:11 PROJECT WC453                                  *
     H*                GET THE LAST ACTIVE AGENT AS OF THE FYR, NOT   *
     H*                AS OF THE RUN DATE.                            *
     H*****************************************************************
     FWXMIP     O    E             DISK
     FWMEML     IF   E           K DISK
     FWMAHL     IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Key List for the Employer Master File
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     EMKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C* Key List for the Agent History Master File
     C*
     C     AHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
     C*
     C     *LIKE         DEFINE    EMDT          X1DATE
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             2
     C                   PARM                    QQDATE            8
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQSRT             1
     C                   PARM                    QQFAGT            5
     C                   PARM                    QQTAGT            5
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
     C* Set Up and Initialize
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQDATE        XXDATE            8 0
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1DATE
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFAGT        XXFAGT            5 0
     C                   MOVE      QQTAGT        XXTAGT            5 0
     C                   TIME                    DSDTTM
     C*
     C     QQSRT         IFEQ      'E'
     C                   Z-ADD     0             XXFAGT
     C                   Z-ADD     *HIVAL        XXTAGT
     C                   END
     C*
     C* Get the Employer Master Record
     C*
     C     XXEMP#        IFEQ      0
     C     EMKEY         CHAIN     PEM01                              60
     C                   ELSE
     C     EMKEY1        CHAIN     PEM01                              60
     C                   END
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C* Get the Agent # & COMPARE ON RANGE
     C*
     C     AHKEY         SETGT     PAH01
     C                   READP     PAH01                                  50
     C     *IN50         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  50
     C                   END
     C*
     C     *IN50         IFEQ      '1'
     C     XXCO#         ORNE      AHCO#
     C     XXFND         ORNE      AHFND
     C     EMEMP#        ORNE      AHEMP#
     C     EMDIV         ORNE      AHDIV
     C     XXFYR         ORNE      AHFYR
     C                   Z-ADD     0             AHAGT#
     C                   END
     C*
     C     AHAGT#        IFGE      XXFAGT
     C     AHAGT#        ANDLE     XXTAGT
     C*
     C* Move Information to Work File Fields
     C*
     C                   Z-ADD     EMCO#         MICO#             3 0
     C                   Z-ADD     EMFND         MIFND             3 0
     C                   Z-ADD     EMEMP#        MIEMP#
     C                   Z-ADD     EMDIV         MIDIV
     C                   Z-ADD     AHAGT#        MIAGT#            5 0
     C                   MOVE      *BLANKS       MISTAT            1
     C                   MOVE      Q1USER        MIUSER           10
     C                   MOVE      Q1PGM         MIPGM            10
     C                   MOVE      DSTIME        MITIME            6 0
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MIDT
     C                   WRITE     PMI01
     C*
     C                   END
     C*
     C* Get the Next Employer
     C*
     C     XXEMP#        IFEQ      0
     C     EMKEY         READE     PEM01                                  60
     C                   ELSE
     C     EMKEY1        READE     PEM01                                  60
     C                   END
     C*
     C                   END
     C*
     C                   MOVE      '1'           *INLR
