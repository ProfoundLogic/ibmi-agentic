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
     FWDF2L     IF   E           K DISK
     FWDELL3    IF   E           K DISK
     FWMRPL     UF A E           K DISK
      *

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D                 DS
     D  XXOPOL                 1     11
     D  XXFND                  1      3  0
     D  XXOEMP                 4      8  0
     D  XXFYR                  9     11  0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPFDT
      *
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELPFDT        XXPFDT
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
     C                   Z-ADD     F2CO#         XXCO#
     C                   Z-ADD     F2FND         XXFND
     C                   Z-ADD     F2DT01        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQDT
      *
     C                   Z-ADD     1             QQDAYS
      *
     C                   CALL      'SRPAST8'
     C                   PARM                    QQDT              8 0
     C                   PARM                    QQDAYS            5 0
     C                   PARM                    QQERR             1
      *
     C     QQERR         IFNE      *ON
      *
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPFDT
      *
     C                   ELSE
     C                   Z-ADD     F2DT01        XXPFDT
      *
     C                   ENDIF                                                  QQERR IFNE *ON
      *
     C     ELKY          SETGT     PEL01
     C     ELKY1         READPE    PEL01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   Z-ADD     ELDIV         XXDIV
     C                   Z-ADD     ELFYR         XXFYR
     C                   MOVE      *OFF          *IN61
      *
     C     RPKY          CHAIN     PRP01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     RPCAP#        IFNE      *BLANKS
     C                   MOVE      *ON           *IN61
     C                   ENDIF                                                  RPCAP# IFNE *BL
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C     *IN61         IFEQ      *OFF
      *
     C                   TIME                    DSDTTM
      *
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PRP01
     C                   Z-ADD     ELEMP#        XXOEMP
     C                   MOVEL(P)  XXOPOL        RPCAP#
     C                   Z-ADD     ELCO#         RPCO#
     C                   Z-ADD     ELFND         RPFND
     C                   Z-ADD     ELEMP#        RPEMP#
     C                   Z-ADD     ELDIV         RPDIV
     C                   Z-ADD     ELFYR         RPFYR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPIDT
     C                   Z-ADD     DSTIME        RPITM
     C                   MOVEL     Q1USER        RPIUSR
     C                   MOVEL     Q1PGM         RPIPGM
     C                   Z-ADD     RPIDT         RPDT
     C                   Z-ADD     RPITM         RPTIME
     C                   MOVEL     RPIUSR        RPUSER
     C                   MOVEL     RPIPGM        RPPGM
      *
     C                   WRITE     PRP01
      *
     C                   ELSE
      *
     C                   Z-ADD     ELEMP#        XXOEMP
     C                   MOVEL(P)  XXOPOL        RPCAP#
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   Z-ADD     DSTIME        RPTIME
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
      *
     C                   UPDATE    PRP01
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *
     C                   ENDIF                                                  *IN61 IFEQ *ON
      *
     C     ELKY1         READPE    PEL01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
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
