     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWDEHP6 - RESET TRANSACTION DATE FOR WDEHP.
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     FWDEHP     UF   E             DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSDATE                 7     12  0

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
     C                   PARM                    QQREC#            9
     C                   PARM                    QQUPD             1
      *
     C                   MOVE      QQREC#        XXREC#            9 0
     C     QQUPD         IFEQ      'Y'
     C                   MOVE      *ON           *IN66
     C                   ELSE
     C                   MOVE      *OFF          *IN66
     C                   ENDIF
      *
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
      *
     C     XXREC#        CHAIN     PEH01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     EHFAF         IFEQ      'A'
      *
     C                   Z-ADD     EHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
      *
     C                   Z-ADD     EHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8 0
      *
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
      *
     C                   EXCEPT    DET
      *
     C     *IN66         IFEQ      *ON
     C                   EXSR      FIX
     C                   ENDIF
      *
     C                   ENDIF                                                  EHFAF IFEQ  EATRDT
      *
     C                   READ      PEH01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
     C                   MOVE      *ON           *INLR
     C*
     C*****************************************************************
     C*
     C     FIX           BEGSR
     C*
     C                   Z-ADD     0             EHTRDT
     C                   MOVEL(P)  Q1PGM         EHPGM
     C*
     C                   UPDATE    PEH01
     C*
     C                   ENDSR
     C*
     C*****************************************************************
      *
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'PROGRAM NAME:'
     O                                           25 'FIXWDEHP6'
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                                           24 'PGM TO CHECK IF WDEHP '
     O                                           42 'TRANS DT IS WRONG'
     O               66                          73 'UPDATE MODE IS ACTIVE'
     O          E            HEAD           2
     O                                            5 'CO#'
     O                                           11 'FUND'
     O                                           16 'FYR'
     O                                           27 'EMPLOYER#'
     O                                           34 'DIV'
     O                                           42 'STATUS'
     O                                           59 'EHTRDT'
     O                                           76 'EHEFDT'
     O          E            DET            1
     O                       EHCO#         4      5
     O                       EHFND         4     11
     O                       EHFYR         4     16
     O                       EHEMP#        4     27
     O                       EHDIV         4     34
     O                       EHFAF               39
     O                       XXTRDT              59 '  /  /    '
     O                       XXEFDT              76 '  /  /    '
