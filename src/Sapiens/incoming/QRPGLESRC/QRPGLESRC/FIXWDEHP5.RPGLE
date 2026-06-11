     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKWDEHP5 - CHECK/FIX EHFAF/EHFAF2 STATUS ON WDEHP RCDS.
     H*****************************************************************
     H*
     H*  PROGRAM TO CLEAN UP WDEHP RECORDS.  IT CHECKS IF THERE ARE
     H*  INVALID EHFAF/EHFAF2 VALUES.
     H*  THE PROGRAM CAN EITHER BE RUN FOR EVERYONE (1 PARM)
     H*  OR IT CAN BE RUN FOR ONE EMPLR/DIV/FYR (ALL PARMS).
     H*
     H*****************************************************************
     FWDEHL6    UF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSDATE                 7     12  0
     D                 DS
     D  EHTRDT                 1      7  0
     D  XXTRYM                 1      5  0
     D                 DS
     D  EHPFDT                 1      7  0
     D  XXPFYM                 1      5  0

     C*
     C     EHKY          KLIST
     C                   KFLD                    SVCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    SVFYR
     C*
     C     *LIKE         DEFINE    EHCO#         XXCO#
     C     *LIKE         DEFINE    EHFND         XXFND
     C     *LIKE         DEFINE    EHFYR         XXFYR
     C     *LIKE         DEFINE    EHEMP#        XXEMP#
     C     *LIKE         DEFINE    EHDIV         XXDIV
     C*
     C     *LIKE         DEFINE    EHCO#         SVCO#
     C     *LIKE         DEFINE    EHFND         SVFND
     C     *LIKE         DEFINE    EHFYR         SVFYR
     C     *LIKE         DEFINE    EHEMP#        SVEMP#
     C     *LIKE         DEFINE    EHDIV         SVDIV
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQUPD             1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     QQUPD         IFEQ      'Y'
     C                   MOVE      *ON           *IN66
     C                   ELSE
     C                   MOVE      *OFF          *IN66
     C                   ENDIF
     C*
     C                   Z-ADD     0             XXCO#
     C                   Z-ADD     0             XXFND
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C                   Z-ADD     0             XXFYR
     C                   Z-ADD     0             SVTRYM            5 0
     C*
     C                   MOVE      *BLANK        XX1ST             1
     C                   MOVE      *OFF          *IN67
     C*
     C     Q1PRMS        IFGT      1
     C                   MOVE      *ON           *IN67
     C                   MOVE      QQCO#         SVCO#
     C                   MOVE      QQFND         SVFND
     C                   MOVE      QQEMP#        SVEMP#
     C                   MOVE      QQDIV         SVDIV
     C                   MOVE      QQFYR         SVFYR
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*
     C                   EXCEPT    HEAD
     C*
     C  N67              READ      PEH01                                  60
     C   67EHKY          CHAIN     PEH01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *BLANK        XXUP              1
     C*
     C     EHCO#         IFNE      XXCO#
     C     EHFND         ORNE      XXFND
     C     EHFYR         ORNE      XXFYR
     C     EHEMP#        ORNE      XXEMP#
     C     EHDIV         ORNE      XXDIV
     C*
     C                   MOVE      'Y'           XX1ST
     C                   MOVEL     EHTRDT        SVTRYM
     C                   Z-ADD     EHCO#         XXCO#
     C                   Z-ADD     EHFND         XXFND
     C                   Z-ADD     EHFYR         XXFYR
     C                   Z-ADD     EHEMP#        XXEMP#
     C                   Z-ADD     EHDIV         XXDIV
     C*
     C                   ENDIF
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C     XX1ST         IFEQ      ' '
     C     EHFAF         ANDEQ     'E'
     C     EHRWFL        ANDNE     'Y'
     C*
     C                   Z-ADD     EHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
     C                   Z-ADD     EHPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT            8 0
     C*
     C                   EXCEPT    DET
     C*
     C     *IN66         IFEQ      *ON
     C                   EXSR      FIX
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     XX1ST         IFEQ      ' '
     C     EHFAF         ANDEQ     'R'
     C     EHFAF2        ANDEQ     'E'
     C     XXTRYM        ANDGT     SVTRYM
     C     XXTRYM        ANDGT     XXPFYM
     C     EHRWFL        ANDNE     'Y'
     C*
     C                   Z-ADD     EHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
     C                   Z-ADD     EHPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT            8 0
     C*
     C                   EXCEPT    DET
     C*
     C     *IN66         IFEQ      *ON
     C                   EXSR      FIX2
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     XXUP          IFEQ      'Y'
     C                   UPDATE    PEH01
     C                   MOVE      *BLANK        XXUP
     C                   ENDIF
     C*
     C                   MOVE      ' '           XX1ST
     C*
     C  N67              READ      PEH01                                  60
     C   67EHKY          READE     PEH01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C*
     C     FIX           BEGSR
     C*
     C*  WRITTEN PREMIUM FIELDS
     C*
     C                   MOVE      'R'           EHFAF
     C                   MOVE      'E'           EHFAF2
     C*
     C                   MOVEL(P)  Q1PGM         EHPGM
     C*
     C                   MOVE      'Y'           XXUP              1
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*
     C     FIX2          BEGSR
     C*
     C*  WRITTEN PREMIUM FIELDS
     C*
     C                   MOVE      'R'           EHFAF
     C                   MOVE      ' '           EHFAF2
     C*
     C                   MOVEL(P)  Q1PGM         EHPGM
     C*
     C                   MOVE      'Y'           XXUP              1
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'PROGRAM NAME:'
     O                                           25 'FIXWDEHP5'
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                                           24 'PGM TO CHECK IF WDEHP '
     O                                           42 'EHFAF IS CORRECT.'
     O               66                          70 'UPDATE MODE IS ACTIVE'
     O                       PAGE                80
     O          E            HEAD           2
     O                                            5 'CO#'
     O                                           11 'FUND'
     O                                           16 'FYR'
     O                                           27 'EMPLOYER#'
     O                                           34 'DIV'
     O                                           41 'EHFAF'
     O                                           49 'EHFAF2'
     O                                           60 'EHTRDT'
     O                                           73 'EHPFDT'
     O          E            DET            1
     O                       EHCO#         4      5
     O                       EHFND         4     11
     O                       EHFYR         4     16
     O                       EHEMP#        4     27
     O                       EHDIV         4     34
     O                       EHFAF               39
     O                       EHFAF2              46
     O                       XXTRDT              60 '  /  /    '
     O                       XXPFDT              73 '  /  /    '
