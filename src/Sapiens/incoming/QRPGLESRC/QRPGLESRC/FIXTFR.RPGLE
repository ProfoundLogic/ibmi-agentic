     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FIMDOL     IF   E           K DISK
     F                                     RENAME(PDO01:DO01)
     FSDCML6A   IF   E           K DISK
     FSMDLL     UF   E           K DISK
     FIMDOLA    UF   E           K DISK
     F                                     RENAME(PDO011:DO011)
     FIMDOLC    UF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C     CMK           KLIST
     C                   KFLD                    DODCID
     C*
     C     DLK           KLIST
     C                   KFLD                    SVKEY
     C                   KFLD                    SVDSQ#
     C*
     C*
     C     DO1K          KLIST
     C                   KFLD                    SVKEY
     C                   KFLD                    SVDSQ#
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *ALL'_'       LINE            198
     C                   EXCEPT    HEAD
     C                   READ      DO01                                   60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      DODKEY        SVKEY            40
     C                   MOVE      DODSQ#        SVDSQ#            5 0
     C                   MOVE      *BLANKS       XXDL
     C                   MOVE      *BLANKS       XXDO
     C                   MOVE      *BLANKS       XXDO1
     C     CMK           CHAIN     PCM01                              61
     C*
     C*** LOOP CMS
     C     LOOP1         TAG
     C     *IN61         IFEQ      *OFF
     C     CMIDOC        IFEQ      DODCID
     C     CMSEQ#        ANDEQ     DODSQ#
     C     CMPGM         ANDEQ     'W8700R  '
     C     DODCID        ANDNE     *BLANKS
     C*
     C                   MOVE      CMKEY         NWKEY            40
     C                   ADD       1             XXCNT             9 0
     C*
     C                   MOVE      *BLANK        XXDL              1
     C     DLK           CHAIN     PDL01                              62
     C     *IN62         IFEQ      *OFF
     C*
     C                   MOVEL     NWKEY         DLKEY
     C                   MOVEL     CMSEQ#        DLSEQ#
     C                   MOVEL     'FIXTFR'      DLPGM
     C                   UPDATE    PDL01
     C                   MOVE      'Y'           XXDL
     C                   ENDIF                                                         DL
     C     DO1K          CHAIN     DO011                              63
     C                   MOVE      *BLANKS       XXDO1             1
     C     *IN63         IFEQ      *OFF
     C                   MOVEL     NWKEY         DODKEY
     C                   MOVEL     CMSEQ#        DODSQ#
     C                   MOVEL(P)  'FIXTFR  '    DOLPGM
     C                   UPDATE    DO011
     C                   MOVE      'Y'           XXDO1
     C                   ENDIF
     C     DO1K          CHAIN     PDO01                              64
     C                   MOVE      *BLANKS       XXDO              1
     C     *IN64         IFEQ      *OFF
     C                   MOVEL     NWKEY         DODKEY
     C                   MOVEL     CMSEQ#        DODSQ#
     C                   MOVEL(P)  'FIXTFR  '    DOLPGM
     C                   UPDATE    PDO01
     C                   MOVE      'Y'           XXDO
     C                   ENDIF
     C                   EXCEPT    DET
     C                   ELSE
     C*
     C     CMK           READE     PCM01                                  61
     C                   GOTO      LOOP1
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                         CM
     C*
     C*
     C*
     C                   READ      DO01                                   60
     C                   ENDDO
     C                   EXCEPT    CNT
     C                   MOVE      *ON           *INLR
     C*
     O*
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       DSDATE        Y    198
     O          E            HEAD           2
     O                                           10 'FIXTFR'
     O          E            HEAD           2
     O                                           06 '      '
     O          E            HEAD           1
     O                                           15 'FROM KEY (DO)'
     O                                           59 'FROM SEQ#'
     O                                           77 'DOCID'
     O                                           93 'TO KEY (CM)'
     O                                          132 'TO SEQ #'
     O                                          139 'DL'
     O                                          143 'DO'
     O                                          148 'DO1'
      *
     O          E            HEAD        0  2
     O                       LINE               198
     O          E            DET         1
     O                       SVKEY               42
     O                       SVDSQ#        Z     59
     O                       DODCID              77
     O                       NWKEY              122
     O                       CMSEQ#             132
     O                       XXDL               139
     O                       XXDO               143
     O                       XXDO1              148
     O          E            CNT         1  1
     O                                           20 'COUNT:'
     O                       XXCNT         Z     31
