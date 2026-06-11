     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*================================================================
     H*   C:58   5.18.99    SUPPORT 17594, 17233, 16802
     H*
     H*    Remove extraneous characters at end of DOCID field.
     H*
     H*================================================================
     FSDCMP     UPE  E             DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D                 DS
     D  CMDCID                 1     12
     D  DOT1                   7      7
     D  DOT2                   8      8
     D  BLANK                 10     10
     D  BLANK2                11     12
     D  BLANKL                12     12
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     F*

     IPCM01         55

     C*=====================================================================
     C*
     C     *IN55         IFEQ      *ON
     C*
     C     BLANK2        IFNE      *BLANK
     C*
     C                   SELECT
     C*
     C     DOT1          WHENEQ    '.'
     C     BLANK         OREQ      ' '
     C                   MOVEL(P)  CMKEY         XXKEY            20
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   SETOFF                                       OF
     C                   ADD       1             XXCNT
     C                   MOVE      '  '          BLANK2
     C                   UPDATE    PCM01
     C*
     C     DOT2          WHENEQ    '.'
     C     BLANKL        ANDNE     *BLANK
     C                   MOVEL(P)  CMKEY         XXKEY            20
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   SETOFF                                       OF
     C                   ADD       1             XXCNT
     C                   MOVE      ' '           BLANKL
     C                   UPDATE    PCM01
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  . BLANK2<>
     C*
     C                   ENDIF                                                  . IN55 ON
     C*
     CLR                 EXCEPT    HEAD
     CLR                 EXCEPT    TOT
     C*
     C**************************************************************************
     C     *INZSR        BEGSR
     C*
     C                   Z-ADD     0             XXCNT             5 0
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   ENDSR
     C**************************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                                            8 'FIXDCID'
     O                                           66 'REMOVE EXTRANEOUS DATA'
     O                                           88 ' IN DOCUMENT ID FIELD '
     O                       DSDATE        Y    132
     O          E            HEAD           1
     O                                           22 'DARTS KEY             '
     O                                           44 'DOC ID BEFORE     DATE'
     O                                           78 'SEQ#'
     O                                           68 'INIT PGM'
     O                                           81 'INIT DATE'
     O                                           94 'INIT USER'
     O          E            DET            1
     O                       XXKEY               21
     O                       CMDCID              34
     O                       CMDCDT              46 '    /  /  '
     O                       CMSEQ#        4     58
     O                       CMIPGM              70
     O                       CMIDT               81 '    /  /  '
     O                       CMIUSR              95
     O          E            TOT            1
     O                                           25 'NBR RECORDS UPDATED: '
     O                       XXCNT         4     31
