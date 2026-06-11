     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     FSDCMP     UF   E             DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     F*

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*=====================================================================
     C*
     C                   Z-ADD     0             XXCNT             5 0
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*================================================================
     C*   Remove all records written by W9653R.
     C*
     C                   READ      PCM01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     CMIPGM        IFEQ      'W9653R'
     C*
     C                   MOVEL(P)  CMKEY         XXKEY            20
     C                   MOVEL(P)  CMCOMM        XXCOMM           20
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       1             XXCNT
     C*
     C                   DELETE    PCM01
     C                   ENDIF
     C*
     C                   READ      PCM01                                  60
     C                   ENDDO
     C*
     C                   EXCEPT    TOT
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C**************************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                                            8 'FIXSDCMP'
     O                                           68 'REMOVE DARTS RECORDS'
     O                                           86 'CREATED BY W9653R'
     O                       DSDATE        Y    132
     O          E            HEAD           1
     O                                           28 'REMARK'
     O                                           35 'DIARY'
     O                                           44 'DIARY'
     O          E            HEAD           1
     O                                            9 'DARTS KEY'
     O                                           27 'CODE'
     O                                           35 'FLAG'
     O                                           48 'CMPLT DATE'
     O                                           58 'COMMENT'
     O                                           78 'SEQ#'
     O                                           88 'INIT PGM'
     O                                          101 'INIT DATE'
     O                                          114 'INIT USER'
     O                                          123 'CMPNAM'
     O          E            DET            1
     O                       XXKEY               21
     O                       CMRMCD              28
     O                       CMDIFL              33
     O                       CMDCDT              48 '    /  /  '
     O                       XXCOMM              71
     O                       CMSEQ#        4     78
     O                       CMIPGM              90
     O                       CMIDT              101 '    /  /  '
     O                       CMIUSR             115
     O                       CMPNAM             127
     O          E            TOT            1
     O                                           25 '# of records deleted:'
     O                       XXCNT         4     31
