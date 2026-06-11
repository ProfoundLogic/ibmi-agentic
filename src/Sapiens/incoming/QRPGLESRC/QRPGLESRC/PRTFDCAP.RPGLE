     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PRTFDCAP - PRINT FDCAP IF DUPLICATES FOUND
     FFDCALF    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     C*
     C     *LIKE         DEFINE    CACO#         SVCO#
     C     *LIKE         DEFINE    CASUB#        SVSUB#
     C     *LIKE         DEFINE    CAFND         SVFND
     C     *LIKE         DEFINE    CAFYR         SVFYR
     C     *LIKE         DEFINE    CACASE        SVCASE
     C     *LIKE         DEFINE    CADSEQ        SVDSEQ
     C  N99              EXCEPT    HEADER
     C  N99              SETON                                        99
     C*
     C                   Z-ADD     *ZEROS        SVCO#
     C                   Z-ADD     *ZEROS        SVSUB#
     C                   Z-ADD     *ZEROS        SVFND
     C                   Z-ADD     *ZEROS        SVFYR
     C                   Z-ADD     *ZEROS        SVCASE
     C                   Z-ADD     *ZEROS        SVDSEQ
     C                   Z-ADD     *ZEROS        COUNT             6 0
     C*
     C* READ THE DETAIL CHARGE FILE
     C*
     C                   READ      PCA01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C     CACO#         IFEQ      SVCO#
     C     CASUB#        ANDEQ     SVSUB#
     C     CAFND         ANDEQ     SVFND
     C     CAFYR         ANDEQ     SVFYR
     C     CACASE        ANDEQ     SVCASE
     C     CADSEQ        ANDEQ     SVDSEQ
     C   OF              EXCEPT    HEADER
     C   OF              SETOFF                                       OF
     C                   EXCEPT    DETAIL
     C                   ADD       1             COUNT
     C                   END
     C                   Z-ADD     CACO#         SVCO#
     C                   Z-ADD     CASUB#        SVSUB#
     C                   Z-ADD     CAFND         SVFND
     C                   Z-ADD     CAFYR         SVFYR
     C                   Z-ADD     CACASE        SVCASE
     C                   Z-ADD     CADSEQ        SVDSEQ
     C*
     C                   READ      PCA01                                  50
     C                   ENDDO                                                  END *IN50 DOWEQ '0'
     C                   EXCEPT    TOTAL
     C*
     C* END THE PROGRAM
     C*
     C                   MOVE      '1'           *INLR
     OQSYSPRT   E            HEADER         2  4
     O                                            9 'PRTFDCAP'
     O                                           40 'PRINT DUPLICATES -FDCAP'
     O                                           72 'PAGE:'
     O                       PAGE          Z     77
     O          E            HEADER         2
     O                                            6 'CO#'
     O                                           20 'SUB#'
     O                                           36 'FND'
     O                                           44 'FYR'
     O                                           54 'CASE'
     O                                           64 'DSEQ'
     O          E            DETAIL         1
     O                       CACO#                6
     O                       CASUB#              20
     O                       CAFND               36
     O                       CAFYR               44
     O                       CACASE              54
     O                       CADSEQ              64
     O                       CAPGM               84
     O                       CADT                94
     O          E            TOTAL          1
     O                                           30 'TOTAL DUP'
     O                       COUNT         J     44
