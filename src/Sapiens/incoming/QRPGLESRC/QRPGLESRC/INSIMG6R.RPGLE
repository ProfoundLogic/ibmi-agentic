     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     FIMWSL     IF A E           K DISK
     FIMDIL     IF A E           K DISK
     FIMWSL2    IF   E           K DISK
     F                                     RENAME(PWS01:PWS02)
      *

     D SUBDIR          C                   CONST('SUBDIREC.001')
     D DCID            C                   CONST('DLS00000.001')
     D                 DS
     D  OCDCID                 1     12
     D  ##DLS                  1      3
     D  ##DC#                  4      8  0
     D  ##PRID                 9      9
     D  ##SD#                 10     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     WSKY          KLIST
     C                   KFLD                    @@WSID
     C*
     C     WSKY2         KLIST
     C                   KFLD                    @@JBNM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@LIB            10
     C*
     C*
      ********************** OBJECT IMPORTS (OI) **********************
     C                   MOVE      'OI'          @@WSID            2
      *
     C     WSKY          CHAIN     PWS01                              62
      *
     C     *IN62         IFEQ      *OFF
      *
     C                   ELSE
     C*
     C                   MOVEL     'OI'          WSWSID
     C                   MOVEL     'IMPORT  '    WSJBNM
     C                   MOVEL     'IMPORT  '    WSLUNM
     C                   MOVEL     'VWIMPORT'    WSVQUE
     C                   MOVEL     @@LIB         WSVQLB
     C                   MOVEL     @@LIB         WSSRLB
     C                   MOVEL     @@LIB         WSSSLB
     C                   MOVEL     @@LIB         WSVRQL
     C                   MOVEL     'SRIMPORT'    WSSRCV
     C                   MOVEL     'SSIMPORT'    WSSSND
     C                   MOVEL     'VRIMPORT'    WSVRQ
     C                   MOVEL     'Y'           WSSCAP
     C                   MOVEL     'I'           WSIDRV
     C                   MOVEL     'A'           WSSTAT
     C                   MOVEL     Q1USER        WSIUSR
     C                   MOVEL     Q1PGM         WSIPGM
     C                   MOVE      010101        WSITIM
     C                   MOVE      010101        WSIDT
     C                   MOVEL     Q1USER        WSLUSR
     C                   MOVEL     Q1PGM         WSLPGM
     C                   MOVE      010101        WSLTIM
     C                   MOVE      010101        WSLDT
     C                   WRITE     PWS01
     C                   ENDIF
      *
      *
     C     WSKY          CHAIN     PDI01                              62
      *
     C     *IN62         IFEQ      *OFF
      *
     C                   ELSE
     C*
     C                   MOVEL     'OI'          DIWSID
     C                   MOVE      000001        DISEQ#
     C                   MOVEL     'TIF'         DISUFX
     C                   MOVE      0001          DIBCHS
     C                   MOVEL     'A'           DISTAT
     C                   MOVEL     Q1USER        DIIUSR
     C                   MOVEL     Q1PGM         DIIPGM
     C                   MOVE      010101        DIITIM
     C                   MOVE      010101        DIIDT
     C                   MOVEL     Q1USER        DILUSR
     C                   MOVEL     Q1PGM         DILPGM
     C                   MOVE      010101        DILTIM
     C                   MOVE      010101        DILDT
     C                   WRITE     PDI01
     C                   ENDIF
      ********************** DART MAIL (DM) ***************************
     C                   MOVE      'DM'          @@WSID            2
      *
     C     WSKY          CHAIN     PWS01                              62
      *
     C     *IN62         IFEQ      *OFF
      *
     C                   ELSE
     C*
     C                   MOVEL     'DM'          WSWSID
     C                   MOVEL     'DARTMAIL'    WSJBNM
     C                   MOVEL     'DM      '    WSLUNM
     C                   MOVEL     'VWDM    '    WSVQUE
     C                   MOVEL     @@LIB         WSVQLB
     C                   MOVEL     @@LIB         WSSRLB
     C                   MOVEL     @@LIB         WSSSLB
     C                   MOVEL     @@LIB         WSVRQL
     C                   MOVEL     'SRDM    '    WSSRCV
     C                   MOVEL     'SSDM    '    WSSSND
     C                   MOVEL     'VRDM    '    WSVRQ
     C                   MOVEL     'Y'           WSSCAP
     C                   MOVEL     'I'           WSIDRV
     C                   MOVEL     'A'           WSSTAT
     C                   MOVEL     Q1USER        WSIUSR
     C                   MOVEL     Q1PGM         WSIPGM
     C                   MOVE      010101        WSITIM
     C                   MOVE      010101        WSIDT
     C                   MOVEL     Q1USER        WSLUSR
     C                   MOVEL     Q1PGM         WSLPGM
     C                   MOVE      010101        WSLTIM
     C                   MOVE      010101        WSLDT
     C                   WRITE     PWS01
     C                   ENDIF
      *
      *
     C     WSKY          CHAIN     PDI01                              62
      *
     C     *IN62         IFEQ      *OFF
      *
     C                   ELSE
     C*
     C                   MOVEL     'DM'          DIWSID
     C                   MOVE      000001        DISEQ#
     C                   MOVEL     'TIF'         DISUFX
     C                   MOVE      0001          DIBCHS
     C                   MOVEL     'A'           DISTAT
     C                   MOVEL     Q1USER        DIIUSR
     C                   MOVEL     Q1PGM         DIIPGM
     C                   MOVE      010101        DIITIM
     C                   MOVE      010101        DIIDT
     C                   MOVEL     Q1USER        DILUSR
     C                   MOVEL     Q1PGM         DILPGM
     C                   MOVE      010101        DILTIM
     C                   MOVE      010101        DILDT
     C                   WRITE     PDI01
     C                   ENDIF
      *
      ********************** IMG IMPORT (Z1) **************************
     C                   MOVE      'IS800R'      @@JBNM           10
     C                   MOVE      'Z1'          @@WSID            2
      *
     C     WSKY2         CHAIN     PWS02                              62
      *
     C     *IN62         IFEQ      *OFF
      *
     C                   ELSE
     C*
     C                   MOVEL     'Z1'          WSWSID
     C                   MOVEL     'IS800R  '    WSJBNM
     C                   MOVEL     'IS800R  '    WSLUNM
     C                   MOVEL     'VWIMGI  '    WSVQUE
     C                   MOVEL     @@LIB         WSVQLB
     C                   MOVEL     @@LIB         WSSRLB
     C                   MOVEL     @@LIB         WSSSLB
     C                   MOVEL     @@LIB         WSVRQL
     C                   MOVEL     'SRIMGI  '    WSSRCV
     C                   MOVEL     'SSIMGI  '    WSSSND
     C                   MOVEL     'VRIMGI  '    WSVRQ
     C                   MOVEL     'Y'           WSSCAP
     C                   MOVEL     'I'           WSIDRV
     C                   MOVEL     'A'           WSSTAT
     C                   MOVEL     Q1USER        WSIUSR
     C                   MOVEL     Q1PGM         WSIPGM
     C                   MOVE      010101        WSITIM
     C                   MOVE      010101        WSIDT
     C                   MOVEL     Q1USER        WSLUSR
     C                   MOVEL     Q1PGM         WSLPGM
     C                   MOVE      010101        WSLTIM
     C                   MOVE      010101        WSLDT
     C                   WRITE     PWS01
     C                   ENDIF
      *
      *
     C     WSKY          CHAIN     PDI01                              62
      *
     C     *IN62         IFEQ      *OFF
      *
     C                   ELSE
     C*
     C                   MOVEL     'Z1'          DIWSID
     C                   MOVE      000001        DISEQ#
     C                   MOVEL     'TIF'         DISUFX
     C                   MOVE      0001          DIBCHS
     C                   MOVEL     'A'           DISTAT
     C                   MOVEL     Q1USER        DIIUSR
     C                   MOVEL     Q1PGM         DIIPGM
     C                   MOVE      010101        DIITIM
     C                   MOVE      010101        DIIDT
     C                   MOVEL     Q1USER        DILUSR
     C                   MOVEL     Q1PGM         DILPGM
     C                   MOVE      010101        DILTIM
     C                   MOVE      010101        DILDT
     C                   WRITE     PDI01
     C                   ENDIF
      *
      *
      ********************** IMG IMPORT (Z2) **************************
     C                   MOVE      'IS828R'      @@JBNM           10
     C                   MOVE      'Z2'          @@WSID            2
      *
     C     WSKY2         CHAIN     PWS02                              62
      *
     C     *IN62         IFEQ      *OFF
      *
     C                   ELSE
     C*
     C                   MOVEL     'Z2'          WSWSID
     C                   MOVEL     'IS828R  '    WSJBNM
     C                   MOVEL     'IS828R  '    WSLUNM
     C                   MOVEL     'VWIMG2  '    WSVQUE
     C                   MOVEL     @@LIB         WSVQLB
     C                   MOVEL     @@LIB         WSSRLB
     C                   MOVEL     @@LIB         WSSSLB
     C                   MOVEL     @@LIB         WSVRQL
     C                   MOVEL     'SRIMG2  '    WSSRCV
     C                   MOVEL     'SSIMG2  '    WSSSND
     C                   MOVEL     'VRIMG2  '    WSVRQ
     C                   MOVEL     'Y'           WSSCAP
     C                   MOVEL     'I'           WSIDRV
     C                   MOVEL     'A'           WSSTAT
     C                   MOVEL     Q1USER        WSIUSR
     C                   MOVEL     Q1PGM         WSIPGM
     C                   MOVE      010101        WSITIM
     C                   MOVE      010101        WSIDT
     C                   MOVEL     Q1USER        WSLUSR
     C                   MOVEL     Q1PGM         WSLPGM
     C                   MOVE      010101        WSLTIM
     C                   MOVE      010101        WSLDT
     C                   WRITE     PWS01
     C                   ENDIF
      *
      *
     C     WSKY          CHAIN     PDI01                              62
      *
     C     *IN62         IFEQ      *OFF
      *
     C                   ELSE
     C*
     C                   MOVEL     'Z2'          DIWSID
     C                   MOVE      000001        DISEQ#
     C                   MOVEL     'TIF'         DISUFX
     C                   MOVE      0001          DIBCHS
     C                   MOVEL     'A'           DISTAT
     C                   MOVEL     Q1USER        DIIUSR
     C                   MOVEL     Q1PGM         DIIPGM
     C                   MOVE      010101        DIITIM
     C                   MOVE      010101        DIIDT
     C                   MOVEL     Q1USER        DILUSR
     C                   MOVEL     Q1PGM         DILPGM
     C                   MOVE      010101        DILTIM
     C                   MOVE      010101        DILDT
     C                   WRITE     PDI01
     C                   ENDIF
      *
     C                   MOVE      *ON           *INLR
