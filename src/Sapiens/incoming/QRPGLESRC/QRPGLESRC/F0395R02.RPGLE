     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F0395R02 - BILL HISTORY UPDATE                               *
     H*****************************************************************
     H*                                                               *
     H*  CALLED FROM: F1393R                                          *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  CREATED ON 08/16/2002 BY C:62 FOR PROJECT 114MDC
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FFAEOP     IF   E             DISK
     FFAESP     O    E             DISK

     I*
     D/COPY SCOPYMT01D
     I***PY SCOPYMT01I
     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@BLST            1
     C                   PARM                    @@STCD            3
     C                   PARM                    @@REAS           60
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   EXSR      PGMSTR
     C                   EXSR      WRITER
     C                   EXSR      PGMEND
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     EOCO#         ESCO#
     C                   Z-ADD     EOSUB#        ESSUB#
     C                   Z-ADD     EOFND         ESFND
     C                   Z-ADD     EOFYR         ESFYR
     C                   Z-ADD     EOCASE        ESCASE
     C                   Z-ADD     EOEOB#        ESEOB#
     C                   Z-ADD     EORCN         ESRCN
     C                   MOVEL     EOSS#         ESSS#
     C                   Z-ADD     EOACDT        ESACDT
     C*
     C                   MOVEL     @@BLST        ESBLST
     C                   MOVEL     @@STCD        ESSTCD
     C                   MOVEL     @@REAS        ESREAS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@RRN         CHAIN     PEO01                              99
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   PES01
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        ESUSER
     C                   MOVEL     Q1USER        ESIUSR
     C                   MOVEL     Q1PGM         ESPGM
     C                   MOVEL     Q1PGM         ESIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ESDT
     C*
     C                   MOVEL     SRDTO         ESIDT
     C*
     C                   MOVEL     DSTIME        ESTIME
     C                   MOVEL     DSTIME        ESITIM
     C*
     C                   WRITE     PES01
     C                   FEOD      FAESP
     C*
     C                   ENDSR
