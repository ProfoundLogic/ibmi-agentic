     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS910R   UNLOCK ANY WAYWARD INDEX RECORDS
     H*****************************************************************
     H* THIS PROGRAM HANDLES THE CHANGING OF THE INDEX FILE TO UNLOCK *
     H*   ALL LOCKED RECORDS                                          *
     H*                                                               *
     H*    CALLS TO:                                                  *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* 60  GENERAL CHAIN INDICATOR                                   *
     H*****************************************************************
     H** FILES:                                                      **
     H*****************************************************************
     F*
     FIMIEL4    UF   E           K DISK
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LOVAL        SETLL     PIE01
     C                   READ      PIE01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVEL     ' '           IESTAT
     C*
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      XXDT              6 0
     C                   Z-ADD     XXDT          IELDT
     C                   Z-ADD     DSTIME        IELTIM
     C                   MOVEL     Q1USER        IELUSR
     C                   MOVEL     Q1PGM         IELPGM
     C*
     C                   UPDATE    PIE01
     C*
     C                   READ      PIE01                                  60
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
