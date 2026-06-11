     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  CLRCALTR# - CLEAR CALTR#                                     *
     H*****************************************************************
     H*  CREATED ON 11/15/02 BY C:62 FOR PROJECT 114MDC               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FFDCAP     UF   E             DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C*
     C                   READ      PCA01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     *ZEROS        CALTR#
     C                   MOVE      'F'           CAPGM
     C*
     C                   UPDATE    PCA01
     C*
     C                   READ      PCA01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
