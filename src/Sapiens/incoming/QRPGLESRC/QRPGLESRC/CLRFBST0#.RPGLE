     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  CLRFBST0# - CLEAR STATE FIELDS                               *
     H*****************************************************************
     H*  CREATED ON 11/15/02 BY C:62 FOR PROJECT 114MDC               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FSFBPF     UF   E             DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C*
     C                   READ      SFB01                                  99
     C     *IN99         IFEQ      *OFF
     C*
     C                   CLEAR                   FBST01
     C                   CLEAR                   FBST02
     C                   MOVEL     ' '           FBST03
     C                   CLEAR                   FBST04
     C                   CLEAR                   FBST05
     C                   CLEAR                   FBST06
     C                   CLEAR                   FBST07
     C                   CLEAR                   FBST08
     C                   CLEAR                   FBST09
     C                   CLEAR                   FBST10
     C*
     C                   UPDATE    SFB01
     C                   ENDIF
     C*
     C                   SETON                                        LR
