     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  FIXTRDT - POPULATE EATRDT FOR 'M' AND 'U''S   C:62 03/13/00  *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FFAEAP     UF   E             DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C                   READ      PEA01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     EASYS         IFEQ      'M'
     C     EASYS         OREQ      'U'
     C*
     C                   Z-ADD     EAIDT         EATRDT
     C                   UPDATE    PEA01
     C*
     C                   ELSE
     C*
     C                   UPDATE    PEA01
     C*
     C                   ENDIF
     C*
     C                   READ      PEA01                                  99
     C*
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*
