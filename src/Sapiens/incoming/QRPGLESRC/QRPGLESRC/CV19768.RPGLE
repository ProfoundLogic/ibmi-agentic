     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FSTSTL1    UF   E           K DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C                   MOVEL(P)  'UC'          KKKEY            10
     C*
     C     KKKEY         CHAIN     PST01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVE      'Y'           ST1A5
     C                   UPDATE    PST01
     C                   ENDIF
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
