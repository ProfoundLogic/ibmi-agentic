     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CV3HCPOS-CONVERT HCPOS AND HC10A1
     F*
     FFDHCP     UF   E           K DISK

     D*
     D* HC10A1 DATA STRUCTURE
     D                 DS
     D  XX10A1                 1     10
     D  X510A1                 5      6
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C*  GET THE DETAIL CHARGE AUXILIARY RECORD
     C*
     C                   READ      PHC01                                  60
     C     *IN60         DOWEQ     '0'
     C     HCPOS         IFEQ      '0'
     C     HCPOS         OREQ      *BLANKS
     C                   MOVE      '011'         HCPOS
     C                   MOVE      'FS'          X510A1
     C                   MOVE      XX10A1        HC10A1
     C                   MOVE      *BLANKS       HCPGM
     C                   MOVEL     'CV3HCPOS'    HCPGM
     C                   UPDATE    PHC01
     C                   END
     C                   READ      PHC01                                  60
     C                   END
     C*
     C*
     C                   MOVE      '1'           *INLR
