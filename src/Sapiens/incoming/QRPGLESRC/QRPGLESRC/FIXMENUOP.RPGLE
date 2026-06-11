     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXMENUOP - ASSIGN 'W'/'F' AS LAST BYTE OF OPEXCL FOR
     H/TITLE             CMD 13 TO WORK FROM MENU
     FMMOPP     UF   E           K DISK

     C*
     C                   READ      POP01                                  60
     C     *IN60         DOWEQ     '0'
     C     OPMYN         IFEQ      'Y'
     C     OPAPP         IFEQ      'MWC'
     C     OPAPP         OREQ      'LWC'
     C     OPAPP         OREQ      'LQT'
     C     OPAPP         OREQ      'LGL'
     C                   MOVE      'W'           OPEXCL
     C                   UPDATE    POP01
     C                   ELSE
     C     OPAPP         IFEQ      'LFS'
     C                   MOVE      'F'           OPEXCL
     C                   UPDATE    POP01
     C                   ELSE
     C     OPAPP         IFEQ      'LSC'
     C                   MOVE      'S'           OPEXCL
     C                   UPDATE    POP01
     C                   END
     C                   END
     C                   END
     C                   END
     C                   READ      POP01                                  60                R
     C                   END
     C*
     C                   MOVE      '1'           *INLR
