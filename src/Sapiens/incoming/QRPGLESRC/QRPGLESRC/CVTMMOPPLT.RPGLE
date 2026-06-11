     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: CVTMMOPP
     H*****************************************************************
     FMMOPP     UF   E           K DISK

     C*
     C                   READ      POP01                                  50
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C     OPAPP         IFEQ      'LFS'
     C                   MOVEL     'TFS'         OPAPP
     C                   ELSE
     C     OPAPP         IFEQ      'LWC'
     C                   MOVEL     'TWC'         OPAPP
     C                   END
     C                   END
     C*
     C                   UPDATE    POP01
     C*
     C                   READ      POP01                                  50
     C                   END                                                    END DOWEQ *IN50
     C*
     C                   MOVE      '1'           *INLR
