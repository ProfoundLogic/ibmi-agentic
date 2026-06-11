     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: CVTFND
     H*    DESCRIPTION: UPDATE FND IN FMCMP
     H*****************************************************************
     FFMCMP     UF   E           K DISK

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QFFND             3
     C                   PARM                    QTFND             3
     C*
     C                   MOVE      QFFND         XFFND
     C                   MOVE      QTFND         XTFND
     C*
     C     *LIKE         DEFINE    CMFND         XFFND
     C     *LIKE         DEFINE    CMFND         XTFND
     C*
     C                   READ      PCM01                                  50
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C     CMFND         IFEQ      XFFND
     C                   Z-ADD     XTFND         CMFND
     C                   UPDATE    PCM01
     C                   END                                                    END EQ XFFND
     C*
     C                   READ      PCM01                                  50
     C                   END                                                    END DOWEQ *IN50
     C*
     C                   MOVE      '1'           *INLR
