     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: CVTSUB#                                           *
     H*    DESCRIPTION: ADD SUB# TO FMCMP
     FFMCMP     UF   E           K DISK

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QFSUB#            5
     C                   PARM                    QTSUB#            5
     C*
     C                   MOVE      QFSUB#        XFSUB#
     C                   MOVE      QTSUB#        XTSUB#
     C*
     C     *LIKE         DEFINE    CMSUB#        XFSUB#
     C     *LIKE         DEFINE    CMSUB#        XTSUB#
     C*
     C                   READ      PCM01                                  50
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C     CMSUB#        IFEQ      XFSUB#
     C                   Z-ADD     XTSUB#        CMSUB#
     C                   UPDATE    PCM01
     C                   END                                                    END EQ XFSUB#
     C*
     C                   READ      PCM01                                  50
     C                   END                                                    END DOWEQ *IN50
     C*
     C                   MOVE      '1'           *INLR
