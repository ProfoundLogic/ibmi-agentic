     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE DA002R
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: EDIDLOG                                           *
     H*    CREATED: 06/19/06  C:53  PROJECT: DR215                    *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FSAOIL     UF A E           K DISK
      *

     C     *ENTRY        PLIST
     C                   PARM                    QQDATE            6 0
     C                   PARM                    QQTIME            6 0
     C                   PARM                    QQFDIR           80
     C                   PARM                    QQFFIL          132
     C                   PARM                    QQEDIR           80
     C                   PARM                    QQADIR           80
     C                   PARM                    QQTDIR           80
     C                   PARM                    QQTFIL          132
     C                   PARM                    QQERCD            2
     C                   PARM                    QQERDE           80
     C                   PARM                    QQSTS             1
     C                   PARM                    QQPGM            10
     C                   PARM                    QQINLR            1
      *
     C     OIKY          KLIST
     C                   KFLD                    QQSTS
     C                   KFLD                    QQFDIR
     C                   KFLD                    QQFFIL
      *
      *** SETON LR CALL
      *
     C     QQINLR        IFEQ      'Y'
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF
      *
      *** ALREADY THERE ? EXIT
      *
     C     OIKY          CHAIN     POI01                              99
     C     *IN99         IFEQ      *OFF
     C                   UPDATE    POI01
     C                   RETURN
     C                   ENDIF
      *
     C                   CLEAR                   POI01
      *
     C                   Z-ADD     QQDATE        OIDATE
     C                   Z-ADD     QQTIME        OITIME
     C                   MOVEL     QQFDIR        OIFDIR
     C                   MOVEL     QQFFIL        OIFFIL
     C                   MOVEL     QQEDIR        OIEDIR
     C                   MOVEL     QQADIR        OIADIR
     C                   MOVEL     QQTDIR        OITDIR
     C                   MOVEL     QQTFIL        OITFIL
     C                   MOVEL     QQERCD        OIERCD
     C                   MOVEL     QQERDE        OIERDE
     C                   MOVEL     QQSTS         OISTS
     C                   MOVEL     QQPGM         OIPGM
      *
     C                   WRITE     POI01
     C                   RETURN
      *
