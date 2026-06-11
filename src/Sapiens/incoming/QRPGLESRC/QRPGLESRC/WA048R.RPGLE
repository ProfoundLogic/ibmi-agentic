     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA048R - API TO RETRIEVE VALUES FROM WMUPP
     H*****************************************************************
     H*  Created: 05/03/02  C:53  Project:  91WFO                     *
     H*****************************************************************
     FWMUPL     IF   E           K DISK

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@USR
     C                   PARM      UPMAIL        @@MAIL
     C                   PARM      *IN99         @@RTCD
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    UPUSR         @@USR
     C     *LIKE         DEFINE    UPMAIL        @@MAIL
     C     *LIKE         DEFINE    UPSTAT        @@RTCD
     C*
     C                   CLEAR                   PUP01
     C*
     C     @@USR         CHAIN     WMUPL                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     UPMAIL        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN99
     C                   ENDIF
     C*
     C                   RETURN
