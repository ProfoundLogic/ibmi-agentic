     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTMMOPP - CONVERT OPAPP FROM APP CODE TO APP CODE
     FMMOPP     UF   E           K DISK

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFAPP            3
     C                   PARM                    QQTAPP            3
     C*
     C*** MENU OPTION FILE
     C                   READ      POP01                                  60
     C     *IN60         DOWEQ     '0'
     C     OPAPP         IFEQ      QQFAPP
     C                   MOVE      QQTAPP        OPAPP
     C                   UPDATE    POP01
     C                   END                                                    END EQ QQFAPP
     C                   READ      POP01                                  60
     C                   END                                                    ENDDOW 60
     C*
     C                   MOVE      '1'           *INLR
