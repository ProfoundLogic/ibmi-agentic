     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTRMCD - CONVERSION OF OLD RMCD TO NEW RMCD
     FSDCMLTP   UF   E           K DISK

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQRMCD            5
     C*
     C*** REMARK DETAIL FILE
     C                   READ      PCM01                                  60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      QQRMCD        CMRMCD
     C                   UPDATE    PCM01
     C                   READ      PCM01                                  60
     C                   END                                                    ENDDOW 60
     C*
     C                   MOVE      '1'           *INLR
