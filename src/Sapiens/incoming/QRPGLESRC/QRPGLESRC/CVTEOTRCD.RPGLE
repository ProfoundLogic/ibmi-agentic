     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTEOMBCDS - CONVERT PDS EOMB CODES IN FDCAP TO TX STATE CODES
     H**************************************************************************
     FFAEOP     UF   E           K DISK

     C*
     C* READ THE EOMB FILE
     C*
     C                   READ      PEO01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C* UPDATE THE TRANSACTION CODE
     C*
     C                   MOVE      'MC'          EOTRCD
     C*
     C                   UPDATE    PEO01
     C*
     C* READ THE NEXT RECORD
     C*
     C                   READ      PEO01                                  60
     C                   END                                                    ENDDOW 60
     C*
     C* END THE PROGRAM
     C*
     C                   MOVE      '1'           *INLR
