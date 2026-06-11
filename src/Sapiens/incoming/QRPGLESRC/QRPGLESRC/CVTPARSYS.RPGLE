     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CVTPARSYS - UPDATE NMPHP/PHFL01 AND NMHSP/HSFL01
     H**************************************************************************
     H*
     FNMHSP     UF   E           K DISK
     FNMPHP     UF   E           K DISK

     C*
     C* READ THE HOSPITAL PARTICIPATING PROVIDER FILE
     C*
     C                   READ      PHS01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C* UPDATE THE SYSTEM FLAG TO AN 'H'
     C*
     C                   MOVE      'H'           HSFL01
     C                   UPDATE    PHS01
     C*
     C                   READ      PHS01                                  60
     C                   END                                                    ENDDOW 60
     C*
     C* READ THE PHYSICIAN PARTICIPATING PROVIDER FILE
     C*
     C                   READ      PPH01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C* UPDATE THE SYSTEM FLAG TO AN 'H'
     C*
     C                   MOVE      'P'           PHFL01
     C                   UPDATE    PPH01
     C*
     C                   READ      PPH01                                  60
     C                   END                                                    ENDDOW 60
     C*
     C                   MOVE      '1'           *INLR
