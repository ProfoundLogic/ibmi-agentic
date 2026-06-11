     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE CVTWC976 - LOAD F2TPA
     H*****************************************************************
     H*                                                               *
     H*                                                               *
     H*****************************************************************
      *
     FWDF2P     UF   E             DISK
      *
      *  Convert file
      *

     C                   READ      PF201                                  60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      'Y'           F2TPA
     C                   UPDATE    PF201
     C                   READ      PF201                                  60
     C                   ENDDO
      *
      * END PROGRAM
      *
     C                   SETON                                        LR
      *
