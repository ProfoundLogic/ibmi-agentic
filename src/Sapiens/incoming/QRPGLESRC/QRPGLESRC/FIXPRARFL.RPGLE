     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXPRARFL - LOAD 'Y' INTO PRARFL IN WMPRP
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWMPRP     UF   E             DISK

     C                   READ      PPR01                                  60
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      'Y'           PRARFL
     C                   UPDATE    PPR01
     C                   READ      PPR01                                  60
     C                   ENDDO
     C                   MOVE      *ON           *INLR
