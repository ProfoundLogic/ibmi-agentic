     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWDRBP - INCREASE/CONVERT FIELD
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/02/04  C:53  PROJECT: WC1180                               *
     H*                 LOAD NEW FIELD. FROM 2 DEC POS TO 4
     H*                                                               *
     H*****************************************************************
     FWDRBP     UF   E           K DISK

     C*
     C*   Read every record in the file
     C                   READ      PRB01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C     RBREPC        IFNE      0
     C                   Z-ADD     RBREPC        RBCMPR
     C                   UPDATE    PRB01
     C                   ENDIF                                                              0
     C*
     C                   READ      PRB01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   MOVE      *ON           *INLR
