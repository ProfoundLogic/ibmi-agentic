     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWCPOLS - INSERT CARRIER NUMBER
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWCPOLS    UF   E             DISK
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C                   MOVE      '10975'       XXCARR            5
      *
     C                   READ      RPOLS                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVEL     XXCARR        RDATA
      *
     C                   UPDATE    RPOLS
      *
     C                   READ      RPOLS                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C                   MOVE      *ON           *INLR
