     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWCPOLS - INSERT FLORIDA STATE CODE
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWCPOLS    UF   E             DISK
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D DATA            DS
     D  XXLINK                 1     43
     D  XXSTCD                44     45
     D  XXRCTY                46     47
     D  XXTOAD                48     48
     D  XXFILL                49    126
     D  XXMPST               127    128
     D  XXFIL2               129    154
     D  XXSTLN               155    156
      *
      *

     C                   READ      RPOLS                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVEL     RDATA         DATA
      *
     C     XXRCTY        IFEQ      '04'
     C     XXRCTY        OREQ      '05'
      *
     C                   MOVE      '09'          XXSTCD
     C                   MOVEL     DATA          RDATA
      *
     C                   UPDATE    RPOLS
      *
     C                   ENDIF                                                  XXRCTY IFEQ '04'
      *
     C     XXRCTY        IFEQ      '01'
      *
     C                   MOVE      '09'          XXMPST
     C                   MOVEL     DATA          RDATA
      *
     C                   UPDATE    RPOLS
      *
     C                   ENDIF                                                  XXRCTY IFEQ '04'
      *
     C     XXRCTY        IFEQ      '03'
     C     XXTOAD        ANDEQ     '1'
     C     XXRCTY        OREQ      '03'
     C     XXTOAD        ANDEQ     '2'
      *
     C                   MOVE      '09'          XXSTLN
     C                   MOVEL     DATA          RDATA
      *
     C                   UPDATE    RPOLS
      *
     C                   ENDIF                                                  XXRCTY IFEQ '04'
      *
     C                   READ      RPOLS                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C                   MOVE      *ON           *INLR
