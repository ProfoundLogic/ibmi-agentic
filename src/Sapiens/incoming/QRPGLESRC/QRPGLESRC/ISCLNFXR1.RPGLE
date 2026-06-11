     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE ISCLNFXR1 - DAILY FAX RECORDS MAINTENANCE
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* 60  GENERAL CHAIN INDICATOR                                   *
     H* 61  CHAIN INDICATOR                                           *
     H*****************************************************************
     H** FILES:                                                      **
     H*****************************************************************
     H** NOTE:
     H*****************************************************************
      *** IAFTL2 IS SELECT FAXC = ' ' OR 'R'
     FIAFTL2    UF   E           K DISK

     D*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C                   TIME                    DSDTTM
     C*
     C*=============================================================
     C*
     C                   READ      PFT01                                  60
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      'E'           FTFAXC
     C                   UPDATE    PFT01
     C                   READ      PFT01                                  60
     C                   ENDDO
     C*
     C* END OF THE PGM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
