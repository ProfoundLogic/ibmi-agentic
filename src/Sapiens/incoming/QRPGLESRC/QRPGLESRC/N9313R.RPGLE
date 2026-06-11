     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H/TITLE N9313R - NETWORK VERIFY BILL AMOUNT GREATER THAN ZERO
     H*
     H*****************************************************************
     H*                                                               *
     H**   PROGRAM: N9313R                                           **
     H**   DESCRIPTION: READS ALL RECORDS FOR A SUBSCRIBER THAT ARE  **
     H**                TO BE BILLED.  IF RECORD HAS A ZERO BILL     **
     H**                BILL AMOUNT UPDATES EANHRF TO BE AN "H".     **
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  CALLED FROM: N9310C                                          *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  CREATED ON 01/22/2002 BY C:62 FOR PROJECT AR033              *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FFAEAL1    UF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* READ EOB FILE
     C*
     C                   READ      PEA01                                  99
     C     *IN99         DOWEQ     *OFF
     C     EANAMT        IFLE      *ZEROS
     C                   MOVE      'H'           EANHRF
     C                   ENDIF
     C                   UPDATE    PEA01
     C                   READ      PEA01                                  99
     C                   ENDDO
     C                   SETON                                        LR
