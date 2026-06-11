     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     FSMPRL3    UF   E           K DISK
     F*

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
      * =================================== *
      * * *  KEY LISTS                      *
      * =================================== *

     C*
     C*** PURGE/DELETE TRANSACTION FILE
      *
     C     PRKY          KLIST
     C                   KFLD                    XXSTAT            1
      *
     C*
      * * * PROCESS RCDS SELECTED FOR PURGE
      *
     C                   MOVE      'S'           XXSTAT                              PRGD RCDS
      *
      *
      *** READ EACH RECORD SELECTED FOR PURGE.
     C*
     C     PRKY          CHAIN     PPR01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      ' '           PRTSFL
      *
     C                   UPDATE    PPR01
     C*
     C*
     C     PRKY          READE     PPR01                                  60
     C*
     C                   ENDDO
     C*
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
