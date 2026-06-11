     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *****************************************************************
      *TITLE SC828R - OPTICAL PURGE/DELETE END PURGE TO OPTICAL PGM   *
      *****************************************************************
      *    PROGRAM DESCRIPTION:                                       *
      *        BACKUP AND BATCH - End purge to Optical Program        *
      *                                                               *
      *  CALLED FROM: Menu                                            *
      *                                                               *
      *  CALLS TO:                                                    *
      *                                                               *
      *  CREATED: 03/31/03    PROGROGRAMMER: C:145    PROJECT: 124IAA *
      *****************************************************************
      ** REVISIONS:                                                   *
      *  CREATED:             PROGRAMMER:             PROJECT:        *
      *                                                               *
      *  03/31/02             C:145                   124IAA          *
      *                Original program for optical purge/delete      *
      *                end purge program was SC858R.  Program renamed *
      *                and modified to reflect changes for new        *
      *                batch/backup process.                          *
      ******************************************************************
      * INDICATOR USAGE                                                *
      *                                                                *
      *  60                    CHAIN/DOWEQ                         **
      ******************************************************************
      *
     FSMOTL     UF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     OTKY          KLIST
     C                   KFLD                    QQOSYS
     C                   KFLD                    QQLIBN
      *
     C                   MOVE      'G'           QQOSYS            1
     C                   MOVEL     'OPTMLB01'    QQLIBN           10
      *
     C     OTKY          CHAIN     POT01                              6299
     C   99OTKY          CHAIN     POT01                              62
      *
     C     *IN62         IFEQ      '0'
     C                   MOVE      'N'           OTPPFL
     C                   UPDATE    POT01
     C                   ENDIF
      *
      *  End the program
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
