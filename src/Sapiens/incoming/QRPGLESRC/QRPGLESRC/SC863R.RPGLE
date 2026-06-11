     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***************************************************************
     H*                                                            **
     H* PROGRAM:     SC863R                                        **
     H* DESCRIPTION: MAINTAIN SMDCP TO END PURGE TO OPTICAL        **
     H*                                                            **
     H* CREATED BY:  C:02   PROJ:OPT      DATE:  06/07/93          **
     H***************************************************************
     H* REVISIONS:                                                 **
     H***************************************************************
     H* INDICATOR USAGE:                                           **
     H*  60                    CHAIN/DOWEQ                         **
     H***************************************************************
     FSMDCP     UF   E             DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
      *
     C*
     C*
     C     1             CHAIN     PDC01                              6299
     C   991             CHAIN     PDC01                              62
      *
     C     *IN62         IFEQ      '0'
     C                   MOVE      'N'           DCDPFL
     C                   UPDATE    PDC01
     C                   ENDIF
     C*
      *
      *
     C*
     C*** END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
