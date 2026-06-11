     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***************************************************************
     H*                                                            **
     H* PROGRAM:     SC858R                                        **
     H* DESCRIPTION: MAINTAIN SMOCP TO END PURGE TO OPTICAL        **
     H*              SDCMP                                         **
     H* CREATED BY:  C:02   PROJ:OPT      DATE:  06/02/93          **
     H***************************************************************
     H* REVISIONS:                                                 **
     H***************************************************************
     H* INDICATOR USAGE:                                           **
     H*  60                    CHAIN/DOWEQ                         **
     H***************************************************************
     FSMOCL     UF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
      *
     C     OCKY          KLIST
     C                   KFLD                    QQOSYS
     C                   KFLD                    QQLIBN
     C*
     C*
     C                   MOVE      'G'           QQOSYS            1
     C                   MOVEL     'OPTMLB01'    QQLIBN           10
     C*
     C     OCKY          CHAIN     POC01                              6299
     C   99OCKY          CHAIN     POC01                              62
      *
     C     *IN62         IFEQ      '0'
     C                   MOVE      'N'           OCPPFL
     C                   UPDATE    POC01
     C                   ENDIF
     C*
      *
      *
     C*
     C*** END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
