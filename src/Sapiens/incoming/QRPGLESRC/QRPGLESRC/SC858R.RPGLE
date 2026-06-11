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
     H**  C:02    120IAD   -  MODIFY TO GET THE SMOCP FILE TO RETRIEVE
     H**                  01/26/04    THE OPTICAL LIBRARY NAME
     H***************************************************************
     H* INDICATOR USAGE:                                           **
     H*  60                    CHAIN/DOWEQ                         **
     H***************************************************************
     FSMOCL     UF   E           K DISK
     F**** ADD 01/26/04 ***********************************************
     FSMOCP     IF   E             DISK
     F                                     RENAME(POC01:POC02)
     F**** END ADD 01/26/04 *******************************************

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
     C*** 01/26/04 ** ADD *****************************************8
     C     1             CHAIN     SMOCP                              64
     C     *IN64         IFEQ      *OFF
     C                   MOVEL     OCOSYS        QQOSYS            1
     C                   MOVEL     OCLIBN        QQLIBN           10
     C                   ENDIF
     C*** 01/26/04 ** END ADD *************************************8
     C*
     C*** DEL 1/26/04 ***  MOVE 'G'       QQOSYS  1
     C*** DEL 1/26/04 ***  MOVEL'OPTMLB01'QQLIBN 10
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
