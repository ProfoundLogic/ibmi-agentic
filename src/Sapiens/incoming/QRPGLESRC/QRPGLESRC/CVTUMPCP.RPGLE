     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: CVTUMPCP
     H*****************************************************************
     H*  PROGRAMMER: C:41               PROJECT: UR017                *
     H*  CREATED: 02/27/96                                            *
     H*  THIS PROGRAM LOADS SS# AND ACDT FROM WMCMP FILE.             *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  60    GENERAL CHAIN INDICATOR                                *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     F*
     FUMPCP     UPE  E             DISK
     FFMCML1    IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     CMKY          KLIST
     C                   KFLD                    PCCO#
     C                   KFLD                    PCSUB#
     C                   KFLD                    PCFND
     C                   KFLD                    PCFYR
     C                   KFLD                    PCCASE
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMSS#         PCSS#
     C                   MOVE      CMACDT        PCACDT
     C                   UPDATE    PPC01
     C                   ENDIF
