     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - FA724R - GET EOFRDT AND EOTODT.                          DATA
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA724R                                            *
     H*    CREATED: 03/10/00  C:62                                    *
     H*    DESCRIPTION: GO THROUGH T/M ACTIVITY AND GET FRDT AND TODT *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*    *IN99                GENERAL CHAINING INDICATOR            *
     H*****************************************************************
     F*
     FFAEOL3    IF   E           K DISK
     FFMFNL1    IF   E           K DISK
     FWARPP     UF   E             DISK

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  EOMB FILE
     C*
     C     EOKY          KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    FNSUB#
     C                   KFLD                    RPEOB#
     C*
     C*  GROUP FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* CHAIN OUT TO WARPP RECORD.
     C*
     C                   READ      PRP01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     RPEOB#        IFNE      *ZEROS
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C     EOKY          CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C     EOSYS         ANDEQ     'M'
     C     *IN99         OREQ      *OFF
     C     EOSYS         ANDEQ     'U'
     C                   Z-ADD     EOFRDT        RPFRDT
     C                   Z-ADD     EOTODT        RPTODT
     C                   MOVEL     Q1PGM         RPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   Z-ADD     DSTIME        RPTIME
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   UPDATE    PRP01
     C*
     C                   READ      PRP01                                  99
     C                   ENDDO
     C*
     C*  END THE PROGRAM
     C*
     C                   SETON                                        LR
