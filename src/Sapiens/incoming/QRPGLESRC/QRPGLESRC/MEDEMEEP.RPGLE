     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE MEDEMEEP
     H*****************************************************************
     H*
     H*  CREATED:  11/13/02  C:03  PROJECT: SUPP (EX093 #34864)
     H*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H*****************************************************************
     FEMEEL     UF   E           K DISK
      *
      *

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      * CLAIM DETAIL FILE
      *
     C     EEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C                   TIME                    DSDTTM
      *
     C     *LIKE         DEFINE    EECO#         XXCO#
     C     *LIKE         DEFINE    EEFND         XXFND
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
      *
      *  BEGIN MAIN LOOP
      *
     C     EEKY          CHAIN     PEE01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     EEMULT        IFEQ      '6'
     C                   MOVE      ' '           EEMULT
     C                   MOVEL     Q1USER        EEUSER
     C                   MOVEL     Q1PGM         EEPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EEDT
     C                   MOVEL     DSTIME        EETIME
     C                   UPDATE    PEE01
     C                   END
      *
     C     EEKY          READE     PEE01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C                   MOVE      '1'           *INLR
