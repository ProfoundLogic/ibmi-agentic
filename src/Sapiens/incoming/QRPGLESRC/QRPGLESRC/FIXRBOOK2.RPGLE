     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: FIXRBOOK
     H*    DESCRIPTION: Fix NDC's DRUG CODE FILE
     H*****************************************************************
     H* Created 03/01/2002 by C:62 for Project 114MBH.
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*****************************************************************
     H*  INDICATORS:
     H*  99    CHAIN ON PRX01
     H*****************************************************************
     H*  FILES:
     H*  FMRXP  (PRX01)
     H*****************************************************************
     FFMRXP     UF   E             DISK
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

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
     C                   READ      PRX01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   EXSR      UPDATR
     C                   READ      PRX01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C***************************************************************
     C* UPDATE FMRXP
     C***************************************************************
     C     UPDATR        BEGSR
     C*
     C     RXSTRG        IFEQ      *BLANKS
     C     RXSTRN        ANDNE     'EA'
     C     RXSTRN        ANDNE     'DZ'
     C*
     C                   MOVEL(P)  RXSTRN        RXSTRG
     C                   CLEAR                   RXSTRN
     C*
     C***** MAINTENANCE FIELDS ****************************************
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RXTIME
     C                   MOVEL(P)  Q1USER        RXUSER
     C                   MOVEL(P)  Q1PGM         RXPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RXDT
     C*
     C                   ENDIF
     C*
     C                   UPDATE    PRX01
     C*
     C                   ENDSR
