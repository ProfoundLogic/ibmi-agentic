     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE DLT96MEW - DELETES NON-MIDWEST ZIPS/CODES
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM DELETES NON-MIDWEST ZIPS/CODES    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    COMMAND LINE                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 12/26/01  PROGRAMMER: C:62  PROJECT: 96MEW          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS:                                                   *
     H*   SCR01:                                                      *
     H*   99  READ  - FMGZP                                           *
     H*****************************************************************
     FFMGZP     UF   E             DISK
     FFMRCL     UF   E           K DISK
     F*

     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C     RCKY          KLIST
     C                   KFLD                    GZCO#
     C                   KFLD                    GZZIPA
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
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  LIKE DEFINES
     C*
     C                   TIME                    DSDTTM
     C*
     C                   READ      PGZ01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     2             SUBST     GZDESC:1      XXST              2
     C*
     C     XXST          IFNE      'IA'
     C     XXST          ANDNE     'IL'
     C     XXST          ANDNE     'IN'
     C     XXST          ANDNE     'KS'
     C     XXST          ANDNE     'KY'
     C     XXST          ANDNE     'MI'
     C     XXST          ANDNE     'MN'
     C     XXST          ANDNE     'MO'
     C     XXST          ANDNE     'ND'
     C     XXST          ANDNE     'NE'
     C     XXST          ANDNE     'OH'
     C     XXST          ANDNE     'SD'
     C     XXST          ANDNE     'TN'
     C     XXST          ANDNE     'WI'
     C*
     C     RCKY          CHAIN     PRC01                              99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    PRC01
     C     RCKY          READE     PRC01                                  99
     C                   ENDDO
     C                   DELETE    PGZ01
     C                   ELSE
     C                   UPDATE    PGZ01
     C                   ENDIF
     C*
     C* GET NEXT RECORD
     C*
     C                   READ      PGZ01                                  99
     C                   ENDDO
     C*
     C* END THE PROGRAM
     C*
     C                   SETON                                        LR
