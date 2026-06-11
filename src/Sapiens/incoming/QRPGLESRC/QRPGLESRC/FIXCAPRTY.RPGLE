     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE FIXCAPRTY - ASSIGN VALUES TO CAPRTY
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM ASSIGNS VALUE TO CAPRTY           *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    COMMAND LINE                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 04/08/02  PROGRAMMER: C:62  PROJECT: FS236          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS:                                                   *
     H*   SCR01:                                                      *
     H*   99  READ  - PCFDCAP                                         *
     H*****************************************************************
     FFDCALR    UF   E           K DISK
     FFAEOP     IF   E             DISK
     F*

     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXCMSV                 1     11
     D  XXRVCD                 1      3
     D  XXHCPC                 4      8
     D  XXUNTS                 9     11

     C*
     C     CAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
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
     C                   READ      PEO01                                  99
     C     *IN99         DOWEQ     '0'
     C*
     C     CAKY          CHAIN     PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C                   MOVEL(P)  '1'           CAPRTY
     C     EOBTYP        WHENEQ    'H'
     C                   MOVEL(P)  '4'           CAPRTY
     C                   OTHER
     C                   MOVEL(P)  EOBTYP        CAPRTY
     C                   ENDSL
     C                   MOVEL(P)  Q1USER        CAUSER
     C                   MOVEL(P)  Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C*
     C                   UPDATE    PCA01
     C     CAKY          READE     PCA01                                  99
     C                   ENDDO
     C*
     C                   READ      PEO01                                  99
     C                   ENDDO
     C*
     C*  END THE PROGRAM
     C*
     C                   SETON                                        LR
