     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F7071R2 - CHANGE EOSTAT TO A 'P'                         *
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* PROGRAMMER: C:94                                              *
     H* CREATED:    01/29/99  PROJECT: 48MBV                          *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/27/99 C:62 PROJECT:MC201                                   *
     H*               REMOVE UPDATE OF EOFLG3.                        *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60 - GENERAL CHAIN INDICATOR                                  *
     H* 61 - GENERAL CHAIN INDICATOR                                  *
     H*                                                               *
     H*****************************************************************
     F*** EOMB File selected on EOSTAT *NE 'P'
     FFAEOL2    UF   E           K DISK
     F*

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)

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
     C*  INITIALIZE WORK FIELDS
     C*
     C*
     C                   TIME                    DSDTTM
     C*
     C* READ THE EOB FILE
     C*
     C                   READ      PEO01                                  60
     C     *IN60         DOWEQ     *OFF
     C                   MOVEL     'P'           EOSTAT
     C*** BEGIN DEL *** 07/27/99 **************************************
     C**********           MOVEL'Y'       EOFLG3
     C*** END   DEL *** 07/27/99 **************************************
     C*
     C                   MOVE      Q1PGM         EOPGM
     C*** BEGIN ADD *** 07/27/99 **************************************
     C                   MOVEL(P)  Q1USER        EOUSER
     C*** END   ADD *** 07/27/99 **************************************
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*
     C                   Z-ADD     DSTIME        EOTIME
     C*
     C                   UPDATE    PEO01
     C*
     C                   READ      PEO01                                  60
     C                   ENDDO
      *
     C                   SETON                                        LR
