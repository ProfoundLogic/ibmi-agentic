     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE FIXEMEBP
     H*****************************************************************
     H*
     H*  CREATED:  09/29/98  C:35  PROJECT: SUPP (WA071 #13814)
     H*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H*****************************************************************
     FEMEBL     UF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMEML     IF   E           K DISK
      *
      *

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D                 DS
     D  CD                     1     35  0
     D                                     DIM(5)                               CX DATES
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
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
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      * EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    EBCO#
     C                   KFLD                    EBFND
     C                   KFLD                    EBEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    EBYEAR
      *
      * EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    EBCO#
     C                   KFLD                    EBFND
     C                   KFLD                    EBEMP#
     C                   KFLD                    XXDIV
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     0             XXDIV             5 0
      *
      *  BEGIN MAIN LOOP
      *
     C                   READ      PEB01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     ELKY          CHAIN     PEL01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDTE
      *
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE             8 0
     C                   PARM      1             SRPST             5 0
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
      *
     C                   Z-ADD     SRDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
      *
     C     EMKY          CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   Z-ADD     1             X                 1 0
     C                   SETOFF                                       61
      *
     C     X             DOWLE     5
     C     *IN61         ANDEQ     *OFF
      *
     C     CD(X)         IFGE      ELPFDT
     C     CD(X)         ANDLE     ELPTDT
     C                   MOVE      *ON           *IN61
     C                   ENDIF                                                  CD,X IFGE ELPFDT
      *
     C  N61              ADD       1             X
      *
     C                   ENDDO                                                  X DOWLE 5
      *
     C     *IN61         IFEQ      *ON
     C                   Z-ADD     CD(X)         EBTODT
     C                   ELSE
     C                   Z-ADD     XXDATE        EBTODT
     C                   ENDIF                                                  *IN61 IFEQ *ON
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EBDT
     C                   Z-ADD     DSTIME        EBTIME
     C                   MOVE      Q1PGM         EBPGM
     C                   MOVE      Q1USER        EBUSER
      *
     C                   UPDATE    PEB01
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   READ      PEB01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C                   MOVE      '1'           *INLR
