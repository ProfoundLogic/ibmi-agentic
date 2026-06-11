     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXDELEMP - CREATE MISSING EMPLOYER MASTER RECORDS
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FWDELL     IF   E           K DISK
     FWMEML     IF A E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D NAME            C                   CONST('DELETED EMPLOYER')
     D ADR1            C                   CONST('100 TEST ADDRESS')
     D CITY            C                   CONST('ORLANDO         ')

     C*
     C* EMPLOYER MASTER KEY
     C*
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C* READ ALL EMPLR DETAIL RECORDS AND CHECK IF CORRESPONDING EMPLR MASTER
     C*
     C                   READ      PEL01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   CLEAR                   PEM01
     C*
     C                   Z-ADD     ELCO#         EMCO#
     C                   Z-ADD     ELFND         EMFND
     C                   Z-ADD     ELEMP#        EMEMP#
     C                   Z-ADD     ELDIV         EMDIV
     C*
     C                   MOVEL     NAME          EMNAME
     C                   MOVEL     ELPFDT        EMEFD1
     C                   MOVEL     ADR1          EMMAD1
     C                   MOVEL     CITY          EMMCTY
     C                   MOVE      'FL'          EMMST
     C                   MOVE      'M'           EMMPFL
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C                   MOVEL     SRDTO         EMIDT
     C                   Z-ADD     DSTIME        EMTIME
     C                   Z-ADD     DSTIME        EMITIM
     C                   MOVEL     Q1PGM         EMPGM
     C                   MOVEL     Q1PGM         EMIPGM
     C*
     C                   WRITE     PEM01
     C                   ENDIF
     C*
     C                   READ      PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   MOVE      *ON           *INLR
     C*
