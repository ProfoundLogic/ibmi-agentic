     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXWMECP - Copy Employer Contact Info to WMECP
     H*****************************************************************
     H* DESCRIPTION:
     H*   Run with first install of WC1070.  Copies Employer contact
     H*   information to WMECP records.
     H*      EMCONT -> WMECP ECPCNT=1
     H*      EMATTN -> WMECP ECPCNT=2
     H*      EMCNT3 -> WMECP ECPCNT=3
     H*
     H*   Pgm starts from right to left on Employer contact field.
     H*   First string placed into Last Name ECLSN.  Any remaining
     H*   preceding text is placed in First Name Field.
     H*
     H*****************************************************************
      *
      * Employer Master
     FWMEML     IF   E           K DISK
      *
      * Employer Contact
     FWMECL1    IF A E           K DISK
      *
      *
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *
      *===============================================================
      * KLIST
      *===============================================================
      *
      *  Employer Contact
      *

     C     ECKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKPCNT            1
      *
      *
      *
      *===============================================================
      * PLIST
      *===============================================================
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *
      *
      *
      *===============================================================
      * *LIKE
      *===============================================================
     C     *LIKE         DEFINE    EMCONT        XXCONT
     C     *LIKE         DEFINE    EMCONT        XXTMP1
      *
      *
      *
      *===============================================================
      * BEGIN
      *===============================================================
     C                   READ      PEM01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVE      '1'           KKPCNT
     C                   EXSR      SRCONT
      *
     C                   MOVE      '2'           KKPCNT
     C                   EXSR      SRCONT
      *
     C                   MOVE      '3'           KKPCNT
     C                   EXSR      SRCONT
      *
     C                   READ      PEM01                                  60
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
      *
      *
      *
      *===============================================================
      * SRCONT - Get Employer Contact Names and phone
      *===============================================================
      *
     C     SRCONT        BEGSR
      *
     C                   SELECT
      *
     C     KKPCNT        WHENEQ    '1'
     C                   MOVEL(P)  EMCONT        XXCONT
      *
     C     KKPCNT        WHENEQ    '2'
     C                   MOVEL(P)  EMATTN        XXCONT
      *
     C     KKPCNT        WHENEQ    '3'
     C                   MOVEL(P)  EMCNT3        XXCONT
      *
     C                   OTHER
     C                   MOVEL(P)  *BLANKS       XXCONT
      *
     C                   ENDSL
      *
     C                   CLEAR                   PEC01
      *
     C     XXCONT        IFNE      *BLANKS
      *
      * Write Contact Name
      *
     C     ECKY          CHAIN     PEC01                              60
      *
     C     *IN60         IFEQ      *ON
      *
     C                   MOVEL(P)  XXCONT        ECFSN
     C                   MOVE      XXCONT        XXTMP            25
     C                   MOVEL(P)  XXTMP         ECLSN
      *
     C     ECLSN         IFNE      *BLANKS
     C     ECFSN         ORNE      *BLANKS
     C                   Z-ADD     EMCO#         ECCO#
     C                   Z-ADD     EMFND         ECFND
     C                   Z-ADD     EMEMP#        ECEMP#
     C                   Z-ADD     EMDIV         ECDIV
     C                   MOVE      KKPCNT        ECPCNT
      *
     C                   MOVEL     Q1USER        ECIUSR
     C                   MOVEL     Q1USER        ECUSER
      *
     C                   MOVEL     Q1PGM         ECIPGM
     C                   MOVEL     Q1PGM         ECPGM
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ECIDT
     C                   MOVEL     SRDTO         ECDT
      *
     C                   MOVEL     DSTIME        ECITIM
     C                   MOVEL     DSTIME        ECTIME
      *
     C                   WRITE     PEC01
     C                   ENDIF                                                  LSN, FSN
      *
     C                   ENDIF                                                  ECKY
      *
     C                   ENDIF                                                  XXCONT
      *
     C                   ENDSR                                                  SRCONT
      *
      *
