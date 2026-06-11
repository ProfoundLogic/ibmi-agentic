     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXFEE - UPDATE WDELP FEE FIELDS IF NECESSARY
     H*****************************************************************
     H*                                                               *
     H* PROGRAMMER: C:65                                              *
     H* CREATED:    11/30/01            PROJECT: SUPPORT              *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H* 61  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FWDELL     UF   E           K DISK
     FWMEAL     IF   E           K DISK
     FWTABL     IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  TABLE FILE KEY
     C*
     C     TBKEY         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    XXKEY            10
     C*
     C*   KEY LIST FOR THE EMPLOYER A/R FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   TIME                    DSDTTM
     C*
     C* READ ALL EMPLOYER DETAIL RECORDS.
     C*
     C                   READ      PEL01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C* CHECK RECORDS WITH 0 FEE AMOUNTS.
     C*
     C     ELMFAM        IFEQ      *ZERO
     C     ELSFAM        ANDEQ     *ZERO
     C     EL1AAM        ANDEQ     *ZERO
     C     EL2AAM        ANDEQ     *ZERO
     C     EL3AAM        ANDEQ     *ZERO
     C*
     C                   Z-ADD     0             XXMFAM           11 2
     C                   Z-ADD     0             XXSFAM           11 2
     C                   Z-ADD     0             XX1AAM           11 2
     C                   Z-ADD     0             XX2AAM           11 2
     C                   Z-ADD     0             XX3AAM           11 2
     C*
     C* ACCUMULATE A/R TOTALS FOR FEES.
     C*
     C     EAKY          CHAIN     PEA01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EAARTY        IFEQ      'F'
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   SELECT
     C     TTCTYP        WHENEQ    'M'
     C                   ADD       EAAMT         XXMFAM
     C     TTCTYP        WHENEQ    'S'
     C                   ADD       EAAMT         XXSFAM
     C     TTCTYP        WHENEQ    '1'
     C                   ADD       EAAMT         XX1AAM
     C     TTCTYP        WHENEQ    '2'
     C                   ADD       EAAMT         XX2AAM
     C     TTCTYP        WHENEQ    '3'
     C                   ADD       EAAMT         XX3AAM
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C     EAKY          READE     PEA01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* UPDATE RECORD WITH FEE AMOUNTS IF NECESSARY.
     C*
     C     XXMFAM        IFNE      *ZERO
     C     XXSFAM        ORNE      *ZERO
     C     XX1AAM        ORNE      *ZERO
     C     XX2AAM        ORNE      *ZERO
     C     XX3AAM        ORNE      *ZERO
     C*
     C                   Z-ADD     XXMFAM        ELMFAM
     C                   Z-ADD     XXSFAM        ELSFAM
     C                   Z-ADD     XX1AAM        EL1AAM
     C                   Z-ADD     XX2AAM        EL2AAM
     C                   Z-ADD     XX3AAM        EL3AAM
     C*
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C*
     C                   UPDATE    PEL01
     C*
     C                   ENDIF                                                  XXMFAM IFNE
     C*
     C                   ENDIF                                                  ELMFAM IFEQ
     C*
     C* READ NEXT EMPLOYER DETAIL.
     C*
     C                   READ      PEL01                                  60
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
     C*
