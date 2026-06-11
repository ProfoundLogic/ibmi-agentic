     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE F6212R - TRANSMISSION CONFIRMATION PROCESSOR
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FFEMFL     UF   E           K DISK

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   ENTRY PARAMETERS (ALL ALPHANUMERIC)
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQJST             2
     C                   PARM                    QQOTDT            8
     C                   PARM                    QQOTTM            6
     C                   PARM                    QQTSET            3
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQJST         XXJST
     C                   MOVE      QQOTDT        X1OTDT            8 0
     C                   MOVE      QQOTTM        XXOTTM
     C                   MOVE      QQTSET        XXTSET
     C*
     C                   Z-ADD     X1OTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXOTDT
     C*
     C*   INITIALIZATION SUBROUTINE
     C*
     C                   EXSR      SRINIT
     C*
     C*  RETRIEVE THE TRANSMISSION MANAGEMENT RECORD.
     C*
     C     MFKY          CHAIN     PMF01                              60
     C     *IN60         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C                   MOVEL     'S'           MFSTAT
     C                   UPDATE    PMF01
     C*
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C**************************************************************************
     C* DEFINE ALL KLISTS, FIELD DEFINITIONS, ETC...
     C**************************************************************************
     C     SRINIT        BEGSR
     C*
     C*  FEMFL - 837 MANAGEMENT RECORDS
     C*
     C     MFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXTSET
     C                   KFLD                    XXOTDT
     C                   KFLD                    XXOTTM
     C*
     C     *LIKE         DEFINE    MFCO#         XXCO#
     C     *LIKE         DEFINE    MFJST         XXJST
     C     *LIKE         DEFINE    MFTSET        XXTSET
     C     *LIKE         DEFINE    MFOTDT        XXOTDT
     C     *LIKE         DEFINE    MFOTTM        XXOTTM
     C                   TIME                    DSDTTM
     C*
     C                   ENDSR
