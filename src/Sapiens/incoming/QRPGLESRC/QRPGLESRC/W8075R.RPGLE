     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8075R - TRANSMISSION CONFIRMATION PROCESSOR
     H*
     H*****************************************************************
     H*                                                               *
     H*  CREATED: 06/18/97  for Version 3 BASE
     H*                    C:58  PROJECT: WC468
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 07/28/10 C:220 PROJECT 139WNN - Add key EDIF for WMM2L file
     H*
     H* 07/29/10 C:053 PROJECT 139WNN - PRISS 44
     H*                ADD LOOP TO GET ALL RECORDS WITH THE KEY
     H*
     H*****************************************************************
     FWMM2L     UF   E           K DISK
     FSSYLF     IF   E           K DISK

     D DN              S            150    DIM(999)
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
     C*** Beg   add *** 07/28/10 **************************************
     C                   PARM                    QQEDIF            3
     C*** End   add *** 07/28/10 **************************************
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
     C*  VERIFY EDI CONTROL FLAG IS 'Y'
     C*
     C                   MOVEL     'WC'          WCKEY
     C     WCKEY         CHAIN     SWC01                              60
     C     *IN60         IFEQ      *ON
     C     WCFL09        ORNE      'Y'
     C                   GOTO      TERM
     C                   END
     C*
     C*  RETRIEVE THE TRANSMISSION MANAGEMENT RECORD.
     C*
     C     M2KY          CHAIN     PM201                              60
     C     *IN60         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*** BEG DEL *** 07/29/10 **************************************
     C*** Beg   add *** 07/28/10 **************************************
     C**********           MOVE *OFF      *IN65
     C********** QQEDIF    IFEQ M2EDIF
     C**********           MOVE *ON       *IN65
     C**********           ENDIF
     C********** *IN65     IFEQ *ON
     C*** End   add *** 07/28/10 **************************************
     C*** END DEL *** 07/29/10 **************************************
     C*
     C*** BEG ADD *** 07/29/10 **************************************
     C     *IN60         DOWEQ     *OFF
     C     QQEDIF        IFEQ      M2EDIF
     C*** END ADD *** 07/29/10 **************************************
     C                   MOVEL     'S'           M2STAT
     C                   UPDATE    PM201
     C*** BEG ADD *** 07/29/10 **************************************
     C                   MOVE      *ON           *IN60
     C                   ELSE
     C     M2KY          READE     PM201                                  60
     C*** END ADD *** 07/29/10 **************************************
     C*** Beg   add *** 07/28/10 **************************************
     C                   ENDIF
     C*** End   add *** 07/28/10 **************************************
     C*** BEG ADD *** 07/29/10 **************************************
     C                   ENDDO
     C*** END ADD *** 07/29/10 **************************************
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C**************************************************************************
     C* DEFINE ALL KLISTS, FIELD DEFINITIONS, ETC...
     C**************************************************************************
     C     SRINIT        BEGSR
     C*
     C*  WMUPL - USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  WMM2L - SYSTEM MANAGEMENT RECORDS
     C*
     C     M2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXTSET
     C                   KFLD                    XXOTDT
     C                   KFLD                    XXOTTM
     C*
     C     *LIKE         DEFINE    M2CO#         XXCO#
     C     *LIKE         DEFINE    M2JST         XXJST
     C     *LIKE         DEFINE    M2TSET        XXTSET
     C     *LIKE         DEFINE    M2OTDT        XXOTDT
     C     *LIKE         DEFINE    M2OTTM        XXOTTM
     C                   TIME                    DSDTTM
     C*
     C                   ENDSR
