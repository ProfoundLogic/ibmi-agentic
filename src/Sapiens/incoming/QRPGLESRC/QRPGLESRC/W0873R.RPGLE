     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0873R - TRANSMISSION MANAGEMENT SEARCH
     H*
     H*****************************************************************
     H*  PROGRAM DESCRIPTIONS:                                        *
     H*     THIS PROGRAM SEARCHES TRANSMISSION MANAGEMENT FILE.       *
     H*                                                               *
     H*  CALLED BY: W8073R  - EDI/NOI MAINTENANCE                     *
     H*                                                               *
     H*  CALLS TO:  NONE                                              *
     H*                                                               *
     H*****************************************************************
     H*  PROGRAMMER: C:58   Version 3 New                             *
     H*  CREATED:   07/18/97           PROJECT: WC468                 *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  08/09/99  C:118  PROJECT: WC468                              *
     H*                   Made V3 display file chgs.                  *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*   20       RECORDS NOT FOUND                                  *
     H*   50       SUBFILE DISPLAY                                    *
     H*   59-65    GENERAL CHAIN & READ                               *
     H*****************************************************************
     H*
     FW0873F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:RRN)
     F                                     INFDS(FIDS01)
     FWMM2L1    IF   E           K DISK
     FSMCOL     IF   E           K DISK
     F*

     D PRDS            S             40    DIM(1) CTDATA PERRCD(1)
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C/EJECT
     C* ============================================================
     C*        M A I N L I N E
     C* ============================================================
     C                   EXSR      INIT
     C*
     C                   EXSR      SCR01T
     C*
     C                   SELECT
     C*
     C*   F12 = CANCEL
     C*
     C     *IN12         WHENEQ    *ON
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C*
     C                   ENDSL                                                  END CMD KEYS
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C     1             CHAIN     SCR01S                             50
     C     *IN50         DOWEQ     *OFF
     C*
     C     SCSEL         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      SCOTDT        QQOTDT
     C                   MOVE      SCOTTM        QQOTTM
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C                   READC     SCR01S                                 50
     C                   ENDDO
     C*
     C*   END OF PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C/EJECT
     C* ============================================================
     C*   ROUTINE - SCR01T
     C* ============================================================
     C     SCR01T        BEGSR
     C*
     C*   Display Formats
     C*
     C     *IN12         DOUEQ     *ON
     C     SCPOS         OREQ      *ZEROS
     C*
     C                   Z-ADD     0             RRN
     C                   MOVE      *ON           *IN50
     C                   WRITE     SCR01C
     C                   MOVE      *OFF          *IN50
     C*
     C*   Retrieve the Submission File
     C*
     C     M2KY          SETLL     PM201
     C                   EXSR      LOAD
     C*
     C     *IN59         IFEQ      *ON
     C     RRN           ANDEQ     0
     C                   ADD       1             RRN
     C                   WRITE     SCR01S                                       SFL
     C                   END
     C*
     C                   Z-ADD     *ZEROS        XXOTDT
     C                   Z-ADD     *ZEROS        XXOTTM
     C                   Z-ADD     *ZEROS        SCPOS
     C                   WRITE     SCR01                                        CMD KEYS
     C                   EXFMT     SCR01C
     C*
     C     SCPOS         IFNE      *ZEROS
     C                   MOVE      SCPOS         XXDATE                         Trans Dt
     C                   Z-ADD     XXDATE        SRDTI                           ACK ENTERED
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDYY--> CYMD
     C                   MOVEL(P)  SRDTO         XXOTDT
     C                   Z-ADD     *ZEROS        XXOTTM
     C                   END
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C* ============================================================
     C*** ROUTINE - INIT
     C* ============================================================
     C*
     C     INIT          BEGSR
     C*
     C*** Define Parameter Lists
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQJST             2
     C                   PARM                    QQOTDT            8
     C                   PARM                    QQOTTM            6
     C                   PARM                    QQRTCD            1
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
     C*** Define Key Lists
     C*
     C***        SMCOL - Company Master
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C***        WMM2L - Transmission Mgt File
     C*
     C     M2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXOTDT
     C                   KFLD                    XXOTTM
     C*
     C     M2KY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C*
     C     *LIKE         DEFINE    M2CO#         XXCO#
     C     *LIKE         DEFINE    M2JST         XXJST
     C     *LIKE         DEFINE    M2OTDT        XXOTDT
     C     *LIKE         DEFINE    M2OTTM        XXOTTM
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQJST         XXJST
     C                   MOVE      QQOTDT        XXDATE            8 0
     C                   Z-ADD     XXDATE        SRDTI                           ACK ENTERED
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDYY--> CYMD
     C                   MOVEL(P)  SRDTO         XXOTDT
     C                   MOVE      *ZEROS        XXOTTM
     C*
     C*   Retrieve Company Record
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       CONAME
     C                   END
     C*
     C*   Initialize Screen Fields
     C*
     C***** BEGIN DEL **** 08/09/99 ***********************************
     C**********           MOVE Q1USER    SCUSER
     C***** END   DEL **** 08/09/99 ***********************************
     C                   MOVE      PRDS(1)       SCPRDS
     C                   MOVEL     CONAME        SCCONM
     C                   MOVE      XXJST         SCJST
     C                   MOVE      *BLANKS       SCMSG
     C*
     C                   TIME                    DSDTTM
     C*
     C                   ENDSR
     C/EJECT
     C* ============================================================
     C*** ROUTINE - LOAD
     C* ============================================================
     C     LOAD          BEGSR
     C*
     C     *IN59         DOUEQ     *ON
     C     M2KY2         READE     PM201                                  59
     C*
     C     *IN59         IFEQ      *OFF
     C                   Z-ADD     M2OTDT        SRDTI                           ORIG TRANS
     C                   CALL      'SRCVTDAT'    DTMDYY                         CYMD--> MDYY
     C                   MOVEL(P)  SRDTO         SCOTDT
     C                   MOVE      M2OTTM        SCOTTM                         TRANSMSN TM
     C                   Z-ADD     M2LTDT        SRDTI                           LAST TRANS
     C                   CALL      'SRCVTDAT'    DTMDYY                         CYMD--> MDYY
     C                   MOVEL(P)  SRDTO         SCLTDT
     C                   Z-ADD     M2ARCV        SRDTI                          ACK RECEIVED
     C                   CALL      'SRCVTDAT'    DTMDYY                         CYMD--> MDYY
     C                   MOVEL(P)  SRDTO         SCARCV
     C                   Z-ADD     M2AENT        SRDTI                          ACK ENTERED
     C                   CALL      'SRCVTDAT'    DTMDYY                         CYMD--> MDYY
     C                   MOVEL(P)  SRDTO         SCAENT
     C                   MOVEL     M2TSET        SCTSET                         MTC
     C                   ADD       1             RRN
     C                   WRITE     SCR01S
     C                   END
     C                   ENDDO
     C*
     C                   ENDSR
     C* ************************************************************************
** PGM DESC
  TRANSMISSION MANAGEMENT SEARCH
