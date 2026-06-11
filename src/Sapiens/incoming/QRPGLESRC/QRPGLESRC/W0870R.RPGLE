     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0870R - EXTRACT MANAGEMENT SEARCH
     H*
     H*****************************************************************
     H*  PROGRAM DESCRIPTIONS:                                        *
     H*     THIS PROGRAM SEARCHES EXTRACT MANAGEMENT FILE.            *
     H*                                                               *
     H*  CALLED BY: W8070R  - EDI/NOI MAINTENANCE                     *
     H*                                                               *
     H*  CALLS TO:  NONE                                              *
     H*                                                               *
     H*****************************************************************
     H*  PROGRAMMER: C:58   Version 3 New                             *
     H*  CREATED:   07/18/97           PROJECT: WC468                 *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  07/15/99  C:118  PROJECT: WC468                              *
     H*                   Made V3 changes to display file and RPG.    *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*   20       RECORDS NOT FOUND                                  *
     H*   50       SUBFILE DISPLAY                                    *
     H*   59-65    GENERAL CHAIN & READ                               *
     H*****************************************************************
     H*
     FW0870F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:RRN)
     F                                     INFDS(FIDS01)
     FWMM1L     IF   E           K DISK
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
     C*
     C                   EXSR      INIT
     C*
     C                   EXSR      SCR01T
     C*
     C                   SELECT
     C*
     C*** F12 = CANCEL
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
     C                   MOVE      SCEXDT        QQEXDT
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
     C* ============================================================
     C*** ROUTINE - SCR01T
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
     C*   Retrieve the Extract File
     C*
     C     M1KY          SETLL     PM101
     C                   EXSR      LOAD
     C*
     C     *IN59         IFEQ      *ON
     C     RRN           ANDEQ     0
     C                   ADD       1             RRN
     C                   WRITE     SCR01S                                       SFL
     C                   END
     C*
     C                   Z-ADD     *ZEROS        XXEXDT
     C                   Z-ADD     *ZEROS        SCPOS
     C                   WRITE     SCR01                                        CMD KEYS
     C                   EXFMT     SCR01C
     C*
     C     SCPOS         IFNE      *ZEROS
     C                   MOVE      SCPOS         XXDATE                         EXTRACT DT
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
     C                   END
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C* ============================================================
     C*   ROUTINE - INIT
     C* ============================================================
     C*
     C     INIT          BEGSR
     C*
     C*   Define Parameter Lists
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQJST             2
     C                   PARM                    QQEXDT            8
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
     C*   Define Key Lists
     C*
     C*          SMCOL - Company Master
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*          WMM1L - Extract Mgt File
     C*
     C     M1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXEXDT
     C*
     C     M1KY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C*
     C     *LIKE         DEFINE    M1CO#         XXCO#
     C     *LIKE         DEFINE    M1JST         XXJST
     C     *LIKE         DEFINE    M1EXDT        XXEXDT
     C     *LIKE         DEFINE    M1EXDT        XXDATE
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQJST         XXJST
     C                   MOVE      QQEXDT        XXDATE
     C                   Z-ADD     XXDATE        SRDTI                           ACK ENTERED
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDYY--> CYMD
     C                   MOVEL(P)  SRDTO         XXEXDT
     C*
     C*** Retrieve Company Record
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       CONAME
     C***** BEGIN ADD **** 07/15/99 ***********************************
     C                   ELSE
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C***** END   ADD **** 07/15/99 ***********************************
     C                   END
     C*
     C*   Initiallize Screen Fields
     C*
     C***** BEGIN DEL **** 07/15/99 ***********************************
     C**********           MOVE Q1USER    SCUSER
     C***** END   DEL **** 07/15/99 ***********************************
     C*
     C                   MOVE      PRDS(1)       SCPRDS
     C*
     C***** BEGIN ADD **** 07/15/99 ***********************************
     C                   MOVEL(P)  SCPRDS        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C***** END   ADD **** 07/15/99 ***********************************
     C*
     C***** BEGIN DEL **** 07/15/99 ***********************************
     C**********           MOVELCONAME    SCCONM
     C***** END   DEL **** 07/15/99 ***********************************
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
     C     M1KY2         READE     PM101                                  59
     C*
     C     *IN59         IFEQ      *OFF
     C                   Z-ADD     M1EXDT        SRDTI                           EXTRACT
     C                   CALL      'SRCVTDAT'    DTMDYY                         CYMD--> MDYY
     C                   MOVEL(P)  SRDTO         SCEXDT
     C                   Z-ADD     M1OTDT        SRDTI                           ORIG TRANS
     C                   CALL      'SRCVTDAT'    DTMDYY                         CYMD--> MDYY
     C                   MOVEL(P)  SRDTO         SCOTDT
     C                   Z-ADD     M1MTCD        SRDTI                          SUBM SENT
     C                   CALL      'SRCVTDAT'    DTMDYY                         CYMD--> MDYY
     C                   MOVEL(P)  SRDTO         SCMTCD
     C                   Z-ADD     M1ARCV        SRDTI                          ACK RCVD
     C                   CALL      'SRCVTDAT'    DTMDYY                         CYMD--> MDYY
     C                   MOVEL(P)  SRDTO         SCARCV
     C                   Z-ADD     M1AENT        SRDTI                           ACK ENTERED
     C                   CALL      'SRCVTDAT'    DTMDYY                         CYMD--> MDYY
     C                   MOVEL(P)  SRDTO         SCAENT
     C*
     C                   MOVEL     M1TSET        SCTSET                         MTC
     C                   ADD       1             RRN
     C                   WRITE     SCR01S
     C                   END
     C                   ENDDO
     C*
     C                   ENDSR
      * ************************************************************************
** PGM DESC
    EXTRACT MANAGEMENT SEARCH
