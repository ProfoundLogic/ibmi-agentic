     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE F0870R - EXTRACT MANAGEMENT SEARCH
     H*
     H*****************************************************************
     H*  PROGRAM DESCRIPTIONS:                                        *
     H*     THIS PROGRAM SEARCHES EXTRACT MANAGEMENT FILE.            *
     H*                                                               *
     H*  CALLED BY: F8070R                                            *
     H*                                                               *
     H*  CALLS TO:  NONE                                              *
     H*                                                               *
     H*  PROGRAMMER: C:47                                             *
     H*  CREATED:   03/14/97           PROJECT: 87MBN                 *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*   20       RECORDS NOT FOUND                                  *
     H*   50       SUBFILE DISPLAY                                    *
     H*   59-65    GENERAL CHAIN & READ                               *
     H*****************************************************************
     H*
     FF0870F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:RRN)
     F                                     INFDS(FIDS01)
     FFMM1L     IF   E           K DISK
     FSMCOL     IF   E           K DISK
      *

     D PRDS            S             40    DIM(1) CTDATA PERRCD(1)
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
      /EJECT
      * ============================================================
      *        M A I N L I N E
      * ============================================================
      *

     C                   EXSR      INIT
      *
     C                   EXSR      SCR01T
      *
     C                   SELECT
      *** F3  = EXIT
      *
     C     *IN03         WHENEQ    *ON
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
      *
      *** F12 = CANCEL
      *
     C     *IN12         WHENEQ    *ON
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
      *
     C                   ENDSL                                                  END CMD KEYS
      *
      *  CHECK FOR SEARCH SELECTION
      *
     C     1             CHAIN     SCR01S                             50
     C     *IN50         DOWEQ     *OFF
      *
     C     SCSEL         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      SCEXDT        QQEXDT
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   READC     SCR01S                                 50
     C                   ENDDO
      *
      *** END OF PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /EJECT
      * ============================================================
      *** ROUTINE - SCR01T
      * ============================================================
     C     SCR01T        BEGSR
      *
      *** Display Formats
      *
     C     *IN03         DOUEQ     *ON
     C     *IN12         OREQ      *ON
     C     SCPOS         OREQ      *ZEROS
      *
     C                   Z-ADD     0             RRN
     C                   MOVE      *ON           *IN50
     C                   WRITE     SCR01C
     C                   MOVE      *OFF          *IN50
      *
      *** Retrieve the Extract File
      *
     C     M1KY          SETLL     PM101
     C                   EXSR      LOAD
      *
     C     *IN59         IFEQ      *ON
     C     RRN           ANDEQ     0
     C                   ADD       1             RRN
     C                   WRITE     SCR01S                                       SFL
     C                   END
      *
     C                   Z-ADD     *ZEROS        XXEXDT
     C                   Z-ADD     *ZEROS        SCPOS
     C                   WRITE     SCR01                                        CMD KEYS
     C                   EXFMT     SCR01C
      *
     C     SCPOS         IFNE      *ZEROS
     C                   MOVE      SCPOS         XXDATE                         EXTRACT DT
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
     C                   END
      *
     C                   ENDDO
      *
     C                   ENDSR
      * ============================================================
      *** ROUTINE - INIT
      * ============================================================
      *
     C     INIT          BEGSR
      *
      *** Define Parameter Lists
      *
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
      *
      *** Define Key Lists
      *
      ***        SMCOL - Company Master
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      ***        WMM1L - Extract Mgt File
      *
     C     M1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXEXDT
      *
     C     M1KY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
      *
     C     *LIKE         DEFINE    M1CO#         XXCO#
     C     *LIKE         DEFINE    M1JST         XXJST
     C     *LIKE         DEFINE    M1EXDT        XXEXDT
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQJST         XXJST
     C                   MOVE      QQEXDT        XXDATE            8 0
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
      *
      *** Retrieve Company Record
      *
     C     COKY          CHAIN     PCO01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       CONAME
     C                   END
      *
      *** Initiallize Screen Fields
      *
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      PRDS(1)       SCPRDS
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVE      XXJST         SCJST
     C                   MOVE      *BLANKS       SCMSG
      *
     C                   TIME                    DSDTTM
      *
     C                   ENDSR
      /EJECT
      * ============================================================
      *** ROUTINE - LOAD
      * ============================================================
     C     LOAD          BEGSR
      *
     C     *IN59         DOUEQ     *ON
     C     M1KY2         READE     PM101                                  59
      *
     C     *IN59         IFEQ      *OFF
     C                   Z-ADD     M1EXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C                   Z-ADD     M1OTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCOTDT
     C                   Z-ADD     M1MTCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMTCD
     C                   Z-ADD     M1ARCV        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCARCV
     C                   Z-ADD     M1AENT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCAENT
     C                   MOVEL     M1TSET        SCTSET                         MTC
     C                   ADD       1             RRN
     C                   WRITE     SCR01S
     C                   END
     C                   ENDDO
      *
     C                   ENDSR
      * ************************************************************************
** PGM DESC
    EXTRACT MANAGEMENT SEARCH
