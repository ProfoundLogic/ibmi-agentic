     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0140R - RATE MASTER MAINTENANCE
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*    10-11-91 *** LPK *** PROJECT AKD ***                      *
     H*      1. ADDED 30 ALPHA DESCRIPTION FIELD.                    *
     H*      2. USER REQUIRED TO ENTER DECIMAL ON NEW RATE AND       *
     H*         RENEW RATE ENTRIES.                                  *
     H****************************************************************
     FW0140F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMARL     UF A E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D QQPRDS          C                   CONST('Rate Maintenance')

     C*
     C*   KEY LIST FOR THE RATE MASTER FILE
     C*
     C     ARKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCRT#
     C                   KFLD                    XXEFDT
     C*
     C     ARKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C*
     C     *LIKE         DEFINE    ARDT          XXEFDT
     C*
     C*   PARAMETER LIST FOR CALLED PROGRAM
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         QQKC              1
     C                   PARM      *INKL         QQKL              1
     C                   PARM                    QQCALL            1
     C                   PARM                    XXCO#
     C                   PARM                    SCFND
     C                   PARM                    QQKEY            10
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
     C     *LIKE         DEFINE    FNCO#         XXCO#
     C*
     C                   MOVEL(P)  QQPRDS        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C*   LOAD THE OUTPUT CONSTANTS FROM THE PARMS
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C     QQCALL        COMP      '1'                                    09
     C*
     C     TOP           TAG
     C*
     C                   MOVEL     *BLANKS       CONAME
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C  N60              MOVEL(P)  CONAME        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   MOVEL     *BLANKS       SCNAME
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C*
     C*  LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*  DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C*  PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  F3=EXIT
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   MOVEL     'A'           QQRTC
     C                   GOTO      TERM
     C                   END
     C*
     C*  F10=SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C                   CALL      'W0640R'
     C                   PARM                    SCPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCRT#
     C                   PARM                    SCEFDT
     C                   PARM                    XXCO#
     C                   PARM                    SCFND
     C                   PARM                    QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCRT#         OREQ      *ZERO
     C     SCEFDT        OREQ      *ZERO
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END
     C*
     C*  F12=CANCEL
     C*
     C     *INKL         IFEQ      *ON
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   END
     C                   END
     C*
     C*  F20=CHANGE STATUS
     C*
     C     *INKU         IFEQ      *ON
     C     *IN40         ANDEQ     *OFF
     C*
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   END
     C*
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  RATE NUMBER MUST BE GREATER THAN ZERO
     C*
     C     SCRT#         CABLE     *ZERO         STRT                   2020
     C*
     C*  VALIDATE EFFECTIVE DATE
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     21
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     ARKY          CHAIN     WMARL                              40
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   EXCEPT    PAR01E
     C*
     C                   MOVEL     UA(2)         SCMODE
     C                   MOVE      ARRT#         SCRT#
     C*
     C                   Z-ADD     AREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVEL     ARSTAT        SCSTAT
     C                   MOVEL     ARDESC        SCDESC
     C                   MOVEL     ARNWRT        SCNWRT
     C                   MOVEL     ARRNRT        SCRNRT
     C*
     C                   ELSE
     C*
     C     *IN09         CABEQ     *ON           STRT                     22
     C*
     C                   MOVEL     UA(1)         SCMODE
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      *BLANK        SCDESC
     C                   Z-ADD     *ZERO         SCNWRT
     C                   Z-ADD     *ZERO         SCRNRT
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*  DESCRIPTION MUST NOT BE BLANK
     C*
     C     SCDESC        CABEQ     *BLANK        STRT                     23
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      *OFF
     C*
     C     ARKY          CHAIN     WMARL                              40
     C*
     C                   Z-ADD     XXCO#         ARCO#
     C                   Z-ADD     SCFND         ARFND
     C                   MOVEL     SCRT#         ARRT#
     C                   Z-ADD     XXEFDT        AREFDT
     C                   MOVEL     SCNWRT        ARNWRT
     C                   MOVEL     SCRNRT        ARRNRT
     C                   MOVEL     SCDESC        ARDESC
     C                   MOVE      SCSTAT        ARSTAT
     C                   MOVE      Q1USER        ARUSER
     C                   MOVE      Q1PGM         ARPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   Z-ADD     DSTIME        ARTIME
     C*
     C     *IN40         IFEQ      *OFF
     C                   UPDATE    PAR01
     C                   ELSE
     C                   WRITE     PAR01
     C                   END
     C*
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     *ZERO         SCRT#
     C                   Z-ADD     *ZERO         SCEFDT
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     OPAR01     E            PAR01E
**  TABLE UA
-- ADD --
- UPDATE -
