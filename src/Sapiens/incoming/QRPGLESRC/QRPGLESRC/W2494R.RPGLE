     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W2494R - CLIENT TABLE ID CODE MAINTENANCE
     H*****************************************************************
     H* PROGRAM DESCIPTION:  THIS PROGRAM ALLOWS THE USER TO ENTER    *
     H*    AND UPDATE CLIENT TABLE ID CODES.                          *
     H*                                                               *
     H* CALLED FROM:  NONE                                            *
     H*                                                               *
     H* CALLS TO:  W2495R                                             *
     H*                                                               *
     H* CREATED:  07/24/98            PROJECT: 60ARN                  *
     H* Adapted from W2490R                                           *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*     *IN09 - INQUIRY MODE                                      *
     H*     *IN40 - ADD MODE                                          *
     H*     *IN60 - CHAIN (GENERAL USAGE)                             *
     H*                                                               *
     H*   SCR01:                                                      *
     H*     *IN20 - VALUE CANNOT BE BLANK                             *
     H*     *IN21 - RECORD NOT FOUND                                  *
     H*                                                               *
     H*   SCR02:                                                      *
     H*     *IN20 - VALUE CANNOT BE BLANK                             *
     H*     *IN21 - VALUE CANNOT BE BLANK                             *
     H*                                                               *
     H*****************************************************************
      *
     FW2494F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWTCIL     UF A E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
      *
      *** Constants
      *
     D XCTKLB          C                   CONST('       Table ID:')
     D XCKDSC          C                   CONST('Table ID:')
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *  KEY LIST FOR THE COMPANY FILE
      *

     C     COKY          KLIST
     C                   KFLD                    UPCO#
      *
      *  KEY LIST FOR USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *  KEY LIST FOR ID TABLE FILE
      *
     C     IDKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXID
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     *LIKE         DEFINE    CITBID        QQID
     C     *LIKE         DEFINE    CITBID        XXID
      *
      *  GET THE USER PARAMETER FILE
      *
     C     TOP           TAG
      *
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
      *
      *  GET THE COMPANY RECORD
      *
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
      *
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCID
      *
      *   DISPLAY ALL FORMATS
      *
     C     STRT          TAG
      *
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C     ' '           CHECKR    Q2RECF        @P                5 0
     C     10            SUB       @P            @P
     C                   MOVE      *BLANKS       SCRECF
     C     SCRECF        CAT       Q2RECF:@P     SCRECF                          FOR DISPLAY
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      XCTKLB        SCTKLB
     C                   EXFMT     SCR01
     C                   END
      *
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     XCKDSC        SCKDSC
     C                   EXFMT     SCR02
     C                   END
      *
      *   PROCESS ALL COMMAND KEYS
      *
     C     *IN11         IFEQ      '1'
      *
      *   COMMAND 03 = END OF JOB
      *
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
      *
      *   COMMAND 12 = PRIOR SCREEN
      *
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
      *
      *   COMMAND 05 = SEARCH LIST CALL
      *
     C     *INKJ         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      *BLANK        QQID
     C                   MOVEL     SCID          QQID
     C                   CALL      'W2495R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQID
     C                   PARM                    QQRTC             1
     C                   MOVEL     QQID          SCID
     C     QQRTC         CABEQ     'A'           TERM
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
     C     QQRTC         IFEQ      'B'
     C     QQID          OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
      *
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END
      *
     C                   END
      *
      *   CHANGE STATUS
      *
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   END
     C                   GOTO      STRT
     C                   END
      *
      *   PROCESS HELP REQUESTS
      *
     C     *IN10         IFEQ      '1'
      *
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
      *
     C                   GOTO      STRT
     C                   END
      *
      *   PROCESS CHANGE OF USER PARAMETERS
      *
     C     *INKM         IFEQ      '1'
      *
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
      *
     C                   END
      *
      *   GOTO THE APPROPRIATE SCREEN
      *
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
      *
      *   PROCESS REQUEST SCREEN
      *
     C     SCR01T        TAG
      *
      *  THE ID CODE CANNOT BE EQUAL TO BLANKS
      *
     C     SCID          IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
      *
      *  GET THE REQUESTED RECORD, LOAD INFORMATION
      *
     C                   MOVE      *BLANK        XXID
     C                   MOVEL     SCID          XXID
     C     IDKY          CHAIN     PCI01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PCI01E
      *
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     CIDESC        SCDESC
     C                   MOVEL     CIFN01        SCFN01
     C                   MOVE      CISTAT        SCSTAT
      *
     C                   ELSE
      *
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
      *
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCFN01
     C                   MOVE      'A'           SCSTAT
      *
     C                   END
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
      *
     C     SCR02T        TAG
      *
      *  THE DESCRIPTION CANNOT BE BLANK
      *
     C     SCDESC        IFEQ      *BLANKS
     C     *IN09         ANDEQ     '0'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
      *
      *  THE FIELD LABEL CANNOT BE BLANK
      *
     C     SCFN01        IFEQ      *BLANKS
     C     *IN09         ANDEQ     '0'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
      *
      *  ALL EDITS HAVE BEEN PASSED
      *
     C     *IN09         IFEQ      '0'
     C                   MOVE      *BLANK        XXID
     C                   MOVEL     SCID          XXID
     C     IDKY          CHAIN     PCI01                              40
     C                   MOVE      UPCO#         CICO#
     C                   MOVE      *BLANK        CITBID
     C                   MOVEL     SCID          CITBID
     C                   MOVEL     SCDESC        CIDESC
     C                   MOVEL     SCFN01        CIFN01
     C                   MOVE      SCSTAT        CISTAT
     C                   MOVE      Q1USER        CIUSER
     C                   MOVE      Q1PGM         CIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CIDT
     C                   Z-ADD     DSTIME        CITIME
      *
     C     *IN40         IFEQ      '0'
      *
     C                   UPDATE    PCI01
      *
     C                   ELSE
      *
     C                   MOVE      Q1USER        CIIUSR
     C                   MOVE      Q1PGM         CIIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CIIDT
     C                   Z-ADD     DSTIME        CIITM
      *
     C                   WRITE     PCI01
      *
     C                   END
      *
     C                   END
      *
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCID
     C                   GOTO      STRT
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPCI01     E            PCI01E
**  TABLE UA
-- ADD --
- UPDATE -
