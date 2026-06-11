     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W2490R - SYSTEM TABLE ID CODE MAINTENANCE
     H*****************************************************************
     H* PROGRAM DESCIPTION:  THIS PROGRAM ALLOWS THE USER TO ENTER    *
     H*    AND UPDATE SYSTEM TABLE ID CODES.                          *
     H*                                                               *
     H* CALLED FROM:  NONE                                            *
     H*                                                               *
     H* CALLS TO:  W2491R                                             *
     H*                                                               *
     H* CREATED:  08/07/96            PROJECT: WC523                  *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  07/24/98 C:61 PROJECT 60ARN                                  *
     H*                Change F5=Search to F10=Search                 *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*     *IN09 - INQUIRY MODE                                      *
     H*     *IN40 - ADD MODE                                          *
     H*     *IN60 - CHAIN (GENERAL USAGE)                             *
     H*   SCR01:                                                      *
     H*     *IN20 - VALUE CANNOT BE BLANK                             *
     H*     *IN21 - RECORD NOT FOUND                                  *
     H*   SCR02:                                                      *
     H*     *IN20 - VALUE CANNOT BE BLANK                             *
     H*     *IN21 - VALUE CANNOT BE BLANK                             *
     H*****************************************************************
     FW2490F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWTGIL     UF A E           K DISK

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

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*  KEY LIST FOR USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR ID TABLE FILE
     C*
     C     IDKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXID
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C     *LIKE         DEFINE    GITBID        QQID
     C     *LIKE         DEFINE    GITBID        XXID
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCID
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C     ' '           CHECKR    Q2RECF        @P                5 0
     C     10            SUB       @P            @P
     C                   MOVE      *BLANKS       SCRECF
     C     SCRECF        CAT       Q2RECF:@P     SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      XCTKLB        SCTKLB
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     XCKDSC        SCKDSC
     C                   EXFMT     SCR02
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*   COMMAND 05 = SEARCH LIST CALL
     C*
      ***** BEG CHG ******* 07/24/98 **********************************
     C********** *INKE     IFEQ '1'
     C     *INKJ         IFEQ      '1'
      ***** END CHG ******* 07/24/98 **********************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      *BLANK        QQID
     C                   MOVEL     SCID          QQID
     C                   CALL      'W2491R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQID
     C                   PARM                    QQRTC             1
     C                   MOVEL     QQID          SCID
     C     QQRTC         CABEQ     'A'           TERM
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQID          OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END
     C*
     C                   END
     C*
     C*   CHANGE STATUS
     C*
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   END
     C                   GOTO      STRT
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
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
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
     C*  THE ID CODE CANNOT BE EQUAL TO BLANKS
     C*
     C     SCID          IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      *BLANK        XXID
     C                   MOVEL     SCID          XXID
     C     IDKY          CHAIN     PGI01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PGI01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     GIDESC        SCDESC
     C                   MOVEL     GIFN01        SCFN01
     C                   MOVE      GISTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCFN01
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*  THE DESCRIPTION CANNOT BE BLANK
     C*
     C     SCDESC        IFEQ      *BLANKS
     C     *IN09         ANDEQ     '0'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE FIELD LABEL CANNOT BE BLANK
     C*
     C     SCFN01        IFEQ      *BLANKS
     C     *IN09         ANDEQ     '0'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     *IN09         IFEQ      '0'
     C                   MOVE      *BLANK        XXID
     C                   MOVEL     SCID          XXID
     C     IDKY          CHAIN     PGI01                              40
     C                   MOVE      UPCO#         GICO#
     C                   MOVE      *BLANK        GITBID
     C                   MOVEL     SCID          GITBID
     C                   MOVEL     SCDESC        GIDESC
     C                   MOVEL     SCFN01        GIFN01
     C                   MOVE      SCSTAT        GISTAT
     C                   MOVE      Q1USER        GIUSER
     C                   MOVE      Q1PGM         GIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GIDT
     C                   Z-ADD     DSTIME        GITIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PGI01
     C                   ELSE
     C                   WRITE     PGI01
     C                   END
     C*
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCID
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPGI01     E            PGI01E
**  TABLE UA
-- ADD --
- UPDATE -
