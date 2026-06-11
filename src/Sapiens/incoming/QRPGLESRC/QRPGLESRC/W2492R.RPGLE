     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*                                                               *
     H*                                                               *
     H/TITLE W2490R - SYSTEM TABLE CODE MAINTENANCE                   *
     H*****************************************************************
     H* PROGRAM DESCIPTION:  THIS PROGRAM ALLOWS THE USER TO ENTER    *
     H*    AND UPDATE SYSTEM TABLE VALUES.                            *
     H*                                                               *
     H* CALLED FROM:  NONE                                            *
     H*                                                               *
     H* CALLS TO:  W2491R, W2493R                                     *
     H*                                                               *
     H* CREATED:  08/07/96            PROJECT: WC523                  *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/23/02 C:136 SUPPORT:WA099 31783                            *
     H*                CHANGE TO F10=SEARCH.                          *
     H*                                                               *
     H*                                                               *
     H* 05/04/01  C:134  PROJECT: WC577                               *
     H*                  UPDATED SO ONLY ACTIVE TABLE ENTRIES         *
     H*                  ARE DISPLAYED WHILE IN SEARCH MODE.          *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*     *IN09 - INQUIRY MODE                                      *
     H*     *IN40 - ADD MODE                                          *
     H*     *IN60 - CHAIN (GENERAL USAGE)                             *
     H*   SCR01:                                                      *
     H*     *IN20 - VALUE CANNOT BE BLANK                             *
     H*     *IN21 - RECORD NOT FOUND                                  *
     H*     *IN30 - POSITION CURSOR                                   *
     H*   SCR02:                                                      *
     H*     *IN20 - VALUE CANNOT BE BLANK                             *
     H*     *IN21 - VALUE CANNOT BE BLANK                             *
     H*****************************************************************
     FW2492F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWTGIL     IF   E           K DISK
     FWTGTL     UF A E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
      *
      *** Constants
      *
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
     C*  KEY LIST FOR SYSTEM TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXID
     C                   KFLD                    XXKEY
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
     C     *LIKE         DEFINE    GTKEY         QQKEY
     C     *LIKE         DEFINE    GITBID        XXID
     C     *LIKE         DEFINE    GTKEY         XXKEY
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
     C                   MOVE      *BLANK        SCKEY
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
     C                   EXFMT     SCR01
     C                   MOVE      *OFF          *IN30
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      GITBID        SCID
     C                   MOVEL     GIDESC        SCIDSC
     C                   MOVEL     GIFN01        SCKDSC
     C     SCKDSC        CAT       ':':0         SCKDSC
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
      *** BEG CHG **** 10/23/02 ************************************************
     C********** *INKE     IFEQ '1'
     C     *INKJ         IFEQ      '1'
      *** END CHG **** 10/23/02 ************************************************
     C     Q2RECF        IFEQ      'SCR01'
      *
     C                   MOVE      *BLANKS       QQRTC
      *
      ***        If the ID was left blank, display the list of ID's.
      *
     C     SCID          IFNE      *BLANKS
     C                   MOVE      SCID          XXID
     C     IDKY          CHAIN     PGI01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           *IN20
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCID          IFEQ      *BLANKS
     C*** BEGIN ADD ************05/04/01********************************
     C     QQCALL        IFEQ      ' '
     C                   MOVE      'X'           QQCALL
     C                   ENDIF
     C*** END ADD **************05/04/01*************************************
     C                   MOVE      SCID          QQID
     C                   MOVE      *BLANKS       QQRTC
     C                   CALL      'W2491R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQID
     C                   PARM                    QQRTC             1
     C                   MOVEL     QQID          SCID
     C*** BEGIN ADD ************05/04/01********************************
     C     QQCALL        IFEQ      'X'
     C                   MOVE      ' '           QQCALL
     C                   ENDIF
     C*** END ADD **************05/04/01*************************************
      *
     C     QQRTC         CABEQ     'A'           TERM
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQID          OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
      *
     C                   ENDIF
      *
     C     QQRTC         IFEQ      '*'
     C                   MOVEL     SCID          XXID
     C     IDKY          CHAIN(N)  PGT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
      ***        Display the list of values for the given table.
      *
     C     SCID          IFGT      *BLANKS
     C     *IN60         ANDEQ     *OFF
     C     QQRTC         OREQ      '*'
     C*** BEGIN ADD ************05/04/01********************************
     C     QQCALL        IFEQ      ' '
     C                   MOVE      'X'           QQCALL
     C                   ENDIF
     C*** END ADD **************05/04/01*************************************
      *
     C                   MOVE      *ON           *IN30
     C                   MOVE      SCID          QQID
     C                   MOVE      SCKEY         QQKEY
     C                   MOVE      *BLANKS       QQRTC
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     QQKEY         SCKEY
      *
     C*** BEGIN ADD ************05/04/01********************************
     C     QQCALL        IFEQ      'X'
     C                   MOVE      ' '           QQCALL
     C                   ENDIF
     C*** END ADD **************05/04/01*************************************
     C                   ENDIF
      *
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
     C                   ENDIF
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
     C*  THE ID CODE MUST BE VALID
     C*
     C                   MOVE      SCID          XXID
     C     IDKY          CHAIN     PGI01                              60
     C     *IN60         IFEQ      *ON
     C     SCID          OREQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  THE KEY CODE CANNOT BE BLANK
     C*
     C     SCKEY         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      *BLANK        XXID
     C                   MOVEL     SCID          XXID
     C                   MOVEL     SCKEY         XXKEY
     C     GTKY          CHAIN     PGT01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PGT01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     GTDESC        SCDESC
     C                   MOVE      GTSTAT        SCSTAT
     C                   MOVE      GTFG01        SCFL01
     C                   MOVE      GTFG02        SCFL02
     C                   MOVE      GTFG03        SCFL03
     C                   MOVE      GTFG04        SCFL04
     C                   MOVE      GTFG05        SCFL05
     C                   MOVE      GTFG06        SCFL06
     C                   MOVE      GTFG07        SCFL07
     C                   MOVE      GTFG08        SCFL08
     C                   MOVE      GTFG09        SCFL09
     C                   MOVE      GTFG10        SCFL10
     C                   MOVE      GTCF01        SCCF01
     C                   MOVE      GTCF02        SCCF02
     C                   MOVE      GTCF03        SCCF03
     C                   MOVE      GTCF04        SCCF04
     C                   MOVE      GTCF05        SCCF05
     C                   MOVE      GTN501        SCN501
     C                   MOVE      GTN502        SCN502
     C                   MOVE      GTN503        SCN503
     C                   MOVE      GTN504        SCN504
     C                   MOVE      GTN505        SCN505
     C                   MOVE      GTN901        SCN901
     C                   MOVE      GTN902        SCN902
     C                   MOVE      GTN903        SCN903
     C                   MOVE      GTN904        SCN904
     C                   MOVE      GTN905        SCN905
     C                   MOVE      GTNB01        SCNB01
     C                   MOVE      GTNB02        SCNB02
     C                   MOVE      GTNB03        SCNB03
     C                   MOVE      GTNB04        SCNB04
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
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      *BLANKS       SCFL01
     C                   MOVE      *BLANKS       SCFL02
     C                   MOVE      *BLANKS       SCFL03
     C                   MOVE      *BLANKS       SCFL04
     C                   MOVE      *BLANKS       SCFL05
     C                   MOVE      *BLANKS       SCFL06
     C                   MOVE      *BLANKS       SCFL07
     C                   MOVE      *BLANKS       SCFL08
     C                   MOVE      *BLANKS       SCFL09
     C                   MOVE      *BLANKS       SCFL10
     C                   MOVE      *BLANKS       SCCF01
     C                   MOVE      *BLANKS       SCCF02
     C                   MOVE      *BLANKS       SCCF03
     C                   MOVE      *BLANKS       SCCF04
     C                   MOVE      *BLANKS       SCCF05
     C                   MOVE      *ZEROS        SCN501
     C                   MOVE      *ZEROS        SCN502
     C                   MOVE      *ZEROS        SCN503
     C                   MOVE      *ZEROS        SCN504
     C                   MOVE      *ZEROS        SCN505
     C                   MOVE      *ZEROS        SCN901
     C                   MOVE      *ZEROS        SCN902
     C                   MOVE      *ZEROS        SCN903
     C                   MOVE      *ZEROS        SCN904
     C                   MOVE      *ZEROS        SCN905
     C                   MOVE      *ZEROS        SCNB01
     C                   MOVE      *ZEROS        SCNB02
     C                   MOVE      *ZEROS        SCNB03
     C                   MOVE      *ZEROS        SCNB04
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
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     *IN09         IFEQ      '0'
     C                   MOVE      *BLANK        XXID
     C                   MOVEL     SCID          XXID
     C                   MOVEL     SCKEY         XXKEY
     C     GTKY          CHAIN     PGT01                              40
     C                   MOVE      UPCO#         GTCO#
     C                   MOVE      SCID          GTTBID
     C                   MOVE      SCKEY         GTKEY
     C                   MOVEL     SCDESC        GTDESC
     C                   MOVE      SCSTAT        GTSTAT
     C                   MOVE      Q1USER        GTUSER
     C                   MOVE      Q1PGM         GTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GTDT
     C                   Z-ADD     DSTIME        GTTIME
     C                   MOVE      SCFL01        GTFG01
     C                   MOVE      SCFL02        GTFG02
     C                   MOVE      SCFL03        GTFG03
     C                   MOVE      SCFL04        GTFG04
     C                   MOVE      SCFL05        GTFG05
     C                   MOVE      SCFL06        GTFG06
     C                   MOVE      SCFL07        GTFG07
     C                   MOVE      SCFL08        GTFG08
     C                   MOVE      SCFL09        GTFG09
     C                   MOVE      SCFL10        GTFG10
     C                   MOVE      SCCF01        GTCF01
     C                   MOVE      SCCF02        GTCF02
     C                   MOVE      SCCF03        GTCF03
     C                   MOVE      SCCF04        GTCF04
     C                   MOVE      SCCF05        GTCF05
     C                   MOVE      SCN501        GTN501
     C                   MOVE      SCN502        GTN502
     C                   MOVE      SCN503        GTN503
     C                   MOVE      SCN504        GTN504
     C                   MOVE      SCN505        GTN505
     C                   MOVE      SCN901        GTN901
     C                   MOVE      SCN902        GTN902
     C                   MOVE      SCN903        GTN903
     C                   MOVE      SCN904        GTN904
     C                   MOVE      SCN905        GTN905
     C                   MOVE      SCNB01        GTNB01
     C                   MOVE      SCNB02        GTNB02
     C                   MOVE      SCNB03        GTNB03
     C                   MOVE      SCNB04        GTNB04
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PGT01
     C                   ELSE
     C                   WRITE     PGT01
     C                   END
     C*
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *ON           *IN30
     C                   MOVE      *BLANK        SCKEY
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPGT01     E            PGT01E
**  TABLE UA
-- ADD --
- UPDATE -
