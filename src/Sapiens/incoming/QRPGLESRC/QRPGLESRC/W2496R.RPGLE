     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W2496R - CLIENT TABLE CODE MAINTENANCE
     H*****************************************************************
     H* PROGRAM DESCIPTION:  THIS PROGRAM ALLOWS THE USER TO ENTER    *
     H*    AND UPDATE CLIENT TABLE VALUES.                            *
     H*                                                               *
     H* CALLED FROM:  NONE                                            *
     H*                                                               *
     H* CALLS TO:  W2495R, W2497R                                     *
     H*                                                               *
     H* CREATED:  07/24/98            PROJECT: 60ARN                  *
     H* Adapted from W2492R                                           *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/28/02  C:134  PROJECT: WC577                               *
     H*           C:61   UPDATED SO ONLY ACTIVE TABLE ENTRIES         *
     H*                  ARE DISPLAYED WHILE IN SEARCH MODE.          *
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
     H*     *IN30 - POSITION CURSOR                                   *
     H*                                                               *
     H*   SCR02:                                                      *
     H*     *IN20 - VALUE CANNOT BE BLANK                             *
     H*     *IN21 - VALUE CANNOT BE BLANK                             *
     H*                                                               *
     H*****************************************************************
      *
     FW2496F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWTCIL     IF   E           K DISK
     FWTVLL     UF A E           K DISK

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
      *  KEY LIST FOR CLIENT TABLE FILE
      *
     C     VLKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXID
     C                   KFLD                    XXKEY
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
     C     *LIKE         DEFINE    VLKEY         QQKEY
     C     *LIKE         DEFINE    CITBID        XXID
     C     *LIKE         DEFINE    VLKEY         XXKEY
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
     C                   MOVE      *BLANK        SCKEY
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
     C                   EXFMT     SCR01
     C                   MOVE      *OFF          *IN30
     C                   END
      *
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      CITBID        SCID
     C                   MOVEL     CIDESC        SCIDSC
     C                   MOVEL     CIFN01        SCKDSC
     C     SCKDSC        CAT       ':':0         SCKDSC
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
      *
     C                   MOVE      *BLANKS       QQRTC
      *
      ***        If the ID was left blank, display the list of ID's.
      *
     C     SCID          IFNE      *BLANKS
     C                   MOVE      SCID          XXID
     C     IDKY          CHAIN     PCI01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           *IN20
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCID          IFEQ      *BLANKS
     C*** BEGIN ADD ************06/28/02********************************
     C     QQCALL        IFEQ      ' '
     C                   MOVE      'X'           QQCALL
     C                   ENDIF
     C*** END ADD **************06/28/02*************************************
     C                   MOVE      SCID          QQID
     C                   MOVE      *BLANKS       QQRTC
     C                   CALL      'W2495R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQID
     C                   PARM                    QQRTC             1
     C                   MOVEL     QQID          SCID
      *                                                                *
     C*** BEGIN ADD ************06/28/02********************************
     C     QQCALL        IFEQ      'X'
     C                   MOVE      ' '           QQCALL
     C                   ENDIF
     C*** END ADD **************06/28/02*************************************
      *
     C     QQRTC         CABEQ     'A'           TERM
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
     C     QQRTC         IFEQ      'B'
     C     QQID          OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
      *
     C                   ENDIF
      *
     C     QQRTC         IFEQ      '*'
     C                   MOVEL     SCID          XXID
     C     IDKY          CHAIN(N)  PVL01                              60
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
     C*** BEGIN ADD ************06/28/02********************************
     C     QQCALL        IFEQ      ' '
     C                   MOVE      'X'           QQCALL
     C                   ENDIF
     C*** END ADD **************06/28/02*************************************
      *
     C                   MOVE      *ON           *IN30
     C                   MOVE      SCID          QQID
     C                   MOVE      SCKEY         QQKEY
     C                   MOVE      *BLANKS       QQRTC
     C                   CALL      'W2497R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     QQKEY         SCKEY
      *
     C*** BEGIN ADD ************06/28/02********************************
     C     QQCALL        IFEQ      'X'
     C                   MOVE      ' '           QQCALL
     C                   ENDIF
     C*** END ADD **************06/28/02*************************************
     C                   ENDIF
      *
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
     C                   ENDIF
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
      *  THE ID CODE MUST BE VALID
      *
     C                   MOVE      SCID          XXID
     C     IDKY          CHAIN     PCI01                              60
     C     *IN60         IFEQ      *ON
     C     SCID          OREQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  THE KEY CODE CANNOT BE BLANK
      *
     C     SCKEY         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
      *
      *  GET THE REQUESTED RECORD, LOAD INFORMATION
      *
     C                   MOVE      *BLANK        XXID
     C                   MOVEL     SCID          XXID
     C                   MOVEL     SCKEY         XXKEY
     C     VLKY          CHAIN     PVL01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PVL01E
      *
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     VLDESC        SCDESC
     C                   MOVE      VLSTAT        SCSTAT
     C                   MOVE      VLFG01        SCFL01
     C                   MOVE      VLFG02        SCFL02
     C                   MOVE      VLFG03        SCFL03
     C                   MOVE      VLFG04        SCFL04
     C                   MOVE      VLFG05        SCFL05
     C                   MOVE      VLFG06        SCFL06
     C                   MOVE      VLFG07        SCFL07
     C                   MOVE      VLFG08        SCFL08
     C                   MOVE      VLFG09        SCFL09
     C                   MOVE      VLFG10        SCFL10
     C                   MOVE      VLCF01        SCCF01
     C                   MOVE      VLCF02        SCCF02
     C                   MOVE      VLCF03        SCCF03
     C                   MOVE      VLCF04        SCCF04
     C                   MOVE      VLCF05        SCCF05
     C                   MOVE      VLN501        SCN501
     C                   MOVE      VLN502        SCN502
     C                   MOVE      VLN503        SCN503
     C                   MOVE      VLN504        SCN504
     C                   MOVE      VLN505        SCN505
     C                   MOVE      VLN901        SCN901
     C                   MOVE      VLN902        SCN902
     C                   MOVE      VLN903        SCN903
     C                   MOVE      VLN904        SCN904
     C                   MOVE      VLN905        SCN905
     C                   MOVE      VLNB01        SCNB01
     C                   MOVE      VLNB02        SCNB02
     C                   MOVE      VLNB03        SCNB03
     C                   MOVE      VLNB04        SCNB04
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
      *  ALL EDITS HAVE BEEN PASSED
      *
     C     *IN09         IFEQ      '0'
     C                   MOVE      *BLANK        XXID
     C                   MOVEL     SCID          XXID
     C                   MOVEL     SCKEY         XXKEY
     C     VLKY          CHAIN     PVL01                              40
     C                   MOVE      UPCO#         VLCO#
     C                   MOVE      SCID          VLTBID
     C                   MOVE      SCKEY         VLKEY
     C                   MOVEL     SCDESC        VLDESC
     C                   MOVE      SCSTAT        VLSTAT
     C                   MOVE      Q1USER        VLUSER
     C                   MOVE      Q1PGM         VLPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         VLDT
     C                   Z-ADD     DSTIME        VLTIME
     C                   MOVE      SCFL01        VLFG01
     C                   MOVE      SCFL02        VLFG02
     C                   MOVE      SCFL03        VLFG03
     C                   MOVE      SCFL04        VLFG04
     C                   MOVE      SCFL05        VLFG05
     C                   MOVE      SCFL06        VLFG06
     C                   MOVE      SCFL07        VLFG07
     C                   MOVE      SCFL08        VLFG08
     C                   MOVE      SCFL09        VLFG09
     C                   MOVE      SCFL10        VLFG10
     C                   MOVE      SCCF01        VLCF01
     C                   MOVE      SCCF02        VLCF02
     C                   MOVE      SCCF03        VLCF03
     C                   MOVE      SCCF04        VLCF04
     C                   MOVE      SCCF05        VLCF05
     C                   MOVE      SCN501        VLN501
     C                   MOVE      SCN502        VLN502
     C                   MOVE      SCN503        VLN503
     C                   MOVE      SCN504        VLN504
     C                   MOVE      SCN505        VLN505
     C                   MOVE      SCN901        VLN901
     C                   MOVE      SCN902        VLN902
     C                   MOVE      SCN903        VLN903
     C                   MOVE      SCN904        VLN904
     C                   MOVE      SCN905        VLN905
     C                   MOVE      SCNB01        VLNB01
     C                   MOVE      SCNB02        VLNB02
     C                   MOVE      SCNB03        VLNB03
     C                   MOVE      SCNB04        VLNB04
      *
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PVL01
     C                   ELSE
      *
     C                   MOVE      Q1USER        VLIUSR
     C                   MOVE      Q1PGM         VLIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         VLIDT
     C                   Z-ADD     DSTIME        VLITM
      *
     C                   WRITE     PVL01
      *
     C                   END
      *
     C                   END
      *
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *ON           *IN30
     C                   MOVE      *BLANK        SCKEY
     C                   GOTO      STRT
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPVL01     E            PVL01E
**  TABLE UA
-- ADD --
- UPDATE -
