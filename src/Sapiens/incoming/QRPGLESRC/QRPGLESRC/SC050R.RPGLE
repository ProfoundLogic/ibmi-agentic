     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SC050R - TABLE BACK-UP MAINTENANCE
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*== 07/29/91 === LJD === PROJ: 90007/D0007 =====================*
     H*   Added processing to allow display of new field 35A          *
     H*                                                               *
     H*   09/24/91 TMN -RELOAD DESCRIPTION IF CUSTOM ID CHANGES ON    *
     H*                 SCR0A & SCR0B.                                *
     H*****************************************************************
     H*  INDICATORS:                                                  *
     H*  ALL SCREENS:                                                 *
     H*   09   INQUIRY MODE                                           *
     H*   60   GENERAL USAGE/CHAIN                                    *
     H*  N71   WORKING WITH RECORD FORMAT 'A'                         *
     H*   71   WORKING WITH RECORD FORMAT 'B'                         *
     H*  SCR01:                                                       *
     H*   20   CUSTOM ID MUST BE VALID                                *
     H*  SCR0A:                                                       *
     H*   21   FIELD NAME MUST BE VALID                               *
     H*   22   USER ASSIGNED ALPHA FIELD MUST BE IN VUA SCRN FIELD    *
     H*   23   USER ASSIGNED DATE FIELD MUST BE IN VUD SCRN FIELD     *
     H*   24   USER ASSIGNED AMT FIELD MUST BE IN VUN SCRN FIELD      *
     H*   25   CUSTOM GROUP ID REQUIRED FOR FIELD NAME SEARCH         *
     H*   26   CANNOT INQUIRE ON AN ADD/WRITE                         *
     H*   30   NO SEL FROM CUSTOM GROUP ID SRCH, POS CUR AT CUST ID   *
     H*   31   SEL FROM FIELD NAME SRCH, POS CUR AT FIELD NAME        *
     H*   32   SEL FROM FIELD SEARCH, POS CUR AT USER ALPHA FIELD     *
     H*  N40   UPDATE TABLE RECORD                                    *
     H*   40   ADD/WRITE TABLE RECORD                                 *
     H*   50   DISPLAY VUA FIELD ON SCR02                             *
     H*   51   DISPLAY VUD FIELD ON SCR02                             *
     H*   52   DISPLAY VUN FIELD ON SCR02                             *
     H*  SCR0B:                                                       *
     H*   21   FIELD NAME MUST BE VALID                               *
     H*   26   CANNOT INQUIRE ON AN ADD/WRITE                         *
     H*   31   SEL FROM FIELD NAME SRCH, POS CUR AT FIELD NAME        *
     H*   32   SEL FROM FIELD SEARCH, POS CUR AT USER ALPHA FIELD     *
     H*  N40   UPDATE TABLE RECORD                                    *
     H*   40   ADD/WRITE TABLE RECORD                                 *
     H*   53   DISPLAY VUB FIELD ON SCR02                             *
     H*  SCR02:                                                       *
     H*   20   DESCRIPTION REQUIRED                                   *
     H*   21   DISPLAY ERR MSG THAT CODE CANNOT BE DELETED            *
     H*****************************************************************
     FSC050F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSTTBL     UF A E           K DISK
     FSTTBL1    UF A E           K DISK
     F                                     RENAME(PTB01:PTB0B)
     FSMCGL     IF   E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D MSG             S             75    DIM(1) CTDATA PERRCD(1)              ERROR MESSAGE
     D FL              S              4    DIM(20) CTDATA PERRCD(20)            FIELD NAMES (RFMT A)
     D FB              S              4    DIM(6) CTDATA PERRCD(6)              FIELD NAMES (RFMT B)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXFLD                  1      4
     D  XXLT1                  1      1
     D  XXLT2                  2      2
     D  XXTST                  3      4
     D  XXNUM                  3      4  0
     D*
     D                 DS
     D  DA                     1    200
     D                                     DIM(20)                              FIELD HEADINGS (RFMT
     D  CGDA01                 1     10
     D  CGDA02                11     20
     D  CGDA03                21     30
     D  CGDA04                31     40
     D  CGDA05                41     50
     D  CGDA06                51     60
     D  CGDA07                61     70
     D  CGDA08                71     80
     D  CGDA09                81     90
     D  CGDA10                91    100
     D  CGDD01               101    110
     D  CGDD02               111    120
     D  CGDD03               121    130
     D  CGDD04               131    140
     D  CGDD05               141    150
     D  CGDN01               151    160
     D  CGDN02               161    170
     D  CGDN03               171    180
     D  CGDN04               181    190
     D  CGDN05               191    200
     D*
     D                 DS
     D  DB                     1     60
     D                                     DIM(6)                               FIELD HEADINGS (RFMT
     D  CGDB01                 1     10
     D  CGDB02                11     20
     D  CGDB03                21     30
     D  CGDB04                31     40
     D  CGDB05                41     50
     D  CGDB06                51     60
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
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
     C*   KEY LIST FOR CUSTOM GROUP ID MASTER FILE
     C*
     C     CGKEY         KLIST
     C                   KFLD                    XXCSID
     C*
     C*   KEY LIST FOR THE TABLE BACK-UP FILE - FOR RECORD FORMAT 'A'
     C     TBKEYA        KLIST
     C                   KFLD                    XXCSID
     C                   KFLD                    XXFLD
     C                   KFLD                    XXVUA
     C                   KFLD                    XXVUD
     C                   KFLD                    XXVUN
     C*
     C*   KEY LIST FOR THE TABLE BACK-UP FILE - FOR RECORD FORMAT 'B'
     C     TBKEYB        KLIST
     C                   KFLD                    XXCSID
     C                   KFLD                    XXFLD
     C                   KFLD                    XXVUB
     C*
     C*** INITIALIZE WORK FIELDS
     C     *LIKE         DEFINE    TBCSID        XXCSID
     C     *LIKE         DEFINE    TBVUA         XXVUA
     C     *LIKE         DEFINE    TBVUD         XXVUD
     C     *LIKE         DEFINE    TBVUN         XXVUN
     C     *LIKE         DEFINE    TBVUA         QQVUA
     C     *LIKE         DEFINE    TBVUB         XXVUB
     C     *LIKE         DEFINE    TBVUB         QQVUB
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCMSG
     C                   MOVE      *BLANKS       SCRFMT
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR0A'
     C                   EXFMT     SCR0A
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR0B'
     C                   EXFMT     SCR0B
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C********************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C******************************************
     C*   F3 = EXIT                            *
     C******************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F5                                 *
     C****************************************
     C     *INKE         IFEQ      '1'
     C****************************************
     C*   SCR01 = CUSTOM GROUP ID SEARCH     *
     C****************************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'SC520R'
     C                   PARM                    SCCSID
     C                   PARM                    SCRFMT
     C                   PARM      *BLANKS       QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCCSID        OREQ      *BLANK
     C                   GOTO      STRT
     C                   END                                                    ENDEQ 'B'
     C     QQRTC         IFEQ      '*'
     C                   GOTO      SCR01T
     C                   END                                                    ENDEQ '*'
     C                   END                                                    END EQ 'SCR01'
     C*
     C****************************************
     C*   SCR0A = CUSTOM GROUP ID SEARCH     *
     C****************************************
     C     Q2RECF        IFEQ      'SCR0A'
     C                   CALL      'SC525R'
     C                   PARM                    SCCSID
     C                   PARM                    SCFLD
     C                   PARM      *BLANKS       QQRTC
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR0A'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCFLD         OREQ      *BLANK
     C                   MOVE      '1'           *IN31                          POS CUR SCFLD
     C                   END                                                    ENDEQ 'B'
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN32                          POS CUR SCVUA
     C                   END                                                    ENDEQ '*'
     C                   END                                                    END EQ 'SCR0A'
     C*
     C****************************************
     C*   SCR0B = CUSTOM GROUP ID SEARCH     *
     C****************************************
     C     Q2RECF        IFEQ      'SCR0B'
     C                   CALL      'SC525R'
     C                   PARM                    SCCSID
     C                   PARM                    SCFLD
     C                   PARM      *BLANKS       QQRTC
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR0B'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCFLD         OREQ      *BLANK
     C                   MOVE      '1'           *IN31                          POS CUR SCFLD
     C                   END                                                    ENDEQ 'B'
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN32                          POS CUR SCVUB
     C                   END                                                    ENDEQ '*'
     C                   END                                                    END EQ 'SCR0B'
     C***
     C                   GOTO      STRT
     C                   END                                                    END *INKE
     C******************************************
     C*   F7 = TABLE VALUE SEARCH              *
     C******************************************
     C     *INKG         IFEQ      '1'
     C************
     C*   SCR0A  *
     C************
     C     Q2RECF        IFEQ      'SCR0A'
     C                   MOVE      *BLANKS       QQKEY             6
     C                   MOVEL     SCCSID        QQKEY
     C                   MOVE      SCFLD         QQKEY
     C                   CALL      'SC550R'
     C                   PARM                    QQKEY
     C                   PARM                    QQVUA
     C                   PARM                    QQVUD             6
     C                   PARM                    QQVUN             9
     C                   PARM                    QQVUB
     C                   PARM                    QQRTC
     C*
     C                   MOVEL     QQKEY         SCCSID
     C                   MOVE      QQKEY         SCFLD
     C                   MOVE      QQVUA         SCVUA
     C                   MOVE      QQVUD         SCVUD
     C                   MOVE      QQVUN         SCVUN
     C                   MOVE      QQVUB         SCVUB
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR0A'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCFLD         OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR0AT
     C                   END                                                    END EQ 'SCR0A'
     C************
     C*   SCR0B  *
     C************
     C     Q2RECF        IFEQ      'SCR0B'
     C                   MOVE      *BLANKS       QQKEY             6
     C                   MOVEL     SCCSID        QQKEY
     C                   MOVE      SCFLD         QQKEY
     C                   CALL      'SC550R'
     C                   PARM                    QQKEY
     C                   PARM                    QQVUA
     C                   PARM                    QQVUD             6
     C                   PARM                    QQVUN             9
     C                   PARM                    QQVUB
     C                   PARM                    QQRTC
     C*
     C                   MOVEL     QQKEY         SCCSID
     C                   MOVE      QQKEY         SCFLD
     C                   MOVE      QQVUA         SCVUA
     C                   MOVE      QQVUD         SCVUD
     C                   MOVE      QQVUN         SCVUN
     C                   MOVE      QQVUB         SCVUB
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR0B'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCFLD         OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR0BT
     C                   END                                                    END EQ 'SCR0B'
     C*
     C                   GOTO      STRT
     C                   END                                                    END *INKG
     C******************************************
     C*   F12 = PRIOR SCREEN (CANCEL)          *
     C******************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR0A'
     C     Q2RECF        OREQ      'SCR0B'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     HLRECF        Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C******************************************
     C*   *IN10 = PROCESS HELP REQUESTS        *
     C******************************************
     C*** *IN10 IS NO LONGER USED FOR HELP(DDS HLPDOC IS UTILIZED)
     C  N10
     CAN 10*IN10         IFEQ      '1'
     C  N10
     CAN 10              MOVE      '1'           *IN10
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
     C                   END                                                    END *IN10
     C******************************************
     C*   F23 = DELETE RECORD                  *
     C******************************************
     C     *INKX         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     TBKEYA        DELETE    PTB01                              21
     C     *IN21         IFEQ      '1'
     C                   WRITE     SCR99
     C                   MOVEL     MSG(1)        SCMSG                          NO RCD EXISTS
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   ELSE                                                   ELSE 1 *IN21
     C                   MOVE      *BLANKS       SCCSID
     C                   MOVE      *BLANKS       SCFLD
     C                   MOVE      *BLANKS       SCVUA
     C                   MOVE      *ZEROS        SCVUD
     C                   MOVE      *ZEROS        SCVUN
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END                                                    END 1 *IN21
     C                   GOTO      STRT
     C                   END                                                    END *INKX
     C*
     C                   END                                                    END CMDKEYS
     C**********************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR0A'       SCR0AT
     C     Q2RECF        CABEQ     'SCR0B'       SCR0BT
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C* =====================================================================
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  CUSTOM ID ENTERED MUST BE VALID
     C*
     C                   MOVE      *BLANKS       XXCSID
     C                   MOVEL     SCCSID        XXCSID
     C     CGKEY         CHAIN     PCG01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      CGRFMT        SCRFMT
     C                   MOVEL     CGDESC        SCCGDS
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C*  PER THE RECORD FORMAT, DISPLAY THE PROPER NEXT SCREEN
     C     SCRFMT        IFEQ      'B'
     C                   MOVEL     'SCR0B'       Q2RECF
     C                   MOVE      '1'           *IN71
     C                   MOVE      *BLANKS       SCFLD
     C                   MOVE      *BLANKS       SCVUB
     C                   ELSE
     C                   MOVEL     'SCR0A'       Q2RECF
     C                   MOVE      '0'           *IN71
     C                   MOVE      *BLANKS       SCFLD
     C                   MOVE      *BLANKS       SCVUA
     C                   MOVE      *ZEROS        SCVUD
     C                   MOVE      *ZEROS        SCVUN
     C                   END
     C*
     C                   GOTO      STRT
     C*
     C* =====================================================================
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR0AT        TAG
     C*
     C                   MOVE      SCFLD         XXFLD
     C                   MOVE      SCVUA         XXVUA
     C                   Z-ADD     SCVUD         XXVUD
     C                   Z-ADD     SCVUN         XXVUN
     C*
     C*   GET CUSTOM ID DESCRIPTION IF CHANGES
     C*
     C     SCCSID        IFNE      XXCSID
     C                   MOVE      *BLANKS       XXCSID
     C                   MOVEL     SCCSID        XXCSID
     C     CGKEY         CHAIN     PCG01                              60
     C                   MOVE      *BLANKS       SCCGDS
     C                   MOVEL     CGDESC        SCCGDS
     C                   END
     C*
     C                   Z-ADD     1             HA                5 0
     C*  FIELD NAME MUST BE VALID
     C     SCFLD         LOOKUP    FL(HA)                                 60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  AT LEAST ONE VALUE MUST BE SPECIFIED
     C*
     C     XXVUA         IFEQ      *BLANKS
     C     XXVUD         ANDEQ     *ZEROS
     C     XXVUN         ANDEQ     *ZEROS
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF THE SPECIFIED FIELD IS ALPHA THEN A VALUE MUST BE PLACED
     C*  IN THE USER FIELD VALUE FIELD AND THE OTHER TWO MUST BE LEFT
     C*  BLANK
     C*
     C     XXLT2         IFEQ      'A'
     C     XXVUA         IFEQ      *BLANKS
     C     XXVUD         ORNE      *ZEROS
     C     XXVUN         ORNE      *ZEROS
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  IF THE SPECIFIED FIELD IS DATE/NUMERIC THEN A VALUE MUST BE PLACED
     C*  IN THE USER DATE VALUE FIELD AND THE OTHER TWO MUST BE LEFT
     C*  BLANK
     C*
     C     XXLT2         IFEQ      'D'
     C     XXVUA         IFNE      *BLANKS
     C     XXVUD         OREQ      *ZEROS
     C     XXVUN         ORNE      *ZEROS
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  IF THE SPECIFIED FIELD IS AMOUNT/NUMERIC THEN A VALUE MUST BE PLACED
     C*  IN THE USER AMOUNT VALUE FIELD AND THE OTHER TWO MUST BE LEFT
     C*  BLANK
     C*
     C*
     C     XXLT2         IFEQ      'N'
     C     XXVUA         IFNE      *BLANKS
     C     XXVUD         ORNE      *ZEROS
     C     XXVUN         OREQ      *ZEROS
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCCSID        XXCSID
     C                   MOVE      SCFLD         XXFLD
     C                   MOVE      SCVUA         XXVUA
     C                   MOVE      SCVUD         XXVUD
     C                   MOVE      SCVUN         XXVUN
     C     TBKEYA        CHAIN     PTB01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PTB01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     DA(HA)        SCHEAD
     C                   MOVE      TBDESC        SCTBDS
     C                   MOVE      *BLANKS       SCVUB
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVEL     DA(HA)        SCHEAD
     C                   MOVE      *BLANKS       SCTBDS
     C                   MOVE      *BLANKS       SCVUB
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C*  ONLY DISPLAY PROPER FIELD
     C*
     C                   MOVEA     '0000'        *IN(50)
     C     XXVUA         IFNE      *BLANKS
     C                   MOVE      '1'           *IN50
     C                   END
     C*
     C     XXVUD         IFNE      *ZEROS
     C                   MOVE      '1'           *IN51
     C                   END
     C*
     C     XXVUN         IFNE      *ZEROS
     C                   MOVE      '1'           *IN52
     C                   END
     C*
     C*** HOLD ONTO THIS SCREEN NAME, MAY BE USED IN F12 FROM SCR02
     C                   MOVE      *BLANKS       HLRECF           10
     C                   MOVEL     'SCR0A'       HLRECF
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVE      *BLANKS       SCMSG
     C                   GOTO      STRT
     C*
     C*
     C* =====================================================================
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR0BT        TAG
     C*
     C                   MOVE      SCFLD         XXFLD
     C                   MOVE      SCVUB         XXVUB
     C*
     C*   GET CUSTOM ID DESCRIPTION IF CHANGES
     C*
     C     SCCSID        IFNE      XXCSID
     C                   MOVE      *BLANKS       XXCSID
     C                   MOVEL     SCCSID        XXCSID
     C     CGKEY         CHAIN     PCG01                              60
     C                   MOVE      *BLANKS       SCCGDS
     C                   MOVEL     CGDESC        SCCGDS
     C                   END
     C*
     C                   Z-ADD     1             HB                5 0
     C*  FIELD NAME MUST BE VALID
     C     SCFLD         LOOKUP    FB(HB)                                 60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  AT LEAST ONE VALUE MUST BE SPECIFIED
     C*
     C     XXVUB         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCCSID        XXCSID
     C                   MOVE      SCFLD         XXFLD
     C                   MOVE      SCVUB         XXVUB
     C     TBKEYB        CHAIN     PTB0B                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PTB0BE
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     DB(HB)        SCHEAD
     C                   MOVE      TBDESC        SCTBDS
     C                   MOVE      *BLANKS       SCVUA
     C                   MOVE      *ZEROS        SCVUD
     C                   MOVE      *ZEROS        SCVUN
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVEL     DB(HB)        SCHEAD
     C                   MOVE      *BLANKS       SCTBDS
     C                   MOVE      *BLANKS       SCVUA
     C                   MOVE      *ZEROS        SCVUD
     C                   MOVE      *ZEROS        SCVUN
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C*  ONLY DISPLAY PROPER FIELD
     C*
     C                   MOVEA     '0000'        *IN(50)
     C                   MOVE      '1'           *IN53
     C*
     C*** HOLD ONTO THIS SCREEN NAME, MAY BE USED IN F12 FROM SCR02
     C                   MOVE      *BLANKS       HLRECF
     C                   MOVEL     'SCR0B'       HLRECF
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVE      *BLANKS       SCMSG
     C                   GOTO      STRT
     C*
     C* =====================================================================
     C     SCR02T        TAG
     C*
     C*  DESCRIPTION CANNOT BE EQUAL TO BLANKS
     C*
     C     SCTBDS        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C  N71TBKEYA        CHAIN     PTB01                              40
     C   71TBKEYB        CHAIN     PTB0B                              40
     C                   MOVE      SCCSID        TBCSID
     C                   MOVE      SCFLD         TBFLD
     C                   MOVE      SCVUA         TBVUA
     C                   Z-ADD     SCVUD         TBVUD
     C                   Z-ADD     SCVUN         TBVUN
     C                   MOVEL     SCVUB         TBVUB
     C                   MOVE      SCTBDS        TBDESC
     C                   MOVE      *BLANK        TBSTAT
     C                   MOVE      Q1USER        TBUSER
     C                   MOVE      Q1PGM         TBPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TBDT
     C*
     C                   Z-ADD     DSTIME        TBTIME
     C*
     C     *IN40         IFEQ      '0'
     C  N71              UPDATE    PTB01
     C   71              UPDATE    PTB0B
     C                   ELSE
     C  N71              WRITE     PTB01
     C   71              WRITE     PTB0B
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCCSID
     C                   MOVE      *BLANKS       SCFLD
     C                   MOVE      *BLANKS       SCVUA
     C                   MOVE      *ZEROS        SCVUD
     C                   MOVE      *ZEROS        SCVUN
     C                   MOVE      *BLANKS       SCVUB
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPTB01     E            PTB01E
     OPTB0B     E            PTB0BE
**  TABLE UA
-- ADD --
- UPDATE -
**  MSG - SCREEN MESSAGE
THIS RECORD DOES NOT EXIST IN THE FILE.  IT CANNOT BE DELETED AT THIS TIME.
** FL = FIELD NAMES, RECORD FORMAT A
UA01UA02UA03UA04UA05UA06UA07UA08UA09UA10UD01UD02UD03UD04UD05UN01UN02UN03UN04UN05
** FB = FIELD NAMES, RECORD FORMAT B
UB01UB02UB03UB04UB05UB06
