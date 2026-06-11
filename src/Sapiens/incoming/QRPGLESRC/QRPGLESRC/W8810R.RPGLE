     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W8810R - PROMPT FOR AGGREGATE LOSS REPORTS
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 10/13/15 C:242 PHW37852
     H*                 Removed Compiler director if defined(web)
     H*
     H*  12/01/99 C:118 PROJECT: WC577                                *
     H*                 Made V3 changes to display file.              *
     H*                                                               *
     H*  01/28/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  10/31/97  C:62 PROJECT: WC767                                *
     H*                 IF PRIOR RUN DATE IS ZERO USE 1ST DAY OF RUN  *
     H*                 MONTH AND YEAR.                               *
     H*                                                               *
     H*  04/03/92  LPK  PROJECT: MD027                                *
     H*                CHANGED CODE TO USE EMP/DIV ON PROMPT SCREEN.  *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     F*****************************************************************
     F*** BEGIN CHG *** 10/13/15 **************************************
     F*****W8810F    CF   E             WORKSTN
      *****/If Defined (WEB)
     F*****                                Handler('PROFOUNDUI(HANDLER)')
      *****/Endif
     F*****                                     INFDS(FIDS01)
     FW8810F    CF   E             WORKSTN Handler('PROFOUNDUI(HANDLER)')
     F                                     INFDS(FIDS01)
     F*** End   CHG *** 10/13/15 **************************************
     FWMFNL     IF   E           K DISK
     F*** BEGIN ADD *** 01/28/98 **************************************
     F                                     INFDS(DBFDS)
     FWMEMP     IF   E             DISK
     F*** END   ADD *** 01/28/98 **************************************
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMRIL1    IF   E           K DISK

     D*
     D*** BEGIN ADD *** 01/28/98 **************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*** END   ADD *** 01/28/98 **************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS                  INZ
     D  SCPRDT                 1      8  0
     D  SCDAY                  3      4  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*                                                               *
     D*** BEGIN ADD *** 01/28/98 ***********************************************
     D OPTNM           C                   CONST('Aggregate Loss Repor-
     D                                     ts')

     I*** END   ADD *** 01/28/98 ***********************************************
     I*

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  KEY LIST FOR REINSURANCE POLICY MASTER FILE
     C*
     C     RIKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXASFL            1
     C                   KFLD                    XXEFDT
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND             3 0
     C*
     C     *LIKE         DEFINE    RIDT          XXEFDT
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C*
     C     *LIKE         DEFINE    EMEMP#        Q@EMP#
     C     *LIKE         DEFINE    EMDIV         Q@DIV
     C     *LIKE         DEFINE    SCEFDT        Q@EFDT
     C*
     C*** END   ADD *** 01/28/98 **************************************
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
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C                   MOVE      UPFND         SCFND
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRNDT
     C*
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE FUND NAME
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C***** BEGIN ADD **** 12/01/99 ***********************************
     C     *IN60         IFEQ      *OFF
     C***** END   ADD **** 12/01/99 ***********************************
     C*
     C                   MOVE      FNNAME        SCNAME
     C*
     C***** BEGIN ADD **** 12/01/99 ***********************************
     C                   ELSE
     C                   MOVEL     *BLANKS       SCNAME
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 12/01/99 ***********************************
     C*
     C*** BEGIN DEL *** 01/28/98 **************************************
     C**********           TIME           DSDTTM
     C*** END   DEL *** 01/28/98 **************************************
     C*
     C                   MOVE      Q1USER        SCUSER
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C*
     C**********           MOVE QQPRDS    SCPRDS
     C*
     C*  CENTER PROGRAM NAME
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        SCPRDS
     C                   MOVEL(P)  ##DATA        QQPRDS
     C*
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C*
     C*  SCREEN 1
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*  F10 = SEARCH - EMPLOYER NUMBER OR DIVISION
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C**      ERROR CHECKING - NOT ON VALID FIELD FOR SEARCH
     C*
     C     HFLD          COMP      'SCRNDT'                               36
     C     HFLD          COMP      'SCPRDT'                               37
     C     HFLD          COMP      'SCREV'                                38
     C     HFLD          COMP      'SCSDF'                                39
     C     HFLD          COMP      'SCARPT'                               40
     C*
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM      *ZEROS        @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @@RRN         ANDNE     *ZEROS
     C*
     C     @@RRN         CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C                   MOVE      *ON           *IN31                          SCEFDT POSITION
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKL=OFF,@@RRN
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C                   MOVE      *ON           *IN30                          SCEMP# POSITION
     C                   ENDIF                                                  *INKL=ON,@@RRN
     C*
     C                   ENDIF                                                  HFLD=SCEMP#
     C*
     C*  F5 = SEARCH - EFFECTIVE DATE
     C*
     C     HFLD          IFEQ      'SCEFDT'
     C*
     C                   CALL      'W0311R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      SCEMP#        Q@EMP#
     C                   PARM      SCDIV         Q@DIV
     C                   PARM      SCEFDT        Q@EFDT
     C                   PARM      *BLANK        Q@ASFL            1
     C                   PARM      *BLANK        QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      '*'
     C     Q@EFDT        ANDNE     *ZEROS
     C                   Z-ADD     Q@EMP#        SCEMP#
     C                   Z-ADD     Q@DIV         SCDIV
     C                   Z-ADD     Q@EFDT        SCEFDT
     C                   MOVE      *ON           *IN32                          SCRNDT POSITION
     C                   ENDIF                                                  QQRTC=*        F
     C*
     C     QQRTC         IFEQ      'B'
     C     Q@EFDT        OREQ      *ZEROS
     C                   MOVE      *ON           *IN31                          SCEFDT POSITION
     C                   ENDIF                                                  QQRTC=B        F
     C*
     C                   ENDIF                                                  HFLD=SCEFDT
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKJ=ON
     C*
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      UPFND         SCFND
     C*                                                                8
     C*  GET THE FUND NAME
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      FNNAME        SCNAME
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  EFFECTIVE DATE MUST BE VALID
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE EMP/DIV
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   MOVE      'A'           XXASFL
     C     RIKEY         CHAIN     PRI01                              23
     C     *IN23         CABEQ     '1'           STRT
     C*
     C*  RUN DATE MUST BE VALID
     C*
     C                   Z-ADD     SCRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRNDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  PRIOR RUN DATE MUST BE VALID IF ENTERED
     C*
     C     SCPRDT        IFNE      *ZERO
     C*
     C                   Z-ADD     SCPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPRDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  REVISED RUN FLAG MUST BE 'Y' OR 'N'
     C*
     C     SCREV         IFNE      'Y'
     C     SCREV         ANDNE     'N'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*  SUBMIT AGGREGATE EXCESS REPORT FLAG MUST BE 'Y' OR 'N'
     C*
     C     SCARPT        IFNE      'Y'
     C     SCARPT        ANDNE     'N'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*** BEGIN ADD *** 10/31/97 **************************************
     C*
     C*  IF PRIOR RUN DATE IS ZERO LOAD 1ST DAY OF RUN DATE.
     C*
     C     SCPRDT        IFEQ      *ZEROS
     C*
     C                   Z-ADD     SCRNDT        SCPRDT
     C                   Z-ADD     1             SCDAY
     C*
     C                   ENDIF
     C*** END   ADD *** 10/31/97 **************************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         X@CO#
     C                   MOVE      SCFND         X@FND
     C                   MOVE      SCEMP#        X@EMP#
     C                   MOVE      SCDIV         X@DIV
     C                   MOVE      SCEFDT        X@EFDT
     C                   MOVE      SCRNDT        X@RNDT
     C                   MOVE      SCPRDT        X@PRDT
     C                   MOVE      SCSDF         X@SDF
     C                   MOVE      SCREV         X@REV
     C                   MOVE      SCARPT        X@ARPT
     C                   CALL      'W8810C'
     C                   PARM                    X@CO#             3
     C                   PARM                    X@FND             3
     C                   PARM                    X@EMP#            9
     C                   PARM                    X@DIV             5
     C                   PARM                    X@EFDT            8
     C                   PARM                    X@RNDT            8
     C                   PARM                    X@PRDT            8
     C                   PARM                    X@SDF            11
     C                   PARM                    X@REV             1
     C                   PARM                    X@ARPT            1
     C                   PARM                    UPOUTQ
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
