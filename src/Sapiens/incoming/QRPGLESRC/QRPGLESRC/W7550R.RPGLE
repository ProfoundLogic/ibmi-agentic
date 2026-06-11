     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7550R - ENTITY/LOCATION EMPLOYER LABEL; PROMPT PGM
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  THIS PROGRAM PROMPTS THE USER TO ENTER SELECTIONS AND SUBMIT *
     H*  A REQUEST TO PRINT EMPLOYER LABELS FOR ENTITIES/LOCATIONS.   *
     H*                                                               *
     H*  CALLED BY: W0075R1 - ENTITY/LOCATION MAINTENANCE             *
     H*                                                               *
     H*  CALLS TO:  W7550C - SUBMITTER CL                             *
     H*             W0076R - ENTITY/LOCATION NUMBERIC SEARCH          *
     H*             W0077R - ENTITY/LOCATION ALPHA SEARCH             *
     H*             W0078R - LOCATION PHYSICAL ADDRESS SEARCH         *
     H*                                                               *
     H*  PROGRAMMER: C:24                                             *
     H*  CREATED:    11/01/94          PROJECT: WC378                 *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  GLOBAL                                                       *
     H*   91  PGM IS CALLED FROM ENTITY/LOCATION MAINTENANCE PGM.     *
     H*       DON'T ALLOW COMMAND-13; ALLOW CMD-2.                    *
     H* N91   PGM IS CALLED FROM MENU. ALLOW COMMAND 13. NO CMD-2.    *
     H*  60   GENERAL READ/CHAIN                                      *
     H*  94   PRINT MESSAGE THAT LABELS WILL BE PRODUCED FOR ALL      *
     H*       ENTITIES/LOCATIONS AND ALL EMPLOYERS                    *
     H*  95   DETERMINE IF ENTER IS PRESSED WITH MESSAGE ON SCREEN    *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  20   INVALID EMPLOYER/DIV                                    *
     H*  21   E/L # AND EMP/DIV ARE MUTUALLY INCLUSIVE                *
     H*  22   INVALID ENTITY/LOCATION #                               *
     H*  23   INVALID LABEL TYPE                                      *
     H*  24   LABELS CANNOT BE REQUESTED FOR CANCELLED EMPLOYERS      *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/31/02 C:61  PROJECT WC1070                                 *
     H*                Remove reference to w0077r.                    *
     H*                                                               *
     H*****************************************************************
     FW7550F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMUPL     IF   E           K DISK
     FWMEMP     IF   E             DISK
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
     FWMALL     IF   E           K DISK

     D MSG             S             41    DIM(2) CTDATA PERRCD(1)              MSG FOR ALL E/L, EMP
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        Effect Date
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        Cancel Date
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  KEY LIST FOR THE ENTITY/LOCATION MASTER FILE
     C*
     C     ALKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXELID
     C*
     C     *LIKE         DEFINE    ALEMP#        XXEMP#
     C     *LIKE         DEFINE    ALDIV         XXDIV
     C     *LIKE         DEFINE    ALELID        XXELID
     C     *LIKE         DEFINE    ALDT          XXDATE
     C*
     C*   PARM LIST FOR LOCATION PHYSICAL ADDRESS SEARCH (W0078R)
     C*
     C     PL0078        PLIST
     C                   PARM      *BLANK        QQPRDS           40
     C                   PARM                    QQCALL
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQELID
     C                   PARM                    QQRTCD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQELID           10
     C                   PARM                    QQRTCD            1
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C*
     C*  NOTE:  @@CO# & @@FND MUST BE PASSED IF QQRTCD IS PASSED AS 'Z'
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
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   MOVE      QQEMP#        SCEMP#
     C                   MOVE      QQDIV         SCDIV
     C                   MOVE      QQELID        SCELID
     C*
     C     QQRTCD        COMP      '1'                                    91
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C     QQRTCD        IFEQ      'Z'
     C*
     C     *IN23         DOUEQ     *OFF
     C*
     C                   Z-ADD     @@CO#         UPCO#
     C                   Z-ADD     @@FND         UPFND
     C*
     C                   EXFMT     LBLWDW
     C*
     C*  LABEL TYPE MUST BE VALID IF ENTERED
     C*
     C     SCLBTY        IFNE      *BLANK                                       ALL TYPES
     C     SCLBTY        ANDNE     'E'                                          EMPLOYER TYPE LABEL
     C     SCLBTY        ANDNE     'L'                                          LOGO LABEL
     C     SCLBTY        ANDNE     'P'                                          POSTER LABEL
     C                   MOVE      *ON           *IN23
     C                   ENDIF
     C*
     C                   ENDDO                                                  *IN23=*OFF
     C*
     C     *INKL         IFEQ      *OFF
     C                   GOTO      SCR01T
     C                   ELSE
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C                   ENDIF                                                  QQRTCD=Z
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     UPFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
     C*  LOAD THE RECORD FORMAT INFO FOR FIRST TIME
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
     C*  COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C*  COMMAND 05 = EMPLOYER NUMERIC SEARCH
     C*
     C     *INKE         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      2             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZERO
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   ELSE
     C*
     C     @@RRN         CHAIN     WMEMP                              60
     C*
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  COMMAND 07 = EMPLOYER ALPHA SEARCH
     C*
     C     *INKG         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZERO
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   ELSE
     C*
     C     @@RRN         CHAIN     WMEMP                              60
     C*
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
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
     C*  COMMAND 11 = ENTITY/LOCATION SEARCH (NUMERIC)
     C*
     C     *INKK         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C                   MOVE      *BLANKS       QQCALL            1
     C                   MOVE      SCEMP#        QQEMP#
     C                   MOVE      SCDIV         QQDIV
     C                   MOVE      *BLANKS       QQTYPE            1
     C                   MOVE      *BLANKS       QQELID           10
     C                   CALL      'W0076R'
     C                   PARM      *BLANK        QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQTYPE
     C                   PARM                    QQELID
     C                   PARM                    QQRTCD            1
     C                   MOVEL     QQEMP#        SCEMP#
     C                   MOVEL     QQDIV         SCDIV
     C                   MOVEL     QQELID        SCELID
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTCD        IFEQ      'B'
     C     SCELID        OREQ      *BLANKS
     C                   MOVE      '1'           *IN41                          PC @ EMP#
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN43
     C                   GOTO      STRT
     C                   END
     C                   END
      ***** BEG DEL ******* 01/31/02 **********************************
     C*********
     C***COMMAND 15 = ENTITY/LOCATION SEARCH (ALPHA)
     C*********
     C*********  *INKP     IFEQ '1'
     C*********  Q2RECF    ANDEQ'SCR01'
     C*********            MOVE *BLANKS   QQCALL
     C*********            MOVE SCEMP#    QQEMP#
     C*********            MOVE SCDIV     QQDIV
     C*********            MOVE *BLANKS   QQTYPE
     C*********            MOVE *BLANKS   QQELID
     C*********            CALL 'W0077R'
     C*********            PARM *BLANK    QQPRDS
     C*********            PARM           QQCALL
     C*********            PARM           QQEMP#
     C*********            PARM           QQDIV
     C*********            PARM           QQTYPE
     C*********            PARM           QQELID
     C*********            PARM           QQRTCD  1
     C*********            MOVELQQEMP#    SCEMP#
     C*********            MOVELQQDIV     SCDIV
     C*********            MOVELQQELID    SCELID
     C*********  QQRTCD    CABEQ'A'       TERM
     C*********
     C*********            WRITESCR99
     C*********            MOVE *BLANK    Q2RECF
     C*********            MOVEL'SCR01'   Q2RECF
     C*********  QQRTCD    IFEQ 'B'
     C*********  SCELID    OREQ *BLANKS
     C*********            MOVE '1'       *IN41            PC @ EMP#
     C*********            GOTO STRT
     C*********            END
     C*********
     C*********  QQRTCD    IFEQ '*'
     C*********            MOVE '1'       *IN43
     C*********            GOTO STRT
     C*********            END
     C*********            END
      ***** END DEL ******* 01/31/02 **********************************
     C*
     C*  COMMAND 19 = LOCATION PHYSICAL ADDRESS SEARCH
     C*               (USER MUST ENTER EMPLOYER# TO SEARCH)
     C*
     C     *INKT         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C     SCEMP#        CABEQ     *ZERO         STRT                     20
     C                   MOVE      *BLANK        QQCALL
     C                   MOVE      SCEMP#        QQEMP#
     C                   MOVE      SCDIV         QQDIV
     C                   MOVE      *BLANKS       QQELID
     C                   CALL      'W0078R'      PL0078
     C                   MOVEL     QQELID        SCELID
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTCD        IFEQ      'B'
     C     SCELID        OREQ      *BLANKS
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN43
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   END                                                    IF *IN11
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  EMPLOYER NUMBER MUST BE VALID IF ENTERED
     C*
     C     SCEMP#        IFNE      *ZERO
     C     SCDIV         ORNE      *ZERO
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCDIV         XXDIV
     C                   MOVEA     *ZEROS        ED
     C                   MOVEA     *ZEROS        CD
     C     EMKEY         CHAIN     PEM02                              20
     C     *IN20         CABEQ     '1'           STRT
     C                   END
     C     SCEMP#        IFNE      *ZEROS
     C*
     C*  CHECK TO SEE IF EMPLOYER IS CANCELLED
     C*
     C                   Z-ADD     1             Y                 2 0
     C     *ZERO         LOOKUP    ED(Y)                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   END
     C*
     C     CD(Y)         IFNE      0
     C     CD(Y)         ANDLE     XXDATE
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   MOVEL     SCELID        XXELID
     C*
     C     XXELID        IFNE      *BLANKS
     C     XXELID        ANDNE     *ZEROS
     C     SCEMP#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     SCELID        IFNE      *BLANKS
     C     SCELID        ANDNE     *ZEROS
     C*
     C*  ENTITY/LOCATION ID MUST BE VALID IF ENTERED
     C*
     C     ALKEY         CHAIN     PAL01                              22
     C     *IN22         CABEQ     '1'           STRT
     C                   END
     C*
     C*  LABEL TYPE MUST BE VALID IF ENTERED
     C*
     C     SCLBTY        IFNE      *BLANK
     C     SCLBTY        ANDNE     'E'                                          EMPLOYER TYPE LABEL
     C     SCLBTY        ANDNE     'L'                                          LOGO LABEL
     C     SCLBTY        ANDNE     'P'                                          POSTER LABEL
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  SHOW MESSAGE AND REQUIRE ENTER TO BE PRESSED IF LABELS
     C*  ARE REQUESTED FOR ALL ENTITIES/LOCATIONS, ALL EMPLOYERS
     C*
     C     QQRTCD        IFNE      'Z'
     C*
     C     *IN95         IFEQ      '0'
     C     SCELID        IFEQ      *ZEROS
     C     SCELID        OREQ      *BLANKS
     C     SCEMP#        IFEQ      *ZEROS
     C     SCDIV         ANDEQ     *ZEROS
     C                   MOVE      '1'           *IN94
     C                   MOVE      '1'           *IN95
     C                   MOVE      MSG(1)        SCMSG1
     C                   MOVE      MSG(2)        SCMSG2
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
     C*
     C                   ENDIF                                                  QQRTCD<>Z
     C*
     C*  ALL EDITS PASSED.  CALL CL PGM TO SUBMIT REQUEST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      UPFND         QQFND             3
     C                   MOVE      SCEMP#        QQEMP#
     C                   MOVE      SCDIV         QQDIV
     C                   MOVE      SCELID        QQELID           10
     C                   MOVE      SCLBTY        QQLBTY            1
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   CALL      'W7550C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQELID
     C                   PARM                    QQLBTY
     C                   PARM                    QQOUTQ
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
** MESSAGES
    PRESS ENTER TO PRODUCE E/L LABELS
FOR ALL ENTITIES/LOCATIONS, ALL EMPLOYERS
