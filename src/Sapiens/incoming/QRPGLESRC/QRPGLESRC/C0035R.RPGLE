     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE C0035R - UNDERWRITERS REVIEW - PROMPT PROGRAM
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program is the Underwriters Review - Prompt       **
     H**   CALLED FROM:                                             **
     H**     ______ - ___________________________                   **
     H**   CALLS TO:                                                **
     H**     C0561R - Vehicle Number Search.                        **
     H****************************************************************
     H*
     H****************************************************************
     H** REVISIONS:
     F**  04/07/00 *** C:31 *** V2000 ***
     H**           1. Compile only
     F**
     H**  08/16/99 *** C:107 *** V2000 ***
     H**           1. Compile only
     H**
     H**  11/18/96 *** C:107 *** V2000 ***
     H**           1. Version 2 format changes.
     H****************************************************************
     H*
     H****************************************************************
     H** INDICATOR USAGE:                                           **
     H**   INDICATOR            FUNCTION DESCRIPTION                **
     H**      __       ___________________________                  **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H**   FILES:                                                   **
     H**   ______ (_____): ___________________ (___/___)            **
     H****************************************************************
     FC0035F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCMPML     IF   E           K DISK
     FCMPXL     IF   E           K DISK
     FCMAGL2    IF   E           K DISK
     FCMAIL1    IF   E           K DISK
     FCTABL     IF   E           K DISK

     D AI              S              2    DIM(4)
     D KEY             S              2    DIM(4)
     D CX              S             18    DIM(2) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  SCACMY                 1      4  0
     D  XXMTH                  1      2  0
     D  XXYR                   3      4  0
     D                 DS
     D  FL                     1      4
     D                                     DIM(4)
     D  SCFL01                 1      1
     D  SCFL02                 2      2
     D  SCFL03                 3      3
     D  SCFL04                 4      4
     D                 DS
     D  NM                     1    240
     D                                     DIM(4)
     D  SCNM01                 1     60
     D  SCNM02                61    120
     D  SCNM03               121    180
     D  SCNM04               181    240
     D                 DS
     D  SC                     1     40
     D                                     DIM(20)
     D  PMCC01                 1      2
     D  PMCC02                 3      4
     D  PMCC03                 5      6
     D  PMCC04                 7      8
     D  PMCC05                 9     10
     D  PMCC06                11     12
     D  PMCC07                13     14
     D  PMCC08                15     16
     D  PMCC09                17     18
     D  PMCC10                19     20
     D  PMCC11                21     22
     D  PMCC12                23     24
     D  PMCC13                25     26
     D  PMCC14                27     28
     D  PMCC15                29     30
     D  PMCC16                31     32
     D  PMCC17                33     34
     D  PMCC18                35     36
     D  PMCC19                37     38
     D  PMCC20                39     40
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE KEY LISTS
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  TYPE CODE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  POLICY MASTER FILE & POLICY AUXILLARY FILE
     C*
     C     PMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCAGT#
     C*
     C*  ADDITIONAL INTERESTS FILE
     C*
     C     AIKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#           13
     C                   KFLD                    XXTYPE            1
     C                   KFLD                    XXADI             2
     C*
     C     AIKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    XXTYPE
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQPOL#
     C                   PARM                    QQITEM
     C                   PARM                    QQCLAS
     C                   PARM                    QQLOC#
     C                   PARM                    QQPRM#
     C                   PARM                    QQRTCD
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
     C                   MOVE      'C'           XXTYPE
     C                   MOVE      QQPOL#        SCPOL#
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              50
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
     C                   Z-ADD     0             QQDLDT
     C*
     C*   DISPLAY ALL FORMATS
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
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C                   EXFMT     SCR04
     C                   END
     C*
     C*   PROCESS ALL FUNCTION KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  FUNCTION 01 = END OF JOB
     C*
     C     *INKA         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  FUNCTION 02 = PRIOR SCREEN
     C*
     C     *INKB         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C     Q2RECF        IFEQ      'SCR04'
     C                   MOVEL     'SCR02'       Q2RECF                         GOTO SCR01
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*  FUNCTION 03 = RESTART
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCPOL#
     C                   GOTO      STRT
     C                   END
     C*
     C*  FUNCTION 05 = SEARCH POLICY MASTER - POLICY ORDER - SCREEN 1
     C*
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVE      *BLANK        QQPOL#           13
     C                   MOVE      SCPOL#        QQPOL#
     C                   CALL      'C0530R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQPOL#        SCPOL#
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCPOL#        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C*
     C                   END
     C*
     C*  FUNCTION 05-A/R INQUIRY - SCREEN2
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   MOVE      SCPOL#        QQPOL#
     C                   CALL      'C0180R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C*
     C                   WRITE     SCR99
     C                   MOVE      KEY(1)        SCADI
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      SCR01T
     C                   END
     C*
     C*  FUNCTION 05 = LOCATION / BUILDING SEARCH - SCREEN 3
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C                   MOVE      SCPOL#        QQPOL#
     C                   MOVE      SCLOC#        QQLOC#
     C                   MOVE      SCPRM#        QQPRM#
     C                   CALL      'C0640R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQLOC#
     C                   PARM                    QQPRM#
     C                   PARM                    QQRTCD
     C                   MOVE      QQLOC#        SCLOC#
     C                   MOVE      QQPRM#        SCPRM#
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCLOC#        OREQ      *ZERO
     C                   MOVE      '0'           *IN12
     C                   MOVE      '1'           *IN11
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        CABEQ     '*'           STRT
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN12
     C                   MOVE      '0'           *IN11
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 07 = SEARCH POLICY MASTER BY NAME, SCR01
     C*
     C     *INKG         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      *BLANKS       QQPOL#
     C                   CALL      'C0531R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQPOL#        SCPOL#
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCPOL#        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C*
     C                   END
     C*
     C*  FUNCTION 07 = VEHICLE SEARCH - SCREEN 3
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C                   MOVE      SCPOL#        QQPOL#
     C                   Z-ADD     SCVEH#        QXITM#            5 0
     C                   Z-ADD     0             QQDLDT            8 0
     C                   MOVE      *BLANKS       QQCLAS            6
     C                   CALL      'C0561R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QXITM#
     C                   PARM                    QQDLDT
     C                   PARM                    QQCLAS
     C                   PARM                    QQRTCD
     C                   MOVE      QXITM#        SCVEH#
     C     QXITM#        IFEQ      0
     C                   MOVE      *BLANKS       QQITEM            5
     C                   ELSE
     C                   MOVE      QXITM#        QQITEM
     C                   END
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCVEH#        OREQ      *ZERO
     C                   MOVE      '1'           *IN12
     C                   MOVE      '0'           *IN11
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '0'           *IN12
     C                   MOVE      '1'           *IN11
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR02T
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR02T
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
     C                   CALL      'C0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              50
     C     *IN50         IFEQ      '1'
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
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C     Q2RECF        CABEQ     'SCR04'       SCR04T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*   VALIDATE POLICY NUMBER
     C*
     C     PMKEY         CHAIN     PPM01                              40
     C     *IN40         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ELSE
     C*
     C                   MOVE      PMDBA         SCDBA
     C                   MOVE      PMFIN         SCFIN
     C                   MOVE      PMADR1        SCADR1
     C                   MOVE      PMADR2        SCADR2
     C                   MOVE      PMCTY         SCCTY
     C                   MOVE      PMST          SCST
     C                   MOVEL     PMZIP         SCZIP1
     C                   MOVE      PMZIP         SCZIP2
     C                   MOVE      PMAGCO        SCAGT#
     C     AGKEY         CHAIN     PAG01                              60
     C                   MOVEL     AGNAME        SCNAME
     C                   MOVE      PMPFEE        SCPFEE
     C                   MOVE      PMNOB         SCNOB
     C                   Z-ADD     PMEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     PMCNDT        IFNE      *ZERO
     C                   MOVE      '1'           *IN31
     C                   MOVEL     CX(1)         SCCNEX
     C                   Z-ADD     PMCNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C                   ELSE
     C                   MOVE      '0'           *IN31
     C                   MOVEL     CX(2)         SCCNEX
     C                   Z-ADD     PMEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C                   END
     C*
     C                   MOVEL     PMBLCD        XXKEY
     C     TBKEY         CHAIN     PBC01                              60
     C                   MOVE      BCDESC        SCBCDS
     C*
     C                   END
     C*
     C*   GET POLICY AUXILLARY FILE RECORD OR Z-ADD ZERO TO ANNUAL PREM
     C*
     C     PMKEY         CHAIN     PPX01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         SCPREM
     C                   ELSE
     C                   Z-ADD     PXANPR        SCPREM
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      REPOS
     C*
     C*   SCREEN 2
     C*
     C     SCR02T        TAG
     C*
     C*  INVALID ROLL REQUEST
     C*
     C     XXUP          IFNE      *BLANK
     C     *IN18         ANDEQ     *ON
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C     XXDOWN        IFNE      *BLANK
     C     *IN19         ANDEQ     *ON
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVEL     KEY(2)        XXADI
     C     AIKEY         CHAIN     PAI01                              60
     C     AIKEY2        READE     PAI01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     KEY(1)        XXADI
     C     AIKEY         CHAIN     PAI01                              60
     C                   READP     PAI01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     UPCO#         IFNE      AICO#
     C     SCPOL#        ORNE      AIPOL#
     C     XXTYPE        ORNE      AIIN
     C                   MOVE      *ON           *IN60
     C                   END
     C                   END
      *
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCADI         IFNE      *BLANKS
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEY
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVE      SCADI         XXADI
     C                   MOVE      *BLANKS       SCADI             2
     C     AIKEY         SETLL     PAI01
     C     AIKEY2        READE     PAI01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       18            I1                3 0
     C*
     C*  VALID CURSOR POSITION - RETURN WITH SELECTION
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     4
     C                   MOVE      AI(I1)        XXADI
     C*
     C*   GET ADDITIONAL INTEREST RECORD AND GOTO SCREEN 4
     C*
     C     AIKEY         CHAIN     PAI01                              60
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVE      AIADI         S4ADI
     C                   MOVE      AINAME        S4NAME
     C                   MOVE      AIADR1        S4ADR1
     C                   MOVE      AIADR2        S4ADR2
     C                   MOVE      AICTY         S4CTY
     C                   MOVE      AIST          S4ST
     C                   MOVEL     AIZIP         S4ZIP1
     C                   MOVE      AIZIP         S4ZIP2
     C                   MOVE      AIIN          S4IN
     C                   MOVE      AINOR         S4NOR
     C                   MOVE      AINOD         S4NOD
     C                   MOVE      AICOMM        S4COMM
     C                   MOVE      AISTAT        S4STAT
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANK        S4ADI
     C                   MOVE      *BLANK        S4NAME
     C                   MOVE      *BLANK        S4ADR1
     C                   MOVE      *BLANK        S4ADR2
     C                   MOVE      *BLANK        S4CTY
     C                   MOVE      *BLANK        S4ST
     C                   MOVE      *BLANK        S4ZIP1
     C                   MOVE      *BLANK        S4ZIP2
     C                   MOVE      *BLANK        S4IN
     C                   MOVE      *BLANK        S4NOR
     C                   MOVE      *BLANK        S4NOD
     C                   MOVE      *BLANK        S4COMM
     C                   MOVE      *BLANK        S4STAT
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  INVALID CURSOR POSITION - GOTO SCREEN 3
     C*
     C                   MOVE      *ZERO         SCLOC#
     C                   MOVE      *ZERO         SCPRM#
     C                   MOVE      *ZERO         SCVEH#
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C*   SCREEN 3
     C*
     C     SCR03T        TAG
     C*
     C*   IF LOCATION AND BUILDING ARE NOT BLANK - VARIFY THAT THE POLICY
     C*   HAS COVERAGE SECTIONS FOR LOCATION/BUILDINGS AND CALL PROGRAM
     C*
     C     SCLOC#        IFNE      *ZERO
     C     SCPRM#        ORNE      *ZERO
     C*
     C*   VARIFY LOCATION/BUILDING
     C*
     C                   Z-ADD     1             X                 5 0
     C                   MOVE      'N'           XXLOCS            1
     C     X             DOWLE     20
     C     XXLOCS        ANDEQ     'N'
     C*
     C     SC(X)         IFNE      *BLANK
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SC(X)         XXKEY
     C     TBKEY         CHAIN     PCV01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     CVVAL         IFNE      'A'
     C                   MOVE      'Y'           XXLOCS
     C                   END
     C                   END
     C                   END
     C*
     C                   ADD       1             X
     C                   END
     C*
     C     XXLOCS        IFEQ      'N'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*   CALL LOCATION/BUILDING REVIEW PROGRAM
     C*
     C                   MOVEL     SCPOL#        QQPOL#           13
     C                   MOVE      SCLOC#        QQLOC#            3
     C                   MOVE      SCPRM#        QQPRM#            3
     C                   MOVE      ' '           QQRTCD            1
     C                   CALL      'C0036R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQLOC#
     C                   PARM                    QQPRM#
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C*
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      'C'
     C                   MOVE      *BLANK        SCPOL#
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE
     C*
     C                   END
     C*
     C*   IF VEHICAL NUMBER IS NOT BLANK - VARIFY THAT THE POLICY  HAS
     C*   AUTO COVERAGE AND CALL PROGRAM
     C*
     C     SCVEH#        IFNE      *ZERO
     C*
     C*   VARIFY AUTO COVERAGE
     C*
     C                   Z-ADD     1             X
     C                   MOVE      'N'           XXAUTO            1
     C     X             DOWLE     20
     C     XXAUTO        ANDEQ     'N'
     C*
     C     SC(X)         IFNE      *BLANK
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SC(X)         XXKEY
     C     TBKEY         CHAIN     PCV01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     CVVAL         IFEQ      'A'
     C                   MOVE      'Y'           XXAUTO
     C                   END
     C                   END
     C                   END
     C*
     C                   ADD       1             X
     C                   END
     C*
     C     XXAUTO        IFEQ      'N'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*   CALL AUTO REVIEW PROGRAM
     C*
     C                   MOVEL     SCPOL#        QQPOL#           13
     C                   MOVE      SCVEH#        QQVEH#            5
     C                   MOVE      ' '           QQRTCD            1
     C                   CALL      'C0037R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQVEH#
     C                   PARM                    QQDLDT
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C*
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      'C'
     C                   MOVE      *BLANK        SCPOL#
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        SCPOL#
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*   SCREEN 4
     C*
     C     SCR04T        TAG
     C*
     C                   MOVE      KEY(1)        SCADI
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      SCR01T
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       AI
     C                   MOVE      *BLANKS       NM
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVEL     AIADI         AI(I1)
     C                   MOVEL     AINAME        NM(I1)
     C                   ENDSR
     C*
     C*  LOAD FORWARD SUBROUTINE
     C*
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C     OVER          TAG
     C*
     C*  END OF FILE
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONE
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     AIKEY2        READE     PAI01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      4                                    61
     C     *IN61         CABEQ     *ON           OVER
     C     DONE          TAG
     C                   MOVE      AI(1)         KEY(1)
     C                   MOVE      AI(4)         KEY(2)
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     5             I1
     C     OVERP         TAG
     C*
     C*  BEGINNING OF FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADB
     C                   GOTO      DONEP
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   SUB       1             I1
     C                   EXSR      FILL
     C                   READP     PAI01                                  60
     C     *IN60         IFEQ      *OFF
     C     UPCO#         IFNE      AICO#
     C     SCPOL#        ORNE      AIPOL#
     C     XXTYPE        ORNE      AIIN
     C                   MOVE      *ON           *IN60
     C                   END
     C                   END
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C     *IN61         CABEQ     *ON           OVERP
     C     DONEP         TAG
     C                   MOVE      AI(1)         KEY(1)
     C                   MOVE      AI(4)         KEY(2)
     C                   ENDSR
     C*
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXADI
     C     AIKEY         SETLL     PAI01
     C     AIKEY2        READE     PAI01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     AIKEY2        READE     PAI01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      4                                    61
     C     *IN61         CABEQ     *ON           OVERB
     C     DONEB         ENDSR
     C*
** TABLE FOR CANELLATION OR EXPIRATION DATE LABEL
Cancellation Date:
  Expiration Date:
