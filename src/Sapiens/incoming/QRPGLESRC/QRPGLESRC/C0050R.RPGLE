     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0050R - COVERAGE MAINTENANCE - GENERAL LIABILITY
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program _______________________                   **
     H**   CALLED FROM:                                             **
     H**     ______ - ___________________________                   **
     H**   CALLS TO:                                                **
     H**     ______ - ___________________________                   **
     H****************************************************************
     H*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/19/01 C:15  Project: 94CAX                                 *
     H*               -Removed various edits to allow easier entry of *
     H*                policy.                                        *
     H*                                                               *
     H**  11/18/96 *** C:107 *** V2000 ***                          **
     H**           1. Version 2 format changes.                     **
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
     H**   _____  (_____): __________________________ (___)         **
     H****************************************************************
     H*
     F*
     FC0050F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCMPML     IF   E           K DISK
     FCMGLL     UF A E           K DISK
     FCMGLL1    IF   E           K DISK
     F                                     RENAME(PGL01:PGL02)
     FCMLLL     UF A E           K DISK
     FCMLLL1    IF   E           K DISK
     F                                     RENAME(PLL01:PLL02)
     FCMLOL     IF   E           K DISK
     FCMCCL1    IF   E           K DISK
     FCTABL     IF   E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      *
     D                 DS
     D  DSACMY                 1      5  0
     D  DSACCN                 1      1  0
     D  DSACYY                 2      3  0
     D  DSACMM                 4      5  0
     D                 DS
     D  SCACMY                 1      6  0
     D  XXMTH                  1      2  0
     D  XXYR                   3      6  0
     D  SCACMM                 1      2  0
     D  SCACYY                 3      6  0
     D  SCACY2                 5      6  0
     D                 DS
     D  SC                     1     40
     D                                     DIM(20) ASCEND
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
     D                 DS
     D  EC                     1     10
     D                                     DIM(10) ASCEND
     D  SCEC01                 1      1
     D  SCEC02                 2      2
     D  SCEC03                 3      3
     D  SCEC04                 4      4
     D  SCEC05                 5      5
     D  SCEC06                 6      6
     D  SCEC07                 7      7
     D  SCEC08                 8      8
     D  SCEC09                 9      9
     D  SCEC10                10     10
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  GENERAL LIABILITY COV FILE
     C*
     C     GLKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#           13
     C                   KFLD                    SCCC              2
     C                   KFLD                    SCLOC#            3 0
     C                   KFLD                    SCPRM#            3 0
     C                   KFLD                    SCSUBL            3
     C                   KFLD                    SCCLAS            6
     C                   KFLD                    SCTRNS            2
     C                   KFLD                    XXTRDT            7 0
     C*
     C     GLKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#           13
     C                   KFLD                    SCCC              2
     C                   KFLD                    SCLOC#            3 0
     C                   KFLD                    SCPRM#            3 0
     C                   KFLD                    SCSUBL            3
     C                   KFLD                    SCCLAS            6
     C                   KFLD                    XXTRDT
     C*
     C*  GENERAL LIABILITY LIMITS FILE KEY LISTS
     C*
     C     LLKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C                   KFLD                    SCTRNS
     C                   KFLD                    XXTRDT
     C*
     C     LLKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C                   KFLD                    XXTRDT
     C*
     C*  POLICY MASTER FILE
     C*
     C     PMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*
     C*  LOCATION - PREMISES FILE
     C*
     C     LOKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCLOC#
     C                   KFLD                    SCPRM#
     C*
     C*  CLASS CODE FILE KEY LIST
     C*
     C     CCKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C*   TABLE FILE KEY LIST
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C     TBKY1         KLIST
     C                   KFLD                    UPCO#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQPOL#           13
     C                   PARM                    QQCC              2
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
     C                   MOVE      QQPOL#        SCPOL#
     C                   MOVE      QQCC          SCCC
     C                   MOVE      '0'           *IN78                          TURN OFF COL
     C                   Z-ADD     *ZERO         SCACMY
     C*                                                    SEP. ON ERR
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME - IF CALLED FROM
     C*   POLICY MAINT PGM, GO TO POLICY FORM MAINT & NEVER ALLOW SCR01
     C*   TO EXECUTED.
     C*
     C     SCPOL#        IFEQ      *BLANK
     C     SCCC          OREQ      *BLANK
     C                   MOVE      '0'           *IN90
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   ELSE
     C                   MOVE      '1'           *IN90
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'PLFRM'       Q2RECF
     C                   END
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
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
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'PLFRM'
     C*
     C*  GET THE POLICY NAME FROM POLICY MASTER FILE
     C*
     C     PMKEY         CHAIN     PPM01                              60
     C*   SET INDICATOR TO DISPLAY POLICY CANCELLED IF CANCELATION
     C*   FLAG(PMCANC) IS EQUAL TO "C"
     C*
     C     PMCANC        IFEQ      'C'
     C                   MOVE      '1'           *IN03
     C                   ELSE
     C                   MOVE      '0'           *IN03
     C                   END
     C*
     C                   MOVE      *BLANK        QQPOL#
     C                   MOVE      SCPOL#        QQPOL#
     C                   MOVE      *BLANK        QQFIN            40
     C                   MOVEL     PMFIN         QQFIN
     C                   MOVEL     PMFIN         SCFIN
     C*
     C                   MOVE      *BLANK        QQCC
     C                   MOVE      SCCC          QQCC
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0150R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQFIN
     C                   PARM                    QQCC
     C                   PARM                    QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQRTC         OREQ      'C'
     C                   WRITE     SCR99
     C     *IN90         IFEQ      '1'                                          GO BACK TO POL.
     C                   GOTO      TERM                                         MAINT. PGM
     C                   ELSE
     C     QQRTC         IFEQ      'C'
     C                   MOVE      *BLANK        SCPOL#
     C                   MOVE      *BLANK        SCCC
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCCC          XXKEY
     C     TBKEY         CHAIN     PCV01                              60
     C                   MOVEL     CVDESC        SCCODS
     C                   MOVE      *BLANKS       SCLOC#
     C                   MOVE      *BLANKS       SCPRM#
     C                   MOVE      *BLANK        SCSUBL
     C                   MOVE      *BLANK        SCCLAS
     C*** Begin dlt *** 09/19/01 **************************************
     C******************** MOVE *BLANKS   SCTRNS
     C******************** Z-ADDUPPRDT    SRDTI
     C*** End dlt ***** 09/19/01 **************************************
     C*** Begin add *** 09/19/01 **************************************
     C                   MOVE      'NB'          SCTRNS
     C                   Z-ADD     PMEFDT        SRDTI
     C*** End add ***** 09/19/01 **************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C                   MOVE      'N'           SCUPRV
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
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
     C*
     C*   PROCESS ALL FUNCTION KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  FUNCTION 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  FUNCTION 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'PLFRM'       Q2RECF
     C                   END
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C     Q2RECF        IFEQ      'SCR04'
     C     SCUPRV        IFEQ      'Y'
     C                   GOTO      SCR02T
     C                   ELSE
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*  FUNCTION 03 = RESTART - IF CALLED FROM POL. MAINT. GO TO
     C*                          POL.FORMS, OTHERWISE GOTO SCR01
     C*
     C     *INKA         IFEQ      '1'
     C     *IN90         IFEQ      '0'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCPOL#
     C                   MOVE      *BLANK        SCCC
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL     'PLFRM'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  FUNCTION 05 = SEARCH POLICY MASTER - POLICY ORDER - SCREEN 1
     C*
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVE      *BLANK        QQPOL#
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
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQPOL#        OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 05 = COVERAGE DETAIL SEARCH - SCREEN 2
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   MOVE      SCPOL#        QQPOL#
     C                   MOVE      SCCC          QQCC              2
     C                   MOVE      SCLOC#        QQLOC#            3
     C                   MOVE      SCPRM#        QQPRM#            3
     C                   MOVE      *BLANKS       QQSUBL            3
     C                   MOVE      *BLANKS       QQCLAS            6
     C                   MOVE      *BLANKS       QQTRNS            2
     C                   MOVE      *BLANKS       QQTRDT            8
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0550R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCC
     C                   PARM                    QQLOC#
     C                   PARM                    QQPRM#
     C                   PARM                    QQSUBL
     C                   PARM                    QQCLAS
     C                   PARM                    QQTRNS
     C                   PARM                    QQTRDT
     C                   PARM                    QQRTC
     C*
     C                   MOVE      QQPOL#        SCPOL#
     C                   MOVE      QQCC          SCCC
     C                   MOVE      QQLOC#        SCLOC#
     C                   MOVE      QQPRM#        SCPRM#
     C                   MOVE      QQSUBL        SCSUBL
     C                   MOVE      QQCLAS        SCCLAS
     C                   MOVE      QQTRNS        SCTRNS
     C                   MOVE      QQTRDT        SCTRDT
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQLOC#        OREQ      *BLANK
     C     QQPRM#        OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 05 = LIMITS CODE SEARCH - SCREEN 3
     C*
     C                   END
     C*
     C*  FUNCTION 07 = SEARCH POLICY MASTER BY NAME, SCR01
     C*                CALL ADDITIONAL LOCATION PREMISIS, SCR02
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
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQPOL#        OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  CALL ADDITIONAL LOCATION PREMISES - SCREEN 2
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   MOVE      *BLANK        QQPOL#
     C                   MOVEL     SCPOL#        QQPOL#
     C                   CALL      'C0140R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      '1'           *IN30
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 09 = POLICY COVERAGE SEARCH SCR01
     C*                LOCATION PREMISIS SEARCH SCR02
     C     *INKI         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     SCPOL#        IFEQ      *BLANKS
     C                   GOTO      SCR01T
     C                   END
     C                   MOVE      SCPOL#        QQPOL#           13
     C                   MOVE      SCCC          QQCC              2
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0650R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCC
     C                   PARM                    QQRTC             1
     C                   MOVE      QQCC          SCCC
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCCC          OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      SCPOL#        QQPOL#           13
     C                   MOVE      SCLOC#        QQLOC#            3
     C                   MOVE      SCPRM#        QQPRM#            3
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0640R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQLOC#
     C                   PARM                    QQPRM#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQLOC#        SCLOC#
     C                   MOVE      QQPRM#        SCPRM#
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCLOC#        OREQ      *ZERO
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   END
     C*
     C*  TABLE FILE SEARCH  - COVERAGE CODE SCR01; TRANS. CODE SCR02;
     C*                       DEDUCTIBLE CODE SCR03.
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVEL     SCCC          XXQ               2
     C     XXQ           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   CALL      'C2001R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCCC
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      '1'           *IN24
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           STRT
     C*
     C                   END
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   MOVEL     SCTRNS        XXQ               2
     C     XXQ           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   CALL      'C2041R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCTRNS
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C                   MOVEL     SCBIDC        XXQ               2
     C     XXQ           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVE      *BLANK        QQRTC
     C                   CALL      'C2011R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCBIDC
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN71
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN72
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   MOVEL     SCPDDC        XXQ               2
     C     XXQ           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVE      *BLANK        QQRTC
     C                   CALL      'C2011R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     XXKEY         SCPDDC
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN73
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN74
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C                   END
     C*
     C*  FUNCTION 15 = SUBLINE CODE SEARCH - SCREEN 2
     C*
     C     *INKP         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0751R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C                   MOVE      QQKEY         SCSUBL
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 17 = CLASS CODE SEARCH - SCREEN 2
     C*
     C     *INKR         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCCLAS        QQKEY
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0741R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C                   MOVE      QQKEY         SCCLAS
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 20 - CHANGE STATUS
     C*
     C     *INKU         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C     *IN61         ANDEQ     '0'
     C*
     C     SCSTA3        IFEQ      'A'
     C                   MOVE      'I'           SCSTA3
     C                   ELSE
     C                   MOVE      'A'           SCSTA3
     C                   END
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C     *IN62         ANDEQ     '0'
     C*
     C     SCSTA4        IFEQ      'A'
     C                   MOVE      'I'           SCSTA4
     C                   ELSE
     C                   MOVE      'A'           SCSTA4
     C                   END
     C                   END
     C*
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
     C                   CALL      'C0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
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
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C     Q2RECF        CABEQ     'SCR04'       SCR04T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE POLICY NUMBER
     C*
     C     PMKEY         CHAIN     PPM01                              20
     C     *IN20         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C*  COVERAGE CODE MUST BE VALID CODE, GENERAL LIAB TYPE, & ACTIVE
     C*
     C                   Z-ADD     1             X                 2 0
     C     SCCC          LOOKUP    SC(X)                                  21
     C     *IN21         IFNE      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCCC          XXKEY
     C     TBKEY         CHAIN     PCV01                              60
     C*
     C     CVVAL         IFNE      'L'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C     CVSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  ALL EDITS PASSED ON SCR01 GO TO POLICY FORM MAINTENANCE
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'PLFRM'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*   IF ALL FIELDS ARE BLANK ON PROMPT SCREEN(SCR02) RETURN TO
     C*   SCR01 OR POLICY MASTER MAINTENANCE.
     C*
     C     SCLOC#        IFEQ      *ZERO
     C     SCPRM#        ANDEQ     *ZERO
     C     SCSUBL        ANDEQ     *BLANK
     C     SCCLAS        ANDEQ     *BLANK
     C     SCTRNS        ANDEQ     *BLANK
     C*          SCTRDT    ANDEQ*ZERO
     C*
     C     *IN90         IFEQ      '1'                                          GO BACK TO POL.
     C                   GOTO      TERM                                         MAINT. PGM
     C                   ELSE
     C                   MOVE      *BLANK        SCPOL#
     C                   MOVE      *BLANK        SCCC
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*
     C* Chain to find out if record exists; if the record exists
     C* do not edit screen information.  User wants to see data that
     C* may have an inactive  codes (data entered prior to inactivating
     C* codes)
     C*
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C     GLKEY         CHAIN     PGL01                              40
     C     *IN40         IFNE      '0'
     C*
     C*   VALIDATE LOCATION & PREMISES NUMBER
     C*
     C     LOKEY         CHAIN     PLO01                              20
     C     *IN20         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C     LOSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE SUBLINE - MUST BE IN SUBLINE TABLE
     C*
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCSUBL        QQKEY
     C                   MOVE      'V'           QQRTC             1
     C                   CALL      'C0751R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C     QQRTC         IFEQ      'B'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE CLASS CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVE      SCCLAS        XXKEY
     C     CCKEY         CHAIN     PCC01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  TRANSACTION CODE MUST BE VALID AND ACTIVE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCTRNS        XXKEY
     C     TBKEY         CHAIN     PTT01                              23
     C     *IN23         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C     TTSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*  TRANS. DATE MUST BE A VALID DATE & BETWEEN POL. FROM & TO DATE
     C*
     C     SCTRDT        IFEQ      *ZERO
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      *BLANKS       SRDTE             6
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTRDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     PMCNDT        IFNE      0
     C                   Z-ADD     PMCNDT        XXTODT            7 0
     C                   ELSE
     C                   Z-ADD     PMEXDT        XXTODT
     C                   END
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C     XXTRDT        IFLT      PMEFDT
     C     XXTRDT        ORGT      XXTODT
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE UPDATE/REVIEW POLICY LIMITS FIELD - Y/N
     C*
     C     SCUPRV        IFNE      'Y'
     C     SCUPRV        ANDNE     'N'
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE
     C*
     C*  GET TRANSACTION CODE TYPE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCTRNS        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   END                                                    Edit bypass
     C*
     C*   ALL EDITS PASSED, GET THE REQUESTED RECORD, LOAD INFORMATION
     C*   IF THERE IS NO HIT ON THE EXACT INFO, GET THE PREVIOUS DATES
     C*   INFO.
     C*
     C     GLKEY         CHAIN     PGL01                              40
     C*
     C     *IN40         IFEQ      '1'
     C*
     C*** Begin dlt *** 09/19/01 **************************************
     C*   VALIDATE THAT TRANS DATE IS EQUAL TO USER PARM PROCESS DATE
     C*   IF ADDING A RECORD OF COVERAGE
     C**********
     C********** XXTRDT    IFNE UPPRDT
     C**********           MOVE '1'       *IN36
     C**********           GOTO STRT
     C**********           END
     C*** End dlt ***** 09/19/01 **************************************
     C*
     C*   INDICATOR TO CONTROL CHANGING STATUS
     C*
     C                   MOVE      '1'           *IN62
     C*
     C*  CASE MUST ALREADY EXIST IF THIS IS IN INQUIRE MODE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C     GLKY1         SETGT     PGL02
     C                   MOVE      'I'           GLSTAT
     C                   MOVE      '0'           *IN40
     C     GLSTAT        DOWEQ     'I'
     C     *IN40         ANDNE     '1'
     C                   READP     PGL02                                  40
      *
     C     *IN40         IFEQ      *OFF
     C     GLCO#         IFNE      UPCO#
     C     GLPOL#        ORNE      SCPOL#
     C     GLCC          ORNE      SCCC
     C     GLLOC#        ORNE      SCLOC#
     C     GLPRM#        ORNE      SCPRM#
     C     GLSUBL        ORNE      SCSUBL
     C     GLCLAS        ORNE      SCCLAS
     C                   MOVE      *ON           *IN40
     C                   END
     C                   END
      *
     C                   END
     C*       SET MODE TO 'ADD'. CLEAR PREMIUM FIELD & MOVE CORRECT
     C*       END# INTO GLEND# FOR ADD MODE.
     C                   MOVE      UA(1)         SCMOD4
     C                   MOVE      *BLANKS       GLPREM
     C                   MOVE      *BLANKS       GLPDPM
     C                   MOVE      UMONTH        GLACMY
     C                   MOVEL     UYEAR         GLACMY
     C                   Z-ADD     PMNEND        GLEND#
     C*
     C                   ELSE
     C                   MOVE      '0'           *IN62
     C                   EXCEPT    PGL01E
     C                   MOVE      UA(2)         SCMOD4
     C                   END
     C*
     C     *IN40         IFEQ      '0'
     C*
     C                   Z-ADD     GLACMY        DSACMY
     C                   MOVEL     DSACMM        SCACMM
     C                   Z-ADD     DSACYY        SCACYY
     C     DSACCN        IFEQ      1
     C                   ADD       2000          SCACYY
     C                   ELSE
     C                   ADD       1900          SCACYY
     C                   END
      *
     C     TTTYPE        IFEQ      'E'
     C                   Z-ADD     GLEND#        SCEND4
     C                   ELSE
     C                   Z-ADD     0             SCEND4
     C                   END
     C                   MOVE      GLTERF        SCTERF
     C                   MOVE      GLPRBS        SCPRBS
     C                   MOVE      GLEXP         SCEXP
     C                   MOVE      GLPREM        SCPREM
     C                   MOVE      GLPDPM        SCPDPM
     C                   MOVE      GLSTAT        SCSTA4
     C*
     C                   ELSE
     C*
     C     TTTYPE        IFEQ      'E'
     C                   Z-ADD     PMNEND        SCEND4
     C                   ELSE
     C                   Z-ADD     0             SCEND4
     C                   END
     C                   MOVEL     UMONTH        SCACMM
     C                   MOVE      *YEAR         SCACYY
     C                   MOVE      *BLANK        SCTERF
     C                   MOVE      *BLANK        SCEXP
     C                   MOVE      *BLANK        SCPREM
     C                   MOVE      *BLANK        SCPDPM
     C                   MOVE      *BLANK        SCPRBS
     C                   MOVE      'A'           SCSTA4
     C*
     C                   END
     C*
     C*   GET THE REQUESTED RECORDS FOR SCREEN 3, LOAD INFORMATION
     C*   IF THERE IS NO HIT ON THE EXACT INFO, GET THE PREVIOUS DATES
     C*   INFO.
     C*
     C     LLKEY         CHAIN     PLL01                              40
     C*
     C     *IN40         IFEQ      '1'
     C*
     C*   INDICATOR TO CONTROL CHANGING STATUS
     C*
     C                   MOVE      '1'           *IN61
     C*
     C*   RECORD MUST EXIST IN INQUIRY MODE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*   GET RECORD FOR LAST TRANS ACTION DATE AND LOAD SCREEN WITH
     C*   THAT RECORDS INFORMATION TO ADD TO FILE
     C*
     C     LLKY1         SETGT     PLL02
     C                   MOVE      'I'           LLSTAT
     C                   MOVE      '0'           *IN40
     C     LLSTAT        DOWEQ     'I'
     C     *IN40         ANDNE     '1'
     C                   READP     PLL02                                  40
      *
     C     *IN40         IFEQ      *OFF
     C     LLCO#         IFNE      UPCO#
     C     LLPOL#        ORNE      SCPOL#
     C     LLCC          ORNE      SCCC
     C                   MOVE      *ON           *IN40
     C                   END
     C                   END
      *
     C                   END
     C*
     C                   MOVE      UA(1)         SCMOD3
     C                   Z-ADD     PMNEND        LLEND#
     C*
     C                   ELSE
     C                   MOVE      '0'           *IN61
     C                   EXCEPT    PLL01E
     C                   MOVE      UA(2)         SCMOD3
     C                   END
     C*
     C*   LOAD SCREEN 4
     C*
     C     *IN40         IFEQ      '0'
     C     TTTYPE        IFEQ      'E'
     C                   Z-ADD     LLEND#        SCEND3
     C                   ELSE
     C                   Z-ADD     0             SCEND3
     C                   END
     C                   MOVE      LLBFGL        SCBFGL
     C                   MOVE      LLLIMT        SCLIMT
     C*
     C                   MOVE      LLBLMT        SCBLMT
     C                   MOVE      LLPLMT        SCPLMT
     C*
     C                   MOVE      LLGAG         SCGAG
     C                   MOVE      LLPCPA        SCPCPA
     C                   MOVE      LLPDDC        SCPDDC
     C                   MOVE      LLBIDC        SCBIDC
     C                   MOVE      LLCOVG        SCCOVG
      *** BEG CHG *** 8/11/98 **************************************
     C********             MOVE LLECMD    SCECMD
     C                   Z-ADD     LLECMD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCECMD
      *** BEG CHG *** 8/11/98 **************************************
     C                   MOVE      LLRISK        SCRISK
     C                   MOVE      LLCMMD        SCCMMD
     C                   MOVE      LLFDML        SCFDML
     C                   MOVE      LLMDPL        SCMDPL
     C                   MOVE      LLEXTL        SCEXTL
     C                   MOVE      LLEC01        SCEC01
     C                   MOVE      LLEC02        SCEC02
     C                   MOVE      LLEC03        SCEC03
     C                   MOVE      LLEC04        SCEC04
     C                   MOVE      LLEC05        SCEC05
     C                   MOVE      LLEC06        SCEC06
     C                   MOVE      LLEC07        SCEC07
     C                   MOVE      LLEC08        SCEC08
     C                   MOVE      LLEC09        SCEC09
     C                   MOVE      LLEC10        SCEC10
     C                   MOVE      LLSTAT        SCSTA3
     C*
     C                   ELSE
     C*
     C                   MOVE      'Y'           SCUPRV
     C     TTTYPE        IFEQ      'E'
     C                   Z-ADD     LLEND#        SCEND3
     C                   ELSE
     C                   Z-ADD     0             SCEND3
     C                   END
     C                   MOVE      'Y'           SCBFGL
     C                   MOVE      *BLANKS       SCLIMT
     C                   MOVE      *BLANKS       SCPLMT
     C                   MOVE      *BLANKS       SCBLMT
     C                   MOVE      *BLANKS       SCGAG
     C                   MOVE      *BLANKS       SCPCPA
     C*
     C*   GET FIRST RECORD IN DEDUCTIBLE TABLE FOR DEFAULT
     C*
     C     TBKY1         CHAIN     PDE01                              60
     C                   MOVEL     DEKEY         SCPDDC
     C                   MOVEL     DEKEY         SCBIDC
     C                   MOVE      *BLANKS       SCCOVG
     C                   MOVE      *BLANKS       SCECMD
     C                   MOVE      *BLANKS       SCRISK
     C                   MOVE      *BLANKS       SCCMMD
     C                   MOVE      *BLANKS       SCFDML
     C                   MOVE      *BLANKS       SCMDPL
     C                   MOVE      'N'           SCEXTL
     C                   MOVE      *BLANKS       SCEC01
     C                   MOVE      *BLANKS       SCEC02
     C                   MOVE      *BLANKS       SCEC03
     C                   MOVE      *BLANKS       SCEC04
     C                   MOVE      *BLANKS       SCEC05
     C                   MOVE      *BLANKS       SCEC06
     C                   MOVE      *BLANKS       SCEC07
     C                   MOVE      *BLANKS       SCEC08
     C                   MOVE      *BLANKS       SCEC09
     C                   MOVE      *BLANKS       SCEC10
     C                   MOVE      'A'           SCSTA3
     C                   END
     C*
     C*   DETERMINE IF WE NEED TO GOTO SCREEN 3- SCUPRV = Y; NO RECORD
     C*   IN LIABILITY LIMITS FILE; SUBLINE 336 AND NO PROD/COOP AGGRAGATE
     C*
     C     SCUPRV        IFEQ      'Y'
     C     SCSUBL        OREQ      '336'
     C     LLPCPA        ANDEQ     *ZERO
     C                   MOVE      'Y'           SCUPRV
     C*
     C     SCSUBL        IFEQ      '336'
     C     LLPCPA        ANDEQ     *ZERO
     C                   MOVE      '1'           *IN49
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     SCR03T        TAG
     C*
     C     *IN09         CABEQ     '1'           SCR03B
     C*
     C*   LIMIT ID MUST BE "1", "2", OR "3"
     C*
     C     SCLIMT        IFNE      '1'
     C     SCLIMT        ANDNE     '2'
     C     SCLIMT        ANDNE     '3'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*   BI/CSL LIMIT MUST BE GREATER THAN ZERO
     C*
     C     SCBLMT        IFLE      *ZERO
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*   DED CD BI MUST BE IN DEDUTIBLE TABLE FILE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBIDC        XXKEY
     C     TBKEY         CHAIN     PDE01                              22
     C     *IN22         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C     DESTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*   PD LIMIT MUST BE GREATER THAN ZERO IF SPLIT LIMIT
     C*   PD LIMIT MUST BE ZERO IF SINGLE LIMIT
     C*
     C     SCLIMT        IFEQ      '3'
     C     SCPLMT        IFEQ      *ZERO
     C                   MOVE      '1'           *IN75
     C                   GOTO      STRT
     C                   END
     C                   ELSE
     C     SCPLMT        IFNE      *ZERO
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   DED CD PD MUST BE IN DEDUTIBLE TABLE FILE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCPDDC        XXKEY
     C     TBKEY         CHAIN     PDE01                              25
     C     *IN25         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C     DESTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*   BROAD FORM CGL MUST BE Y OR N
     C*
     C     SCBFGL        IFNE      'Y'
     C     SCBFGL        ANDNE     'N'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE EXCLUSIONS
     C*
     C                   Z-ADD     1             X
     C     X             DOWLE     10
     C     *BLANK        LOOKUP    EC(X)                              60
     C     *IN60         IFEQ      '1'
     C*
     C     EC(X)         IFNE      'I'
     C     EC(X)         ANDNE     'C'
     C     EC(X)         ANDNE     'F'
     C     EC(X)         ANDNE     'E'
     C     EC(X)         ANDNE     'A'
     C     EC(X)         ANDNE     'N'
     C     EC(X)         ANDNE     'G'
     C     EC(X)         ANDNE     'H'
     C     EC(X)         ANDNE     'T'
     C     EC(X)         ANDNE     'W'
     C     EC(X)         ANDNE     'D'
     C     X             ADD       27            Z                 5 0
     C                   MOVE      '1'           *IN(Z)
     C                   GOTO      STRT
     C                   END
     C                   ELSE
     C                   Z-ADD     10            X
     C                   END
     C                   ADD       1             X
     C                   END
     C*
     C*   RISK ID MUST BE "1", "2", "3", "4", OR BLANK
     C*
     C     SCRISK        IFNE      '1'
     C     SCRISK        ANDNE     '2'
     C     SCRISK        ANDNE     '3'
     C     SCRISK        ANDNE     '4'
     C     SCRISK        ANDNE     ' '
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END
     C*
     C*   COVERAGE COD MUST BE "1" THRU "5"
     C*
     C     SCCOVG        IFNE      '1'
     C     SCCOVG        ANDNE     '2'
     C     SCCOVG        ANDNE     '3'
     C     SCCOVG        ANDNE     '4'
     C     SCCOVG        ANDNE     '5'
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   END
     C*
     C*   MUST HAVE VALID RETRO DATE IF NOT BLANK
     C*
     C     SCECMD        IFNE      *ZERO
     C                   Z-ADD     SCECMD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCECMD
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   VALIDATE Y/N FOR EXTENDED TAIL
     C*
     C     SCEXTL        IFNE      'Y'
     C     SCEXTL        ANDNE     'N'
     C                   MOVE      '1'           *IN44
     C                   GOTO      STRT
     C                   END
     C*
     C*   FIRE DAMAGE LIMIT MUST BE GREATER THAN ZERO
     C*
     C     SCFDML        IFLT      *ZERO
     C                   MOVE      '1'           *IN45
     C                   GOTO      STRT
     C                   END
     C*
     C*   MED PAY LIMIT MUST BE BLANK OR FROM 500 THRU 1,000,000
     C*
     C     SCMDPL        IFNE      *ZERO
     C     SCMDPL        IFLT      500
     C     SCMDPL        ORGT      1000000
     C                   MOVE      '1'           *IN46
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   VALIDATE GENERAL AGGRAGATE NOT LESS THAN ZERO
     C*
     C     SCGAG         IFLT      *ZERO
     C                   MOVE      '1'           *IN47
     C                   GOTO      STRT
     C                   END
     C*
     C     SCGAG         IFEQ      *ZERO
     C                   MOVE      99999999      SCGAG
     C                   END
     C*
     C*   VALIDATE PROD/COOP AGGRAGATE IF SUBLINE 336
     C*
     C     SCSUBL        IFEQ      '336'
     C     SCPCPA        IFEQ      *ZERO
     C                   MOVE      99999999      SCPCPA
     C                   END
     C*
     C                   ELSE
     C*
     C     SCPCPA        IFLT      *ZERO
     C                   MOVE      '1'           *IN48
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     LLKEY         CHAIN     PLL01                              40
     C                   MOVE      UPCO#         LLCO#
     C                   MOVE      SCPOL#        LLPOL#
     C                   MOVE      SCCC          LLCC
     C                   MOVE      SCTRNS        LLTRNS
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LLTRDT
     C                   MOVE      SCEND3        LLEND#
     C                   MOVE      SCBFGL        LLBFGL
     C                   MOVE      SCLIMT        LLLIMT
     C                   MOVE      SCBLMT        LLBLMT
     C                   MOVE      SCPLMT        LLPLMT
     C                   MOVE      SCGAG         LLGAG
     C                   MOVE      SCPCPA        LLPCPA
     C                   MOVE      SCPDDC        LLPDDC
     C                   MOVE      SCBIDC        LLBIDC
     C                   MOVE      SCCOVG        LLCOVG
     C                   Z-ADD     SCECMD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LLECMD
     C                   MOVE      SCRISK        LLRISK
     C                   MOVE      SCCMMD        LLCMMD
     C                   MOVE      SCFDML        LLFDML
     C                   MOVE      SCMDPL        LLMDPL
     C                   MOVE      SCEXTL        LLEXTL
     C                   MOVE      SCEC01        LLEC01
     C                   MOVE      SCEC02        LLEC02
     C                   MOVE      SCEC03        LLEC03
     C                   MOVE      SCEC04        LLEC04
     C                   MOVE      SCEC05        LLEC05
     C                   MOVE      SCEC06        LLEC06
     C                   MOVE      SCEC07        LLEC07
     C                   MOVE      SCEC08        LLEC08
     C                   MOVE      SCEC09        LLEC09
     C                   MOVE      SCEC10        LLEC10
     C                   MOVE      SCSTA3        LLSTAT
     C                   MOVE      Q1USER        LLUSER
     C                   MOVE      Q1PGM         LLPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LLDT
     C                   Z-ADD     DSTIME        LLTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PLL01
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LLRCDT
     C                   Z-ADD     *ZEROS        LLBUL3
     C                   WRITE     PLL01
     C                   END
     C     SCR03B        TAG
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR04T        TAG
     C*
     C     *IN09         CABEQ     '1'           SCR04B
     C*
     C*   VALIDATE TERRITORY CODE
     C*
     C*** Begin dlt *** 09/19/01 **************************************
     C********** SCTERF    IFNE '001'
     C********** SCTERF    ANDNE'002'
     C********** SCTERF    ANDNE'003'
     C********** SCTERF    ANDNE'004'
     C********** SCTERF    ANDNE'005'
     C********** SCTERF    ANDNE'006'
     C********** SCTERF    ANDNE'999'
     C*** End dlt ***** 09/19/01 **************************************
     C*** Begin add *** 09/19/01 **************************************
     C     SCTERF        IFNE      *BLANKS
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCTERF        QQKEY
     C                   MOVE      'V'           QQRTC             1
     C                   CALL      'C0730R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C*** End add ***** 09/19/01 **************************************
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*** Begin add *** 09/19/01 **************************************
     C                   END
     C*** End add ***** 09/19/01 **************************************
     C*
     C*** Begin dlt *** 09/19/01 **************************************
     C*   PREMIUM BASIS CANNOT BE BLANK
     C**********
     C********** SCPRBS    IFEQ *BLANK
     C**********           MOVE '1'       *IN21
     C**********           GOTO STRT
     C**********           END
     C*** End dlt ***** 09/19/01 **************************************
     C*
     C*   MUST HAVE VALID ACCT DT - MONTH / YEAR
     C*
     C     XXMTH         IFLT      1
     C     XXMTH         ORGT      12
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     GLKEY         CHAIN     PGL01                              40
     C                   MOVE      UPCO#         GLCO#
     C                   MOVE      SCPOL#        GLPOL#
     C                   MOVE      SCCC          GLCC
     C                   MOVE      SCLOC#        GLLOC#
     C                   MOVE      SCPRM#        GLPRM#
     C                   MOVE      SCTRNS        GLTRNS
     C                   CLEAR                   DSACMY
     C                   MOVE      SCACYY        DSACYY
     C                   MOVEL     SCACMM        DSACMM
     C     SCACYY        IFGT      1999
     C                   MOVEL     1             DSACCN
     C                   ELSE
     C                   MOVEL     0             DSACCN
     C                   END
     C                   Z-ADD     DSACMY        GLACMY
      *
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GLTRDR
     C                   MOVE      SCEND4        GLEND#
     C                   MOVE      SCTERF        GLTERF
     C                   MOVE      SCSUBL        GLSUBL
     C                   MOVE      SCCLAS        GLCLAS
     C                   MOVE      SCEXP         GLEXP
     C                   MOVE      SCPREM        GLPREM
     C                   MOVE      SCPDPM        GLPDPM
     C                   MOVE      SCPRBS        GLPRBS
     C                   MOVE      SCSTA4        GLSTAT
     C                   MOVE      Q1USER        GLUSER
     C                   MOVE      Q1PGM         GLPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GLDT
     C                   Z-ADD     DSTIME        GLTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PGL01
     C                   MOVE      'U'           QQUPAD            1
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GLRCDT
     C                   Z-ADD     *ZEROS        GLBUL3
     C                   WRITE     PGL01
     C                   MOVE      'A'           QQUPAD
     C                   END
     C*
     C*   CALL PREMIUM BEARING ENDORSEMENT MAINTENANCE IF AN ENDORSEMENT RECORD
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCTRNS        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C     TTTYPE        IFEQ      'E'
     C                   Z-ADD     *ZERO         XXAMT             8 0
     C     GLPREM        ADD       GLPDPM        XXAMT
     C*
     C                   MOVE      SCPOL#        QQPOL#           13
     C                   MOVE      SCCC          QQCC              2
     C                   MOVE      SCTRDT        QQTRDT            8
     C                   MOVE      SCEND4        QQEND#            3
     C                   MOVE      XXAMT         QQAMT             8 0
     C                   CALL      'C0281R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCC
     C                   PARM                    QQTRDT
     C                   PARM                    QQEND#
     C                   PARM                    QQAMT
     C                   PARM                    QQUPAD
     C*
     C                   END
     C*
     C     SCR04B        TAG
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVE      *BLANKS       SCLOC#
     C                   MOVE      *BLANKS       SCPRM#
     C                   MOVE      *BLANK        SCCLAS
     C                   MOVE      *BLANK        SCSUBL
     C*** Begin dlt *** 09/19/01 **************************************
     C******************** MOVE *BLANKS   SCTRNS
     C******************** Z-ADDUPPRDT    SRDTI
     C*** End dlt ***** 09/19/01 **************************************
     C*** Begin add *** 09/19/01 **************************************
     C                   MOVE      'NB'          SCTRNS
     C                   Z-ADD     PMEFDT        SRDTI
     C*** End add ***** 09/19/01 **************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C                   MOVE      'N'           SCUPRV
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPGL01     E            PGL01E
     OPLL01     E            PLL01E
**  TABLE UA
-- ADD --
- UPDATE -
