     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0110R - COVERAGE MAINTENANCE - BUSINESS OWNER
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program _______________________                   **
     H**   CALLED FROM:                                             **
     H**     C0281R -  PREMIUM ENDORSEMENT ENTRY                    **
     H**   CALLS TO:                                                **
     H**     ______ - ___________________________                   **
     H****************************************************************
     H*
     H****************************************************************
     H** REVISIONS:                                                 **
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
     FC0110F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCMPML     IF   E           K DISK
     FCMBOL     UF A E           K DISK
     FCMBOL1    IF   E           K DISK
     F                                     RENAME(PBO01:PBO02)
     FCMBLL     UF A E           K DISK
     FCMBLL1    IF   E           K DISK
     F                                     RENAME(PBL01:PBL02)
     FCMP1L     UF A E           K DISK
     FCMLOL     IF   E           K DISK
     FCDIIL     IF   E           K DISK
     FCTABL     IF   E           K DISK
     F                                     IGNORE(PPC01)

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D                 DS
     D  DSACMY                 1      5  0
     D  DSACCN                 1      1  0
     D  DSACYY                 2      3  0
     D  DSACMM                 4      5  0
     D                 DS
     D  SCACMY                 1      6  0
     D  SCACMM                 1      2  0
     D  SCACYY                 3      6  0
     D  SCACY2                 5      6  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
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
     C*  BUSINESSOWNER MASTER FILE
     C*
     C     BOKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C                   KFLD                    SCLOC#
     C                   KFLD                    SCPRM#
     C                   KFLD                    SCCVBO
     C                   KFLD                    SCTRNS
     C                   KFLD                    XXTRDT
     C*
     C     BOKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C                   KFLD                    SCLOC#
     C                   KFLD                    SCPRM#
     C                   KFLD                    SCCVBO
     C                   KFLD                    XXTRDT
     C*
     C*  BUSINESSOWNER LIABILITY MASTER FILE
     C*
     C     BLKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C                   KFLD                    SCTRNS
     C                   KFLD                    XXTRDT
     C*
     C     BLKY1         KLIST
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
     C*  INLAND MARINE ITEM FILE
     C*
     C     IIKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C                   KFLD                    SCLOC#
     C                   KFLD                    SCPRM#
     C                   KFLD                    XXCLAS
     C*
     C*  POLICY COVERAGE FILE
     C*
     C     PCKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    SCCC
     C*
     C*  TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQPOL#
     C                   PARM                    QQCC
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
     C*
     C*
     C     *LIKE         DEFINE    BOTRDT        XXTRDT
     C                   MOVE      *BLANKS       QQTRDT            8
     C     *LIKE         DEFINE    PMPOL#        QQPOL#
     C     *LIKE         DEFINE    PMPOL#        XXPOL#
     C     *LIKE         DEFINE    PMCC01        QQCC
     C     *LIKE         DEFINE    PMCC01        XXCC
     C                   MOVE      *BLANK        QQLOC#            3
     C                   MOVE      *BLANK        QQPRM#            3
     C     *LIKE         DEFINE    BOCVBO        QQCVBO
     C     *LIKE         DEFINE    BOTRNS        QQTRNS
     C                   MOVE      *BLANK        XXCLAS            6
     C*
     C*
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME - IF CALLED FROM
     C*   POLICY MAINT PGM, GO TO POLICY FORM MAINT & NEVER ALLOW SCR01
     C*   TO BE EXECUTED.
     C*
     C     SCPOL#        IFEQ      *BLANK
     C     SCCC          OREQ      *BLANK
     C                   MOVE      '0'           *IN76
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   ELSE
     C                   MOVE      '1'           *IN76
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
     C*   IF CALLED FROM ANOTHER PROGRAM GOTO SCR01T
     C*
     C     *IN76         IFEQ      '1'
     C                   GOTO      SCR01T
     C                   END
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
     C     Q2RECF        IFEQ      'SCR05'
     C                   EXFMT     SCR05
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
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     *IN76         IFEQ      '1'
     C                   GOTO      TERM
     C                   ELSE
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   GOTO      SCR01T
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C                   MOVEL     'SCR03'       Q2RECF
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR05'
     C     SCCVBO        IFEQ      '01'
     C     SCCVBO        OREQ      '02'
     C     SCCVBO        OREQ      '23'
     C     SCCVBO        OREQ      '24'
     C     SCCVBO        OREQ      ' 1'
     C     SCCVBO        OREQ      ' 2'
     C                   MOVEL     'SCR04'       Q2RECF
     C                   ELSE
     C                   MOVEL     'SCR03'       Q2RECF
     C                   END
     C                   END
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*  FUNCTION 03 = RESTART - IF CALLED FROM POL. MAINT. GO TO
     C*                          POL.FORMS, OTHERWISE GOTO SCR01
     C*
     C     *INKA         IFEQ      '1'
     C     *IN76         IFEQ      '0'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCPOL#
     C                   MOVE      *BLANK        SCCC
     C                   GOTO      STRT
     C                   ELSE
     C                   GOTO      SCR01T
     C                   END
     C                   END
     C*
     C*  FUNCTION 05 = SEARCH POLICY MASTER - POLICY ORDER - SCREEN 1
     C*                DETAIL SEARCH - SCREEN 3
     C*                TERRITORY SEARCH - SCREEN 4
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
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 05 = COVERAGE DETAIL SEARCH - SCREEN 3
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C                   MOVE      SCPOL#        QQPOL#
     C                   MOVE      SCCC          QQCC
     C                   MOVE      SCLOC#        QQLOC#
     C                   MOVE      SCPRM#        QQPRM#
     C                   MOVE      *BLANKS       QQCVBO
     C                   MOVE      *BLANKS       QQTRNS
     C                   MOVE      *BLANKS       QQTRDT
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0610R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCC
     C                   PARM                    QQLOC#
     C                   PARM                    QQPRM#
     C                   PARM                    QQCVBO
     C                   PARM                    QQTRNS
     C                   PARM                    QQTRDT
     C                   PARM                    QQRTC
     C*
     C                   MOVE      QQPOL#        SCPOL#
     C                   MOVE      QQCC          SCCC
     C                   MOVE      QQLOC#        SCLOC#
     C                   MOVE      QQPRM#        SCPRM#
     C                   MOVE      QQCVBO        SCCVBO
     C                   MOVE      QQTRNS        SCTRNS
     C                   MOVE      QQTRDT        SCTRDT
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQLOC#        OREQ      *BLANK
     C     QQPRM#        OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR03T
     C*
     C                   END
     C*
     C*  FUNCTION 05 = TERRITORY SEARCH - SCR04
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCTERR        QQKEY
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0730R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C                   MOVE      QQKEY         SCTERR
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C*                    MOVE '1'       *IN70
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN71
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 07 = SEARCH POLICY MASTER BY NAME - SCR01
     C*                CALL ADDITIONAL LOCATION PREMISIS - SCR03
     C*                CLASS SEARCH - SCR04
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
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  CALL ADDITIONAL LOCATION PREMISES - SCREEN 2
     C*
     C     Q2RECF        IFEQ      'SCR03'
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
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   CLASS SEARCH - SCREEN 4
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCCLAS        QQKEY
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0746R'
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
     C                   MOVEL     'SCR04'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
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
     C                   END
     C*
     C*  FUNCTION 09 = POLICY COVERAGE SEARCH SCR01
     C*                LOCATION PREMISIS SEARCH SCR03
     C     *INKI         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     SCPOL#        IFEQ      *BLANKS
     C                   GOTO      SCR01T
     C                   END
     C                   MOVE      SCPOL#        QQPOL#
     C                   MOVE      SCCC          QQCC
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
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  FUNCTION 9 - SEARCH LOCATION/BUILDING MASTER
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVE      SCPOL#        QQPOL#
     C                   MOVE      SCLOC#        QQLOC#
     C                   MOVE      SCPRM#        QQPRM#
     C                   MOVE      *BLANK        QQRTC
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
     C                   MOVEL     'SCR03'       Q2RECF
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
     C*  TABLE FILE SEARCH  - TRANS. CODE SEARCH - SCR03;
     C*                       COVERAGE CODE SEARCH - SCR03.
     C*                       DEDUCTIBLE CODE SEARCH - SCR04.
     C*                       DEDUCTIBLE CODE SEARCH - SCR05.
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C     SCTRNS        IFEQ      '? '
     C     SCTRNS        OREQ      ' ?'
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
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
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
     C*   COVERAGE CODE SEARCH
     C*
     C     SCCVBO        IFEQ      '? '
     C     SCCVBO        OREQ      ' ?'
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCTERR        QQKEY
     C                   MOVE      *BLANK        QQRTC             1
     C                   CALL      'C0782R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C                   MOVE      QQKEY         SCCVBO
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C*                    MOVE '1'       *IN31
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
     C*   DEDUCTIBLE CODE SEARCH SCREEN 4
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C*
     C     SCDECD        IFEQ      '? '
     C     SCDECD        OREQ      ' ?'
     C*
     C                   MOVE      *BLANK        QQKEY
     C                   MOVE      *BLANK        QQRTC
     C                   CALL      'C2011R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     QQKEY         SCDECD
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
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
     C*
     C*   DEDUCTIBLE CODE SEARCH SCREEN 5
     C*
     C     Q2RECF        IFEQ      'SCR05'
     C*
     C     SCDEDC        IFEQ      '? '
     C     SCDEDC        OREQ      ' ?'
     C*
     C                   MOVE      *BLANK        QQKEY
     C                   MOVE      *BLANK        QQRTC
     C                   CALL      'C2011R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     QQKEY         SCDEDC
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN70
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN71
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C                   END
     C*
     C*  FUNCTION 12 = ITEM MAINTENANCE - SCREEN 4
     C*
     C     *INKB         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C*
     C     SCCVBO        IFEQ      '04'
     C     SCCVBO        OREQ      '10'
     C     SCCVBO        OREQ      '11'
     C     SCCVBO        OREQ      ' 4'
     C*
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVEL     SCPOL#        QQPOL#
     C                   MOVEL     SCCC          QQCC
     C                   MOVE      SCLOC#        QQLOC#
     C                   MOVE      SCPRM#        QQPRM#
     C                   MOVE      SCCVBO        QQCVBO
     C                   MOVE      SCTRDT        QQTRDT
     C                   CALL      'C0115R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCC
     C                   PARM                    QQLOC#
     C                   PARM                    QQPRM#
     C                   PARM                    QQCVBO
     C                   PARM                    QQTRDT
     C*
     C*   GET EXPOSURE
     C*
     C                   Z-ADD     0             XXEXP             8 0
     C     IIKEY         CHAIN     PII01                              50
     C     *IN50         DOWEQ     '0'
     C     IISTAT        IFEQ      'A'
     C                   ADD       IIVAL         XXEXP
     C                   END
     C     IIKEY         READE     PII01                                  50
     C                   END
     C                   Z-ADD     XXEXP         SCEXP
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*
     C     SCCVBO        IFEQ      '03'
     C     SCCVBO        OREQ      ' 3'
     C*
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVEL     SCPOL#        QQPOL#
     C                   MOVEL     SCCC          QQCC
     C                   MOVE      SCLOC#        QQLOC#
     C                   MOVE      SCPRM#        QQPRM#
     C                   MOVE      SCCVBO        QQCVBO
     C                   MOVE      SCTRDT        QQTRDT
     C                   CALL      'C0116R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQCC
     C                   PARM                    QQLOC#
     C                   PARM                    QQPRM#
     C                   PARM                    QQCVBO
     C                   PARM                    QQTRDT
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C*
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
     C     Q2RECF        IFEQ      'SCR04'
     C     *IN40         ANDEQ     '0'
     C*
     C     SCSTA4        IFEQ      'A'
     C                   MOVE      'I'           SCSTA4
     C                   ELSE
     C                   MOVE      'A'           SCSTA4
     C                   END
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR05'
     C     *IN41         ANDEQ     '0'
     C*
     C     SCSTA5        IFEQ      'A'
     C                   MOVE      'I'           SCSTA5
     C                   ELSE
     C                   MOVE      'A'           SCSTA5
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
     C     Q2RECF        CABEQ     'SCR05'       SCR05T
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
     C*   SET INDICATOR TO DISPLAY POLICY CANCELLED IF CANCELATION
     C*   FLAG(PMCANC) IS EQUAL TO "C"
     C*
     C     PMCANC        IFEQ      'C'
     C                   MOVE      '1'           *IN03
     C                   ELSE
     C                   MOVE      '0'           *IN03
     C                   END
     C*
     C*
     C*  COVERAGE CODE MUST BE - VALID CODE, BUS.OWNER, & ACTIVE
     C*
     C     SCCC          IFNE      *BLANK
     C*
     C                   Z-ADD     1             X                 2 0
     C     SCCC          LOOKUP    SC(X)                                  22
     C     *IN22         IFNE      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCCC          XXKEY
     C     TBKEY         CHAIN     PCV01                              60
     C*
     C     CVVAL         IFNE      'O'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C     CVSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEL     CVDESC        SCCCDS
     C                   MOVEL     PMFIN         SCFIN
     C*
     C*  ALL EDITS PASSED ON SCR01 GO TO POLICY FORM MASTER AND LOAD
     C*
     C     PCKEY         CHAIN     PPC01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PPC01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      PCFR01        SCFR01
     C                   MOVE      PCFR02        SCFR02
     C                   MOVE      PCFR03        SCFR03
     C                   MOVE      PCFR04        SCFR04
     C                   MOVE      PCFR05        SCFR05
     C                   MOVE      PCFR06        SCFR06
     C                   MOVE      PCFR07        SCFR07
     C                   MOVE      PCFR08        SCFR08
     C                   MOVE      PCFR09        SCFR09
     C                   MOVE      PCFR10        SCFR10
     C                   MOVE      PCCMNR        SCAGPC
     C                   MOVE      PCREIN        SCREIN
     C                   MOVE      PCRTID        SCRTID
     C                   MOVE      PCRTMF        SCRTMD
     C                   MOVE      PCRTDP        SCRTDP
     C                   MOVE      PCMINP        SCMNPR
     C                   MOVE      PCASLB        SCASLB
     C                   MOVE      PCSUBL        SCSUBL
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN91
     C                   GOTO      STRT
     C                   END
     C*
     C*   CHAIN OUT TO POLICY MASTER FOR COMMISSION RATE
     C*
     C     PMKEY         CHAIN     PPM01                              60
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANK        SCFR01
     C                   MOVE      *BLANK        SCFR02
     C                   MOVE      *BLANK        SCFR03
     C                   MOVE      *BLANK        SCFR04
     C                   MOVE      *BLANK        SCFR05
     C                   MOVE      *BLANK        SCFR06
     C                   MOVE      *BLANK        SCFR07
     C                   MOVE      *BLANK        SCFR08
     C                   MOVE      *BLANK        SCFR09
     C                   MOVE      *BLANK        SCFR10
     C                   Z-ADD     PMCMNR        SCAGPC
     C                   MOVE      *BLANK        SCREIN
     C                   MOVE      *BLANK        SCRTID
     C                   MOVE      *BLANK        SCRTMD
     C                   MOVE      *BLANK        SCRTDP
     C                   MOVE      'N'           SCMNPR
     C                   MOVE      *BLANK        SCASLB
     C                   MOVE      *BLANK        SCSUBL
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR02T        TAG
     C*
     C     *IN09         CABEQ     '1'           SCR02B
     C*
     C*   COMMISSION RATE MUST BE FROM 0.01 THRU 999.99
     C*
     C     SCAGPC        IFLT      0.01
     C     SCAGPC        ORGT      999.99
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*   RATE DEPARTURE MUST BE FROM 1 THRU 999
     C*
     C     SCRTDP        IFLT      1
     C     SCRTDP        ORGT      999
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*   RATE MOD MUST BE FROM 1 THRU 999
     C*
     C     SCRTMD        IFLT      1
     C     SCRTMD        ORGT      999
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*   MINIMUM PREMIUM MUST BE Y OR N.
     C*
     C     SCMNPR        IFNE      'Y'
     C     SCMNPR        ANDNE     'N'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*   SUBLINE MUST BE 915 NEW OR 910 OLD
     C*
     C     SCSUBL        IFNE      '915'
     C     SCSUBL        ANDNE     '910'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE ANNUAL STATEMENT LINE OF BUSINESS DEPENDING
     C*   UPON VALUE OF SECTION CODE
     C*
     C*   BUSINESS OWNERS SECTION
     C*
     C     SCASLB        IFNE      '050'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     PCKEY         CHAIN     PPC01                              40
     C                   MOVE      UPCO#         PCCO#
     C                   MOVE      SCPOL#        PCPOL#
     C                   MOVE      SCCC          PCCC
     C                   MOVE      SCFR01        PCFR01
     C                   MOVE      SCFR02        PCFR02
     C                   MOVE      SCFR03        PCFR03
     C                   MOVE      SCFR04        PCFR04
     C                   MOVE      SCFR05        PCFR05
     C                   MOVE      SCFR06        PCFR06
     C                   MOVE      SCFR07        PCFR07
     C                   MOVE      SCFR08        PCFR08
     C                   MOVE      SCFR09        PCFR09
     C                   MOVE      SCFR10        PCFR10
     C                   MOVE      SCAGPC        PCCMNR
     C                   MOVE      *BLANK        PCREIN
     C                   MOVE      *BLANK        PCRTID
     C                   MOVE      SCRTMD        PCRTMF
     C                   MOVE      SCRTDP        PCRTDP
     C                   MOVE      SCMNPR        PCMINP
     C                   MOVE      SCASLB        PCASLB
     C                   MOVE      SCSUBL        PCSUBL
     C                   MOVE      *BLANKS       PCSTAT
     C                   MOVE      Q1USER        PCUSER
     C                   MOVE      Q1PGM         PCPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PCDT
     C                   Z-ADD     DSTIME        PCTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PPC01
     C                   ELSE
     C                   WRITE     PPC01
     C                   END
     C*
     C     SCR02B        TAG
     C*
     C     SCSUBL        IFEQ      '915'
     C                   MOVE      '1'           *IN80
     C                   ELSE
     C                   MOVE      '0'           *IN80
     C                   END
     C*
     C                   MOVE      *BLANK        SCLOC#
     C                   MOVE      *BLANK        SCPRM#
     C                   MOVE      *BLANK        SCCVBO
     C                   MOVE      *BLANK        SCTRNS
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C*
     C*   PROCESS SCREEN 3
     C*
     C     SCR03T        TAG
     C*
     C*   IF ALL FIELDS ARE BLANK ON PROMPT SCREEN(SCR03) RETURN TO
     C*   SCR01 OR POLICY MASTER MAINTENANCE.
     C*
     C     SCLOC#        IFEQ      *ZERO
     C     SCPRM#        ANDEQ     *ZERO
     C     SCCVBO        ANDEQ     *BLANK
     C     SCTRNS        ANDEQ     *BLANK
     C*
     C     *IN76         IFEQ      '1'                                          GO BACK TO POL.
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
     C* Chain to find out if record exists; if the record exists
     C* do not edit screen information.  User wants to see data that
     C* may have an inactive  codes (data entered prior to inactivating
     C* codes)
     C*
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C     BOKEY         CHAIN     PBO01                              40
     C     *IN40         IFNE      '0'
     C*
     C*   DON'T EDIT LOCATION OR BUILDING IF COVERAGE CODE IS "19" OR " 9"
     C*
     C     SCCVBO        IFNE      '19'
     C     SCCVBO        ANDNE     ' 9'
     C*
     C*   VALIDATE LOCATION & PREMISES NUMBER
     C*
     C     LOKEY         CHAIN     PLO01                              20
     C     *IN20         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C     LOSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   VALIDATE COVERAGE CODE
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCCVBO        QQKEY
     C                   MOVE      'V'           QQRTC             1
     C                   CALL      'C0782R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*   CHECK FOR UNAVAILBLE CODES "12" AND " A"
     C*
     C     SCCVBO        IFEQ      '12'
     C     SCCVBO        OREQ      ' A'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE COVERAGE CODE FOR SUBLINE
     C*
     C     SCSUBL        IFEQ      '910'
     C     SCCVBO        IFNE      ' 1'
     C     SCCVBO        ANDNE     ' 2'
     C     SCCVBO        ANDNE     ' 8'
     C     SCCVBO        ANDNE     ' 3'
     C     SCCVBO        ANDNE     ' 4'
     C     SCCVBO        ANDNE     ' 5'
     C     SCCVBO        ANDNE     ' 6'
     C     SCCVBO        ANDNE     ' 7'
     C     SCCVBO        ANDNE     ' 9'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE
     C*
     C     SCCVBO        IFNE      '01'
     C     SCCVBO        ANDNE     '02'
     C     SCCVBO        ANDNE     '21'
     C     SCCVBO        ANDNE     '22'
     C     SCCVBO        ANDNE     '23'
     C     SCCVBO        ANDNE     '24'
     C     SCCVBO        ANDNE     '08'
     C     SCCVBO        ANDNE     '16'
     C     SCCVBO        ANDNE     '03'
     C     SCCVBO        ANDNE     '04'
     C     SCCVBO        ANDNE     '05'
     C     SCCVBO        ANDNE     '06'
     C     SCCVBO        ANDNE     '07'
     C     SCCVBO        ANDNE     '10'
     C     SCCVBO        ANDNE     '17'
     C     SCCVBO        ANDNE     '16'
     C     SCCVBO        ANDNE     '13'
     C     SCCVBO        ANDNE     '14'
     C     SCCVBO        ANDNE     '15'
     C     SCCVBO        ANDNE     '19'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  TRANSACTION CODE MUST BE VALID AND ACTIVE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCTRNS        XXKEY
     C     TBKEY         CHAIN     PTT01                              25
     C     *IN25         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C     TTSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*  TRANS. DATE MUST BE A VALID DATE & BETWEEN POL. FROM & TO DATE
     C*
     C     SCTRDT        IFEQ      *ZERO
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   ELSE
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTRDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN27
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
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C                   ELSE                                                   ELSE FOR BYPASS
     C*
     C*  GET TRANSACTION CODE TYPE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCTRNS        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C                   END
     C*
     C*   ALL EDITS PASSED, GET THE REQUESTED RECORD, LOAD INFORMATION
     C*   IF THERE IS NO HIT ON THE EXACT INFO, GET THE PREVIOUS DATES
     C*   INFO.
     C*
     C*   GET BUSINESSOWNER LIABILITY MASTER FILE FOR BUILDING OR CONTENTS CODES
     C*
     C     SCCVBO        IFEQ      '01'
     C     SCCVBO        OREQ      '02'
     C     SCCVBO        OREQ      '23'
     C     SCCVBO        OREQ      '24'
     C     SCCVBO        OREQ      '13'
     C     SCCVBO        OREQ      '14'
     C     SCCVBO        OREQ      '19'
     C     SCCVBO        OREQ      ' 1'
     C     SCCVBO        OREQ      ' 2'
     C     SCCVBO        OREQ      ' 9'
     C*
     C     BLKEY         CHAIN     PBL01                              41
     C*
     C     *IN41         IFEQ      '1'
     C*
     C*   INDICATOR TO CONTROL CHANGING STATUS
     C*
     C                   MOVE      '1'           *IN62
     C*
     C*   VALIDATE THAT TRANS DATE IS EQUAL TO USER PARM PROCESS DATE
     C*   IF ADDING A RECORD OF COVERAGE
     C*
     C****       XXTRDT    IFNE UPPRDT
     C****                 MOVE '1'       *IN29
     C****                 GOTO STRT
     C****                 END
     C*
     C*  CASE MUST ALREADY EXIST IF THIS IS IN INQUIRE MODE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN91
     C                   GOTO      STRT
     C                   END
     C*
     C     BLKY1         SETGT     PBL02
     C                   MOVE      'I'           BLSTAT
     C                   MOVE      '0'           *IN41
     C     BLSTAT        DOWEQ     'I'
     C     *IN41         ANDEQ     '0'
     C                   READP     PBL02                                  41
     C     BLCO#         IFNE      UPCO#
     C     BLPOL#        ORNE      SCPOL#
     C     BLCC          ORNE      SCCC
     C                   MOVE      *ON           *IN41
     C                   END
     C                   END
     C*       SET MODE TO 'ADD'. CLEAR PREMIUM FIELDS & MOVE CORRERT
     C*       END# INTO BOEND# FOR ADD MODE.
     C                   MOVE      UA(1)         SCMOD5
     C                   Z-ADD     PMNEND        BLEND#
     C*
     C                   ELSE
     C                   MOVE      '0'           *IN62
     C                   EXCEPT    PBL01E
     C                   MOVE      UA(2)         SCMOD5
     C                   END
     C*
     C     *IN41         IFEQ      '0'
     C*
     C                   MOVE      BLLFMC        SCLFMC
     C                   MOVE      BLLIMT        SCLIMT
     C                   MOVE      BLDEDC        SCDEDC
     C                   Z-ADD     BLRODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRODT
     C                   MOVE      BLCMMD        SCCMMD
     C                   MOVE      BLFDML        SCFDML
     C                   MOVE      BLGAG         SCGAG
     C                   MOVE      BLSTAT        SCSTA5
     C                   Z-ADD     BLEND#        SCEND5
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       SCLFMC
     C                   MOVE      *BLANKS       SCLIMT
     C                   MOVE      *BLANKS       SCDEDC
     C                   MOVE      *BLANKS       SCRODT
     C                   MOVE      *BLANKS       SCCMMD
     C                   MOVE      *BLANKS       SCFDML
     C                   MOVE      *BLANKS       SCGAG
     C                   MOVE      'A'           SCSTA5
     C     TTTYPE        IFEQ      'E'
     C                   Z-ADD     PMNEND        SCEND5
     C                   ELSE
     C                   Z-ADD     0             SCEND5
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*
     C*   DON'T GET MASTER FILE IF ONLY UPDATING LIABILITY SCREEN
     C*
     C     SCCVBO        IFNE      '19'
     C     SCCVBO        ANDNE     ' 9'
     C*
     C*   GET BUSINESSOWNER MASTER FILE
     C*
     C     BOKEY         CHAIN     PBO01                              40
     C*
     C     *IN40         IFEQ      '1'
     C*
     C*   INDICATOR TO CONTROL CHANGING STATUS
     C*
     C                   MOVE      '1'           *IN61
     C*
     C*   VALIDATE THAT TRANS DATE IS EQUAL TO USER PARM PROCESS DATE
     C*   IF ADDING A RECORD OF COVERAGE
     C*
     C****       XXTRDT    IFNE UPPRDT
     C****                 MOVE '1'       *IN29
     C****                 GOTO STRT
     C****                 END
     C*
     C*  CASE MUST ALREADY EXIST IF THIS IS IN INQUIRE MODE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN91
     C                   GOTO      STRT
     C                   END
     C*
     C     BOKY1         SETGT     PBO02
     C                   MOVE      'I'           BOSTAT
     C                   MOVE      '0'           *IN40
     C     BOSTAT        DOWEQ     'I'
     C     *IN40         ANDEQ     '0'
     C                   READP     PBO02                                  40
     C     BOCO#         IFNE      UPCO#
     C     BOPOL#        ORNE      SCPOL#
     C     BOCC          ORNE      SCCC
     C     BOLOC#        ORNE      SCLOC#
     C     BOPRM#        ORNE      SCPRM#
     C     BOCVBO        ORNE      SCCVBO
     C                   MOVE      *ON           *IN40
     C                   END
     C                   END
     C*       SET MODE TO 'ADD'. CLEAR PREMIUM FIELDS & MOVE CORRERT
     C*       END# INTO BOEND# FOR ADD MODE.
     C                   MOVE      UA(1)         SCMOD4
     C                   MOVE      *BLANKS       BOPREM
     C                   Z-ADD     PMNEND        BOEND#
     C*
     C                   ELSE
     C                   MOVE      '0'           *IN61
     C                   EXCEPT    PBO01E
     C                   MOVE      UA(2)         SCMOD4
     C                   END
     C*
     C     *IN40         IFEQ      '0'
     C*
     C                   MOVE      BOTERR        SCTERR
     C                   MOVE      BOCLAS        SCCLAS
     C                   MOVE      BOEXP         SCEXP
     C                   MOVE      BOCOIN        SCCOIN
     C                   MOVE      BODECD        SCDECD
     C                   MOVE      BORTBO        SCRTBO
     C                   MOVE      BOREIN        SCREIN
     C                   MOVE      BORTID        SCRTID
     C                   MOVE      BOAINC        SCAINC
     C                   MOVE      BOPREM        SCPREM
      *
     C                   Z-ADD     BOACMY        DSACMY
     C                   MOVEL     DSACMM        SCACMM
     C                   Z-ADD     DSACYY        SCACYY
     C     DSACCN        IFEQ      1
     C                   ADD       2000          SCACYY
     C                   ELSE
     C                   ADD       1900          SCACYY
     C                   END
      *
     C                   MOVE      BOEXPB        SCEXPB
     C                   MOVE      BOSTAT        SCSTA4
     C                   Z-ADD     BOEND#        SCEND4
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       SCTERR
     C                   MOVE      *BLANKS       SCCLAS
     C                   MOVE      *BLANKS       SCEXP
     C                   MOVE      *BLANKS       SCCOIN
     C                   MOVE      *BLANKS       SCDECD
     C                   MOVE      *BLANKS       SCRTBO
     C                   MOVE      *BLANKS       SCREIN
     C                   MOVE      *BLANKS       SCRTID
     C                   MOVE      *BLANKS       SCAINC
     C                   MOVE      *BLANKS       SCPREM
     C                   Z-ADD     *YEAR         SCACYY
     C                   MOVEL     UMONTH        SCACMY
     C                   MOVE      *BLANK        SCEXPB
     C                   MOVE      'A'           SCSTA4
     C     TTTYPE        IFEQ      'E'
     C                   Z-ADD     PMNEND        SCEND4
     C                   ELSE
     C                   Z-ADD     0             SCEND4
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*   SET INDICATIOR ACCORDING TO TYPE OF COVERAGE
     C*
     C     SCCVBO        IFEQ      '04'
     C     SCCVBO        OREQ      '10'
     C     SCCVBO        OREQ      '11'
     C     SCCVBO        OREQ      ' 4'
     C                   MOVE      '1'           *IN81
     C                   ELSE
     C                   MOVE      '0'           *IN81
     C                   END
     C*
     C     SCCVBO        IFEQ      '03'
     C     SCCVBO        OREQ      ' 3'
     C                   MOVE      '1'           *IN82
     C                   ELSE
     C                   MOVE      '0'           *IN82
     C                   END
     C*
     C     SCCVBO        IFEQ      '08'
     C     SCCVBO        OREQ      '05'
     C     SCCVBO        OREQ      '16'
     C     SCCVBO        OREQ      '17'
     C     SCCVBO        OREQ      ' 8'
     C     SCCVBO        OREQ      ' 5'
     C                   MOVE      '1'           *IN83
     C                   ELSE
     C                   MOVE      '0'           *IN83
     C                   END
     C*
     C     SCCVBO        IFEQ      '06'
     C     SCCVBO        OREQ      '07'
     C     SCCVBO        OREQ      '15'
     C     SCCVBO        OREQ      ' 6'
     C     SCCVBO        OREQ      ' 7'
     C                   MOVE      '1'           *IN84
     C                   ELSE
     C                   MOVE      '0'           *IN84
     C                   END
     C*
     C     SCCVBO        IFEQ      '13'
     C     SCCVBO        OREQ      '14'
     C                   MOVE      '1'           *IN85
     C                   ELSE
     C                   MOVE      '0'           *IN85
     C                   END
     C*
     C     SCCVBO        IFEQ      '21'
     C     SCCVBO        OREQ      '22'
     C     SCCVBO        OREQ      '23'
     C     SCCVBO        OREQ      '24'
     C                   MOVE      '1'           *IN86
     C                   ELSE
     C                   MOVE      '0'           *IN86
     C                   END
     C*
     C*
     C                   MOVE      *BLANK        Q2RECF
     C     SCCVBO        IFEQ      '19'
     C     SCCVBO        OREQ      ' 9'
     C                   MOVEL     'SCR05'       Q2RECF
     C                   ELSE
     C                   MOVEL     'SCR04'       Q2RECF
     C                   END
     C*
     C                   GOTO      STRT
     C*
     C     SCR04T        TAG
     C*
     C     *IN09         CABEQ     '1'           SCR04B
     C*
     C*   IF NOT BUILDING OR CONTENTS CODES SKIP EDITS
     C*
     C     SCCVBO        IFEQ      '01'
     C     SCCVBO        OREQ      '02'
     C     SCCVBO        OREQ      '21'
     C     SCCVBO        OREQ      '22'
     C     SCCVBO        OREQ      '23'
     C     SCCVBO        OREQ      '24'
     C     SCCVBO        OREQ      ' 1'
     C     SCCVBO        OREQ      ' 2'
     C*
     C*   VALIDATE TERRITORY CODE
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCTERR        QQKEY
     C                   MOVE      'V'           QQRTC             1
     C                   CALL      'C0730R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE CLASS CODE
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      SCCLAS        QQKEY
     C                   MOVE      'V'           QQRTC             1
     C                   CALL      'C0746R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE RATING BASIS
     C*
     C     SCCVBO        IFNE      '23'
     C     SCCVBO        ANDNE     '24'
     C*
     C     SCRTBO        IFNE      '1'
     C     SCRTBO        ANDNE     '2'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   VALIDATE REINSURANCE FLAG
     C*
     C     SCREIN        IFNE      *BLANK
     C     SCREIN        ANDNE     'F'
     C     SCREIN        ANDNE     'T'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE RATING BASIS
     C*
     C     SCCVBO        IFNE      '23'
     C     SCCVBO        ANDNE     '24'
     C*
     C     SCRTID        IFNE      '4'
     C     SCRTID        ANDNE     '5'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*   VALIDATE EXPOSURE BASIS
     C*
     C     SCCVBO        IFEQ      '23'
     C     SCCVBO        OREQ      '24'
     C*
     C     SCEXPB        IFNE      '4'
     C     SCEXPB        ANDNE     '5'
     C     SCEXPB        ANDNE     '6'
     C     SCEXPB        ANDNE     '9'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  VALIDATE DEDUCTIBLE CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCDECD        XXKEY
     C     TBKEY         CHAIN     PDE01                              28
     C     *IN28         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C     DESTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C*   MONTH/YEAR MUST BE VALID
     C*
     C                   MOVEL     SCACMY        XXMNTH            2 0
     C                   MOVE      SCACMY        XXYEAR            4 0
     C     XXMNTH        IFLT      1
     C     XXMNTH        ORGT      12
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C     XXYEAR        IFLT      1
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     BOKEY         CHAIN     PBO01                              40
     C                   MOVE      UPCO#         BOCO#
     C                   MOVE      SCPOL#        BOPOL#
     C                   MOVE      SCCC          BOCC
     C                   MOVE      SCLOC#        BOLOC#
     C                   MOVE      SCPRM#        BOPRM#
     C                   MOVE      SCCVBO        BOCVBO
     C                   MOVE      SCTRNS        BOTRNS
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BOTRDT
     C                   MOVE      SCEND4        BOEND#
     C                   MOVE      SCTERR        BOTERR
     C                   MOVE      SCCLAS        BOCLAS
     C                   MOVE      SCEXP         BOEXP
     C                   MOVE      SCCOIN        BOCOIN
     C                   MOVE      SCDECD        BODECD
     C                   MOVE      SCRTBO        BORTBO
     C                   MOVE      SCREIN        BOREIN
     C                   MOVE      SCRTID        BORTID
     C                   MOVEL     SCAINC        BOAINC
     C                   MOVE      SCPREM        BOPREM
      *
     C                   CLEAR                   DSACMY
     C                   MOVE      SCACYY        DSACYY
     C                   MOVEL     SCACMM        DSACMM
     C     SCACYY        IFGT      1999
     C                   MOVEL     1             DSACCN
     C                   ELSE
     C                   MOVEL     0             DSACCN
     C                   END
     C                   Z-ADD     DSACMY        BOACMY
     C                   MOVE      SCEXPB        BOEXPB
     C                   MOVE      SCSTA4        BOSTAT
     C                   MOVE      Q1USER        BOUSER
     C                   MOVE      Q1PGM         BOPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BODT
     C                   Z-ADD     DSTIME        BOTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PBO01
     C                   MOVE      'U'           QQUPAD            1
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BORCDT
     C                   MOVE      'A'           BOSTAT
     C                   Z-ADD     *ZEROS        BOBUL3
     C                   WRITE     PBO01
     C                   MOVE      'A'           QQUPAD
     C                   END
     C*
     C*   CALL PREMIUM BEARING ENDORSEMENT MAINTENANCE IF AN ENDORSEMENT RECORD
     C*
     C     SCCVBO        IFNE      '01'
     C     SCCVBO        ANDNE     '02'
     C     SCCVBO        ANDNE     '23'
     C     SCCVBO        ANDNE     '24'
     C     SCCVBO        ANDNE     ' 1'
     C     SCCVBO        ANDNE     ' 2'
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCTRNS        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C     TTTYPE        IFEQ      'E'
     C                   Z-ADD     BOPREM        XXAMT             8 0
     C*
     C                   MOVE      SCPOL#        QQPOL#
     C                   MOVE      SCCC          QQCC
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
     C                   END
     C*
     C*
     C     SCR04B        TAG
     C*
     C*
     C     SCCVBO        IFEQ      '01'
     C     SCCVBO        OREQ      '02'
     C                   MOVE      '19'          SCCVB5
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C                   ELSE
     C     SCCVBO        IFEQ      '23'
     C                   MOVE      '23'          SCCVB5
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C                   ELSE
     C     SCCVBO        IFEQ      '24'
     C                   MOVE      '24'          SCCVB5
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C                   ELSE
     C     SCCVBO        IFEQ      ' 1'
     C     SCCVBO        OREQ      ' 2'
     C                   MOVE      ' 9'          SCCVB5
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C                   ELSE
     C                   MOVE      *BLANKS       SCLOC#
     C                   MOVE      *BLANKS       SCPRM#
     C                   MOVE      *BLANK        SCCVBO
     C                   MOVE      *BLANKS       SCTRNS
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C                   GOTO      STRT
     C*
     C*
     C     SCR05T        TAG
     C*
     C     *IN09         CABEQ     '1'           SCR05B
     C*
     C*   LIABILITY FORMS CODE MUST BE '1', '3' OR '4'.
     C*
     C     SCLFMC        IFNE      '1'
     C     SCLFMC        ANDNE     '3'
     C     SCLFMC        ANDNE     '4'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*   LIMIT CANNOT BE LESS THAN ZERO
     C*
     C     SCLIMT        IFLT      *ZERO
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE DEDUCTIBLE CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCDEDC        XXKEY
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
     C*   VALIDATE RETRO DATE
     C*
     C     SCRODT        IFNE      *ZERO
     C                   Z-ADD     SCRODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRODT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   FIRE DAMAGE LIMIT CANNOT BE LESS THAN ZERO
     C*
     C     SCFDML        IFLT      *ZERO
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*   FIRE DAMAGE LIMIT CANNOT BE LESS THAN ZERO
     C*
     C     SCFDML        IFLT      *ZERO
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*   GENERAL AGGAGATE CANNOT BE LESS THAN ZERO
     C*
     C     SCGAG         IFLT      *ZERO
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     BLKEY         CHAIN     PBL01                              40
     C                   MOVE      UPCO#         BLCO#
     C                   MOVE      SCPOL#        BLPOL#
     C                   MOVE      SCCC          BLCC
     C                   MOVE      SCTRNS        BLTRNS
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BLTRDT
     C                   MOVE      SCEND5        BLEND#
     C                   MOVE      SCLFMC        BLLFMC
     C                   MOVE      SCLIMT        BLLIMT
     C                   MOVE      SCDEDC        BLDEDC
     C                   Z-ADD     SCRODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BLRODT
     C                   MOVEL     SCCMMD        BLCMMD
     C                   MOVE      SCFDML        BLFDML
     C                   MOVE      SCGAG         BLGAG
     C                   MOVE      SCSTA5        BLSTAT
     C                   MOVE      Q1USER        BLUSER
     C                   MOVE      Q1PGM         BLPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BLDT
     C                   Z-ADD     DSTIME        BLTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PBL01
     C*
     C*
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BLRCDT
     C                   MOVE      'A'           BLSTAT
     C                   Z-ADD     *ZEROS        BLBUL3
     C                   WRITE     PBL01
     C*
     C*
     C                   END
     C*
     C*   CALL PREMIUM BEARING ENDORSEMENT MAINTENANCE IF AN ENDORSEMENT RECORD
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCTRNS        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C     TTTYPE        IFEQ      'E'
     C                   Z-ADD     BOPREM        XXAMT             8 0
     C*
     C                   MOVE      SCPOL#        QQPOL#
     C                   MOVE      SCCC          QQCC
     C                   MOVE      SCTRDT        QQTRDT            8
     C                   MOVE      SCEND5        QQEND#            3
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
     C     SCR05B        TAG
     C*
     C                   MOVE      *BLANKS       SCLOC#
     C                   MOVE      *BLANKS       SCPRM#
     C                   MOVE      *BLANK        SCCVBO
     C                   MOVE      *BLANKS       SCTRNS
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*
     OPBO01     E            PBO01E
     OPBL01     E            PBL01E
     OPPC01     E            PPC01E
     O*
**  TABLE UA
-- ADD --
- UPDATE -
