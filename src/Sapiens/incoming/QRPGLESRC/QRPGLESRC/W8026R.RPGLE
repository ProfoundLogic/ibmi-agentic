     H*copyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H*StoneRiver is a trademark of StoneRiver, Inc.')
     H*
     H/TITLE W8026R - Collateral Dissolve prompt
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*   This prompt program to invoke Collateral Dissolve prompt.   *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   NONE                                                        *
     H* CALLS TO:                                                     *
     H*   W8026C                                                      *
     H* PROGRAMMER: C:232                                             *
     H* CREATED: 01/25/10               PROJECT: WC1851               *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*   *IN60 - GENERAL CHAIN, DOWEQ                                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW8026F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FWTABL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D XXPRDS                        40
      *
      *=============================================================
      *  KLIST
      *=============================================================
      *
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST                                                          SSSS
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
      *
      *===========================================================
      *  *LIKE DEFN
      *===========================================================
      *
      *===========================================================
      *  PLIST
      *===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQMODE            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *===========================================================
      *  MAIN
      *===========================================================
      *
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL44        IFNE      'Y'
     C                   GOTO      TERM
     C                   ENDIF
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
     C
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER           10
     C                   EVAL      XXPRDS = '      Collateral Dissolve         '
     C                   EVAL      SCPRDS = '    ' + XXPRDS
     C                   MOVE      *BLANK        Q2RECF           10
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C
     C*
     C*  COMMAND 03 AND 12 FOR END OF JOB..
     C*
     C     *INKC         IFEQ      '1'
     C     *INKL         OREQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*                  MOVEL     SCKEY         SCCOM1
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           TERM
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*
     C*  PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
      *
      * VALIDATION STARTE HERE---
      *
      * No Open Claims as of date must be valid
     C*
     C     SCOCDT        IFNE      *ZEROS
      *
     C                   Z-ADD     SCOCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCOCDT
     C
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN62
     C                   MOVE      '1'           *IN76
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDIF
     C*
      *
     C*  No Claims Activity date must be Valid...
     C*
     C     SCNADT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCNADT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCNADT
     C
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   MOVE      '1'           *IN77
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
     C     SCOCDT        IFEQ      *ZEROS
     C     SCNADT        ANDEQ     *ZEROS
     C*
     C                   MOVE      '1'           *IN63
     C                   MOVE      '1'           *IN76
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C* REPORT ONLY FLAG MUST BE VALID...
      *
     C     SCROFL        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN30
     C                   MOVE      '1'           *IN44
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     SCROFL        IFNE      'Y'
     C     SCROFL        ANDNE     'N'
     C                   MOVE      '1'           *IN29
     C                   MOVE      '1'           *IN44
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCOCDT        QQOCDT            8
     C                   MOVE      SCNADT        QQNADT            8
     C                   MOVE      SCROFL        QQROFL            1
     C                   MOVE      UPOUTQ        QQOUTQ           10
      *
     C*
     C                   CALL      'W8026C'
     C                   PARM                    QQCO#
     C                   PARM                    QQOCDT
     C                   PARM                    QQNADT
     C                   PARM                    QQROFL
     C                   PARM                    QQOUTQ
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      *BLANKS       QQPRDS
     C                   MOVE      '1'           *INLR
     C*
