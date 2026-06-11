     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H****** IS202R1 - PRINT/FAX  SELECTION SCREEN PGM            *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  21    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  22    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  23    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  24    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  25    INVALID RECIPIENT ID ENTERED                           *
     H*****************************************************************
     FIS202F1   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMFRL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F*
     FSMUPL     IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D                 DS
     D  XXTIME                 1      6  0
     D  XXMIN                  3      4  0
     D  XXSEC                  5      6  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     FRKY          KLIST
     C                   KFLD                    SCRCID
     C*
      *
      *  USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
     C*
     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQSELC            1
     C                   PARM                    QQRECP           10
     C                   PARM                    QQKEY            40
     C                   PARM                    QQSEQ             5
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM                    QQOUTQ           10
     C                   PARM                    QQCPYS            3
     C                   PARM                    QQRTCD            1
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
     C                   MOVEL     QQRECP        SCRCID
     C                   MOVEL     QQKEY         XXKEY            40
     C                   MOVE      QQSEQ         XXSEQ#            5 0
     C*
     C*** GET DEFAULT FOR OUTQ FIELD FROM USER PARMS *************
     C*
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     'QPRINT'      UPOUTQ
     C                   ENDIF
      *
     C                   MOVEL     UPOUTQ        SCOUTQ
     C*
     C*
     C                   Z-ADD     1             SCCPYS
     C*
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        XXTIME
     C*
     C     'SY'          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *ON
     C     SYFXIF        ORNE      'Y'
     C                   MOVEL     'P'           QQSELC
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*                    MOVELQQPRDS    SCPRDS
     C*
      *===============================================================
      * INITIALIZATION    AREA
      *
     C     TOP           TAG
     C                   MOVEL     'WDW01'       Q2RECF
      *
      *=============================================2=================
      *
     C     MAINLN        TAG
      *
      *===============================================================
      * SCREEN EXECUTION AREA
      *
     C                   EXSR      SCREXC
      *
      *===============================================================
      * HANDLE F KEYS IF PRESSED
      *
     C     *IN11         CASEQ     '1'           FKEYS
     C                   END                                                      END *IN11
      *
      *===============================================================
      *
     C     Q2RECF        CASEQ     'WDW01 '      WDW01T
     C                   ENDCS
      *
     C*
      *===============================================================
      * TERMINATION OF PROGRAM
      *
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
      *===============================================================
     C     SCREXC        BEGSR
      *===============================================================
      *
      *
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C                   EXFMT     WDW01
     C                   END
     C*
     C*
     C*
     C                   ENDSR
      *
     C/EJECT
      *===============================================================
     C     FKEYS         BEGSR
      *===============================================================
      *
     C     *INKA         IFEQ      '1'
     C                   END
      *
     C     *INKB         IFEQ      '1'
     C                   END
      *
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD            1
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
      *
     C     *INKD         IFEQ      '1'
     C                   END
      *
      *****************************************
     C*   F5 = FAX RECIPIENT SEARCH           *
     C*****************************************
      *
     C     *INKE         IFEQ      '1'
     C*
     C*   PARM LIST FAX RECIPIENT ID SEARCH
     C*
     C     PL0510        PLIST
     C                   PARM                    SCRCID
     C                   PARM                    QQRTC             1
     C                   CALL      'I0510R'      PL0510
     C*
     C                   MOVEL     'WDW01'       Q2RECF
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCRCID        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           TOP
     C                   END
      *
      *
     C     *INKF         IFEQ      '1'
     C                   END
      *
      *
     C     *INKG         IFEQ      '1'
     C                   END
      *
      *
     C     *INKH         IFEQ      '1'
     C                   END
      *
      *
     C     *INKI         IFEQ      '1'
     C                   END
      *
      *
     C     *INKJ         IFEQ      '1'
     C                   END
      *
     C     *INKK         IFEQ      '1'
     C                   END
      *
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   END                                                    END *INKL
     C*
     C     *INKM         IFEQ      '1'
     C                   END
      *
     C     *INKN         IFEQ      '1'
     C                   END
      *
     C     *INKP         IFEQ      '1'
     C                   END
      *
     C     *INKQ         IFEQ      '1'
     C                   END
      *
     C     *INKR         IFEQ      '1'
     C                   END
      *
     C     *INKS         IFEQ      '1'
     C                   END
      *
     C     *INKT         IFEQ      '1'
     C                   END
      *
     C     *INKU         IFEQ      '1'
     C                   END
      *
     C     *INKV         IFEQ      '1'
     C                   END
      *
     C     *INKW         IFEQ      '1'
     C                   END
      *
     C     *INKX         IFEQ      '1'
     C                   END
      *
     C*
     C                   GOTO      MAINLN
      *
     C                   ENDSR
     C/EJECT
      *===================WDW01T======================================*
     C     WDW01T        BEGSR
     C**** EDIT FIELDS ENTERED HERE **************
     C*** 1. SELECT PRINT OR FAX
     C     SCSELP        IFEQ      *BLANK
     C     SCSELF        ANDEQ     *BLANK
     C                   MOVE      *ON           *IN21
     C                   GOTO      MAINLN
     C                   ENDIF
     C*
     C     SCSELP        IFNE      *BLANK
     C     SCSELF        ANDNE     *BLANK
     C                   MOVE      *ON           *IN22
     C                   GOTO      MAINLN
     C                   ENDIF
     C*
     C*** 2. VALID CHAR IN SELECT FIELDS
     C     SCSELP        IFNE      *BLANK
     C     SCSELP        ANDNE     '1'
     C                   MOVE      *ON           *IN23
     C                   GOTO      MAINLN
     C                   ENDIF
     C*
     C*
     C     SCSELF        IFNE      *BLANK
     C     SCSELF        ANDNE     '1'
     C                   MOVE      *ON           *IN24
     C                   GOTO      MAINLN
     C                   ENDIF
     C*
     C*** 3. IF RECIPIENT IS ENTERED, MUST BE VALID FROM FILE
     C*
     C     SCSELF        IFEQ      '1'
     C     SCRCID        IFNE      *BLANKS
     C     FRKY          CHAIN     PFR01                              60
     C     *IN60         CABEQ     *ON           MAINLN                   25
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** 4. IF PRINT, VALIDATE THE OUTQ AND THE COPIES
     C*
     C     SCSELP        IFEQ      '1'
     C     SCOUTQ        CABEQ     *BLANKS       MAINLN                   26
     C                   CALL      'SVALOBJ'
     C                   PARM                    SCOUTQ
     C                   PARM      '*OUTQ  '     ##OBTP            7
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     ##RTCD        CABEQ     '1'           MAINLN                   26
     C****
     C     SCCPYS        CABLE     *ZERO         MAINLN                 2727
     C*
     C                   ENDIF
     C*
     C*
     C*
     C**** LOAD VALUES TO RETURN WITH ***
     C                   MOVEL     SCOUTQ        QQOUTQ
     C                   MOVE      SCCPYS        QQCPYS
     C*
     C                   MOVEL     SCRCID        QQRECP
     C     SCSELP        IFNE      *BLANKS
     C                   MOVE      'P'           QQSELC
     C                   ENDIF
     C     SCSELF        IFNE      *BLANKS
     C                   MOVE      'F'           QQSELC
     C                   ENDIF
     C*
     C**** MS OFFICE INTEGRATION HERE ***********************************'
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C                   MOVEL     'SC'          XXSYK
     C     SYKY          CHAIN     SDR01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     DRFL06        ANDEQ     'Y'
     C                   CALL      'SCFXMSC'
     C                   PARM                    QQDCID           12
     C                   PARM                    XXRTN             1
     C     XXRTN         IFEQ      'Y'
     C                   MOVEL     'M'           QQSELC
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD MS OFFIC INTEGRATION *********
     C*** END OF THE PGM
     C*
     C                   GOTO      TERM
     C*
     C*
     C                   ENDSR
