     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H/TITLE F0072R - POST AUDIT/RECON PROCESSING                     *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR01:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/09/20  C:212 Project : SY420                              *
     H*                  Mask SSN                                     *
     H*                                                               *
     H*  04/08/99 C:56 Support: MC095 #17568                          *
     H*               1. Add CO#/SUB#/FND TO CALL OF F0575R           *
     H*****************************************************************
     FF0072F    CF   E             WORKSTN USROPN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(WDW01S:RRN2)
     F*
     FFMCDL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFAEOL4    IF   E           K DISK
     FFAE1L     IF   E           K DISK

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* INITIALIZE SECTION
     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C*
     C                   MOVE      *BLANKS       QQLAST           56
     C                   MOVE      *BLANKS       QQFRST           15
     C                   MOVE      *BLANKS       QQTERM            1
     C                   MOVE      *BLANKS       PCFL              1
     C                   MOVE      *BLANKS       XXAR              1
     C                   MOVE      *BLANKS       XXAUD             3
     C                   Z-ADD     *ZEROS        RRN2              5 0
     C                   Z-ADD     *ZEROS        SVRRN2            5 0
     C                   Z-ADD     *ZEROS        XXRRN2            5 0
     C                   Z-ADD     *ZEROS        Y                 5 0
     C*
     C* KEY LIST FOR THE EOMB DETAIL FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*
     C* KEY LIST FOR THE EOMB AUXILIARY FILE
     C*
     C     E1KEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     E1KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    XXEOB#
     C*
     C* KEY LIST FOR THE PROVIDER MASTER FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*
     C* CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    SCSS#
     C*
     C* PARAMETER LIST FOR CALLED PROGRAM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQEOB#            9
     C                   PARM                    QQSS#             9
     C                   PARM                    QQACDT            8
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQAR              1
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        SCSUB#
     C                   MOVE      QQFND         SCFND
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQCASE        SCCASE
     C                   MOVE      QQSS#         SCSS#
     C                   MOVE      QQACDT        SCACDT
     C                   MOVE      QQFEIN        SCFEIN
     C                   MOVE      QQMOD         SCMOD
     C*
     C* IF QQRTCD IS PASSES AS A 'V' THEN VERIFY EOMB SELECTED IS CORRECT
     C* FOR THE AUDIT OR RECON SEELCTED
     C*
     C     QQRTCD        IFEQ      'V'
     C*
     C                   MOVE      QQEOB#        XXEOB#
     C     E1KY1         CHAIN     PE101                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     9             Y
     C     1             SUBST     E120A1:Y      XXAR
     C*
     C     XXAR          IFNE      *BLANKS
     C*
     C     XXAR          IFNE      QQAR
     C*
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C*
     C                   ELSE                                                   ELSE XXAR NE QQAR
     C*
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C*
     C                   END                                                    END XXAR NE QQAR
     C*
     C                   ELSE                                                   ELSE XXAR NE *BLANKS
     C*
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C*
     C                   END                                                    END XXAR NE *BLANKS
     C*
     C                   ELSE                                                   ELSE *IN60 EQ '0'
     C*
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C*
     C                   END                                                    END 8IN60 EQ '0'
     C*
     C                   ELSE                                                   ELSE QQRTCD EQ 'V'
     C*
     C                   OPEN      F0072F
     C*
     C                   END                                                    END QQRTCD EQ 'V'
     C*
     C* GET CLAIMANT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST
     C                   MOVEL     CDFSN         QQFRST
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCCNAM
     C                   ELSE
     C                   MOVEL     *BLANKS       SCCNAM
     C                   END
     C*
     C* GET THE PROVIDER NAME
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PDNAME        QQLAST
     C                   MOVEL     PDFSN         QQFRST
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCPNAM
     C                   ELSE
     C                   MOVEL     *BLANKS       SCPNAM
     C                   END
     C*
     C* DETERMINE IF AUDIT OR RECON MESAAGE SHOULD BE DISPLAYED
     C*
     C     QQAR          IFEQ      'A'
     C*
     C                   MOVE      '1'           *IN30
     C*
     C                   ELSE                                                   ELSE QQAR EQ 'A'
     C*
     C     QQAR          IFEQ      'R'
     C*
     C                   MOVE      '0'           *IN30
     C*
     C                   END                                                    END QQAR EQ 'R'
     C                   END                                                    END QQAR EQ 'A'
     C*
     C* INITIALIZE SCREEN
     C*
     C     TOP           TAG
     C                   EXSR      INZSR
     C*
     C* MAIN LINE CODE
     C*
     C     MAINLN        TAG
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       scSS# = MaskData('SSN':'SCSS#':SCSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C* SCREEN EXECUTION AREA
     C*
     C                   EXSR      SCREXC
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       scSS# = UnMaskData('SSN':'SCSS#':SCSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C* HANDLE F KEYS IF PRESSED
     C*
     C     *IN11         CASEQ     '1'           FKEYS
     C                   END                                                      END *IN11
     C*
     C                   Z-ADD     RRN2          SVRRN2
     C*
     C                   READC     WDW01S                                 50
     C     *IN50         IFEQ      '0'
     C* IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C* SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
     C     SCSEL         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      SCEOB#        QQEOB#
     C                   GOTO      TERM
     C*
     C                   ELSE                                                   ELSE EQ '1'
     C*
     C     SCSEL         IFEQ      '5'
     C                   MOVE      SCFYR         Q1FYR
     C                   MOVE      SCCASE        Q1CASE
     C                   MOVE      SCSS#         Q1SS#
     C                   MOVE      SCACDT        Q1ACDT
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   MOVE(P)   XXCO#         Q1CO#
     C                   MOVE(P)   SCSUB#        Q1SUB#
     C                   MOVE(P)   SCFND         Q1FND
     C*** END ADD *** 04/08/99 ****************************************
     C                   CALL      'F0575R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    Q1FYR             3 0
     C                   PARM                    Q1CASE            7 0
     C                   PARM                    Q1SS#             9
     C                   PARM                    Q1ACDT            8 0
     C                   PARM                    Q1RTCD            1
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   PARM                    Q1CO#             3 0
     C                   PARM                    Q1SUB#            5 0
     C                   PARM                    Q1FND             3 0
     C*** END ADD *** 04/08/99 ****************************************
     C*
     C                   Z-ADD     RRN2          #@$2
     C                   Z-ADD     SVRRN2        RRN2
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    WDW01S
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C                   GOTO      MAINLN
     C*
     C                   ELSE                                                   ELSE EQ '5'
     C* IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '5' TO INDICATE
     C* A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   Z-ADD     RRN2          #@$2
     C                   Z-ADD     SVRRN2        RRN2
     C                   UPDATE    WDW01S
     C                   MOVE      *BLANK        SCSEL
     C                   GOTO      MAINLN
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '5'
     C                   END                                                    END EQ '1'
     C* IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
     C* WITHOUT A SELECTED RECORD
     C                   ELSE
     C                   GOTO      MAINLN
     C                   END                                                    END 0 *IN50
     C*
     C                   GOTO      TOP                                          RE-SET SUBFILE
     C*
     C* TERMINATION OF PROGRAM
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
     C**************************************************************************
     C* I N I T - SUBROUTINE TO INITIALIZE SCREEN                              *
     C**************************************************************************
     C     INZSR         BEGSR
     C*
     C     EOKEY         SETLL     PEO01                              60
     C*
     C* CLEAR SUBFILE
     C*
     C                   MOVE      '1'           *IN04
     C                   WRITE     WDW01C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C                   ENDSR
     C**************************************************************************
     C* L O A D - SUBROUTINE TO LOAD DATA TO SCREEN                            *
     C**************************************************************************
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C*
     C                   MOVE      'Y'           PCFL
     C*
     C* LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C*
     C                   DO        7             X                 5 0
     C*
     C     NXTEO1        TAG
     C     EOKEY         READE     PEO01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C*
     C* IF AUDIT/RECON FLAG IS AN 'R' ONLY DISPLAY EOMBS FOR THIS CASE/PROVIDER
     C* THAT HAVE NOT BEEN AUDITTED
     C*
     C     QQAR          IFEQ      'R'
     C*
     C     EOFEIN        IFEQ      SCFEIN
     C     EOMOD         ANDEQ     SCMOD
     C*
     C* CHAIN TO EOMB AUXILIARY FILE TO MAKE SURE THAT WE ONLY DISPLAY EOMBS
     C* THAT HAVE NOT BEEN AUDITTED
     C*
     C     E1KEY         CHAIN     PE101                              65
     C     *IN65         IFEQ      '0'
     C*
     C                   Z-ADD     9             Y
     C     1             SUBST     E120A1:Y      XXAR
     C                   Z-ADD     9             Y
     C     3             SUBST     E120A1:Y      XXAUD
     C     XXAR          IFEQ      'A'
     C*
     C                   GOTO      NXTEO1
     C*
     C                   END                                                    END XXAR EQ 'R'
     C*
     C                   ELSE                                                   ELSE *IN65 EQ '0'
     C*
     C                   GOTO      NXTEO1
     C*
     C                   END                                                    END 8IN65 EQ '0'
     C*
     C                   ELSE                                                   ELSE EOFEIN EQ SCFEI
     C*
     C                   GOTO      NXTEO1
     C*
     C                   END                                                    END EOFEIN EQ SCFEIN
     C*
     C                   ELSE                                                   ELSE QQAR EQ 'R'
     C*
     C* IF AUDIT/RECON FLAG IS AN 'A' ONLY DISPLAY EOMBS FOR THIS CASE/PROVIDER
     C* THAT HAVE BEEN AUDITTED
     C*
     C     QQAR          IFEQ      'A'
     C*
     C     EOFEIN        IFEQ      SCFEIN
     C     EOMOD         ANDEQ     SCMOD
     C*
     C* CHAIN TO EOMB AUXILIARY FILE TO MAKE SURE THAT WE ONLY DISPLAY EOMBS
     C* THAT HAVE NOT BEEN AUDITTED
     C*
     C     E1KEY         CHAIN     PE101                              65
     C     *IN65         IFEQ      '0'
     C*
     C                   Z-ADD     9             Y
     C     1             SUBST     E120A1:Y      XXAR
     C                   Z-ADD     9             Y
     C     3             SUBST     E120A1:Y      XXAUD
     C     XXAR          IFNE      'A'
     C*
     C                   GOTO      NXTEO1
     C*
     C                   END                                                    END XXAR EQ 'R'
     C*
     C                   ELSE                                                   ELSE *IN65 EQ '0'
     C*
     C                   GOTO      NXTEO1
     C*
     C                   END                                                    END 8IN65 EQ '0'
     C*
     C                   ELSE                                                   ELSE EOFEIN EQ SCFEI
     C*
     C                   GOTO      NXTEO1
     C*
     C                   END                                                    END EOFEIN EQ SCFEIN
     C*
     C                   END                                                    END QQAR EQ 'A'
     C                   END                                                    END QQAR EQ 'R'
     C                   END                                                    END *IN60 EQ '0'
     C*
     C     *IN60         IFEQ      '0'
     C                   ADD       1             RRN2
     C                   Z-ADD     EOEOB#        SCEOB#
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFRDT
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
     C                   Z-ADD     EOAMTC        SCTOTC
     C                   Z-ADD     EOAMT         SCPPD
     C*
     C* SAVE 1ST SUBFILE RRN OF CURRENT PAGE
     C*
     C     PCFL          IFEQ      'Y'
     C                   Z-ADD     RRN2          XXRRN2
     C                   MOVE      'N'           PCFL
     C                   END
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     WDW01S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* INZSRIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C*
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   Z-ADD     *ZEROS        SCEOB#
     C                   Z-ADD     *ZEROS        SCFRDT
     C                   Z-ADD     *ZEROS        SCTODT
     C                   Z-ADD     *ZEROS        SCTOTC
     C                   Z-ADD     *ZEROS        SCPPD
     C                   Z-ADD     1             RRN2
     C*
     C* SAVE 1ST SUBFILE RRN OF THE CURRENT PAGE
     C*
     C     PCFL          IFEQ      'Y'
     C                   Z-ADD     RRN2          XXRRN2
     C                   MOVE      'N'           PCFL
     C                   END
     C*
     C                   MOVE      '1'           *IN06
     C                   WRITE     WDW01S
     C                   END
     C*
     C* TO POSITION SCREEN AND CURSOR
     C*
     C                   Z-ADD     XXRRN2        #@$2
     C*
     C                   ENDSR
     C/EJECT
     C**************************************************************************
     C* S C R E X C - SUBROUTINE TO DISPLAY THE SCREEN                         *
     C**************************************************************************
     C     SCREXC        BEGSR
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01C'
     C     Q2RECF        OREQ      'WDW01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     WDW01C
     C                   WRITE     WDW01
     C                   EXFMT     WDW01C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C                   ENDSR
     C*
     C/EJECT
     C**************************************************************************
     C* F K E Y S - SUBROUTINE TO PROCESS FUNCTION KEYS                        *
     C**************************************************************************
     C     FKEYS         BEGSR
     C*
     C     *INKA         IFEQ      '1'
     C                   END
     C*
     C     *INKB         IFEQ      '1'
     C                   END
     C*
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   END
     C*
     C     *INKD         IFEQ      '1'
     C                   END
     C*
     C*
     C     *INKE         IFEQ      '1'
     C                   END
     C*
     C*
     C     *INKF         IFEQ      '1'
     C                   END
     C*
     C*
     C     *INKG         IFEQ      '1'
     C                   END
     C*
     C*
     C     *INKH         IFEQ      '1'
     C                   END
     C*
     C*
     C     *INKI         IFEQ      '1'
     C                   END
     C*
     C*
     C     *INKJ         IFEQ      '1'
     C                   END
     C*
     C     *INKK         IFEQ      '1'
     C                   END
     C*
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01S'
     C     Q2RECF        OREQ      'WDW01C'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   END                                                    END *INKL
     C****************************************
     C     *INKM         IFEQ      '1'
     C                   END
     C*
     C     *INKN         IFEQ      '1'
     C                   END
     C*
     C     *INKP         IFEQ      '1'
     C                   END
     C*
     C     *INKQ         IFEQ      '1'
     C                   END
     C*
     C     *INKR         IFEQ      '1'
     C                   END
     C*
     C     *INKS         IFEQ      '1'
     C                   END
     C*
     C     *INKT         IFEQ      '1'
     C                   END
     C*
     C     *INKU         IFEQ      '1'
     C                   END
     C*
     C     *INKV         IFEQ      '1'
     C                   END
     C*
     C     *INKW         IFEQ      '1'
     C                   END
     C*
     C     *INKX         IFEQ      '1'
     C                   END
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           MAINLN                         NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      MAINLN
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C                   GOTO      MAINLN
     C*
     C                   ENDSR
     C/EJECT
