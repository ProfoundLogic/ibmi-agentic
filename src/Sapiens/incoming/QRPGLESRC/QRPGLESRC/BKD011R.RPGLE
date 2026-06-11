     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE BKD011R- CHART OF ACCOUNTS SEARCH                        *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*****************************************************************
     FBKD011F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F*
     FBKCAL1    IF   E           K DISK
     FBKSYL1    IF   E           K DISK

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
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C*
     C*   KEY LIST FOR THE CHART OF ACCOUNTS
     C*
     C     GLKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCGLD#
     C                   KFLD                    SCACC#
     C                   KFLD                    SCSUB#
     C*
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    SCCO#
     C                   PARM                    SCFAC#
     C                   PARM                    SCGLD#
     C                   PARM                    SCACC#
     C                   PARM                    SCSUB#
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQRTCD            1
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C                   MOVE      '0'           *IN59
     C     QQTYPE        COMP      'G'                                    08
     C     'BK'          CHAIN     PSY01                              54
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C     GLKEY         SETLL     PCA01                              60
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q1USER        SCUSER                                     ILE
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      '1'
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   END                                                    END *INKL
     C*
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C**************************************************************************
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C     SCFAC#        IFNE      *ZERO
     C     SCGLD#        ORNE      *ZERO
     C     SCACC#        ORNE      *ZERO
     C     SCSUB#        ORNE      *ZERO
     C                   GOTO      TOP
     C                   END                                                    END SCUCID NE BLKS
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C                   MOVE      '0'           *IN59
     C     TAGRC         TAG
     C*
     C                   READC     SCR01S                                 50
     C     *IN50         IFEQ      '0'
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
     C     SCSEL         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   ELSE                                                   ELSE EQ '1'
     C     SCSEL         IFEQ      '5'
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     GLKEY         CHAIN     PCA01                              40
     C                   MOVEL     ' INQUIRY'    SCMODE
     C                   MOVE      CAFAC#        SCFAC#
     C                   MOVE      CADESC        SCDESC
     C                   MOVE      CAGLD#        SCGLD#
     C                   MOVE      CAACC#        SCACC#
     C                   MOVE      CASUB#        SCSUB#
     C                   MOVE      CATYPE        SCTYPE
     C                   MOVE      CACODE        SCCODE
     C                   MOVE      CATOTL        SCTOTL
     C                   MOVE      CATOTL        SCTOTL
     C     *IN08         IFEQ      '0'
     C                   GOTO      TAGOV
     C                   END
     C                   Z-ADD     CALF01        SCLF01
     C                   Z-ADD     CALF02        SCLF02
     C                   Z-ADD     CALF03        SCLF03
     C                   Z-ADD     CALF04        SCLF04
     C                   Z-ADD     CALF05        SCLF05
     C                   Z-ADD     CALF06        SCLF06
     C                   Z-ADD     CALF07        SCLF07
     C                   Z-ADD     CALF08        SCLF08
     C                   Z-ADD     CALF09        SCLF09
     C                   Z-ADD     CALF10        SCLF10
     C                   Z-ADD     CALF11        SCLF11
     C                   Z-ADD     CACF12        SCLF12
     C                   Z-ADD     CAFISC        SCCF00
     C                   Z-ADD     CACF01        SCCF01
     C                   Z-ADD     CACF02        SCCF02
     C                   Z-ADD     CACF03        SCCF03
     C                   Z-ADD     CACF04        SCCF04
     C                   Z-ADD     CACF05        SCCF05
     C                   Z-ADD     CACF06        SCCF06
     C                   Z-ADD     CACF07        SCCF07
     C                   Z-ADD     CACF08        SCCF08
     C                   Z-ADD     CACF09        SCCF09
     C                   Z-ADD     CACF10        SCCF10
     C                   Z-ADD     CACF11        SCCF11
     C                   Z-ADD     CACF12        SCCF12
     C                   Z-ADD     CABG01        SCBG01
     C                   Z-ADD     CABG02        SCBG02
     C                   Z-ADD     CABG03        SCBG03
     C                   Z-ADD     CABG04        SCBG04
     C                   Z-ADD     CABG05        SCBG05
     C                   Z-ADD     CABG06        SCBG06
     C                   Z-ADD     CABG07        SCBG07
     C                   Z-ADD     CABG08        SCBG08
     C                   Z-ADD     CABG09        SCBG09
     C                   Z-ADD     CABG10        SCBG10
     C                   Z-ADD     CABG11        SCBG11
     C                   Z-ADD     CABG12        SCBG12
     C*
     C     TAGOV         TAG
     C*
     C                   MOVE      '1'           *IN59
     C                   EXFMT     SCR02
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C                   GOTO      TAGRC
     C                   END
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '1'
     C*** IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
     C*** WITHOUT A SELECTED RECORD
     C                   ELSE
     C**
     C*** IF THE SUBFILE HAS CHANGED DISPLAYED CHART DATA REDISPLAY    INDICATE
     C*** G/L# LIST SUB FILE
     C     *IN59         IFEQ      '1'
     C                   MOVE      ' '           SCSEL
     C                   GOTO      TOP
     C                   END
     C                   GOTO      TERM
     C                   END                                                    END 0 *IN50
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INIT          BEGSR
     C                   Z-ADD     0             RRN2              5 0
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   ENDSR
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        13            X                 5 0
     C*
     C     SCCO#         READE     PCA01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   ADD       1             RRN2
     C     X             IFEQ      1
     C                   Z-ADD     RRN2          #@$2
     C                   END
     C                   MOVEL     CAFAC#        SCFAC#
     C                   MOVEL     CAGLD#        SCGLD#
     C                   MOVEL     CAACC#        SCACC#
     C                   MOVEL     CASUB#        SCSUB#
     C                   MOVEL     CADESC        SCDESC
     C                   MOVEL     CATYPE        SCTYPE
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANKS       SCFAC#
     C                   MOVE      *BLANKS       SCGLD#
     C                   MOVE      *BLANKS       SCACC#
     C                   MOVE      *BLANKS       SCSUB#
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCTYPE
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     1             #@$2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01S
     C                   END
     C*
     C                   MOVE      *BLANKS       SCFAC#
     C                   MOVE      *BLANKS       SCGLD#
     C                   MOVE      *BLANKS       SCACC#
     C                   MOVE      *BLANKS       SCSUB#
     C                   MOVE      *BLANKS       SCTYPE
     C*
     C                   ENDSR
