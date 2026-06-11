     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE F0529R - SERVICE CODE DESCRIPTION SEARCH                 *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  30    INVALID AND/OR FLAG                                    *
     H*  31    IF NO RECORDS MEET SELECTION CRITERIA SEND MESSAGE     *
     H*  33    IF ALL PATTERNS ARE BLANK SEND MESSAGE                 *
     H*  34    3 PATTERNS CANNOT BE BLANK WITH AN 'A' TYPE            *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*****************************************************************
     FF0529F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(WDW02S:RRN2)
     F*
     FFMSVL1    IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FSTABL     IF   E           K DISK

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
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    SVCMSV        XXCMSV
     C     *LIKE         DEFINE    SVCMSV        QQCMSV
     C     *LIKE         DEFINE    STKEY         XXKEY
     C     *LIKE         DEFINE    SVDT          XXEFDT
     C*
     C*  USER PARAMETER FILE
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR STATE FILE
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*  SERVICE CODE FILE
     C     SVKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXCMSV
     C                   KFLD                    XXEFDT                         ADD 07/16/90
     C*
     C     SVKEY1        KLIST
     C                   KFLD                    UPCO#
     C*
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCMSV
     C                   PARM                    QQST              2
     C                   PARM                    QQEFDT            8 0          ADD 07/16/90
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
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C                   MOVEL     QQST          SCST
     C                   MOVEL     SCST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     STNAME        SCSTNM
     C                   END
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C*
     C                   TIME                    DSDTTM
     C*
     C     STRT          TAG
     C     Q2RECF        IFEQ      'WDW01'
     C                   WRITE     WDW01
     C                   EXFMT     WDW01
     C                   END                                                    END SCR01
     C*
     C     *IN11         CASEQ     '1'           FKEYS
     C                   END                                                      END *IN11
     C*
     C                   Z-ADD     0             A
     C     SCPAT1        IFEQ      *BLANKS
     C     A             ADD       1             A                 2 0
     C                   END
     C     SCPAT2        IFEQ      *BLANKS
     C     A             ADD       1             A
     C                   END
     C     SCPAT3        IFEQ      *BLANKS
     C     A             ADD       1             A
     C                   END
     C     SCPAT4        IFEQ      *BLANKS
     C     A             ADD       1             A
     C                   END
     C     A             IFEQ      3
     C     SCFLAG        IFEQ      'A'
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      'O'           SCFLAG
     C                   END
     C                   END
     C     A             IFEQ      4
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C     SCFLAG        IFNE      'A'
     C     SCFLAG        ANDNE     'O'
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
      *===============================================================
      * INITIALIZATION    AREA
      *
     C     TOP           TAG
     C                   EXSR      INZSR
      *
      *===============================================================
      *
     C     MAINLN        TAG
      *
      *===============================================================
      * SCREEN EXECUTION AREA
      *
     C                   EXSR      SCREXC
      *
      *===============================================================
      * HANDLE F KEYS IF PRESSED  FROM WINDOW 2
      *
     C     *IN11         CASEQ     '1'           FKEYS
     C                   END                                                      END *IN11
      *
      *===============================================================
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C                   Z-ADD     RRN2          SVRRN2            5 0
     C*
     C                   READC     WDW02S                                 50
     C     *IN50         IFEQ      '0'
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
     C     SCSEL         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD            1
     C                   MOVE      *BLANK        QQCMSV
     C                   Z-ADD     *ZEROS        QQEFDT
     C                   MOVEL     SCCMSV        QQCMSV
     C                   Z-ADD     SCEFDT        QQEFDT
     C                   ELSE                                                   ELSE EQ '1'
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   MOVE      '1'           *IN21
     C                   Z-ADD     RRN2          #@$2
     C                   UPDATE    WDW02S
     C                   MOVE      *BLANK        SCSEL
     C                   GOTO      MAINLN
     C                   END                                                    END NE ' '
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '1'
     C*
      *===============================================================
      * TERMINATION OF PROGRAM
      *
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
      *====================================================================*
      * * *   INZSR SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INZSR         BEGSR
     C*
     C     SVKEY         SETLL     PSV01                              60
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     WDW02C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C*
     C                   ENDSR
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C*
     C                   MOVEL     SCPAT1        PATT1
     C                   MOVEL     SCPAT2        PATT2
     C                   MOVEL     SCPAT3        PATT3
     C                   MOVEL     SCPAT4        PATT4
     C                   MOVEL     SCFLAG        AOFLG
     C                   MOVE      *BLANKS       FLAG
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        999           X                 5 0
     C     *IN05         CABEQ     '1'           LDOUT
     C*
     C     SVKEY1        READE     PSV01                                  60
     C                   MOVE      *BLANKS       FLAG
     C                   MOVE      *BLANKS       STRING
     C                   MOVEL     SVDESC        STRING
     C                   CALL      'F0529C'
     C                   PARM                    STRING           40
     C                   PARM                    PATT1            15
     C                   PARM                    PATT2            15
     C                   PARM                    PATT3            15
     C                   PARM                    PATT4            15
     C                   PARM                    AOFLG             1
     C                   PARM                    FLAG              1
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C     FLAG          IFEQ      'Y'
     C                   ADD       1             RRN2              5 0
     C                   MOVEL     SVDESC        SCDESC
     C                   MOVEL     SVCMSV        SCCMSV
     C*
     C                   Z-ADD     SVEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVEL     SVSTAT        SCSTAT
      *
     C                   MOVE      '0'           *IN06
     C                   MOVE      '0'           *IN31
     C                   WRITE     WDW02S
     C                   END
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C     LDOUT         TAG
     C* * * INZSRIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     FLAG          ANDEQ     ' '
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCCMSV
     C                   Z-ADD     *ZEROS        SCEFDT
     C                   MOVE      *BLANKS       SCSTAT
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   MOVE      '1'           *IN31
     C                   WRITE     WDW02S
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     1             #@$2
     C*
     C*
     C                   ENDSR
     C/EJECT
      *===============================================================
     C     SCREXC        BEGSR
      *===============================================================
      *
      *
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C******************** MOVELQ2RECF    SCRECF            FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'WDW02'
     C     Q2RECF        OREQ      'WDW02C'
     C     Q2RECF        OREQ      'WDW02S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     WDW02C
     C                   WRITE     WDW02
     C                   EXFMT     WDW02C
     C                   MOVE      '0'           *IN01
     C     SVRRN2        IFNE      0
     C                   Z-ADD     SVRRN2        RRN2
     C                   Z-ADD     0             SVRRN2
     C                   END
     C                   END
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
     C     *INKC         IFEQ      '1'
     C                   END
      *
     C     *INKD         IFEQ      '1'
     C                   END
      *
      *
     C     *INKE         IFEQ      '1'
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
     C     Q2RECF        IFEQ      'WDW01'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C     Q2RECF        IFEQ      'WDW02'
     C     Q2RECF        OREQ      'WDW02S'
     C     Q2RECF        OREQ      'WDW02C'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C                   MOVE      *BLANK        SCSEL
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END *INKL
     C****************************************
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
     C                   GOTO      MAINLN
      *
     C                   ENDSR
     C/EJECT
