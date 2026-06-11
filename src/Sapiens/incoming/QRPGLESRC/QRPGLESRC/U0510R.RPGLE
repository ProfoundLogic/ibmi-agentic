     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE U0520R - PRE-CERTIFICATION SEARCH                        *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*  06/09/20  C:295  Project : SY420                             *
     H*                                                               *
     H*  03/12/93  C:20   Proj: 99AAE                                 *
     H*                   CHANGE TO INITALIZE SUBFILE CORRECTLY IF NO *
     H*                   RECORDS TO DISPLAY                          *
     H*****************************************************************
     FU0510F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(WDW01S:RRN2)
     F*
     FUMPCL     IF   E           K DISK    USROPN
     FUMPCL2    IF   E           K DISK    USROPN
     F                                     RENAME(PPC01:PPC02)
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FUMCHL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMSML     IF   E           K DISK

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

      ***  Begin Add  ***  06/09/20  ****************************
      /copy sPrMask
      ***  End   Add  ***  06/09/20  ****************************

     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C*
     C*   KEY LIST FOR THE CUSTOM GROUP ID FILE
     C*
     C     PCKY1         KLIST
     C                   KFLD                    QQCO#
     C*
     C     PCKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQCERT
     C                   KFLD                    QQEXT
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  PRE-CERT HISTORY FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCERT
     C                   KFLD                    SCEXT
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C*
     C*   KEY LIST FOR THE SUBSCRIBER
     C*
     C     SMKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQSUB#            5 0
     C                   PARM                    QQFND             3 0
     C                   PARM                    QQCERT           15
     C                   PARM                    QQEXT             3 0
     C                   PARM                    QQSTAT            1
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
     C                   MOVEL     QQPRDS        SCPRDS
     C                   Z-ADD     *ZEROS        XXCO#             3 0
     C                   Z-ADD     *ZEROS        XXSUB#            5 0
     C                   Z-ADD     *ZEROS        XXFND             3 0
     C                   Z-ADD     *ZEROS        XXFYR             3 0
     C                   Z-ADD     *ZEROS        XXCASE            7 0
     C     QQSTAT        IFEQ      'O'
     C                   MOVE      '1'           *IN25
     C                   OPEN      UMPCL2
     C                   ELSE
     C                   MOVE      '0'           *IN25
     C                   OPEN      UMPCL
     C                   END
     C*
     C*  GET THE SUBSCRIBER RECORD
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C                   MOVE      QQSUB#        SCSUB#
     C                   MOVEL     SMNAME        SCSBNM
     C*
     C*  GET THE FUND RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      QQFND         SCFND
     C                   MOVEL     FNNAME        SCFDNM
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
      * HANDLE F KEYS IF PRESSED
      *
     C     *IN11         CASEQ     '1'           FKEYS
     C                   END                                                      END *IN11
      *
      *===============================================================
     C*
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C     SCCER#        IFNE      *BLANKS
     C                   MOVE      SCCER#        QQCERT
     C                   MOVE      SCEXT#        QQEXT
     C                   MOVE      *BLANKS       SCCER#
     C                   Z-ADD     *ZEROS        SCEXT#
     C                   GOTO      TOP
     C                   END                                                    END SCSUBS
     C*
     C                   Z-ADD     RRN2          SVRRN2            5 0
     C*
     C                   READC     WDW01S                                 50
     C     *IN50         IFEQ      '0'
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
     C     SCSEL         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD            1
     C                   Z-ADD     *ZEROS        QQSUB#            5 0
     C                   Z-ADD     *ZEROS        QQFND             3 0
     C                   MOVE      *BLANKS       QQCERT           15
     C                   Z-ADD     *ZEROS        QQEXT             3 0
     C                   MOVEL     SCCERT        QQCERT
     C                   Z-ADD     SCEXT         QQEXT
     C                   GOTO      TERM
     C                   ELSE                                                   ELSE EQ '1'
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   Z-ADD     RRN2          #@$2
     C                   UPDATE    WDW01S
     C                   MOVE      *BLANK        SCSEL
     C                   GOTO      MAINLN
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '1'
     C*** IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
     C*** WITHOUT A SELECTED RECORD
     C                   ELSE
     C                   GOTO      MAINLN
     C                   END                                                    END 0 *IN50
     C*
     C                   GOTO      TOP                                          RE-SET SUBFILE
     C*
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
     C  N25PCKEY         SETLL     PPC01                              60
     C   25PCKEY         SETLL     PPC02                              60
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     WDW01C
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
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        7             X                 5 0
     C*
     C  N25PCKY1         READE     PPC01                                  60
     C   25PCKY1         READE     PPC02                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C     PCSUB#        ANDEQ     QQSUB#
     C     PCFND         ANDEQ     QQFND
     C                   ADD       1             RRN2              5 0
     C                   MOVEL     PCCERT        SCCERT
     C                   Z-ADD     PCEXT         SCEXT
     C                   MOVE      PCSTAT        SCSTAT
     C                   Z-ADD     PCCO#         XXCO#
     C                   Z-ADD     PCSUB#        XXSUB#
     C                   Z-ADD     PCFND         XXFND
     C                   Z-ADD     PCFYR         XXFYR
     C                   Z-ADD     PCCASE        XXCASE
     C     CMKY1         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C***  Beg Chg *** 06/09/20 ****************************************
     C****               MOVEL     CMSS#         SCSS#
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     SCSS#         PARM      CMSS#         QQVAL           100
     C***  End Chg *** 06/09/20 ****************************************
     C                   ELSE
     C                   Z-ADD     *ZEROS        SCACDT
     C                   MOVE      *BLANKS       SCSS#
     C                   END
     C***  Beg Chg *** 06/09/20 ****************************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      SCSS#         QQVAL           100
     C***  SCSS#         CHAIN     PCD01                              60
     C     CMSS#         CHAIN     PCD01                              60
     C***  End Chg *** 06/09/20 ****************************************
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST           56
     C     CDFSN         IFNE      *BLANK
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        SCNAME
     C                   END                                                    END CDFSN
     C                   ELSE
     C                   MOVE      *BLANKS       SCNAME
     C                   END                                                    END *IN60
     C     PCSTAT        IFEQ      'R'
     C     CHKEY         CHAIN     PCH01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRCDT
     C*
     C                   END
     C                   ELSE
     C                   Z-ADD     *ZEROS        SCRCDT
     C                   END
      *
     C                   MOVE      '0'           *IN06
     C                   WRITE     WDW01S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INZSRIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C                   MOVE      *BLANKS       SCCERT
     C                   MOVE      *ZEROS        SCEXT
     C                   MOVE      *ZEROS        SCRCDT
     C                   MOVE      *ZEROS        SCACDT
     C                   MOVE      *BLANKS       SCSS#
     C                   MOVE      *BLANKS       SCNAME
     C                   MOVE      *BLANKS       SCSTAT
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     WDW01S
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     RRN2          #@$2
     C*
     C                   MOVEL     *BLANK        SCCER#
     C*
     C                   ENDSR
     C/EJECT
      *===============================================================
     C     SCREXC        BEGSR
      *===============================================================
      *
      *
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
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKC
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
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01S'
     C     Q2RECF        OREQ      'WDW01C'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *BLANKS       QQCERT
     C                   MOVE      *ZEROS        QQEXT
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
      ****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           MAINLN                         NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      MAINLN
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C                   GOTO      MAINLN
      *
     C                   ENDSR
     C/EJECT
