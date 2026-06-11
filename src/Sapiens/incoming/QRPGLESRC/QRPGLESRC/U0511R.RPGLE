     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE U0511R - PRE-CERTIFICATION SEARCH BY CASE/DATE           *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  03/25/98 C:47  Project: 87MCB
     H*                 CHANGED CALL: U0010R TO U0060R01 - PRE-CERT APP
     H*
     H*  08/06/93 C:20    PROJ: URMNT                                 *
     H*                   1. CHANGED F3 TO RETURN LIKE F12 NOT TO EXIT*
     H*                   2. CHANGED TO DISPLAY ALL PRE-CERTS FOR CASE*
     H*                      REGARDLESS OF STATUS OR PROVIDER         *
     H*  03/18/97 C:45    REQ/PROJ: UR022                             *
     H*                   1. CREATED NEW LOGICAL UMPCL5 TO ONLY READ  *
     H*                      NON CLOSED PRE-CERTS.                    *
     H*                   2. CHANGED THE PROGRAM TO READ UMPCL5.      *
     H*                                                               *
     H*****************************************************************
     FU0511F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(WDW01S:RRN2)
     F*
     FUMPCL5    IF   E           K DISK
     F                                     RENAME(PPC01:PPC02)
     FUMPCL1    IF   E           K DISK
     F                                     RENAME(PPC01:PPC03)
     FFMCMLA    IF   E           K DISK
     FFMPDL     IF   E           K DISK
      *** BEG ADD *** 03/25/98 **************************************
     FUMPCL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
      *** END ADD *** 03/25/98 **************************************

     D*
     D PTL             S             40    DIM(1) CTDATA PERRCD(1)              PGM TITLES
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      *** BEG ADD *** 03/25/98 **************************************
      *  FMCMLA
     D DBFDS2          DS
     D  FLSTCM           *STATUS
     D  DBRRN2               397    400B 0
      *** END ADD *** 03/25/98 **************************************
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE CUSTOM GROUP ID FILE
     C*
     C     PCKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     PCKY1         KLIST
     C                   KFLD                    XXCO#
     C*
      *** BEG ADD *** 3/25/98 ***********************************
     C     PCKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    SCCERT
     C                   KFLD                    SCEXT
      *** END ADD *** 3/25/98 ***********************************
     C*
     C     PCKY3         KLIST
     C                   KFLD                    XXCERT
     C                   KFLD                    XXEXT
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXSS#
     C                   KFLD                    XXACDT
     C*
     C*  PROVIDER FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    X1FEIN
     C                   KFLD                    X1MOD
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQSS#             9
     C                   PARM                    QQACDT            8
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQCERT           15
     C                   PARM                    QQEXT             3
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
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    CMCO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    CMSUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    CMFND         XXFND                          3.0N
     C     *LIKE         DEFINE    CMFYR         XXFYR                          3.0N
     C     *LIKE         DEFINE    CMCASE        XXCASE                         7.0N
     C     *LIKE         DEFINE    CMSS#         XXSS#                          9A
     C     *LIKE         DEFINE    CMACDT        XXACDT                         7.0N
     C     *LIKE         DEFINE    PDFEIN        XXFEIN                         9.0N
     C     *LIKE         DEFINE    PDMOD         XXMOD                          3.0N
     C     *LIKE         DEFINE    PDFEIN        X1FEIN                         9.0N
     C     *LIKE         DEFINE    PDMOD         X1MOD                          3.0N
     C     *LIKE         DEFINE    PCCERT        XXCERT                         15A
     C     *LIKE         DEFINE    PCEXT         XXEXT                          3.0N
     C*
     C                   MOVEL     QQPRDS        SCPRDS
     C                   MOVE      QQSS#         XXSS#
     C                   MOVE      QQACDT        X1ACDT            8 0
     C                   MOVE      QQFEIN        XXFEIN
     C                   MOVE      QQMOD         XXMOD
     C                   MOVE      '0'           *IN25
     C*
     C                   Z-ADD     X1ACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C*
     C* CHECK TO SEE IF SEARCH IS FOR ONE SS/ACDT/FEIN/MOD OR ALL AND
     C* SET APPROPRIATE INDICATOR ON OR OFF
     C*
     C     XXSS#         IFEQ      *BLANKS
     C     XXACDT        ANDEQ     *ZEROS
     C     XXFEIN        ANDEQ     *ZEROS
     C     XXMOD         ANDEQ     *ZEROS
     C                   MOVE      '1'           *IN25
     C                   END
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
     C                   MOVE      SCCER#        XXCERT
     C                   Z-ADD     SCEXT#        XXEXT
     C                   MOVE      *BLANKS       SCCER#
     C                   Z-ADD     *ZEROS        SCEXT#
     C                   MOVE      '1'           *IN26
     C     *IN25         IFEQ      '1'
     C                   MOVE      '1'           *IN27
     C     PCKY3         CHAIN     PPC03                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     PCCO#         XXCO#
     C                   Z-ADD     PCSUB#        XXSUB#
     C                   Z-ADD     PCFND         XXFND
     C                   END
     C                   END
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
     C                   MOVE      '2'           QQRTCD
     C                   MOVE      *BLANKS       QQCERT
     C                   MOVE      *ZEROS        QQEXT
     C                   MOVEL     SCCERT        QQCERT
     C                   MOVE      SCEXT         QQEXT
     C                   GOTO      TERM
     C                   ELSE                                                   ELSE EQ '1'
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** CALL PRE-CERT MAINT. IN INQUIRY MODE GO DIRECTLY TO SCR02
     C     SCSEL         IFEQ      '5'
     C                   MOVE      *BLANKS       SCSEL
     C                   UPDATE    WDW01S
     C                   MOVE      *BLANKS       Q1PRDS           40
     C                   MOVE      *BLANKS       Q1CERT           15
     C                   MOVE      *ZEROS        Q1EXT             3
     C                   MOVEL     SCCERT        Q1CERT
     C                   MOVEL     SCEXT         Q1EXT
     C                   MOVEL     PTL(1)        Q1PRDS
      *** BEG DEL *** 03/35/98 **************************************
     C***                  CALL 'U0010R'
     C***                  PARM           Q1PRDS
     C***                  PARM '2'       QQCALL  1
     C***                  PARM           Q1CERT
     C***                  PARM           Q1EXT
      *** END DEL *** 03/35/98 **************************************
     C*
     C*** BEG ADD *** 03/25/98 **************************************
     C**                   MOVE XXCO#     QQCO#
     C**                   MOVE XXSUB#    QQSUB#
     C**                   MOVE XXFND     QQFND
     C**                   MOVE XXFYR     QQFYR
     C**                   MOVE XXCASE    QQCASE
     C     PCKY2         CHAIN     PPC01                              60
     C                   MOVE      PCEXT         QQEXT
      *
     C                   CALL      'U0060R01'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      'D'           @@MODE            1
     C                   PARM      DBRRN2        @@RRN             9 0
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM      PCCERT        QQCERT           15
     C                   PARM                    QQEXT             3
     C                   PARM      *BLANK        QQOCST            1
     C                   PARM      'Y'           QQCONT            1
     C*
     C*** END ADD *** 03/25/98 **************************************
     C                   ELSE                                                   ELSE EQ '5'
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   Z-ADD     RRN2          #@$2
     C                   UPDATE    WDW01S
     C                   MOVE      *BLANK        SCSEL
     C                   GOTO      MAINLN
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '5'
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
     C     *IN25         IFEQ      '0'
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     CMCO#         XXCO#
     C                   Z-ADD     CMSUB#        XXSUB#
     C                   Z-ADD     CMFND         XXFND
     C                   Z-ADD     CMFYR         XXFYR
     C                   Z-ADD     CMCASE        XXCASE
     C     *IN26         IFEQ      '0'
     C     PCKEY         CHAIN     PPC02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PCCERT        XXCERT
     C                   Z-ADD     PCEXT         XXEXT
     C                   ELSE
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C                   END
     C                   END
     C                   END                                                    END *IN25
     C                   MOVE      '0'           *IN26
     C*
     C     PCKEY         SETLL     PPC02                              60
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
     C     PCKEY         READE     PPC02                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C*
     C     *IN25         IFEQ      '0'
     C     PCCASE        IFNE      XXCASE
     C                   GOTO      NXTRCD
     C                   END
     C                   END
     C*
     C                   ADD       1             RRN2              5 0
     C                   MOVEL     PCCERT        SCCERT
     C                   Z-ADD     PCEXT         SCEXT
     C*
     C                   Z-ADD     PCADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCADDT
     C*
     C                   Z-ADD     PCFEIN        X1FEIN
     C                   Z-ADD     PCMOD         X1MOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PDSPCD        SCSPEC
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        SCNAME
     C                   ELSE
     C                   MOVEL     PDNAME        SCNAME
     C                   END                                                    END CDFSN
     C                   END                                                    END *IN60
     C                   Z-ADD     X1FEIN        SCFEIN
     C                   Z-ADD     X1MOD         SCMOD
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     WDW01S
     C     NXTRCD        TAG
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INZSRIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C     *IN26         IFEQ      '1'
     C     *IN27         OREQ      '1'
     C                   MOVE      *BLANKS       SCCERT
     C                   MOVE      *ZEROS        SCEXT
     C                   MOVE      *ZEROS        SCADDT
     C                   MOVE      *ZEROS        SCFEIN
     C                   MOVE      *ZEROS        SCMOD
     C                   MOVE      *BLANKS       SCNAME
     C                   MOVE      *BLANKS       SCSPEC
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     WDW01S
     C                   ELSE
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C                   END
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
     C                   MOVE      '*'           QQRTCD
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
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANKS       QQCERT
     C                   MOVE      *ZEROS        QQEXT
     C                   GOTO      TERM
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
**  PTL - PROGRAM TITLES (PASSED IN Q1PRDS)
   PRE-CERTIFICATION MASTER MAINTENANCE
