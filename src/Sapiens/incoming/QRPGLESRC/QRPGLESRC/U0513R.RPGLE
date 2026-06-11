     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE U0513R - OPEN PRE-CERT BY SS# - SRCH PGM.
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program allows the user to more easily locate      **
     H**     Pre-Certs on a claimant.                                **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     CALL = '1' U0010R                                       **
     H**     CALL = '2' Called from other external program           **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     CALL = '2' U0010R in Inquiry Mode.                      **
     H**                                                             **
     H**                                                             **
     H**   PROGRAMMER: C:41                                          **
     H**      CREATED: 02/16/96                                      **
     H**  REQ/PROJECT: 179/UR017                                     **
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  06/09/20    C:295     Project : SY420                        *
     H*                                                               *
     H*****************************************************************
     H** INDICATORS    FUNCTION DESCRIPTION                          **
     H**                                                             **
     H**  06   IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN         **
     H**       SEPARATOR ON SEL FLAG FIELDS.                         **
     H**  20   INVALID CHARACTER USED TO INDICATE A SELECTED RECORD  **
     H**  50   CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)   **
     H**  60   GENERAL USAGE (CHAIN)                                 **
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**    UMPCL4 (PPC01): SS#/ACDT/FEIN                            **
     H**                    NO S/O                                   **
     H**    FMUPL  (PCD01): SS#                                      **
     H**                    NO S/O                                   **
     H**    SMCOL  (PCO01): CO                                       **
     H**                    NO S/O                                   **
     H*****************************************************************
     H DFTNAME(U0513R)
     FU0513F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(WDW01S:RRN2)
     FUMPCL4    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FSMCOL     IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D SCDATA          DS
     D  SCSSN                  1      9
     D  SCADAT                10     17  0
     D  SCFIN#                18     26  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

      ***  Begin Add  ***  06/09/20  ****************************
      /copy sPrMask
      ***  End   Add  ***  06/09/20  ****************************

     C*
     C*   FIELD INITIALIZATION
     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    CODT          XXACDT
     C*
     C                   CLEAR                   SCDATA
     C*
     C*   KEY LIST FOR THE CARRIER FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C*
     C*   KEY LIST FOR THE CUSTOM GROUP ID FILE
     C*
     C     PCKEY         KLIST
     C                   KFLD                    XXSS#
     C                   KFLD                    XXACDT
     C                   KFLD                    XXFEIN
     C*
     C*-------------------------------------*
     C*   PARAMETER LIST FOR CALLED PROGRAM
     C*-------------------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQSS#             9
     C                   PARM                    QQACDT            8
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
     C                   MOVEL     QQPRDS        SCPRDS
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   MOVE      QQSS#         XXSS#             9
     C                   MOVE      QQACDT        X1ACDT            8 0
     C*
     C                   Z-ADD     X1ACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C*
     C                   MOVE      QQCERT        XXCERT           15
     C                   MOVE      QQEXT         XXEXT             3 0
     C                   MOVE      *ZEROS        XXFEIN            9 0
     C*
     C*===============================================================
     C* CALL U0010R IN INQUIRY MODE IF EXTERNAL CALL.
     C*
     C     QQCALL        IFEQ      '2'
     C*
     C                   CALL      'U0010R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCERT
     C                   PARM                    QQEXT
     C*
     C     QQCALL        CABEQ     '2'           TERM
     C*
     C                   ENDIF
     C*
     C* GET THE COMPANY RECORD AND USER
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVE      Q1USER        SCUSER
     C*
     C*===============================================================
     C* INITIALIZATION    AREA
     C*
     C     TOP           TAG
     C                   EXSR      INZSR
     C                   CLEAR                   SCDATA
     C*
     C*===============================================================
     C*
     C     MAINLN        TAG
     C*
     C*===============================================================
     C*
     C* SCREEN EXECUTION AREA
     C*
     C                   EXSR      SCREXC
     C*
     C*===============================================================
     C*
     C* HANDLE F KEYS IF PRESSED
     C*
     C     *IN11         CASEQ     '1'           FKEYS
     C                   ENDCS                                                    END *IN11
     C*
     C*===============================================================
     C*
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C*
     C     SCSSN         IFNE      *BLANKS
     C     SCADAT        ORNE      *ZEROS
     C     SCFIN#        ORNE      *ZEROS
     C                   MOVEL     SCSSN         XXSS#
     C*
     C                   Z-ADD     SCADAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
     C*
     C                   Z-ADD     SCFIN#        XXFEIN
     C                   GOTO      TOP
     C                   END                                                    REPOSITON
     C*
     C                   Z-ADD     RRN2          SVRRN2            5 0
     C                   READC     WDW01S                                 50
     C*
     C     *IN50         IFEQ      '0'
     C*
     C*                    - If the subfile has changed and the user has
     C*                    - selected a record, then save that data to be
     C*                    - passed back to the calling program.
     C*
     C     QQCALL        IFEQ      '1'
     C     SCSEL         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANKS       QQSS#
     C                   MOVE      *BLANKS       QQACDT
     C                   MOVE      *BLANKS       QQSUB#
     C                   MOVE      *BLANKS       QQFND
     C                   MOVE      *BLANKS       QQCERT
     C                   MOVE      *BLANKS       QQEXT
     C                   MOVE      SCSUB#        QQSUB#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCCERT        QQCERT
     C                   MOVE      SCEXT         QQEXT
     C                   GOTO      TERM
     C                   ELSE                                                   ELSE EQ '1'
     C*
     C*                    - If the subfile has changed but the user did
     C*                    - not use a '1' to indicate a selected record
     C*                    - send an error message back to the screen.
     C*
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   Z-ADD     RRN2          #@$2
     C                   UPDATE    WDW01S
     C                   MOVE      *BLANK        SCSEL
     C                   GOTO      MAINLN
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '1'
     C                   ENDIF                                                  CALL = '1'
     C*
     C*                    - If the subfile has not been changed, return
     C*                    - to the calling program without a selected
     C*                    - record.
     C*
     C                   ELSE
     C                   GOTO      MAINLN
     C                   END                                                    END 0 *IN50
     C*
     C                   GOTO      TOP                                          RE-SET SUBFILE
     C*
     C*===============================================================
     C* TERMINATION OF PROGRAM
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*====================================================================*
     C* * *   INZSR SUBROUTINE                                    I N I T ***
     C*====================================================================*
     C*
     CSR   INZSR         BEGSR
     C*
     C     PCKEY         SETLL     PPC01                              60
     C*
     C* CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     WDW01C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     CSR                 ENDSR
     C*
     C*====================================================================*
     C* * *   LOAD SUBROUTINE                                    L O A D ***
     C*====================================================================*
     C*
     CSR   LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C*
     C*                    - LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C*
     C                   DO        7             X                 5 0
     C                   READ      PPC01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C*
     C     PCSS#         IFEQ      *BLANKS
     C     PCACDT        OREQ      *ZEROS
     C     PCFEIN        OREQ      *ZEROS
     C                   SUB       1             X
     C                   ITER                                                   READ NXT REC
     C                   ENDIF
     C*
     C                   ADD       1             RRN2              5 0
     C*
     C                   Z-ADD     PCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C***  Beg Chg *** 06/09/20 ****************************************
     C***                MOVE      PCSS#         SCSS#
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'PCSS#'       QQNAME           25
     C     SCSS#         PARM      PCSS#         QQVAL           100
     C***  End Chg *** 06/09/20 ****************************************
     C                   Z-ADD     PCFEIN        SCREIN
     C                   MOVE      PCCERT        SCCERT
     C                   Z-ADD     PCEXT         SCEXT
     C                   Z-ADD     PCSUB#        SCSUB#
     C                   Z-ADD     PCFND         SCFND
     C***  Beg Add *** 06/09/20 ****************************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'PCSS#'       QQNAME           25
     C     PCSS#         PARM      SCSS#         QQVAL           100
     C***  End Add *** 06/09/20 ****************************************
     C     PCSS#         CHAIN     PCD01                              60
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
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     WDW01S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C*                    - INZSRIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C*
     C     RRN2          IFEQ      0
     C                   MOVE      *BLANKS       SCCERT
     C                   MOVE      *ZEROS        SCACDT
     C                   MOVE      *BLANKS       SCSS#
     C                   MOVE      *BLANKS       SCNAME
     C                   MOVE      *ZEROS        SCREIN
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     WDW01S
     C                   END
     C*
     C*                    - TO POSITION SCREEN AND CURSOR
     C*
     C                   Z-ADD     RRN2          #@$2
     C*
     CSR                 ENDSR
     C*
     C*===============================================================
     CSR   SCREXC        BEGSR
     C*===============================================================
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
     C     SVRRN2        IFNE      0
     C                   Z-ADD     SVRRN2        RRN2
     C                   Z-ADD     0             SVRRN2
     C                   END
     C                   END
     C*
     CSR                 ENDSR
     C*
     C*===============================================================
     CSR   FKEYS         BEGSR
     C*===============================================================
     C*
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C*
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01S'
     C     Q2RECF        OREQ      'WDW01C'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *BLANKS       QQCERT
     C                   MOVE      *BLANKS       QQEXT
     C                   GOTO      TERM
     C                   END
     C*
     C                   END                                                    END *INKL
     C*
     C****************************************
     C*   *IN18 - ROOLUP REQUEST             *
     C****************************************
     C*
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           MAINLN                         NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      MAINLN
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C                   GOTO      MAINLN
     C*
     CSR                 ENDSR
