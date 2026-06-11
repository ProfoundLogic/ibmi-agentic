     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE BKTI95R- PROGRAM PROFILE SEARCH                          *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*****************************************************************
     FBKTI95F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F                                     SFILE(SCR04S:SX)
     F*
     FBKTBL     UF   E           K DISK
     FBKSYL1    IF   E           K DISK

     D*
0073 D*                         *************************************
0074 D*                         * DAYS/ MONTHS                      *
0075 D*                         *************************************
0070 D AUW             S              1    DIM(40)                                WORDS
0071 D TABM#           S              2  0 DIM(12) CTDATA PERRCD(6)               MON
0071 D TABMON          S              3    DIM(12) ALT(TABM#)
0072 D TABD#           S              1  0 DIM(7) CTDATA PERRCD(7)                DAYS
0072 D TABDAY          S              3    DIM(7) ALT(TABD#)
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
     C     *LIKE         DEFINE    SCPGM         XXPGM
     C     *LIKE         DEFINE    SCPGM         X@PGM
     C*
     C*   KEY LIST FOR THE TABLE FILE
     C*
     C     TBLKEY        KLIST
     C                   KFLD                    SCTBL
     C                   KFLD                    SCVAL
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    SCPGNM           30
     C                   PARM                    X@PGM
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
     C                   MOVEL     'PP'          SCTBL             2
     C     'BK'          CHAIN     PSY01                              54
     C     *IN54         IFEQ      '0'
     C                   MOVEL     SYNAME        SCCONM
     C                   ELSE
     C                   MOVEL     '*SETUP *'    SCCONM
     C                   END
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C*
     C                   MOVEL     SCPGM         SCVAL            10
     C                   MOVE      '0'           *IN59
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C     TBLKEY        SETLL     PPP01                              60
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
     C     SCPGM         IFNE      *BLANKS
     C                   GOTO      TOP
     C                   END                                                    END SCUCID NE BLKS
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C                   MOVE      '0'           *IN59
     C                   Z-ADD     *ZERO         SX
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR04C
     C                   MOVE      '0'           *IN04
     C     TAGRC         TAG
     C*
     C                   READC     SCR01S                                 50
     C     *IN50         IFEQ      '0'
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
     C     SCSEL         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   MOVEL     SCPGM         X@PGM
     C                   ELSE                                                   ELSE EQ '1'
     C*
     C     SCSEL         IFEQ      '4'
     C                   ADD       1             DELETS            1 0
     C     SX            IFLT      999
     C                   ADD       1             SX
     C                   WRITE     SCR04S
     C                   END
     C     SX            IFEQ      1
     C                   MOVE      SCPGM         XXPGM
     C                   END
     C                   GOTO      TAGRC
     C                   ELSE                                                   ELSE EQ '4'
     C     SCSEL         IFEQ      '5'
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVEL     SCPGM         SCVAL
     C     TBLKEY        CHAIN     PPP01                              40
     C  N40              EXCEPT    PPREL
     C                   MOVEL     PPDESC        SCDESC
     C                   MOVEL     PPPW          SCPW
     C                   MOVEL     PPLOG         SCLOG
     C*
     C                   Z-ADD     PPRUN         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRUN
     C*
     C                   MOVE      *BLANKS       SCWDDT
     C*
     C********************************************
     C*  GET THE DATE WORD SUBR IN ACTION.       *
     C********************************************
     C*
     C     SCRUN         IFGT      *ZERO
     C                   Z-ADD     SCRUN         W6DATE            8 0
     C                   EXSR      AUDWRD
     C                   MOVEA     AUW           SCWDDT
     C                   END
     C*
     C                   MOVEL     PPCLPW        SCCLPW
     C                   MOVEL     PP20PN        SC20PN
     C                   MOVEL     PP20PM        SC20PM
     C                   MOVEL     PP20PW        SC20PW
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
     C                   END
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   UPDATE    SCR01S
     C                   MOVE      ' '           SCSEL
     C                   GOTO      STRT
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '1'
     C*** IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
     C*** WITHOUT A SELECTED RECORD
     C                   ELSE
     C**
     C***                                                              INDICATE
     C**
     C     *IN59         IFEQ      '1'
     C                   MOVE      ' '           SCSEL
     C                   GOTO      TOP
     C                   END
     C**
     C*** CHECK FOR DELETE CONFIRMATION SCREEN DISPLAY                 INDICATE
     C     SX            IFGT      *ZERO
     C                   MOVE      '1'           *IN01
     C                   Z-ADD     SX            #@$4
     C                   WRITE     SCR04
     C                   EXFMT     SCR04C
     C                   MOVE      '0'           *IN01
     C     *INKL         IFEQ      '1'
     C                   MOVE      *BLANKS       SCVAL
     C                   MOVE      *BLANKS       SCPGM
     C                   MOVE      ' '           SCSEL
     C                   GOTO      TOP
     C                   END
     C*
     C                   SETOFF                                       60
     C                   Z-ADD     *ZERO         SX                5 0
     C     *IN60         DOUEQ     '1'
     C                   ADD       1             SX
     C     SX            IFLE      999
     C     SX            CHAIN     SCR04S                             60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     'PP'          SCTBL             2
     C                   MOVEL     SCPGM         SCVAL            10
     C     TBLKEY        CHAIN     BKTBL                              61
     C  N61              DELETE    PPP01
     C                   END
     C                   ELSE
     C                   SETON                                        60
     C                   END
     C*
     C                   END
     C                   MOVE      *BLANKS       SCVAL
     C                   MOVE      *BLANKS       SCPGM
     C                   MOVE      ' '           SCSEL
     C                   GOTO      TOP
     C                   END                                                    END DELETS
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
     C                   READ      PPP01                                  60
     C  N60              EXCEPT    PPREL
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
     C                   MOVEL     PPVAL         SCPGM
     C                   MOVEL     PPDESC        SCDESC
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C*
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANKS       SCPGM
     C                   MOVE      *BLANKS       SCDESC
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     1             #@$2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01S
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   MOVEL     *BLANK        SCPGM
     C                   MOVE      *BLANKS       SCDESC
     C*
     C                   ENDSR
0616 C************************  *************************************
0617 C* AUDWRD SUBROUTINE    *  * DATE/WORD SUBROUTINE              *
0618 C************************  *************************************
0619 C     AUDWRD        BEGSR
0620 C*
0621 C                   MOVE      '0'           XX40              1
0622 C                   MOVE      '0'           XX41              1
0623 C                   MOVE      '0'           XX42              1
0624 C*
0625 C   40              MOVE      '1'           XX40
0626 C   41              MOVE      '1'           XX41
0627 C   42              MOVE      '1'           XX42
0628 C*
0629 C                   Z-ADD     *ZERO         AUJYD
0630 C                   Z-ADD     *ZERO         AUJD
0631 C                   MOVE      *BLANK        AUW
0632 C                   MOVEL     W6DATE        AUMD              4 0
0633 C                   MOVE      W6DATE        AUY1              2 0
0634 C                   MOVEL     AUMD          AUM1              2 0
0635 C                   MOVE      AUMD          AUD1              2 0
0636 C*
0637 C                   Z-ADD     1900          AUYYYY            4 0
0638 C                   ADD       AUY1          AUYYYY
0639 C*
0640 C     AUM1          SUB       3             AUWKMM            2 0  4041
0641 C   40              ADD       12            AUWKMM
0642 C  N41AUWKMM        MULT(H)   30.6          AUJD              7 0
0643 C                   ADD       AUD1          AUJD
0644 C     AUYYYY        SUB       1900          AUWKYY            3 0    42
     C   40
0645 CANN42              SUB       1             AUWKYY
0646 C     AUWKYY        MULT      365.25        AUJYD             7 0
0647 C                   ADD       AUJYD         AUJD
0648 C     AUJD          DIV       7             AUWK7             7 0
0649 C                   MVR                     AUPTR             1 0
0650 C*          AUPTR     ADD  1         AUPTR
0651 C*
0652 C     AUM1          LOOKUP    TABM#         TABMON                   40
0653 C     AUPTR         LOOKUP    TABD#         TABDAY                   40
0654 C*
0655 C                   Z-ADD     1             I                 3 0
0656 C                   MOVEA     TABDAY        AUW(1)
0657 C     ' '           LOOKUP    AUW(I)                                 40
0658 C                   ADD       1             I
0659 C                   MOVEA     TABMON        AUW(I)
0660 C     ' '           LOOKUP    AUW(I)                                 40
0661 C                   ADD       1             I
0662 C                   MOVE      AUD1          AXD1              2
0662 C                   MOVEA     AXD1          AUW(I)
0663 C     ' '           LOOKUP    AUW(I)                                 40
0664 C                   MOVEA     ','           AUW(I)
0665 C                   ADD       1             I
0666 C                   MOVE      AUYYYY        AXYYYY            4
0666 C                   MOVEA     AXYYYY        AUW(I)
0667 C*
0668 C     XX40          COMP      '1'                                    40
0669 C     XX41          COMP      '1'                                    41
0670 C     XX42          COMP      '1'                                    42
0671 C                   ENDSR
     OPPP01     E            PPREL
**
01JAN02FEB03MAR04APR05MAY06JUN
07JUL08AUG09SEP10OCT11NOV12DEC
**
0WED1THU2FRI3SAT4SUN5MON6TUE
