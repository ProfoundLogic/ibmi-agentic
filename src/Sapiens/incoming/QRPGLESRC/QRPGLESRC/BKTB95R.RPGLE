     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE BKTB95R- PROGRAM PROFILE MAINTENANCE
     H******************************************************************
     H* INDICATOR USAGE                                                *
     H*  09    INQUIRY ONLY                                            *
     H* SCR01:                                                         *
     H*  20    PROGRAM NAME IS REQUIRED                                *
     H* SCR02:                                                         *
     H*  21    DESCRIPTION IS REQUIRED                                 *
     H*  22    RUN DATE IS NOT VALID                                   *
     H******************************************************************
     FBKTB95F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FBKTBL     UF A E           K DISK
     FBKSYL1    IF   E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
0070 D AUW             S              1    DIM(40)                                WORDS
0071 D TABM#           S              2  0 DIM(12) CTDATA PERRCD(6)               MON
0071 D TABMON          S              3    DIM(12) ALT(TABM#)
0072 D TABD#           S              1  0 DIM(7) CTDATA PERRCD(7)                DAYS
0072 D TABDAY          S              3    DIM(7) ALT(TABD#)
0195 D SCL             S             80    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCALL            1
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
     C*   KEY LIST FOR THE EARNINGS CODE FILE.
     C*
     C     TBLKEY        KLIST
     C                   KFLD                    TBLID
     C                   KFLD                    TBLVAL
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVEL     'PP'          TBLID             2
     C     'BK'          CHAIN     PSY01                              54
     C     *IN54         IFEQ      '0'
     C                   MOVEL     SYNAME        SCCONM
     C                   ELSE
     C                   MOVEL     '*SETUP *'    SCCONM
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL     'PP'          TBLID             2
     C                   MOVEL     Q1PGM         TBLVAL
     C     TBLKEY        CHAIN     PPP01                              54
     C     *IN54         IFEQ      '0'
     C                   EXCEPT    PPP01E
     C                   MOVEL     PPDESC        SCPGNM
     C                   MOVEL     PPCLPW        XXCLPW           10
     C                   MOVEL     PP20PN        XX20PN           10
     C                   MOVEL     PP20PM        XX20PM            1
     C                   MOVEL     PP20PW        XX20PW           10
     C********************************************
     C* PPPW  = IF PASSWORD IS REQUIRED          *
     C********************************************
     C     PPPW          IFNE      *BLANK
     C                   CALL      'BKSE99R'
     C                   PARM                    SCCONM
     C                   PARM                    SCPGNM
     C                   PARM                    PPPW
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END                                                    END PPPW
     C********************************************
     C                   ELSE
     C                   MOVE      *BLANK        XXCLPW           10
     C                   MOVE      *BLANK        XX20PW           10
     C                   MOVE      *BLANK        XX20PM            1
     C                   MOVE      *BLANK        XX20PW           10
     C                   END
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVE      Q2RECF        SCRECF                          FOR DISPLAY
     C                   MOVEL     Q1USER        SCUSER
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C**********************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C********************************************
     C*   F3 = EXIT                              *
     C********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END                                                    END SCR02
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C********************************************
     C*   F5 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKE         IFEQ      '1'
     C********************************************
     C* SCR01 = TABLE FILE SEARCH                *
     C********************************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'BKTI95R'
     C                   PARM                    SCPGNM
     C                   PARM                    X@PGM            10
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     X@PGM         IFGT      *BLANKS
     C                   MOVEL     X@PGM         SCPGM
     C                   END
     C*
     C     QQRTC         IFEQ      'B'
     C     SCPGM         OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END                                                    END SCR01
     C                   END                                                    END *INKE
     C*
     C********************************************
     C*   F20 = HOT KEY PROGRAM REQUEST          *
     C********************************************
     C     *INKU         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C     XX20PN        ANDNE     *BLANK
     C********************************************
     C*  VALIDATE PASSWORD IF REQUIRED           *
     C********************************************
     C     XX20PW        IFNE      *BLANK
     C                   MOVEL     'L'           QQRTC
     C                   CALL      'BKSE99R'
     C                   PARM                    SCCONM
     C                   PARM                    SCPGNM
     C                   PARM                    XX20PW
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'L'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END XX20PW
     C********************************************
     C* F20 = CALL REQUESTED PROGRAM             *
     C********************************************
     C*
     C                   CALL      XX20PN
     C                   PARM      XX20PM        QQRTC
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END                                                    END XXPN
     C*
     C********************************************
     C*   F23 = COMMAND LINE REQUEST             *
     C********************************************
     C     *INKX         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C********************************************
     C*   PW  = IF PASSWORD IS REQUIRED          *
     C********************************************
     C     XXCLPW        IFNE      *BLANK
     C                   MOVEL     'L'           QQRTC
     C                   CALL      'BKSE99R'
     C                   PARM                    SCCONM
     C                   PARM                    SCPGNM
     C                   PARM                    XXCLPW
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'L'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   Z-ADD     9             SCLSTR           15 5
     C                   CALL      'QCMDEXC '
     C                   PARM                    SCL(1)
     C                   PARM                    SCLSTR
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END                                                    END COMMAND
     C**
     C                   END                                                    END CMDKEYS
     C**                                                               ********
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C********************************************
     C*  PROGRAM NAME MUST BE NOT EQUAL TO BLANKS*
     C********************************************
     C*
     C     SCPGM         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL     SCPGM         TBLVAL           10
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     TBLKEY        CHAIN     PPP01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PPP01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      *BLANK        SCWDDT
     C                   MOVEL     PPDESC        SCDESC
     C                   MOVEL     PPPW          SCPW
     C*
     C                   Z-ADD     PPRUN         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRUN
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
     C                   MOVEL     PPLOG         SCLOG
     C                   MOVEL     PPCLPW        SCCLPW
     C                   MOVEL     PP20PN        SC20PN
     C                   MOVEL     PP20PM        SC20PM
     C                   MOVEL     PP20PW        SC20PW
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCPW
     C                   MOVE      *BLANKS       SCRUN
     C                   MOVE      *BLANKS       SCLOG
     C                   MOVE      *BLANKS       SCCLPW
     C                   MOVE      *BLANKS       SC20PN
     C                   MOVE      *BLANKS       SC20PM
     C                   MOVE      *BLANKS       SC20PW
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C********************************************
     C*  DESCRIPTION CANNOT BE EQUAL TO BLANKS   *
     C********************************************
     C*
     C     SCDESC        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C********************************************
     C*  TEST THE RUN DATE.                      *
     C********************************************
     C*
     C     SCRUN         IFNE      *ZEROS
     C*
     C                   Z-ADD     SCRUN         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRUN
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     TBLKEY        CHAIN     PPP01                              40
     C*
     C                   MOVE      'PP'          PPTBL
     C                   MOVEL     SCPGM         PPVAL
     C                   MOVEL     SCDESC        PPDESC
     C                   MOVEL     SCPW          PPPW
     C*
     C                   Z-ADD     SCRUN         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PPRUN
     C*
     C                   MOVEL     SCLOG         PPLOG
     C                   MOVEL     SCCLPW        PPCLPW
     C                   MOVEL     SC20PN        PP20PN
     C                   MOVEL     SC20PM        PP20PM
     C                   MOVEL     SC20PW        PP20PW
     C*
     C                   MOVE      Q1USER        PPCUSR
     C                   MOVEL     Q1PGM         PPIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PPCDT
     C*
     C                   Z-ADD     DSTIME        PPCTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PPP01
     C                   ELSE
     C                   MOVE      Q1USER        PPIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PPIDT
     C*
     C                   Z-ADD     DSTIME        PPITIM
     C                   WRITE     PPP01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCPGM
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCPW
     C                   MOVE      *BLANKS       SCRUN
     C                   MOVE      *BLANKS       SCLOG
     C                   MOVE      *BLANKS       SCCLPW
     C                   MOVE      *BLANKS       SC20PN
     C                   MOVE      *BLANKS       SC20PM
     C                   MOVE      *BLANKS       SC20PW
     C                   GOTO      STRT
     C*
     C********************************************
     C*  END THE PROGRAM                         *
     C********************************************
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
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
     OPPP01     E            PPP01E
**  TABLE UA
-- ADD --
- UPDATE -
**
01JAN02FEB03MAR04APR05MAY06JUN
07JUL08AUG09SEP10OCT11NOV12DEC
**
0WED1THU2FRI3SAT4SUN5MON6TUE
**   CALLED PROCEDURES
CALL QCMD
