     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE BKI002R - SYSTEM DATE MASTER MAINTENANCE
     H******************************************************************
     H******************************************************************
     FBKI002F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FBKDTL1    UF A E           K DISK
     FBKSYL1    IF   E           K DISK

0076 D*
     D UA              S             10    DIM(3) CTDATA PERRCD(1)
     D OP              S             30    DIM(4) CTDATA PERRCD(1)
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
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
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
     C                   TIME                    DSDTTM
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN34
     C     'BK'          CHAIN     PSY01                              54
     C                   MOVEL     '1'           SCCODE
     C     QQCALL        IFEQ      'G'
     C                   MOVEL     '2'           SCCODE
     C                   END
     C     QQCALL        IFEQ      'P'
     C                   MOVEL     '3'           SCCODE
     C                   END
     C     QQCALL        IFEQ      'T'
     C                   MOVEL     '4'           SCCODE
     C                   END
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C                   MOVEL     Q1USER        SCUSER
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR21'
     C                   EXFMT     SCR21
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR22'
     C                   EXFMT     SCR22
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR23'
     C                   EXFMT     SCR23
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR24'
     C                   EXFMT     SCR24
     C                   END
     C***                                                              *****
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
     C     Q2RECF        IFEQ      'SCR21'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END                                                    END SCR21
     C     Q2RECF        IFEQ      'SCR22'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END                                                    END SCR22
     C     Q2RECF        IFEQ      'SCR23'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END                                                    END SCR23
     C     Q2RECF        IFEQ      'SCR24'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END                                                    END SCR23
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C**
     C                   END                                                    END CMDKEYS
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR21'       SCR21T
     C     Q2RECF        CABEQ     'SCR22'       SCR22T
     C     Q2RECF        CABEQ     'SCR23'       SCR23T
     C     Q2RECF        CABEQ     'SCR24'       SCR24T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  TEST THE ACTION CODE FOR 1,2, OR 3.
     C*
     C     SCCODE        IFNE      '1'
     C     SCCODE        ANDNE     '2'
     C     SCCODE        ANDNE     '3'
     C     SCCODE        ANDNE     '4'
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END
     C*
     C     SCCODE        COMP      '2'                                080607
     C     SCCODE        COMP      '4'                                    05
     C   06              MOVE      OP(1)         SCOPTN
     C   06              MOVE      'AP'          APPKEY            2
     C   07              MOVE      OP(2)         SCOPTN
     C   07              MOVE      'GL'          APPKEY
     C   08              MOVE      OP(3)         SCOPTN
     C   08              MOVE      'PR'          APPKEY
     C   05              MOVE      OP(4)         SCOPTN
     C   05              MOVE      'TA'          APPKEY
     C*
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     APPKEY        CHAIN     PDT01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PDT01E
     C*
     C     *IN09         IFEQ      '0'
     C                   MOVE      UA(2)         SCMODE
     C                   ELSE
     C                   MOVE      UA(3)         SCMODE
     C                   END
     C*
     C                   Z-ADD     DTAPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCAPDT
     C*
     C                   Z-ADD     DTCDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCDT
     C*
     C                   Z-ADD     DTCTIM        SCCTIM
     C                   MOVEL     DTCUSR        SCCSER
     C                   MOVEL     DTIPGM        SCIPGM
     C*
     C*  GET THE DATE WRORD SUBR IN ACTION.
     C*
      *** BEG CHG * V3
     C*****                Z-ADDSCAPDT    W6DATE  60
     C                   Z-ADD     SCAPDT        W8DATE            8 0
      *** BEG CHG * V3
     C                   EXSR      AUDWRD
     C                   MOVEA     AUW           SCWDAT
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   MOVE      UA(3)         SCMODE
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCWDAT
     C                   MOVE      *ZEROS        SCAPDT
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C   06              MOVEL     'SCR21'       Q2RECF
     C   07              MOVEL     'SCR22'       Q2RECF
     C   08              MOVEL     'SCR23'       Q2RECF
     C   05              MOVEL     'SCR24'       Q2RECF
     C*
     C                   GOTO      STRT
     C*
     C     SCR21T        TAG
     C*
     C*
     C*  PROCESSING DATE MUST BE VALID.
     C*
     C     SCAPDT        IFEQ      *ZERO
     C                   MOVE      '1'           *IN52
     C                   GOTO      STRT
     C                   ELSE
     C*
     C                   Z-ADD     SCAPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCAPDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN52
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     APPKEY        CHAIN     PDT01                              40
     C*
     C                   MOVE      APPKEY        DTAPP
     C*
     C                   Z-ADD     SCAPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DTAPDT
     C*
     C                   MOVE      Q1USER        DTCUSR
     C                   MOVE      Q1PGM         DTIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DTCDT
     C*
     C                   Z-ADD     DSTIME        DTCTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PDT01
     C                   ELSE
     C                   MOVE      Q1USER        DTIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DTIDT
     C*
     C                   Z-ADD     DSTIME        DTITIM
     C                   WRITE     PDT01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR22T        TAG
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     APPKEY        CHAIN     PDT01                              40
     C*
     C                   Z-ADD     SCAPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DTAPDT
     C*
     C                   MOVE      APPKEY        DTAPP
     C*
     C                   MOVE      Q1USER        DTCUSR
     C                   MOVE      Q1PGM         DTIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DTCDT
     C*
     C                   Z-ADD     DSTIME        DTCTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PDT01
     C                   ELSE
     C                   MOVE      Q1USER        DTIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DTIDT
     C*
     C                   Z-ADD     DSTIME        DTITIM
     C                   WRITE     PDT01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*
     C     SCR24T        TAG
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     APPKEY        CHAIN     PDT01                              40
     C*
     C                   Z-ADD     SCAPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DTAPDT
     C*
     C                   MOVE      APPKEY        DTAPP
     C*
     C                   MOVE      Q1USER        DTCUSR
     C                   MOVE      Q1PGM         DTIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DTCDT
     C*
     C                   Z-ADD     DSTIME        DTCTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PDT01
     C                   ELSE
     C                   MOVE      Q1USER        DTIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DTIDT
     C*
     C                   Z-ADD     DSTIME        DTITIM
     C                   WRITE     PDT01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR23T        TAG
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     APPKEY        CHAIN     PDT01                              40
     C*
     C                   Z-ADD     SCAPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DTAPDT
     C*
     C                   MOVE      APPKEY        DTAPP
     C*
     C                   MOVE      Q1USER        DTCUSR
     C                   MOVE      Q1PGM         DTIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DTCDT
     C*
     C                   Z-ADD     DSTIME        DTCTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PDT01
     C                   ELSE
     C                   MOVE      Q1USER        DTIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DTIDT
     C*
     C                   Z-ADD     DSTIME        DTITIM
     C                   WRITE     PDT01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C/EJECT
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
      *** BEG CHG * V3
0632 C*****                MOVELW6DATE    AUMD    40
0633 C*****                MOVE W6DATE    AUY1    20
     C                   MOVEL     W8DATE        AUMD              4 0
     C                   MOVE      W8DATE        AUYYYY            4 0
      *** END CHG * V3
0634 C                   MOVEL     AUMD          AUM1              2 0
0635 C                   MOVE      AUMD          AUD1              2 0
0636 C*
      *** BEG DEL * V3
0637 C*****                Z-ADD1900      AUYYYY  40
0638 C*****                ADD  AUY1      AUYYYY
      *** END DEL * V3
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
     OPDT01     E            PDT01E
**  TABLE UA
-- ADD --
- UPDATE -
- INQUIRY-
**  TABLE OP OPTION SELECTED.
 1. Accounts Payable
 2. General Ledger
 3. Payroll
 4. Time & Attendance
**
01JAN02FEB03MAR04APR05MAY06JUN
07JUL08AUG09SEP10OCT11NOV12DEC
**
0WED1THU2FRI3SAT4SUN5MON6TUE
