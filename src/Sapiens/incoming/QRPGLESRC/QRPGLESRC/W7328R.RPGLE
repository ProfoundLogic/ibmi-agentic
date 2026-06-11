     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7328R - PROMPT FOR SUMMARY INSTALLMENT SCHEDULE
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:                                                 *
     H*    This program prompts for policy year, employer number,     *
     H*    installments posted on or after date and three comment     *
     H*    codes.  It then passes this info on so as to produce a     *
     H*    corresponding report.                                      *
     H*                                                               *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    Menu                                                       *
     H*  CALLS TO:                                                    *
     H*    W7328C                                                     *
     H*                                                               *
     H*  CREATED: 10/22/98  PROGRAMMER- C:87  PROJECT: 96WAO          *
     H*                                                               *
     H*****************************************************************
     H*   INDICATORS:                                                 *
     H*                                                               *
     H*   60 - General chain                                          *
     H*   71 - No employer selected                                   *
     H*   72 - Valid policy year for employer                         *
     H*   73 - Valid date                                             *
     H*   74 - Valid comment 1                                        *
     H*   75 - Valid comment 2                                        *
     H*   76 - Valid comment 3                                        *
     H*   90 - Controls display of screen, set on for error           *
     H*                                                               *
     H*****************************************************************
     H*   REVISIONS:                                                  *
     H*                                                               *
     H*****************************************************************
     F*
     FW7328F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMEML     IF   E           K DISK
     F                                     INFDS(DBFDSD)
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     FWDELL1    IF   E           K DISK
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL02)
     FWTABL     IF   E           K DISK

     D*
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDSD          DS
     D  DBRRND               397    400B 0
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*-----------------------------------------------------------------
     C*  KEY LISTS
     C*-----------------------------------------------------------------
     C*    Company file
     C     COKY          KLIST
     C                   KFLD                    UPCO#
     C*
     C*    User parameter file
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*    Fund master file
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*    Employer master file
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C*
     C*    Employer detail file
     C     ELKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEMP#
     C*
     C*    Table file
     C     TBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C*-----------------------------------------------------------------
     C*  PARM LISTS
     C*-----------------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
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
     C*-----------------------------------------------------------------
     C*  *LIKE DEFINES
     C*-----------------------------------------------------------------
     C     *LIKE         DEFINE    ELCO#         KKCO#
     C     *LIKE         DEFINE    ELFND         KKFND
     C     *LIKE         DEFINE    ELFYR         KKFYR
     C     *LIKE         DEFINE    ELEMP#        KKEMP#
     C     *LIKE         DEFINE    BCKEY         KKKEY
     C*
     C*
     C*  Get the user parameter file
     C     UPKY          CHAIN     PUP01                              60
     C  N60              Z-ADD     UPCO#         KKCO#
     C   60              Z-ADD     *ZERO         KKCO#
     C                   MOVE      UPFND         SCFND
     C*
     C*  Get company and fund info
     C                   EXSR      GETCOF
     C*
     C*----------------------------------------------------------------
     C*  Process the screen
     C*----------------------------------------------------------------
B01  C     *IN90         DOUEQ     *OFF
     C     *INKC         OREQ      *ON
     C*
     C*  Output the screen
     C                   EXFMT     SCR01
     C*
     C*    Process all command keys
B02  C     *IN11         IFEQ      *ON
     C*
     C*      Do not process if F3=Exit is pressed
B03  C     *INKC         IFEQ      *OFF
     C*
     C*      F10=Search
B04  C     *INKJ         IFEQ      *ON
     C*
     C     SCDATE        IFNE      *ZERO
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFNE      '1'
     C                   MOVEL     SRDTO         SCDATE
     C                   ELSE
     C                   SETON                                        73
     C                   ENDIF                                                  SRERR<>'1'
     C                   ENDIF                                                  SCDATE<>*BLANK
     C*
     C                   SETON                                        90
     C*
     C                   SELECT
     C*
     C*        Policy year search
     C     HFLD          WHENEQ    'SCFYR'
     C*
     C     SCEMP#        IFEQ      *ZERO
     C                   EXSR      SRESRC
     C                   ELSE
     C                   EXSR      SRPSRC
     C                   ENDIF                                                  SCEMP#=*ZERO
     C*
     C*        Employer number search
     C     HFLD          WHENEQ    'SCEMP#'
     C*
     C                   EXSR      SRESRC
     C*
     C*        Comment code search
     C     HFLD          WHENEQ    'SCCOM1'
     C     HFLD          OREQ      'SCCOM2'
     C     HFLD          OREQ      'SCCOM3'
     C*
     C                   CALL      'W2191R'
     C                   PARM      '2'           Q2CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      *BLANKS       QQKEY            10
     C*
B11  C     *INKC         IFEQ      *OFF
     C*
B12  C     *INKL         IFEQ      *ON
     C     QQKEY         OREQ      *BLANKS
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCCOM1'
     C                   MOVE      *ON           *IN84
     C*
     C     HFLD          WHENEQ    'SCCOM2'
     C                   MOVE      *ON           *IN85
     C*
     C     HFLD          WHENEQ    'SCCOM3'
     C                   MOVE      *ON           *IN86
     C*
     C                   ENDSL
     C*
E12  C                   ENDIF                                                  *INKL=*ON
     C*
B13  C     QQKEY         IFNE      *BLANKS
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCCOM1'
     C                   MOVE      *ON           *IN85
     C                   MOVEL     QQKEY         SCCOM1
     C*
     C     HFLD          WHENEQ    'SCCOM2'
     C                   MOVE      *ON           *IN86
     C                   MOVEL     QQKEY         SCCOM2
     C*
     C     HFLD          WHENEQ    'SCCOM3'
     C                   MOVE      *ON           *IN82
     C                   MOVEL     QQKEY         SCCOM3
     C*
     C                   ENDSL
     C*
E13  C                   ENDIF                                                  QQKEY<>*BLANKS
     C*
E11  C                   ENDIF                                                  *INKC=*OFF
     C*
     C                   ENDSL
     C*
E04  C                   ENDIF                                                  *INKJ=*ON
     C*
     C*      F13=Chg user parms
     C     *INKM         IFEQ      *ON
     C                   SETON                                        90
     C*
     C                   CALL      'W0006R'
     C*
     C*        Get the user parameter file
     C                   MOVE      *ZERO         KKCO#
     C     UPKY          CHAIN     PUP01                              60
     C  N60              MOVE      UPCO#         KKCO#
     C*
     C                   EXSR      GETCOF
     C*
     C                   ENDIF                                                  *INKM=*ON
     C*
E03  C                   ENDIF                                                  *INKC=*OFF
     C*
E02  C                   ENDIF                                                  *IN11=*ON
     C*
     C*    Check for screen errors
     C     *IN90         IFEQ      *OFF
     C*
     C                   EXSR      ERRCK1
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
E01  C                   ENDDO                                                  *IN90=*ON
     C*
     C*----------------------------------------------------------------
     C*  CALL THE CLP TO SUBMIT THE JOB - if F3=Exit is not pressed
     C*----------------------------------------------------------------
     C     *INKC         IFEQ      *OFF
     C*
     C                   MOVE      UPCO#         QQCO#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCEMP#        QQEMP#
     C                   MOVE      SCDATE        QQDATE
     C*
     C                   CALL      'W7328C'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDATE            8
     C                   PARM      SCCOM1        QQCOM1            2
     C                   PARM      SCCOM2        QQCOM2            2
     C                   PARM      SCCOM3        QQCOM3            2
     C                   PARM      UPOUTQ        QQOUTQ           10
     C                   PARM      'D'           QQMODE            1
     C                   PARM                    QQCALL
     C*
     C                   ENDIF                                                  *INKC=*OFF
     C*
     C                   MOVE      *ON           *INLR
     C*
     C*================================================================
     C*  GETCOF - Gets the company and fund names
     C*================================================================
     C     GETCOF        BEGSR
     C*
     C*  Get the company record
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVE      UPFND         SCFND
     C*
     C*  Get the fund name
     C                   Z-ADD     SCFND         KKFND
     C                   MOVE      *BLANK        SCFNAM
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVE      FNNAME        SCFNAM
     C*
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C                   ENDSR                                                  GETCOF
     C*================================================================
     C*  ERRCK1 - Checks for errors on SCR01
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*    Validate employer number
     C     SCEMP#        IFEQ      *ZERO
     C                   SETON                                        91  90
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SCEMP#        KKEMP#
     C     EMKY          CHAIN     PEM01                              60
     C   60              SETON                                        71  90
     C*
     C                   ENDIF                                                  SCEMP#=*BLANK
     C*
     C*    Validate policy year
     C  N90SCFYR         IFEQ      *ZERO
     C                   SETON                                        92  90
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SCFYR         KKFYR
     C     ELKY          CHAIN     PEL01                              60
     C   60              SETON                                        72  90
     C*
     C                   ENDIF                                                  SCFYR=*BLANK
     C*
     C*    Validate date
     C  N90SCDATE        IFNE      *ZERO
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFNE      '1'
     C                   MOVEL     SRDTO         SCDATE
     C                   ELSE
     C                   SETON                                        73  90
     C                   ENDIF                                                  SRERR
     C*
     C                   ENDIF                                                  SCDATE<>*BLANK
     C*
     C*    Validate comments
     C*        Comment field 1
     C  N90SCCOM1        IFNE      *BLANK
     C                   MOVEL(P)  SCCOM1        KKKEY
     C     TBKY          CHAIN     PBC01                              74
     C     *IN74         IFEQ      *ON
     C     BCSTAT        OREQ      'I'
     C                   SETON                                        74  90
     C                   ENDIF                                                  *IN22=*ON
     C                   ENDIF                                                  SCCOM1<>*BLANK
     C*
     C*        Comment field 2
     C  N90SCCOM2        IFNE      *BLANK
     C                   MOVEL(P)  SCCOM2        KKKEY
     C     TBKY          CHAIN     PBC01                              75
     C     *IN75         IFEQ      *ON
     C     BCSTAT        OREQ      'I'
     C                   SETON                                        75  90
     C                   ENDIF                                                  *IN23=*ON
     C                   ENDIF                                                  SCCOM2<>*BLANK
     C*
     C*        Comment field 3
     C  N90SCCOM3        IFNE      *BLANK
     C                   MOVEL(P)  SCCOM3        KKKEY
     C     TBKY          CHAIN     PBC01                              76
     C     *IN76         IFEQ      *ON
     C     BCSTAT        OREQ      'I'
     C                   SETON                                        76  90
     C                   ENDIF                                                  *IN24=*ON
     C                   ENDIF                                                  SCCOM3<>*BLANK
     C*
     C                   ENDSR                                                  ERRCK1
     C*================================================================
     C*  SRESRC - Search for employer number subroutine
     C*================================================================
     C     SRESRC        BEGSR
     C*
     C                   Z-ADD     2             QQVIEW            1 0
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '2'           Q2CALL            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM      DBRRN         QQRRN2            9 0
     C                   PARM                    QQQTE             1
     C                   PARM                    QQVIEW
     C*
     C     *INKC         IFEQ      *OFF
     C*
     C*
     C     *INKL         IFEQ      *ON
     C     QQRRN         OREQ      *ZERO
     C     HFLD          IFEQ      'SCFYR'
     C                   SETON                                        81
     C                   ELSE
     C                   SETON                                        82
     C                   ENDIF                                                  HFLD='SCFYR'
     C                   ENDIF                                                  *INKL=*ON
     C*
     C     *INKL         IFEQ      *OFF
     C     QQRRN         IFNE      *ZERO
     C     QQRRN         CHAIN     PEM02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     EMEMP#        SCEMP#
     C                   SETON                                        83
     C*
     C     HFLD          IFEQ      'SCFYR'
     C     SCFYR         OREQ      *ZERO
     C                   EXSR      SRPSRC
     C                   ENDIF                                                  HFLD='SCFYR'
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  QQRRN<>*ZERO
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C                   ENDIF                                                  *INKC=*OFF
     C*
     C                   ENDSR                                                  SRESRC
     C*================================================================
     C*  SRPSRC - Search for policy year subroutine
     C*================================================================
     C     SRPSRC        BEGSR
     C*
     C*    Validate employer number
     C                   Z-ADD     SCEMP#        KKEMP#
     C     EMKY          CHAIN     PEM01                              60
     C   60              SETON                                        71
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     2             QQVIEW            1 0
     C*
     C                   CALL      'W0051R'
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '2'           Q2CALL            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM      DBRRND        QQRRN2            9 0
     C                   PARM                    QQQTE             1
     C*
     C     *INKC         IFEQ      *OFF
     C*
     C*
     C     *INKL         IFEQ      *ON
     C     QQRRN         OREQ      *ZERO
     C                   SETON                                        81
     C                   ENDIF                                                  *INKL=*ON
     C*
     C     *INKL         IFEQ      *OFF
     C     QQRRN         IFNE      *ZERO
     C     QQRRN         CHAIN     PEL02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     ELFYR         SCFYR
     C                   SETON                                        83
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  QQRRN<>*ZERO
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C                   ENDIF                                                  *INKC=*OFF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  SRPSRC
