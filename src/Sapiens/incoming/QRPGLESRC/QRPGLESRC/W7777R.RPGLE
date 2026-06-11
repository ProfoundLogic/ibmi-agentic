     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7777R - RESERVE HISTORY REPORT - PROMPT
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*   THIS PROGRAM PROMPTS FOR FYR, RESERVE FROM/TO DATES,        *
     H*   EMPLOYER/DIV, CASE, CLAIM STATUS, AND LOST TIME FLAG.       *
     H*   AFTER VALIDATING THE VALUES, IT WILL SUBMIT THE RESERVE     *
     H*   HISTORY REPORT FOR PROCESSING.                              *
     H* CALLED FROM:                                                  *
     H*   MENU                                                        *
     H* CALLS TO:                                                     *
     H*   W7777C                                                      *
     H* PROGRAMMER: C:57                                              *
     H* CREATED: 03/19/97               PROJECT: 68AYS                *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  05/15/01  C:136 PROJECT: WC577                               *
     H*                  Display location of output file.             *
     H*                                                               *
     H* 06/28/00 C:15  Project: WC577                                 *
     H*                Chgd QQDIV from 3A to 5A.                      *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H*  20 WCR0517  FYR INV FROM DATE                                *
     H*  21 WCR0500  INVALID FROM DATE                                *
     H*  22 WCR0500  INVALID TO DATE                                  *
     H*  23 WCR0750  FROM DATE GREATER THAN TO DATE                   *
     H*  24 WCR0538  EMP/DIV NOT IN FUND                              *
     H*  25 WCR0505  EMP/DIV/FYR INVALID                              *
     H*  26 WCR1190  FYR REQUIRED FOR CASE SELECTION                  *
     H*  27 WCR0786  CASE INVALID                                     *
     H*  28 WCR0715  CLAIM STATUS INVALID                             *
     H*  29 WCR0501  LOST TIME FLAG INVALID                           *
     H*  60          GENERAL INDICATOR USED TO CHAIN TO FILES         *
     H*****************************************************************
     F*
     FW7777F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
      **** BEGIN ADD **** 06/30/98 ************************************
     F                                     INFDS(DBFDS1)
      **** END   ADD **** 06/30/98 ************************************
     FWDF1L     IF   E           K DISK
     FWDELL     IF   E           K DISK
      **** BEGIN CHG **** 06/12/98 ************************************
     F********* FWMCMLR1 IF  E           K        DISK
     FWMCML1    IF   E           K DISK
      **** END   CHG **** 06/12/98 ************************************
      **** BEGIN ADD **** 06/12/98 ************************************
     FWMEMP     IF   E             DISK
     FWMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PCM02)
      **** END   ADD **** 06/12/98 ************************************

     D*
     D MSG             S             44    DIM(1) CTDATA PERRCD(1)
     D*
     D***  BEGIN ADD  ***  05/15/01  *********************************
     D OPTSEL          C                   CONST(', if selected.')
     D***  END   ADD  ***  05/15/01  *********************************
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
      **** BEGIN ADD **** 06/12/98 ************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS1          DS
     D  DBRRN2               397    400B 0
      **** END   ADD **** 06/12/98 ************************************

     C*
     C********************************************************************
     C*  FIELD DEFINITIONS
     C********************************************************************
     C     *LIKE         DEFINE    UPOUTQ        XXOUTQ
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C*
     C********************************************************************
     C*  KLIST DEFINITIONS
     C********************************************************************
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  FUND DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCFYR
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCFYR
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*
     C********************************************************************
     C*  ENTRY PARAMETERS
     C********************************************************************
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
     C********************************************************************
     C*  PROGRAM SETUP
     C********************************************************************
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         XXCO#
     C                   MOVE      *ZERO         XXFND
     C                   ELSE
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPFND         XXFND
     C                   MOVE      UPOUTQ        XXOUTQ
     C                   ENDIF
     C*
     C********************************************************************
     C*  MAINLINE
     C********************************************************************
     C     STRT01        TAG
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE FUND NAME
     C*
     C                   MOVE      XXFND         SCFND
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVE      FNNAME        SCNAME
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C     STRT          TAG
     C*
     C*** BEG ADD ******** 05/15/01 ********************************************
     C*
     C*   DISPLAY LOCATION OF OUTPUT FILE
     C*
     C                   CALL      'SA030R'
     C                   PARM                    QQSUFX            3
     C     QQSUFX        IFEQ      *BLANKS
     C     'PDSRPT'      CAT(P)    OPTSEL        XXRPTL           23
     C                   ELSE
     C     'PDSRPT'      CAT(P)    QQSUFX        XXRPTL
     C                   CAT(P)    OPTSEL:0      XXRPTL
     C                   ENDIF
     C                   MOVE      XXRPTL        SCRPTL
     C*
     C*** END ADD ******** 05/15/01 ********************************************
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        SCMSG
     C*
     C********************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C********************************************************************
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF
      **** BEGIN ADD **** 06/12/98 ************************************
     C*
     C*   F10 = SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
      **** BEGIN ADD **** 06/30/98 ************************************
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCFRDT
     C                   ENDIF
     C*
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCTRDT
     C                   ENDIF
      **** END   ADD **** 06/30/98 ************************************
     C*
     C*   EMPLOYER SEARCH
     C*
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN2        @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM                    @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     HFLD          IFEQ      'SCEMP#'
     C     *INKL         CABEQ     *ON           STRT                     34
     C                   ENDIF                                                  IFEQ SCEMP
     C*
     C     HFLD          IFEQ      'SCDIV'
     C     *INKL         CABEQ     *ON           STRT                     35
     C                   ENDIF                                                  IFEQ SCDIV
     C*
     C     @@RRN         CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   MOVE      *ON           *IN36
     C                   ELSE
     C     HFLD          IFEQ      'SCEMP#'
     C                   MOVE      *ON           *IN34
     C                   ELSE
     C                   MOVE      *ON           *IN35
     C                   ENDIF                                                  IFEQ SCEMP#
     C*
     C                   ENDIF                                                  IFEQ *IN60
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD
     C*
     C*    CLAIM NUMBER SEARCH
     C*
     C     HFLD          IFEQ      'SCCASE'
     C*
     C                   CALL      'W0070R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM      *ZEROS        @@RRNC            9 0
     C                   PARM      'CN'          @@SORT            2
     C                   PARM      DBRRN         @@RRN2            9 0
      **** BEGIN DEL **** 06/30/98 ************************************
     C                   PARM      SCFYR         @@FYR             3 0
      **** END   DEL **** 06/30/98 ************************************
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRNC        OREQ      *ZERO
     C                   MOVE      *ON           *IN36
     C                   GOTO      STRT
     C                   ELSE
     C*
     C     @@RRNC        CHAIN     WMCMP                              60
     C                   MOVE      CMCASE        SCCASE
     C                   MOVE      *ON           *IN37
     C                   ENDIF                                                  IFEQ *INKL
     C*
     C                   GOTO      STRT                                         HFLD
     C                   ENDIF
     C*
     C                   MOVE      *ON           *IN90
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  INKJ
      **** END   ADD **** 06/12/98 ************************************
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      *ON
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C*
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      *ON
     C                   CALL      'W0006R'
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         XXCO#
     C                   MOVE      *ZERO         XXFND
     C                   ELSE
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPFND         XXFND
     C                   MOVE      UPOUTQ        XXOUTQ
     C                   ENDIF
     C                   GOTO      STRT01
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN11 = *ON
     C*
     C********************************************************************
     C*   PROCESS REQUEST SCREEN
     C********************************************************************
     C     SCR01T        TAG
     C*
     C*   VALIDATE THE FUND YEAR IF ENTERED
     C*
     C     SCFYR         IFNE      *ZERO
     C     F1KY          CHAIN     PF101                              20
     C     *IN20         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*   VALIDATE THE "FROM DATE"
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*   VALIDATE THE "TO DATE"
     C*
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTRDT
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  THE DATES MUST BE IN SEQUENCE
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
      **** BEGIN CHG **** 06/30/98 ************************************
     C******************** MOVELSRDTO     XXFRDT  70
     C                   MOVEL     SRDTO         XXFRDT            8 0
     C                   Z-ADD     SCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C******************** MOVELSRDTO     XXTRDT  70
     C                   MOVEL     SRDTO         XXTRDT            8 0
      **** BEGIN CHG **** 06/30/98 ************************************
     C*
     C     XXFRDT        IFGT      XXTRDT
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  VALIDATE THE EMPLOYER EXISTS
     C*
     C     SCEMP#        IFNE      *ZERO
     C     SCDIV         ORNE      *ZERO
     C     ELKY          CHAIN     PEL01                              24
     C     *IN24         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*  VALIDATE THE EMPLOYER EXISTS FOR THE FUND YEAR
     C*
     C     SCFYR         IFNE      *ZERO
     C     SCEMP#        ANDNE     *ZERO
     C*
     C     ELKY2         CHAIN     PEL01                              25
     C     *IN25         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  FYR REQUIRED IF CASE ENTERED
     C*
     C     SCCASE        IFNE      *ZERO
     C*
     C     SCFYR         IFEQ      *ZERO
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*   VALIDATE THE CASE NUMBER IF ENTERED
     C*
     C     SCCASE        IFNE      *ZERO
     C     CMKY          CHAIN     PCM01                              27
     C     *IN27         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C*   VALIDATE THE CASE STATUS SELECTION FLAG
     C*
     C     SCSTAT        IFEQ      *BLANK
     C                   MOVE      *ON           *IN31
     C                   GOTO      STRT
     C                   ENDIF
     C*
      **** BEGIN CHG **** 06/12/98 ************************************
     C********** SCSTAT    IFNE *BLANK                     BOTH
     C     SCSTAT        IFNE      'B'                                          BOTH
      **** END   CHG **** 06/12/98 ************************************
     C     SCSTAT        ANDNE     'C'                                          CLOSED
     C     SCSTAT        ANDNE     'O'                                          OPEN
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*   VALIDATE THE LOST TIME SELECTION FLAG
     C*
     C     SCLTF         IFEQ      *BLANK
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   ENDIF
     C*
      **** BEGIN CHG **** 06/12/98 ************************************
     C********** SCLTF     IFNE *BLANK                     BOTH
     C     SCLTF         IFNE      'B'                                          BOTH
      **** END   CHG **** 06/12/98 ************************************
     C     SCLTF         ANDNE     'N'                                          NO LOST TIME
     C     SCLTF         ANDNE     'L'                                          LOST TIME
     C                   MOVE      *ON           *IN29
     C                   GOTO      STRT
     C                   ENDIF
      **** BEGIN ADD **** 06/12/98 ************************************
     C*
     C*   VALIDATE REPORT SELECTION
     C*
     C     SCRPT         IFEQ      *BLANK
     C                   MOVE      *ON           *IN33
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     SCRPT         IFNE      'P'                                          PRINT
     C     SCRPT         ANDNE     'F'                                          FILE
     C     SCRPT         ANDNE     'B'                                          BOTH
     C                   MOVE      *ON           *IN30
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  IFEQ SCRPT
      **** END   ADD **** 06/12/98 ************************************
     C*
     C*  CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      XXFND         QQFND             3
      **** BEGIN CHG **** 06/30/98 ************************************
     C*** BEGIN CHG ***  10/20/98  ************************************
     C******************** MOVE SCFYR     QQFYR   3
     C                   MOVE      SCFYR         QQFYR             3
     C*** END   CHG ***  10/20/98  ************************************
     C******************** MOVE SCFRDT    QQFRDT  6
     C******************** MOVE SCTRDT    QQTRDT  6
     C                   MOVE      SCFRDT        QQFRDT            8
     C                   MOVE      SCTRDT        QQTRDT            8
      **** END   CHG **** 06/30/98 ************************************
     C                   MOVE      SCEMP#        QQEMP#            9
     C*** BEGIN CHG *** 06/28/00 **************************************
     C******************** MOVE SCDIV     QQDIV   3
     C                   MOVE      SCDIV         QQDIV             5
     C*** END CHG ***** 06/28/00 **************************************
     C                   MOVE      SCCASE        QQCASE            7
     C                   MOVE      SCSTAT        QQSTAT            1
     C                   MOVE      SCLTF         QQLTF             1
     C                   MOVE      XXOUTQ        QQOUTQ           10
      **** BEGIN ADD **** 06/12/98 ************************************
     C                   MOVE      SCRPT         QQRPT             1
      **** END   ADD **** 06/12/98 ************************************
     C*
     C                   CALL      'W7777C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
      **** BEGIN DEL **** 06/30/98 ************************************
     C******************** PARM           QQFYR
     C*** BEGIN ADD ***  10/20/98  ************************************
     C                   PARM                    QQFYR
     C*** END   ADD ***  10/20/98  ************************************
      **** END   DEL **** 06/30/98 ************************************
     C                   PARM                    QQFRDT
     C                   PARM                    QQTRDT
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQCASE
     C                   PARM                    QQSTAT
     C                   PARM                    QQLTF
     C                   PARM                    QQOUTQ
      **** BEGIN ADD **** 06/12/98 ************************************
     C                   PARM                    QQRPT
      **** END   ADD **** 06/12/98 ************************************
     C*
     C                   MOVE      MSG(1)        SCMSG
      **** BEGIN CHG **** 06/12/98 ************************************
     C                   Z-ADD     *ZERO         SCFRDT
     C                   Z-ADD     *ZERO         SCTRDT
     C                   Z-ADD     *ZERO         SCEMP#
     C                   Z-ADD     *ZERO         SCDIV
     C                   Z-ADD     *ZERO         SCCASE
     C                   Z-ADD     *ZERO         SCFYR
     C                   MOVE      *BLANKS       SCSTAT
     C                   MOVE      *BLANKS       SCLTF
     C                   MOVE      *BLANKS       SCRPT
     C                   GOTO      STRT01
     C*******************  GOTO STRT
      **** BEGIN CHG **** 06/12/98 ************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C********************************************************************
**
       Your Report Has Been Submitted
