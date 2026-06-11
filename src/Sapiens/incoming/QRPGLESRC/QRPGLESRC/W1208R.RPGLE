     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W1208R - A/R BATCH INQUIRY BY EMP#
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*       THIS PROGRAM DISPLAYS ALL EMPLOYER A/R BATCH TRANS-     *
     H*       ACTIONS IN POSTING DATE ORDER FOR VIEWING ONLY.  THE    *
     H*       INFORMATION TO BE DISPLAYED INCLUDE: FUND NUMBER & NAME,*
     H*       FUND YEAR, EMPLOYER/DIV, POST DATE, CHECK NUMBER, CHECK *
     H*       AMOUNT, A/R CODE & TYPE, A/R DESCRIPTION, TRANSFER DATE,*
     H*       BATCH NUMBER, POSTED AMOUNT, SYSTEM CODE & DESCRIPTION. *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*       MENU                                                    *
     H*  CALLS TO:                                                    *
     H*       NONE                                                    *
     H*  PROGRAMMER: MARIE NGUYEN                                     *
     H*  CREATED:    07/17/91                                         *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*       18      ROLL UP REQUEST                                 *
     H*       19      ROLL DOWN REQUEST                               *
     H*       20      SCR01 - FUND YEAR CANNOT BE ZERO                *
     H*       20      SCR02 - INVALID ROLL REQUEST                    *
     H*       21      SCR01 - INVALID EMPLOYER/DIV                    *
     H*       21      SCR02 - START AT BEGIN OF FILE                  *
     H*       23      SCR01 - NO RECORD FOUND FOR FYR, EMP# & DIV     *
     H*       60      GENERAL CHAINS & READS                          *
     H*       61      SCREEN NOT FULLY FILLED                         *
     H*       62      TABLE FILE CHAINS                               *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/02/99 C:118 PROJECT: WC577                                 *
     H*                Group number and name not updating correctly.  *
     H*                Search not working on policy year.             *
     H*                                                               *
     H* 09/13/99 C:118 PROJECT: WC577                                 *
     H*                Modified program name and F13 key in display.  *
     H*                                                               *
     H* 03/09/98 C:85 PROJECT WC577                                   *
     H*               VERSION 3 STANDARDS, AND SEARCHES.              *
     H*                                                               *
     H* 02/05/97 C:62 PROJECT WC650                                   *
     H*               FIX ROLL FUNCTION.                              *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     FW1208F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDEAL2    IF   E           K DISK
     FWDELL1    IF   E           K DISK
     FWMEMP     IF   E             DISK
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
     F*
     F*** BEGIN ADD *** 03/09/98 **************************************
     F                                     INFDS(DBFDS2)
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL02)
     F*** END   ADD *** 03/09/98 **************************************
     F*
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWTABL     IF   E           K DISK

     D FL              S              1    DIM(6)
     D TIM             S              6  0 DIM(6)                               TIME
     D KEYD            S              7  0 DIM(2)                               DATE KEY
     D KEYT            S              6  0 DIM(2)                               TIME KEY
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D*** BEGIN ADD *** 03/09/98 **************************************
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*** END   ADD *** 03/09/98 **************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  PD                     1    048  0
     D                                     DIM(6)                               POST DATE
     D  SCPD01               001    008  0
     D  SCPD02               009    016  0
     D  SCPD03               017    024  0
     D  SCPD04               025    032  0
     D  SCPD05               033    040  0
     D  SCPD06               041    048  0
     D                 DS
     D  TD                     1    048  0
     D                                     DIM(6)                               TRANSFER DATE
     D  SCTD01               001    008  0
     D  SCTD02               009    016  0
     D  SCTD03               017    024  0
     D  SCTD04               025    032  0
     D  SCTD05               033    040  0
     D  SCTD06               041    048  0
     D                 DS
     D  CK                     1     42  0
     D                                     DIM(6)                               CHECK #
     D  SCCK01                 1      7  0
     D  SCCK02                 8     14  0
     D  SCCK03                15     21  0
     D  SCCK04                22     28  0
     D  SCCK05                29     35  0
     D  SCCK06                36     42  0
     D                 DS
     D  BT                     1     54  0
     D                                     DIM(6)                               BATCH #
     D  SCBT01                 1      9  0
     D  SCBT02                10     18  0
     D  SCBT03                19     27  0
     D  SCBT04                28     36  0
     D  SCBT05                37     45  0
     D  SCBT06                46     54  0
     D                 DS
     D  CA                     1     54  2
     D                                     DIM(6)                               CHECK AMT
     D  SCCA01                 1      9  2
     D  SCCA02                10     18  2
     D  SCCA03                19     27  2
     D  SCCA04                28     36  2
     D  SCCA05                37     45  2
     D  SCCA06                46     54  2
     D                 DS
     D  PA                     1     54  2
     D                                     DIM(6)                               POST AMT
     D  SCPA01                 1      9  2
     D  SCPA02                10     18  2
     D  SCPA03                19     27  2
     D  SCPA04                28     36  2
     D  SCPA05                37     45  2
     D  SCPA06                46     54  2
     D                 DS
     D  AR                     1     12
     D                                     DIM(6)                               A/R CODE
     D  SCAR01                 1      2
     D  SCAR02                 3      4
     D  SCAR03                 5      6
     D  SCAR04                 7      8
     D  SCAR05                 9     10
     D  SCAR06                11     12
     D                 DS
     D  TP                     1      6
     D                                     DIM(6)                               A/R TYPE
     D  SCTP01                 1      1
     D  SCTP02                 2      2
     D  SCTP03                 3      3
     D  SCTP04                 4      4
     D  SCTP05                 5      5
     D  SCTP06                 6      6
     D                 DS
     D  SY                     1     12
     D                                     DIM(6)                               SYSTEM CODE
     D  SCSY01                 1      2
     D  SCSY02                 3      4
     D  SCSY03                 5      6
     D  SCSY04                 7      8
     D  SCSY05                 9     10
     D  SCSY06                11     12
     D                 DS
     D  SD                     1    180
     D                                     DIM(6)                               SYSTEM CD DESC
     D  SCSD01                 1     30
     D  SCSD02                31     60
     D  SCSD03                61     90
     D  SCSD04                91    120
     D  SCSD05               121    150
     D  SCSD06               151    180
     D                 DS
     D  AD                     1    180
     D                                     DIM(6)                               A/R CD DESCPTN
     D  SCAD01                 1     30
     D  SCAD02                31     60
     D  SCAD03                61     90
     D  SCAD04                91    120
     D  SCAD05               121    150
     D  SCAD06               151    180
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C***** BEGIN ADD **** 12/02/99 ***********************************
     C     FNKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C***** END   ADD **** 12/02/99 ***********************************
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C***** BEGIN ADD **** 12/02/99 ***********************************
     C     EMKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C***** END   ADD **** 12/02/99 ***********************************
     C*
     C*  EMPLOYER A/R BATCH FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     EAKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXPODT
     C*
     C     EAKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXPODT
     C                   KFLD                    XXTIME
     C*
     C*   KEY LIST FOR THE TABLE FILE (SYSTEM & A/R CODES)
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY
     C*
     C     *LIKE         DEFINE    EAFYR         XXFYR
     C     *LIKE         DEFINE    EAEMP#        XXEMP#
     C     *LIKE         DEFINE    EADIV         XXDIV
     C     *LIKE         DEFINE    EAPODT        XXPODT
     C     *LIKE         DEFINE    EATIME        XXTIME
     C     *LIKE         DEFINE    SYKEY         XXKEY
     C*
     C*** BEGIN ADD *** 03/09/98 **************************************
     C     *LIKE         DEFINE    EAFND         XXFND
     C*** END   ADD *** 03/09/98 **************************************
     C*
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   MOVE      *ZERO         UPFYR
     C                   MOVE      *ZERO         UPEMP#
     C                   MOVE      *ZERO         UPDIV
     C                   END
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C***** BEGIN ADD **** 09/13/99 ***********************************
     C     *IN60         IFEQ      *OFF
     C***** END   ADD **** 09/13/99 ***********************************
     C                   MOVEL     UPFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C                   Z-ADD     UPFYR         SCFYR
     C                   Z-ADD     UPEMP#        SCEMP#
     C                   Z-ADD     UPDIV         SCDIV
     C***** BEGIN ADD **** 09/13/99 ***********************************
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       SCFND
     C                   MOVEL(P)  *BLANKS       SCNAME
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 09/13/99 ***********************************
     C*
     C*   LOAD SCREEN 1 FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C     XXUP          IFEQ      '1'
     C                   MOVE      *BLANKS       SCUP              9
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   END
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN            9
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   END
     C*
     C                   EXFMT     SCR02
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
     C*
     C*   F12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*** BEGIN DEL *** 03/09/98 **************************************
     C****COMMAND 05 = EMPLOYER NUMERIC SEARCH
     C**********
     C********** *INKE     IFEQ '1'
     C********** Q2RECF    IFEQ 'SCR01'
     C**********           CALL 'W0050R'
     C********** *INKC     PARM           @@KC    1
     C********** *INKL     PARM           @@KL    1
     C**********           PARM '2'       @@CALL  1
     C**********           PARM           @@RRN   90
     C**********           PARM DBRRN     @@RRN2  90
     C**********           PARM           @@QTE   1
     C**********           PARM 2         @@VIEW  10
     C**********           PARM           @@RRN3  90
     C**********
     C********** *INKC     CABEQ*ON       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C********** *INKL     IFEQ *ON
     C********** @@RRN     OREQ *ZERO
     C**********           GOTO STRT
     C**********           ELSE
     C**********
     C********** @@RRN     CHAINWMEMP                60
     C**********
     C**********           MOVE EMEMP#    SCEMP#
     C**********           MOVE EMDIV     SCDIV
     C**********           GOTO SCR01T
     C**********
     C**********           END
     C**********           END
     C**********           END
     C**********
     C****COMMAND 07 = EMPLOYER ALPHA SEARCH
     C**********
     C********** *INKG     IFEQ '1'
     C********** Q2RECF    IFEQ 'SCR01'
     C**********           CALL 'W0050R'
     C********** *INKC     PARM           @@KC    1
     C********** *INKL     PARM           @@KL    1
     C**********           PARM '2'       @@CALL  1
     C**********           PARM           @@RRN   90
     C**********           PARM DBRRN     @@RRN2  90
     C**********           PARM           @@QTE   1
     C**********           PARM 1         @@VIEW  10
     C**********           PARM           @@RRN3  90
     C**********
     C********** *INKC     CABEQ*ON       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C********** *INKL     IFEQ *ON
     C********** @@RRN     OREQ *ZERO
     C**********           GOTO STRT
     C**********           ELSE
     C**********
     C********** @@RRN     CHAINWMEMP                60
     C**********
     C**********           MOVE EMEMP#    SCEMP#
     C**********           MOVE EMDIV     SCDIV
     C**********           GOTO SCR01T
     C**********
     C**********           ENDIF
     C**********           END
     C**********           END
     C*** END   DEL *** 03/09/98 **************************************
     C*** BEGIN ADD *** 03/09/98 **************************************
     C*
     C*  F10 - SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*  F10 - TABLE FILE SEARCH - POLICY YEAR
     C*
     C     HFLD          IFEQ      'SCFYR'
     C*
     C*  EMPLOYER NUMBER EQUAL TO ZERO
     C*
     C     SCEMP#        IFEQ      *ZEROS
     C*
     C                   MOVE      *ZEROS        @@RRN
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @@RRN         ANDNE     *ZEROS
     C*
     C     @@RRN         CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   ENDIF                                                  *IN60=OFF
     C*
     C***** BEGIN ADD **** 12/02/99 ***********************************
     C     FNKY2         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C***** END   ADD **** 12/02/99 ***********************************
     C*
     C                   ENDIF                                                  *INKL=OFF,@@RRN
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C                   MOVE      *ON           *IN30                          SCFYR POSITION
     C***** BEGIN ADD **** 12/02/99 ***********************************
     C                   GOTO      STRT
     C***** END   ADD **** 12/02/99 ***********************************
     C                   ENDIF                                                  *INKL=ON,@@RRN
     C*
     C                   ENDIF                                                  SCEMP#=0
     C***** BEGIN DEL **** 12/02/99 ***********************************
     C*  EMPLOYER NUMBER NOT EQUAL TO ZERO
     C********** SCEMP#    IFNE *ZEROS
     C*
     C**********           MOVE SCFND     XXFND
     C**********           MOVE SCEMP#    XXEMP#
     C**********           MOVE SCDIV     XXDIV
     C*
     C********** EMKEY     CHAINPEM02                60
     C***** END   DEL **** 12/02/99 ***********************************
     C***** BEGIN ADD **** 12/02/99 ***********************************
     C*
     C     EMKY2         CHAIN     PEM02                              60
     C*
     C***** END   ADD **** 12/02/99 ***********************************
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '2'           @@CALL
     C***** BEGIN CHG **** 12/02/99 ***********************************
     C**********           Z-ADDXXFND     @1RRN
     C                   Z-ADD     *ZEROS        @1RRN
     C***** END   CHG **** 12/02/99 ***********************************
     C                   Z-ADD     DBRRN2        @1RRN2
     C*
     C                   CALL      'W0051R'
     C                   PARM                    @1KC              1
     C                   PARM                    @1KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @1RRN             9 0
     C                   PARM                    @1RRN2            9 0
     C                   PARM                    QQQTE             1
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     @1RRN         IFNE      *ZEROS
     C*
     C     @1RRN         CHAIN     PEL02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELFYR         SCFYR
     C***                  MOVE *ON       *IN41            SCEFDT POSITION
     C***** BEGIN ADD **** 12/02/99 ***********************************
     C                   GOTO      STRT
     C***** END   ADD **** 12/02/99 ***********************************
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  @1RRN<>0
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     *INKL         IFEQ      *ON
     C     @1RRN         OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C                   MOVE      *ON           *IN30                          SCFYR POSITION
     C***** BEGIN ADD **** 12/02/99 ***********************************
     C                   GOTO      STRT
     C***** END   ADD **** 12/02/99 ***********************************
     C                   ENDIF                                                  *INKL=ON,@@RRN
     C*
     C***** BEGIN DEL **** 12/02/99 ***********************************
     C**********           ENDIF                           SCEMP#=0
     C***** END   DEL **** 12/02/99 ***********************************
     C*
     C                   ENDIF                                                  HFLD=SCFYR
     C*
     C*  F10 - TABLE FILE SEARCH - EMPLOYER NUMBER OR DIVISION
     C*
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   MOVE      *ZEROS        @@RRN
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @@RRN         ANDNE     *ZEROS
     C*
     C     @@RRN         CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C*
     C***** BEGIN ADD **** 12/02/99 ***********************************
     C     FNKY2         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C***** END   ADD **** 12/02/99 ***********************************
     C*
     C*  GROUP YEAR EQUAL TO ZERO
     C*
     C     SCFYR         IFEQ      *ZEROS
     C*
     C***** BEGIN DEL **** 12/02/99 ***********************************
     C**********           MOVE SCFND     XXFND
     C**********           MOVE SCEMP#    XXEMP#
     C**********           MOVE SCDIV     XXDIV
     C*
     C********** EMKEY     CHAINPEM02                60
     C***** END   DEL **** 12/02/99 ***********************************
     C***** BEGIN ADD **** 12/02/99 ***********************************
     C*
     C     EMKY2         CHAIN     PEM02                              60
     C*
     C***** END   ADD **** 12/02/99 ***********************************
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '2'           @@CALL
     C***** BEGIN CHG **** 12/02/99 ***********************************
     C**********           Z-ADDXXFND     @1RRN
     C                   Z-ADD     *ZEROS        @1RRN
     C***** END   CHG **** 12/02/99 ***********************************
     C                   Z-ADD     DBRRN2        @1RRN2
     C*
     C                   CALL      'W0051R'
     C                   PARM                    @1KC              1
     C                   PARM                    @1KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @1RRN             9 0
     C                   PARM                    @1RRN2            9 0
     C                   PARM                    QQQTE             1
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     @1RRN         IFNE      *ZEROS
     C*
     C     @1RRN         CHAIN     PEL02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELFYR         SCFYR
     C***                  MOVE *ON       *IN41            SCEFDT POSITION
     C***** BEGIN ADD **** 12/02/99 ***********************************
     C                   GOTO      STRT
     C***** END   ADD **** 12/02/99 ***********************************
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  @1RRN<>0
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     *INKL         IFEQ      *ON
     C     @1RRN         OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C                   MOVE      *ON           *IN30                          SCFYR POSITION
     C***** BEGIN ADD **** 12/02/99 ***********************************
     C                   GOTO      STRT
     C***** END   ADD **** 12/02/99 ***********************************
     C                   ENDIF                                                  *INKL=ON,@@RRN
     C*
     C                   ENDIF                                                  SCFYR=0
     C*
     C***** BEGIN DEL **** 12/02/99 ***********************************
     C*  GROUP YEAR NOT EQUAL TO ZERO
     C********** SCFYR     IFNE *ZEROS
     C***                  MOVE *ON       *IN41            SCEFDT POSITION
     C**********           ENDIF                           SCFYR<>0
     C***** END   DEL **** 12/02/99 ***********************************
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKL=OFF,@@RRN
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C                   MOVE      *ON           *IN31                          SCEMP# POSITION
     C***** BEGIN ADD **** 12/02/99 ***********************************
     C                   GOTO      STRT
     C***** END   ADD **** 12/02/99 ***********************************
     C                   ENDIF                                                  *INKL=ON,@@RRN
     C*
     C                   ENDIF                                                  HFLD=SCEMP#
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C                   ENDIF                                                  *INKJ=ON
     C*
     C*** END   ADD *** 03/09/98 **************************************
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C                   CALL      'W0006R'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C***** BEGIN DEL **** 12/02/99 ***********************************
     C***** BEGIN ADD **** 09/13/99 ***********************************
     C* Clear previous group name and load new name
     C********** FNKEY     CHAINPFN01                60
     C********** *IN60     IFEQ *OFF
     C**********           MOVELFNNAME    SCNAME    P
     C**********           ELSE
     C**********           MOVEL*BLANKS   SCNAME    P
     C**********           ENDIF                           *IN60=OFF
     C***** END   ADD **** 09/13/99 ***********************************
     C                   GOTO      TOP
     C                   END
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR02T
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR02T
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*  PROCESS REQUEST SCREENS
     C*
     C     SCR01T        TAG
     C*
     C*  YEAR CANNOT BE LESS THAN OR EQUAL TO ZERO
     C*
     C     SCFYR         CABLE     *ZERO         STRT                   2020
     C                   Z-ADD     SCFYR         XXFYR
     C*
     C*  EMPLOYER NUMBER MUST EXIST
     C*
     C     EMKEY         CHAIN     PEM02                              21
     C     *IN21         CABEQ     '1'           STRT
     C*
     C*  FND, FYR, EMP# & DIV MUST BE VALID (MUST EXIST IN WDELP)
     C*
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
     C     EAKEY         CHAIN     PEL01                              22
     C     *IN22         CABEQ     '1'           STRT
     C*
     C*  ALL EDITS PASSED, GET EMPLOYER NAME & GO TO SCREEN 2
     C*
     C                   MOVE      *BLANKS       SCENAM
     C                   MOVEL     EMNAME        SCENAM
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVE      '1'           *IN21
     C                   Z-ADD     *ZERO         SCDT
     C                   GOTO      REPOS
     C*
     C     SCR02T        TAG
     C*
     C*  INVALID ROLL REQUEST
     C*
     C   18XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     KEYD(2)       XXPODT
     C                   Z-ADD     KEYT(2)       XXTIME
     C*
     C*** BEGIN DEL *** 02/05/97 **************************************
     C**********           MULT 10000.01  XXPODT
     C*** END   DEL *** 02/05/97 **************************************
     C*
     C     EAKEY2        CHAIN     PEA01                              60
     C     EAKEY         READE     PEA01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYD(1)       XXPODT
     C                   Z-ADD     KEYT(1)       XXTIME
     C*
     C*** BEGIN DEL *** 02/05/97 **************************************
     C**********           MULT 10000.01  XXPODT
     C*** END   DEL *** 02/05/97 **************************************
     C*
     C     EAKEY2        CHAIN     PEA01                              60
     C  N60              READP     PEA01                                  60
     C  N60UPCO#         COMP      EACO#                              6060
     C  N60UPFND         COMP      EAFND                              6060
     C  N60XXFYR         COMP      EAFYR                              6060
     C  N60XXEMP#        COMP      EAEMP#                             6060
     C  N60XXDIV         COMP      EADIV                              6060
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCDT          IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *ZEROS        KEYD
     C                   MOVE      *ZEROS        KEYT
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       21
     C*
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C*
     C                   MOVE      *ZEROS        SCDT
     C     EAKEY1        SETLL     PEA01
     C     EAKEY         READE     PEA01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C*  CLEAR SCREEN ARRAYS
     C*****************************************************************
     C     CLR           BEGSR
     C                   MOVE      *ZEROS        PD
     C                   MOVE      *ZEROS        CK
     C                   MOVE      *ZEROS        CA
     C                   MOVE      *BLANKS       AR
     C                   MOVE      *BLANKS       TP
     C                   MOVE      *BLANKS       AD
     C                   MOVE      *ZEROS        TD
     C                   MOVE      *ZEROS        BT
     C                   MOVE      *ZEROS        PA
     C                   MOVE      *BLANKS       SY
     C                   MOVE      *BLANKS       SD
     C                   MOVE      *ZEROS        TIM
     C                   Z-ADD     *ZERO         I1                1 0
     C                   ENDSR
     C*****************************************************************
     C*  FILL SCREEN ARRAYS
     C*****************************************************************
     C     FILL          BEGSR
     C*
     C                   Z-ADD     EAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PD(I1)
     C*
     C                   MOVE      EACHK#        CK(I1)                         CHECK#
     C                   MOVE      EACKAM        CA(I1)                         CHECK AMT
     C                   MOVE      EAARCD        AR(I1)                         A/R CODE
     C*
     C                   MOVE      *BLANK        XXKEY                          FIND A/R TYPE
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              62
     C  N62              MOVEL     TTTYPE        TP(I1)                         A/R TYPE
     C*
     C                   MOVEL     EADESC        AD(I1)                         A/R CODE DESCRIPTION
     C*
     C                   MOVE      *BLANK        XXKEY                          FIND SYS CODE DESCPT
     C                   MOVEL     EASYCD        XXKEY
     C     TBKEY         CHAIN     PSY01                              62
     C  N62              MOVEL     SYDESC        SD(I1)
     C     EATFAM        IFNE      *ZEROS
     C*
     C                   Z-ADD     EATFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         TD(I1)
     C*
     C                   ELSE
     C                   Z-ADD     *ZEROS        TD(I1)
     C                   END
     C                   MOVE      EABAT#        BT(I1)                         BATCH#
     C                   MOVE      EAAMT         PA(I1)                         POSTED AMT
     C                   MOVE      EASYCD        SY(I1)                         SYSTEM CODE
     C*
     C                   MOVE      EATIME        TIM(I1)                        TIME
     C                   ENDSR
     C*
     C*****************************************************************
     C*  LOAD FORWARD SUBROUTINE
     C*****************************************************************
     C*
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C     OVER          TAG
     C*
     C*  END OF FILE
     C*
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     EAKEY         READE     PEA01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      6                                    61
     C   61              GOTO      OVER
     C     DONE          TAG
     C*
     C                   Z-ADD     PD(1)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KEYD(1)
     C*
     C                   Z-ADD     PD(6)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KEYD(2)
     C*
     C                   Z-ADD     TIM(1)        KEYT(1)
     C                   Z-ADD     TIM(6)        KEYT(2)
     C                   ENDSR
     C*
     C*****************************************************************
     C*  LOAD BACKWARD SUBROUTINE
     C*****************************************************************
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     7             I1
     C     OVERP         TAG
     C*
     C*  BEGINNING OF FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADB
     C                   GOTO      DONEP
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   SUB       1             I1
     C                   EXSR      FILL
     C                   READP     PEA01                                  60
     C  N60EACO#         COMP      UPCO#                              6060
     C  N60EAFND         COMP      UPFND                              6060
     C  N60EAFYR         COMP      XXFYR                              6060
     C  N60EAEMP#        COMP      XXEMP#                             6060
     C  N60EADIV         COMP      XXDIV                              6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C*
     C                   Z-ADD     PD(1)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KEYD(1)
     C*
     C                   Z-ADD     PD(6)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KEYD(2)
     C*
     C                   Z-ADD     TIM(1)        KEYT(1)
     C                   Z-ADD     TIM(6)        KEYT(2)
     C                   ENDSR
     C*
     C*****************************************************************
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*****************************************************************
     C*
     C     LOADB         BEGSR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C*** BEGIN ADD *** 02/05/97 **************************************
     C                   Z-ADD     *ZERO         XXPODT
     C*** END   ADD *** 02/05/97 **************************************
     C     EAKEY1        SETLL     PEA01
     C     EAKEY         READE     PEA01                                  60
     C                   EXSR      LOAD
     C                   ENDSR
**
ROLL UP
ROLL DOWN
