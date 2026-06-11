     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W4130R - Prompt -  PREMIUM/PAYMENT COMPARISON REPORT
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  09/13/99  C:118  PROJECT: WC577                              *
     H*                   SCREEN FIXES AND CLEAR GROUP NAME IF NO     *
     H*                   GROUP NUMBER PASSED.                        *
     H*                                                               *
     H*****************************************************************
     F* Prompt Screen File
     FW4130F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter Master File
     FWMUPL     IF   E           K DISK
     F*
     F* Fund Master File
     FWMFNL     IF   E           K DISK
     F*
     F* Table file logical
     FWTABL     IF   E           K DISK

     D*
     D SCD             S             10    DIM(30) CTDATA PERRCD(1) ASCEND      A/R CODE TABLE
     D PCIND           S              2  0 DIM(30) CTDATA PERRCD(30) ASCEND     POSITION CURSOR
     D QQCD            S              2    DIM(30) ASCEND                       A/R CODE ARRAY
     D*
     D* System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* Screen Number Reference
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D                 DS                  INZ
     D  CD                     1     60
     D                                     DIM(30) ASCEND                       A/R CODE ARRAY
     D  SCCD01                 1      2
     D  SCCD02                 3      4
     D  SCCD03                 5      6
     D  SCCD04                 7      8
     D  SCCD05                 9     10
     D  SCCD06                11     12
     D  SCCD07                13     14
     D  SCCD08                15     16
     D  SCCD09                17     18
     D  SCCD10                19     20
     D  SCCD11                21     22
     D  SCCD12                23     24
     D  SCCD13                25     26
     D  SCCD14                27     28
     D  SCCD15                29     30
     D  SCCD16                31     32
     D  SCCD17                33     34
     D  SCCD18                35     36
     D  SCCD19                37     38
     D  SCCD20                39     40
     D  SCCD21                41     42
     D  SCCD22                43     44
     D  SCCD23                45     46
     D  SCCD24                47     48
     D  SCCD25                49     50
     D  SCCD26                51     52
     D  SCCD27                53     54
     D  SCCD28                55     56
     D  SCCD29                57     58
     D  SCCD30                59     60
     D* External Program File Information
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      * Key List for the Company Master File
      *

     C     COKEY         KLIST
     C                   KFLD                    XXCO#
      *
      * Key List for the User Parameter Master File
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      * Key List for the Fund Master File
      *
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      * Key List for the Table file
      *
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
      *
      * Entry Parameters
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *================================================================
      * Variable definitions
      *================================================================
     C     *LIKE         DEFINE    FNEFDT        XXFRDT
     C     *LIKE         DEFINE    FNEFDT        XXTODT
     C     *LIKE         DEFINE    FNCO#         XXCO#
     C     *LIKE         DEFINE    FNFND         XXFND
      *================================================================
      * Get the User Parameter Record
      *================================================================
     C     UPKEY         CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         XXCO#
     C                   MOVE      *ZERO         SCFND
     C                   MOVE      *ZERO         XXFND
      *
     C                   ELSE
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFND         XXFND
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *================================================================
      * Get the Company Record
      *================================================================
     C     STRT01        TAG
      *
     C     COKEY         CHAIN     PCO01                              60
      *
     C                   MOVEL(P)  CONAME        SRDFLD
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
      *
     C                   MOVEL     SRDFLD        SCCONM
      *================================================================
      * Load system date into Payments as of Date.
      *================================================================
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C                   MOVEL     SRDTO         SCPMTD
      *================================================================
      * Get the Requested Record & Load Information
      *================================================================
     C     FNKEY         CHAIN     PFN01                              60
     C***** BEGIN ADD **** 09/13/99 ***********************************
     C     *IN60         IFEQ      *OFF
     C***** END   ADD **** 09/13/99 ***********************************
     C                   MOVEL     FNNAME        SCNAME
     C***** BEGIN ADD **** 09/13/99 ***********************************
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       SCNAME
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 09/13/99 ***********************************
      *
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
      *
      * Load the Record Format Infomation  for the First Time
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
      *================================================================
     C     STRT          TAG
      *================================================================
      *
      * Convert screen dates to MM/DD/YYYY.
      *
     C                   Z-ADD     SCFEFD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFEFD
      *
     C                   Z-ADD     SCTEFD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTEFD
      *
     C                   Z-ADD     SCPMTD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPMTD
      *================================================================
      * Display All Formats
      *================================================================
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
      *================================================================
      * Process All Function Keys
      *================================================================
     C     *IN11         IFEQ      *ON
      *
      * Function 03 = End of Job
      *
     C     *INKC         IFEQ      *ON
      *
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
      *
     C                   ENDIF                                                  *INKC IFEQ 1
      *================================================================
      * Function 10 = Table Search
      *================================================================
     C     *INKJ         IFEQ      *ON
      *
     C                   Z-ADD     1             X                 2 0
      *
     C     HFLD          LOOKUP    SCD(X)                                 60
      *
     C     *IN60         IFEQ      *ON
      *
     C                   CALL      'W2161R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      *BLANKS       SCKEY            10
      *
      *  POSITION THE CURSOR
      *
     C     SCKEY         IFNE      *BLANKS
      *
     C                   MOVEL     SCKEY         CD(X)
      *
      *  IS IT THE LAST FIELD ON THE SCREEN
      *
     C     X             IFEQ      30
      *
     C                   Z-ADD     1             X
      *
     C                   ELSE
     C                   ADD       1             X
      *
     C                   ENDIF                                                  X=30
      *
     C                   ENDIF                                                  SCKEY NE 0
      *
     C                   MOVE      PCIND(X)      Y                 2 0
      *
     C                   MOVE      *ON           *IN(Y)
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  IN60=ON
      *
     C                   ENDIF                                                  *INKJ IFEQ 1
      *
      * Process HELP Requests
      *
     C     *IN10         IFEQ      *ON
      *
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
      *
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *IN10 IFEQ *ON
      *
      * Process Change of User Parameters
      *
     C     *INKM         IFEQ      *ON
      *
     C                   CALL      'W0006R'
      *
      * Get the User Parameter Record
      *
     C     UPKEY         CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         XXCO#
     C                   MOVE      *ZERO         XXFND
      *
     C                   ELSE
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPFND         XXFND
     C                   MOVE      UPFND         SCFND
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *
     C                   GOTO      STRT01
      *
     C                   ENDIF                                                  *INKM IFEQ *ON
      *
     C                   ENDIF                                                  *IN11 IFEQ *ON
      *================================================================
      * Process Request Screen
      *================================================================
     C     SCR01T        TAG
      *
      * Validate the Policy effective dates entered
      *
     C                   Z-ADD     SCFEFD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFEFD
      *
     C     SRERR         IFEQ      *ON
      *
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SPERR IFEQ *ON
      *
     C                   Z-ADD     SCTEFD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTEFD
      *
     C     SRERR         IFEQ      *ON
      *
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SPERR IFEQ *ON
      *
      * Both dates must be entered
      *
     C     SCFEFD        IFNE      *ZERO
     C     SCTEFD        ANDEQ     *ZERO
      *
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCFEFD NE *ZERO
      *
     C     SCTEFD        IFNE      *ZERO
     C     SCFEFD        ANDEQ     *ZERO
      *
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCFEFD NE *ZERO
      *
      * From date cannot be greater than to date
      *
     C                   Z-ADD     SCFEFD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
      *
     C                   Z-ADD     SCTEFD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
      *
     C     XXFRDT        IFGT      XXTODT
      *
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  XXFRDT > XXTODT
      *================================================================
      * Validate the Payments as of Date entered
      *================================================================
     C                   Z-ADD     SCPMTD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPMTD
      *
     C     SRERR         IFEQ      *ON
      *
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
      * Validate the Account Type entered
      *
     C     SCACTY        IFNE      'E'
     C     SCACTY        ANDNE     'S'
     C     SCACTY        ANDNE     'B'
      *
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
      * Validate the Account Status entered
      *
     C     SCACST        IFNE      'A'
     C     SCACST        ANDNE     'N'
     C     SCACST        ANDNE     'B'
      *
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
      * Validate the Sort selection
      *
     C     SCSORT        IFNE      'EA'
     C     SCSORT        ANDNE     'EN'
     C     SCSORT        ANDNE     'AA'
     C     SCSORT        ANDNE     'AN'
      *
     C                   MOVE      *ON           *IN29
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
      * Validate the A/R selection (include/exclude/  )
      *
     C     SCARFL        IFNE      'I'
     C     SCARFL        ANDNE     'E'
     C     SCARFL        ANDNE     *BLANK
      *
     C                   MOVE      *ON           *IN62
     C                   GOTO      STRT
      *
     C                   ENDIF
      *================================================================
      * Validate the A/R payment codes entered
      *================================================================
      *
      * Use two sets of indicators to flag errors on the screen
      * producing 30 combinations with indicators 63-68 AND 69-73.
      *
     C                   Z-ADD     6             N                 2 0
     C                   Z-ADD     *ZERO         N1                2 0
     C                   Z-ADD     63            XX                2 0
     C                   Z-ADD     68            XY                2 0
      *
      * Process all 30 A/R codes
      *
     C     1             DO        30            X
      *
     C                   ADD       1             XY
     C     N             SUB       X             N1
      *
      * Is this the end of the row
      *
     C     N1            IFEQ      *ZERO
     C                   ADD       5             N
     C                   ADD       1             XX
     C                   MOVE      69            XY
     C                   ENDIF
      *
      * Validate the code
      *
     C     CD(X)         IFNE      *BLANKS
     C                   MOVEL(P)  CD(X)         XXKEY
      *
     C     TBKY          CHAIN     PTT01                              60
      *
     C     *IN60         IFEQ      *ON
      *
     C                   MOVE      *ON           *IN(XX)
     C                   MOVE      *ON           *IN(XY)
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF
      *
     C                   ENDDO
      *
      * If A/R codes were entered then SCARFL must be I or E
      *
     C                   MOVEA     CD            CDTEMP           60
      *
     C     SCARFL        IFEQ      *BLANK
     C     CDTEMP        ANDNE     *BLANKS
      *
     C                   MOVE      *ON           *IN74
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCARFL EQ BLANK
      *
      * If SCARFL is not blank then A/R codes must be entered
      *
     C     SCARFL        IFNE      *BLANK
     C     CDTEMP        ANDEQ     *BLANKS
      *
     C                   MOVE      *ON           *IN75
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  SCARFL EQ BLANK
      *================================================================
      *  Modify the A/R code array so it can be passed to the CL program
      *================================================================
      *
      *  Sort the A/R code array
      *
     C                   SORTA     CD
      *
      *  Move blank array elements to the end of the array
      *
     C                   MOVE      *BLANKS       QQCD
      *
     C                   Z-ADD     1             X
      *
      * Locate the first non blank A/R code
      *
     C     CD(X)         DOUNE     *BLANKS
     C     X             OREQ      30
      *
     C                   ADD       1             X
      *
     C                   ENDDO                                                  FD,X DOUNE
      *
      *  Load the new A/R code array to be passed to the CL program
      *
     C                   MOVEA     CD(X)         QQCD
      *
      *================================================================
      * Call the CLP to Submit the Report
      *================================================================
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      UPFND         QQFND             3
     C                   MOVE      SCFEFD        QQSTDT            8
     C                   MOVE      SCTEFD        QQENDT            8
     C                   MOVE      SCPMTD        QQRNDT            8
     C                   MOVE      'B'           QQRDST            1
     C                   MOVE      SCACTY        QQTYPE            1
     C                   MOVE      SCACST        QQACST            1
     C                   MOVE      'E'           QQSTAT            1
     C                   MOVE      SCSORT        QQSRT1            2
     C                   MOVE      *BLANK        QQSRT2            2
     C                   MOVE      'M'           QQEMPF            1
     C                   MOVE      'M'           QQAMPF            1
     C                   MOVE      *BLANKS       QQPRTY            2
     C                   MOVE      *ZEROS        QQEMP#            9
     C                   MOVE      *ZEROS        QQDIV             5
     C                   MOVE      *ZEROS        QQFYR             3
     C                   MOVE      *BLANKS       QQSEL1            2
     C                   MOVE      *BLANKS       QQS1FR           15
     C                   MOVE      *BLANKS       QQS1TO           15
     C                   MOVE      *BLANKS       QQSEL2            2
     C                   MOVE      *BLANKS       QQS2FR           15
     C                   MOVE      *BLANKS       QQS2TO           15
     C                   MOVE      SCARFL        QQARFL            1
     C                   MOVE      SCSORT        QQSORT            2
     C                   MOVE      UPOUTQ        QQOUTQ           10
      *
     C                   CALL      'W4130C'
      *
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQSTDT
     C                   PARM                    QQENDT
     C                   PARM                    QQRNDT
     C                   PARM                    QQRDST
     C                   PARM                    QQTYPE
     C                   PARM                    QQACST
     C                   PARM                    QQSTAT
     C                   PARM                    QQSRT1
     C                   PARM                    QQSRT2
     C                   PARM                    QQEMPF
     C                   PARM                    QQAMPF
     C                   PARM                    QQPRTY
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQFYR
     C                   PARM                    QQSEL1
     C                   PARM                    QQS1FR
     C                   PARM                    QQS1TO
     C                   PARM                    QQSEL2
     C                   PARM                    QQS2FR
     C                   PARM                    QQS2TO
     C                   PARM                    QQCD
     C                   PARM                    QQARFL
     C                   PARM                    QQSORT
     C                   PARM                    QQOUTQ
      *
      * End the Program
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  SCD
SCCD01
SCCD02
SCCD03
SCCD04
SCCD05
SCCD06
SCCD07
SCCD08
SCCD09
SCCD10
SCCD11
SCCD12
SCCD13
SCCD14
SCCD15
SCCD16
SCCD17
SCCD18
SCCD19
SCCD20
SCCD21
SCCD22
SCCD23
SCCD24
SCCD25
SCCD26
SCCD27
SCCD28
SCCD29
SCCD30
** PCIND
303132333435363738394041424344454647484950515253545556575859
