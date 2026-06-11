     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0312R - UNCOVERED PAYMENT CODE MAINTENANCE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/28/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  04/03/92 LPK  PROJECT: MD027                                 *
     H*                CHANGED CODE TO USE EMP/DIV INSTEAD OF FYR.    *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     F*****************************************************************
     FW0312F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMRIL1    UF   E           K DISK
     FWTABL     IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  CD                     1     60
     D                                     DIM(30) ASCEND                       PMT CODES
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
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*                                                               *
     D*** BEGIN ADD *** 01/28/98 ***********************************************
     D OPTNM           C                   CONST('Reinsurance Policy M-
     D                                     aintenance')

     I*** END   ADD *** 01/28/98 ***********************************************
     I*

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C*
     C*  REINSURANCE POLICY FILE
     C*
     C     RIKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXASFL            1
     C                   KFLD                    XXEFDT
     C*
     C*  KEY LIST FOR TABLE FILE
     C*
     C     TBKY1         KLIST
     C                   KFLD                    UPCO#
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C     *LIKE         DEFINE    RIEMP#        XXEMP#
     C     *LIKE         DEFINE    RIDIV         XXDIV
     C     *LIKE         DEFINE    RIEMP#        QQEMP#
     C     *LIKE         DEFINE    RIDIV         QQDIV
     C     *LIKE         DEFINE    RIDT          XXEFDT
     C*
     C     *ENTRY        PLIST
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQEFDT            8 0
     C                   PARM                    QQASFL            1
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
     C                   Z-ADD     QQEMP#        XXEMP#
     C                   Z-ADD     QQDIV         XXDIV
     C*
     C                   Z-ADD     QQEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   MOVE      QQASFL        XXASFL            1
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVE      UPFND         SCFND
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C                   Z-ADD     UPFND         XXFND
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C                   MOVE      QQEMP#        SCEMP#
     C                   MOVE      QQDIV         SCDIV
     C                   MOVE      QQEFDT        SCEFDT
     C                   MOVE      QQASFL        SCASFL
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C*
     C**********           MOVE QQPRDS    SCPRDS
     C*
     C*  CENTER PROGRAM NAME
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        SCPRDS
     C                   MOVEL(P)  ##DATA        QQPRDS
     C*
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   GET INFORMATION
     C*
     C                   MOVE      *BLANKS       CD
     C     RIKEY         CHAIN     PRI01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C     RIKEY         CHAIN     PRI01                              60
     C                   END
     C     *IN60         IFEQ      '0'
     C                   MOVEA     RIUNCV        CD
     C                   EXCEPT    PRI01E
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*     ERROR INDICATORS FOR CODES ARE *IN20 - *IN26, *IN65 - *IN70
     C*     POSITION CURSOR INDICATORS ARE *IN27 - *IN56
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   F12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*** BEGIN DEL *** 01/28/98 **************************************
     C*
     C***TABLE*FILE SEARCH
     C**********
     C********** *INKJ     IFEQ '1'
     C********** Q2RECF    ANDEQ'SCR01'
     C**********           Z-ADD1         X       20
     C**********
     C********** X         DOUEQ31
     C**********           MOVELCD,X      XX2     2
     C********** XX2       IFEQ '? '
     C**********
     C**********           MOVE *BLANK    SCKEY  10
     C**********           CALL 'W2146R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCKEY
     C**********           PARM           QQRTC   1
     C**********           MOVELSCKEY     CD,X
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** SCKEY     OREQ *BLANK
     C********** X         ADD  26        X1      20
     C**********           MOVE '1'       *IN,X1
     C**********           GOTO STRT
     C**********           END
     C**********
     C********** QQRTC     IFEQ '*'
     C********** X         ADD  27        X1      20
     C********** X1        IFNE 57
     C**********           MOVE '1'       *IN,X1
     C**********           END
     C**********           GOTO STRT
     C**********           END
     C**********
     C**********           END
     C**********           ADD  1         X
     C**********           END
     C**********           GOTO STRT
     C**********           END
     C*
     C*** END   DEL *** 01/28/98 **************************************
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C*
     C*  F10 = SEARCHES
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C*  F10 = SEARCH - CODE
     C*
     C     HFLD          IFGE      'SCCD01'
     C     HFLD          ANDLE     'SCCD30'
     C*
     C**       EXTRACT NUMBER FROM HFLD POSITION
     C*
     C                   MOVEL(P)  HFLD          HFLD06            6
     C                   MOVE(P)   HFLD06        XX                2 0
     C*
     C     XX            IFGE      1
     C     XX            ANDLE     30
     C*
     C                   CALL      'W2146R'
     C                   PARM      '2'           QQCALL                         SELECT MODE
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      *BLANKS       SCKEY            10
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     SCKEY         ANDNE     *BLANKS
     C*
     C                   MOVEL     SCKEY         CD(XX)
     C*
     C     XX            ADD       27            X                 2 0
     C                   MOVE      *ON           *IN(X)                         SCCDXX POSITION
     C                   ENDIF                                                  *INKL=OFF      F
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C     XX            ADD       26            X                 2 0
     C                   MOVE      *ON           *IN(X)                         SCCDXX POSITION
     C                   ENDIF                                                  *INKL=ON
     C*
     C                   ENDIF                                                  XX >1 & <30
     C*
     C                   ENDIF                                                  HFLD=SCCDXX
     C*
     C                   ENDIF                                                  *INKJ=ON
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
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
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C     *IN09         IFNE      '1'
     C*
     C*  VALIDATE THE PMT CODES
     C*
     C                   Z-ADD     1             X
     C     X             DOUEQ     31
     C*
     C     CD(X)         IFNE      *BLANKS
     C*
     C                   MOVEA     '000000'      *IN(65)
     C     X             DIV       5.1           XXROW             2 0
     C     XXROW         ADD       65            X2                2 0
     C                   MOVE      '1'           *IN(X2)
     C                   MULT      5             XXROW
     C     X             SUB       XXROW         XXCOL             2 0
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     CD(X)         XXKEY
     C     TBKEY         CHAIN     PB201                              60
     C     *IN60         IFEQ      '1'
     C     XXCOL         ADD       21            X1                2 0
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C                   ADD       1             X
     C                   END
     C*
     C*   ALL EDITS PASSED, UPDATE REINSURANCE FILE
     C*
     C     RIKEY         CHAIN     PRI01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEA     CD            RIUNCV
     C                   UPDATE    PRI01
     C                   END
     C*
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPRI01     E            PRI01E
