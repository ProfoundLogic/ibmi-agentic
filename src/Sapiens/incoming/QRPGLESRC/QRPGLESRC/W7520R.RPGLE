     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7520R - PROMPT FOR CERTIFICATE HOLDERS ROLLOVER
     H*****************************************************************
     H*  REVISIONS:
     H*
     H* 03/09/09  C:190 Project WC1750
     H*                 Change the screen to remove the all existing selection
     H*                 fields and replace them by fields 'From Expiration Date'
     H*                 and 'To Expiration Date'
     F*
     H*  11/24/99   C:118   PROJECT:  WC577
     H*                     MAKE V3 DISPLAY FILE CHANGES.
     H*
     H*  03/10/99   C:118   PROJECT:  WC577
     H*                     ADD EMPLOYER SEARCH WITH LINK TO POLICY YR
     H*****************************************************************
     H*
     FW7520F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*
     F*  USER PARAMETERS MASTER FILE
     F*
     FWMUPL     IF   E           K DISK
     F*
     F*  COMPANY MASTER FILE
     F*
     FSMCOL     IF   E           K DISK
     F*
     F*  FUND MASTER FILE
     F*
     FWMFNL     IF   E           K DISK
     F*
     F***** BEGIN ADD **** 03/10/99 ***********************************
     F                                     INFDS(DBFDSF)
     F***** END ADD ****** 03/10/99 ***********************************
     F*
     F***** BEGIN DLT **** 03/09/09 ***********************************
     F***FUND DETAIL FILE
     F*******
     F*******WDF1L   IF  E           K        DISK
     F*******
     F***EMPLOYER MASTER FILE
     F*******
     F*******WMEML   IF  E           K        DISK
     F*******
     F***** BEGIN ADD **** 03/10/99 **********************************
     F*******                                       KINFDS DBFDSE
     F***** END ADD ****** 03/10/99 **********************************
     F*******
     F***EMPLOYER DETAIL FILE
     F*******
     F*******WDELL1  IF  E           K        DISK
     F*******
     F***CERTIFICATE HOLDERS MASTER FILE
     F*******
     F*******WMCEL   IF  E           K        DISK
     F*******
     F***** BEGIN ADD **** 03/10/99 **********************
     F*******
     F***EMPLOYER MASTER FILE
     F*******
     F*******WMEMP   IF  E                    DISK
     F*******     PEM01                             KRENAMEPEM02
     F*******
     F*******WDELP   IF  E                    DISK
     F*******     PEL01                             KRENAMEPEL02
     F*******                                       KINFDS DBFDSY
     F***** END   DLT **** 03/09/09 ***********************************
     F***** END ADD ****** 03/10/99 **********************

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D***** BEGIN ADD **** 03/10/99 **********************
     D*
     D DBFDSE          DS
     D  DBRRNE               397    400B 0
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D DBFDSY          DS
     D  DBRRNY               397    400B 0

     I*
     I***** END ADD ****** 03/10/99 **********************

     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*  KEY LIST FOR FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C***** BEGIN DLT **** 03/09/09 ***********************************
     C***KEY LIST FOR THE EMPLOYER MASTER FILE
     C*********
     C*********  EMKEY     KLIST
     C*********            KFLD           UPCO#
     C*********            KFLD           XXFND
     C*********            KFLD           XXEMP#
     C*********            KFLD           XXDIV
     C*********
     C***KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*********
     C*********  ELKEY     KLIST
     C*********            KFLD           UPCO#
     C*********            KFLD           XXFND
     C*********            KFLD           XXFYR
     C*********            KFLD           XXEMP#
     C*********            KFLD           XXDIV
     C*********
     C***KEY LIST FOR THE CERTIFICATE HOLDERS FILE
     C*********
     C*********  CEKEY     KLIST
     C*********            KFLD           UPCO#
     C*********            KFLD           XXFND
     C*********            KFLD           XXFYR
     C*********            KFLD           XXEMP#
     C*********            KFLD           XXDIV
     C*********            KFLD           XXCER#  50
     C*********
     C*********  *LIKE     DEFN EMEMP#    XXEMP#
     C*********  *LIKE     DEFN EMDIV     XXDIV
     C***** END   DLT **** 03/09/09 ***********************************
      *** BEGIN ADD *** 03/09/09 **************************************
      *
      *  DEFINE FROM & TO DATES FOR ASCENDING ORDER COMPARISON
      *
     C                   Z-ADD     0             X1FTDT            7 0          FROM DATE
     C                   Z-ADD     0             X1TTDT            7 0          TO DATE
      *
      *** END   ADD *** 03/09/09 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQCER#            5
     C                   PARM                    QQRTC             1
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
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C***** BEGIN DLT **** 03/09/09 ***********************************
     C**********           MOVE QQEMP#    XXEMP#
     C**********           MOVE QQDIV     XXDIV
     C**********           MOVE QQCER#    XXCER#  50
     C***** END   DLT **** 03/09/09 ***********************************
     C*
     C                   Z-ADD     XXFND         SCFND
     C***** BEGIN DLT **** 03/09/09 ***********************************
     C***********          Z-ADDXXFYR     SCFYR
     C***********          Z-ADDXXEMP#    SCEMP#
     C***********          Z-ADDXXDIV     SCDIV
     C***********          Z-ADDXXCER#    SCCER#
     C***** END   DLT **** 03/09/09 ***********************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C     QQFND         IFEQ      '000'
     C                   Z-ADD     UPFND         XXFND
     C***** BEGIN ADD **** 03/09/09 ***********************************
     C                   Z-ADD     XXFND         SCFND
     C***** END   ADD **** 03/09/09 ***********************************
     C                   END
     C***** BEGIN DLT **** 03/09/09 ***********************************
     C********** QQFYR     IFEQ '000'
     C**********           Z-ADDUPFYR     SCFYR
     C**********           END
     C********** QQEMP#    IFEQ '00000'
     C**********           Z-ADDUPEMP#    SCEMP#
     C**********           END
     C********** QQDIV     IFEQ '000'
     C**********           Z-ADDUPDIV     SCDIV
     C**********           END
     C***** END   DLT **** 03/09/09 ***********************************
     C*
     C     TOP           TAG
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*  GET THE FUND RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
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
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTC
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTC
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C***** BEGIN DEL **** 03/10/99 ***********************************
     C*** COMMAND 07 = SEARCH LIST CALL ALPHA
     C*********
     C*********  *INKG     IFEQ '1'
     C*********  Q2RECF    IFEQ 'SCR01'
     C*********            CALL 'W0811R'
     C*********            PARM           QQPRDS
     C*********            PARM           QQCALL
     C*********            PARM           SCEMP#
     C*********            PARM           SCDIV
     C*********            PARM           SCFYR
     C*********            PARM           SCCER#
     C*********            PARM           QQRTC   1
     C*********  QQRTC     CABEQ'A'       TERM
     C*********
     C*********            MOVE *BLANK    Q2RECF
     C*********            MOVEL'SCR01'   Q2RECF
     C*********  QQRTC     IFEQ 'B'
     C*********  SCCER#    OREQ *ZERO
     C*********            GOTO STRT
     C*********            END
     C*********
     C*********  QQRTC     CABEQ'*'       SCR01T
     C*********            END
     C*********
     C*********            END
     C*********
     C***** END DEL ****** 03/10/99 **********************************
     C*
     C***** BEGIN ADD **** 03/10/99 ***********************************
     C***** BEGIN DLT **** 03/09/09 ***********************************
     C**********
     C***EMPLOYER NUMBER/DIVISION SEARCH
     C**********
     C****F10 = SEARCH ON EMPLOYER OR DIVISION
     C********** *INKJ     IFEQ *ON
     C**********
     C****TABLE SEARCH - EMPLOYER
     C**********
     C********** HFLD      IFEQ 'SCEMP#'
     C********** HFLD      OREQ 'SCDIV'
     C**********           MOVE '2'       QQCALL
     C**********           EXSR SRESRC
     C**********           EXSR POSCSR
     C**********
     C*** IF NO EMPLOYER WAS SELECTED, REFRESH SCREEN-OTHERWISE GO
     C****DIRECTLY TO FUND YEAR SEARCH
     C**********
     C********** *INKL     IFEQ *ON
     C********** QQRRN     OREQ *ZERO
     C**********           EXSR POSCSR
     C**********           GOTO STRT
     C**********
     C**********           ELSE
     C**********
     C**********           MOVEL'SCFYR'   HFLD      P
     C**********
     C**********           MOVE *OFF      *IN34
     C**********
     C**********           ENDIF                           *INKL EQ *ON
     C**********           ENDIF                           HFLD IFEQ
     C**********
     C***TABLE SEARCH - POLICY YEAR
     C**********
     C********** HFLD      IFEQ 'SCFYR'
     C********** SCEMP#    IFEQ *ZERO
     C**********
     C**********           MOVE 'Y'       QQCALL
     C**********           EXSR SRESRC
     C**********           Z-ADDQQRRN2    QQRRN
     C**********           EXSR POSCSR
     C**********
     C**********           ELSE
     C**********           MOVELSCFND     XXFND
     C**********           MOVELSCEMP#    XXEMP#
     C**********           MOVELSCDIV     XXDIV
     C********** EMKEY     CHAINPEM01                60
     C**********
     C**********           CALL 'W0051R'
     C********** *INKC     PARM           QQKC    1
     C********** *INKL     PARM           QQKL    1
     C**********           PARM '2'       QQCALL  1
     C**********           PARM 0         QQRRN   90
     C**********           PARM DBRRNE    QQRRN2  90
     C**********           PARM           QQQTE   1
     C**********
     C**********           ENDIF                           SCEMP# EQ *ZERO
     C**********
     C********** *INKC     IFEQ *ON
     C**********           GOTO TERM
     C**********           ENDIF
     C**********
     C********** *INKL     IFEQ *ON
     C********** QQRRN     OREQ *ZERO
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           ENDIF
     C**********
     C********** QQRRN     IFNE *ZERO
     C********** QQRRN     CHAINPEL02                60
     C********** *IN60     IFEQ *OFF
     C**********           MOVELELFYR     SCFYR
     C**********           MOVE *ON       *IN34
     C**********           ENDIF
     C**********           ENDIF                           QQRRN NE *ZERO
     C**********
     C**********           ENDIF                           HFLD EQ SCFYR
     C**********
     C**********           GOTO STRT
     C**********
     C**********           ENDIF
     C***** END ADD ****** 03/10/99 ************************
     C**********
     C****PROCESS HELP REQUESTS
     C**********
     C********** *IN10     IFEQ '1'
     C**********
     C**********           MOVE Q2RECF    XXRECF 10
     C**********           MOVEL'QHLP    'FILNAM 10
     C**********           MOVEL'*LIBL   'LIBNAM 10
     C**********           MOVE Q1PGM     MBRNAM
     C**********           MOVE XXSCR#    MBRNAM 10
     C**********           CALL 'M0006C'
     C**********           PARM           FILNAM
     C**********           PARM           LIBNAM
     C**********           PARM           MBRNAM
     C**********           MOVE XXRECF    Q2RECF
     C**********
     C**********           GOTO STRT
     C**********           END
     C**********
     C***** END   DLT **** 03/09/09 ***********************************
     C*   COMMAND 13 - PROCESS CHANGE OF USER PARAMETERS
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
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      UPFND         SCFND
     C***** BEGIN DLT **** 03/09/09 ***********************************
     C***********          Z-ADDUPFYR     SCFYR
     C***********          Z-ADDUPEMP#    SCEMP#
     C***********          Z-ADDUPDIV     SCDIV
     C***** END   DLT **** 03/09/09 ***********************************
     C                   GOTO      TOP
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C***** BEG   DLT **** 03/09/09 ***********************************
     C*  VALIDATE THE FUND YEAR
     C**********
     C**********           Z-ADDSCFND     XXFND
     C**********           Z-ADDSCFYR     XXFYR
     C********** F1KEY     CHAINPF101                21
     C********** *IN21     CABEQ'1'       STRT
     C**********
     C***EMPLOYER AND DIVISION MUST EXIST IN EMPLOYER MASTER
     C**********
     C**********           Z-ADDSCFND     XXFND
     C**********           Z-ADDSCFYR     XXFYR
     C**********           Z-ADDSCEMP#    XXEMP#
     C**********           Z-ADDSCDIV     XXDIV
     C**********
     C********** XXEMP#    IFNE 0
     C********** EMKEY     CHAINPEM01                20
     C**********
     C********** *IN20     CABEQ'1'       STRT
     C**********
     C***STATUS CODE MUST BE 'A'-ACTIVE
     C**********
     C********** EMSTAT    CABEQ'I'       STRT           20
     C**********
     C**********
     C***THE FUND YEAR MUST BE VALID FOR THE EMPLOYER
     C**********
     C********** ELKEY     CHAINPEL01                20
     C**********
     C********** *IN20     CABEQ'1'       STRT
     C**********
     C**********           END
     C**********
     C** CERTIFICATE NUMBER MUST BE VALID IF ENTERED
     C**********
     C********** SCCER#    IFNE 0
     C**********           Z-ADDSCFND     XXFND
     C**********           Z-ADDSCFYR     XXFYR
     C**********           Z-ADDSCEMP#    XXEMP#
     C**********           Z-ADDSCDIV     XXDIV
     C**********           Z-ADDSCCER#    XXCER#
     C********** CEKEY     CHAINPCE01                24
     C********** *IN24     CABEQ'1'       STRT
     C**********           END
     C**********
     C*** VALIDATE NEW FUND YEAR
     C**********
     C**********           Z-ADDSCNFYR    XXFYR
     C********** F1KEY     CHAINPF101                25
     C********** *IN25     CABEQ'1'       STRT
     C**********
     C**********
     C***** END   DLT **** 03/09/09 ***********************************
     C***** BEGIN ADD **** 03/09/09 ***********************************
      * VALIDATE 'FROM' EXPIRATION DATE
      *
      *
     C                   Z-ADD     SCFTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFTDT
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN41
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR IFEQ
      *
      * VALIDATE 'TO' EXPIRATION DATE
      *
     C                   Z-ADD     SCTTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTTDT
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN42
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR IFEQ
      *
      *
      *  DATES MUST BE IN ASCENDING ORDER
      *
     C                   Z-ADD     SCFTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1FTDT
      *
     C                   Z-ADD     SCTTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1TTDT
      *
     C     X1FTDT        IFGT      X1TTDT
     C                   MOVE      '1'           *IN40
     C                   GOTO      STRT
     C                   END
     C***** END   ADD **** 03/09/09 ***********************************
     C***** BEGIN CHG **** 03/09/09 ***********************************
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      '0'           QQFYR
     C                   MOVE      '0'           QQEMP#
     C                   MOVE      '0'           QQDIV
     C                   MOVE      '0'           QQCER#
     C                   MOVE      '0'           QQNFYR            3
     C                   MOVE      SCFTDT        @@FTDT            8
     C                   MOVE      SCTTDT        @@TTDT            8
     C                   CALL      'W7520C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQCER#
     C                   PARM                    QQNFYR
     C                   PARM      @@FTDT        QQFTDT            8
     C                   PARM      @@TTDT        QQTTDT            8
     C*
     C************         MOVE UPCO#     QQCO#
     C************         MOVE SCFND     QQFND
     C************         MOVE SCFYR     QQFYR
     C************         MOVE SCEMP#    QQEMP#
     C************         MOVE SCDIV     QQDIV
     C************         MOVE SCCER#    QQCER#
     C************         MOVE SCNFYR    QQNFYR  3
     C************         CALL 'W7520C'
     C************         PARM           QQCO#
     C************         PARM           QQFND
     C************         PARM           QQFYR
     C************         PARM           QQEMP#
     C************         PARM           QQDIV
     C************         PARM           QQCER#
     C************         PARM           QQNFYR
     C***** END   CHG **** 03/09/09 ***********************************
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C**** BEGIN ADD ***** 03/10/99 ***********************************
     C***** BEGIN DLT **** 03/09/09 ***********************************
     C*****************************************************************
     C* SUBROUTINE TO SEARCH FOR AND RETURN AN EMPLOYER NUMBER        *
     C*****************************************************************
     C*********
     C*********
     C*********  SRESRC    BEGSR
     C*********            CALL 'W0050R'
     C*********  *INKC     PARM           QQKC    1
     C*********  *INKL     PARM           QQKL    1
     C*********            PARM           QQCALL  1
     C*********            PARM 0         QQRRN   90
     C*********            PARM DBRRNF    QQRRN2  90
     C*********            PARM           QQQTE   1
     C*********
     C*********  *INKC     IFEQ *ON
     C*********            EXSR POSCSR
     C*********            GOTO TERM
     C*********            ENDIF                           INKC IFEQ
     C*********
     C*********  *INKL     IFEQ *ON
     C*********  QQRRN     OREQ *ZERO
     C*********            CLEARQQRRN2
     C*********            EXSR POSCSR
     C*********            MOVEL'SCR01'   Q2RECF    P
     C*********            ENDIF                           *INKL IF EQ
     C*********
     C*  LOAD EMPLOYER NUMBER AND DIVISION
     C*********
     C*********  QQRRN     IFNE *ZERO
     C*********
     C*********  QQRRN     CHAINPEM02                60
     C*********
     C*********  *IN60     IFEQ *OFF
     C*********            MOVELEMEMP#    SCEMP#
     C*********            MOVELEMDIV     SCDIV
     C*********
     C*********            ENDIF                           *IN60 IFEQ
     C*********
     C*********            ENDIF
     C*********
     C*********            ENDSR
     C*********
     C*  SUBROUTINE TO POSITION CURSOR
     C*********
     C*********  POSCSR    BEGSR
     C*********
     C***IF A SELECTION WAS MADE, POSITION CURSOR TO NEXT FIELD
     C*********
     C*********  QQRRN     IFNE *ZERO
     C*********
     C*********            SELEC
     C*********  HFLD      WHEQ 'SCEMP#'
     C*********  HFLD      OREQ 'SCDIV'
     C*********            MOVE *ON       *IN34
     C*********
     C*********  HFLD      WHEQ 'SCFYR'
     C*********            MOVE *ON       *IN34
     C*********
     C*********            ENDSL
     C*********
     C*********            ELSE
     C***IF NO SELECTION WAS MADE, POSITION CURSOR ON SAME FIELD
     C*********
     C*********            SELEC
     C*********  HFLD      WHEQ 'SCEMP#'
     C*********  HFLD      OREQ 'SCDIV'
     C*********            MOVE *ON       *IN32
     C*********  HFLD      WHEQ 'SCFYR'
     C*********            MOVE *ON       *IN33
     C*********
     C*********            ENDSL
     C*********
     C*********            ENDIF                           QQRRN NE *ZERO
     C*********
     C*********            ENDSR
     C*********
     C***** END   DLT **** 03/09/09 ***********************************
     C***** END ADD ****** 03/10/99 **********************************
