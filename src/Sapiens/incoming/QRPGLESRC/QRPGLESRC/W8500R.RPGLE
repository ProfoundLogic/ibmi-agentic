     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*                                                               *
     H/TITLE W8500R - PROMPT FOR FUND, FUND YEAR, EMP#, AND DIV       *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS                                                    *
     H*                                                               *
     H*  12/03/99  C:118  PROJECT: WC577                              *
     H*                   V3 SCREEN FIXES.                            *
     H*                                                               *
     H*  11/03/99  C:118  PROJECT: WC577                              *
     H*                   Add F10=Search on Policy year and Employer/ *
     H*                   Division. Add error check if year and emp   *
     H*                   left blank.                                 *
     H*                                                               *
     H*****************************************************************
     FW8500F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F***** BEGIN ADD **** 11/03/99 ***********************************
     F                                     INFDS(DBFDS)
     F***** END   ADD **** 11/03/99 ***********************************
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMEML     IF   E           K DISK
     F***** BEGIN ADD **** 11/03/99 ***********************************
     F                                     INFDS(DBFDS2)
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL02)
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     F***** END   ADD **** 11/03/99 ***********************************

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
     D***** BEGIN ADD **** 11/03/99 ***********************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0

     I***** END   ADD **** 11/03/99 ***********************************

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
     C*  FUND YEAR KEY LIST.
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR             3 0
     C*
     C***** BEGIN ADD **** 11/03/99 ***********************************
     C*  EMPLOYER DETAIL KEY LIST
     C*
     C     ELKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCFYR
     C*
     C     EMKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C***** END   ADD **** 11/03/99 ***********************************
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
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
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
     C                   MOVE      UPFYR         SCFYR
     C                   MOVE      UPEMP#        SCEMP#
     C                   MOVE      UPDIV         SCDIV
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C***** BEGIN ADD **** 12/03/99 ***********************************
     C     *IN60         IFEQ      *OFF
     C***** END   ADD **** 12/03/99 ***********************************
     C                   MOVEL     FNNAME        SCNAME
     C***** BEGIN ADD **** 12/03/99 ***********************************
     C                   ELSE
     C                   MOVEL     *BLANKS       SCNAME
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 12/03/99 ***********************************
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
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
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C***** BEGIN ADD **** 11/03/99 ***********************************
     C*
     C*  COMMAND F10 = SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCFYR'
     C*
     C*  If cursor position on search is SCFYR, select Emp/div first
     C*
     C     SCEMP#        IFEQ      *ZEROS
     C*
     C                   CALL      'W0050R'
     C                   PARM                    QQKC              1
     C                   PARM                    QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM      DBRRN         QQRRN2            9 0
     C                   PARM                    QQQTE             1
     C                   PARM      1             QQVIEW            1 0
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC=ON
     C*
     C     *INKL         IFEQ      *ON
     C     QQRRN         OREQ      *ZEROS
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL=ON
     C*                                                    QQRRN=O
     C     QQRRN         IFNE      *ZEROS
     C*
     C     QQRRN         CHAIN     PEM02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         XXFND
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C***** BEGIN ADD **** 12/03/99 ***********************************
     C     *IN60         IFEQ      *OFF
     C***** END   ADD **** 12/03/99 ***********************************
     C                   MOVEL     FNNAME        SCNAME
     C***** BEGIN ADD **** 12/03/99 ***********************************
     C                   ELSE
     C                   MOVEL     *BLANKS       SCNAME
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 12/03/99 ***********************************
     C*
     C                   ENDIF                                                  QQRRN<>0
     C*
     C                   ENDIF                                                  SCEMP#=0
     C*
     C*  Call Emp policy years
     C*
     C     EMKY2         CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   CALL      'W0051R'
     C                   PARM                    QQKC
     C                   PARM                    QQKL
     C                   PARM      '2'           QQCALL
     C                   PARM      0             QQRRN
     C                   PARM      DBRRN2        QQRRN2
     C                   PARM                    QQQTE
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC=ON
     C*
     C     *INKL         IFEQ      *ON
     C     QQRRN         OREQ      *ZEROS
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL=ON
     C*                                                    QQRRN=0
     C     QQRRN         IFNE      *ZERO
     C*
     C     QQRRN         CHAIN     PEL02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELFYR         SCFYR
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  QQRRN<>0
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C*  If cursor position is Emp/div on search call WW Emp.
     C*
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   CALL      'W0050R'
     C                   PARM                    QQKC              1
     C                   PARM                    QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM      DBRRN         QQRRN2            9 0
     C                   PARM                    QQQTE             1
     C                   PARM      1             QQVIEW            1 0
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC=ON
     C*
     C     *INKL         IFEQ      *ON
     C     QQRRN         OREQ      *ZEROS
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL=ON
     C*                                                    QQRRN=0
     C     QQRRN         IFNE      *ZEROS
     C*
     C     QQRRN         CHAIN     PEM02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   ENDIF                                                  *IN60=OFF
     C                   ENDIF                                                  QQRRN<>0
     C*
     C*  If policy year is blank call Emp policy years
     C*
     C     SCFYR         IFEQ      *ZEROS
     C*
     C     EMKY2         CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   CALL      'W0051R'
     C                   PARM                    QQKC
     C                   PARM                    QQKL
     C                   PARM      '2'           QQCALL
     C                   PARM      0             QQRRN
     C                   PARM      DBRRN2        QQRRN2
     C                   PARM                    QQQTE
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC=ON
     C*
     C     *INKL         IFEQ      *ON
     C     QQRRN         OREQ      *ZEROS
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL=ON
     C*                                                    QQRRN=0
     C     QQRRN         IFNE      *ZERO
     C*
     C     QQRRN         CHAIN     PEL02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELFYR         SCFYR
     C                   GOTO      STRT
     C                   ELSE
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  QQRRN<>0
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCFYR=0
     C*
     C                   GOTO      STRT
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *INKJ=OFF
     C*
     C***** END   ADD **** 11/03/99 ***********************************
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
     C*  VALIDATE THE FUND YEAR ENTERED.
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C     F1KEY         CHAIN     PF101                              20
     C     *IN20         CABEQ     '1'           STRT
     C*
     C*  VALIDATE EMP# & DIV IFNE 0
     C*
     C     SCEMP#        IFNE      0
     C     SCDIV         ORNE      0
     C     EMKEY         CHAIN     PEM01                              21
     C     *IN21         CABEQ     '1'           STRT
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      SCDIV         QQDIV             5
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   CALL      'W8500C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQOUTQ
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
