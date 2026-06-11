     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H/TITLE W7531R - CUMULATIVE LOSS RATIO - BUILD WORKFILE
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM IS A COPY FROM PCA VERSION OF CUMULATIVE RATIO   *
     H* REPORT WITH THE THE EXCEPTION OF EMPLOYER UNDERWRITER.        *
     H* THIS PROGRAM BUILDS THE WORKFILE FOR THE CUMULATIVE LOSS      *
     H* RATIO REPORT.                                                 *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W7531C                                                      *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H* PROGRAMMER:   C:60   09/17/96        PROJECT: 48WDV           *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/12/05 C:132 PROJECT: 114WIP                                *
     H*                Load SIC code from employer master file into   *
     H*                workfile field CUSIC/WXCUP.                    *
     H*                                                               *
     H* 10/09/00 C:61  SUPPORT WC091 24642                            *
     H*                Correct date in work file / outfile.           *
     H*                                                               *
     H*  10/28/98 C:33 SEQ#: WC090/14649                              *
     H*                IF AN EMPLOYER DOES NOT HAVE AN AGENT HISTORY  *
     H*                RECORD, LOAD ZERO TO THE AGENT #.  THIS WILL   *
     H*                ALLOW THE PROGRAM TO RECOGNIZE THE EMPLOYER    *
     H*                WHEN A RANGE OF ALL AGENTS IS SELECTED.        *
     H*                                                               *
     H*  01/07/98 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 REPORT STANDARDS.                    *
     H*                                                               *
     H* 06/26/97 C:62 PROJECT WC710                                   *
     H*               COUNTY CODE NOW IS 4A, STATE + COUNTY.          *
     H*               ALTERNATE COMPANY NAME IF NOT BLANK.            *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H*   20  ADD MEMBER TO WORKFILE                                  *
     H*   60  GENERAL CHAIN/DOWEQ INDICATOR                           *
     H*                                                               *
     H*****************************************************************
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWXCUL     O    E           K DISK
     F*

     D*
     D                 DS
     D  EFDT                   1    035  0
     D                                     DIM(5)                               EFFECTIVE DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CNDT                   1    035  0
     D                                     DIM(5)                               CANCEL DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D* Selection Data
     D                 DS
     D  DATA                   1    100
     D  X1FFYR                 1      3  0
     D  X1TFYR                 4      6  0
     D  X1FEMP                 7     15  0
     D  X1TEMP                16     24  0
     D  X1FDIV                25     29  0
     D  X1TDIV                30     34  0
     D  X1FCNT                35     38
     D  X1TCNT                39     42
     D  X1FCHP                43     45  0
     D  X1TCHP                46     48  0
     D  X1FRGN                49     51  0
     D  X1TRGN                52     54  0
     D  X1FAGT                55     59  0
     D  X1TAGT                60     64  0
     D  SCFLOS                65     71  2
     D  SCTLOS                72     78  2
     D  SCFPRM                79     89  2
     D  SCTPRM                90    100  2
     D*
     D* Keys for employer/division range
     D                 DS
     D  XXKEY                  1     14  0
     D  EMEMP#                 1      9  0
     D  EMDIV                 10     14  0
     D                 DS
     D  XXFKEY                 1     14  0
     D  XXFEMP                 1      9  0
     D  XXFDIV                10     14  0
     D                 DS
     D  XXTKEY                 1     14  0
     D  XXTEMP                 1      9  0
     D  XXTDIV                10     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     EMKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    X1FEMP
     C                   KFLD                    X1FDIV
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C     ELKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFFYR
     C*
     C*   KEY LIST FOR THE AGENT HISTORY FILE
     C*
     C     AHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
     C*
     C     AHKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMEFD1        XXEFDT
     C     *LIKE         DEFINE    EMEFD1        XXRNDT
     C     *LIKE         DEFINE    EMCAD1        XXCNDT
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    AHAGT#        XXAGT#
     C     *LIKE         DEFINE    ELFYR         XXFFYR
     C     *LIKE         DEFINE    CUCNTY        XXCNTY
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQDTE             8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQDATA          100
     C                   PARM                    QQSORT            1
     C                   PARM                    QQACTV            1
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
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQDTE         X1RNDT            8 0
     C*
     C                   Z-ADD     X1RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
     C*
     C                   MOVE      QQDATA        DATA
     C                   Z-ADD     X1FFYR        XXFFYR
     C                   Z-ADD     X1TFYR        XXFYR
     C                   Z-ADD     X1FEMP        XXEMP#
     C                   Z-ADD     X1FDIV        XXDIV
     C                   Z-ADD     X1FEMP        XXFEMP
     C                   Z-ADD     X1FDIV        XXFDIV
     C                   Z-ADD     X1TEMP        XXTEMP
     C                   Z-ADD     X1TDIV        XXTDIV
     C                   TIME                    DSDTTM
     C*
     C*  READ EMPLOYER FILE
     C*
     C     EMKEY1        SETLL     PEM01
     C     EMKEY         READE     PEM01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      EMEMP#        XXEMP#
     C                   MOVE      EMDIV         XXDIV
     C*
     C     EMPST         IFNE      *BLANKS
     C*
     C                   MOVEL     EMPST         XXCNTY
     C*
     C                   ELSE
     C*
     C                   MOVEL     EMMST         XXCNTY
     C*
     C                   ENDIF
     C*
     C                   MOVE      EMCNTY        XXCNTY
     C*
     C*
     C* GET MOST CURRENT ACTIVE AGENT
     C*
     C                   Z-ADD     0             XXAGT#
     C*
     C     AHKEY         SETGT     PAH01
     C     AHKY2         READPE    PAH01                                  50
     C*
     C     *IN50         DOWEQ     *OFF
     C     AHSTAT        ANDEQ     'I'
     C     AHKY2         READPE    PAH01                                  50
     C                   ENDDO                                                  *IN50=*OFF
     C*
     C     *IN50         IFEQ      *OFF
     C                   Z-ADD     AHAGT#        XXAGT#
     C                   ENDIF                                                  *IN50=*OFF
     C*
     C* CHECK TO MAKE SURE SELECTIONS ARE WITHIN RANGE
     C*
     C     QQSORT        IFEQ      'E'
     C     XXKEY         IFLT      XXFKEY
     C     XXKEY         ORGT      XXTKEY
     C                   GOTO      NEXT
     C                   END
     C                   END
     C     QQSORT        IFEQ      'C'
     C     XXCNTY        IFLT      X1FCNT
     C     XXCNTY        ORGT      X1TCNT
     C                   GOTO      NEXT
     C                   END
     C                   END
     C     QQSORT        IFEQ      'H'
     C     EMCHAP        IFLT      X1FCHP
     C     EMCHAP        ORGT      X1TCHP
     C                   GOTO      NEXT
     C                   END
     C                   END
     C     QQSORT        IFEQ      'R'
     C     EMRGN#        IFLT      X1FRGN
     C     EMRGN#        ORGT      X1TRGN
     C                   GOTO      NEXT
     C                   END
     C                   END
     C     QQSORT        IFEQ      'A'
     C     XXAGT#        IFLT      X1FAGT
     C     XXAGT#        ORGT      X1TAGT
     C                   GOTO      NEXT
     C                   END
     C                   END
     C*
     C* FIND THE EMPLOYERS EARLIEST EFFECTIVE DATE
     C*
     C                   MOVE      *ZEROS        XXEFDT
     C                   MOVE      *ZEROS        XXCNDT
     C                   SORTA     EFDT
     C                   SORTA     CNDT
     C                   Z-ADD     1             X                 3 0
     C     X             DOWLT     6
     C     EFDT(X)       IFNE      0
     C                   MOVE      EFDT(X)       XXEFDT
     C                   GOTO      DETUP
     C                   END
     C                   ADD       1             X
     C                   END
     C*
     C* FIND THE LAST CANCEL DATE
     C*
     C     DETUP         TAG
     C     CNDT(5)       IFNE      *ZERO
     C     CNDT(5)       IFLT      EFDT(5)
     C                   MOVE      *ZEROS        XXCNDT
     C                   ELSE
     C                   MOVE      CNDT(5)       XXCNDT
     C                   END
     C                   END
     C*
     C*  IF RUNNING FOR ACTIVE MEMBERS ONLY, CHECK CANCEL DATE
     C*
     C     QQACTV        IFEQ      'Y'
     C     XXCNDT        IFNE      0
     C     XXCNDT        ANDLE     XXRNDT
     C                   MOVE      '1'           *IN20
     C                   ELSE
     C                   MOVE      '0'           *IN20
     C                   END
     C                   END
     C*
     C     *IN20         IFEQ      '0'
     C*
     C* GET EMPLOYERS DETAIL RECORD FOR THE LOWEST POLICY YEAR
     C*
     C     ELKEY1        SETLL     PEL01
     C     ELKEY         READE     PEL01                                  60
     C*
     C* Check to see if employer's policy year is within selected range
     C*
     C     ELFYR         DOWLE     X1TFYR
     C     *IN60         ANDEQ     '0'
     C*
     C* CLEAR FIELDS
     C*
     C                   MOVE      *BLANKS       CUENAM
     C                   MOVE      *BLANKS       CUWALS
     C                   MOVE      *BLANKS       CUCNTY
     C*
     C* LOAD FIELDS TO WORKFILE
     C*
     C                   Z-ADD     EMCO#         CUCO#
     C                   Z-ADD     EMFND         CUFND
     C                   Z-ADD     ELFYR         CUFYR
     C                   Z-ADD     EMEMP#        CUEMP#
     C                   Z-ADD     EMDIV         CUDIV
     C                   MOVEL     EMNAME        CUENAM
     C                   Z-ADD     XXEFDT        CUEFDT
     C                   Z-ADD     XXCNDT        CUCNDT
     C                   MOVEL     EMWALS        CUWALS
     C                   Z-ADD     EMCHAP        CUCHAP
     C                   MOVE      XXCNTY        CUCNTY
     C                   Z-ADD     EMRGN#        CURGN#
     C                   Z-ADD     XXAGT#        CUAGT#
     C                   Z-ADD     ELEXM1        CUEXM1
     C                   Z-ADD     ELEMD1        CUEMD1
     C                   Z-ADD     ELEXM2        CUEXM2
     C                   Z-ADD     ELEMD2        CUEMD2
     C                   Z-ADD     ELEXM3        CUEXM3
     C                   Z-ADD     ELEMD3        CUEMD3
     C                   MOVE      Q1USER        CUUSER
     C                   MOVE      Q1PGM         CUPGM
     C                   Z-ADD     DSTIME        CUTIME
      *
     C                   MOVEL(P)  EMSIC         CUSIC
      *
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CUDT
      *
     C*
     C* LOAD SORT SELECTION FIELD
     C*
     C                   MOVE      *BLANKS       CUSRT1
     C*
     C     QQSORT        IFEQ      'E'
     C                   MOVEL     XXKEY         CUSRT1
     C                   END
     C     QQSORT        IFEQ      'C'
     C                   MOVEL     XXCNTY        CUSRT1
     C                   END
     C     QQSORT        IFEQ      'H'
     C                   MOVEL     EMCHAP        CUSRT1
     C                   END
     C     QQSORT        IFEQ      'R'
     C                   MOVEL     EMRGN#        CUSRT1
     C                   END
     C     QQSORT        IFEQ      'A'
     C                   MOVEL     XXAGT#        CUSRT1
     C                   END
     C*
     C                   WRITE     PCU01
     C*
     C     ELKEY         READE     PEL01                                  60
     C                   END
     C*
     C                   END
     C*
     C     NEXT          TAG
     C     EMKEY         READE     PEM01                                  60
     C                   END
     C*
     C     END           TAG
     C                   MOVE      '1'           *INLR
