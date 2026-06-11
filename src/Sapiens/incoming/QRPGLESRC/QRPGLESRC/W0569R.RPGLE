     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0569R - DWC13 SEARCH
     H*****************************************************************
     H* DESCRIPTION:                                                  *
     H*    THIS PROGRAM SEARCHES THE FILE WSFRP.                      *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*    W4099R                                                     *
     H* CALLS TO:                                                     *
     H*    NONE                                                       *
     H*                                                               *
     H* PROGRAMMER: LARRY KORZON   3/23/92   PROJECT: ANM             *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 09/10/02 C:15  Support: WC094/34135                           *
     H*                Increase FYR from 2.0 to 3.0                   *
     H*                                                               *
     H*****************************************************************
     FW0569F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWSFRL     UF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK

     D FL              S              1    DIM(14)
     D*** Begin chg *** 09/10/02 *************************************
     D******************* YR         14  2 0
     D*** End chg ***** 09/10/02 *************************************
     D MSG1            S             29    DIM(1) CTDATA PERRCD(1)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEYC            S              7  0 DIM(2)
     D*** Begin chg *** 09/10/02 *************************************
     D******************* KEYY        2  2 0
     D KEYY            S              3  0 DIM(2)
     D*** End chg ***** 09/10/02 *************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D*** Begin dlt *** 09/10/02 *************************************
     D********************                    1  28 YR
     D********************                    1   20SCYR01
     D********************                    3   40SCYR02
     D********************                    5   60SCYR03
     D********************                    7   80SCYR04
     D********************                    9  100SCYR05
     D********************                   11  120SCYR06
     D********************                   13  140SCYR07
     D********************                   15  160SCYR08
     D********************                   17  180SCYR09
     D********************                   19  200SCYR10
     D********************                   21  220SCYR11
     D********************                   23  240SCYR12
     D********************                   25  260SCYR13
     D********************                   27  280SCYR14
     D*** End dlt ***** 09/10/02 *************************************
     D*** Begin add *** 09/10/02 *************************************
     D  YR                     1     42  0
     D                                     DIM(14)
     D  SCYR01                 1      3  0
     D  SCYR02                 4      6  0
     D  SCYR03                 7      9  0
     D  SCYR04                10     12  0
     D  SCYR05                13     15  0
     D  SCYR06                16     18  0
     D  SCYR07                19     21  0
     D  SCYR08                22     24  0
     D  SCYR09                25     27  0
     D  SCYR10                28     30  0
     D  SCYR11                31     33  0
     D  SCYR12                34     36  0
     D  SCYR13                37     39  0
     D  SCYR14                40     42  0
     D*** End add ***** 09/10/02 *************************************
     D                 DS
     D  CA                     1     98  0
     D                                     DIM(14)
     D  SCCA01                 1      7  0
     D  SCCA02                 8     14  0
     D  SCCA03                15     21  0
     D  SCCA04                22     28  0
     D  SCCA05                29     35  0
     D  SCCA06                36     42  0
     D  SCCA07                43     49  0
     D  SCCA08                50     56  0
     D  SCCA09                57     63  0
     D  SCCA10                64     70  0
     D  SCCA11                71     77  0
     D  SCCA12                78     84  0
     D  SCCA13                85     91  0
     D  SCCA14                92     98  0
     D                 DS
     D  AD                     1    112  0
     D                                     DIM(14)
     D  SCAD01               001    008  0
     D  SCAD02               009    016  0
     D  SCAD03               017    024  0
     D  SCAD04               025    032  0
     D  SCAD05               033    040  0
     D  SCAD06               041    048  0
     D  SCAD07               049    056  0
     D  SCAD08               057    064  0
     D  SCAD09               065    072  0
     D  SCAD10               073    080  0
     D  SCAD11               081    088  0
     D  SCAD12               089    096  0
     D  SCAD13               097    104  0
     D  SCAD14               105    112  0
     D                 DS
     D  PY                     1    112  0
     D                                     DIM(14)
     D  SCPY01               001    008  0
     D  SCPY02               009    016  0
     D  SCPY03               017    024  0
     D  SCPY04               025    032  0
     D  SCPY05               033    040  0
     D  SCPY06               041    048  0
     D  SCPY07               049    056  0
     D  SCPY08               057    064  0
     D  SCPY09               065    072  0
     D  SCPY10               073    080  0
     D  SCPY11               081    088  0
     D  SCPY12               089    096  0
     D  SCPY13               097    104  0
     D  SCPY14               105    112  0
     D                 DS
     D  CY                     1    112  0
     D                                     DIM(14)
     D  SCCY01               001    008  0
     D  SCCY02               009    016  0
     D  SCCY03               017    024  0
     D  SCCY04               025    032  0
     D  SCCY05               033    040  0
     D  SCCY06               041    048  0
     D  SCCY07               049    056  0
     D  SCCY08               057    064  0
     D  SCCY09               065    072  0
     D  SCCY10               073    080  0
     D  SCCY11               081    088  0
     D  SCCY12               089    096  0
     D  SCCY13               097    104  0
     D  SCCY14               105    112  0
     D                 DS
     D  TD                     1    112  0
     D                                     DIM(14)
     D  SCTD01               001    008  0
     D  SCTD02               009    016  0
     D  SCTD03               017    024  0
     D  SCTD04               025    032  0
     D  SCTD05               033    040  0
     D  SCTD06               041    048  0
     D  SCTD07               049    056  0
     D  SCTD08               057    064  0
     D  SCTD09               065    072  0
     D  SCTD10               073    080  0
     D  SCTD11               081    088  0
     D  SCTD12               089    096  0
     D  SCTD13               097    104  0
     D  SCTD14               105    112  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
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
     C*  DWC13 MASTER FILE
     C*
     C     FRKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     FRKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C     FRKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXTYPE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C*** Begin chg *** 09/10/02 *************************************
     C******************** PARM           QQFYR   20
     C                   PARM                    QQFYR             3 0
     C*** End chg ***** 09/10/02 *************************************
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQRTCD            1
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
     C*  SETUP AND INITIALIZE
     C*
     C     *LIKE         DEFINE    FRFYR         XXFYR
     C     *LIKE         DEFINE    FRCASE        XXCASE
     C     *LIKE         DEFINE    FRTYPE        XXTYPE
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   Z-ADD     QQFYR         SCFYR
     C                   Z-ADD     QQCASE        SCCASE
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     UPFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C     QQFYR         COMP      *ZERO                                  21
     C                   GOTO      REPOS
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     XXUP          IFEQ      '1'
     C                   MOVE      *BLANKS       SCUP
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   END
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   END
     C                   EXFMT     SCR01
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   Z-ADD     *ZERO         QQFYR
     C                   Z-ADD     *ZERO         QQCASE
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   Z-ADD     *ZERO         QQFYR
     C                   Z-ADD     *ZERO         QQCASE
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 23 = DELETE RECORD
     C*
     C     *INKX         IFEQ      '1'
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       7             I1                3 0
     C                   Z-ADD     I1            X                 2 0
     C     I1            IFGE      1
     C     I1            ANDLE     14
     C     I1            ADD       20            Y                 2 0
     C     *IN83         IFEQ      '0'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN83
     C                   MOVE      '1'           *IN(Y)
     C                   MOVE      MSG1(1)       SCMSG1
     C                   Z-ADD     X             XXSVX             2 0
     C                   GOTO      STRT
     C                   ELSE
     C     X             IFEQ      XXSVX
     C                   MOVE      YR(I1)        XXFYR
     C                   MOVE      CA(I1)        XXCASE
     C                   MOVE      '0'           *IN83
     C                   MOVE      '0'           *IN(Y)
     C     FRKEY         SETLL     PFR01
     C     FRKEY         READE     PFR01                                  61
     C     *IN61         DOWEQ     '0'
     C                   DELETE    PFR01
     C     FRKEY         READE     PFR01                                  61
     C                   END
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      KEYY(1)       SCFYR
     C                   MOVE      KEYC(1)       SCCASE
     C                   GOTO      REPOS
     C                   END
     C                   END
     C                   END
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR01T
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR01T
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
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
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
     C                   Z-ADD     KEYY(2)       XXFYR
     C                   Z-ADD     KEYC(2)       XXCASE
     C     FRKEY         CHAIN     PFR01                              60
     C  N60              EXCEPT    PFR01E
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYY(1)       XXFYR
     C                   Z-ADD     KEYC(1)       XXCASE
     C     FRKEY         CHAIN     PFR01                              60
     C  N60              EXCEPT    PFR01E
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCFYR         IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEYC
     C                   MOVE      *BLANK        KEYY
     C                   MOVE      *BLANK        XXUP
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   SETOFF                                       21
     C                   Z-ADD     SCFYR         XXFYR
     C                   MOVE      *BLANKS       SCFYR
     C                   Z-ADD     SCCASE        XXCASE
     C                   MOVE      *BLANKS       SCCASE
     C     FRKEY         SETLL     PFR01
     C     FRKEY1        READE     PFR01                                  60
     C  N60              EXCEPT    PFR01E
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       7             I1                3 0
     C*
     C*  VALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     14
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     YR(I1)        QQFYR
     C                   Z-ADD     CA(I1)        QQCASE
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     *ZERO         QQFYR
     C                   Z-ADD     *ZERO         QQCASE
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   Z-ADD     0             YR
     C                   Z-ADD     0             CA
     C                   Z-ADD     0             AD
     C                   Z-ADD     0             PY
     C                   Z-ADD     0             CY
     C                   Z-ADD     0             TD
     C                   Z-ADD     0             I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   Z-ADD     FRFYR         YR(I1)
     C                   Z-ADD     FRCASE        CA(I1)
     C*
     C                   Z-ADD     FRACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         AD(I1)
     C*
     C                   Z-ADD     XXPRDT        PY(I1)
     C                   Z-ADD     XXCUDT        CY(I1)
     C                   Z-ADD     XXTODT        TD(I1)
     C                   ENDSR
     C*
     C*  LOAD FORWARD SUBROUTINE
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
     C                   EXSR      SRFWDT
     C                   EXSR      FILL
     C     FRKEY         SETGT     PFR01
     C     FRKEY1        READE     PFR01                                  40
     C  N40              EXCEPT    PFR01E
     C   40              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     YR(1)         KEYY(1)
     C                   Z-ADD     YR(14)        KEYY(2)
     C                   Z-ADD     CA(1)         KEYC(1)
     C                   Z-ADD     CA(14)        KEYC(2)
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     15            I1
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
     C                   EXSR      SRBWDT
     C                   EXSR      FILL
     C     FRKEY         SETLL     PFR01
     C     FRKEY1        READPE    PFR01                                  60
     C  N60              EXCEPT    PFR01E
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     YR(1)         KEYY(1)
     C                   Z-ADD     YR(14)        KEYY(2)
     C                   Z-ADD     CA(1)         KEYC(1)
     C                   Z-ADD     CA(14)        KEYC(2)
     C                   ENDSR
     C*
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     *ZERO         XXFYR
     C                   Z-ADD     *ZERO         XXCASE
     C     FRKEY         SETLL     PFR01
     C     FRKEY1        READE     PFR01                                  60
     C  N60              EXCEPT    PFR01E
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C                   ADD       1             I1
     C                   EXSR      SRFWDT
     C                   EXSR      FILL
     C     FRKEY1        READE     PFR01                                  60
     C  N60              EXCEPT    PFR01E
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C*****************************************************************
     C*  THIS SUBROUTINE LOADS DATES BY READING FILE FORWARDS
     C*****************************************************************
     C     SRFWDT        BEGSR
     C*
     C                   Z-ADD     FRFYR         XXFYR
     C                   Z-ADD     FRCASE        XXCASE
     C                   Z-ADD     0             XXPRDT            8 0
     C                   Z-ADD     0             XXCUDT            8 0
     C                   Z-ADD     0             XXTODT            8 0
     C                   MOVE      '0'           *IN50
     C*
     C                   MOVE      'P'           XXTYPE
     C     FRKEY2        CHAIN     PFR01                              50
     C  N50              EXCEPT    PFR01E
     C*
     C                   Z-ADD     FRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPRDT
     C*
     C                   MOVE      'C'           XXTYPE
     C     FRKEY2        CHAIN     PFR01                              50
     C  N50              EXCEPT    PFR01E
     C*
     C                   Z-ADD     FRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCUDT
     C*
     C                   MOVE      'T'           XXTYPE
     C     FRKEY2        CHAIN     PFR01                              50
     C  N50              EXCEPT    PFR01E
     C*
     C                   Z-ADD     FRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*  THIS SUBROUTINE LOADS DATES BY READING FILE BACKWARDS
     C*****************************************************************
     C*
     C     SRBWDT        BEGSR
     C*
     C                   Z-ADD     FRFYR         XXFYR
     C                   Z-ADD     FRCASE        XXCASE
     C                   Z-ADD     0             XXPRDT
     C                   Z-ADD     0             XXCUDT
     C                   Z-ADD     0             XXTODT
     C                   MOVE      '0'           *IN50
     C*
     C                   MOVE      'T'           XXTYPE
     C     FRKEY2        CHAIN     PFR01                              50
     C  N50              EXCEPT    PFR01E
     C*
     C                   Z-ADD     FRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   MOVE      'C'           XXTYPE
     C     FRKEY2        CHAIN     PFR01                              50
     C  N50              EXCEPT    PFR01E
     C*
     C                   Z-ADD     FRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCUDT
     C*
     C                   MOVE      'P'           XXTYPE
     C     FRKEY2        CHAIN     PFR01                              50
     C  N50              EXCEPT    PFR01E
     C*
     C                   Z-ADD     FRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPRDT
     C*
     C                   ENDSR
     C*****************************************************************
     OPFR01     E            PFR01E
**  TABLE MSG1
Press CMD 23 again to delete.
**
ROLL UP
ROLL DOWN
