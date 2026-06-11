     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE S0502R - SYSTEM PROGRAM FILE SEARCH
     FS0502F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK

     D FL              S              1    DIM(14)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEY             S             11    DIM(2)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  CO                     1    140
     D                                     DIM(14)
     D  SCCO01                 1     10
     D  SCCO02                11     20
     D  SCCO03                21     30
     D  SCCO04                31     40
     D  SCCO05                41     50
     D  SCCO06                51     60
     D  SCCO07                61     70
     D  SCCO08                71     80
     D  SCCO09                81     90
     D  SCCO10                91    100
     D  SCCO11               101    110
     D  SCCO12               111    120
     D  SCCO13               121    130
     D  SCCO14               131    140
     D                 DS
     D  NM                     1    840
     D                                     DIM(14)
     D  SCNM01                 1     60
     D  SCNM02                61    120
     D  SCNM03               121    180
     D  SCNM04               181    240
     D  SCNM05               241    300
     D  SCNM06               301    360
     D  SCNM07               361    420
     D  SCNM08               421    480
     D  SCNM09               481    540
     D  SCNM10               541    600
     D  SCNM11               601    660
     D  SCNM12               661    720
     D  SCNM13               721    780
     D  SCNM14               781    840
     D                 DS
     D  F#                     1     14
     D                                     DIM(14)
     D  SCF#01                 1      1
     D  SCF#02                 2      2
     D  SCF#03                 3      3
     D  SCF#04                 4      4
     D  SCF#05                 5      5
     D  SCF#06                 6      6
     D  SCF#07                 7      7
     D  SCF#08                 8      8
     D  SCF#09                 9      9
     D  SCF#10                10     10
     D  SCF#11                11     11
     D  SCF#12                12     12
     D  SCF#13                13     13
     D  SCF#14                14     14
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE PROGRAM FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXPGM            10
     C                   KFLD                    XXFIL#            1
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY1         KLIST
     C                   KFLD                    UPCO#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQKEY            10
     C                   PARM                    QQKEY1            1
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
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C     COKY1         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C                   MOVEL     QQKEY         SCPGM
     C                   MOVEL     QQKEY1        SCFIL#
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
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C********************************
     C*   F3 = EXIT                  *
     C********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      *BLANKS       QQKEY1
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C********************************
     C*   F12 = PREVIOUS             *
     C********************************
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      *BLANKS       QQKEY1
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C********************************
     C*   *IN18 = ROLL UP REQUEST    *
     C********************************
     C     *IN18         CABEQ     '1'           SCR01T
     C********************************
     C*   *IN19= ROLL DOWN REQUEST   *
     C********************************
     C     *IN19         CABEQ     '1'           SCR01T
     C********************************
     C*   *IN10 PROCESS HELP REQUESTS *
     C********************************
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
     C********
     C                   END
     C*******************************************************************
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
     C                   MOVEL     KEY(2)        XXPGM
     C                   MOVE      KEY(2)        XXFIL#
     C     COKEY         CHAIN     POR01                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     KEY(1)        XXPGM
     C                   MOVE      KEY(1)        XXFIL#
     C     COKEY         CHAIN     POR01                              60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCPGM         IFNE      *BLANKS
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEY
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVE      SCPGM         XXPGM
     C                   MOVE      SCFIL#        XXFIL#
     C                   MOVE      *BLANK        SCPGM
     C                   MOVE      *BLANK        SCFIL#
     C     COKEY         SETLL     POR01
     C                   READ      POR01                                  60
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
     C                   MOVE      CO(I1)        QQKEY
     C                   MOVE      F#(I1)        QQKEY1
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANKS       QQKEY
     C                   MOVE      *BLANKS       QQKEY1
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
     C                   MOVE      *BLANKS       CO
     C                   MOVE      *BLANKS       NM
     C                   MOVE      *BLANKS       F#
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVE      ORPNAM        CO(I1)
     C                   MOVEL     ORTITL        NM(I1)
     C                   MOVEL     ORFIL#        F#(I1)
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
     C                   EXSR      FILL
     C                   READ      POR01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVEL     CO(1)         KEY(1)
     C                   MOVE      F#(1)         KEY(1)
     C                   MOVEL     CO(14)        KEY(2)
     C                   MOVE      F#(14)        KEY(2)
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
     C                   EXSR      FILL
     C                   READP     POR01                                  60
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVEL     CO(1)         KEY(1)
     C                   MOVE      F#(1)         KEY(1)
     C                   MOVEL     CO(14)        KEY(2)
     C                   MOVE      F#(14)        KEY(2)
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
     C                   MOVE      *BLANKS       XXPGM
     C     COKEY         SETLL     POR01
     C                   READ      POR01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C                   READ      POR01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
**
ROLL UP
ROLL DOWN
