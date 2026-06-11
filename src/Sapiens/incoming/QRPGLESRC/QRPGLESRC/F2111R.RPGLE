     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE F2111R - CLAIM STATUS CODE MASTER FILE SEARCH
     H*
     H*****************************************************************
     H* REVISION:
     H*
     H* 09/25/01 C:62 PROJECT:FSMNT
     H*               EXPAND ARRAY TO 5.2
     H*
     H*****************************************************************
     FF2111F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FFTABL     IF   E           K DISK
     FSMUPL     IF   E           K DISK

     D FL              S              1    DIM(14)                                POSITIONAL FLAG
     D*** BEGIN CHG *** 09/25/01 **************************************
     D**********          OP         14  3 0               OVERRIDE %
     D*** END   CHG *** 09/25/01 **************************************
     D RR              S              9    DIM(2) CTDATA PERRCD(1)                ROLL KEY LABELS
     D KEY             S              3    DIM(2)                                 KEY - CLM STAT COD
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  PR                     1     42
     D                                     DIM(14)                                CLAIM STATUS CODE
     D  SCPR01                 1      3
     D  SCPR02                 4      6
     D  SCPR03                 7      9
     D  SCPR04                10     12
     D  SCPR05                13     15
     D  SCPR06                16     18
     D  SCPR07                19     21
     D  SCPR08                22     24
     D  SCPR09                25     27
     D  SCPR10                28     30
     D  SCPR11                31     33
     D  SCPR12                34     36
     D  SCPR13                37     39
     D  SCPR14                40     42
     D                 DS
     D  NM                     1    560
     D                                     DIM(14)                                CODE DESCRIPTION
     D  SCNM01                 1     40
     D  SCNM02                41     80
     D  SCNM03                81    120
     D  SCNM04               121    160
     D  SCNM05               161    200
     D  SCNM06               201    240
     D  SCNM07               241    280
     D  SCNM08               281    320
     D  SCNM09               321    360
     D  SCNM10               361    400
     D  SCNM11               401    440
     D  SCNM12               441    480
     D  SCNM13               481    520
     D  SCNM14               521    560
     D                 DS
     D  DP                     1     14
     D                                     DIM(14)                                DISPOSITION
     D  SCDP01                 1      1
     D  SCDP02                 2      2
     D  SCDP03                 3      3
     D  SCDP04                 4      4
     D  SCDP05                 5      5
     D  SCDP06                 6      6
     D  SCDP07                 7      7
     D  SCDP08                 8      8
     D  SCDP09                 9      9
     D  SCDP10                10     10
     D  SCDP11                11     11
     D  SCDP12                12     12
     D  SCDP13                13     13
     D  SCDP14                14     14
     D                 DS
     D  OV                     1     14
     D                                     DIM(14)                                OVERRIDE FLAG
     D  SCOV01                 1      1
     D  SCOV02                 2      2
     D  SCOV03                 3      3
     D  SCOV04                 4      4
     D  SCOV05                 5      5
     D  SCOV06                 6      6
     D  SCOV07                 7      7
     D  SCOV08                 8      8
     D  SCOV09                 9      9
     D  SCOV10                10     10
     D  SCOV11                11     11
     D  SCOV12                12     12
     D  SCOV13                13     13
     D  SCOV14                14     14
     D                 DS
     D*** BEGIN CHG *** 09/25/01 **************************************
     D**********                              1  420OP
     D**********                              1   30SCOP01
     D**********                              4   60SCOP02
     D**********                              7   90SCOP03
     D**********                             10  120SCOP04
     D**********                             13  150SCOP05
     D**********                             16  180SCOP06
     D**********                             19  210SCOP07
     D**********                             22  240SCOP08
     D**********                             25  270SCOP09
     D**********                             28  300SCOP10
     D**********                             31  330SCOP11
     D**********                             34  360SCOP12
     D**********                             37  390SCOP13
     D**********                             40  420SCOP14
     D  OP                     1     70  2
     D                                     DIM(14)                                OVERRIDE %
     D  SCOP01                 1      5  2
     D  SCOP02                 6     10  2
     D  SCOP03                11     15  2
     D  SCOP04                16     20  2
     D  SCOP05                21     25  2
     D  SCOP06                26     30  2
     D  SCOP07                31     35  2
     D  SCOP08                36     40  2
     D  SCOP09                41     45  2
     D  SCOP10                46     50  2
     D  SCOP11                51     55  2
     D  SCOP12                56     60  2
     D  SCOP13                61     65  2
     D  SCOP14                66     70  2
     D*** END   CHG *** 09/25/01 **************************************
     D                 DS
     D  ST                     1     14
     D                                     DIM(14)                                STATUS
     D  SCST01                 1      1
     D  SCST02                 2      2
     D  SCST03                 3      3
     D  SCST04                 4      4
     D  SCST05                 5      5
     D  SCST06                 6      6
     D  SCST07                 7      7
     D  SCST08                 8      8
     D  SCST09                 9      9
     D  SCST10                10     10
     D  SCST11                11     11
     D  SCST12                12     12
     D  SCST13                13     13
     D  SCST14                14     14
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  CLAIN STATUS CODE TABLE FILE
     C*
     C     CSKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
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
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQKEY             3
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
     C     COKEY         CHAIN     PCO01                              60
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
     C                   MOVEL     QQKEY         SCCSCD
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
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      *BLANK        QQKEY
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *BLANK        QQKEY
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
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
     C                   MOVEL     KEY(2)        XXKEY
     C     CSKY1         CHAIN     FTABL                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     KEY(1)        XXKEY
     C     CSKY1         CHAIN     FTABL                              60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCCSCD        IFNE      *BLANK
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEY
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVEL     SCCSCD        XXKEY
     C                   MOVE      *BLANK        SCCSCD
     C     CSKY1         SETLL     FTABL
     C     UPCO#         READE     PCS01                                  60
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
     C                   MOVEL     PR(I1)        QQKEY
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANK        QQKEY
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
     C                   MOVE      *BLANKS       PR
     C                   MOVE      *BLANKS       NM
     C                   MOVE      *BLANKS       DP
     C                   MOVE      *BLANKS       OV
     C                   MOVE      *BLANKS       OP
     C                   MOVE      *BLANKS       ST
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVEL     CSKEY         PR(I1)
     C                   MOVEL     CSDESC        NM(I1)
     C                   MOVE      CSDISC        DP(I1)
     C                   MOVE      CSOVRD        OV(I1)
     C                   MOVE      CSOVRP        OP(I1)
     C                   MOVE      CSSTAT        ST(I1)
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
     C     UPCO#         READE     PCS01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVEL     PR(1)         KEY(1)
     C                   MOVEL     PR(14)        KEY(2)
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
     C                   READP     PCS01                                  60
     C  N60CSCO#         COMP      UPCO#                              6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVEL     PR(1)         KEY(1)
     C                   MOVEL     PR(14)        KEY(2)
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
     C                   MOVE      *BLANK        XXKEY
     C     CSKY1         SETLL     FTABL
     C     UPCO#         READE     PCS01                                  60
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
     C     UPCO#         READE     PCS01                                  60
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
