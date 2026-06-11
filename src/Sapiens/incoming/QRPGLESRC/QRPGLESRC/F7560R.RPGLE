     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE F7560R - CASE TRANSFER SEARCH
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*  08/06/91 TMN -ADD COMMAND 23 & ALLOW DELETION OF RECORDS.    *
     H*                CHANGED FILE FACTP FROM INPUT TO UPDATE.       *
     H*                                                               *
     H*****************************************************************
     FF7560F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFACTP     UF   E           K DISK

     D FL              S              1    DIM(14)
     D ST              S              1    DIM(14)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEY1            S              3  0 DIM(2)
     D KEY2            S              3  0 DIM(2)
     D KEY3            S              3  0 DIM(2)
     D KEY4            S              7  0 DIM(2)
     D KEY5            S              5  0 DIM(5)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  C1                     1     42  0
     D                                     DIM(14)
     D  SCC101                 1      3  0
     D  SCC102                 4      6  0
     D  SCC103                 7      9  0
     D  SCC104                10     12  0
     D  SCC105                13     15  0
     D  SCC106                16     18  0
     D  SCC107                19     21  0
     D  SCC108                22     24  0
     D  SCC109                25     27  0
     D  SCC110                28     30  0
     D  SCC111                31     33  0
     D  SCC112                34     36  0
     D  SCC113                37     39  0
     D  SCC114                40     42  0
     D                 DS
     D  C2                     1     42  0
     D                                     DIM(14)
     D  SCC201                 1      3  0
     D  SCC202                 4      6  0
     D  SCC203                 7      9  0
     D  SCC204                10     12  0
     D  SCC205                13     15  0
     D  SCC206                16     18  0
     D  SCC207                19     21  0
     D  SCC208                22     24  0
     D  SCC209                25     27  0
     D  SCC210                28     30  0
     D  SCC211                31     33  0
     D  SCC212                34     36  0
     D  SCC213                37     39  0
     D  SCC214                40     42  0
     D                 DS
     D  S1                     1     70  0
     D                                     DIM(14)
     D  SCS101                 1      5  0
     D  SCS102                 6     10  0
     D  SCS103                11     15  0
     D  SCS104                16     20  0
     D  SCS105                21     25  0
     D  SCS106                26     30  0
     D  SCS107                31     35  0
     D  SCS108                36     40  0
     D  SCS109                41     45  0
     D  SCS110                46     50  0
     D  SCS111                51     55  0
     D  SCS112                56     60  0
     D  SCS113                61     65  0
     D  SCS114                66     70  0
     D                 DS
     D  S2                     1     70  0
     D                                     DIM(14)
     D  SCS201                 1      5  0
     D  SCS202                 6     10  0
     D  SCS203                11     15  0
     D  SCS204                16     20  0
     D  SCS205                21     25  0
     D  SCS206                26     30  0
     D  SCS207                31     35  0
     D  SCS208                36     40  0
     D  SCS209                41     45  0
     D  SCS210                46     50  0
     D  SCS211                51     55  0
     D  SCS212                56     60  0
     D  SCS213                61     65  0
     D  SCS214                66     70  0
     D                 DS
     D  F1                     1     42  0
     D                                     DIM(14)
     D  SCF101                 1      3  0
     D  SCF102                 4      6  0
     D  SCF103                 7      9  0
     D  SCF104                10     12  0
     D  SCF105                13     15  0
     D  SCF106                16     18  0
     D  SCF107                19     21  0
     D  SCF108                22     24  0
     D  SCF109                25     27  0
     D  SCF110                28     30  0
     D  SCF111                31     33  0
     D  SCF112                34     36  0
     D  SCF113                37     39  0
     D  SCF114                40     42  0
     D                 DS
     D  F2                     1     42  0
     D                                     DIM(14)
     D  SCF201                 1      3  0
     D  SCF202                 4      6  0
     D  SCF203                 7      9  0
     D  SCF204                10     12  0
     D  SCF205                13     15  0
     D  SCF206                16     18  0
     D  SCF207                19     21  0
     D  SCF208                22     24  0
     D  SCF209                25     27  0
     D  SCF210                28     30  0
     D  SCF211                31     33  0
     D  SCF212                34     36  0
     D  SCF213                37     39  0
     D  SCF214                40     42  0
     D                 DS
     D  Y1                     1     42  0
     D                                     DIM(14)
     D  SCY101                 1      3  0
     D  SCY102                 4      6  0
     D  SCY103                 7      9  0
     D  SCY104                10     12  0
     D  SCY105                13     15  0
     D  SCY106                16     18  0
     D  SCY107                19     21  0
     D  SCY108                22     24  0
     D  SCY109                25     27  0
     D  SCY110                28     30  0
     D  SCY111                31     33  0
     D  SCY112                34     36  0
     D  SCY113                37     39  0
     D  SCY114                40     42  0
     D                 DS
     D  Y2                     1     42  0
     D                                     DIM(14)
     D  SCY201                 1      3  0
     D  SCY202                 4      6  0
     D  SCY203                 7      9  0
     D  SCY204                10     12  0
     D  SCY205                13     15  0
     D  SCY206                16     18  0
     D  SCY207                19     21  0
     D  SCY208                22     24  0
     D  SCY209                25     27  0
     D  SCY210                28     30  0
     D  SCY211                31     33  0
     D  SCY212                34     36  0
     D  SCY213                37     39  0
     D  SCY214                40     42  0
     D                 DS
     D  A1                     1     98  0
     D                                     DIM(14)
     D  SCA101                 1      7  0
     D  SCA102                 8     14  0
     D  SCA103                15     21  0
     D  SCA104                22     28  0
     D  SCA105                29     35  0
     D  SCA106                36     42  0
     D  SCA107                43     49  0
     D  SCA108                50     56  0
     D  SCA109                57     63  0
     D  SCA110                64     70  0
     D  SCA111                71     77  0
     D  SCA112                78     84  0
     D  SCA113                85     91  0
     D  SCA114                92     98  0
     D                 DS
     D  A2                     1     98  0
     D                                     DIM(14)
     D  SCA201                 1      7  0
     D  SCA202                 8     14  0
     D  SCA203                15     21  0
     D  SCA204                22     28  0
     D  SCA205                29     35  0
     D  SCA206                36     42  0
     D  SCA207                43     49  0
     D  SCA208                50     56  0
     D  SCA209                57     63  0
     D  SCA210                64     70  0
     D  SCA211                71     77  0
     D  SCA212                78     84  0
     D  SCA213                85     91  0
     D  SCA214                92     98  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

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
     C*  TRANSFER FILE
     C*
     C     CTKEY1        KLIST
     C                   KFLD                    UPCO#             3 0
     C                   KFLD                    XXSUB#            5 0
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXCASE            7 0
     C*
     C*  TRANSFER FILE
     C*
     C     CTKEY2        KLIST
     C                   KFLD                    UPCO#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQSUB#            5 0
     C                   PARM                    QQFND             3 0
     C                   PARM                    QQFYR             3 0
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
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C*
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET COMPANY NAME
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCCONM
     C                   ELSE
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C                   MOVE      QQCO#         SCCO#
     C                   MOVE      QQSUB#        SCSUB#
     C                   MOVE      QQFND         SCFND
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQCASE        SCCASE
     C     QQCO#         COMP      *ZEROS                                 21
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
     C                   MOVE      *ZEROS        QQCO#
     C                   MOVE      *ZEROS        QQSUB#
     C                   MOVE      *ZEROS        QQFND
     C                   MOVE      *ZEROS        QQFYR
     C                   MOVE      *ZEROS        QQCASE
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *ZEROS        QQCO#
     C                   MOVE      *ZEROS        QQSUB#
     C                   MOVE      *ZEROS        QQFND
     C                   MOVE      *ZEROS        QQFYR
     C                   MOVE      *ZEROS        QQCASE
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 23 = DELETE SELECTED RECORD
     C*
     C     *INKX         IFEQ      '1'
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       7             I1                3 0
     C*
     C*  VALID CURSOR POSITION - RETURN WITH SELECTION
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     14
     C                   Z-ADD     C1(I1)        XXCO#
     C                   Z-ADD     S1(I1)        XXSUB#
     C                   Z-ADD     F1(I1)        XXFND
     C                   Z-ADD     Y1(I1)        XXFYR
     C                   Z-ADD     A1(I1)        XXCASE
     C**
     C     CTKEY1        CHAIN     PCT01                              60
     C     *IN60         IFEQ      '0'
     C                   DELETE    PCT01
     C                   END
     C*
     C*  REDISPLAY SCREEN AFTER RECORD IS DELETED
     C*
     C                   EXSR      LOADB
     C                   END
     C*
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
     C                   MOVE      KEY1(2)       XXCO#
     C                   MOVE      KEY5(2)       XXSUB#
     C                   MOVE      KEY2(2)       XXFND
     C                   MOVE      KEY3(2)       XXFYR
     C                   MOVE      KEY4(2)       XXCASE
     C     CTKEY1        CHAIN     PCT01                              60
     C                   READ      PCT01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      KEY1(1)       XXCO#
     C                   MOVE      KEY5(1)       XXSUB#
     C                   MOVE      KEY2(1)       XXFND
     C                   MOVE      KEY3(1)       XXFYR
     C                   MOVE      KEY4(1)       XXCASE
     C     CTKEY1        CHAIN     PCT01                              60
     C                   READP     PCT01                                  60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCCO#         IFNE      *ZEROS
     C     REPOS         TAG
     C   21              MOVE      '1'           XXDOWN
     C  N21              MOVE      *BLANK        XXDOWN
     C                   MOVE      '0'           *IN21
     C                   MOVE      *BLANKS       KEY1
     C                   MOVE      *BLANKS       KEY2
     C                   MOVE      *BLANKS       KEY3
     C                   MOVE      *BLANKS       KEY4
     C                   MOVE      *BLANKS       KEY5
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      SCCO#         XXCO#
     C                   MOVE      *ZEROS        SCCO#
     C                   MOVE      SCSUB#        XXSUB#
     C                   MOVE      *ZEROS        SCSUB#
     C                   MOVE      SCFND         XXFND
     C                   MOVE      *ZEROS        SCFND
     C                   MOVE      SCFYR         XXFYR
     C                   MOVE      *ZEROS        SCFYR
     C                   MOVE      SCCASE        XXCASE
     C                   MOVE      *ZEROS        SCCASE
     C     CTKEY1        SETLL     PCT01
     C                   READ      PCT01                                  60
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
     C                   MOVE      C1(I1)        QQCO#
     C                   MOVE      S1(I1)        QQSUB#
     C                   MOVE      F1(I1)        QQFND
     C                   MOVE      Y1(I1)        QQFYR
     C                   MOVE      A1(I1)        QQCASE
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *ZEROS        QQCO#
     C                   MOVE      *ZEROS        QQSUB#
     C                   MOVE      *ZEROS        QQFND
     C                   MOVE      *ZEROS        QQFYR
     C                   MOVE      *ZEROS        QQCASE
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
     C                   MOVE      *ZEROS        C1
     C                   MOVE      *ZEROS        C2
     C                   MOVE      *ZEROS        S1
     C                   MOVE      *ZEROS        S2
     C                   MOVE      *ZEROS        F1
     C                   MOVE      *ZEROS        F2
     C                   MOVE      *ZEROS        Y1
     C                   MOVE      *ZEROS        Y2
     C                   MOVE      *ZEROS        A1
     C                   MOVE      *ZEROS        A2
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVE      CTFCO#        C1(I1)
     C                   MOVE      CTTCO#        C2(I1)
     C                   MOVE      CTFSUB        S1(I1)
     C                   MOVE      CTTSUB        S2(I1)
     C                   MOVE      CTFFND        F1(I1)
     C                   MOVE      CTTFND        F2(I1)
     C                   MOVE      CTFFYR        Y1(I1)
     C                   MOVE      CTTFYR        Y2(I1)
     C                   MOVE      CTFCAS        A1(I1)
     C                   MOVE      CTTCAS        A2(I1)
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
     C                   READ      PCT01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVE      C1(1)         KEY1(1)
     C                   MOVE      S1(1)         KEY5(1)
     C                   MOVE      F1(1)         KEY2(1)
     C                   MOVE      Y1(1)         KEY3(1)
     C                   MOVE      A1(1)         KEY4(1)
     C                   MOVE      C1(14)        KEY1(2)
     C                   MOVE      F1(14)        KEY2(2)
     C                   MOVE      Y1(14)        KEY3(2)
     C                   MOVE      A1(14)        KEY4(2)
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
     C                   READP     PCT01                                  60
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVE      C1(1)         KEY1(1)
     C                   MOVE      S1(1)         KEY5(1)
     C                   MOVE      F1(1)         KEY2(1)
     C                   MOVE      Y1(1)         KEY3(1)
     C                   MOVE      A1(1)         KEY4(1)
     C                   MOVE      C1(14)        KEY1(2)
     C                   MOVE      F1(14)        KEY2(2)
     C                   MOVE      Y1(14)        KEY3(2)
     C                   MOVE      A1(14)        KEY4(2)
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
     C                   MOVE      *ZEROS        XXCO#
     C                   MOVE      *ZEROS        XXSUB#
     C                   MOVE      *ZEROS        XXFND
     C                   MOVE      *ZEROS        XXFYR
     C                   MOVE      *ZEROS        XXCASE
     C     CTKEY1        SETLL     PCT01
     C                   READ      PCT01                                  60
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
     C                   READ      PCT01                                  60
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
