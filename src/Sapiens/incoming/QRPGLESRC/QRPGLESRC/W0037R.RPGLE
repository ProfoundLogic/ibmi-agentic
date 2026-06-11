     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0037R - GROUP PROVIDER ALPHA SEARCH
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  THIS PROGRAM DISPLAYS A LIST OF ALL     *
     H*   GROUP PROVIDERS BY NAME.                                    *
     H* CALLED FROM: W0035R                                           *
     H* CALLS TO:    NONE                                             *
     H* PROGRAMMER:  C:15                                             *
     H* CREATED:     09/06/94       PROJECT: WC361                    *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/17/00 C:15  Project: WC361                                 *
     H*                Chgd display file to allow lower case in the   *
     H*                position to fields.                            *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*     20  -  INVALID ROLL REQUEST                               *
     H*     60  -  END OF FILE (WMNRP)                                *
     H*     61  -  SCREEN IS FILLED                                   *
     H*     65  -  GENERAL USAGE                                      *
     H*****************************************************************
     H* FILE USAGE:                                                   *
     H*     WMUPL (PUP01): Q1USER                                     *
     H*     SMCOL (PCO01): CO#                                        *
     H*     WMNRL2(PNR01): CO#/NAME/FSN/FEIN/MOD                      *
     H*****************************************************************
     FW0037F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMNRL2    IF   E           K DISK

     D FL              S              1    DIM(14)
     D N1              S             40    DIM(14)
     D N2              S             15    DIM(14)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEY1            S             40    DIM(2)
     D KEY2            S             15    DIM(2)
     D KEYF            S              9  0 DIM(2)
     D KEYM            S              3  0 DIM(2)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  FE                     1    126  0
     D                                     DIM(14)
     D  SCFE01                 1      9  0
     D  SCFE02                10     18  0
     D  SCFE03                19     27  0
     D  SCFE04                28     36  0
     D  SCFE05                37     45  0
     D  SCFE06                46     54  0
     D  SCFE07                55     63  0
     D  SCFE08                64     72  0
     D  SCFE09                73     81  0
     D  SCFE10                82     90  0
     D  SCFE11                91     99  0
     D  SCFE12               100    108  0
     D  SCFE13               109    117  0
     D  SCFE14               118    126  0
     D                 DS
     D  MO                     1     42  0
     D                                     DIM(14)
     D  SCMO01                 1      3  0
     D  SCMO02                 4      6  0
     D  SCMO03                 7      9  0
     D  SCMO04                10     12  0
     D  SCMO05                13     15  0
     D  SCMO06                16     18  0
     D  SCMO07                19     21  0
     D  SCMO08                22     24  0
     D  SCMO09                25     27  0
     D  SCMO10                28     30  0
     D  SCMO11                31     33  0
     D  SCMO12                34     36  0
     D  SCMO13                37     39  0
     D  SCMO14                40     42  0
     D                 DS
     D  NM                     1    560
     D                                     DIM(14)
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
     D  GT                     1     14
     D                                     DIM(14)
     D  SCGT01                 1      1
     D  SCGT02                 2      2
     D  SCGT03                 3      3
     D  SCGT04                 4      4
     D  SCGT05                 5      5
     D  SCGT06                 6      6
     D  SCGT07                 7      7
     D  SCGT08                 8      8
     D  SCGT09                 9      9
     D  SCGT10                10     10
     D  SCGT11                11     11
     D  SCGT12                12     12
     D  SCGT13                13     13
     D  SCGT14                14     14
     D                 DS
     D  ST                     1     14
     D                                     DIM(14)
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
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  NETWORK REVIEW FILE
     C*
     C     NRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXLAST
     C                   KFLD                    XXFRST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFEIN            9 0
     C                   PARM                    QQMOD             3 0
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
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    NRNAME        XXLAST
     C     *LIKE         DEFINE    NRFSN         XXFRST
     C     *LIKE         DEFINE    NRFEIN        XXFEIN
     C     *LIKE         DEFINE    NRMOD         XXMOD
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C                   MOVE      *ZERO         UPCO#
     C     UPKY          CHAIN     PUP01                              65
     C                   Z-ADD     UPCO#         XXCO#
     C     COKY          CHAIN     PCO01                              65
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
      /Free
      /If Defined (WEB)
       Clear scUp;
       Clear scDown;
      /Endif
      /End-free
     C                   EXFMT     SCR01
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
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
     C*   INVALID ROLL REQUEST
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
     C*   ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVEL     KEY1(2)       XXLAST
     C                   MOVEL     KEY2(2)       XXFRST
     C                   Z-ADD     KEYF(2)       XXFEIN
     C                   Z-ADD     KEYM(2)       XXMOD
     C     NRKY          CHAIN     PNR01                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVEL     KEY1(1)       XXLAST
     C                   MOVEL     KEY2(1)       XXFRST
     C                   Z-ADD     KEYF(1)       XXFEIN
     C                   Z-ADD     KEYM(1)       XXMOD
     C     NRKY          CHAIN     PNR01                              60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCLAST        IFNE      *BLANKS
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEY1
     C                   MOVE      *BLANK        KEY2
     C                   MOVE      *BLANK        KEYF
     C                   MOVE      *BLANK        KEYM
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVE      *BLANK        XXMOD
     C                   MOVE      *BLANK        XXFEIN
     C                   MOVEL     SCLAST        XXLAST
     C                   MOVE      SCFRST        XXFRST
     C                   MOVE      *BLANKS       SCLAST
     C                   MOVE      *BLANKS       SCFRST
     C     NRKY          SETLL     PNR01
     C                   READ      PNR01                                  60
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
     C                   Z-ADD     FE(I1)        QQFEIN
     C                   Z-ADD     MO(I1)        QQMOD
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     *ZERO         QQFEIN
     C                   Z-ADD     *ZERO         QQMOD
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
     C                   MOVE      *ZEROS        FE
     C                   MOVE      *ZEROS        MO
     C                   MOVE      *BLANKS       NM
     C                   MOVE      *BLANKS       GT
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       N1
     C                   MOVE      *BLANKS       N2
     C                   Z-ADD     *ZERO         I1
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVE      NRFEIN        FE(I1)
     C                   MOVE      NRMOD         MO(I1)
     C     NRFSN         IFEQ      *BLANKS
     C                   MOVEL     NRNAME        NM(I1)
     C                   ELSE
     C                   MOVEL     NRNAME        QQLAST           56
     C                   MOVEL     NRFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        NM(I1)
     C                   END
     C                   MOVEL     NRGRTY        GT(I1)
     C                   MOVEL     NRSTAT        ST(I1)
     C                   MOVEL     NRNAME        N1(I1)
     C                   MOVEL     NRFSN         N2(I1)
     C                   ENDSR
     C*
     C*  LOAD FORWARD SUBROUTINE
     C*
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVER          TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C                   READ      PNR01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     FE(1)         KEYF(1)
     C                   Z-ADD     FE(14)        KEYF(2)
     C                   Z-ADD     MO(1)         KEYM(1)
     C                   Z-ADD     MO(14)        KEYM(2)
     C                   MOVEL     N1(1)         KEY1(1)
     C                   MOVEL     N1(14)        KEY1(2)
     C                   MOVEL     N2(1)         KEY2(1)
     C                   MOVEL     N2(14)        KEY2(2)
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     15            I1
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERP         TAG
     C                   SUB       1             I1
     C                   EXSR      FILL
     C                   READP     PNR01                                  60
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   EXSR      LOADB
     C                   GOTO      DONEP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     FE(1)         KEYF(1)
     C                   Z-ADD     FE(14)        KEYF(2)
     C                   Z-ADD     MO(1)         KEYM(1)
     C                   Z-ADD     MO(14)        KEYM(2)
     C                   MOVEL     N1(1)         KEY1(1)
     C                   MOVEL     N1(14)        KEY1(2)
     C                   MOVEL     N2(1)         KEY2(1)
     C                   MOVEL     N2(14)        KEY2(2)
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
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   Z-ADD     *ZERO         XXFEIN
     C                   Z-ADD     *ZERO         XXMOD
     C     NRKY          SETLL     PNR01
     C                   READ      PNR01                                  60
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
     C                   READ      PNR01                                  60
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
