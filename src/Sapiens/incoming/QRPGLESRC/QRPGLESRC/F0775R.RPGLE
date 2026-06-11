     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr**** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: F0775R                                            *
     H*    PROGRAM DESCRIPTION: CASE/LETTER# SEARCH - SCRN PGM        *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*  07/22/92 KRR      PROJ: 99AAC                                *
     H*                    ADDED SUB# TO ALL PROCESSING               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*****************************************************************
     FF0775F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FFALTL1    IF   E           K DISK
     FSMUPL     IF   E           K DISK

     D*
     D FL              S              1    DIM(14)                              POSITION FLAG
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEY1            S              3  0 DIM(2)                               FUND  KEY
     D KEY2            S              3  0 DIM(2)                               YEAR KEY
     D KEY3            S              7  0 DIM(2)                               CASE KEY
     D KEY4            S              9  0 DIM(2)                               LTR# KEY
     D KEY5            S              5  0 DIM(2)                               SUB#
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  GR                     1     42  0
     D                                     DIM(14)                              FUND
     D  SCGR01                 1      3  0
     D  SCGR02                 4      6  0
     D  SCGR03                 7      9  0
     D  SCGR04                10     12  0
     D  SCGR05                13     15  0
     D  SCGR06                16     18  0
     D  SCGR07                19     21  0
     D  SCGR08                22     24  0
     D  SCGR09                25     27  0
     D  SCGR10                28     30  0
     D  SCGR11                31     33  0
     D  SCGR12                34     36  0
     D  SCGR13                37     39  0
     D  SCGR14                40     42  0
     D                 DS
     D  YR                     1     42  0
     D                                     DIM(14)                              YEAR
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
     D                 DS
     D  CS                     1     98  0
     D                                     DIM(14)                              CASE
     D  SCCS01                 1      7  0
     D  SCCS02                 8     14  0
     D  SCCS03                15     21  0
     D  SCCS04                22     28  0
     D  SCCS05                29     35  0
     D  SCCS06                36     42  0
     D  SCCS07                43     49  0
     D  SCCS08                50     56  0
     D  SCCS09                57     63  0
     D  SCCS10                64     70  0
     D  SCCS11                71     77  0
     D  SCCS12                78     84  0
     D  SCCS13                85     91  0
     D  SCCS14                92     98  0
     D                 DS
     D  SB                     1     70  0
     D                                     DIM(14)                              SUB#
     D  SCSB01                 1      5  0
     D  SCSB02                 6     10  0
     D  SCSB03                11     15  0
     D  SCSB04                16     20  0
     D  SCSB05                21     25  0
     D  SCSB06                26     30  0
     D  SCSB07                31     35  0
     D  SCSB08                36     40  0
     D  SCSB09                41     45  0
     D  SCSB10                46     50  0
     D  SCSB11                51     55  0
     D  SCSB12                56     60  0
     D  SCSB13                61     65  0
     D  SCSB14                66     70  0
     D                 DS
     D  LT                     1    126  0
     D                                     DIM(14)                              LETTER #
     D  SCLT01                 1      9  0
     D  SCLT02                10     18  0
     D  SCLT03                19     27  0
     D  SCLT04                28     36  0
     D  SCLT05                37     45  0
     D  SCLT06                46     54  0
     D  SCLT07                55     63  0
     D  SCLT08                64     72  0
     D  SCLT09                73     81  0
     D  SCLT10                82     90  0
     D  SCLT11                91     99  0
     D  SCLT12               100    108  0
     D  SCLT13               109    117  0
     D  SCLT14               118    126  0
     D                 DS
     D  PR                     1    126  0
     D                                     DIM(14)                              PROVIDER
     D  SCPR01                 1      9  0
     D  SCPR02                10     18  0
     D  SCPR03                19     27  0
     D  SCPR04                28     36  0
     D  SCPR05                37     45  0
     D  SCPR06                46     54  0
     D  SCPR07                55     63  0
     D  SCPR08                64     72  0
     D  SCPR09                73     81  0
     D  SCPR10                82     90  0
     D  SCPR11                91     99  0
     D  SCPR12               100    108  0
     D  SCPR13               109    117  0
     D  SCPR14               118    126  0
     D                 DS
     D  MD                     1     42  0
     D                                     DIM(14)                              MODIFIER
     D  SCMD01                 1      3  0
     D  SCMD02                 4      6  0
     D  SCMD03                 7      9  0
     D  SCMD04                10     12  0
     D  SCMD05                13     15  0
     D  SCMD06                16     18  0
     D  SCMD07                19     21  0
     D  SCMD08                22     24  0
     D  SCMD09                25     27  0
     D  SCMD10                28     30  0
     D  SCMD11                31     33  0
     D  SCMD12                34     36  0
     D  SCMD13                37     39  0
     D  SCMD14                40     42  0
     D                 DS
     D  PD                     1    112  0
     D                                     DIM(14)                              POST DATE
     D  SCPD01               001    008  0
     D  SCPD02               009    016  0
     D  SCPD03               017    024  0
     D  SCPD04               025    032  0
     D  SCPD05               033    040  0
     D  SCPD06               041    048  0
     D  SCPD07               049    056  0
     D  SCPD08               057    064  0
     D  SCPD09               065    072  0
     D  SCPD10               073    080  0
     D  SCPD11               081    088  0
     D  SCPD12               089    096  0
     D  SCPD13               097    104  0
     D  SCPD14               105    112  0
     D                 DS
     D  AM                     1    126  2
     D                                     DIM(14)                              CHARGES
     D  SCAM01                 1      9  2
     D  SCAM02                10     18  2
     D  SCAM03                19     27  2
     D  SCAM04                28     36  2
     D  SCAM05                37     45  2
     D  SCAM06                46     54  2
     D  SCAM07                55     63  2
     D  SCAM08                64     72  2
     D  SCAM09                73     81  2
     D  SCAM10                82     90  2
     D  SCAM11                91     99  2
     D  SCAM12               100    108  2
     D  SCAM13               109    117  2
     D  SCAM14               118    126  2
     D                 DS
     D  RQ                     1     14
     D                                     DIM(14)                              REQUEST #     UE
     D  SCRQ01                 1      1
     D  SCRQ02                 2      2
     D  SCRQ03                 3      3
     D  SCRQ04                 4      4
     D  SCRQ05                 5      5
     D  SCRQ06                 6      6
     D  SCRQ07                 7      7
     D  SCRQ08                 8      8
     D  SCRQ09                 9      9
     D  SCRQ10                10     10
     D  SCRQ11                11     11
     D  SCRQ12                12     12
     D  SCRQ13                13     13
     D  SCRQ14                14     14
     D                 DS
     D  ST                     1     14
     D                                     DIM(14)                              STATUS        UE
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
     C*   KEY LIST FOR THE REQUEST LETTER FILE
     C*
     C     LTKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXLTR#
     C*
     C*   KEY LIST FOR THE A/R FILE
     C*
     C     LTKEY2        KLIST
     C                   KFLD                    UPCO#
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
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQLTR#            9
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      '-'           XXHYP1            1
     C                   MOVE      '-'           XXHYP2            1
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   MOVE      QQLTR#        XXLTR#            9 0
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
     C                   Z-ADD     *ZERO         GR
     C                   Z-ADD     *ZERO         SB
     C                   Z-ADD     *ZERO         YR
     C                   Z-ADD     *ZERO         CS
     C                   Z-ADD     *ZERO         LT
     C                   Z-ADD     *ZERO         PR
     C                   Z-ADD     *ZERO         MD
     C                   Z-ADD     *ZERO         PD
     C                   Z-ADD     *ZERO         AM
     C                   MOVE      *BLANKS       RQ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      QQSUB#        SCSUB#
     C                   MOVE      QQFND         SCFND
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQCASE        SCCASE
     C                   MOVE      QQLTR#        SCLTR#
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
     C*****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*********************************
     C*   COMMAND 03 = END OF JOB     *
     C*********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVEL     *BLANK        QQLTR#
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    ENDIF *INKC
     C*********************************
     C*   COMMAND 12 = PRIOR SCREEN   *
     C*********************************
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVEL     *BLANK        QQLTR#
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    ENDIF *INKL
     C**************************************
     C*   COMMAND 06 = PENDED CHARGES      *
     C**************************************
     C     *INKF         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      Q2RECF        XXRECF           10
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
     C*
     C                   Z-ADD     SB(I1)        XXSUB#
     C                   Z-ADD     GR(I1)        XXFND
     C                   Z-ADD     YR(I1)        XXFYR
     C                   Z-ADD     CS(I1)        XXCASE
     C                   Z-ADD     LT(I1)        XXLTR#
     C     LTKEY         CHAIN     PLT01                              49
     C     *IN49         IFEQ      '0'
     C*
     C                   Z-ADD     LTFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQFRDT            8
     C*
     C                   ELSE
     C                   MOVE      *ZEROS        QQFRDT
     C                   END
     C*
     C                   MOVE      SB(I1)        QQSUB#            5
     C                   MOVE      GR(I1)        QQFND             3
     C                   MOVE      YR(I1)        QQFYR             3
     C                   MOVE      CS(I1)        QQCASE            7
     C                   MOVE      PR(I1)        QQFEIN            9
     C                   MOVE      MD(I1)        QQMOD             3
     C                   MOVE      LT(I1)        QQLTR#            9
     C                   CALL      'F0760R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM                    QQLTR#
     C                   PARM                    QQFRDT
     C                   PARM                    QQRTCD
     C*
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C                   END                                                    ENDIF GE/LE
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDIF SCR01
     C                   END                                                    ENDIF *INKF
     C*********************************
     C*   ROLL UP REQUEST             *
     C*********************************
     C     *IN18         CABEQ     '1'           SCR01T
     C*********************************
     C*   ROLL DOWN REQUEST           *
     C*********************************
     C     *IN19         CABEQ     '1'           SCR01T
     C*********************************
     C*   PROCESS HELP REQUESTS       *
     C*********************************
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
     C                   GOTO      STRT
     C                   END                                                    ENDIF *IN10
     C*
     C                   END                                                    END CMDKEY
     C*****************************************************************
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
     C                   MOVE      KEY5(2)       XXSUB#
     C                   MOVE      KEY1(2)       XXFND
     C                   MOVE      KEY2(2)       XXFYR
     C                   MOVE      KEY3(2)       XXCASE
     C                   MOVE      KEY4(2)       XXLTR#
     C     LTKEY         CHAIN     PLT01                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      KEY5(1)       XXSUB#
     C                   MOVE      KEY1(1)       XXFND
     C                   MOVE      KEY2(1)       XXFYR
     C                   MOVE      KEY3(1)       XXCASE
     C                   MOVE      KEY4(1)       XXLTR#
     C     LTKEY         CHAIN     PLT01                              60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCFND         IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *ZERO         KEY1
     C                   MOVE      *ZERO         KEY2
     C                   MOVE      *ZERO         KEY3
     C                   MOVE      *ZERO         KEY4
     C                   MOVE      *ZERO         KEY5
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   MOVE      SCSUB#        XXSUB#
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SCFYR         XXFYR
     C                   MOVE      SCCASE        XXCASE
     C                   MOVE      SCLTR#        XXLTR#
     C                   MOVE      *ZERO         SCSUB#
     C                   MOVE      *ZERO         SCFND
     C                   MOVE      *ZERO         SCFYR
     C                   MOVE      *ZERO         SCCASE
     C                   MOVE      *ZERO         SCLTR#
     C     LTKEY         SETLL     PLT01
     C     LTKEY2        READE     PLT01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
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
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      SB(I1)        QQSUB#
     C                   MOVE      GR(I1)        QQFND
     C                   MOVE      YR(I1)        QQFYR
     C                   MOVE      CS(I1)        QQCASE
     C                   MOVE      LT(I1)        QQLTR#
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   MOVEL     *BLANK        QQLTR#
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C*  CLEAR SCREEN ARRAYS - SUBROUTINE (CLR)
     C*****************************************************************
     C     CLR           BEGSR
     C                   Z-ADD     *ZERO         SB
     C                   Z-ADD     *ZERO         GR
     C                   Z-ADD     *ZERO         YR
     C                   Z-ADD     *ZERO         CS
     C                   Z-ADD     *ZERO         LT
     C                   Z-ADD     *ZERO         PR
     C                   Z-ADD     *ZERO         MD
     C                   Z-ADD     *ZERO         PD
     C                   Z-ADD     *ZERO         AM
     C                   MOVE      *BLANKS       RQ
     C                   MOVE      *BLANKS       ST
     C                   Z-ADD     *ZERO         I1
     C                   Z-ADD     *ZERO         XHSUB#
     C                   Z-ADD     *ZERO         XHFND
     C                   Z-ADD     *ZERO         XHFYR
     C                   Z-ADD     *ZERO         XHCASE
     C                   Z-ADD     *ZERO         XHLTR#
     C                   ENDSR
     C*****************************************************************
     C*  FILL SCREEN ARRAYS - SUBROUTINE (FILL)
     C*****************************************************************
     C     FILL          BEGSR
     C                   MOVE      LTSUB#        SB(I1)
     C                   MOVE      LTFND         GR(I1)
     C                   MOVE      LTFYR         YR(I1)
     C                   MOVE      LTCASE        CS(I1)
     C                   MOVE      LTLTR#        LT(I1)
     C                   MOVE      LTFEIN        PR(I1)
     C                   MOVE      LTMOD         MD(I1)
     C*
     C                   Z-ADD     LTPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PD(I1)
     C*
     C                   MOVE      LTAMT         AM(I1)
     C                   MOVE      LTFLG1        RQ(I1)
     C                   MOVE      LTSTAT        ST(I1)
     C                   ENDSR
     C*****************************************************************
     C*  LOAD FORWARD - SUBROUTINE (LOAD)
     C*****************************************************************
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
     C     LTSUB#        IFNE      XHSUB#
     C     LTFND         ORNE      XHFND
     C     LTFYR         ORNE      XHFYR
     C     LTCASE        ORNE      XHCASE
     C     LTLTR#        ORNE      XHLTR#
     C                   ADD       1             I1
     C     I1            CABGT     14            NEWT
     C                   MOVE      LTSUB#        XHSUB#            5 0
     C                   MOVE      LTFND         XHFND             3 0
     C                   MOVE      LTFYR         XHFYR             3 0
     C                   MOVE      LTCASE        XHCASE            7 0
     C                   MOVE      LTLTR#        XHLTR#            5 0
     C                   END
     C*
     C     I1            IFEQ      0
     C                   Z-ADD     1             I1
     C                   END
     C*
     C                   EXSR      FILL
     C     LTKEY2        READE     PLT01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     NEWT          TAG
     C     I1            COMP      14                                   6161
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   MOVE      SB(1)         KEY5(1)
     C                   MOVE      GR(1)         KEY1(1)
     C                   MOVE      YR(1)         KEY2(1)
     C                   MOVE      CS(1)         KEY3(1)
     C                   MOVE      LT(1)         KEY4(1)
     C                   MOVE      SB(14)        KEY5(2)
     C                   MOVE      GR(14)        KEY1(2)
     C                   MOVE      YR(14)        KEY2(2)
     C                   MOVE      CS(14)        KEY3(2)
     C                   MOVE      LT(14)        KEY4(2)
     C                   ENDSR
     C*****************************************************************
     C*  LOAD BACKWARD - SUBROUTINE (LOADP)
     C*****************************************************************
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
     C     LTSUB#        IFNE      XHSUB#
     C     LTFND         ORNE      XHFND
     C     LTFYR         ORNE      XHFYR
     C     LTCASE        ORNE      XHCASE
     C     LTLTR#        ORNE      XHLTR#
     C                   SUB       1             I1
     C     I1            CABLT     1             NEWT2
     C                   MOVE      LTSUB#        XHSUB#
     C                   MOVE      LTFND         XHFND
     C                   MOVE      LTFYR         XHFYR
     C                   MOVE      LTCASE        XHCASE
     C                   MOVE      LTLTR#        XHLTR#
     C                   END
     C*
     C                   EXSR      FILL
     C                   READP     PLT01                                  60
     C  N60LTCO#         COMP      UPCO#                              6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     NEWT2         TAG
     C     I1            COMP      1                                  61  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   MOVE      SB(1)         KEY5(1)
     C                   MOVE      GR(1)         KEY1(1)
     C                   MOVE      YR(1)         KEY2(1)
     C                   MOVE      CS(1)         KEY3(1)
     C                   MOVE      LT(1)         KEY4(1)
     C                   MOVE      SB(14)        KEY5(2)
     C                   MOVE      GR(14)        KEY1(2)
     C                   MOVE      YR(14)        KEY2(2)
     C                   MOVE      CS(14)        KEY3(2)
     C                   MOVE      LT(14)        KEY4(2)
     C                   ENDSR
     C*****************************************************************
     C*  LOAD FROM BEGINNING OF FILE - SUBROUTINE (LOADB)
     C*****************************************************************
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *ZEROS        XXSUB#
     C                   MOVE      *ZEROS        XXFND
     C                   MOVE      *ZEROS        XXFYR
     C                   MOVE      *ZEROS        XXCASE
     C                   MOVE      *ZEROS        XXLTR#
     C     LTKEY         SETLL     PLT01
     C     LTKEY2        READE     PLT01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C*
     C     LTSUB#        IFNE      XHSUB#
     C     LTFND         ORNE      XHFND
     C     LTFYR         ORNE      XHFYR
     C     LTCASE        ORNE      XHCASE
     C     LTLTR#        ORNE      XHLTR#
     C                   ADD       1             I1
     C     I1            CABGT     14            NEWT1
     C                   MOVE      LTSUB#        XHSUB#
     C                   MOVE      LTFND         XHFND
     C                   MOVE      LTFYR         XHFYR
     C                   MOVE      LTCASE        XHCASE
     C                   MOVE      LTLTR#        XHLTR#
     C                   END
     C*
     C                   EXSR      FILL
     C     LTKEY2        READE     PLT01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     NEWT1         TAG
     C     I1            COMP      14                                   6161
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
      *========================================================================*
**
ROLL UP
ROLL DOWN
