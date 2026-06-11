     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE API101R - VOUCHER MASTER MAINTENANCE
     H******************************************************************
     H****** REVISED-CRIMS MOD-5/13/92
     H* ADDED CHECK DESCRIPTION FIELD TO SCREEN AND FILE
     H******************************************************************
     H****** 3/25/93 ** C:22 ******************************************
     H* ADDED A MOVE STATEMENT TO CLEAR THE CHECK DESRIPTION          *
     H* FIELD ON THE SCREEN AFTER VOUCHER DATA IS STORED              *
     H*****************************************************************
     FAPI101F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FAPADL1    UF A E           K DISK
     FAPGDL1    UF A E           K DISK
     FBKCFL1    IF   E           K DISK
     FAPVML1    UF   E           K DISK
     FAPAPL1    UF   E           K DISK
     FBKCAL1    IF   E           K DISK
     FAPBKL1    IF   E           K DISK
     FAPCSL1    IF   E           K DISK
      *** BEG ADD * AP018 * 7/12/97
     FAPADL3    IF   E           K DISK
     F                                     RENAME(PAD01:PAD03)
     FAPCHL4    IF   E           K DISK
     F                                     RENAME(PCH01:PCH04)
      *** MAY OR MAY NOT USE THIS FILE
     F**BKDTL1  IF  E           K        DISK
      *** END ADD * AP018 * 7/12/97
     FBKSYL1    IF   E           K DISK
     FAPCHL1    IF   E           K DISK
     FBKTBL     IF   E           K DISK

     D*
0076 D*
     D UA              S             10    DIM(3) CTDATA PERRCD(1)
0073 D*                         *************************************
0074 D*                         * NORMAL/LEAP YEAR DAY COUNTS       *
0075 D*                         *************************************
0076 D*
0077 D NY              S              3  0 DIM(12) CTDATA PERRCD(12) ASCEND
0078 D LY              S              3  0 DIM(12) CTDATA PERRCD(12) ASCEND
0070 D AUW             S              1    DIM(40)                                WORDS
0071 D TABM#           S              2  0 DIM(12) CTDATA PERRCD(6)               MON
0071 D TABMON          S              3    DIM(12) ALT(TABM#)
0072 D TABD#           S              1  0 DIM(7) CTDATA PERRCD(7)                DAYS
0072 D TABDAY          S              3    DIM(7) ALT(TABD#)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  C                      1     27  0
     D                                     DIM(9)
     D  SCCO1                  1      3  0
     D  SCCO2                  4      6  0
     D  SCCO3                  7      9  0
     D  SCCO4                 10     12  0
     D  SCCO5                 13     15  0
     D  SCCO6                 16     18  0
     D  SCCO7                 19     21  0
     D  SCCO8                 22     24  0
     D  SCCO9                 25     27  0
     D                 DS
     D  F                      1     27  0
     D                                     DIM(9)
     D  SCFAC1                 1      3  0
     D  SCFAC2                 4      6  0
     D  SCFAC3                 7      9  0
     D  SCFAC4                10     12  0
     D  SCFAC5                13     15  0
     D  SCFAC6                16     18  0
     D  SCFAC7                19     21  0
     D  SCFAC8                22     24  0
     D  SCFAC9                25     27  0
     D                 DS
     D  D                      1     27  0
     D                                     DIM(9)
     D  SCDPT1                 1      3  0
     D  SCDPT2                 4      6  0
     D  SCDPT3                 7      9  0
     D  SCDPT4                10     12  0
     D  SCDPT5                13     15  0
     D  SCDPT6                16     18  0
     D  SCDPT7                19     21  0
     D  SCDPT8                22     24  0
     D  SCDPT9                25     27  0
     D                 DS
     D  A                      1     27  0
     D                                     DIM(9)
     D  SCACC1                 1      3  0
     D  SCACC2                 4      6  0
     D  SCACC3                 7      9  0
     D  SCACC4                10     12  0
     D  SCACC5                13     15  0
     D  SCACC6                16     18  0
     D  SCACC7                19     21  0
     D  SCACC8                22     24  0
     D  SCACC9                25     27  0
     D                 DS
     D  S                      1     27  0
     D                                     DIM(9)
     D  SCSUB1                 1      3  0
     D  SCSUB2                 4      6  0
     D  SCSUB3                 7      9  0
     D  SCSUB4                10     12  0
     D  SCSUB5                13     15  0
     D  SCSUB6                16     18  0
     D  SCSUB7                19     21  0
     D  SCSUB8                22     24  0
     D  SCSUB9                25     27  0
     D                 DS
     D  AM                     1     99  2
     D                                     DIM(9)
     D  SCAM01                 1     11  2
     D  SCAM02                12     22  2
     D  SCAM03                23     33  2
     D  SCAM04                34     44  2
     D  SCAM05                45     55  2
     D  SCAM06                56     66  2
     D  SCAM07                67     77  2
     D  SCAM08                78     88  2
     D  SCAM09                89     99  2
     D                 DS
     D  DE                     1    270
     D                                     DIM(9)
     D  SCDE01                 1     30
     D  SCDE02                31     60
     D  SCDE03                61     90
     D  SCDE04                91    120
     D  SCDE05               121    150
     D  SCDE06               151    180
     D  SCDE07               181    210
     D  SCDE08               211    240
     D  SCDE09               241    270
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D CMDACT          DS
     D  ACT                    1    200
     D QLFDAP          DS
     D  SSPGM                  1     10
     D  SSLIB                 11     20
     D HELPCT          DS
     D  HLP                    1     60

     C*
     C*
     C     *LIKE         DEFINE    SCSEQ1        SCSEQ2
     C     *LIKE         DEFINE    SCCO1         X@CO#
     C     *LIKE         DEFINE    SCFAC1        X@FAC#
     C     *LIKE         DEFINE    SCDPT1        X@DPT#
     C     *LIKE         DEFINE    SCACC1        X@ACC#
     C     *LIKE         DEFINE    SCSUB1        X@SUB#
     C     *LIKE         DEFINE    ADAMT         XXAMT
     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
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
     C*   PARM LIST FOR WINDOWS RETRIEVE PARMS.
     C*
     C     RTVPRM        PLIST
     C                   PARM      QLFDAP        PGMLIB           20
     C     CMDACT        PARM                    CMDACT
     C     BARDTA        PARM                    BARDTA           79
     C     INFOA1        PARM                    INFOA1           78
     C     INFOA2        PARM                    INFOA2           78
     C     HELPCT        PARM                    HELPCT
     C                   PARM      'A'           SSTYPE            1
     C     SSFLG1        PARM                    SSFLG1            1
     C     SSFLG2        PARM                    SSFLG2            1
     C     SSFLG3        PARM                    SSFLG3            1
     C     SSFLG4        PARM                    SSFLG4            1
     C     SSFLG5        PARM                    SSFLG5            1
     C                   PARM                    BARDSC           50
     C*
     C*   PARM LIST FOR WINDOWS CALL.
     C*
     C     EXCPRM        PLIST
     C                   PARM      QLFDAP        PGMLIB
     C                   PARM      'A'           SSTYPE            1
     C     ACTION        PARM      ACTION        ACTION            8
     C                   PARM      HELPCT        HELPCT
     C                   PARM      CMDACT        CMDACT
     C                   PARM      SSFLG1        SSFLG1            1
     C                   PARM      SSFLG2        SSFLG2            1
     C                   PARM      SSFLG3        SSFLG3            1
     C                   PARM      SSFLG4        SSFLG4            1
     C                   PARM      SSFLG5        SSFLG5            1
     C     SSEXT         PARM      ' '           SSEXT             1
     C     SSCNL         PARM      ' '           SSCNL             1
     C     SSRTN         PARM      ' '           SSRTN             1
     C*
     C*   KEY LIST FOR THE VOUCHER LIST
     C*
     C     ADKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCVDR
     C                   KFLD                    SCSEQ1
     C*
     C*   KEY LIST FOR THE VOUCHER SELECTION FILE.
     C*
     C     CSKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCVDR
     C                   KFLD                    SCINV#
     C                   KFLD                    SCSEQ1
     C*
     C*   KEY LIST FOR THE VOUCHER G/L DETAIL.
     C*
     C     GDKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCVDR
     C                   KFLD                    SCSEQ1
     C                   KFLD                    SCSEQ2
     C*
     C*   KEY LIST FOR THE COMPANY/FACILITY MASTER FILE.
     C*
     C     CFKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C*
     C*   KEY LIST FOR THE CHART OF ACCOUNTS
     C*
     C     GLKEY         KLIST
     C                   KFLD                    X@CO#
     C                   KFLD                    X@FAC#
     C                   KFLD                    X@DPT#
     C                   KFLD                    X@ACC#
     C                   KFLD                    X@SUB#
     C*
     C     TBKEY         KLIST
     C                   KFLD                    TCTBL
     C                   KFLD                    TCVAL
      *** BEG ADD * AP018 * 7/12/97
     C*
     C*   KEY LIST FOR OPEN & PAID PAYABLES FILES.
     C*
     C     APKEY         KLIST
     C                   KFLD                    SCVDR
     C                   KFLD                    SCINV#
     C                   KFLD                    SCSEQ1
     C*
     C     APKEY2        KLIST
     C                   KFLD                    SCVDR
     C                   KFLD                    SCINV#
     C*
      *** BEG ADD * AP018 * 7/12/97
     C                   TIME                    DSDTTM
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   GET THE DEFAULT COMPANY AND FACILITY
     C*
     C     'AP'          CHAIN     PAP01                              60
     C     *IN60         IFEQ      '0'
     C     APCO#         IFGT      0
     C                   Z-ADD     APCO#         DFTCO             3 0
     C                   END
     C     APFAC#        IFGT      0
     C                   Z-ADD     APFAC#        DFTFAC            3 0
     C                   END
     C                   EXCEPT    PAP01E
     C                   END                                                    END *IN60
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN34
     C                   MOVE      'AP'          APPKEY            2
     C     DFTCO         IFGT      0
     C                   Z-ADD     DFTCO         SCCO#
     C                   ELSE
     C                   MOVE      '001'         SCCO#
     C                   END
     C     DFTFAC        IFGT      0
     C                   Z-ADD     DFTFAC        SCFAC#
     C                   ELSE
     C                   MOVE      '001'         SCFAC#
     C                   END
     C                   Z-ADD     *ZERO         C
     C                   Z-ADD     *ZERO         F
     C                   Z-ADD     *ZERO         D
     C                   Z-ADD     *ZERO         A
     C                   Z-ADD     *ZERO         S
     C                   Z-ADD     *ZERO         AM
      *** BEG ADD * 7/10/97 * AP007
     C                   CLEAR                   SCTDST
      *** END ADD * 7/10/97 * AP007
     C                   MOVE      *BLANK        DE
     C     'BK'          CHAIN     PSY01                              54
     C     *IN54         IFEQ      '0'
     C                   MOVE      'KLBEFCB '    SSPGM
     C                   MOVEL     '*LIBL'       SSLIB
     C                   MOVEL     'ACTION'      ACTBAR           10
     C                   MOVE      'BAR '        ACTBAR
     C                   CALL      'QAB155'      RTVPRM
     C                   MOVE      *BLANKS       ACTION
     C                   MOVEL     SYPOP4        ACTION
     C                   END
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C                   MOVEL     Q1USER        SCUSER
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR29'
     C                   XFOOT     AM            SCTDST
     C                   EXFMT     SCR29
      *** BEG ADD * 7/10/97 * AP007
     C                   MOVE      *OFF          *IN29
     C                   XFOOT     AM            SCTDST
     C     *INKB         IFEQ      *ON
     C     SCTDST        IFNE      SCAMT
     C                   MOVE      *ON           *IN29
     C                   END
     C                   GOTO      STRT
     C                   END
      *** END ADD * 5/10/97 * AP007
     C                   END
     C*                                                                *****
     C*   REMOVE TEMPORARY VENDOR IF NOT UPDATED
     C*
     C   65*INKC         IFEQ      '1'
     C     *INKL         OREQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C     ADKEY         CHAIN     PAD01                              60
     C  N60              EXCEPT    PAD01E
     C     *IN60         IFEQ      '1'
     C     SCVDR         CHAIN     PVM01                              60
     C  N60              DELETE    PVM01
     C                   END
     C                   END
     C                   END
     C*                                                                *****
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C********************************************
     C*   F3 = EXIT                              *
     C********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C********************************************
     C*   F4 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKD         IFEQ      '1'
     C*********
     C* SCR01 *
     C*********
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'APD101R'
     C                   PARM                    SCCODE
     C                   PARM                    SCCO#
     C                   PARM                    SCFAC#
     C                   PARM                    SCVDR
     C                   PARM                    SCSEQ1
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      SCR01A
     C*
     C                   END                                                    END SCR01
     C*
     C                   END                                                    END *INKD
     C********************************************
     C*   F5 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKE         IFEQ      '1'
     C*********
     C* SCR01 *
     C*********
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      '1'           X@CODE
     C                   CALL      'APD010R'
     C                   PARM                    SCVDR
     C                   PARM                    X@CODE            1
     C                   PARM      '1'           QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      '1'           *IN35
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END                                                    END SCR01
     C*
     C                   END                                                    END *INKE
     C********************************************
     C*   F6 = OPERATIONS INQUIRY                *
     C********************************************
     C     *INKF         IFEQ      '1'
     C     SYPOP4        ANDNE     *BLANK
     C     SYINS4        ANDEQ     'Y'
     C                   MOVE      *BLANK        ACTION
     C                   MOVEL     SYPOP4        ACTION
     C                   CALL      'QAB152'      EXCPRM
     C                   GOTO      STRT
     C                   END                                                    END *INKF
     C********************************************
     C*   F7 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKG         IFEQ      '1'
     C                   MOVE      ' '           QQTYPE
     C                   MOVE      '4'           X@TYPE            1
     C*********
     C* SCR29 *
     C*********
     C     Q2RECF        IFEQ      'SCR29'
     C                   MOVE      '4'           X@TYPE
     C                   MOVE      SCCO#         X@CO#
     C                   MOVE      *ZEROS        X@FAC#
     C                   MOVE      *ZEROS        X@DPT#
     C                   MOVE      *ZEROS        X@ACC#
     C                   MOVE      *ZEROS        X@SUB#
     C***                  CALL 'BKD010R'
     C***                  PARM           X@TYPE
     C                   CALL      'BKD011R'
     C                   PARM                    X@CO#
     C                   PARM                    X@FAC#
     C                   PARM                    X@DPT#
     C                   PARM                    X@ACC#
     C                   PARM                    X@SUB#
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQRTC             1
     C     QQRTC         IFEQ      'A'
     C     *IN65         IFEQ      '1'
     C     ADKEY         CHAIN     PAD01                              60
     C  N60              EXCEPT    PAD01E
     C     *IN60         IFEQ      '1'
     C     SCVDR         CHAIN     PVM01                              60
     C  N60              DELETE    PVM01
     C                   END
     C                   END
     C*
     C                   GOTO      TERM
     C                   END
     C*
     C     QQRTC         IFEQ      'B'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR29'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     1             DO        9             X
     C     F(X)          IFEQ      *ZERO
     C     AM(X)         OREQ      *ZERO
     C******               MOVE X@CO#     C,X
     C                   MOVE      SCCO#         C(X)
     C                   MOVE      X@FAC#        F(X)
     C                   MOVE      X@DPT#        D(X)
     C                   MOVE      X@ACC#        A(X)
     C                   MOVE      X@SUB#        S(X)
     C     X             ADD       70            X7                3 0
     C                   MOVE      '1'           *IN(X7)
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR29'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END                                                    END SCR02
     C*
     C                   END                                                    END *INKG
     C********************************************
     C*   F9 = COMPANY FACILITY INQUIRY          *
     C********************************************
     C     *INKI         IFEQ      '1'
     C*********
     C* SCR01 *
     C*********
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      '1'           QQRTC
     C                   CALL      'BKD001R'
     C                   PARM                    SCCO#
     C                   PARM                    SCFAC#
     C                   PARM                    QQRTC             1
     C     QQRTC         IFEQ      'A'
     C     *IN65         IFEQ      '1'
     C     ADKEY         CHAIN     PAD01                              60
     C  N60              EXCEPT    PAD01E
     C     *IN60         IFEQ      '1'
     C     SCVDR         CHAIN     PVM01                              60
     C  N60              DELETE    PVM01
     C                   END
     C                   END
     C*
     C                   GOTO      TERM
     C                   END
     C*
     C                   WRITE     SCR99
     C                   MOVE      '1'           *IN34
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END                                                    END SCR02
     C                   END                                                    END *INKF
     C********************************************
     C*   F9 = DISTRIBUTION ENTRY                *
     C********************************************
     C     *INKI         IFEQ      '1'
     C*********
     C* SCR02 *
     C*********
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR29'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END SCR02
     C                   END                                                    END *INKI
     C********************************************
     C*   F10= AUTO ASSIGN TEMP VENDOR NUMBER    *
     C********************************************
     C     *INKJ         IFEQ      '1'
     C  N65Q2RECF        IFEQ      'SCR02'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C     TEMPTG        TAG
     C     Q2RECF        IFEQ      'SCR01'
     C     'AP'          CHAIN     PAP01                              60
     C     *IN60         IFEQ      '0'
     C     ADDVDR        TAG
     C                   ADD       1             APTVDR
     C                   MOVE      APTVDR        SCVDR
     C                   MOVEL     '999'         SCVDR
     C     SCVDR         CHAIN     PVM01                              61
     C  N61              GOTO      ADDVDR
     C                   UPDATE    PAP01
     C                   END                                                    END *IN60
     C                   END                                                    END SCR01
     C*
     C                   MOVE      '1'           QQRTC
     C                   CALL      'API102R'
     C                   PARM                    QQRTC
     C                   PARM                    SCCO#
     C                   PARM                    SCVDR
     C     QQRTC         IFEQ      'A'
     C     *IN65         IFEQ      '1'
     C     ADKEY         CHAIN     PAD01                              60
     C  N60              EXCEPT    PAD01E
     C     *IN60         IFEQ      '1'
     C     SCVDR         CHAIN     PVM01                              60
     C  N60              DELETE    PVM01
     C                   END
     C                   END
     C*
     C                   GOTO      TERM
     C                   END
     C*
     C     QQRTC         IFEQ      'B'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01   '    Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ELSE
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      SCR01T
     C                   END
     C*
     C                   END                                                    END *INKJ
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C   65              GOTO      TEMPTG
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END                                                    END SCR02
     C     Q2RECF        IFEQ      'SCR29'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END                                                    END SCR02
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C**
     C                   END                                                    END CMDKEYS
     C**                                                               ********
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR29'       SCR29T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C                   MOVE      *ZERO         SCSEQ1
     C     SCR01A        TAG
     C                   MOVE      '1'           SCCODE            1
     C*
     C*  GET THE REQUESTED COMPANY/FACILITY RECORD.
     C*
     C     CFKEY         CHAIN     PCF01                              40
     C     *IN40         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEL     CFNAME        SCCFNM
     C*
     C*  NO TEMP VENDOR UNLESS CHANGE
     C*
     C                   MOVEL     SCVDR         TEMP              3 0
     C  NKD
     CANNKJTEMP          IFEQ      999
     C                   SETON                                        42
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED VENDOR RECORD.
     C*
     C     SCVDR         CHAIN     PVM01                              41
     C     *IN41         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C                   EXCEPT    VMREL
     C                   MOVEL     VMNAME        SCNAME
     C                   MOVEL     VMTERM        STERMS
     C                   MOVE      *BLANK        STDESC
     C                   MOVE      'TC'          TCTBL
     C                   MOVE      *BLANKS       TCVAL
     C                   MOVEL     VMTERM        TCVAL
     C     TBKEY         CHAIN     PTC01                              60
     C  N60              MOVEL     TCDESC        STDESC
     C                   MOVE      *BLANK        SCWIDT
     C                   MOVE      *BLANK        SCWDDT
     C                   MOVEL     SCVDR         TEMP
     C     TEMP          COMP      999                                    65
     C*
     C*  GET THE REQUESTED RECORD
     C*
     C     ADKEY         CHAIN     PAD01                              40
     C  N40              EXCEPT    PAD01E
     C*
     C*  NO CHANGE IF VOUCHER WAS SELECTED FOR PAYMENT
     C*
     C                   MOVE      *BLANKS       SCINV#
     C  N40              MOVE      ADINV#        SCINV#
     C     CSKEY         CHAIN     PCS01                              43
     C     *IN43         IFEQ      '0'
     C                   GOTO      STRT
     C                   END
     C*
     C*  LOAD INFORMATION
     C*
     C     *IN40         IFEQ      '0'
     C*
     C     *IN09         IFEQ      '0'
     C                   MOVE      UA(2)         SCMODE
     C                   ELSE
     C                   MOVE      UA(3)         SCMODE
     C                   END
     C*
     C*  GET THE REQUESTED BANK NAME-INFORMATION
     C*
     C                   MOVEL     ADBNK#        SCBNK#
     C     SCBNK#        CHAIN     PBK01                              60
     C   60              MOVE      *BLANK        BKNAME
     C                   MOVEL     BKNAME        SCBNAM
     C*
     C                   MOVE      ADINV#        SCINV#
     C                   Z-ADD     ADINVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCINVD
     C*
     C*  GET THE DATE WORD SUBR IN ACTION.
     C*
     C                   Z-ADD     SCINVD        W6DATE            6 0
     C                   EXSR      AUDWRD
     C                   MOVEA     AUW           SCWIDT
     C*
     C                   Z-ADD     ADDUED        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDUED
     C*
     C*  GET THE DATE WORD SUBR IN ACTION.
     C*
     C     SCDUED        IFGT      *ZERO
     C                   Z-ADD     SCDUED        W6DATE            6 0
     C                   EXSR      AUDWRD
     C                   MOVEA     AUW           SCWDDT
     C                   END
     C*
     C                   MOVE      ADBTCH        SCBTCH
     C                   MOVE      ADPONO        SCPONO
     C                   MOVE      ADPODS        SCPODS
     C**** ADDED 05/13/92
     C******************** MOVE ADCHKD    SCCHKD
     C**** END ADD 05/13/92
     C                   Z-ADD     ADAMT         SCAMT
     C                   Z-ADD     ADFEED        SCFEED
     C                   Z-ADD     ADDISC        SCDISC
     C                   Z-ADD     *ZERO         SCPCT
     C                   Z-ADD     *ZERO         BFPCT             5 4
     C**********           Z-ADDADCHK#    SCCHK#
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   Z-ADD     1             SCSEQ2
     C                   Z-ADD     1             X                 3 0
     C                   MOVE      *ZEROS        C
     C                   MOVE      *ZEROS        F
     C                   MOVE      *ZEROS        D
     C                   MOVE      *ZEROS        A
     C                   MOVE      *ZEROS        S
     C                   MOVE      *ZEROS        AM
     C                   MOVE      *BLANKS       DE
     C     GDTAG         TAG
     C     GDKEY         CHAIN     PGD01                              60
     C     *IN60         IFEQ      '0'
     C     GDTYPE        ANDEQ     '1'
     C     GDSTAT        ANDNE     'D'
     C                   MOVE      GDCO1         C(X)
     C                   MOVE      GDFAC1        F(X)
     C                   MOVE      GDGLD1        D(X)
     C                   MOVE      GDACC1        A(X)
     C                   MOVE      GDSUB1        S(X)
     C                   Z-ADD     GDAMT         AM(X)
     C                   MOVEL     GDDESC        DE(X)
     C                   ADD       1             X
     C                   Z-ADD     X             SCSEQ2
     C                   GOTO      GDTAG
     C*
     C                   END
     C                   XFOOT     AM            SCTDST
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   MOVE      UA(3)         SCMODE
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED BANK NAME-INFORMATION
     C*
     C                   MOVEL     CFBNK#        SCBNK#
     C     SCBNK#        CHAIN     PBK01                              60
     C   60              MOVE      *BLANK        BKNAME
     C                   MOVEL     BKNAME        SCBNAM
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCINV#
     C                   MOVE      *BLANKS       SCINVD
     C                   MOVE      *BLANKS       SCDUED
     C                   MOVE      *BLANKS       SCPONO
     C                   MOVE      *BLANKS       SCPODS
     C*** BEGIN ADD  3/25/93 ************************************
     C******************** MOVE *BLANKS   SCCHKD
     C*** END ADD    3/25/93 ************************************
     C                   Z-ADD     *ZEROS        SCAMT
     C                   Z-ADD     *ZEROS        SCFEED
     C                   Z-ADD     *ZEROS        SCDISC
     C                   Z-ADD     *ZEROS        SCPCT
     C                   Z-ADD     *ZEROS        BFPCT
     C**********           Z-ADD*ZEROS    SCCHK#
     C                   Z-ADD     *ZERO         C
     C                   Z-ADD     *ZERO         F
     C                   Z-ADD     *ZERO         D
     C                   Z-ADD     *ZERO         A
     C                   Z-ADD     *ZERO         S
     C                   Z-ADD     *ZERO         AM
     C                   MOVE      *BLANK        DE
     C*****                MOVE VMCO#     C,1
     C                   MOVE      SCCO#         C(1)
     C                   MOVE      VMFAC#        F(1)
     C                   MOVE      VMGLD#        D(1)
     C                   MOVE      VMACC#        A(1)
     C                   MOVE      VMSUB#        S(1)
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C                   MOVE      *BLANKS       REDISP            1
     C*
     C*  GET THE REQUESTED BANK MASTER.
     C*
     C     SCBNK#        IFEQ      *ZERO
     C                   MOVE      '1'           *IN58
     C                   GOTO      STRT
     C                   ELSE
     C*
     C     SCBNK#        CHAIN     PBK01                              59
     C*
     C     *IN59         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  CASH ACCOUNT MUST BE SAME COMPANY
     C*
     C*
     C     SCCO#         IFNE      BKCO3
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
      *** BEG ADD * AP018 * 7/12/97
      ** CHECK TO SEE IF INVOICE FOR THIS VENDOR HAS ALREADY BEEN
      ** ENTERED.
     C                   MOVE      *OFF          *IN66
     C     APKEY         SETLL     PAD03
     C     APKEY         SETLL     PCH04
     C     APKEY2        READE     PAD03                                  66
     C     *IN66         IFEQ      *ON
     C                   MOVE      *OFF          *IN66
     C     APKEY2        READE     PCH04                                  66
     C                   END
     C     *IN66         IFEQ      *OFF
     C                   MOVE      *ON           *IN48
     C                   GOTO      STRT
     C                   END
     C*
     C*  INVOICE AMOUNT CAN NOT BE ZERO.
     C*
     C     SCAMT         IFEQ      *ZERO
     C                   MOVE      '1'           *IN51
     C                   GOTO      STRT
     C                   END
      *** BEG ADD * 11/10/97 * V3
      *
     C                   MOVE      '0'           SRERR
      * VALIDATE THE INVOICE DATE
     C                   Z-ADD     SCINVD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCINVD
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN52
     C                   GOTO      STRT
     C                   END
      *** END ADD * 11/10/97 * V3
     C*
     C*  CALCULATE DISCOUNT AMT IF PCT ENTERED
     C*
     C     SCPCT         IFNE      BFPCT
     C     SCAMT         MULT      SCPCT         SCDISC
     C                   Z-ADD     SCPCT         BFPCT
     C                   MOVE      'Y'           REDISP            1
     C                   END
     C*
     C*  DISCOUNT MAY NOT BE GREATER THAN INVOICE TOTAL
     C*
     C     SCAMT         IFGT      *ZERO
     C     SCDISC        ANDGT     SCAMT
     C                   MOVE      '1'           *IN49
     C                   GOTO      STRT
     C                   END
     C*
     C     SCAMT         IFLT      *ZERO
     C     SCDISC        ANDNE     *ZERO
     C                   MOVE      '1'           *IN49
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE DISTRIBUTION MUST BE EQUAL TO THE INVOICE AMOUNT.
     C*
     C                   XFOOT     AM            XAM              11 2
     C                   ADD       SCFEED        XAM                            ALREADY FED
     C     XAM           IFEQ      *ZERO
     C                   Z-ADD     SCAMT         AM(1)
     C                   MOVEL     SCPODS        DE(1)
     C                   ELSE
     C     XAM           IFNE      SCAMT
     C                   MOVE      '1'           *IN55
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  INVOICE DATE MUST BE A VALID DATE.
     C*
      ** BEG DEL * 11/10/97 * V3 * THIS HAS ALREADY BEEN CHECKED.
     C*****      SCINVD    IFEQ *ZERO
     C*****                MOVE '1'       *IN52
     C*****                GOTO STRT
     C*****                ELSE
     C*****                Z-ADDSCINVD    AUDATE
     C*****                EXSR AUDCK
     C*****      XXRTCD    IFEQ 1
     C*****                MOVE '1'       *IN52
     C*****                GOTO STRT
     C*****                END
     C*****                END
     C*
     C*****                MOVE SCINVD    TESTYR  20
     C*****      TESTYR    IFLT 90
     C*****                MOVE '1'       *IN52
     C*****                GOTO STRT
     C*****                END
      ** END DEL * 11/10/97 * V3 * THIS HAS ALREADY BEEN CHECKED.
     C*
     C*  CALCULATE DUE DATE IF BLANK BASED ON TERMS CODE
     C*
     C     SCDUED        IFEQ      *ZERO
     C     SCINVD        ANDGT     *ZERO
     C     STERMS        ANDGT     *BLANK
     C                   MOVE      *BLANK        STDESC
     C                   MOVE      'TC'          TCTBL
     C                   MOVE      *BLANKS       TCVAL
     C                   MOVEL     STERMS        TCVAL
     C     TBKEY         CHAIN     PTC01                              60
     C  N60TCDAY1        IFGT      *ZERO
     C                   Z-ADD     SCINVD        AUDATE
     C                   Z-ADD     TCDAY1        AUDAYS
     C                   EXSR      AUFUTR
     C                   Z-ADD     AUDATE        SCDUED
     C     SCAMT         MULT(H)   TCPCT1        SCDISC
     C                   MOVE      'Y'           REDISP
     C                   END
     C                   END
      *** BEG ADD * 11/10/97 * V3
     C*
     C*  DUE DATE MUST BE A VALID DATE.
     C*
     C     SCDUED        IFEQ      *ZERO
     C                   MOVE      '1'           *IN54
     C                   GOTO      STRT
     C                   ELSE
     C     SCDUED        IFLT      368
     C                   Z-ADD     SCINVD        AUDATE
     C                   Z-ADD     SCDUED        AUDAYS
     C                   EXSR      AUFUTR
     C                   Z-ADD     AUDATE        SCDUED
     C                   MOVE      'Y'           REDISP
     C                   END
     C                   Z-ADD     SCDUED        AUDATE
      * VALIDATE THE DUE DATE
     C                   Z-ADD     SCDUED        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDUED
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN54
     C                   GOTO      STRT
     C                   END
     C*****                EXSR AUDCK
     C*****      XXRTCD    IFEQ 1
     C*****                MOVE '1'       *IN54
     C*****                GOTO STRT
     C*****                END
      *** END ADD * 11/10/97 * V3
     C                   END
     C*
      *** BEG DEL * 11/10/97 * V3
     C*****                MOVE SCDUED    TESTYR  20
     C*****      TESTYR    IFLT 90
     C*****                MOVE '1'       *IN54
     C*****                GOTO STRT
     C*****                END
      *** END DEL * 11/10/97 * V3
     C*
     C*  REDISPLAY IF INDICATED
     C*
     C     REDISP        IFEQ      'Y'
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE G/L DISTRIBUTION MUST BE VALID.
     C*
     C                   Z-ADD     *ZERO         Y                 2 0
     C                   Z-ADD     1             X
     C     LOOPT         TAG
     C     AM(X)         IFNE      *ZERO
     C*
     C                   Z-ADD     C(X)          X@CO#
     C                   Z-ADD     F(X)          X@FAC#
     C                   Z-ADD     D(X)          X@DPT#
     C                   Z-ADD     A(X)          X@ACC#
     C                   Z-ADD     S(X)          X@SUB#
     C*
     C     GLKEY         CHAIN     PCA01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR29'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   ADD       1             X
     C     X             IFLT      10
     C                   GOTO      LOOPT
     C                   END
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     ADKEY         CHAIN     PAD01                              40
     C*
     C     *IN40         IFEQ      '1'
     C                   Z-ADD     99999999      SCSEQ1            5 0
     C     ADKEY         SETLL     PAD01
     C                   READP     PAD01                                  60
     C  N60              EXCEPT    PAD01E
     C  N60ADCO#         COMP      SCCO#                              6060
     C  N60ADFAC#        COMP      SCFAC#                             6060
     C  N60ADVDR         COMP      SCVDR                              6060
     C   60              Z-ADD     *ZERO         ADSEQ1
     C*
     C     ADKEY         SETLL     PCH01
     C                   READP     PCH01                                  60
     C  N60CHCO#         COMP      SCCO#                              6060
     C  N60CHFAC#        COMP      SCFAC#                             6060
     C  N60CHVDR         COMP      SCVDR                              6060
     C   60              Z-ADD     *ZERO         CHSEQ1
     C     CHSEQ1        IFGT      ADSEQ1
     C     CHSEQ1        ADD       1             SCSEQ1
     C                   ELSE
     C     ADSEQ1        ADD       1             SCSEQ1
     C                   END
     C                   END
     C*
     C                   MOVE      SCSEQ1        ADSEQ1
     C                   MOVE      '1'           ADCODE
     C                   MOVE      SCVDR         ADVDR
     C                   MOVE      SCCO#         ADCO#
     C                   MOVE      SCFAC#        ADFAC#
     C                   MOVE      SCBTCH        ADBTCH
     C                   MOVE      SCBNK#        ADBNK#
     C                   Z-ADD     SCINVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ADINVD
     C                   MOVEL     SCINVD        ADACMO
     C                   MOVE      SCINVD        ADACYR
     C                   MOVE      SCINV#        ADINV#
     C                   Z-ADD     SCDUED        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ADDUED
     C                   MOVE      SCPONO        ADPONO
     C                   MOVE      SCPODS        ADPODS
     C***** ADDED 05/13/92
     C******************** MOVE SCCHKD    ADCHKD
     C***** END ADD 05/13/92
     C                   Z-ADD     ADAMT         XXAMT
     C                   Z-ADD     SCAMT         ADAMT
     C                   Z-ADD     SCDISC        ADDISC
     C                   Z-ADD     *ZERO         ADCHK#
     C*
     C                   MOVE      Q1USER        ADCUSR
     C                   MOVE      Q1PGM         ADIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ADCDT
     C                   Z-ADD     DSTIME        ADCTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PAD01
     C                   ELSE
     C                   MOVE      Q1USER        ADIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ADIDT
     C                   Z-ADD     DSTIME        ADITIM
     C                   MOVE      *ZEROS        ADFEED
     C                   Z-ADD     *ZERO         ADSEQ2
     C                   MOVE      *BLANKS       ADBUL1
     C                   MOVE      *BLANKS       ADCODE
     C                   MOVE      *BLANKS       ADSEP
     C                   WRITE     PAD01
     C                   END
     C*
     C*  GET THE REQUESTED VENDOR RECORD.
     C*
     C***        SCVDR     CHAINPVM01                41
     C*
     C***                  MOVE Q1USER    VMCUSR
     C***                  MOVE Q1PGM     VMCPGM
     C***                  TIME           DSDTTM
     C***        DSDATE    MULT 10000.01  VMCDT
     C***                  Z-ADDDSTIME    VMCTIM
     C*
     C***        *IN40     IFEQ '0'
     C***                  SUB  XXAMT     VMPAMT
     C***                  ADD  SCAMT     VMPAMT
     C***                  ELSE
     C***                  ADD  1         VMTINV
     C***                  ADD  SCAMT     VMPAMT
     C***        SCINVD    MULT 10000.01  VMLPDT
     C***                  END
     C*
     C***                  UPDATPVM01
     C*
     C                   Z-ADD     1             SCSEQ2
     C                   Z-ADD     *ZERO         Y                 2 0
     C     GDKEY         SETLL     PGD01
     C     LOOPU         TAG
     C*
     C*  DELETE ANY PREVIOUS DISTRIBUTION FOR THIS INVOICE
     C*
     C     ADKEY         READE     PGD01                                  60
     C     *IN60         IFEQ      '0'
     C                   DELETE    PGD01
     C                   GOTO      LOOPU
     C                   END
     C*
     C*  WRITE THE EXPENSE DISTRIBUTION
     C*
     C                   Z-ADD     1             Y                 2 0
     C                   Z-ADD     1             X
     C     LOOPU1        TAG
     C     AM(X)         IFNE      *ZERO
     C*
     C                   Z-ADD     C(X)          GDCO1
     C                   Z-ADD     F(X)          GDFAC1
     C                   Z-ADD     D(X)          GDGLD1
     C                   Z-ADD     A(X)          GDACC1
     C                   Z-ADD     S(X)          GDSUB1
     C                   Z-ADD     AM(X)         GDAMT
     C                   MOVEL     DE(X)         GDDESC
     C                   MOVEL     'AP'          GDSRC
     C*
     C                   Z-ADD     SCINVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GDIDAT
     C                   MOVEL     SCPONO        GDREFR
     C                   MOVEL     SCINVD        GDACMO
     C                   MOVE      SCINVD        GDACYR
     C                   MOVE      SCBTCH        GDBTCH
     C                   Z-ADD     SCSEQ1        GDSEQ1
     C                   Z-ADD     Y             GDSEQ2
     C                   ADD       1             Y
     C                   MOVE      '1'           GDTYPE
     C                   MOVE      SCVDR         GDVDR
     C                   MOVE      SCCO#         GDCO#
     C                   MOVE      SCFAC#        GDFAC#
     C                   MOVE      SCINV#        GDINV#
     C*
     C                   MOVE      Q1USER        GDCUSR
     C                   MOVE      Q1PGM         GDIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GDCDT
     C                   Z-ADD     DSTIME        GDCTIM
     C                   MOVE      *BLANKS       GDSTAT
     C*
     C                   MOVE      Q1USER        GDIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GDIDT
     C                   Z-ADD     DSTIME        GDITIM
     C                   WRITE     PGD01
     C                   END
     C*
     C                   ADD       1             X
     C     X             IFLT      10
     C                   GOTO      LOOPU1
     C                   END
     C*
     C*** WRITE THE A/P TRANSACTION.
     C*
     C                   Z-ADD     CFCO1         GDCO1
     C                   Z-ADD     CFFAC1        GDFAC1
     C                   Z-ADD     CFDPT1        GDGLD1
     C                   Z-ADD     CFACC1        GDACC1
     C                   Z-ADD     CFSUB1        GDSUB1
     C                   Z-SUB     SCAMT         GDAMT
     C                   MOVE      *BLANK        GDDESC
     C                   MOVEL     'ACCOUNTS'    GDDES1           16
     C                   MOVE      ' PAYABLE'    GDDES1
     C                   MOVEL     GDDES1        GDDESC
     C                   MOVEL     'AP'          GDSRC
     C*
     C                   MOVEL     SCINVD        GDACMO
     C                   MOVE      SCINVD        GDACYR
     C                   MOVEL     SCPONO        GDREFR
     C                   Z-ADD     SCINVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GDIDAT
     C                   MOVE      SCBTCH        GDBTCH
     C                   Z-ADD     SCSEQ1        GDSEQ1
     C                   Z-ADD     1             GDSEQ2
     C                   MOVE      '4'           GDTYPE
     C                   MOVE      SCVDR         GDVDR
     C                   MOVE      SCCO#         GDCO#
     C                   MOVE      SCFAC#        GDFAC#
     C                   MOVE      SCINV#        GDINV#
     C*
     C                   MOVE      Q1USER        GDCUSR
     C                   MOVE      Q1PGM         GDIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GDCDT
     C                   Z-ADD     DSTIME        GDCTIM
     C*
     C                   MOVE      Q1USER        GDIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GDIDT
     C                   Z-ADD     DSTIME        GDITIM
     C                   WRITE     PGD01
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCSEQ1
     C                   MOVE      *BLANK        SCVDR
     C                   GOTO      STRT
     C*
     C     SCR29T        TAG
     C*
     C                   Z-ADD     *ZERO         Y                 2 0
     C                   Z-ADD     1             X
     C     LOOPG         TAG
     C     AM(X)         IFNE      *ZERO
     C*
     C                   Z-ADD     SCCO#         C(X)
     C                   Z-ADD     C(X)          X@CO#
     C******
     C                   Z-ADD     F(X)          X@FAC#
     C                   Z-ADD     D(X)          X@DPT#
     C                   Z-ADD     A(X)          X@ACC#
     C                   Z-ADD     S(X)          X@SUB#
     C*
     C     GLKEY         CHAIN     PCA01                              60
     C     *IN60         IFEQ      '0'
     C     DE(X)         IFEQ      *BLANKS
     C                   MOVEL     CADESC        DE(X)
     C                   END
     C     X@CO#         IFNE      SCCO#
     C     X             ADD       50            Y
     C                   MOVE      '1'           *IN(Y)
     C                   GOTO      STRT
     C                   END
     C                   END
     C     *IN60         IFEQ      '1'
     C     X             ADD       80            Y
     C                   MOVE      '1'           *IN(Y)
     C                   GOTO      STRT
     C                   END
     C                   ADD       1             X
     C     X             IFLT      10
     C                   GOTO      LOOPG
     C                   END
     C*
     C                   END
     C                   END                                                    *IN09 = 1
     C*
     C                   MOVE      *BLANK        Q2RECF
      *** BEG ADD * 7/10/97 * AP007
     C     SCTDST        IFEQ      SCAMT
      *** END ADD * 7/10/97 * AP007
     C                   MOVEL     'SCR02'       Q2RECF
      *** BEG ADD * 7/10/97 * AP007
     C                   ELSE
     C                   MOVE      *ON           *IN29
     C                   MOVEL     'SCR29'       Q2RECF
     C                   END
     C     *LIKE         DEFINE    SCTDST        SCTDS2
      *** END ADD * 7/10/97 * AP007
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
0002 C*
0003 C*  COMPUTE A NEW CALENDAR DATE A GIVEN NUMBER OF DAYS IN THE FUTURE
0004 C*  OF A GIVEN CALENDAR DATE.
0005 C*
0006 C*  AUDATE IS THE BASE USED FROM WHICH THE DATE IS COMPUTED.
0007 C*  AUDAYS IS THE NUMBER OF DAYS IN THE FUTURE TO ADVANCE THE DATE.
0008 C*
0009 C*  THE RESULTS OF THE CALCULATION ARE REPLACED IN THE FIELD AUDATE.
0010 C*
0011 C     AUFUTR        BEGSR
0012 C*
0013 C                   MOVE      '0'           XX60              1
0014 C                   MOVE      '0'           XX61              1
0015 C*
0016 C   60              MOVE      '1'           XX60
0017 C   61              MOVE      '1'           XX61
0018 C*
0019 C                   Z-ADD     AUDAYS        AUDAYS            5 0  6060
0020 C   60              GOTO      AUFUT2
0021 C*
0022 C                   MOVEL     AUDATE        AUMD              4 0
0023 C                   MOVE      AUDATE        AUY1              2 0
0024 C                   MOVEL     AUMD          AUM1              2 0
0025 C                   MOVE      AUMD          AUD1              2 0
      *** BEG CHG * 11/10/97 * V3
      * IF AUY1 = 0 THEN ASSUME THE YEAR 2000 AND SET AUX TO 25.
0026 C*****      AUY1      DIV  4         AUX     20
0027 C*****                MVR            AUX            60
     C     AUY1          IFNE      0
     C     AUY1          DIV       4             AUX               2 0
     C                   MVR                     AUX                      60
     C                   ELSE
     C                   Z-ADD     25            AUX
     C                   END
      *** END CHG * 11/10/97 * V3
0028 C*
0029 C   60LY(AUM1)      ADD       AUD1          AUDYS1            5 0
0030 C  N60NY(AUM1)      ADD       AUD1          AUDYS1
0031 C     AUDYS1        ADD       AUDAYS        AUDYS1
0032 C     AUFUT1        TAG
0033 C*
0034 C   60AUDYS1        SUB       366           AUDYS2            5 061
0035 C  N60AUDYS1        SUB       365           AUDYS2               61
0036 C   61              ADD       1             AUY1
0037 C   61              Z-ADD     AUDYS2        AUDYS1
      *** BEG CHG * 11/10/97 * V3
      * IF AUY1 = 0 THEN ASSUME THE YEAR 2000 AND SET AUX TO 25.
0038 C***61      AUY1      DIV  4         AUX
0039 C***61                MVR            AUX            60
     C   61AUY1          IFNE      0
     C     AUY1          DIV       4             AUX               2 0
     C                   MVR                     AUX                      60
     C                   ELSE
     C                   Z-ADD     25            AUX
     C                   END
      *** END CHG * 11/10/97 * V3
0040 C   61              GOTO      AUFUT1
0041 C*
0042 C                   Z-ADD     1             AUM1
0043 C                   Z-ADD     AUDYS1        AUDYS3            3 0
0044 C   60AUDYS3        LOOKUP    LY(AUM1)                           61  61
0045 C  N60AUDYS3        LOOKUP    NY(AUM1)                           61  61
0046 C   61AUM1          SUB       1             AUM1
0047 C  N61              Z-ADD     12            AUM1
0048 C   60AUDYS1        SUB       LY(AUM1)      AUMD
0049 C  N60AUDYS1        SUB       NY(AUM1)      AUMD
0050 C*
0051 C                   MOVEL     AUM1          AUMD
0052 C                   MOVEL     AUMD          AUDATE
      *** BEG CHG * 11/10/97 * V3
0053 C*****                MOVE AUY1      AUDATE
     C     AUY1          IFLT      40
     C     AUY1          ADD       2000          AUY14             4 0
     C                   ELSE
     C     AUY1          ADD       1900          AUY14
     C                   END
     C                   MOVE      AUY14         AUDATE
      *** BEG CHG * 11/10/97 * V3
0054 C*
0055 C     XX60          COMP      '1'                                    60
0056 C     XX61          COMP      '1'                                    61
0057 C*
0058 C*
0059 C     AUFUT2        ENDSR
     C/EJECT
     C*** THIS ROUTINE WILL VALIDATE THE VALUE PASSED IN THE
     C*   FIELD 'AUDATE' FOR A VALID DATE.
     C*
     C*** IF AN INVALID DATE IS FOUND, THE PROGRAM INDICATOR
     C*   XXRTCD WILL BE TURNED ON. THE SETTINGS OF THE USED
     C*   INDICATORS WILL BE SAVED AND LATER RESTORED.
     C*
     C     AUDCK         BEGSR
     C*
     C                   MOVE      '0'           XX60              1
     C                   MOVE      '0'           XX61              1
     C                   MOVE      '0'           XX62              1
     C*
     C   60              MOVE      '1'           XX60
     C   61              MOVE      '1'           XX61
     C   62              MOVE      '1'           XX62
     C*
     C*** IF AN INVALID DATE IS FOUND A 1 WILL BE PLACED IN
     C*** IN THE FIELD XXRTCD.
     C*
     C                   Z-ADD     *ZERO         XXRTCD            1 0
     C                   SETOFF                                       606162
     C*
     C*** BREAK DOWN THE DATE PASSED IN ORDER TO VERIFY IT.
     C*
     C                   MOVEL     AUDATE        SRMD              4 0
     C                   MOVE      AUDATE        SRYC              2 0
     C                   MOVEL     SRMD          SRM1              2 0
     C                   MOVE      SRMD          SRD1              2 0
     C*
     C*** VERIFY THAT THE YEAR SECTION HAS A VALID VALUE.
     C***
     C*** IF THE YEAR IS NEGATIVE FORCE A ZERO AND SETON
     C*** AN ERROR INDICATOR (*IN62).
     C*
     C     SRYC          COMP      *ZERO                                60
     C   60              MOVE      '00'          SRYC
     C   60              SETON                                        62
     C   60              GOTO      SRDCKA
     C*
     C     SRYC          COMP      99                                 60
     C   60              MOVE      '99'          SRYC
     C   60              SETON                                        62
     C*
     C*** VERIFY THAT THE MONTH SECTION HAS A VALID VALUE.
     C*** A VALID MONTH WILL BE ONE OR GREATER, IF NOT SETON
     C*** AN ERROR INDICATOR (*IN62) AND FORCE JANUARY(1).
     C*
     C     SRDCKA        TAG
     C     SRM1          COMP      1                                    60
     C   60              Z-ADD     1             SRM1
     C   60              SETON                                        62
     C   60              GOTO      SRDCKA
     C*
     C*** A VALID MONTH WILL BE 12 OR LESS, IF NOT SETON
     C*** AN ERROR INDICATOR (*IN62) AND FORCE DECEMBER (12).
     C*
     C     SRM1          COMP      12                                 60
     C   60              Z-ADD     12            SRM1
     C   60              SETON                                        62
     C*
     C/EJECT
     C*
     C*** VERIFY THE DAY SECTION OF THE DATE PASSED BUT FIRST
     C*** ESTABLISH IF THE YEAR IS A LEAP YEAR OR A NORMAL
     C*** YEAR.
     C*
     C     SRDCKB        TAG
     C*
     C                   MOVE      SRYC          SRY1              2 0
     C     SRY1          DIV       4             SRX               2 0
     C                   MVR                     SRX                      60
     C   60              Z-ADD     LY(SRM1)      SRDYS1            5 0
     C  N60              Z-ADD     NY(SRM1)      SRDYS1
     C*
     C     SRM1          ADD       1             SRX
     C     SRX           COMP      12                                 61
     C   60
     CANN61              Z-ADD     LY(SRX)       SRDYS2            5 0
     C  N60
     CANN61              Z-ADD     NY(SRX)       SRDYS2
     C   61SRDYS1        ADD       31            SRDYS2
     C     SRDYS2        SUB       SRDYS1        SRDYS1
     C     SRD1          COMP      1                                    60
     C   60              Z-ADD     1             SRD1
     C   60              SETON                                        62
     C   60              GOTO      SRDCKC
     C**
     C     SRD1          COMP      SRDYS1                             60
     C   60              Z-ADD     SRDYS1        SRD1
     C   60              SETON                                        62
     C*
     C*** PUT THE PIECES OF THE DATE BACK TOGETHER IN ORDER
     C*** TO SEND THEM BACK TO THE CALLER.
     C*
     C     SRDCKC        TAG
     C*
     C                   MOVEL     SRM1          SRMD
     C                   MOVE      SRD1          SRMD
      *** BEG CHG * 11/10/97 * V3
     C*****                MOVELSRMD      AUDATE  60
     C                   MOVEL     SRMD          AUDATE            8 0
      *** END CHG * 11/10/97 * V3
     C                   MOVE      SRYC          AUDATE
     C*
     C*** IF AN ERROR HAS OCCURRED *IN62 WILL BE ON, RETURN TO THE
     C*** CALLER A CODE OF '1' TO REFLECT THIS FACT.
     C*
     C   62              Z-ADD     1             XXRTCD
     C*
     C*** RETURN THE INDICATORS BACK TO THEIR STATE BEFORE THIS SUB-
     C*   ROUTINE WAS EXECUTED.
     C*
     C     XX60          COMP      '1'                                    60
     C     XX61          COMP      '1'                                    61
     C     XX62          COMP      '1'                                    62
     C*
     C                   ENDSR
0616 C************************  *************************************
0617 C* AUDWRD SUBROUTINE    *  * DATE/WORD SUBROUTINE              *
0618 C************************  *************************************
0619 C     AUDWRD        BEGSR
0620 C*
0621 C                   MOVE      '0'           XX40              1
0622 C                   MOVE      '0'           XX41              1
0623 C                   MOVE      '0'           XX42              1
0624 C*
0625 C   40              MOVE      '1'           XX40
0626 C   41              MOVE      '1'           XX41
0627 C   42              MOVE      '1'           XX42
0628 C*
0629 C                   Z-ADD     *ZERO         AUJYD
0630 C                   Z-ADD     *ZERO         AUJD
0631 C                   MOVE      *BLANK        AUW
0632 C                   MOVEL     W6DATE        AUMD              4 0
0633 C                   MOVE      W6DATE        AUY1              2 0
0634 C                   MOVEL     AUMD          AUM1              2 0
0635 C                   MOVE      AUMD          AUD1              2 0
0636 C*
0637 C                   Z-ADD     1900          AUYYYY            4 0
0638 C                   ADD       AUY1          AUYYYY
0639 C*
0640 C     AUM1          SUB       3             AUWKMM            2 0  4041
0641 C   40              ADD       12            AUWKMM
0642 C  N41AUWKMM        MULT(H)   30.6          AUJD              7 0
0643 C                   ADD       AUD1          AUJD
0644 C     AUYYYY        SUB       1900          AUWKYY            3 0    42
     C   40
0645 CANN42              SUB       1             AUWKYY
0646 C     AUWKYY        MULT      365.25        AUJYD             7 0
0647 C                   ADD       AUJYD         AUJD
0648 C     AUJD          DIV       7             AUWK7             7 0
0649 C                   MVR                     AUPTR             1 0
0650 C*          AUPTR     ADD  1         AUPTR
0651 C*
0652 C     AUM1          LOOKUP    TABM#         TABMON                   40
0653 C     AUPTR         LOOKUP    TABD#         TABDAY                   40
0654 C*
0655 C                   Z-ADD     1             I                 3 0
0656 C                   MOVEA     TABDAY        AUW(1)
0657 C     ' '           LOOKUP    AUW(I)                                 40
0658 C                   ADD       1             I
0659 C                   MOVEA     TABMON        AUW(I)
0660 C     ' '           LOOKUP    AUW(I)                                 40
0661 C                   ADD       1             I
0662 C                   MOVE      AUD1          AXD1              2
0662 C                   MOVEA     AXD1          AUW(I)
0663 C     ' '           LOOKUP    AUW(I)                                 40
0664 C                   MOVEA     ','           AUW(I)
0665 C                   ADD       1             I
0666 C                   MOVE      AUYYYY        AXYYYY            4
0666 C                   MOVEA     AXYYYY        AUW(I)
0667 C*
0668 C     XX40          COMP      '1'                                    40
0669 C     XX41          COMP      '1'                                    41
0670 C     XX42          COMP      '1'                                    42
0671 C                   ENDSR
     OPVM01     E            VMREL
     OPAD01     E            PAD01E
     OPAP01     E            PAP01E
**  TABLE UA
-- ADD --
- UPDATE -
- INQUIRY-
**   NORMAL YEAR NUMBER OF DAYS ARRAY.
000031059090120151181212243273304334
** LEAP YEAR NUMBER OF DAYS ARRAY.
000031060091121152182213244274305335
**
01JAN02FEB03MAR04APR05MAY06JUN
07JUL08AUG09SEP10OCT11NOV12DEC
**
0WED1THU2FRI3SAT4SUN5MON6TUE
