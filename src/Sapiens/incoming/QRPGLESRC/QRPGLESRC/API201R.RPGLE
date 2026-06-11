     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE API201R - RECURRING VOUCHER MASTER MAINTENANCE
     H******************************************************************
     FAPI201F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FAPRCL1    UF A E           K DISK
     FAPRDL1    UF A E           K DISK
     FBKCFL1    IF   E           K DISK
     FAPVML1    IF   E           K DISK
     FAPAPL1    IF   E           K DISK
     FBKCAL1    IF   E           K DISK
     FAPBKL1    IF   E           K DISK
     FBKSYL1    IF   E           K DISK
     FAPVDP     IF   E           K DISK
     FBKTBL     IF   E           K DISK
     F                                     IGNORE(PRC01)
     FBKDTL1    IF   E           K DISK

     D*
     D PC              S              5  4 DIM(9)
0076 D*
     D UA              S             10    DIM(3) CTDATA PERRCD(1)
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
     D                 DS
     D  VD                     1     27  0
     D                                     DIM(9)
     D  VDGLD1                 1      3  0
     D  VDGLD2                 4      6  0
     D  VDGLD3                 7      9  0
     D  VDGLD4                10     12  0
     D  VDGLD5                13     15  0
     D  VDGLD6                16     18  0
     D  VDGLD7                19     21  0
     D  VDGLD8                22     24  0
     D  VDGLD9                25     27  0
     D                 DS
     D  VA                     1     27  0
     D                                     DIM(9)
     D  VDACC1                 1      3  0
     D  VDACC2                 4      6  0
     D  VDACC3                 7      9  0
     D  VDACC4                10     12  0
     D  VDACC5                13     15  0
     D  VDACC6                16     18  0
     D  VDACC7                19     21  0
     D  VDACC8                22     24  0
     D  VDACC9                25     27  0
     D                 DS
     D  VS                     1     27  0
     D                                     DIM(9)
     D  VDSUB1                 1      3  0
     D  VDSUB2                 4      6  0
     D  VDSUB3                 7      9  0
     D  VDSUB4                10     12  0
     D  VDSUB5                13     15  0
     D  VDSUB6                16     18  0
     D  VDSUB7                19     21  0
     D  VDSUB8                22     24  0
     D  VDSUB9                25     27  0
     D                 DS
     D  VAM                    1     99  2
     D                                     DIM(9)
     D  VDAMT1                 1     11  2
     D  VDAMT2                12     22  2
     D  VDAMT3                23     33  2
     D  VDAMT4                34     44  2
     D  VDAMT5                45     55  2
     D  VDAMT6                56     66  2
     D  VDAMT7                67     77  2
     D  VDAMT8                78     88  2
     D  VDAMT9                89     99  2
     D                 DS
     D  VDS                    1    180
     D                                     DIM(9)
     D  VDDES1                 1     20
     D  VDDES2                21     40
     D  VDDES3                41     60
     D  VDDES4                61     80
     D  VDDES5                81    100
     D  VDDES6               101    120
     D  VDDES7               121    140
     D  VDDES8               141    160
     D  VDDES9               161    180
     D                 DS
     D  VPC                    1     45  4
     D                                     DIM(9)
     D  VDPCT1                 1      5  4
     D  VDPCT2                 6     10  4
     D  VDPCT3                11     15  4
     D  VDPCT4                16     20  4
     D  VDPCT5                21     25  4
     D  VDPCT6                26     30  4
     D  VDPCT7                31     35  4
     D  VDPCT8                36     40  4
     D  VDPCT9                41     45  4
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C     *LIKE         DEFINE    SCSEQ1        SCSEQ2
     C     *LIKE         DEFINE    SCCO1         X@CO#
     C     *LIKE         DEFINE    SCFAC1        X@FAC#
     C     *LIKE         DEFINE    SCDPT1        X@DPT#
     C     *LIKE         DEFINE    SCACC1        X@ACC#
     C     *LIKE         DEFINE    SCSUB1        X@SUB#
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
     C*
     C     TBLKEY        KLIST
     C                   KFLD                    TBLID             2
     C                   KFLD                    TBLVAL           10
     C*
     C     A1KEY         KLIST
     C                   KFLD                    SCVDR
     C                   KFLD                    SCINV#
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   GET THE DEFAULT COMPANY AND FACILITY
     C*
     C     'AP'          CHAIN     PDT01                              60
     C*
     C*** SET THE SYSTEM OPTION FOR CO/FACL DISPLAY
     C*
     C                   MOVE      *BLANKS       TBLVAL
     C                   MOVEL     'OP'          TBLID
     C                   MOVEL     'COFAC'       TBLVAL
     C     TBLKEY        CHAIN     POP01                              60
     C  N60              MOVEL     OPPARM        COFAC             1
     C     COFAC         COMP      'Y'                                    98
     C*
     C     'AP'          CHAIN     PAP01                              60
     C     *IN60         IFEQ      '0'
     C     APCO#         IFGT      0
     C                   Z-ADD     APCO#         DFTCO             3 0
     C                   END
     C     APFAC#        IFGT      0
     C                   Z-ADD     APFAC#        DFTFAC            3 0
     C                   END
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
     C                   MOVE      *BLANK        DE
     C     'BK'          CHAIN     PSY01                              54
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
     C                   EXFMT     SCR29
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
     C                   CALL      'APD201R'
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
     C*   F7 = FREQUENCY CODE INQUIRY            *
     C********************************************
     C     *INKG         IFEQ      '1'
     C*********
     C* SCR01 *
     C*********
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      '1'           QQRTC
     C                   CALL      'BKD514R'
     C                   PARM                    XXCODE           10
     C                   PARM                    QQRTC             1
     C     QQRTC         IFEQ      'A'
     C                   GOTO      TERM
     C                   END
     C*
     C     XXCODE        IFNE      *BLANK
     C                   MOVEL     XXCODE        SCFREQ
     C                   END
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END                                                    END SCR01
     C                   END                                                    END *INKF
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
     C     ADKEY         CHAIN     PRC01                              60
     C  N60              EXCEPT    PRC01E
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
     C*
     C*  CHECK FOR PCT DISTRIBUTION
     C*
     C     1             DO        9             P
     C     PC(P)         IFNE      *ZERO
     C     AM(P)         IFEQ      *ZERO
     C     SCAMT         MULT      PC(P)         AM(P)
     C                   END
     C                   END
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR29'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END SCR02
     C                   END                                                    END *INKI
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
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
     C*  NO TEMPORARY VENDOR
     C*
     C                   MOVEL     SCVDR         TEMP              3 0
     C     TEMP          IFEQ      999
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
     C                   MOVEL     VMNAME        SCNAME
     C                   MOVEL     VMTERM        STERMS
     C                   MOVE      *BLANK        STDESC
     C                   MOVE      'TC'          TCTBL
     C                   MOVE      *BLANKS       TCVAL
     C                   MOVEL     VMTERM        TCVAL
     C     TBKEY         CHAIN     PTC01                              60
     C  N60              MOVEL     TCDESC        STDESC
     C                   MOVEL     SCVDR         TEMP
     C     TEMP          COMP      999                                    65
     C*
     C*  GET THE REQUESTED RECORD
     C*
     C     ADKEY         CHAIN     PRC01                              40
     C  N40              EXCEPT    PRC01E
     C  N40SCFREQ        IFEQ      *BLANKS
     C                   MOVE      RCFREQ        SCFREQ
     C                   END
     C*
     C* EDIT THE FREQUENCY CODE
     C*
     C                   MOVE      'FR'          TCTBL
     C                   MOVE      *BLANKS       TCVAL
     C                   MOVEL     SCFREQ        TCVAL
     C     TBKEY         CHAIN     PFR01                              45
     C     *IN45         IFEQ      '1'
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
     C                   MOVEL     RCBNK#        SCBNK#
     C     SCBNK#        CHAIN     PBK01                              60
     C   60              MOVE      *BLANK        BKNAME
     C                   MOVEL     BKNAME        SCBNAM
     C*
     C                   MOVE      RCINV#        SCINV#
     C*
     C                   MOVE      RCFREQ        SCFREQ
     C                   MOVE      RCPODS        SCPODS
     C                   Z-ADD     RCAMT         SCAMT
     C                   Z-ADD     RCDISC        SCDISC
     C                   Z-ADD     *ZERO         SCPCT
     C                   Z-ADD     *ZERO         BFPCT             5 4
     C*
     C*  LOAD DEFAULT DISTRIBUTION
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
     C                   MOVE      *ZEROS        PC
     C     GDTAG         TAG
     C     GDKEY         CHAIN     PRD01                              60
     C     *IN60         IFEQ      '0'
     C     RDTYPE        ANDEQ     '1'
     C     RDSTAT        ANDNE     'D'
     C                   MOVE      RDCO1         C(X)
     C                   MOVE      RDFAC1        F(X)
     C                   MOVE      RDGLD1        D(X)
     C                   MOVE      RDACC1        A(X)
     C                   MOVE      RDSUB1        S(X)
     C                   Z-ADD     RDAMT         AM(X)
     C                   MOVEL     RDDESC        DE(X)
     C                   ADD       1             X
     C                   Z-ADD     X             SCSEQ2
     C                   GOTO      GDTAG
     C*
     C                   END
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
     C                   MOVE      *BLANKS       SCPODS
     C                   Z-ADD     *ZEROS        SCAMT
     C                   Z-ADD     *ZEROS        SCDISC
     C                   Z-ADD     *ZEROS        SCPCT
     C                   Z-ADD     *ZEROS        BFPCT
     C                   Z-ADD     *ZERO         C
     C                   Z-ADD     *ZERO         F
     C                   Z-ADD     *ZERO         D
     C                   Z-ADD     *ZERO         A
     C                   Z-ADD     *ZERO         S
     C                   Z-ADD     *ZERO         AM
     C                   MOVE      *BLANK        DE
     C                   Z-ADD     *ZERO         PC
     C*
     C                   MOVE      SCCO#         C(1)
     C                   MOVE      VMFAC#        F(1)
     C                   MOVE      VMGLD#        D(1)
     C                   MOVE      VMACC#        A(1)
     C                   MOVE      VMSUB#        S(1)
     C*
     C* GET DISTRIBUTION PROFILE
     C*
     C     SCVDR         CHAIN     PVD01                              41
     C  N411             DO        9             P                 3 0
     C     VD(P)         IFNE      *ZERO
     C     VA(P)         ORNE      *ZERO
     C     VS(P)         ORNE      *ZERO
     C                   MOVE      SCCO#         C(P)
     C                   MOVE      VMFAC#        F(P)
     C                   MOVE      VD(P)         D(P)
     C                   MOVE      VA(P)         A(P)
     C                   MOVE      VS(P)         S(P)
     C                   Z-ADD     VPC(P)        PC(P)
     C                   Z-ADD     VAM(P)        AM(P)
     C                   MOVEL     VDS(P)        DE(P)
     C                   END
     C                   END
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
     C*
     C*  INVOICE AMOUNT CAN NOT BE ZERO.
     C*
     C     SCAMT         IFEQ      *ZERO
     C                   MOVE      '1'           *IN51
     C                   GOTO      STRT
     C                   END
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
     C*  CHECK FOR PCT DISTRIBUTION
     C*
     C     1             DO        9             P
     C     PC(P)         IFNE      *ZERO
     C     AM(P)         IFEQ      *ZERO
     C     SCAMT         MULT      PC(P)         AM(P)
     C                   END
     C                   END
     C                   END
     C*
     C*  THE DISTRIBUTION MUST BE EQUAL TO THE INVOICE AMOUNT.
     C*
     C                   XFOOT     AM            XAM              11 2
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
     C     ADKEY         CHAIN     PRC01                              40
     C*
     C     *IN40         IFEQ      '1'
     C                   Z-ADD     99999999      SCSEQ1            5 0
     C     ADKEY         SETLL     PRC01
     C                   READP     PRC01                                  60
     C  N60              EXCEPT    PRC01E
     C  N60RCCO#         COMP      SCCO#                              6060
     C  N60RCFAC#        COMP      SCFAC#                             6060
     C  N60RCVDR         COMP      SCVDR                              6060
     C   60              Z-ADD     *ZERO         RCSEQ1
     C     RCSEQ1        ADD       1             SCSEQ1
     C                   END
     C*
     C                   MOVE      SCSEQ1        RCSEQ1
     C                   MOVE      '1'           RCCODE
     C                   MOVE      SCVDR         RCVDR
     C                   MOVE      SCCO#         RCCO#
     C                   MOVE      SCFAC#        RCFAC#
     C                   MOVE      SCFREQ        RCFREQ
     C                   MOVE      SCBNK#        RCBNK#
     C                   MOVE      SCINV#        RCINV#
     C                   MOVE      SCPODS        RCPODS
     C                   Z-ADD     SCAMT         RCAMT
     C                   Z-ADD     SCDISC        RCDISC
     C*
     C                   MOVE      Q1USER        RCCUSR
     C                   MOVE      Q1PGM         RCIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RCCDT
     C*
     C                   Z-ADD     DSTIME        RCCTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PRC01
     C                   ELSE
     C                   MOVE      Q1USER        RCIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RCIDT
     C*
     C                   Z-ADD     DSTIME        RCITIM
     C                   MOVE      *BLANKS       RCCODE
     C                   MOVE      *BLANKS       RCSEP
     C                   WRITE     PRC01
     C                   END
     C*
     C                   Z-ADD     1             SCSEQ2
     C                   Z-ADD     *ZERO         Y                 2 0
     C     GDKEY         SETLL     PRD01
     C     LOOPU         TAG
     C*
     C*  DELETE ANY PREVIOUS DISTRIBUTION FOR THIS INVOICE
     C*
     C     ADKEY         READE     PRD01                                  60
     C     *IN60         IFEQ      '0'
     C                   DELETE    PRD01
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
     C                   Z-ADD     C(X)          RDCO1
     C                   Z-ADD     F(X)          RDFAC1
     C                   Z-ADD     D(X)          RDGLD1
     C                   Z-ADD     A(X)          RDACC1
     C                   Z-ADD     S(X)          RDSUB1
     C                   Z-ADD     AM(X)         RDAMT
     C                   MOVEL     DE(X)         RDDESC
     C                   MOVEL     'AP'          RDSRC
     C*
     C                   MOVE      SCFREQ        RDFREQ
     C                   Z-ADD     SCSEQ1        RDSEQ1
     C                   Z-ADD     Y             RDSEQ2
     C                   ADD       1             Y
     C                   MOVE      '1'           RDTYPE
     C                   MOVE      SCVDR         RDVDR
     C                   MOVE      SCCO#         RDCO#
     C                   MOVE      SCFAC#        RDFAC#
     C                   MOVE      SCINV#        RDINV#
     C*
     C                   MOVE      Q1USER        RDCUSR
     C                   MOVE      Q1PGM         RDIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RDCDT
     C*
     C                   Z-ADD     DSTIME        RDCTIM
     C                   MOVE      *BLANKS       RDSTAT
     C*
     C                   MOVE      Q1USER        RDIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RDIDT
     C*
     C                   Z-ADD     DSTIME        RDITIM
     C                   WRITE     PRD01
     C                   END
     C*
     C                   ADD       1             X
     C     X             IFLT      10
     C                   GOTO      LOOPU1
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCSEQ1
     C                   MOVE      *BLANK        SCVDR
     C                   MOVE      *BLANK        SCFREQ
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
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPRC01     E            PRC01E
**  TABLE UA
-- ADD --
- UPDATE -
- INQUIRY-
