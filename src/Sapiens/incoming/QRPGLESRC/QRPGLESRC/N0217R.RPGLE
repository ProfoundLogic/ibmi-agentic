     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*                                                              **
     H*                                                              **
     H*                                                              **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H* P O T E N T I A L     P R O B L E M                          **
     H*   IF USER CALL TIME/MATERIALS ENTRY FROM F0086R SOMEONE ELSE **
     H*   CAN GO INTO HOLD/RELEASE AND UPDATE REVIEWER FEE OR RELEASE**
     H*   EOMB.  THIS MEANS THEY COULD BILL THE INVOICE AND NOT HAVE **
     H*   THE CORRECT AMOUNTS.                                       **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H*                                                              **
     H*                                                              **
     H*                                                              **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H*    PROGRAM: N0217R                                           **
     H*    DESCRIPTION: RELEASE/HOLD - SCREEN PROGRAM FOR PROVIDER   **
     H*                 BILLS                                        **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H*  REVISIONS:                                                  **
     H*                                                               *
     H*  06/09/20 C:212 Project : SY420                               *
     H*                 Mask SSN                                      *
     H*                 Recompile only                                *
     H*                                                               *
     H*  08/15/00 C:62  PROJECT: MC215                                *
     H*                 REPLACE SFBPF REFERENCES WITH APIS.           *
     H*                                                              **
     H*  05/19/00 C:62  PROJECT: MC172                               **
     H*                 ADDED ADDITIONAL PARM TO CALL TO F3040R.     **
     H*                                                              **
     H*  09/23/99 C:62 PROJECT: MC212                                **
     H*                EXPAND FYR TO 3.0P                            **
     H*                                                              **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H*  INDICATOR USAGE:                                            **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     FN0217F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FFAEOLG    IF   E           K DISK
     F                                     INFDS(DBFDS)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FHAEAL     UF   E           K DISK
     FFAEAL1    UF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FFMSML     IF   E           K DISK
     FNDTML     IF   E           K DISK
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 **************************************
     F*

     D*** BEGIN CHG *** 09/23/99 **************************************
     D**********          YR         14  2 0             CASE YEAR
     D*** END   CHG *** 09/23/99 **************************************
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEY             S              9  0 DIM(2)                               EOB#
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  FL                     1     14
     D                                     DIM(14)                              FLAG
     D  SCFL01                 1      1
     D  SCFL02                 2      2
     D  SCFL03                 3      3
     D  SCFL04                 4      4
     D  SCFL05                 5      5
     D  SCFL06                 6      6
     D  SCFL07                 7      7
     D  SCFL08                 8      8
     D  SCFL09                 9      9
     D  SCFL10                10     10
     D  SCFL11                11     11
     D  SCFL12                12     12
     D  SCFL13                13     13
     D  SCFL14                14     14
     D                 DS
     D  EO                     1    126  0
     D                                     DIM(14)                              EOB #
     D  SCEO01                 1      9  0
     D  SCEO02                10     18  0
     D  SCEO03                19     27  0
     D  SCEO04                28     36  0
     D  SCEO05                37     45  0
     D  SCEO06                46     54  0
     D  SCEO07                55     63  0
     D  SCEO08                64     72  0
     D  SCEO09                73     81  0
     D  SCEO10                82     90  0
     D  SCEO11                91     99  0
     D  SCEO12               100    108  0
     D  SCEO13               109    117  0
     D  SCEO14               118    126  0
     D                 DS
     D  RC                     1     42  0
     D                                     DIM(14)                              RECON#
     D  SCRC01                 1      3  0
     D  SCRC02                 4      6  0
     D  SCRC03                 7      9  0
     D  SCRC04                10     12  0
     D  SCRC05                13     15  0
     D  SCRC06                16     18  0
     D  SCRC07                19     21  0
     D  SCRC08                22     24  0
     D  SCRC09                25     27  0
     D  SCRC10                28     30  0
     D  SCRC11                31     33  0
     D  SCRC12                34     36  0
     D  SCRC13                37     39  0
     D  SCRC14                40     42  0
     D                 DS
     D  CT                     1    126
     D                                     DIM(14)                              PRE-CERT
     D  SCCT01                 1      9
     D  SCCT02                10     18
     D  SCCT03                19     27
     D  SCCT04                28     36
     D  SCCT05                37     45
     D  SCCT06                46     54
     D  SCCT07                55     63
     D  SCCT08                64     72
     D  SCCT09                73     81
     D  SCCT10                82     90
     D  SCCT11                91     99
     D  SCCT12               100    108
     D  SCCT13               109    117
     D  SCCT14               118    126
     D                 DS
     D  GR                     1     42  0
     D                                     DIM(14)                              GRP/FND #
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
     D  CS                     1     98  0
     D                                     DIM(14)                              CASE NUMBER
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
     D*** BEGIN CHG *** 09/23/99 **************************************
     D**********  DS
     D**********                              1  280YR
     D**********                              1   20SCYR01
     D**********                              3   40SCYR02
     D**********                              5   60SCYR03
     D**********                              7   80SCYR04
     D**********                              9  100SCYR05
     D**********                             11  120SCYR06
     D**********                             13  140SCYR07
     D**********                             15  160SCYR08
     D**********                             17  180SCYR09
     D**********                             19  200SCYR10
     D**********                             21  220SCYR11
     D**********                             23  240SCYR12
     D**********                             25  260SCYR13
     D**********                             27  280SCYR14
     D                 DS
     D  YR                     1     42  0
     D                                     DIM(14)                              CASE YEAR
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
     D*** END   CHG *** 09/23/99 **************************************
     D                 DS
     D  SY                     1     14
     D                                     DIM(14)                              SYSTEM
     D  SCSY01                 1      1
     D  SCSY02                 2      2
     D  SCSY03                 3      3
     D  SCSY04                 4      4
     D  SCSY05                 5      5
     D  SCSY06                 6      6
     D  SCSY07                 7      7
     D  SCSY08                 8      8
     D  SCSY09                 9      9
     D  SCSY10                10     10
     D  SCSY11                11     11
     D  SCSY12                12     12
     D  SCSY13                13     13
     D  SCSY14                14     14
     D                 DS
     D  CA                     1    112  2
     D                                     DIM(14)                              AMT CHRGD
     D  SCCA01                 1      8  2
     D  SCCA02                 9     16  2
     D  SCCA03                17     24  2
     D  SCCA04                25     32  2
     D  SCCA05                33     40  2
     D  SCCA06                41     48  2
     D  SCCA07                49     56  2
     D  SCCA08                57     64  2
     D  SCCA09                65     72  2
     D  SCCA10                73     80  2
     D  SCCA11                81     88  2
     D  SCCA12                89     96  2
     D  SCCA13                97    104  2
     D  SCCA14               105    112  2
     D                 DS
     D  SA                     1    112  2
     D                                     DIM(14)                              REDUCTIONS
     D  SCSA01                 1      8  2
     D  SCSA02                 9     16  2
     D  SCSA03                17     24  2
     D  SCSA04                25     32  2
     D  SCSA05                33     40  2
     D  SCSA06                41     48  2
     D  SCSA07                49     56  2
     D  SCSA08                57     64  2
     D  SCSA09                65     72  2
     D  SCSA10                73     80  2
     D  SCSA11                81     88  2
     D  SCSA12                89     96  2
     D  SCSA13                97    104  2
     D  SCSA14               105    112  2
     D                 DS
     D  ND                     1    112  2
     D                                     DIM(14)                              NETWORK DISC
     D  SCND01                 1      8  2
     D  SCND02                 9     16  2
     D  SCND03                17     24  2
     D  SCND04                25     32  2
     D  SCND05                33     40  2
     D  SCND06                41     48  2
     D  SCND07                49     56  2
     D  SCND08                57     64  2
     D  SCND09                65     72  2
     D  SCND10                73     80  2
     D  SCND11                81     88  2
     D  SCND12                89     96  2
     D  SCND13                97    104  2
     D  SCND14               105    112  2
     D                 DS
     D  IA                     1    112  2
     D                                     DIM(14)                              ISMUS AMT
     D  SCIA01                 1      8  2
     D  SCIA02                 9     16  2
     D  SCIA03                17     24  2
     D  SCIA04                25     32  2
     D  SCIA05                33     40  2
     D  SCIA06                41     48  2
     D  SCIA07                49     56  2
     D  SCIA08                57     64  2
     D  SCIA09                65     72  2
     D  SCIA10                73     80  2
     D  SCIA11                81     88  2
     D  SCIA12                89     96  2
     D  SCIA13                97    104  2
     D  SCIA14               105    112  2
     D**
     D                 DS
     D  BL                     1     42  0
     D                                     DIM(14)                              NUMBER OF BILLS
     D  SCBL01                 1      3  0
     D  SCBL02                 4      6  0
     D  SCBL03                 7      9  0
     D  SCBL04                10     12  0
     D  SCBL05                13     15  0
     D  SCBL06                16     18  0
     D  SCBL07                19     21  0
     D  SCBL08                22     24  0
     D  SCBL09                25     27  0
     D  SCBL10                28     30  0
     D  SCBL11                31     33  0
     D  SCBL12                34     36  0
     D  SCBL13                37     39  0
     D  SCBL14                40     42  0
     D                 DS
     D  TM                     1     14
     D                                     DIM(14)                              T/M FLAG
     D  SCTM01                 1      1
     D  SCTM02                 2      2
     D  SCTM03                 3      3
     D  SCTM04                 4      4
     D  SCTM05                 5      5
     D  SCTM06                 6      6
     D  SCTM07                 7      7
     D  SCTM08                 8      8
     D  SCTM09                 9      9
     D  SCTM10                10     10
     D  SCTM11                11     11
     D  SCTM12                12     12
     D  SCTM13                13     13
     D  SCTM14                14     14
     D                 DS
     D  RRN                    1    126  0
     D                                     DIM(14)                              RRN
     D  SCDB01                 1      9  0
     D  SCDB02                10     18  0
     D  SCDB03                19     27  0
     D  SCDB04                28     36  0
     D  SCDB05                37     45  0
     D  SCDB06                46     54  0
     D  SCDB07                55     63  0
     D  SCDB08                64     72  0
     D  SCDB09                73     81  0
     D  SCDB10                82     90  0
     D  SCDB11                91     99  0
     D  SCDB12               100    108  0
     D  SCDB13               109    117  0
     D  SCDB14               118    126  0
     D                 DS
     D  CRT                    1    210
     D                                     DIM(14)                              CERT #
     D  SCCE01                 1     15
     D  SCCE02                16     30
     D  SCCE03                31     45
     D  SCCE04                46     60
     D  SCCE05                61     75
     D  SCCE06                76     90
     D  SCCE07                91    105
     D  SCCE08               106    120
     D  SCCE09               121    135
     D  SCCE10               136    150
     D  SCCE11               151    165
     D  SCCE12               166    180
     D  SCCE13               181    195
     D  SCCE14               196    210
     D                 DS
     D  EXT                    1     42  0
     D                                     DIM(14)                              EXTENSION
     D  SCEX01                 1      3  0
     D  SCEX02                 4      6  0
     D  SCEX03                 7      9  0
     D  SCEX04                10     12  0
     D  SCEX05                13     15  0
     D  SCEX06                16     18  0
     D  SCEX07                19     21  0
     D  SCEX08                22     24  0
     D  SCEX09                25     27  0
     D  SCEX10                28     30  0
     D  SCEX11                31     33  0
     D  SCEX12                34     36  0
     D  SCEX13                37     39  0
     D  SCEX14                40     42  0
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   PARTIAL KEY LIST FOR THE EOB WORK FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C*
     C*   KEY LIST FOR THE EOB WORK FILE
     C*
     C     EAKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XZEOB#
     C                   KFLD                    XZRCN#
     C*
     C*   KEY LIST FOR THE EOB FILE
     C*
     C     EAKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN#
     C*
     C     EOKY2         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C***********
     C****KEY*LIST FOR THE HBA TRANS FILE
     C***********
     C***********HBKEY     KLIST
     C***********          KFLD           UPCO#
     C***********          KFLD           UPSUB#
     C***********          KFLD           XXEOB#
     C***********
     C****KEY*LIST FOR THE HBA TRANS FILE
     C***********
     C***********HBKEY1    KLIST
     C***********          KFLD           UPCO#
     C***********          KFLD           UPSUB#
     C***********          KFLD           EO,V
     C***********
     C****PARTIAL KEY LIST FOR THE HBA TRANS FILE
     C***********
     C***********HBKEY2    KLIST
     C***********          KFLD           UPCO#
     C***********          KFLD           UPSUB#
     C***********
     C****KEY*LIST FOR THE NETWORK TRANSACTION FILE
     C***********
     C***********TRKEY     KLIST
     C***********          KFLD           UPCO#
     C***********          KFLD           UPSUB#
     C***********          KFLD           XXFND
     C***********          KFLD           XXFYR
     C***********          KFLD           XXCASE
     C***********          KFLD           XXEOB#
     C***********
     C****KEY*LIST FOR THE NETWORK TRANSACTION FILE
     C***********
     C***********TRKEY1    KLIST
     C***********          KFLD           UPCO#
     C***********          KFLD           UPSUB#
     C***********          KFLD           XXFND
     C***********          KFLD           XXFYR
     C***********          KFLD           XXCASE
     C***********          KFLD           EO,V
     C***********
     C****PARTIAL KEY LIST FOR THE NETWORK TRANSACTION FILE
     C***********
     C***********TRKEY2    KLIST
     C***********          KFLD           UPCO#
     C***********          KFLD           UPSUB#
     C***********
     C****KEY*LIST FOR THE PRE/RE-CERTIFICATION TRANSACTION FILE
     C***********
     C***********PBKEY     KLIST
     C***********          KFLD           UPCO#
     C***********          KFLD           UPSUB#
     C***********          KFLD           XXFND
     C***********          KFLD           XXFYR
     C***********          KFLD           XXCASE
     C***********          KFLD           XXEOB#
     C***********
     C****KEY*LIST FOR THE PRE/RE-CERTIFICATION TRANSACTION FILE
     C***********
     C***********PBKEY1    KLIST
     C***********          KFLD           UPCO#
     C***********          KFLD           UPSUB#
     C***********          KFLD           XXFND
     C***********          KFLD           XXFYR
     C***********          KFLD           XXCASE
     C***********          KFLD           EO,V
     C***********
     C****KEY*LIST FOR THE PRE/RE-CERTIFICATION TRANSACTION FILE
     C***********
     C***********PBKEY2    KLIST
     C***********          KFLD           UPCO#
     C***********          KFLD           UPSUB#
     C***********
     C*   KEY LIST FOR THE TIME/MATERIALS DETAIL CHARGE FILE
     C*
     C     TMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    GR(I1)
     C                   KFLD                    XXFYR
     C                   KFLD                    CS(I1)
     C                   KFLD                    EO(I1)
     C*
     C     TMKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C**********
     C**KEY*LIST FOR THE MEDICAL MANAGEMENT BILLING FILE
     C**********
     C********** MBKEY     KLIST
     C**********           KFLD           UPCO#
     C**********           KFLD           UPSUB#
     C**********           KFLD           XXFND
     C**********           KFLD           XXFYR
     C**********           KFLD           XXCASE
     C**********           KFLD           XXEOB#
     C**********
     C********** MBKEY1    KLIST
     C**********           KFLD           UPCO#
     C**********           KFLD           UPSUB#
     C**********           KFLD           XXFND
     C**********           KFLD           XXFYR
     C**********           KFLD           XXCASE
     C**********           KFLD           EO,V
     C**********
     C********** MBKEY2    KLIST
     C**********           KFLD           UPCO#
     C**********           KFLD           UPSUB#
     C*
     C* FS BASE CONTROL FILE
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** FBKY      KLIST
     C**********           KFLD           XSKEY   2
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C*   PARAMETER LIST FOR CALLED PROGRAM
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
     C*   INITIALIZATION SECTION
     C     *LIKE         DEFINE    EAEOB#        EOB#1
     C     *LIKE         DEFINE    EAEOB#        EOB#14
     C     *LIKE         DEFINE    EAEOB#        QQEOB#
     C     *LIKE         DEFINE    EAEOB#        XZEOB#
     C     *LIKE         DEFINE    EAEOB#        XXEOB#
     C     *LIKE         DEFINE    EAFND         KKFND
     C     *LIKE         DEFINE    EAFYR         KKFYR
     C     *LIKE         DEFINE    EACASE        KKCASE
     C     *LIKE         DEFINE    EAEOB#        KKEOB#
     C     *LIKE         DEFINE    EARCN         XXRCN#
     C     *LIKE         DEFINE    EARCN         XZRCN#
     C     *LIKE         DEFINE    EARCN         KKRCN#
     C     *LIKE         DEFINE    EARCN         RCN#1
     C     *LIKE         DEFINE    EARCN         RCN#14
     C*** BEGIN ADD *** 08/15/00 **************************************
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C*** END   ADD *** 08/15/00 **************************************
     C*
     C*   LOAD THE OUTPUT CONSTANTS FROM THE PARMS
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      '-'           XXHYP1            1
     C                   MOVE      '-'           XXHYP2            1
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           MOVE *BLANKS   XSKEY
     C**********           MOVEL'FB'      XSKEY
     C********** FBKY      CHAINSFB01                60
     C********** *IN60     IFEQ '0'
     C**********           Z-ADD9         T       50
     C********** 1         SUBSTFBAL01:T  XXMFLG  1
     C**********           ELSE
     C**********           MOVE 'N'       XXMFLG
     C**********           END
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
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
     C* GET SUBSCRIBER NAME
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         SCSUB
     C                   MOVE      *BLANK        SCSBNM
     C                   ELSE
     C                   MOVEL     UPSUB#        SCSUB
     C                   MOVEL     SMNAME        SCSBNM
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        KEY
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      '1'           XXDOWN
     C                   Z-ADD     *ZERO         SCFUND
     C                   Z-ADD     *ZEROS        SCCASE
     C                   Z-ADD     *ZEROS        SCYEAR
     C*
     C     EAKEY         SETLL     PEA01
     C     EAKEY         READE     PEA01                                  60
     C*
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
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
     C*
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C*
     C*****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C********************************
     C*   F3 = EXIT                  *
     C********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END IF *INKC
     C*
     C*****************************************************************
     C*   F7 = DETAIL HISTORY                                         *
     C*****************************************************************
     C     *INKG         IFEQ      '1'
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
     C                   Z-ADD     GR(I1)        QQFND             3 0
     C                   Z-ADD     YR(I1)        QQFYR             3 0
     C                   MOVE      CS(I1)        QQCASE            7 0
     C                   MOVE      EO(I1)        QQEOB#
 ??? C****************************************************************
     C***   POSSIBLY NEED TO CHANGE F0515R TO ACCEPT RCN #
     C***                  MOVE RC,I1     QQRCN   30
     C****************************************************************
     C                   MOVE      *ZEROS        QQDATE            8 0
     C                   CALL      'F0515R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQEOB#
     C                   PARM                    QQDATE
     C                   PARM                    QQRTCD            1
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
     C                   END                                                    ENDIF *INKE
     C*
     C*****************************************************************
     C*   F11= TOGGLE BETWEEN SYS AND # OF BILLS                      *
     C*****************************************************************
     C     *INKK         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C*
     C     *IN79         IFEQ      '0'
     C                   MOVE      '1'           *IN79
     C                   ELSE
     C                   MOVE      '0'           *IN79
     C                   END
     C                   GOTO      STRT
     C*
     C                   END                                                    END IF *INKK
     C*
     C***********************************
     C*   F12 = PREVIOUS                *
     C***********************************
     C     *INKL         IFEQ      '1'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     *ZERO         SCFUND
     C                   Z-ADD     *ZERO         SCYEAR
     C                   Z-ADD     *ZERO         SCCASE
     C                   GOTO      STRT
     C                   END                                                    END IF *INKL
     C*****************************************************************
     C*   F15 = TIME/MATERIALS ENTRY                                  *
     C*****************************************************************
     C     *INKP         IFEQ      '1'
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
     C                   Z-ADD     YR(I1)        XXFYR
     C*
     C*
     C                   CALL      'F3040R'
     C                   PARM      ' '           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      RRN(I1)       ##RRN2            9 0
     C                   PARM      *ZEROS        ##RRN3            9 0
     C                   PARM      CRT(I1)       ##CERT           15
     C                   PARM      EXT(I1)       ##EXT             3 0
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   PARM      *BLANKS       ##EOB             1
     C                   PARM      *ZEROS        ##RCN             3 0
     C*** END   ADD *** 05/19/00 **************************************
     C*
     C                   END                                                    ENDIF GE/LE
     C*
     C                   Z-ADD     FUND1         XXFND             3 0
     C                   Z-ADD     YEAR1         XXFYR             3 0
     C                   Z-ADD     CASE1         XXCASE            7 0
     C                   Z-ADD     EOB#1         XZEOB#
     C                   Z-ADD     RCN#1         XXRCN#
     C*
     C     EAKEY1        CHAIN     PEA01                              60
     C                   EXSR      LOAD
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDIF SCR01
     C                   END                                                    ENDIF *INKP
     C*
     C*********************************
     C*   ROLL UP REQUEST             *
     C*********************************
     C     Q2RECF        IFEQ      'SCR01'
     C     *IN18         CABEQ     '1'           SCR01T
     C                   END
     C*********************************
     C*   ROLL DOWN REQUEST           *
     C*********************************
     C     Q2RECF        IFEQ      'SCR01'
     C     *IN19         CABEQ     '1'           SCR01T
     C                   END
     C*********************************
     C*   RELEASE ALL ENTRIES ***     *
     C*********************************
     C     *INKU         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C***********************************************
     C** RELEASE ALL EOB RECORDS                   **
     C***********************************************
     C*
     C*  RELEASE EOB RECORDS
     C*
     C     EAKEY         CHAIN     PEA01                              49
     C     *IN49         DOWEQ     '0'
     C     EASYS         IFNE      'N'
     C                   MOVE      'R'           EABHRF
     C                   ENDIF
     C*
     C                   UPDATE    PEA01
     C     EAKEY         READE     PEA01                                  49
     C                   ENDDO
     C*
     C     EAKEY         CHAIN     PEA02                              49
     C     *IN49         DOWEQ     '0'
     C     EASYS         IFNE      'N'
     C                   MOVE      'R'           EABHRF
     C                   ENDIF
     C*
     C                   UPDATE    PEA02
     C     EAKEY         READE     PEA02                                  49
     C                   ENDDO
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     *ZEROS        SCFYR
     C                   Z-ADD     *ZEROS        SCCASE
     C                   GOTO      TOP
     C                   END
     C*****************************
     C*   HOLD ALL ENTRIES ***    *
     C*****************************
     C     *INKV         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C***********************************************
     C*  HOLD ALL EOB RECORDS                      **
     C***********************************************
     C*
     C*  HOLD ALL EOB RECORDS
     C*
     C     EAKEY         CHAIN     PEA01                              49
     C     *IN49         DOWEQ     '0'
     C                   MOVE      'H'           EABHRF
     C                   UPDATE    PEA01
     C     EAKEY         READE     PEA01                                  49
     C                   ENDDO
     C*
     C     EAKEY         CHAIN     PEA02                              49
     C     *IN49         DOWEQ     '0'
     C                   MOVE      'H'           EABHRF
     C                   UPDATE    PEA02
     C     EAKEY         READE     PEA02                                  49
     C                   ENDDO
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     *ZERO         SCFUND
     C                   Z-ADD     *ZERO         SCYEAR
     C                   Z-ADD     *ZERO         SCCASE
     C                   GOTO      TOP
     C                   END
     C****************************************
     C*   PROCESS CHANGE OF USER PARAMETERS  *
     C****************************************
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END                                                    ENDIF *INKM
     C*
     C                   GOTO      STRT
     C                   END                                                    END CMDKEYS
     C*****************************************************************
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C*
     C*****************************************************************
     C*         SCR01T - SCREEN 1 - PROCESS REQUEST SCREEN            *
     C*****************************************************************
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
     C*  VALIDATE THE HOLD FIELD ENTRIES
     C*
     C                   DO        14            V                 2 0
     C     V             ADD       60            X1                3 0
     C     V             ADD       80            X2                3 0
     C     *IN(X2)       IFEQ      '1'
     C     FL(V)         IFNE      'R'
     C     FL(V)         ANDNE     'H'
     C                   MOVE      '1'           *IN22
     C                   MOVE      '1'           *IN(X1)
     C                   END
     C                   END
     C                   END
     C     *IN22         CABEQ     '1'           STRT
     C*
     C*
     C*  VALIDATE THE NUMBER OF BILLS FOR PBR RECORDS
     C*
     C*
     C* NOTE --- THERE ARE NOT 14 CONSECUTIVE *IN AVAILABLE TO HIGHLITE
     C*          THE # OF BILLS FIELD(S) IN ERROR, SO I HAD TO USE *IN98
     C*          TO HIGHLITE ALL 14 FIELDS AND CONDITION THE ERROR MSG ...
     C*
     C                   DO        14            Y                 2 0
     C*
     C     SY(Y)         IFEQ      'P'
     C*
     C     BL(Y)         IFLT      1
     C     BL(Y)         ORGT      999
 >>> C**********           MOVE '1'       *IN98
     C                   END                                                    SCBL 1-999
     C*
     C                   END                                                    SCSYS IFEQ P
     C*
     C                   END                                                    END - DO 14
     C*
     C*  CHECK TO SEE IF THERE IS AN ERROR IN THE NUMBER OF BILLS.
     C*  IF THERE IS AND THE SCREEN IS DISPLAYING THE SYS CODES THEN
     C*  SWITCH THE SCREEN TO VIEW THE NUMBER OF BILL TO SHOW THE
     C*  ERROR.
     C*
     C     *IN98         IFEQ      '1'
     C     *IN79         ANDEQ     '1'
     C                   MOVE      '0'           *IN79
     C                   END
     C*
     C     *IN98         CABEQ     '1'           STRT
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   EXSR      RITE
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     FUND14        XXFND
     C                   Z-ADD     YEAR14        XXFYR
     C                   Z-ADD     CASE14        XXCASE
     C                   Z-ADD     EOB#14        XZEOB#
     C                   Z-ADD     RCN#14        XZRCN#
     C*
     C     EAKEY1        CHAIN     PEA01                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   EXSR      RITE
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     FUND1         XXFND             3 0
     C                   Z-ADD     YEAR1         XXFYR             3 0
     C                   Z-ADD     CASE1         XXCASE            7 0
     C                   Z-ADD     EOB#1         XZEOB#
     C                   Z-ADD     RCN#1         XZRCN#
     C*
     C     EAKEY1        CHAIN     PEA01                              60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCFUND        IFNE      *ZERO
     C     SCYEAR        ORNE      *ZERO
     C     SCCASE        ORNE      *ZERO
     C                   MOVE      *BLANK        KEY
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C*
     C                   Z-ADD     SCFUND        XXFND
     C                   Z-ADD     SCYEAR        XXFYR
     C                   Z-ADD     SCCASE        XXCASE
     C                   Z-ADD     *ZEROS        XZEOB#
     C                   Z-ADD     *ZEROS        XZRCN#
     C                   Z-ADD     *ZERO         SCFUND
     C                   Z-ADD     *ZERO         SCYEAR
     C                   Z-ADD     *ZERO         SCCASE
     C*
     C     EAKEY1        SETLL     PEA01
     C     EAKEY         READE     PEA01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C                   EXSR      RITE
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     *ZERO         SCFUND
     C                   MOVE      *ZEROS        SCYEAR
     C                   MOVE      *ZEROS        SCCASE
     C                   GOTO      STRT
     C****************************************************************
     C*           SCR02T - SCREEN 2 - DETAIL INQUIRY                 *
     C****************************************************************
     C     SCR02T        TAG
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     *ZERO         SCFUND
     C                   MOVE      *ZEROS        SCCASE
     C                   MOVE      *ZEROS        SCYEAR
     C                   GOTO      STRT
     C****************************************************************
     C*           SCR03T - SCREEN 3 - HBALOCK MESSAGE                *
     C****************************************************************
     C     SCR03T        TAG
     C                   GOTO      TERM
     C****************************************************************
     C*                       END THE PROGRAM                        *
     C****************************************************************
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C/EJECT
     C****************************************************************
     C*             CLR SUBROUTINE - CLEAR SCREEN ARRAYS             *
     C****************************************************************
     CSR   CLR           BEGSR
     C                   MOVE      *BLANKS       FL
     C                   Z-ADD     *ZEROS        EO
     C                   Z-ADD     *ZEROS        RC
     C                   MOVE      *BLANKS       SY
     C                   Z-ADD     *ZERO         CS
     C                   Z-ADD     *ZERO         YR
     C                   Z-ADD     *ZERO         GR
     C                   Z-ADD     *ZERO         CA
     C                   Z-ADD     *ZERO         SA
     C                   Z-ADD     *ZERO         ND
     C                   Z-ADD     *ZERO         IA
     C                   Z-ADD     *ZERO         BL
     C                   MOVE      *BLANKS       TM
     C                   MOVE      *BLANKS       CT
     C                   Z-ADD     *ZERO         RRN
     C                   MOVE      *BLANKS       CRT
     C                   Z-ADD     *ZERO         EXT
     C*
     C                   Z-ADD     *ZERO         I1                3 0
     C                   MOVEA     '00000000'    *IN(31)
     C                   MOVEA     '000000'      *IN(39)
     C                   MOVEA     '00000000'    *IN(81)
     C                   MOVEA     '000000'      *IN(89)
     C                   Z-ADD     *ZERO         Y
     C*
     CSR                 ENDSR
     C/EJECT
     C****************************************************************
     C*  PRIMARY FILL SUBROUTINE - FILLS ALL SYSTEM TYPES
     C****************************************************************
     CSR   FILL1         BEGSR
     C*
     C                   MOVE      EAFND         XXFND
     C                   MOVE      EAFYR         XXFYR
     C                   MOVE      EACASE        XXCASE
     C*
     C                   MOVE      '0'           *IN75
     C*
     C     EASYS         IFEQ      'P'
     C     EABAMT        ANDNE     *ZERO
     C                   Z-ADD     EAEOB#        XXEOB#
     C                   Z-ADD     EARCN         XXRCN#
     C                   ADD       INCR          I1
     C                   MOVE      '1'           *IN75
     C                   EXSR      FILLP
     C                   END
     C*
     C     EASYS         IFEQ      'U'
     C     EABAMT        ANDNE     *ZERO
     C                   Z-ADD     EAEOB#        XXEOB#
     C                   Z-ADD     EARCN         XXRCN#
     C                   ADD       INCR          I1
     C                   MOVE      '1'           *IN75
     C                   EXSR      FILLU
     C                   END
     C*
     C*** BEGIN CHG *** 08/15/00 **************************************
     C********** XXMFLG    IFEQ 'Y'
     C     ##MED         IFEQ      'Y'
     C*** END   CHG *** 08/15/00 **************************************
     C*
     C     EASYS         IFEQ      'M'
     C     EABAMT        ANDNE     *ZERO
     C                   Z-ADD     EAEOB#        XXEOB#
     C                   Z-ADD     EARCN         XXRCN#
     C                   ADD       INCR          I1
     C                   MOVE      '1'           *IN75
     C                   EXSR      FILLM
     C                   ENDIF
     C                   ENDIF
     C*
     C     EASYS         IFEQ      'H'
     C     EABAMT        ANDNE     *ZERO
     C                   Z-ADD     EAEOB#        XXEOB#
     C                   Z-ADD     EARCN         XXRCN#
     C                   ADD       INCR          I1
     C                   MOVE      '1'           *IN75
     C                   EXSR      FILLH
     C                   END
     C*
     C     EASYS         IFEQ      'N'
     C     EABAMT        ANDNE     *ZERO
     C                   Z-ADD     EAEOB#        XXEOB#
     C                   Z-ADD     EARCN         XXRCN#
     C                   ADD       INCR          I1
     C                   MOVE      '1'           *IN75
     C                   EXSR      FILLN
     C                   END
     C*
     C     I1            ADD       80            X1
     C                   MOVE      '1'           *IN(X1)
     CSR                 ENDSR
     C****************************************************************
     C*            NETWORK FILL SUBROUTINE - FILLS SCREEN ARRAYS FOR *
     C*            NETWORK SYSTEM TYPES                              *
     C****************************************************************
     CSR   FILLN         BEGSR
     C                   MOVE      'N'           SY(I1)
     C                   MOVE      EACASE        CS(I1)
     C                   MOVE      EAFYR         YR(I1)
     C                   MOVE      EAFND         GR(I1)
     C                   MOVE      EAEOB#        EO(I1)
     C                   MOVE      EARCN         RC(I1)
     C                   MOVE      EACHRG        CA(I1)
     C     EACHRG        SUB       EAPAID        SA(I1)
     C                   MOVE      *ZEROS        BL(I1)
     C                   MOVE      EANDIS        ND(I1)
     C                   MOVE      *BLANKS       CT(I1)
     C                   Z-ADD     EAEOB#        XXEOB#
     C                   Z-ADD     EARCN         XXRCN#
     C*
     C                   Z-ADD     EAFND         KKFND
     C                   Z-ADD     EAFYR         KKFYR
     C                   Z-ADD     EACASE        KKCASE
     C                   Z-ADD     EAEOB#        KKEOB#
     C                   Z-ADD     EARCN         KKRCN#
     C*
     C     EAKEY2        CHAIN(N)  PEA02                              99
     C*
     C     EOKY2         CHAIN(N)  PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN         RRN(I1)
     C                   MOVEL(P)  EOCERT        CRT(I1)
     C                   Z-ADD     EOEXT         EXT(I1)
     C                   ENDIF
     C                   MOVE      EANHRF        FL(I1)
     C                   MOVE      EANAMT        IA(I1)
     C     TMKY1         CHAIN     PTM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '*'           TM(I1)
     C                   ELSE
     C                   MOVE      *BLANKS       TM(I1)
     C                   END
     C                   ENDSR
     C/EJECT
     C****************************************************************
     C*            PBR FILL SUBROUTINE - FILLS SCREEN ARRAYS FOR     *
     C*            PBR SYSTEM TYPES                                  *
     C****************************************************************
     CSR   FILLP         BEGSR
     C                   MOVE      'P'           SY(I1)
     C                   MOVE      EACASE        CS(I1)
     C                   MOVE      EAFYR         YR(I1)
     C                   MOVE      EAFND         GR(I1)
     C                   MOVE      EAEOB#        EO(I1)
     C                   MOVE      EARCN         RC(I1)
     C                   MOVE      EACHRG        CA(I1)
     C     EACHRG        SUB       EAPAID        SA(I1)
 ??? C*********            MOVE EO#BLL    BL,I1
     C                   MOVE      EANDIS        ND(I1)
     C                   MOVE      EABHRF        FL(I1)
     C                   MOVE      EABAMT        IA(I1)
     C                   MOVE      *BLANKS       CT(I1)
     C*
     C                   Z-ADD     EAFND         KKFND
     C                   Z-ADD     EAFYR         KKFYR
     C                   Z-ADD     EACASE        KKCASE
     C                   Z-ADD     EAEOB#        KKEOB#
     C                   Z-ADD     EARCN         KKRCN#
     C*
     C     EAKEY2        CHAIN(N)  PEA02                              99
     C*
     C     EOKY2         CHAIN(N)  PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN         RRN(I1)
     C                   MOVEL(P)  EOCERT        CRT(I1)
     C                   Z-ADD     EOEXT         EXT(I1)
     C                   ENDIF
     C     TMKY1         CHAIN     PTM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '*'           TM(I1)
     C                   ELSE
     C                   MOVE      *BLANKS       TM(I1)
     C                   END
     C                   ENDSR
     C/EJECT
     C****************************************************************
     C*    PRE/RE-CERT FILL SUBROUTINE - FILLS SCREEN ARRAYS FOR     *
     C*              U SYSTEM TYPES                                  *
     C****************************************************************
     CSR   FILLU         BEGSR
     C                   MOVE      'U'           SY(I1)
     C                   MOVE      EACASE        CS(I1)
     C                   MOVE      EAFYR         YR(I1)
     C                   MOVE      EAFND         GR(I1)
     C                   MOVE      EAEOB#        EO(I1)
     C                   MOVE      EARCN         RC(I1)
     C                   MOVE      EACHRG        CA(I1)
     C     EACHRG        SUB       EAPAID        SA(I1)
     C                   MOVE      *ZEROS        BL(I1)
     C                   MOVE      EANDIS        ND(I1)
     C                   MOVE      EABHRF        FL(I1)
     C                   Z-ADD     EAEOB#        XXEOB#
     C                   Z-ADD     EARCN         XXRCN#
     C*
     C                   Z-ADD     EAFND         KKFND
     C                   Z-ADD     EAFYR         KKFYR
     C                   Z-ADD     EACASE        KKCASE
     C                   Z-ADD     EAEOB#        KKEOB#
     C                   Z-ADD     EARCN         KKRCN#
     C*
     C     EAKEY2        CHAIN(N)  PEA02                              99
     C*
     C     EOKY2         CHAIN(N)  PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN         RRN(I1)
     C                   MOVEL     EOCERT        CT(I1)
     C                   MOVEL(P)  EOCERT        CRT(I1)
     C                   Z-ADD     EOEXT         EXT(I1)
     C                   ENDIF
     C*
     C                   MOVE      EABAMT        IA(I1)
     C*
     C     TMKY1         CHAIN     PTM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '*'           TM(I1)
     C                   ELSE
     C                   MOVE      *BLANKS       TM(I1)
     C                   END
     C                   ENDSR
     C/EJECT
     C****************************************************************
     C*    MEDICAL MANGEMENT SUBROUTINE - FILLS SCREEN ARRAYS FOR    *
     C*              M SYSTEM TYPES                                  *
     C****************************************************************
     CSR   FILLM         BEGSR
     C                   MOVE      'M'           SY(I1)
     C                   MOVE      EACASE        CS(I1)
     C                   MOVE      EAFYR         YR(I1)
     C                   MOVE      EAFND         GR(I1)
     C                   MOVE      EAEOB#        EO(I1)
     C                   MOVE      EARCN         RC(I1)
     C                   MOVE      EACHRG        CA(I1)
     C     EACHRG        SUB       EAPAID        SA(I1)
     C                   MOVE      *ZEROS        BL(I1)
     C                   MOVE      EANDIS        ND(I1)
     C                   MOVE      EABHRF        FL(I1)
     C                   MOVE      *BLANKS       CT(I1)
     C                   Z-ADD     EAEOB#        XXEOB#
     C                   Z-ADD     EARCN         XXRCN#
     C*
     C                   Z-ADD     EAFND         KKFND
     C                   Z-ADD     EAFYR         KKFYR
     C                   Z-ADD     EACASE        KKCASE
     C                   Z-ADD     EAEOB#        KKEOB#
     C                   Z-ADD     EARCN         KKRCN#
     C*
     C     EAKEY2        CHAIN(N)  PEA02                              99
     C*
     C     EOKY2         CHAIN(N)  PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN         RRN(I1)
     C                   MOVEL     EOCERT        CT(I1)
     C                   MOVEL(P)  EOCERT        CRT(I1)
     C                   Z-ADD     EOEXT         EXT(I1)
     C                   ENDIF
     C                   MOVE      EABAMT        IA(I1)
     C     TMKY1         CHAIN     PTM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '*'           TM(I1)
     C                   ELSE
     C                   MOVE      *BLANKS       TM(I1)
     C                   END
     C                   ENDSR
     C/EJECT
     C****************************************************************
     C*            HBA FILL SUBROUTINE - FILLS SCREEN ARRAYS FOR     *
     C*            HBA SYSTEM TYPES                                  *
     C****************************************************************
     CSR   FILLH         BEGSR
     C                   MOVE      'H'           SY(I1)
     C                   MOVE      EACASE        CS(I1)
     C                   MOVE      EAFYR         YR(I1)
     C                   MOVE      EAFND         GR(I1)
     C                   MOVE      EAEOB#        EO(I1)
     C                   MOVE      EARCN         RC(I1)
     C                   MOVE      EACHRG        CA(I1)
     C     EACHRG        SUB       EAPAID        SA(I1)
     C                   MOVE      *ZEROS        BL(I1)
     C                   MOVE      EANDIS        ND(I1)
     C                   MOVE      EABHRF        FL(I1)
     C                   MOVE      *BLANKS       CT(I1)
     C                   Z-ADD     EAEOB#        XXEOB#
     C                   Z-ADD     EARCN         XXRCN#
     C*
     C                   Z-ADD     EAFND         KKFND
     C                   Z-ADD     EAFYR         KKFYR
     C                   Z-ADD     EACASE        KKCASE
     C                   Z-ADD     EAEOB#        KKEOB#
     C                   Z-ADD     EARCN         KKRCN#
     C*
     C     EAKEY2        CHAIN(N)  PEA02                              99
     C*
     C     EOKY2         CHAIN(N)  PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN         RRN(I1)
     C                   MOVEL     EOCERT        CT(I1)
     C                   MOVEL(P)  EOCERT        CRT(I1)
     C                   Z-ADD     EOEXT         EXT(I1)
     C                   ENDIF
     C                   MOVE      EABAMT        IA(I1)
     C     TMKY1         CHAIN     PTM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '*'           TM(I1)
     C                   ELSE
     C                   MOVE      *BLANKS       TM(I1)
     C                   END
     C                   ENDSR
     C/EJECT
     C****************************************************************
     C*            LOAD SUBROUTINE - LOAD FORWARD SUBROUTINE         *
     C****************************************************************
     CSR   LOAD          BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     1             INCR              3 0
     C     OVER          TAG
     C*
     C*  END OF FILE
     C*
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     I1            COMP      14                                     61
     C   61              GOTO      DONE
     C                   EXSR      FILL1
     C     EAKEY         READE     PEA01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C                   GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     GR(1)         FUND1             3 0
     C                   Z-ADD     YR(1)         YEAR1             2 0
     C                   Z-ADD     CS(1)         CASE1             7 0
     C                   Z-ADD     EO(1)         EOB#1
     C                   Z-ADD     RC(1)         RCN#1
     C*
     C                   Z-ADD     GR(14)        FUND14            3 0
     C                   Z-ADD     YR(14)        YEAR14            2 0
     C                   Z-ADD     CS(14)        CASE14            7 0
     C                   Z-ADD     EO(14)        EOB#14
     C                   Z-ADD     RC(14)        RCN#14
     CSR                 ENDSR
     C/EJECT
     C****************************************************************
     C*           LOADP SUBROUTINE - LOAD BACKWARD SUBROUTINE        *
     C****************************************************************
     CSR   LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     -1            INCR
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
     C     I1            COMP      1                                      61
     C   61              GOTO      DONEP
     C                   EXSR      FILL1
     C                   READP     PEA01                                  60
     C  N60EACO#         COMP      UPCO#                              6060
     C  N60EASUB#        COMP      UPSUB#                             6060
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C                   GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     GR(1)         FUND1
     C                   Z-ADD     YR(1)         YEAR1
     C                   Z-ADD     CS(1)         CASE1
     C                   Z-ADD     EO(1)         EOB#1
     C                   Z-ADD     RC(1)         RCN#1
     C*
     C                   Z-ADD     GR(14)        FUND14
     C                   Z-ADD     YR(14)        YEAR14
     C                   Z-ADD     CS(14)        CASE14
     C                   Z-ADD     EO(14)        EOB#14
     C                   Z-ADD     RC(14)        RCN#14
     CSR                 ENDSR
     C/EJECT
     C****************************************************************
     C*        LOADB SUBROUTINE - LOAD FROM BEGINNING SUBROUTINE     *
     C****************************************************************
     CSR   LOADB         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     1             INCR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C     EAKEY         SETLL     PEA01
     C     EAKEY         READE     PEA01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C     I1            COMP      14                                     61
     C   61              GOTO      DONEB
     C                   EXSR      FILL1
     C     EAKEY         READE     PEA01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C                   GOTO      OVERB
     CSR   DONEB         ENDSR
     C/EJECT
     C****************************************************************
     C*      RITE SUBROUTINE - UPDATE THE HOLD/RELEASE FIELD,        *
     C*                        ACTUAL AMOUNT AND/OR # OF BILLS       *
     C****************************************************************
     CSR   RITE          BEGSR
     C*
     C                   DO        14            V
     C     EO(V)         IFGT      *ZERO
     C                   Z-ADD     GR(V)         XXFND
     C                   Z-ADD     YR(V)         XXFYR
     C                   Z-ADD     CS(V)         XXCASE
     C                   Z-ADD     EO(V)         XZEOB#
     C                   Z-ADD     RC(V)         XZRCN#
     C*
     C*  UPDATE EOB FILE
     C*
     C     SY(V)         IFEQ      'P'
     C                   Z-ADD     EAEOB#        XXEOB#
     C                   Z-ADD     EARCN         XXRCN#
     C     EAKEY1        CHAIN     PEA01                              49
     C     *IN49         IFEQ      '0'
     C                   MOVEL     FL(V)         EABHRF
     C                   Z-ADD     IA(V)         EABAMT
 ??? C************         Z-ADDBL,V      EO#BLL
     C                   UPDATE    PEA01
     C                   END
     C     EAKEY1        CHAIN     PEA02                              49
     C     *IN49         IFEQ      '0'
     C     FL(V)         IFEQ      EABHRF
     C     IA(V)         ANDEQ     EABAMT
 ??? C***********BL,V      ANDEQEO#BLL
     C                   ELSE
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   Z-ADD     DSTIME        EATIME
     C                   END
     C                   MOVEL     FL(V)         EABHRF
     C                   Z-ADD     IA(V)         EABAMT
 ??? C*************        Z-ADDBL,V      EO#BLL
     C                   UPDATE    PEA02
     C                   END
     C                   END
     C*
     C*  UPDATE PRE/RE-CERTIFICATION RECORD
     C*
     C     SY(V)         IFEQ      'U'
     C                   Z-ADD     EAEOB#        XXEOB#
     C                   Z-ADD     EARCN         XXRCN#
     C     EAKEY1        CHAIN     PEA01                              49
     C     *IN49         IFEQ      '0'
     C                   MOVEL     FL(V)         EABHRF
     C                   UPDATE    PEA01
     C                   END
     C     EAKEY1        CHAIN     PEA02                              49
     C     *IN49         IFEQ      '0'
     C     FL(V)         IFNE      EABHRF
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   Z-ADD     DSTIME        EATIME
     C                   END
     C                   MOVEL     FL(V)         EABHRF
     C                   Z-ADD     IA(V)         EABAMT
     C                   UPDATE    PEA02
     C                   END
     C                   END
     C*
     C*  UPDATE MEDICAL MANAGEMENT RECORD
     C*
     C*** BEGIN CHG *** 08/15/00 **************************************
     C********** XXMFLG    IFEQ 'Y'
     C     ##MED         IFEQ      'Y'
     C*** END   CHG *** 08/15/00 **************************************
     C*
     C     SY(V)         IFEQ      'M'
     C                   Z-ADD     EAEOB#        XXEOB#
     C                   Z-ADD     EARCN         XXRCN#
     C     EAKEY1        CHAIN     PEA01                              49
     C     *IN49         IFEQ      '0'
     C                   MOVEL     FL(V)         EABHRF
     C                   Z-ADD     IA(V)         EABAMT
     C                   UPDATE    PEA01
     C                   END
     C     EAKEY1        CHAIN     PEA02                              49
     C     *IN49         IFEQ      '0'
     C     FL(V)         IFNE      EABHRF
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   Z-ADD     DSTIME        EATIME
     C                   END
     C                   MOVEL     FL(V)         EABHRF
     C                   Z-ADD     IA(V)         EABAMT
     C                   UPDATE    PEA02
     C                   END
     C                   END
     C*
     C                   END
     C*
     C*  UPDATE HBA RECORD
     C*
     C     SY(V)         IFEQ      'H'
     C                   Z-ADD     EAEOB#        XXEOB#
     C                   Z-ADD     EARCN         XXRCN#
     C*
     C     EAKEY1        CHAIN     PEA01                              49
     C     *IN49         IFEQ      '0'
     C                   MOVEL     FL(V)         EABHRF
     C                   UPDATE    PEA01
     C                   END
     C     EAKEY1        CHAIN     PEA02                              49
     C     *IN49         IFEQ      '0'
     C     FL(V)         IFNE      EABHRF
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   Z-ADD     DSTIME        EATIME
     C                   END
     C                   MOVEL     FL(V)         EABHRF
     C                   Z-ADD     IA(V)         EABAMT
     C                   UPDATE    PEA02
     C                   END
     C                   END
     C*
     C*  UPDATE NETWORK TRANSACTION RECORD
     C*
     C*
     C     SY(V)         IFEQ      'N'
     C                   Z-ADD     EAEOB#        XXEOB#
     C                   Z-ADD     EARCN         XXRCN#
     C*
     C     EAKEY1        CHAIN     PEA01                              49
     C     *IN49         IFEQ      '0'
     C                   MOVEL     FL(V)         EANHRF
     C                   UPDATE    PEA01
     C                   END
     C     EAKEY1        CHAIN     PEA02                              49
     C     *IN49         IFEQ      '0'
     C     FL(V)         IFNE      EANHRF
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   Z-ADD     DSTIME        EATIME
     C                   END
     C                   MOVEL     FL(V)         EANHRF
     C                   Z-ADD     IA(V)         EANAMT
     C                   UPDATE    PEA02
     C                   END
     C                   END
     C                   END                                                    EOB# <> 0
     C*
     C                   END                                                    DO 14 V
     C*
     CSR                 ENDSR
     C/EJECT
     C*
     O**  PTL - PROGRAM TITLES (PASSED IN Q1PRDS)
     O**        TIME/MATERIALS ENTRY
     O**    TIME/MATERIALS MAINTENANCE
**
ROLL UP
ROLL DOWN
