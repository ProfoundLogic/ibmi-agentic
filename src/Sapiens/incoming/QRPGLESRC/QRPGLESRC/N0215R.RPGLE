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
     H*    PROGRAM: N0215R                                           **
     H*    DESCRIPTION: PBR/HBA RELEASE/HOLD - SCREEN PROGRAM      - **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H*  REVISIONS:                                                  **
     H*                                                               *
     H*  06/09/20  C:212 Project : SY420                             **
     H*                  Mask SSN                                    **
     H*                  Recompile only                              **
     H*                                                              **
     H*  05/19/00 C:62  PROJECT: MC172                               **
     H*                 ADDED ADDITIONAL PARM TO CALL TO F3040R.     **
     H*                                                              **
     H*  09/23/99 C:62 PROJECT: MC212                                **
     H*                EXPAND FYR TO 3.0P                            **
     H*                                                              **
     H*  02/16/99  C:62 PROJECT: MC145                               **
     H*                 CHANGE CALL TO F3040R FROM DISPLAY TO MAINT. **
     H*                                                              **
     H*  07/05/95  C:35 REQ/PROJ: NW008                              **
     H*                 1. ADDED M-MEDICAL MANAGEMENT BILLING FILE   **
     H*                 2. CONDITION DISPLAY OF SYSTEM TYPE M RECORDS**
     H*                    ON POS. 9 OF FBAL01 EQ 'Y'.               **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H*  INDICATOR USAGE:                                            **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     FN0215F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFAHBL3    UF   E           K DISK
     FHAEOL     UF   E           K DISK
     FFAEOL4    UF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     F*** BEGIN ADD *** 08/26/98 **************************************
     F                                     INFDS(DBFDS)
     F*** END   ADD *** 08/26/98 **************************************
     FFMSML     IF   E           K DISK
     FNATRL     UF   E           K DISK
     FNAPBL1    UF   E           K DISK
     FNDTML     IF   E           K DISK
     F*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     FNAMBL1    UF   E           K DISK
     FSSYLF     IF   E           K DISK
     F*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     F*

     D*** BEGIN CHG *** 09/23/99 **************************************
     D**********          YR         14  2 0             CASE YEAR
     D*** END   CHG *** 09/23/99 **************************************
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEY             S              9  0 DIM(2)                               EOB#
     D*** BEGIN ADD *** 08/26/98 **************************************
     D*** END   ADD *** 08/26/98 **************************************
     D*** BEGIN DEL *** 08/26/98 **************************************
     D**********          PTL     1   2 40               PGM TITLES
     D*** END   DEL *** 08/26/98 **************************************
     D*** BEGIN ADD *** 08/26/98 **************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*** END   ADD *** 08/26/98 **************************************
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
     D  CA                     1    126  2
     D                                     DIM(14)                              AMT CHRGD
     D  SCCA01                 1      9  2
     D  SCCA02                10     18  2
     D  SCCA03                19     27  2
     D  SCCA04                28     36  2
     D  SCCA05                37     45  2
     D  SCCA06                46     54  2
     D  SCCA07                55     63  2
     D  SCCA08                64     72  2
     D  SCCA09                73     81  2
     D  SCCA10                82     90  2
     D  SCCA11                91     99  2
     D  SCCA12               100    108  2
     D  SCCA13               109    117  2
     D  SCCA14               118    126  2
     D                 DS
     D  SA                     1    126  2
     D                                     DIM(14)                              REDUCTIONS
     D  SCSA01                 1      9  2
     D  SCSA02                10     18  2
     D  SCSA03                19     27  2
     D  SCSA04                28     36  2
     D  SCSA05                37     45  2
     D  SCSA06                46     54  2
     D  SCSA07                55     63  2
     D  SCSA08                64     72  2
     D  SCSA09                73     81  2
     D  SCSA10                82     90  2
     D  SCSA11                91     99  2
     D  SCSA12               100    108  2
     D  SCSA13               109    117  2
     D  SCSA14               118    126  2
     D                 DS
     D  ND                     1    126  2
     D                                     DIM(14)                              NETWORK DISC
     D  SCND01                 1      9  2
     D  SCND02                10     18  2
     D  SCND03                19     27  2
     D  SCND04                28     36  2
     D  SCND05                37     45  2
     D  SCND06                46     54  2
     D  SCND07                55     63  2
     D  SCND08                64     72  2
     D  SCND09                73     81  2
     D  SCND10                82     90  2
     D  SCND11                91     99  2
     D  SCND12               100    108  2
     D  SCND13               109    117  2
     D  SCND14               118    126  2
     D                 DS
     D  IA                     1    126  2
     D                                     DIM(14)                              ISMUS AMT
     D  SCIA01                 1      9  2
     D  SCIA02                10     18  2
     D  SCIA03                19     27  2
     D  SCIA04                28     36  2
     D  SCIA05                37     45  2
     D  SCIA06                46     54  2
     D  SCIA07                55     63  2
     D  SCIA08                64     72  2
     D  SCIA09                73     81  2
     D  SCIA10                82     90  2
     D  SCIA11                91     99  2
     D  SCIA12               100    108  2
     D  SCIA13               109    117  2
     D  SCIA14               118    126  2
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
     D*** BEGIN ADD *** 08/26/98 **************************************
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
     D*** END   ADD *** 08/26/98 **************************************
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
     C     EOKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C*
     C*   KEY LIST FOR THE EOB WORK FILE
     C*
     C     EOKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XZEOB#
     C*** BEGIN ADD *** 08/26/98 **************************************
     C*
     C*   KEY LIST FOR THE EOB FILE
     C*
     C     EOKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** END   ADD *** 08/26/98 **************************************
     C*
     C*   KEY LIST FOR THE HBA TRANS FILE
     C*
     C     HBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXEOB#
     C*
     C*   KEY LIST FOR THE HBA TRANS FILE
     C*
     C     HBKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    EO(V)
     C*
     C*   PARTIAL KEY LIST FOR THE HBA TRANS FILE
     C*
     C     HBKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C*
     C*   KEY LIST FOR THE NETWORK TRANSACTION FILE
     C*
     C     TRKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*   KEY LIST FOR THE NETWORK TRANSACTION FILE
     C*
     C     TRKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    EO(V)
     C*
     C*   PARTIAL KEY LIST FOR THE NETWORK TRANSACTION FILE
     C*
     C     TRKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C*
     C*   KEY LIST FOR THE PRE/RE-CERTIFICATION TRANSACTION FILE
     C*
     C     PBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*   KEY LIST FOR THE PRE/RE-CERTIFICATION TRANSACTION FILE
     C*
     C     PBKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    EO(V)
     C*
     C*   KEY LIST FOR THE PRE/RE-CERTIFICATION TRANSACTION FILE
     C*
     C     PBKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C*
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
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C* KEY LIST FOR THE MEDICAL MANAGEMENT BILLING FILE
     C*
     C     MBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C     MBKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    EO(V)
     C*
     C     MBKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C* FS BASE CONTROL FILE
     C*
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
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
     C     *LIKE         DEFINE    EOEOB#        EOB#1
     C     *LIKE         DEFINE    EOEOB#        EOB#14
     C     *LIKE         DEFINE    EOEOB#        QQEOB#
     C     *LIKE         DEFINE    EOEOB#        XZEOB#
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C*** BEGIN ADD *** 08/26/98 **************************************
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C*** END   ADD *** 08/26/98 **************************************
     C*
     C*   LOAD THE OUTPUT CONSTANTS FROM THE PARMS
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      '-'           XXHYP1            1
     C                   MOVE      '-'           XXHYP2            1
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     9             T                 5 0
     C     1             SUBST     FBAL01:T      XXMFLG            1
     C                   ELSE
     C                   MOVE      'N'           XXMFLG
     C                   END
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
     C     EOKEY         SETLL     PEO01
     C     EOKEY         READE     PEO01                                  60
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
     C*** BEGIN DEL *** 08/26/98 **************************************
     C**********           MOVE *BLANKS   Q1PRDS 40
     C**********           MOVELPTL,1     Q1PRDS
     C**********           MOVE UPCO#     Q1CO#   3
     C**********           MOVE UPSUB#    Q1SUB#  5
     C**********           MOVE GR,I1     Q1FND   3
     C**********           MOVE YR,I1     Q1FYR   3
     C**********           MOVE CS,I1     Q1CASE  7
     C**********           MOVE EO,I1     Q1EOB#  9
     C**********           MOVE SY,I1     Q1SYS   1
     C**********           CALL 'N0040C'
     C**********           PARM           Q1PRDS
     C**********           PARM '2'       Q1CALL  1
     C**********           PARM           Q1CO#
     C**********           PARM           Q1SUB#
     C**********           PARM           Q1FND
     C**********           PARM           Q1FYR
     C**********           PARM           Q1CASE
     C**********           PARM           Q1EOB#
     C**********           PARM           Q1SYS
     C*** END   DEL *** 08/26/98 **************************************
     C*** BEGIN ADD *** 08/26/98 **************************************
     C                   CALL      'F3040R'
     C*** BEGIN CHG *** 02/16/99 **************************************
     C**********           PARM '1'       ##CALL  1
     C                   PARM      ' '           ##CALL            1
     C*** END   CHG *** 02/16/99 **************************************
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      RRN(I1)       ##RRN2            9 0
     C                   PARM      *ZEROS        ##RRN3            9 0
     C                   PARM      CRT(I1)       ##CERT           15
     C                   PARM      EXT(I1)       ##EXT             3 0
     C*** END   ADD *** 08/26/98 **************************************
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
     C     EOKEY1        CHAIN     PEO01                              60
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
     C** RELEASE ALL HBA, NETWORK AND  EOB RECORDS  *
     C***********************************************
     C*  RELEASE HBA RECORDS
     C*
     C     HBKEY2        CHAIN     PHB01                              49
     C     *IN49         DOWEQ     '0'
     C                   MOVE      'R'           HBHRFL
     C*
     C*
     C                   UPDATE    PHB01
     C     HBKEY2        READE     PHB01                                  49
     C                   END
     C*
     C*  RELEASE NETWORK TRANSACTION RECORDS
     C*
     C     TRKEY2        CHAIN     PTR01                              49
     C     *IN49         DOWEQ     '0'
     C                   MOVE      'R'           TRHRFL
     C*
     C                   UPDATE    PTR01
     C     TRKEY2        READE     PTR01                                  49
     C                   END
     C*  RELEASE PRE/RE-CERTIFICATION RECORDS
     C*
     C     PBKEY2        CHAIN     PPB01                              49
     C     *IN49         DOWEQ     '0'
     C                   MOVE      'R'           PBHRFL
     C*
     C*
     C                   UPDATE    PPB01
     C     PBKEY2        READE     PPB01                                  49
     C                   END
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C* RELEASE MEDICAL MANAGEMENTN RECORDS
     C*
     C     XXMFLG        IFEQ      'Y'
     C*
     C     MBKEY2        CHAIN     PMB01                              49
     C     *IN49         DOWEQ     '0'
     C                   MOVE      'R'           MBHRFL
     C*
     C*
     C                   UPDATE    PMB01
     C     MBKEY2        READE     PMB01                                  49
     C                   END
     C*
     C                   END
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C*  RELEASE EOB RECORDS
     C*
     C     EOKEY         CHAIN     PEO01                              49
     C     *IN49         DOWEQ     '0'
     C     EOSYS         IFNE      'N'
     C                   MOVE      'R'           EOHRFL
     C                   END
     C*
     C                   UPDATE    PEO01
     C     EOKEY         READE     PEO01                                  49
     C                   END
     C*
     C     EOKEY         CHAIN     PEO02                              49
     C     *IN49         DOWEQ     '0'
     C     EOSYS         IFNE      'N'
     C                   MOVE      'R'           EOHRFL
     C                   END
     C*
     C                   UPDATE    PEO02
     C     EOKEY         READE     PEO02                                  49
     C                   END
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
     C*  HOLD ALL HBA RECORDS, THEN EOB RECORDS     *
     C***********************************************
     C*
     C*  HOLD ALL HBA RECORDS
     C*
     C     HBKEY2        CHAIN     PHB01                              49
     C     *IN49         DOWEQ     '0'
     C                   MOVE      'H'           HBHRFL
     C*
     C                   UPDATE    PHB01
     C     HBKEY2        READE     PHB01                                  49
     C                   END
     C*
     C*  HOLD ALL NETWORK TRANSACTION RECORDS
     C*
     C     TRKEY2        CHAIN     PTR01                              49
     C     *IN49         DOWEQ     '0'
     C                   MOVE      'H'           TRHRFL
     C*
     C                   UPDATE    PTR01
     C     TRKEY2        READE     PTR01                                  49
     C                   END
     C*
     C*  HOLD ALL PRE/RE-CERTIFICATION RECORDS
     C*
     C     PBKEY2        CHAIN     PPB01                              49
     C     *IN49         DOWEQ     '0'
     C                   MOVE      'H'           PBHRFL
     C*
     C                   UPDATE    PPB01
     C     PBKEY2        READE     PPB01                                  49
     C                   END
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C* HOLD ALL MEDICAL MANAGEMENT RECORDS
     C*
     C     XXMFLG        IFEQ      'Y'
     C*
     C     MBKEY2        CHAIN     PMB01                              49
     C     *IN49         DOWEQ     '0'
     C                   MOVE      'H'           MBHRFL
     C*
     C                   UPDATE    PMB01
     C     MBKEY2        READE     PMB01                                  49
     C                   END
     C*
     C                   END
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C*  HOLD ALL EOB RECORDS
     C*
     C     EOKEY         CHAIN     PEO01                              49
     C     *IN49         DOWEQ     '0'
     C                   MOVE      'H'           EOHRFL
     C                   UPDATE    PEO01
     C     EOKEY         READE     PEO01                                  49
     C                   END
     C*
     C     EOKEY         CHAIN     PEO02                              49
     C     *IN49         DOWEQ     '0'
     C                   MOVE      'H'           EOHRFL
     C*
     C                   UPDATE    PEO02
     C     EOKEY         READE     PEO02                                  49
     C                   END
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
     C                   MOVE      '1'           *IN98
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
     C     EOKEY1        CHAIN     PEO01                              60
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
     C     EOKEY1        CHAIN     PEO01                              60
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
     C                   Z-ADD     *ZERO         SCFUND
     C                   Z-ADD     *ZERO         SCYEAR
     C                   Z-ADD     *ZERO         SCCASE
     C     EOKEY1        SETLL     PEO01
     C     EOKEY         READE     PEO01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C                   EXSR      RITE
     C*
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
     C*** BEGIN ADD *** 08/26/98 **************************************
     C                   Z-ADD     *ZERO         RRN
     C                   MOVE      *BLANKS       CRT
     C                   Z-ADD     *ZERO         EXT
     C*** END   ADD *** 08/26/98 **************************************
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
     C                   MOVE      EOFND         XXFND
     C                   MOVE      EOFYR         XXFYR
     C                   MOVE      EOCASE        XXCASE
     C*
     C                   MOVE      '0'           *IN75
     C*
     C     EOSYS         IFEQ      'P'
     C     EOBAMT        ANDNE     *ZERO
     C                   Z-ADD     EOEOB#        XXEOB#
     C                   ADD       INCR          I1
     C                   MOVE      '1'           *IN75
     C                   EXSR      FILLP
     C                   END
     C*
     C     EOSYS         IFEQ      'U'
     C                   Z-ADD     EOEOB#        XXEOB#
     C     PBKEY         CHAIN     PPB01                              60
     C     *IN60         IFEQ      '0'
     C     PBBAMT        IFNE      *ZERO
     C                   ADD       INCR          I1
     C                   MOVE      '1'           *IN75
     C                   EXSR      FILLU
     C                   END
     C                   END
     C                   END
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C     XXMFLG        IFEQ      'Y'
     C*
     C     EOSYS         IFEQ      'M'
     C                   Z-ADD     EOEOB#        XXEOB#
     C     MBKEY         CHAIN     PMB01                              60
     C     *IN60         IFEQ      '0'
     C     MBBAMT        IFNE      *ZERO
     C                   ADD       INCR          I1
     C                   MOVE      '1'           *IN75
     C                   EXSR      FILLM
     C                   END
     C                   END
     C                   END
     C*
     C                   END
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C     EOSYS         IFEQ      'H'
     C                   Z-ADD     EOEOB#        XXEOB#
     C     HBKEY         CHAIN     PHB01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    RLSPHB
     C     HB1AAM        IFNE      *ZERO
     C                   ADD       INCR          I1
     C                   MOVE      '1'           *IN75
     C                   EXSR      FILLH
     C                   END
     C                   END
     C                   END
     C*
     C     EOSYS         IFEQ      'N'
     C                   Z-ADD     EOEOB#        XXEOB#
     C     TRKEY         CHAIN     PTR01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    RLSPTR
     C     TRBAMT        IFNE      *ZERO
     C                   ADD       INCR          I1
     C                   MOVE      '1'           *IN75
     C                   EXSR      FILLN
     C                   END
     C                   END
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
     C                   MOVE      EOCASE        CS(I1)
     C                   MOVE      EOFYR         YR(I1)
     C                   MOVE      EOFND         GR(I1)
     C                   MOVE      EOEOB#        EO(I1)
     C                   MOVE      EOAMTC        CA(I1)
     C     EOAMTC        SUB       EOAMT         SA(I1)
     C                   MOVE      *ZEROS        BL(I1)
     C                   MOVE      EONDIS        ND(I1)
     C                   MOVE      *BLANKS       CT(I1)
     C                   Z-ADD     EOEOB#        XXEOB#
     C*** BEGIN ADD *** 08/26/98 **************************************
     C*
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C*
     C     EOKEY2        CHAIN(N)  PEO02                              99
     C*
     C                   Z-ADD     DBRRN         RRN(I1)
     C                   MOVEL(P)  EOCERT        CRT(I1)
     C                   Z-ADD     EOEXT         EXT(I1)
     C*** END   ADD *** 08/26/98 **************************************
     C     TRKEY         CHAIN     PTR01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    RLSPTR
     C                   MOVE      TRHRFL        FL(I1)
     C                   MOVE      TRBAMT        IA(I1)
     C                   END
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
     C                   MOVE      EOCASE        CS(I1)
     C                   MOVE      EOFYR         YR(I1)
     C                   MOVE      EOFND         GR(I1)
     C                   MOVE      EOEOB#        EO(I1)
     C                   MOVE      EOAMTC        CA(I1)
     C     EOAMTC        SUB       EOAMT         SA(I1)
     C                   MOVE      EO#BLL        BL(I1)
     C                   MOVE      EONDIS        ND(I1)
     C                   MOVE      EOHRFL        FL(I1)
     C                   MOVE      EOBAMT        IA(I1)
     C                   MOVE      *BLANKS       CT(I1)
     C*** BEGIN ADD *** 08/26/98 **************************************
     C*
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C*
     C     EOKEY2        CHAIN(N)  PEO02                              99
     C*
     C                   Z-ADD     DBRRN         RRN(I1)
     C                   MOVEL(P)  EOCERT        CRT(I1)
     C                   Z-ADD     EOEXT         EXT(I1)
     C*** END   ADD *** 08/26/98 **************************************
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
     C                   MOVE      EOCASE        CS(I1)
     C                   MOVE      EOFYR         YR(I1)
     C                   MOVE      EOFND         GR(I1)
     C                   MOVE      EOEOB#        EO(I1)
     C                   MOVE      EOAMTC        CA(I1)
     C     EOAMTC        SUB       EOAMT         SA(I1)
     C                   MOVE      *ZEROS        BL(I1)
     C                   MOVE      EONDIS        ND(I1)
     C                   MOVE      EOHRFL        FL(I1)
     C                   MOVEL     EOCERT        CT(I1)
     C                   Z-ADD     EOEOB#        XXEOB#
     C*** BEGIN ADD *** 08/26/98 **************************************
     C*
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C*
     C     EOKEY2        CHAIN(N)  PEO02                              99
     C*
     C                   Z-ADD     DBRRN         RRN(I1)
     C                   MOVEL(P)  EOCERT        CRT(I1)
     C                   Z-ADD     EOEXT         EXT(I1)
     C*** END   ADD *** 08/26/98 **************************************
     C     PBKEY         CHAIN     PPB01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    RLSPPB
     C                   MOVE      PBBAMT        IA(I1)
     C                   END
     C     TMKY1         CHAIN     PTM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '*'           TM(I1)
     C                   ELSE
     C                   MOVE      *BLANKS       TM(I1)
     C                   END
     C                   ENDSR
     C/EJECT
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C****************************************************************
     C*    MEDICAL MANGEMENT SUBROUTINE - FILLS SCREEN ARRAYS FOR    *
     C*              M SYSTEM TYPES                                  *
     C****************************************************************
     CSR   FILLM         BEGSR
     C                   MOVE      'M'           SY(I1)
     C                   MOVE      EOCASE        CS(I1)
     C                   MOVE      EOFYR         YR(I1)
     C                   MOVE      EOFND         GR(I1)
     C                   MOVE      EOEOB#        EO(I1)
     C                   MOVE      EOAMTC        CA(I1)
     C     EOAMTC        SUB       EOAMT         SA(I1)
     C                   MOVE      *ZEROS        BL(I1)
     C                   MOVE      EONDIS        ND(I1)
     C                   MOVE      EOHRFL        FL(I1)
     C                   MOVE      *BLANKS       CT(I1)
     C                   Z-ADD     EOEOB#        XXEOB#
     C*** BEGIN ADD *** 08/26/98 **************************************
     C*
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C*
     C     EOKEY2        CHAIN(N)  PEO02                              99
     C*
     C                   Z-ADD     DBRRN         RRN(I1)
     C                   MOVEL(P)  EOCERT        CRT(I1)
     C                   Z-ADD     EOEXT         EXT(I1)
     C*** END   ADD *** 08/26/98 **************************************
     C     MBKEY         CHAIN     PMB01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    RLSPMB
     C                   MOVE      MBBAMT        IA(I1)
     C                   END
     C     TMKY1         CHAIN     PTM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '*'           TM(I1)
     C                   ELSE
     C                   MOVE      *BLANKS       TM(I1)
     C                   END
     C                   ENDSR
     C/EJECT
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C****************************************************************
     C*            HBA FILL SUBROUTINE - FILLS SCREEN ARRAYS FOR     *
     C*            HBA SYSTEM TYPES                                  *
     C****************************************************************
     CSR   FILLH         BEGSR
     C                   MOVE      'H'           SY(I1)
     C                   MOVE      EOCASE        CS(I1)
     C                   MOVE      EOFYR         YR(I1)
     C                   MOVE      EOFND         GR(I1)
     C                   MOVE      EOEOB#        EO(I1)
     C                   MOVE      EOAMTC        CA(I1)
     C     EOAMTC        SUB       EOAMT         SA(I1)
     C                   MOVE      *ZEROS        BL(I1)
     C                   MOVE      EONDIS        ND(I1)
     C                   MOVE      EOHRFL        FL(I1)
     C                   MOVE      *BLANKS       CT(I1)
     C                   Z-ADD     EOEOB#        XXEOB#
     C*** BEGIN ADD *** 08/26/98 **************************************
     C*
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C*
     C     EOKEY2        CHAIN(N)  PEO02                              99
     C*
     C                   Z-ADD     DBRRN         RRN(I1)
     C                   MOVEL(P)  EOCERT        CRT(I1)
     C                   Z-ADD     EOEXT         EXT(I1)
     C*** END   ADD *** 08/26/98 **************************************
     C     HBKEY         CHAIN     PHB01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    RLSPHB
     C                   MOVE      HB1AAM        IA(I1)
     C                   END
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
     C     EOKEY         READE     PEO01                                  60
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
     C*
     C                   Z-ADD     GR(14)        FUND14            3 0
     C                   Z-ADD     YR(14)        YEAR14            2 0
     C                   Z-ADD     CS(14)        CASE14            7 0
     C                   Z-ADD     EO(14)        EOB#14
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
     C                   READP     PEO01                                  60
     C  N60EOCO#         COMP      UPCO#                              6060
     C  N60EOSUB#        COMP      UPSUB#                             6060
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
     C*
     C                   Z-ADD     GR(14)        FUND14
     C                   Z-ADD     YR(14)        YEAR14
     C                   Z-ADD     CS(14)        CASE14
     C                   Z-ADD     EO(14)        EOB#14
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
     C     EOKEY         SETLL     PEO01
     C     EOKEY         READE     PEO01                                  60
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
     C     EOKEY         READE     PEO01                                  60
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
     C*
     C*  UPDATE EOB FILE
     C*
     C     SY(V)         IFEQ      'P'
     C                   Z-ADD     EOEOB#        XXEOB#
     C     EOKEY1        CHAIN     PEO01                              49
     C     *IN49         IFEQ      '0'
     C                   MOVEL     FL(V)         EOHRFL
     C                   Z-ADD     IA(V)         EOBAMT
     C                   Z-ADD     BL(V)         EO#BLL
     C                   UPDATE    PEO01
     C                   END
     C     EOKEY1        CHAIN     PEO02                              49
     C     *IN49         IFEQ      '0'
     C     FL(V)         IFEQ      EOHRFL
     C     IA(V)         ANDEQ     EOBAMT
     C     BL(V)         ANDEQ     EO#BLL
     C                   ELSE
     C                   MOVE      Q1USER        EOUSER
     C                   MOVE      Q1PGM         EOPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   Z-ADD     DSTIME        EOTIME
     C                   END
     C                   MOVEL     FL(V)         EOHRFL
     C                   Z-ADD     IA(V)         EOBAMT
     C                   Z-ADD     BL(V)         EO#BLL
     C                   UPDATE    PEO02
     C                   END
     C                   END
     C*
     C*  UPDATE PRE/RE-CERTIFICATION RECORD
     C*
     C     SY(V)         IFEQ      'U'
     C                   Z-ADD     EOEOB#        XXEOB#
     C     EOKEY1        CHAIN     PEO01                              49
     C     *IN49         IFEQ      '0'
     C                   MOVEL     FL(V)         EOHRFL
     C                   UPDATE    PEO01
     C                   END
     C     EOKEY1        CHAIN     PEO02                              49
     C     *IN49         IFEQ      '0'
     C     FL(V)         IFNE      EOHRFL
     C                   MOVE      Q1USER        EOUSER
     C                   MOVE      Q1PGM         EOPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   Z-ADD     DSTIME        EOTIME
     C                   END
     C                   MOVEL     FL(V)         EOHRFL
     C                   UPDATE    PEO02
     C                   END
     C     PBKEY1        CHAIN     PPB01                              49
     C     *IN49         IFEQ      '0'
     C     IA(V)         IFNE      PBBAMT
     C                   MOVE      Q1USER        PBUSER
     C                   MOVE      Q1PGM         PBPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PBDT
     C                   Z-ADD     DSTIME        PBTIME
     C                   END
     C                   Z-ADD     IA(V)         PBBAMT
     C                   UPDATE    PPB01
     C                   END
     C                   END
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C*  UPDATE MEDICAL MANAGEMENT RECORD
     C*
     C     XXMFLG        IFEQ      'Y'
     C*
     C     SY(V)         IFEQ      'M'
     C                   Z-ADD     EOEOB#        XXEOB#
     C     EOKEY1        CHAIN     PEO01                              49
     C     *IN49         IFEQ      '0'
     C                   MOVEL     FL(V)         EOHRFL
     C                   UPDATE    PEO01
     C                   END
     C     EOKEY1        CHAIN     PEO02                              49
     C     *IN49         IFEQ      '0'
     C     FL(V)         IFNE      EOHRFL
     C                   MOVE      Q1USER        EOUSER
     C                   MOVE      Q1PGM         EOPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   Z-ADD     DSTIME        EOTIME
     C                   END
     C                   MOVEL     FL(V)         EOHRFL
     C                   UPDATE    PEO02
     C                   END
     C     MBKEY1        CHAIN     PMB01                              49
     C     *IN49         IFEQ      '0'
     C     IA(V)         IFNE      MBBAMT
     C                   MOVE      Q1USER        MBUSER
     C                   MOVE      Q1PGM         MBPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MBDT
     C                   Z-ADD     DSTIME        MBTIME
     C                   END
     C                   Z-ADD     IA(V)         MBBAMT
     C                   UPDATE    PMB01
     C                   END
     C                   END
     C*
     C                   END
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C*  UPDATE HBA RECORD
     C*
     C     SY(V)         IFEQ      'H'
     C                   Z-ADD     EOEOB#        XXEOB#
     C     EOKEY1        CHAIN     PEO01                              49
     C     *IN49         IFEQ      '0'
     C                   MOVEL     FL(V)         EOHRFL
     C                   UPDATE    PEO01
     C                   END
     C     EOKEY1        CHAIN     PEO02                              49
     C     *IN49         IFEQ      '0'
     C     FL(V)         IFNE      EOHRFL
     C                   MOVE      Q1USER        EOUSER
     C                   MOVE      Q1PGM         EOPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   Z-ADD     DSTIME        EOTIME
     C                   END
     C                   MOVEL     FL(V)         EOHRFL
     C                   UPDATE    PEO02
     C                   END
     C     HBKEY1        CHAIN     PHB01                              49
     C     *IN49         IFEQ      '0'
     C     IA(V)         IFNE      HB1AAM
     C                   MOVE      Q1USER        HBUSER
     C                   MOVE      Q1PGM         HBPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HBDT
     C                   Z-ADD     DSTIME        HBTIME
     C                   END
     C                   Z-ADD     IA(V)         HB1AAM
     C                   UPDATE    PHB01
     C                   END
     C                   END
     C*
     C*  UPDATE NETWORK TRANSACTION RECORD
     C*
     C     SY(V)         IFEQ      'N'
     C                   Z-ADD     EOEOB#        XXEOB#
     C     TRKEY1        CHAIN     PTR01                              49
     C     *IN49         IFEQ      '0'
     C     FL(V)         IFEQ      TRHRFL
     C     IA(V)         ANDEQ     TRBAMT
     C                   ELSE
     C                   MOVE      Q1USER        TRUSER
     C                   MOVE      Q1PGM         TRPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TRDT
     C                   Z-ADD     DSTIME        TRTIME
     C                   END
     C                   MOVEL     FL(V)         TRHRFL
     C                   Z-ADD     IA(V)         TRBAMT
     C                   UPDATE    PTR01
     C                   END
     C                   END
     C                   END                                                    EOB# <> 0
     C*
     C                   END                                                    DO 14 V
     C*
     CSR                 ENDSR
     C/EJECT
     C*
     OPPB01     E            RLSPPB
     OPTR01     E            RLSPTR
     OPHB01     E            RLSPHB
     OPMB01     E            RLSPMB
     O**  PTL - PROGRAM TITLES (PASSED IN Q1PRDS)
     O**        TIME/MATERIALS ENTRY
     O**    TIME/MATERIALS MAINTENANCE
**
ROLL UP
ROLL DOWN
