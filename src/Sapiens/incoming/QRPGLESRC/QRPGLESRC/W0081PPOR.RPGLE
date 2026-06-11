     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/TITLE W0081PPOR - Issue Network Check for PPO Fee
      //****************************************************************
      // Description:  This program creates the payment request trans- *
      //   actions for EOMBs for Network Checks                        *
      //                                                               *
      // Called from:  FIXBKFLPPO                                      *
      // Calls to:     none                                            *
      //                                                               *
      // Created by:  C:79     Support: 88913      05/15/20            *
      //****************************************************************
      //                                                               *
      //  REVISIONS:                                                   *
      //****************************************************************
      // Indicator usage:                                              *
      //                                                               *
      //  Note on *IN16, *IN17 and *IN18 with regard to Auto DARTS:    *
      //  1) If *IN16 and *IN17 are *ON then Auto DARTS to W0070C50.   *
      //  2) If *IN16 and *IN18 are *ON then Auto DARTS to F0350C.     *
      //  *IN16  -  No provider payment. (RPAMT EQ ZERO).              *
      //  *IN17  -  There is a BRS payment (RPAMT GT ZERO).            *
      //  *IN18  -  No BRS payment (RPAMT EQ ZERO).                    *
      //                                                               *
      //  *IN60  -  General usage                                      *
      //  *IN61  -  General usage                                      *
      //****************************************************************
     FFAEAL1    IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     FFAEOL3    IF   E           K DISK
     FWMCIL     IF   E           K DISK
     FWMCML1    UF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWARPL     IF A E           K DISK
     F                                     INFDS(DBFDS1)
     FWMCXL     IF   E           K DISK
     FWTPSL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PBS01)
     F                                     IGNORE(PTT01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PVS01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)
     FWMNRL     IF   E           K DISK
     FFAESL     IF   E           K DISK
     FWARPLC    IF   E           K DISK    Prefix(R2:2) Rename(prp01:prp03)
     FWDPAL     IF A E           K DISK
     FWDRPL     O    E           K DISK
     F                                     RENAME(PRP01:PRP02)
     FWDELL     UF   E           K DISK
     FWMRHP     O    E             DISK
     FWMFNL     IF   E           K DISK
     F                                     RENAME(PFN01:PFN02)
      //
      //
     D DESC1           C                   CONST('Adjustment due to re-
     D                                     duced BRS fee')
     D DESC2           C                   CONST('Pool Department')
     D DBFDS1          DS
     D  DBRRN                397    400B 0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  F1NW                   1     50
     D                                     DIM(10)
     D  F1NW01                 1      5
     D  F1NW02                 6     10
     D  F1NW03                11     15
     D  F1NW04                16     20
     D  F1NW05                21     25
     D  F1NW06                26     30
     D  F1NW07                31     35
     D  F1NW08                36     40
     D  F1NW09                41     45
     D  F1NW10                46     50
     D                 DS
     D  F1NF                   1     10
     D                                     DIM(10)
     D  F1NF01                 1      1
     D  F1NF02                 2      2
     D  F1NF03                 3      3
     D  F1NF04                 4      4
     D  F1NF05                 5      5
     D  F1NF06                 6      6
     D  F1NF07                 7      7
     D  F1NF08                 8      8
     D  F1NF09                 9      9
     D  F1NF10                10     10
     D                 DS
     D  F1NI                   1     90  0
     D                                     DIM(10)
     D  F1NI01                 1      9  0
     D  F1NI02                10     18  0
     D  F1NI03                19     27  0
     D  F1NI04                28     36  0
     D  F1NI05                37     45  0
     D  F1NI06                46     54  0
     D  F1NI07                55     63  0
     D  F1NI08                64     72  0
     D  F1NI09                73     81  0
     D  F1NI10                82     90  0
     D                 DS
     D  F1NM                   1     30  0
     D                                     DIM(10)
     D  F1NM01                 1      3  0
     D  F1NM02                 4      6  0
     D  F1NM03                 7      9  0
     D  F1NM04                10     12  0
     D  F1NM05                13     15  0
     D  F1NM06                16     18  0
     D  F1NM07                19     21  0
     D  F1NM08                22     24  0
     D  F1NM09                25     27  0
     D  F1NM10                28     30  0
     D                 DS
     D  F1N$                   1     90  2
     D                                     DIM(10)
     D  F1N$01                 1      9  2
     D  F1N$02                10     18  2
     D  F1N$03                19     27  2
     D  F1N$04                28     36  2
     D  F1N$05                37     45  2
     D  F1N$06                46     54  2
     D  F1N$07                55     63  2
     D  F1N$08                64     72  2
     D  F1N$09                73     81  2
     D  F1N$10                82     90  2
     D                 DS
     D  F1NT                   1     10
     D                                     DIM(10)
     D  F1NT01                 1      1
     D  F1NT02                 2      2
     D  F1NT03                 3      3
     D  F1NT04                 4      4
     D  F1NT05                 5      5
     D  F1NT06                 6      6
     D  F1NT07                 7      7
     D  F1NT08                 8      8
     D  F1NT09                 9      9
     D  F1NT10                10     10
      //
     D                 DS
     D  DATA                   1     75
     D  QXTRDT                 1      7  0
     D  QXFRDT                 8     14  0
     D  QXTODT                15     21  0
     D  QXTRCD                22     23
     D  QXRECD                24     25
     D  QXPV1                 26     27
     D  QXPV2                 28     29
     D  QXSEQ#                30     34  0
     D  QXEOB#                35     43
     D  QXFEIN                44     52  0
     D  QXMOD                 53     55  0
     D  QXDSQ#                56     60  0
      //
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS                  INZ
     D  RRT                    1     20
     D                                     DIM(10)
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS                  INZ
     D  RSU                    1     90  2
     D                                     DIM(10)
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
      //
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      /copy wPrClmRsv
      /copy sPrModule
     D/copy WPRBANK
     d ##Co#           s              3
     d ##Fnd           s              3
     d #1fnd           s              3
     d ##Fyr           s              3
     d ##Case          s              7
     d ##Appl          s              2
     d ##Data          s             75
     d ##Cktp          s             10
     d #rtcd           s              1
     d ##Eftc          s              1
     d ##Eft           s              1
     d ##Pre           s              1
     d ##ptyp          s              1
     d #ekey           s             30
     d ##Date          s              7
     d ##key           s             30
     d ##outRRN        s              9
     d ##EobType       s              9
      //
      //========================================================================
      //
     C     CXKEY         KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
      //
      //  REQUEST PAYMENT FILE - PARTIAL
      //
     C     RPKEY2        KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
      //
      //  PAYMENT DETAIL FILE - PARTIAL
      //
     C     PAKY          KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
      //
      //  Process the initialization subroutine
      //
      /FREE
       EXSR SRINIT;
       //
       //  Retrieve the EOMB information
       //
       CHAIN EOKY PEO01;
       *IN60 = NOT %FOUND;
      /END-FREE
     C   60              GOTO      TERM
     C                   MOVE      EORCN         XXRCN
      //
      /FREE
       CHAIN EAKY PEA01;
       *IN60 = NOT %FOUND;
      /END-FREE
     C   60              GOTO      TERM
      //
      //  Retrieve the Claim record
      //
      /FREE
       CHAIN(N) CMKY PCM01;
       *IN60 = NOT %FOUND;
      /END-FREE
     C   60              GOTO      TERM
      //
      //  Determine Next sequence number for payment requests
      //
      /FREE
       SETGT RPKY PRP01;
       READPE(N) RPKY PRP01;
       *IN60 = %EOF;
      /END-FREE
     C   60              Z-ADD     0             RPSEQ#
      //
      //  Process the subroutines which create the various payments.
      //
      /FREE
       IF EASYS = 'P'
       OR EASYS = 'H'
       OR EASYS = 'N';
          EXSR SRNTWK;
       ENDIF;
       //
       //  END THE PROGRAM
       //
      /END-FREE
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
      //*****************************************************************
      //  SRINIT - This subroutine is used to define and initialize
      //    program variables.
      //*****************************************************************
      /FREE
       BEGSR SRINIT;
          //
          //  KEY LIST FOR HEADER BILL AMOUNT FILE
          //
      /END-FREE
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXGRP#
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXRCN             3 0
      //
     C     ESKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXGRP#
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXIDT
      //
     C     ESKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXGRP#
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    XXEOB#
      //  Key list for the WCMC Group Master file
      //
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXGRP#
      //
      //  Key list for the EOMB file
      //
     C     EOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXEOB#
      //
      //  Key list for the EOMB Auxiliary files
      //
     C     EOKYA         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXGRP#
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    XXEOB#
      //
      //  Key list for the WCMC Check Issuance file
      //
     C     CIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPTYP
     C                   KFLD                    XXSYS
      //
      //  Key list for the Claim Master file
      //
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
      //
      //  Key list for the Request Payment file
      //
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
      //
      //  Key list for the Provider file
      //
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
      //
      //  NETWORK REVIEW ID FILE
      //
     C     NRKY          KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFEIN
     C                   KFLD                    RPMOD
      //
      //  Key list for the WCMS table files
      //
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
      //
     C     PAKY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
      //
     C     ELKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C     ELKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
      //
     C     FNKYWC        KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOFND
      //
      //  Entry parm list
      //
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@GRP#            3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@EOB#            9
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@NAMT
     C                   PARM                    @@MRA
      //
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      //
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      //
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANK        SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      //
     C     DTMDCY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANK        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      //
     C     *LIKE         DEFINE    RPCO#         XXCO#
     C     *LIKE         DEFINE    RPFND         XXFND
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOFND         XXGRP#
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    EOFEIN        XXFEIN
     C     *LIKE         DEFINE    EOMOD         XXMOD
     C     *LIKE         DEFINE    CIPTYP        XXPTYP
     C     *LIKE         DEFINE    CISYS         XXSYS
     C     *LIKE         DEFINE    PCKEY         XXKEY
     C     *LIKE         DEFINE    EANAMT        @@NAMT
     C     *LIKE         DEFINE    EANAMT        @@MRA
     C     *LIKE         DEFINE    ESIDT         XXIDT
     C     *LIKE         DEFINE    PASEQ#        XXSEQ#
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    RPAMT         SVAMT
     C                   Z-ADD     1             X                 3 0
     C                   TIME                    DSDTTM
      //
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@SUB#        XXSUB#
     C                   MOVE      @@GRP#        XXGRP#
     C                   MOVE      @@EOB#        XXEOB#
      //
      //  Retrieve the WCMS group #.
      //
     C                   Z-ADD     0             XXFND
      /FREE
          CHAIN FNKY PFN01;
          *IN60 = NOT %FOUND;
          IF *IN60 = '0';
      /END-FREE
     C                   MOVE      FNWCF#        XXFND
      /FREE
          ENDIF;
          //
          //  Retrieve the Group Auxiliary record.
          //
          CHAIN FNKY PF101;
          *IN60 = NOT %FOUND;
          IF *IN60 = '1';
      /END-FREE
     C                   MOVEA     *BLANKS       F1NW
     C                   MOVEA     *BLANKS       F1NF
     C                   MOVEA     *ZEROS        F1NI
     C                   MOVEA     *ZEROS        F1NM
     C                   MOVEA     *ZEROS        F1N$
     C                   MOVEA     *BLANKS       F1NT
      /FREE
          ENDIF;
      /END-FREE
     C                   CALL      'SA001R'
01   C                   PARM                    @@WCIF            1
02   C                   PARM                    @@MCIF            1
03   C                   PARM                    @@WCIN            1
04   C                   PARM                    @@AR              1
05   C                   PARM                    @@NTWK            1
06   C                   PARM                    @@UR              1
07   C                   PARM                    @@MED             1
08   C                   PARM                    @@EDI             1
09   C                   PARM                    @@WCDK            1
10   C                   PARM                    @@RHAB            1
11   C                   PARM                    @@MSS             1
12   C                   PARM                    @@FSCH            1
13   C                   PARM                    @@PRVB            1
14   C                   PARM      ' '           @@NPWC            1
15   C                   PARM      ' '           @@OS              1
16   C                   PARM      ' '           @@ARLS            1
17   C                   PARM      ' '           @@CCOD            1
18   C                   PARM      ' '           @@CRVW            1
19   C                   PARM      ' '           @@PROM            1
20   C                   PARM      ' '           @@MCRV            1
21   C                   PARM      ' '           @@FREQ            1
22   C                   PARM      ' '           @@CRAC            1
23   C                   PARM      ' '           @@PREO            1
24   C                   PARM      ' '           @@PARN            1
25   C                   PARM      ' '           @@MGRI            1
26   C                   PARM      ' '           @@KCIF            1
27   C                   PARM      ' '           @@CURS            1
28   C                   PARM      ' '           @@PCIB            1
      //
     C                   CALL      'RTVCLTID'
     C                   PARM                    @@CLT#            3
      /FREE
       ENDSR;
       //*****************************************************************
       //  SRNTWK - This subroutine creates the network payment.
       //*****************************************************************
       BEGSR SRNTWK;
          //
          //  Create the payment request record.
          //
      /END-FREE
     C                   Z-ADD     XXCO#         RPCO#
     C                   Z-ADD     XXFND         RPFND
     C                   Z-ADD     EOFYR         RPFYR
     C                   Z-ADD     EOCASE        RPCASE
     C                   ADD       1             RPSEQ#
     C                   Z-ADD     EOTRDT        RPTRDT
      //
      //  Retrieve the Network WCMC check issuance record.
      //
     C                   MOVEL     EONTWK        XXNWCD            5
      //
     C                   Z-ADD     1             X
     C     NxtNwCd       TAG
     C     XXNWCD        LOOKUP    F1NW(X)                                61
      //
     C  N61              GOTO      NXTC
      //
      // IF IN-HOUSE NETWORK:
      //
      /FREE
B01       IF F1NF(X) = 'I';
             //
      /END-FREE
     C                   MOVE      'B'           XXPTYP
     C                   MOVE      'N'           XXSYS
      //
      // RETRIEVE WCMC CHECK ISSUANCE INFORMATION:
      //
      /FREE
             CHAIN CIKY PCI01;
             *IN60 = NOT %FOUND;
             //
      /END-FREE
     C   60              GOTO      NXTC
      //
     C                   Z-ADD     CIFEIN        RPFEIN
     C                   Z-ADD     CIMOD         RPMOD
      //
      // RETRIEVE BILLING AMOUNT:
      //
      //
     C   60              GOTO      NXTC
     C                   Z-ADD     EANAMT        RPAMT
      //
      // RETRIEVE BILLING AMOUNT:
      //
      /FREE
S01       ELSE;
             //
             // RETRIEVE WCMC CHECK ISSUANCE INFORMATION:
             //
      /END-FREE
     C                   MOVEL     'N'           XXPTYP
     C                   MOVEL     EOSYS         XXSYS
      //
      /FREE
             CHAIN CIKY PCI01;
             *IN60 = NOT %FOUND;
             //
      /END-FREE
     C   60              GOTO      NXTC
      //
      // ESTABLISH PAYEE FEIN/MOD
      //
     C                   Z-ADD     F1NI(X)       RPFEIN
     C                   Z-ADD     F1NM(X)       RPMOD
      //
      // CALCULATE OUTSIDE NETWORK FEE:
      // IF OUTSIDE NETWORK, DISCOUNT CALCULATED OUTSIDE, THEN USE FEE
      // STORED IN FAEAP/EANAMT
      //
      /FREE
B02          IF F1NF(X) = 'Y';
                //
      /END-FREE
     C                   Z-ADD     EANAMT        RPAMT
      //
      /FREE
S02          ELSE;
B03             IF F1NT(X) = 'B';
                   //
      /END-FREE
     C                   Z-ADD     F1N$(X)       RPAMT
      //
      /FREE
S03             ELSE;
                   //
      /END-FREE
      /Free
        Select;
        When F1NT(X) ='S';
           RPAMT = (F1N$(X)/100) * EAndis;
        When F1NT(X) ='F';
           RPAMT = (F1N$(X)/100) * EAmra;
        Other;
           RPAMT = *Zeros;
        Endsl;
      /End-Free
      //
      /FREE
E03             ENDIF;
                //
                // UPDATE FAEAP/EANAMT WITH CALCULATE NETWORK FEE:
                //
      /END-FREE
      /Free
       Select;
       When F1NT(X) = 'F' and Q1Prms >= 10;
          If EArcn = *zeros AND RPamt < *Zeros;
             @@MRA = *Zeros;
             RPamt = *Zeros;
          Else;
             @@MRA = RPamt;
          Endif;
       When F1NT(X) <> 'F';
          If EArcn = *zeros AND RPamt < *Zeros;
             @@NAMT = *Zeros;
             RPamt = *Zeros;
          Else;
             @@NAMT = RPamt;
          Endif;
       Other;
             @@NAMT = *Zeros;
             RPamt = *Zeros;
       Endsl;
      /End-Free
      //
      /FREE
E02          ENDIF;
             //
E01       ENDIF;
          //
          CHAIN ELKY1 PEL01;
          *IN60 = NOT %FOUND;
          //
      /END-FREE
     C                   MOVE      *OFF          @ICC              1
      //
      /FREE
          SELECT;
             //
             // Identify Cost-containment PV1
             //
          WHEN CMJRST = 'CA'
          AND ELPFDT >= 1100701
          AND *IN60 = *OFF;
             //
      /END-FREE
     C                   MOVE      *BLANKS       XXKEY
      //
      /FREE
             IF CMCVFL = 'Y'
             AND CICABC > *BLANKS;
      /END-FREE
     C                   MOVEL(P)  CICABC        XXKEY
      /FREE
             ELSE;
                IF CICAB2 > *BLANKS;
      /END-FREE
     C                   MOVEL(P)  CICAB2        XXKEY
      /FREE
                ENDIF;                                                        //
             ENDIF;                                                           //
             //
             //  Determine if cost containment PV1 feature is turned on.
             //  Otherwise, use old logic.
             //
             CHAIN TBKY PB201;
             *IN60 = NOT %FOUND;
             IF *IN60 = *OFF
             AND B2CCW = 'Y'
             AND XXKEY > *BLANKS;
      /END-FREE
     C                   MOVEL     XXKEY         RPBR2
     C                   MOVE      *ON           @ICC
      /FREE
             ELSE;
                IF CMCVFL = 'Y';
      /END-FREE
     C                   MOVE      CIP1CV        RPBR2
      /FREE
                ELSE;
      /END-FREE
     C                   MOVE      CIBR2         RPBR2
      /FREE
                ENDIF;
             ENDIF;
             //
          OTHER;
             IF CMCVFL = 'Y';
      /END-FREE
     C                   MOVE      CIP1CV        RPBR2
      /FREE
             ELSE;
      /END-FREE
     C                   MOVE      CIBR2         RPBR2
      /FREE
             ENDIF;
          ENDSL;
          //
          IF CMLTFL = 'I';
      /END-FREE
     C                   MOVE      CIBR7L        RPBR7
      /FREE
          ELSE;
      /END-FREE
     C                   MOVE      CIBR7N        RPBR7
      /FREE
          ENDIF;
          //
      /END-FREE
     C                   MOVE      CMNCCI        RPNCCI
     C                   Z-ADD     EOFRDT        RPFRDT
     C                   Z-ADD     EOTODT        RPTODT
     C                   Z-ADD     0             RPWKS
     C                   Z-ADD     0             RPDAYS
      /FREE
          IF EATRCD <> *BLANKS
          AND @@CLT# <> '146';
      /END-FREE
     C                   MOVE      EATRCD        RPTRCD
      /FREE
          ELSE;
             SELECT;
                //
                // Cost-containment PV1
                //
             WHEN @ICC = *ON;
                IF CMCVFL = 'Y';
      /END-FREE
     C                   MOVE      CICATC        RPTRCD
      /FREE
                ELSE;
      /END-FREE
     C                   MOVE      CICATR        RPTRCD
      /FREE
                ENDIF;
                //
             OTHER;
                IF CMCVFL <> 'Y';
      /END-FREE
     C                   MOVEL(P)  CITRCD        RPTRCD
      /FREE
                ELSE;
      /END-FREE
     C                   MOVEL(P)  CITRCV        RPTRCD
      /FREE
                ENDIF;
             ENDSL;
          ENDIF;                                                              //
          //
      /END-FREE
     C                   MOVEL(P)  RPTRCD        XXKEY
      /FREE
          CHAIN TBKY PPC01;
          *IN60 = NOT %FOUND;
          IF *IN60 = '0';
      /END-FREE
     C                   MOVE      PCRECD        RPRECD
     C                   MOVE      PCTRTP        RPTYPE
     C                   MOVEL     PCDESC        RPDESC
      /FREE
          ELSE;
      /END-FREE
     C                   MOVE      *BLANKS       RPRECD
     C                   MOVE      *BLANKS       RPTYPE
     C                   MOVE      *BLANKS       RPDESC
      /FREE
          ENDIF;
          //
      /END-FREE
     C                   MOVEL(P)  RPRECD        XXKEY
      /FREE
          CHAIN TBKY PRT01;
          *IN60 = NOT %FOUND;
          IF *IN60 = '0';
      /END-FREE
     C                   MOVE      RTTYPE        RPRETP
      /FREE
          ELSE;
      /END-FREE
     C                   MOVE      *BLANKS       RPRETP
      /FREE
          ENDIF;
          //
      /END-FREE
     C                   MOVEL(P)  EACNTL        RPRMDS
     C                   MOVE      *BLANKS       RPRMK1
     C                   MOVE      *BLANKS       RPRMK2
     C                   MOVE      *BLANKS       RPRMK3
      //
     C                   Z-ADD     RPFEIN        XXFEIN
     C                   Z-ADD     RPMOD         XXMOD
      /FREE
          CHAIN PDKY PPD01;
          *IN60 = NOT %FOUND;
      /END-FREE
     C   60              GOTO      NXTC
      /FREE
          IF PDDBA1 <> 'Y';
      /END-FREE
     C                   MOVE      PDNAME        RPPNAM
     C                   MOVE      PDADR1        RPADR1
     C                   MOVE      PDADR2        RPADR2
      /FREE
          ELSE;
      /END-FREE
     C                   MOVE      PDADR1        RPPNAM
     C                   MOVE      PDADR2        RPADR1
     C                   MOVE      *BLANKS       RPADR2
      /FREE
          ENDIF;
      /END-FREE
     C                   MOVE      PDCTY         RPCTY
     C                   MOVE      PDST          RPST
     C                   MOVE      PDZIP         RPZIP
     C                   MOVE      *BLANKS       RPADR3
     C                   MOVE      *BLANKS       RPCTRY
      /FREE
          IF PDADR3 <> *BLANK;
      /END-FREE
     C                   MOVEL(P)  PDCTRY        RPCTRY
     C                   MOVEL(P)  PDADR3        RPADR3
      /FREE
          ENDIF;
          //
      /END-FREE
     C                   Z-ADD     EOEOB#        RPEOB#
     C                   MOVE      'P'           RPSTAT
     C                   Z-ADD     0             RPBUL1
     C                   Z-ADD     0             RPCHK#
     C                   MOVE      'G'           RPPGTP
     C                   MOVE      *BLANK        RPOFF#
      //
     C                   MOVE      EAIUSR        RPUSER
     C                   MOVE      Q1PGM         RPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RPTIME
      //
     C                   Z-ADD     EOCRDT        RPRCVD
     C                   Z-ADD     0             RPDUE
      //
      //  Call API to determine the Bank "Code".
      //
     C                   MOVE      RPCO#         ##CO#
     C                   MOVE      RPFND         ##FND
     C                   MOVE      RPFYR         ##FYR
     C                   MOVE      RPCASE        ##CASE
     C                   Z-ADD     RPTRDT        QXTRDT
     C                   Z-ADD     RPFRDT        QXFRDT
     C                   Z-ADD     RPTODT        QXTODT
     C                   MOVEL     RPTRCD        QXTRCD
     C                   MOVEL     RPRECD        QXRECD
     C                   MOVEL     RPBR2         QXPV1
     C                   MOVEL     RPBR7         QXPV2
     C                   MOVE      RPFEIN        QXFEIN
     C                   MOVE      RPMOD         QXMOD
     C                   MOVE      *BLANKS       RPDSQ#
     C                   MOVE      RPDSQ#        QXDSQ#
      //
      //
      /Free
       //Call the Procedure to retrieve Bank Code.
       ##data = DATA;
       ##appl = 'WC';
       ##eftc = '  ';
       ##cktp = GetBankCode(##co#: ##fnd: ##fyr: ##case: ##appl:
                 ##data: #rtcd: ##eftc);
       RPeftc = ##eftc;
      /End-Free
     C                   MOVE      ##CKTP        RPCKTP
      /FREE
          EXSR CHKEFT;
      /END-FREE
     C                   MOVE      *BLANK        RPACCT
      /FREE
          IF EACKST = *BLANKS;
      /END-FREE
     C                   MOVEL     CISTS         RPSTS
      /FREE
          ELSE;
             IF EACKST = 'PY';
      /END-FREE
     C                   MOVEL     CISTS         RPSTS
      /FREE
             ELSE;
      /END-FREE
     C                   MOVE      EACKST        RPSTS
      /FREE
             ENDIF;
          ENDIF;
          //
          //
          // CHECK TO SEE IF THIS BILL SHOULD BE AUTO-RELEASED
          //
          IF @@CLT# = '132';
             IF RPAMT > 250
             OR CMDNY = 'Y'
             OR CMSTAT = 'C';
      /END-FREE
     C                   MOVEL(P)  'PR'          RPSTS
      /FREE
             ENDIF;
          ENDIF;
          //
      /END-FREE
     C                   MOVEL(P)  RPSTS         XXKEY
      /FREE
          CHAIN TBKY PPS01;
          *IN60 = NOT %FOUND;
          IF *IN60 = '0';
      /END-FREE
     C                   MOVE      PSSRT         RPSORT
      /FREE
          ELSE;
      /END-FREE
     C                   MOVE      *BLANKS       RPSORT
      /FREE
          ENDIF;
          //
      /END-FREE
     C                   MOVE      'N'           RPSENT
     C                   Z-ADD     *ZERO         RPDSEQ
     C                   MOVE      EAIUSR        RPRQST
     C                   MOVE      *BLANKS       RPLTCD
      /FREE
          IF F1NF(X) = 'I';
      /END-FREE
     C                   MOVE      'I'           RPNTWK
      /FREE
          ELSE;
      /END-FREE
     C                   MOVE      *BLANKS       RPNTWK
      /FREE
          ENDIF;
      /END-FREE
     C                   Z-ADD     EARCN         RPEOBS
     C                   MOVE      *BLANKS       RPFNAL
     C                   MOVEL     *BLANKS       RPCKCN
     C                   Z-ADD     RPDT          RPIDT
     C                   Z-ADD     DSTIME        RPITM
     C                   MOVEL     RPUSER        RPIUSR
     C                   MOVEL     Q1PGM         RPIPGM
     C                   Z-ADD     0             RPRDT
     C                   Z-ADD     0             RPRTM
     C                   MOVE      *BLANKS       RPRUSR
     C                   MOVE      *BLANKS       RPRPGM
      /FREE
          IF RPSTAT = ' ';
      /END-FREE
     C                   Z-ADD     RPDT          RPRDT
     C                   Z-ADD     DSTIME        RPRTM
     C                   MOVEL     RPUSER        RPRUSR
     C                   MOVEL     Q1PGM         RPRPGM
      /FREE
          ENDIF;
          //
      /END-FREE
      /free
           If RPamt > 0 And Not IsModEnabled('Negative Fee Checks') And
              EAPNWC <> 'N' And EAPNWC <> '3' and EAPNWC <> '4';
      /end-free
      /FREE
          EXSR EFTSLT;
       ENDIF;
      /END-FREE
      /free
           If RPamt <> 0 And IsModEnabled('Negative Fee Checks') And
              EAPNWC <> 'N' And EAPNWC <> '3' and EAPNWC <> '4';
      /end-free
      /FREE
       WRITE PRP01;
       ENDIF;
       IF RPAMT < 0
       AND @@CLT# = '135';
          //
          SETGT PAKY1 PPA01;
          READPE(N) PAKY1 PPA01;
          *IN60 = %EOF;
      /END-FREE
     C  N60              Z-ADD     PASEQ#        XXSEQ#
     C   60              Z-ADD     0             XXSEQ#
      /FREE
          CLEAR PPA01;
          //
      /END-FREE
     C     XXSEQ#        ADD       1             PASEQ#
      //
     C                   Z-ADD     RPAMT         PAAMT
     C                   Z-ADD     EOCO#         PACO#
     C                   Z-ADD     EOFND         PAFND
     C                   Z-ADD     EOFYR         PAFYR
     C                   Z-ADD     EOCASE        PACASE
     C                   Z-ADD     0             PACHK#
     C                   Z-ADD     0             PABASQ
     C                   Z-ADD     RPFEIN        PAFEIN
     C                   Z-ADD     RPMOD         PAMOD
      //
     C                   Z-ADD     RPDT          PATRDT
     C                   Z-ADD     RPDT          PAPODT
     C                   Z-ADD     RPFRDT        PAFRDT
     C                   Z-ADD     RPTODT        PATODT
     C                   Z-ADD     0             PAWKS
     C                   Z-ADD     0             PADAYS
      //
     C                   MOVE      RPBR2         PABR2
     C                   MOVE      RPBR7         PABR7
      //
      /FREE
          SETLL *LOVAL PPC01;
          READ PPC01;
          *IN60 = %EOF;
          DOW *IN60 = '0';
             IF PCRECD = RPRECD
             AND PCTRTP = 'A';
                LEAVE;
             ENDIF;
             READ PPC01;
             *IN60 = %EOF;
          ENDDO;
          //
      /END-FREE
     C  N60              MOVEL     PCKEY         PATRCD
     C   60              MOVE      RPTRCD        PATRCD
     C                   MOVEL     DESC1         PADESC
     C                   MOVE      'A'           PATYPE
     C                   MOVE      RPRECD        PARECD
     C                   MOVE      RPRETP        PARETP
      //                    MOVE *BLANK    PAADST
     C                   Z-ADD     RPEOB#        PAEOB#
     C                   Z-ADD     RPEOBS        PAEOBS
     C                   MOVEL     CMOFF#        PAOFF#
     C                   MOVE      *BLANK        PASTAT
     C                   MOVEL     Q1USER        PARQST
     C                   MOVEL     Q1USER        PAUSER
     C                   MOVEL     Q1PGM         PAPGM
     C                   Z-ADD     RPDT          PADT
     C                   Z-ADD     RPTIME        PATIME
     C                   MOVEL     Q1USER        PAIUSR
     C                   MOVEL     Q1PGM         PAIPGM
     C                   Z-ADD     RPDT          PAIDT
     C                   Z-ADD     RPTIME        PAITIM
      //
      /FREE
          WRITE PPA01;
          //
          //  CREATE REQUEST PAYMENT DETAIL RECORD
          //
      /END-FREE
     C                   Z-ADD     PATRDT        RPTRDT
     C                   Z-ADD     PASEQ#        RPSEQ#
     C                   MOVE      *BLANKS       RPSTAT
     C                   MOVEL     Q1USER        RPUSER
      /FREE
          WRITE PRP02;
          //
          // PROCESS THE CLAIM MASTER RECORDS EFFECTED.
          //
          CHAIN PAKY1 PCM01;
          *IN60 = NOT %FOUND;
          IF *IN60 = *OFF;
             //
      /END-FREE
     C                   Z-ADD     1             X
     C     PARECD        LOOKUP    RT(X)                                  60
      //
      /FREE
             IF CMSTAT = 'C'
             OR PAAMT > RR(X);
                EXSR RSVHST;
             ELSE;
      /END-FREE
     C                   SUB       PAAMT         RR(X)
      /FREE
             ENDIF;                                                           //
      /END-FREE
     C                   ADD       PAAMT         AR(X)
      //
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
      //
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
      //
      /FREE
             UPDATE PCM01;
             //
             //   UPDATE THE EMPLOYER DETAIL FILE WITH THE OUTSTANDING
             //   RESERVE & TOTAL CLAIMS PAID
             //
             CHAIN ELKY PEL01;
             *IN60 = NOT %FOUND;
             IF *IN60 = *OFF;
                //
                IF CMSTAT = 'O';
      /END-FREE
     C                   SUB       PAAMT         ELOUTS
      /FREE
                ENDIF;                                                        //
      /END-FREE
     C                   ADD       PAAMT         ELTCLP
      //
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
      //
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
      //
      /FREE
                UPDATE PEL01;
                //
             ENDIF;                                                           //
          ENDIF;
          //
       ENDIF;                                                                 //
       //
       IF X < 10;
      /END-FREE
     C                   ADD       1             X
     C                   GOTO      NxtNwCd
      /FREE
       ENDIF;
      /END-FREE
     C     NXTC          TAG
      //
      /FREE
       ENDSR;
       //===============================================================*
       BEGSR CHEDPT;
          //
          //  Check the Request Payment File for exact or possible duplicates.
          //
      /END-FREE
     C                   MOVE      *OFF          *IN66
     C                   MOVE      *OFF          *IN67
      //
      /FREE
          CHAIN RPKEY2 PRP03;
          *IN60 = NOT %FOUND;
          DOW *IN60 = '0';
             //
             //  Skip record if Payment Type is Not 'M' or 'P'.
             //
             IF R2TYPE <> 'M'
             AND R2TYPE <> 'P';
      /END-FREE
     C                   GOTO      NXTRP1
      /FREE
             ENDIF;
             //
             //  Payment Request is an Exact Duplicate if a Previous Request has
             //  the Same Payment Reserve Code, Provider, Dates of Service,
             //  Payment Amount, and the Sequence Number of Both WARPP records
             //  are different.
             //
             IF RPRECD = R2RECD
             AND RPFEIN = R2FEIN
             AND RPFRDT = R2FRDT
             AND RPTODT = R2TODT
             AND RPAMT = R2AMT
             AND R2STAT <> 'D'
             AND RPSEQ# <> R2SEQ#;
                //
      /END-FREE
     C                   MOVE      '1'           *IN65
     C                   MOVE      '1'           *IN66
      //
      /FREE
                *IN90 = *ON;
                //
             ENDIF;                                                           //
      /END-FREE
     C     NXTRP1        TAG
      //
      /FREE
             READE RPKEY2 PRP03;
             *IN60 = %EOF;
          ENDDO;                                                              //
          //
          CHAIN PAKY PPA01;
          *IN60 = NOT %FOUND;
          //
          DOW *IN60 = '0';
             //
             //  Skip record if Payment Type is Not 'M' or 'P'.
             //
             IF PATYPE <> 'M'
             AND PATYPE <> 'P'
             AND PATYPE <> 'A'
             AND PATYPE <> 'Z';
      /END-FREE
     C                   GOTO      NXTPA1
      /FREE
             ENDIF;
             //
             //  Payment Request is an Exact Duplicate if a Previous Payment has
             //  the Same Payment Reserve Code, Provider, Dates of Service and
             //  Payment Amount.
             //
             IF RPRECD = PARECD
             AND RPFEIN = PAFEIN
             AND RPFRDT = PAFRDT
             AND RPTODT = PATODT
             AND RPAMT = PAAMT;
                //
                //  DETERMINE IF A DUP PAYMENT HAS BEEN VOIDED.
                //
                IF PABASQ <> *ZERO
                OR PAADST <> *BLANKS;
      /END-FREE
     C                   MOVE      *ON           XXVOID            1
      /FREE
                ELSE;
      /END-FREE
     C                   MOVE      *OFF          XXVOID
      /FREE
                ENDIF;
                //
                IF XXVOID = *OFF;
                   //
      /END-FREE
     C                   MOVE      '1'           *IN65
     C                   MOVE      '1'           *IN66
      //
      /FREE
                   *IN90 = *ON;
                   //
                ENDIF;
             ENDIF;                                                           //
             //
      /END-FREE
     C     NXTPA1        TAG
      //
      /FREE
             READE PAKY PPA01;
             *IN60 = %EOF;
          ENDDO;                                                              //
          //
       ENDSR;
       //===============================================================*
       //  RSVHST - Write a reserve history record
       //
       //   A RESERVE HISTORY RECORD WILL BE WRITTEN IN THE AMOUNT OF THE
       //   ADJUSTMENT TO KEEP CLAIM MSTR (WMCMP) AND RSRV HIST (WMRHP)
       //   IN BALANCE.
       //===============================================================*
       BEGSR RSVHST;
          //
          CLEAR PRH01;
          //
      /END-FREE
     C                   Z-ADD     CMCO#         RHCO#
     C                   Z-ADD     CMFND         RHFND
     C                   Z-ADD     CMFYR         RHFYR
     C                   Z-ADD     CMCASE        RHCASE
     C                   Z-ADD     1             Y                 3 0
     C     PARECD        LOOKUP    RT(Y)                                  99
     C                   MOVEL     PARECD        RRT(Y)
     C                   Z-ADD     PAAMT         RSU(Y)
     C                   MOVEL     DESC1         RHEXPL
     C                   MOVE      Q1USER        RHUSER
     C                   MOVE      Q1PGM         RHPGM
      //
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RHDT
     C                   MOVEL     SRDTO         RHEFDT
     C                   Z-ADD     DSTIME        RHTIME
      //
     C                   MOVEL     SRDTO         RHIDT
      //
     C                   MOVE      Q1USER        RHIUSR
     C                   MOVE      Q1PGM         RHIPGM
      //
     C                   Z-ADD     DSTIME        RHITIM
      //
      /Free
       RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);
      /End-free
      //
      /FREE
          WRITE PRH01;
          //
       ENDSR;                                                                 //
       //===============================================================*
       //  CHKEFT
       //===============================================================*
       BEGSR CHKEFT;
          // If EFT payee, then determine if pre-notification record is required
      /END-FREE
     C                   MOVE      *OFF          ##EFT
     C                   MOVE      *OFF          ##PRE
     C                   MOVE      CMSS#         XXSS#
      //****RPCKTP        IFEQ      'EFT'
      /Free
       If RPeftc = 'E';
      /End-Free
      //
     C                   MOVEL(P)  RPFEIN        XXWK12           12
     C                   MOVE      RPMOD         XXWK12
     C                   MOVEL(P)  XXWK12        ##KEY
     C                   MOVE      'P'           ##PTYP
     C                   MOVE      RPFND         #1FND
     C                   MOVE      RPDT          ##DATE
      //
      /Free
       //Call the Procedure to retrieve EFT status.
       If Not GetEFTStatus(##co#: ##ptyp: #1fnd: ##key: ##date:
        ##pre:##OutRRN:##EobType:##cktp);
          RPeftc = *Blanks;
       EndIf;
      /End-Free
      //
      /FREE
       ENDIF;                                                                 //
       ENDSR;                                                                 //
       //===============================================================*
       //
       //===============================================================*
       //  EFTSLT - Write EFT RECORDS
       //===============================================================*
       //
       BEGSR EFTSLT;
          SELECT;
          WHEN ##EFT = *OFF
          OR ##EFT = *ON
          AND ##PRE = *OFF;
             // Not EFT payee OR
             // EFT payee but pre-notification is not required, write transactio
             WRITE PRP01;
          WHEN ##EFT = *ON
          AND ##PRE = *ON;
             // EFT payee and Pre-notification is required
      /END-FREE
     C                   Z-ADD     RPAMT         SVAMT
      // Create Pre-notification with RPAMT = 0
      /FREE
             CLEAR RPAMT;
             WRITE PRP01;
             // Create check request
      /END-FREE
     C                   Z-ADD     SVAMT         RPAMT
      /Free
       RPeftc = *Blanks;
      /End-Free
      /FREE
             WRITE PRP01;
          ENDSL;
       ENDSR;                                                                 //
       //===============================================================*
       //***********************************************************************
      /END-FREE
