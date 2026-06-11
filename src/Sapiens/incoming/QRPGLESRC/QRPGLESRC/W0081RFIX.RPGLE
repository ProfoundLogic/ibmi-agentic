     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H/TITLE W0081R - WCMC Check Issuance
     H*****************************************************************
     H* Description:  This program creates the payment request trans- *
     H*   actions for EOMBs.  Up to 3 records can be created, these   *
     H*   are; the provider payment, the bill review service payment, *
     H*   and the network payment.  This program only processes one   *
     H*   EOMB at a time (per call).                                  *
     H*                                                               *
     H* Called from:  F7071R1                                         *
     H* Calls to:     none                                            *
     H*                                                               *
     H* Created by:  C:15     Project: 48MBV      12/01/98            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H* 08/15/17 C:33  Project: SY394                                 *
     H*                Update initial reserve flag                    *
     H*                                                               *
     H* 08/04/06 C:15  Project: 135WHU                                *
     H*                Added code to write a payment adjustment record*
     H*                if the BRS or Network amount < 0.              *
     H*                                                               *
     H* 01/11/06 C:15  Support: WC129/50722                           *
     H*                Added code to clear the out of country address *
     H*                screen fields to prevent invalid data from     *
     H*                being loaded into other WARPP records.         *
     H*                                                               *
     H* 03/23/05 C:15  Project: WC1269                                *
     H*                Added code to load the Payee out of country    *
     H*                address.                                       *
     H*                                                               *
     H*  12/08/04 C:62 PROJECT: NY007                                 *
     H*                -ADD PUBLIC GOODS PROCESS                      *
     H*                                                               *
     H*  06/05/03 C:62 PROJECT: 129MBO                                *
     H*                -ASSIGN RPRMDS WITH EACNTL                     *
     H*                                                               *
     H*  05/28/03 C:62 PROJECT: 129MBO                                *
     H*                -DON'T ISSUE BRS CHECK IF THE PRODUCE BRS      *
     H*                 FLAG EQUALS 'N'                               *
     H*                -DON'T ISSUE NWS CHECK IF THE PRODUCE NWS      *
     H*                 FLAG EQUALS 'N'                               *
     H*                                                               *
     H*  04/16/03 C:62 PROJECT: MCMNT                                 *
     H*                CHECK TO SEE IF PROV CHK IS ISSUED IN BATCH    *
     H*                                                               *
     H*  02/07/03 C:62 PROJECT: MCMNT 246                             *
     H*                -CHK TO SEE IF PROVIDER CHECK IS A GROUP CHECK.*
     H*                -CHANGE HOW STATUS AND OTHER FIELDS POPULATED. *
     H*                                                               *
     H* 04/25/01 C:15  Project: WC1020                                *
     H*                Added code to retrieve the bank code for mult  *
     H*                bank account processing.                       *
     H*
     H*  09/28/00 C:62 PROJECT: MCMNT
     H*                ADD PROCESSING FOR "B" TYPES IN SRNTWK.
     H*                                                               *
     H*  02/25/00 C:15 Project: WC577                                 *
     H*                Added code to load new maint fields added to   *
     H*                WARPP.                                         *
     H*                                                               *
     H*  10/22/99 C:62 PROJECT: MC212                                 *
     H*                EAKY WAS USING THE WCMS CROSS REF GROUP        *
     H*
     H*  09/09/99 C:62 PROJECT: MC201
     H*                AUTO CHECK FOR NETWORK SERVICES.
     H*
     H*  07/27/99 C:62 PROJECT: MC201
     H*                REMOVE UPDATE OF FAEAP AND FAEOP.
     H*
     H*  03/23/99 C:94 PROJECT: MC194
     H*                LOAD RPPNAM, RPADR1, AND RPADR2 BASED ON
     H*                IF 'USED AS DBA' = Y OR N
     H*
     H*  03/18/99 C:94 PROJ: 25MKL
     H*                MOVE EORCN TO RPEOBS
     H*
     H*  03/12/99 C:94 PROJ:MC152
     H*                UPDATE EOSTAT WITH A 'P'
     H*
     H*  03/04/99 C:94 PROJ:MC152
     H*                CHANGE WHERE RPBR2 IS LOADED FOR BRS AND NWS
     H*                CHECKS
     H*
     H*  03/03/99 C:94 PROJ:MC152
     H*                ADD RCN # TO KEY
     H*
     H*  02/16/99 C:94 PROJ:25MKL                                     *
     H*                CHANGE FAEAL1 TO UPDATE EAAPPF AND EAEPFL      *
     H*                                                               *
     H*  01/29/99 C:94 PROJECT: 48MBV                                 *
     H*                CHANGE RPNTWK TO HOLD A 'B' IF A BRS CHECK     *
     H*                AND AND 'I' IF A NETWORK.                      *
     H*                                                               *
     H*****************************************************************
     H* Indicator usage:                                              *
     H*                                                               *
     H*  Note on *IN16, *IN17 and *IN18 with regard to Auto DARTS:    *
     H*  1) If *IN16 and *IN17 are *ON then Auto DARTS to W0070C50.   *
     H*  2) If *IN16 and *IN18 are *ON then Auto DARTS to F0350C.     *
     H*  *IN16  -  No provider payment. (RPAMT EQ ZERO).              *
     H*  *IN17  -  There is a BRS payment (RPAMT GT ZERO).            *
     H*  *IN18  -  No BRS payment (RPAMT EQ ZERO).                    *
     H*                                                               *
     H*  *IN60  -  General usage                                      *
     H*  *IN61  -  General usage                                      *
     H*****************************************************************
     FFAEAL1    IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     FFAEOL3    IF   E           K DISK
     FWMCIL     IF   E           K DISK
     FWMCML1    UF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWARPL     IF   E           K DISK
     FWTPSL     IF   E           K DISK
     FWARPL11   UF A E           K DISK
     F                                     RENAME(PRP01:PRP03)
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PB201)
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
     FFAESL     IF   E           K DISK
     FWDPAL     IF A E           K DISK
     FWDRPL     O    E           K DISK
     F                                     RENAME(PRP01:PRP02)
     FWDELL     UF   E           K DISK
     FWMRHP     O    E             DISK

     D*
     D*
     D DESC1           C                   CONST('Adjustment due to re-
     D                                     duced BRS fee')
     D*
     D DESC2           C                   CONST('Adjustment due to pr-
     D                                     ior denial of bill')
     D*
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
     D*
     D                 DS
     D  DATA                   1     50
     D  QXTRDT                 1      7  0
     D  QXFRDT                 8     14  0
     D  QXTODT                15     21  0
     D  QXTRCD                22     23
     D  QXRECD                24     25
     D  QXPV1                 26     27
     D  QXPV2                 28     29
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
     D*
      //***  Begin Add  ***  08/15/17  ************************************
      /copy wPrClmRsv
      //***  End   Add  ***  08/15/17  ************************************
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*=========================================================================
     C*  Process the initialization subroutine
     C*
     C                   EXSR      SRINIT
     C*
     C*  Retrieve the EOMB information
     C*
     C     EOKY          CHAIN     PEO01                              60
     C   60              GOTO      TERM
     C*
     C     EAKY          CHAIN     PEA01                              60
     C   60              GOTO      TERM
     C*
     C*  Retrieve the Claim record
     C*
     C     CMKY          CHAIN(N)  PCM01                              60
     C   60              GOTO      TERM
     C*
     C*  Determine Next sequence number for payment requests
     C*
     C     RPKY          SETGT     PRP01
     C     RPKY          READPE(N) PRP01                                  60
     C   60              Z-ADD     0             RPSEQ#
     C*
     C*  Process the subroutines which create the various payments.
     C*
     C     EASYS         IFEQ      'P'
     C     EASYS         OREQ      'H'
     C     EASYS         OREQ      'U'
     C     EASYS         OREQ      'M'
     C                   EXSR      SRBRS
     C                   ENDIF
     C     EASYS         IFEQ      'P'
     C     EASYS         OREQ      'H'
     C     EASYS         OREQ      'N'
     C                   EXSR      SRNTWK
     C                   ENDIF
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C******************************************************************
     C*  SRINIT - This subroutine is used to define and initialize
     C*    program variables.
     C******************************************************************
     C     SRINIT        BEGSR
     C*
     C*  KEY LIST FOR HEADER BILL AMOUNT FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C                   KFLD                    @@EOB#
     C                   KFLD                    @@RCN
     C*
     C     ESKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C                   KFLD                    @@EOB#
     C                   KFLD                    XXIDT
     C*
     C     ESKY2         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C                   KFLD                    @@EOB#
     C*
     C*  Key list for the WCMC Group Master file
     C*
     C     FNKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C*
     C*  Key list for the EOMB file
     C*
     C     EOKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@EOB#
     C*
     C*  Key list for the EOMB Auxiliary files
     C*
     C     EOKYA         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C                   KFLD                    @@EOB#
     C*
     C*  Key list for the WCMC Check Issuance file
     C*
     C     CIKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPTYP
     C                   KFLD                    XXSYS
     C*
     C*  Key list for the Claim Master file
     C*
     C     CMKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    XXFND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C*
     C*  Key list for the Request Payment file
     C*
     C     RPKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    XXFND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C*
     C     RPKYD         KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C                   KFLD                    RPEOB#
     C                   KFLD                    RPEOBS
     C                   KFLD                    RPFEIN
     C                   KFLD                    RPMOD
     C*
     C*  Key list for the Provider file
     C*
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  NETWORK REVIEW ID FILE
     C*
     C     NRKY          KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFEIN
     C                   KFLD                    RPMOD
     C*
     C*  Key list for the WCMS table files
     C*
     C     TBKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    XXKEY
     C*
     C     PAKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C*
     C     ELKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*
     C*  Entry parm list
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@EOB#            9 0
     C                   PARM                    @@RCN             3 0
     C                   PARM                    @@BAMT
     C                   PARM                    @@NAMT
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@CRB             1
     C                   PARM                    @@CRN             1
      *
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
     C                   PARM      *BLANK        SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDCY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANK        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *LIKE         DEFINE    RPCO#         XXCO#
     C     *LIKE         DEFINE    RPFND         XXFND
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    EOFEIN        XXFEIN
     C     *LIKE         DEFINE    EOMOD         XXMOD
     C     *LIKE         DEFINE    CIPTYP        XXPTYP
     C     *LIKE         DEFINE    CISYS         XXSYS
     C     *LIKE         DEFINE    PCKEY         XXKEY
     C     *LIKE         DEFINE    EANAMT        @@BAMT
     C     *LIKE         DEFINE    EANAMT        @@NAMT
     C     *LIKE         DEFINE    ESIDT         XXIDT
     C     *LIKE         DEFINE    PASEQ#        XXSEQ#
     C                   Z-ADD     1             X                 3 0
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@SUB#        XXSUB#
     C                   MOVE      @@EOB#        XXEOB#
     C*
     C*  Retrieve the WCMS group #.
     C*
     C                   Z-ADD     0             XXFND
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      FNWCF#        XXFND
     C                   ENDIF
     C*
     C*  Retrieve the Group Auxiliary record.
     C*
     C     FNKY          CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEA     *BLANKS       F1NW
     C                   MOVEA     *BLANKS       F1NF
     C                   MOVEA     *ZEROS        F1NI
     C                   MOVEA     *ZEROS        F1NM
     C                   MOVEA     *ZEROS        F1N$
     C                   MOVEA     *BLANKS       F1NT
     C                   ENDIF
     C*
     C                   CALL      'SA001R'
01   C                   PARM                    @@WCIF            1            WC INST
02   C                   PARM                    @@MCIF            1            MC INST
03   C                   PARM                    @@WCIN            1            WC INTERFACE
04   C                   PARM                    @@AR              1            MC AR INST
05   C                   PARM                    @@NTWK            1            NETWORK INST
06   C                   PARM                    @@UR              1            PRECERT INST
07   C                   PARM                    @@MED             1            MEDMGMT INST
08   C                   PARM                    @@EDI             1            EDI INST
09   C                   PARM                    @@WCDK            1            WC DARTS KEY
10   C                   PARM                    @@RHAB            1            REHAB INST
11   C                   PARM                    @@MSS             1            MS SUPPORT
12   C                   PARM                    @@FSCH            1            FEE SCHEDULE
13   C                   PARM                    @@PRVB            1            PROVIDER BILLS
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
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    @@CLT#            3
     C*
     C                   ENDSR
     C******************************************************************
     C*  SRBRS - This subroutine creates the bill review service pmt.
     C******************************************************************
     C     SRBRS         BEGSR
     C*
     C*
     C*  Retrieve the BRS WCMC check issuance record.
     C*
     C                   MOVE      'B'           XXPTYP
     C                   MOVE      EOSYS         XXSYS
     C     CIKY          CHAIN     PCI01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*
     C*  Create the payment request record.
     C*
     C                   Z-ADD     @@CO#         RPCO#
     C                   Z-ADD     XXFND         RPFND
     C                   Z-ADD     @@FYR         RPFYR
     C                   Z-ADD     @@CASE        RPCASE
     C                   ADD       1             RPSEQ#
     C*
     C                   Z-ADD     EOTRDT        RPTRDT
     C                   MOVE      CIFEIN        RPFEIN
     C                   MOVE      CIMOD         RPMOD
     C*
     C     EAKY          CHAIN     PEA01                              60
     C     *IN60         IFEQ      *ON
     C   60              GOTO      NXTB
     C                   ENDIF
     C*
     C                   Z-ADD     @@BAMT        RPAMT
     C*
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE      CIP1CV        RPBR2
     C                   ELSE
     C                   MOVE      CIBR2         RPBR2
     C                   ENDIF
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVE      CIBR7L        RPBR7
     C                   ELSE
     C                   MOVE      CIBR7N        RPBR7
     C                   ENDIF
     C*
     C                   MOVE      CMNCCI        RPNCCI
     C                   Z-ADD     EOFRDT        RPFRDT
     C                   Z-ADD     EOTODT        RPTODT
     C                   Z-ADD     0             RPWKS
     C                   Z-ADD     0             RPDAYS
     C*
     C     EOSYS         IFEQ      'P'
     C     EOSYS         OREQ      'H'
     C     EATRCD        IFNE      *BLANKS
     C                   MOVE      EATRCD        RPTRCD
     C                   ELSE
     C     CMCVFL        IFNE      'Y'
     C                   MOVEL(P)  CITRCD        RPTRCD
     C                   ELSE
     C                   MOVEL(P)  CITRCV        RPTRCD
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE      CITRCV        RPTRCD
     C                   ELSE
     C                   MOVE      CITRCD        RPTRCD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPTRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PCRECD        RPRECD
     C                   MOVE      PCTRTP        RPTYPE
     C                   MOVEL     PCDESC        RPDESC
     C                   ELSE
     C                   MOVE      *BLANKS       RPRECD
     C                   MOVE      *BLANKS       RPTYPE
     C                   MOVE      *BLANKS       RPDESC
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      RTTYPE        RPRETP
     C                   ELSE
     C                   MOVE      *BLANKS       RPRETP
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       RPRMDS
     C                   MOVE      *BLANKS       RPRMK1
     C                   MOVE      *BLANKS       RPRMK2
     C                   MOVE      *BLANKS       RPRMK3
     C*
     C                   MOVEL     CIFEIN        XXFEIN
     C                   MOVEL     CIMOD         XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C   60              GOTO      NXTB
     C     PDDBA1        IFNE      'Y'
     C                   MOVE      PDNAME        RPPNAM
     C                   MOVE      PDADR1        RPADR1
     C                   MOVE      PDADR2        RPADR2
     C                   ELSE
     C                   MOVE      PDADR1        RPPNAM
     C                   MOVE      PDADR2        RPADR1
     C                   MOVE      *BLANKS       RPADR2
     C                   ENDIF
     C                   MOVE      PDCTY         RPCTY
     C                   MOVE      PDST          RPST
     C                   MOVE      PDZIP         RPZIP
     C                   MOVE      *BLANKS       RPADR3
     C                   MOVE      *BLANKS       RPCTRY
     C     PDADR3        IFNE      *BLANK
     C                   MOVEL(P)  PDCTRY        RPCTRY
     C                   MOVEL(P)  PDADR3        RPADR3
     C                   END
     C*
     C                   Z-ADD     EOEOB#        RPEOB#
     C                   MOVE      'P'           RPSTAT
     C                   Z-ADD     0             RPBUL1
     C                   Z-ADD     0             RPCHK#
     C                   MOVE      'G'           RPPGTP
     C                   MOVE      *BLANK        RPOFF#
     C*
     C                   MOVE      EAUSER        RPUSER
     C                   MOVEL     *BLANKS       RPCKCN
     C                   MOVE      Q1PGM         RPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RPTIME
     C*
     C     EOSYS         IFEQ      'P'
     C     EOSYS         OREQ      'H'
     C     EARCN         IFEQ      *ZEROS
     C                   Z-ADD     EOCRDT        RPRCVD
     C                   ELSE
     C                   Z-ADD     *HIVAL        XXIDT
     C     ESKY          SETGT     PES01
     C     ESKY2         READPE    PES01                                  99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     ESRRDT        RPRCVD
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     EOTRDT        RPRCVD
     C                   ENDIF
     C*
     C                   Z-ADD     0             RPDUE
      *
      *  Call API to determine the Bank "Code".
      *
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
      *
     C                   CALL      'WA023R1'
     C                   PARM      '2'           ##MODE            1            Mode
     C                   PARM                    ##CO#             3            Company
     C                   PARM                    ##FND             3            Group #
     C                   PARM                    ##FYR             3            Year
     C                   PARM                    ##CASE            7            Case
     C                   PARM      'WC'          ##APPL            2            Applic Code
     C                   PARM      DATA          ##DATA           50            Selection criteria
     C                   PARM                    ##CKTP           10            Bank Acct Code
     C                   PARM                    ##RTCD            1            Return Code
      *
     C                   MOVE      ##CKTP        RPCKTP                         Bank Acct# Code
     C                   MOVE      *BLANK        RPACCT
     C*
     C     EOSYS         IFEQ      'P'
     C     EOSYS         OREQ      'H'
     C     EACKST        IFEQ      *BLANKS
     C                   MOVEL     CISTS         RPSTS
     C                   ELSE
     C     EACKST        IFEQ      'PY'
     C                   MOVEL     CISTS         RPSTS
     C                   ELSE
     C                   MOVE      EACKST        RPSTS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVE      CISTS         RPSTS
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPSTS         XXKEY
     C     TBKY          CHAIN     PPS01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PSSRT         RPSORT
     C                   ELSE
     C                   MOVE      *BLANKS       RPSORT
     C                   ENDIF
     C*
     C                   MOVE      'N'           RPSENT
     C                   Z-ADD     *ZERO         RPDSEQ
     C                   MOVEL     EAUSER        RPRQST
     C                   MOVE      *BLANKS       RPLTCD
     C                   MOVE      'B'           RPNTWK
     C                   Z-ADD     EARCN         RPEOBS
     C                   MOVE      *BLANKS       RPFNAL
     C                   Z-ADD     RPDT          RPIDT
     C                   Z-ADD     DSTIME        RPITM
     C                   MOVEL     RPUSER        RPIUSR
     C                   MOVEL     Q1PGM         RPIPGM
     C                   Z-ADD     0             RPRDT
     C                   Z-ADD     0             RPRTM
     C                   MOVE      *BLANKS       RPRUSR
     C                   MOVE      *BLANKS       RPRPGM
     C     RPSTAT        IFEQ      ' '
     C                   Z-ADD     RPDT          RPRDT
     C                   Z-ADD     DSTIME        RPRTM
     C                   MOVEL     RPUSER        RPRUSR
     C                   MOVEL     Q1PGM         RPRPGM
     C                   ENDIF
     C*
     C     @@CRB         IFEQ      'Y'
     C     RPAMT         IFGT      0
     C                   WRITE     PRP03
     C                   ENDIF
     C     EABAMT        IFEQ      *ZEROS
     C     RPKYD         DELETE    PRP03                              99
     C                   ENDIF
     C                   ENDIF
     C*
     C     RPAMT         IFNE      0
     C     @@CLT#        ANDEQ     '135'
     C     @@CRB         ANDNE     'Y'
     C*
     C     PAKY          SETGT     PPA01
     C     PAKY          READPE(N) PPA01                                  60
     C  N60              Z-ADD     PASEQ#        XXSEQ#
     C   60              Z-ADD     0             XXSEQ#
     C                   CLEAR                   PPA01
     C*
     C     XXSEQ#        ADD       1             PASEQ#
     C*
     C                   Z-ADD     RPAMT         PAAMT
     C                   Z-ADD     EOCO#         PACO#
     C                   Z-ADD     EOFND         PAFND
     C                   Z-ADD     EOFYR         PAFYR
     C                   Z-ADD     EOCASE        PACASE
     C                   Z-ADD     0             PACHK#
     C                   Z-ADD     0             PABASQ
     C                   Z-ADD     RPFEIN        PAFEIN
     C                   Z-ADD     RPMOD         PAMOD
     C*
     C                   Z-ADD     RPDT          PATRDT
     C                   Z-ADD     RPDT          PAPODT
     C                   Z-ADD     RPFRDT        PAFRDT
     C                   Z-ADD     RPTODT        PATODT
     C                   Z-ADD     0             PAWKS
     C                   Z-ADD     0             PADAYS
     C*
     C                   MOVE      RPBR2         PABR2
     C                   MOVE      RPBR7         PABR7
     C*
     C     *LOVAL        SETLL     PPC01
     C                   READ      PPC01                                  60
     C     *IN60         DOWEQ     '0'
     C     PCRECD        IFEQ      RPRECD
     C     PCTRTP        ANDEQ     'A'
     C                   LEAVE
     C                   ENDIF
     C                   READ      PPC01                                  60
     C                   ENDDO
     C*
     C  N60              MOVEL     PCKEY         PATRCD
     C   60              MOVE      RPTRCD        PATRCD
     C     RPAMT         IFLT      0
     C                   MOVEL(P)  DESC1         PADESC
     C                   ELSE
     C                   MOVEL(P)  DESC2         PADESC
     C                   ENDIF
     C                   MOVE      'A'           PATYPE
     C                   MOVE      RPRECD        PARECD
     C                   MOVE      RPRETP        PARETP
     C*                    MOVE *BLANK    PAADST
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
     C*
     C                   WRITE     PPA01
     C*
     C*  CREATE REQUEST PAYMENT DETAIL RECORD
     C*
     C                   Z-ADD     PATRDT        RPTRDT
     C                   Z-ADD     PASEQ#        RPSEQ#
     C                   MOVE      *BLANKS       RPSTAT
     C                   MOVEL     Q1USER        RPUSER
     C                   WRITE     PRP02
     C*
     C* PROCESS THE CLAIM MASTER RECORDS EFFECTED.
     C*
     C     PAKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     1             X
     C     PARECD        LOOKUP    RT(X)                                  60
     C*
     C     CMSTAT        IFEQ      'C'
     C     PAAMT         ORGT      RR(X)
     C                   EXSR      RSVHST
     C                   ELSE
     C                   SUB       PAAMT         RR(X)
     C                   ENDIF                                                  CMSTAT=C
     C                   ADD       PAAMT         AR(X)
     C*
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C                   UPDATE    PCM01
     C*
     C*   UPDATE THE EMPLOYER DETAIL FILE WITH THE OUTSTANDING
     C*   RESERVE & TOTAL CLAIMS PAID
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     CMSTAT        IFEQ      'O'
     C                   SUB       PAAMT         ELOUTS
     C                   ENDIF                                                  CMSTAT=O
     C                   ADD       PAAMT         ELTCLP
     C*
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C*
     C                   UPDATE    PEL01
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF
     C*
     C                   ENDIF                                                  EABAMT < 0
     C*
     C                   ENDIF                                                  *IN60 EQ *OFF
     C*
     C     NXTB          TAG
     C*
     C                   ENDSR
     C******************************************************************
     C*  SRNTWK - This subroutine creates the network payment.
     C******************************************************************
     C     SRNTWK        BEGSR
     C*
     C*  Create the payment request record.
     C*
     C                   Z-ADD     @@CO#         RPCO#
     C                   Z-ADD     XXFND         RPFND
     C                   Z-ADD     EOFYR         RPFYR
     C                   Z-ADD     EOCASE        RPCASE
     C                   ADD       1             RPSEQ#
     C                   Z-ADD     EOTRDT        RPTRDT
     C*
     C                   MOVEL     EONTWK        XXNWCD            5
     C*
     C                   Z-ADD     1             X
     C     XXNWCD        LOOKUP    F1NW(X)                                61
     C*
     C  N61              GOTO      NXTC
     C*
     C* IF IN-HOUSE NETWORK:
     C*
     C*
     C                   MOVE      'B'           XXPTYP
     C                   MOVE      'N'           XXSYS
     C*
     C* RETRIEVE WCMC CHECK ISSUANCE INFORMATION:
     C*
     C     CIKY          CHAIN     PCI01                              60
     C*
     C   60              GOTO      NXTC
     C*
     C                   Z-ADD     CIFEIN        RPFEIN
     C                   Z-ADD     CIMOD         RPMOD
     C*
     C* RETRIEVE BILLING AMOUNT:
     C*
     C                   Z-ADD     @@NAMT        RPAMT
     C*
     C* RETRIEVE BILLING AMOUNT:
     C*
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE      CIP1CV        RPBR2
     C                   ELSE
     C                   MOVE      CIBR2         RPBR2
     C                   ENDIF
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVE      CIBR7L        RPBR7
     C                   ELSE
     C                   MOVE      CIBR7N        RPBR7
     C                   ENDIF
     C*
     C                   MOVE      CMNCCI        RPNCCI
     C                   Z-ADD     EOFRDT        RPFRDT
     C                   Z-ADD     EOTODT        RPTODT
     C                   Z-ADD     0             RPWKS
     C                   Z-ADD     0             RPDAYS
     C     EATRCD        IFNE      *BLANKS
     C                   MOVE      EATRCD        RPTRCD
     C                   ELSE
     C     CMCVFL        IFNE      'Y'
     C                   MOVEL(P)  CITRCD        RPTRCD
     C                   ELSE
     C                   MOVEL(P)  CITRCV        RPTRCD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPTRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PCRECD        RPRECD
     C                   MOVE      PCTRTP        RPTYPE
     C                   MOVEL     PCDESC        RPDESC
     C                   ELSE
     C                   MOVE      *BLANKS       RPRECD
     C                   MOVE      *BLANKS       RPTYPE
     C                   MOVE      *BLANKS       RPDESC
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      RTTYPE        RPRETP
     C                   ELSE
     C                   MOVE      *BLANKS       RPRETP
     C                   ENDIF
     C*
     C                   MOVEL(P)  EACNTL        RPRMDS
     C                   MOVE      *BLANKS       RPRMK1
     C                   MOVE      *BLANKS       RPRMK2
     C                   MOVE      *BLANKS       RPRMK3
     C*
     C                   Z-ADD     RPFEIN        XXFEIN
     C                   Z-ADD     RPMOD         XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C   60              GOTO      NXTC
     C     PDDBA1        IFNE      'Y'
     C                   MOVE      PDNAME        RPPNAM
     C                   MOVE      PDADR1        RPADR1
     C                   MOVE      PDADR2        RPADR2
     C                   ELSE
     C                   MOVE      PDADR1        RPPNAM
     C                   MOVE      PDADR2        RPADR1
     C                   MOVE      *BLANKS       RPADR2
     C                   ENDIF
     C                   MOVE      PDCTY         RPCTY
     C                   MOVE      PDST          RPST
     C                   MOVE      PDZIP         RPZIP
     C                   MOVE      *BLANKS       RPADR3
     C                   MOVE      *BLANKS       RPCTRY
     C     PDADR3        IFNE      *BLANK
     C                   MOVEL(P)  PDCTRY        RPCTRY
     C                   MOVEL(P)  PDADR3        RPADR3
     C                   END
     C*
     C                   Z-ADD     EOEOB#        RPEOB#
     C                   MOVE      'P'           RPSTAT
     C                   Z-ADD     0             RPBUL1
     C                   Z-ADD     0             RPCHK#
     C                   MOVE      'G'           RPPGTP
     C                   MOVE      *BLANK        RPOFF#
     C*
     C                   MOVE      EAIUSR        RPUSER
     C                   MOVE      Q1PGM         RPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RPTIME
     C*
     C                   Z-ADD     EOCRDT        RPRCVD
     C                   Z-ADD     0             RPDUE
      *
      *  Call API to determine the Bank "Code".
      *
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
      *
     C                   CALL      'WA023R1'
     C                   PARM      '2'           ##MODE            1            Mode
     C                   PARM                    ##CO#             3            Company
     C                   PARM                    ##FND             3            Group #
     C                   PARM                    ##FYR             3            Year
     C                   PARM                    ##CASE            7            Case
     C                   PARM      'WC'          ##APPL            2            Applic Code
     C                   PARM      DATA          ##DATA           50            Selection criteria
     C                   PARM                    ##CKTP           10            Bank Acct Code
     C                   PARM                    ##RTCD            1            Return Code
      *
     C                   MOVE      ##CKTP        RPCKTP                         Bank Acct# Code
     C                   MOVE      *BLANK        RPACCT
     C     EACKST        IFEQ      *BLANKS
     C                   MOVEL     CISTS         RPSTS
     C                   ELSE
     C     EACKST        IFEQ      'PY'
     C                   MOVEL     CISTS         RPSTS
     C                   ELSE
     C                   MOVE      EACKST        RPSTS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPSTS         XXKEY
     C     TBKY          CHAIN     PPS01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PSSRT         RPSORT
     C                   ELSE
     C                   MOVE      *BLANKS       RPSORT
     C                   ENDIF
     C*
     C                   MOVE      'N'           RPSENT
     C                   Z-ADD     *ZERO         RPDSEQ
     C                   MOVE      EAIUSR        RPRQST
     C                   MOVE      *BLANKS       RPLTCD
     C     F1NF(X)       IFEQ      'I'
     C                   MOVE      'I'           RPNTWK
     C                   ELSE
     C                   MOVE      *BLANKS       RPNTWK
     C                   ENDIF
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
     C     RPSTAT        IFEQ      ' '
     C                   Z-ADD     RPDT          RPRDT
     C                   Z-ADD     DSTIME        RPRTM
     C                   MOVEL     RPUSER        RPRUSR
     C                   MOVEL     Q1PGM         RPRPGM
     C                   ENDIF
     C*
     C     @@CRN         IFEQ      'Y'
     C     RPAMT         IFGT      0
     C                   WRITE     PRP03
     C                   ENDIF
     C     EANAMT        IFEQ      *ZEROS
     C     RPKYD         DELETE    PRP03                              99
     C                   ENDIF
     C                   ENDIF
     C*
     C     RPAMT         IFNE      0
     C     @@CLT#        ANDEQ     '135'
     C     @@CRN         ANDNE     'Y'
     C*
     C     PAKY          SETGT     PPA01
     C     PAKY          READPE(N) PPA01                                  60
     C  N60              Z-ADD     PASEQ#        XXSEQ#
     C   60              Z-ADD     0             XXSEQ#
     C                   CLEAR                   PPA01
     C*
     C     XXSEQ#        ADD       1             PASEQ#
     C*
     C                   Z-ADD     RPAMT         PAAMT
     C                   Z-ADD     EOCO#         PACO#
     C                   Z-ADD     EOFND         PAFND
     C                   Z-ADD     EOFYR         PAFYR
     C                   Z-ADD     EOCASE        PACASE
     C                   Z-ADD     0             PACHK#
     C                   Z-ADD     0             PABASQ
     C                   Z-ADD     RPFEIN        PAFEIN
     C                   Z-ADD     RPMOD         PAMOD
     C*
     C                   Z-ADD     RPDT          PATRDT
     C                   Z-ADD     RPDT          PAPODT
     C                   Z-ADD     RPFRDT        PAFRDT
     C                   Z-ADD     RPTODT        PATODT
     C                   Z-ADD     0             PAWKS
     C                   Z-ADD     0             PADAYS
     C*
     C                   MOVE      RPBR2         PABR2
     C                   MOVE      RPBR7         PABR7
     C*
     C     *LOVAL        SETLL     PPC01
     C                   READ      PPC01                                  60
     C     *IN60         DOWEQ     '0'
     C     PCRECD        IFEQ      RPRECD
     C     PCTRTP        ANDEQ     'A'
     C                   LEAVE
     C                   ENDIF
     C                   READ      PPC01                                  60
     C                   ENDDO
     C*
     C  N60              MOVEL     PCKEY         PATRCD
     C   60              MOVE      RPTRCD        PATRCD
     C     RPAMT         IFLT      0
     C                   MOVEL(P)  DESC1         PADESC
     C                   ELSE
     C                   MOVEL(P)  DESC2         PADESC
     C                   ENDIF
     C                   MOVE      'A'           PATYPE
     C                   MOVE      RPRECD        PARECD
     C                   MOVE      RPRETP        PARETP
     C*                    MOVE *BLANK    PAADST
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
     C*
     C                   WRITE     PPA01
     C*
     C*  CREATE REQUEST PAYMENT DETAIL RECORD
     C*
     C                   Z-ADD     PASEQ#        RPSEQ#
     C                   MOVE      *BLANKS       RPSTAT
     C                   MOVEL     Q1USER        RPUSER
     C                   WRITE     PRP02
     C*
     C* PROCESS THE CLAIM MASTER RECORDS EFFECTED.
     C*
     C     PAKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     1             X
     C     PARECD        LOOKUP    RT(X)                                  60
     C*
     C     CMSTAT        IFEQ      'C'
     C     PAAMT         ORGT      RR(X)
     C                   EXSR      RSVHST
     C                   ELSE
     C                   SUB       PAAMT         RR(X)
     C                   ENDIF                                                  CMSTAT=C
     C                   ADD       PAAMT         AR(X)
     C*
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C                   UPDATE    PCM01
     C*
     C*   UPDATE THE EMPLOYER DETAIL FILE WITH THE OUTSTANDING
     C*   RESERVE & TOTAL CLAIMS PAID
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     CMSTAT        IFEQ      'O'
     C                   SUB       PAAMT         ELOUTS
     C                   ENDIF                                                  CMSTAT=O
     C                   ADD       PAAMT         ELTCLP
     C*
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C*
     C                   UPDATE    PEL01
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF
     C*
     C                   ENDIF                                                  RPAMT < 0
     C*
     C     NXTC          TAG
     C*
     C                   ENDSR
     C*===============================================================*
     C*  RSVHST - Write a reserve history record
     C*
     C*   A RESERVE HISTORY RECORD WILL BE WRITTEN IN THE AMOUNT OF THE
     C*   ADJUSTMENT TO KEEP CLAIM MSTR (WMCMP) AND RSRV HIST (WMRHP)
     C*   IN BALANCE.
     C*===============================================================*
     C     RSVHST        BEGSR
     C*
     C                   CLEAR                   PRH01
     C*
     C                   Z-ADD     CMCO#         RHCO#
     C                   Z-ADD     CMFND         RHFND
     C                   Z-ADD     CMFYR         RHFYR
     C                   Z-ADD     CMCASE        RHCASE
     C                   Z-ADD     1             Y                 3 0
     C     PARECD        LOOKUP    RT(Y)                                  99
     C                   MOVEL     PARECD        RRT(Y)
     C                   Z-ADD     PAAMT         RSU(Y)
     C     PAAMT         IFLT      0
     C                   MOVEL(P)  DESC1         RHEXPL
     C                   ELSE
     C                   MOVEL(P)  DESC2         RHEXPL
     C                   ENDIF
     C                   MOVE      Q1USER        RHUSER
     C                   MOVE      Q1PGM         RHPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RHDT
     C                   MOVEL     SRDTO         RHEFDT
     C                   Z-ADD     DSTIME        RHTIME
      //***  Begin Add  ***  08/15/17  ************************************
      /Free
       RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);
      /End-free
      //***  End   Add  ***  08/15/17  ************************************
     C*
     C                   WRITE     PRH01
     C*
     C                   ENDSR                                                  RSVHST
     C*===============================================================*
