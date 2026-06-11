     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4042RAL - PHARMACY
     H*    DESCRIPTION: ADJUDICATION (ALABAMA RULES - 02/15/95)
     H*   This program is called from F0070R (Claim Adjudication) and will
     H* perform the actual adjudication of the detail claim records writ-
     H* ten by F0070R.  This program was derrived from the ALABAMA State
     H* Fee Schedule Reimbursment Manual issued on 02/15/95
     H* This Sub-program is intended to be used to calculate the amount
     H* that is due a Provder of services, by the Carrier of the Work comp
     H* insurance.  It should be used for any serice provided on or after
     H* 02/15/95.
     H*****************************************************************
     H*  CREATED: 2/20/95   C:05   PROJECT: AL019
     H*****************************************************************
     H*  CONVERSION FACTORS USED IN THIS PROGRAM:
     H*     XXBPRC = 5.00  (Brand-Name Pharmaceuticals percentage)
     H*     XXGPRC = 5.00  (Generic Pharmaceuticals percentage)
     H*     XXBFEE = 5.54  (Dispensing Fee set by state of AL)
     H*     XXGFEE = 7.18  (Dispensing Fee set by state of AL)
     H*****************************************************************
     H*  SPECIALTY CODES USED IN THIS PROGRAM:
     H*****************************************************************
     H*  REVISIONS:
     H*  04/08/99 C:117 Support: MC095 Seq# 17568
     H*                 Added QQCO#, QQSUB#, QQFND to the *ENTRY PARMs
     H*                 and changed all occurences of UPCO#, UPSUB#,
     H*                 UPFND to the above mentioned fields.
     H*
     H*  09/04/98 C:31  Project: 95MAL
     H*                 Ntwk Disc:Lesser of FS/Chr
     H*                 1. Add update of CAMRAA
     H*
     H*  11/18/97 C:86  Project: MC145
     H*         CHECK ALL DATE FIELDS AND ROUTINES TO SEE IF THEY ARE
     H*         CORRECT FOR 7 DIGIT FILE FIELDS & 8 DIGIT SCREEN FIELDS
     H*
     H*  07/15/96 C:32 PROJ: AL028
     H*                ADDED ENDING DATE RANGE FOR RECORD PROCESSING
     H*                DUE TO NEW EFFECTIVE DATE WITH CHG IN RATES
     H*
     H*****************************************************************
     H*  INDICATOR USAGE:
     H*  54    Use with Primary FDCAP file (i.e. DOWEQ, etc on reading charges)
     H*  55    Use with Primary FDHCP file
     H*  60    General Use
     H*  61    General Use
     H*  62    General Use
     H*****************************************************************
     FFDCAL7    UF   E           K DISK
     FFDCALP    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0P)
     FFDHCL     UF   E           K DISK
     FFDHCL1    UF   E           K DISK
     F                                     RENAME(PHC01:PHC02)
     FFMCML1    IF   E           K DISK
     FFMRXL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFTABLAL   IF   E           K DISK
     F*

     D QQS             S              5    DIM(30)                              DETAIL SEQ #
     D QQC             S              2    DIM(30)                              ERROR MESSAGES
     D QQF             S              6    DIM(50)                              FROM DATES
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     I*
     I*  Rename the duplicate field names.
     IPCA0P
     I              CACO#                       PXCO#
     I              CASUB#                      PXSUB#
     I              CAFND                       PXFND
     I              CAFYR                       PXFYR
     I              CACASE                      PXCASE
     I              CAFDAT                      PXFDAT
     I              CADSEQ                      PXDSEQ
     I              CAFEIN                      PXFEIN
     I              CAMOD                       PXMOD
     I              CACPT4                      PXCPT4
     I              CATOTA                      PXTOTA
     I              CATOTD                      PXTOTD
     I              CATOTP                      PXTOTP
     I              CATOTC                      PXTOTC
     I              CAPMOD                      PXPMOD
     I              CAUSER                      PXUSER
     I              CAPGM                       PXPGM
     I              CADT                        PXDT
     I              CATIME                      PXTIME
     I              CACHK#                      PXCHK#
     I              CAEOB#                      PXEOB#
     I              CADIFF                      PXDIFF
     I              CAPART                      PXPART
     I              CADXR                       PXDXR
     I              CAFLAG                      PXFLAG
     I*
     IPHC02
     I              HCCO#                       H2CO#
     I              HCFND                       H2FND
     I              HCFYR                       H2FYR
     I              HCCASE                      H2CASE
     I              HCDSEQ                      H2DSEQ
     I              HCDESC                      H2DESC
     I              HCSTAT                      H2STAT
     I              HCUSER                      H2USER
     I              HCPGM                       H2PGM
     I              HCTIME                      H2TIME
     I              HCDT                        H2DT
     I              HCIUSR                      H2IUSR
     I              HCSUB#                      H2SUB#
     I              HCPOS                       H2POS
     I              HCNDC                       H2NDC
     I              HCRXFD                      H2RXFD
     I              HCRX#                       H2RX#
     I              HCRXMQ                      H2RXMQ
     I              HCRXMM                      H2RXMM
     I              HCRXDY                      H2RXDY
     I              HC1A1                       H21A1
     I              HC1A2                       H21A2
     I              HC1A3                       H21A3
     I              HC1A4                       H21A4
     I              HC1A5                       H21A5
     I              HC6N1                       H26N1
     I              HC6N2                       H26N2
     I              HC6N3                       H26N3
     I              HC6N4                       H26N4
     I              HC6N5                       H26N5
     I              HC52N1                      H252N1
     I              HC52N2                      H252N2
     I              HC52N3                      H252N3
     I              HC52N4                      H252N4
     I              HC52N5                      H252N5
     I              HC92N1                      H292N1
     I              HC92N2                      H292N2
     I              HC92N3                      H292N3
     I              HC92N4                      H292N4
     I              HC92N5                      H292N5
     I              HC10A1                      H210A1
     I              HC10A2                      H210A2
     I              HC10A3                      H210A3
     I              HC10A4                      H210A4
     I              HC10A5                      H210A5
     I              HC40A1                      H240A1
     I              HCEO01                      H2EO01
     I              HCEO02                      H2EO02
     I              HCEO03                      H2EO03
     I              HCEO04                      H2EO04
     I              HCEO05                      H2EO05
     I              HCEO06                      H2EO06
     I              HCEO07                      H2EO07
     I              HCEO08                      H2EO08
     I              HCEO09                      H2EO09
     I              HCEO10                      H2EO10
     I              HCBRNC                      H2BRNC

     C*
     C*  Claim Detail File
     C     CAKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C*
     C     CAKY1A        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C                   KFLD                    SVPRTY
     C                   KFLD                    SVDSEQ
     C                   KFLD                    SVEOB#
     C                   KFLD                    SVCHK#
     C*
     C     CAKYP         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C*
     C*  Detail Charge Auxiliary File
     C     HCKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    CADSEQ
     C*
     C     HCKY2         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    PXDSEQ
     C                   KFLD                    HCNDC
     C*
     C*  Claim Master File
     C     CMKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C*
     C*  Claim Status Code Table File
     C     TCSKY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    TABKEY
     C*
     C*  Dummy key used for release of all CA records
     C     DUMMY         KLIST
     C                   KFLD                    QQCO#
     C*
     C*  NDC Drug File
     C     RXKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    HCNDC
     C*
     C*  User Parameter File
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQFDT           150
     C                   PARM                    QQFDT2          150
     C                   PARM                    QQS
     C                   PARM                    QQC
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQSS#             9
     C                   PARM                    QQACDT            8 0          (MMDDYY ORD)
     C                   PARM                    QQSPCD            3
     C*** BEG ADD *** 04/08/99 ***************************************
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQSUB#            5 0
     C                   PARM                    QQFND             3 0
     C*** END ADD *** 04/08/99 ***************************************
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
     C     QQRTCD        CABEQ     'P'           TERM                           PRE-OPEN FILES
     C*
     C                   MOVEA     QQFDT         QQF(1)
     C                   MOVEA     QQFDT2        QQF(26)
     C*
     C* = Initialization Section ======================================
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C***********          MOVELSRDTO     SYDATE  60
     C                   MOVEL     SRDTO         SYDATE            7 0
     C*** END CHANGE ************** 11/18/97 *****************************
     C*
     C                   Z-ADD     0             Q                 3 0
     C                   Z-ADD     0             S                 3 0
     C                   Z-ADD     *ZERO         XXMRA             9 2
     C                   Z-ADD     1.05          XXBPRC            3 2
     C                   Z-ADD     1.05          XXGPRC            3 2
     C                   Z-ADD     5.54          XXBFEE            3 2
     C                   Z-ADD     7.18          XXGFEE            3 2
     C                   Z-ADD     0             XXOVRP            3 2
     C*
     C     *LIKE         DEFINE    CACHK#        SVCHK#
     C     *LIKE         DEFINE    CADSEQ        SVDSEQ
     C     *LIKE         DEFINE    CAEOB#        SVEOB#
     C     *LIKE         DEFINE    CAFEIN        SVFEIN
     C     *LIKE         DEFINE    CAMOD         SVMOD
     C     *LIKE         DEFINE    CAPRTY        SVPRTY
     C     *LIKE         DEFINE    HCNDC         SVNDC
     C*
     C     *LIKE         DEFINE    CSKEY         TABKEY
     C*
     C     *LIKE         DEFINE    CAEOMB        XPEOMB
     C     *LIKE         DEFINE    CATOTA        XPTOTA
     C     *LIKE         DEFINE    CATOTD        XPTOTP
     C     *LIKE         DEFINE    CATOTP        XPTOTD
     C     *LIKE         DEFINE    HCRXMQ        XXRXMQ
     C*
     C     *LIKE         DEFINE    CAFDAT        XXDATE
     C     *LIKE         DEFINE    CAFDAT        XXYMD
     C*
     C* ===============================================================
     C*
     C*  Get the user parameter file
     C     UPKY          CHAIN     PUP01                              60
     C*
     C*  If the get was unsuccessful, end the program
     C     *IN60         IFEQ      '1'
     C                   MOVE      '99999'       QQS(1)
     C                   MOVE      '95'          QQC(1)
     C* Err Msg 95: "Invalid User Parameter, See Programmer"
     C                   GOTO      TERM
     C                   END
     C*
     C*  Get the Claim Master Record
     C     CMKY          CHAIN     PCM01                              60
     C*
     C*  If the get was unsuccessful, end the program
     C     *IN60         IFEQ      '1'
     C                   MOVE      '99998'       QQS(1)
     C                   MOVE      '96'          QQC(1)
     C* Err Msg 96: Invalid Case Number, See Programmer
     C                   GOTO      TERM
     C                   END
     C*
     C*** Do on each date brought in ********************************
     C*
     C*  Do adjudication for each possible date entered,if after 950215
     C*                                            and less than 960215
     C                   DO        50            S
     C*** BEGIN CHANGE************* 11/18/97 *****************************
     C**********           MOVELQQF,S     XXDATE
     C                   MOVE      QQF(S)        XXDATE
     C*** END   CHANGE************* 11/18/97 *****************************
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C     QQF(S)        IFNE      *ZERO
     C     XXYMD         ANDGE     950215
     C     XXYMD         ANDLT     960215
     C     CAKY1         CHAIN     PCA01                              54
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ
     C                   MOVE      CAFEIN        SVFEIN
     C                   MOVE      CAMOD         SVMOD
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C*
     C*** DOWEQ on FDCAP file****************************************
     C*
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
     C                   EXCEPT    RCA01
     C     CARCFL        IFNE      'X'
     C                   GOTO      NXTRCD
     C                   END
     C*
     C* Get corresponding Detail Charge Auxiliary record (FDHCP)
     C     HCKY          CHAIN     PHC01                              55
     C     *IN55         IFEQ      '0'
     C                   MOVE      HCNDC         SVNDC
     C                   Z-ADD     HCRXMQ        XXRXMQ
     C                   EXCEPT    RHC01
     C                   END                                                     END 0 *IN55
     C*
     C* If the Service Date is greater than the system date, give err message
     C     CAFDAT        IFGT      SYDATE
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '79'          QQC(Q)
     C*** Err Msg 79: "Service Date entered is greater than today's date
     C*                (system date)."
     C                   END                                                    END EQ *ZEROS
     C                   END                                                    END GT SYDATE
     C*
     C*  Get the claim status record.  See if Override Flag is 'Y'.
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C*
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '97'          QQC(Q)
     C* Err Msg 97: "Invalid Claim Status Code, See Programmer"
     C                   END
     C*
     C*** NOT OVERRIDE ************************************************
     C*
     C* If override flag is not equal to 'Y', then process this record
     C*    Else do not adjudicate.
     C*
     C     CSOVRD        IFNE      'Y'
     C*
     C*== =============================================================
     C*
     C*== Hospital ====================================================
     C*
     C*   Hospital Subroutine or Regular Providers.
     C     CAPRTY        IFEQ      '4'
     C                   GOTO      NXTRCD
     C*
     C*== Non-Hospital ================================================
     C*
     C                   ELSE                                                   Else eq '4'
     C*
     C*== Get RX Fields ===============================================
     C*
     C*== Normal Processing of Procedure Code =========================
     C*
     C* Get NDC Drug File Information
     C     RXKY          SETGT     PRX01
     C     NXTRX         TAG
     C                   READP     PRX01                                  60
     C     *IN60         IFEQ      '1'
     C     RXNDC         ORNE      HCNDC
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '94'          QQC(Q)                         INVALID PROC CODE
     C* Err Msg # 94: Invalid Pharmacy NDC Code
     C                   GOTO      TERM
     C                   END
     C     RXSTAT        CABEQ     'I'           NXTRX
     C*
     C* If NDC code is successfully found, then continue with processing
     C*
     C                   Z-ADD     *ZERO         XXMRA
     C* Brand-name Pharmaceuticals
     C     RXPTYP        IFEQ      '01'
     C     RXPTYP        OREQ      '07'
     C*
     C* DETERMINE IF FILL DATE IS LESS THAN AWP EFFECTIVE DATE IF SO
     C* USE PREVIOUS AWP UNIT PRICE ELSE USE CURRENT AWP UNIT PRICE
     C*
     C     CAFDAT        IFLT      RXAWPD
     C     RX1AWP        MULT(H)   XXBPRC        XXMRA
     C     XXMRA         MULT(H)   XXRXMQ        XXMRA
     C     XXMRA         ADD       XXBFEE        XXMRA
     C                   ELSE
     C     RXAWP         MULT(H)   XXBPRC        XXMRA
     C     XXMRA         MULT(H)   XXRXMQ        XXMRA
     C     XXMRA         ADD       XXBFEE        XXMRA
     C                   END
     C*
     C                   ELSE                                                   ELSE EQ '01'
     C*
     C* Generic Pharmaceuticals
     C* DETERMINE IF FILL DATE IS LESS THAN AWP EFFECTIVE DATE IF SO
     C* USE PREVIOUS AWP UNIT PRICE ELSE USE CURRENT AWP UNIT PRICE
     C*
     C     CAFDAT        IFLT      RXAWPD
     C     RX1AWP        MULT(H)   XXGPRC        XXMRA
     C     XXMRA         MULT(H)   XXRXMQ        XXMRA
     C     XXMRA         ADD       XXGFEE        XXMRA
     C                   ELSE
     C     RXAWP         MULT(H)   XXGPRC        XXMRA
     C     XXMRA         MULT(H)   XXRXMQ        XXMRA
     C     XXMRA         ADD       XXGFEE        XXMRA
     C                   END
     C*
     C                   END                                                    END EQ '01'
     C*
     C*** Determine lesser of calculations
     C     CATOTC        COMP      XXMRA                              60
     C   60              Z-ADD     XXMRA         XPTOTA
     C  N60              Z-ADD     CATOTC        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*** Determine lesser of calculations
     C                   EXSR      UPDAT
     C*
     C                   END                                                    END EQ '4'
     C*
     C*** ELSE IS OVERRIDE ********************************************
     C                   ELSE                                                   OVERRIDE TYPE
     C*
     C*  Test for a possible duplicate claim.
     C     CAKYP         CHAIN     PCA0P                              60
     C     DUPCL1        TAG
     C*
     C     *IN60         IFEQ      '0'
     C*
     C     CARCFL        IFEQ      'X'
     C*
     C     CADSEQ        IFNE      PXDSEQ
     C     CADSEQ        OREQ      PXDSEQ
     C     CAEOB#        ANDNE     PXEOB#
     C*
     C     HCKY2         CHAIN     PHC02                              61
     C     *IN61         IFEQ      '0'
     C                   EXCEPT    RHC02
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 62
     C  N62              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C*
     C                   ELSE                                                    ELSE CARCFL
     C     CAKYP         READE     PCA0P                                  60
     C  N60              GOTO      DUPCL1
     C                   END                                                     END CARCFL
     C*
     C                   ELSE                                                    ELSE 0 *IN61
     C*
     C     CAKYP         READE     PCA0P                                  60
     C  N60              GOTO      DUPCL1
     C                   END                                                     END 0 *IN61
     C                   END                                                     END NE EOB#
     C                   END                                                     END 0 *IN60
     C*
     C     CAKY1A        CHAIN     PCA01                              60
     C     *IN60         IFEQ      '0'
     C*   If override type, we must still output the allowed/disallowed
     C     CAEOB#        IFEQ      *ZERO
     C     CATOTD        ANDEQ     0
     C     CAOTHR        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CATOTA        ANDEQ     0
     C     CATOTP        ANDEQ     0
     C*
     C*   If there is an override percentage, calculate
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CATOTC        MULT      XXOVRP        CATOTA
     C                   END
     C*
     C     CATOTC        SUB       CATOTA        CATOTD
     C                   Z-ADD     CATOTA        CATOTP
     C                   END
     C*
     C                   MOVE      Q1USER        CAUSER
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   Z-ADD     DSTIME        CATIME
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA01
     C*
     C                   END                                                    END CAEOB# EQ ZERO
     C                   END                                                    END EQ OVRRD
     C*
     C*** Process next record *****************************************
     C     NXTRCD        TAG
     C*
     C     CAKY1         READE     PCA01                                  54      GET NEXT RCD
     C*                                                      TO PROCESS
     C*
     C     *IN54         IFEQ      '0'                                          SAVE FIELDS OF
     C                   Z-ADD     CADSEQ        SVDSEQ                         CURRENT REC.
     C                   MOVE      CAFEIN        SVFEIN
     C                   MOVE      CAMOD         SVMOD
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C*
     C* Get corresponding Detail Charge Auxiliary record (FDHCP)
     C     HCKY          CHAIN     PHC01                              55
     C     *IN55         IFEQ      '0'
     C                   MOVE      HCNDC         SVNDC
     C                   EXCEPT    RHC01
     C                   END                                                    END 0 *IN55
     C*
     C                   END                                                    END *IN54 EQ 0
     C*
     C                   END                                                    END DOWEQ 0 *IN54
     C*
     C                   END                                                    END QQF,S NE *ZERO
     C*
     C                   END                                                    END DO 50
     C*
     C*** End the program *********************************************
     C     TERM          TAG
     C*
     C*  Release all the formats for a save return.
     C*
     C     DUMMY         CHAIN     PCA01                              54
     C  N54              EXCEPT    RCA01
     C*
     C                   RETURN
     C                   MOVE      '1'           *INLR
     *********************************************************************
     *** UPDATE SUBROUTINE                                     U P D A T *
     *********************************************************************
     C     UPDAT         BEGSR
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** CHARGES EXCEED MAXIMUM ALLOWANCE FOUND IN REIMBURSEMENT MANUAL
     C*
     C     XPTOTD        IFGT      *ZERO
     C     XPTOTP        ANDGT     *ZEROS
     C     XPEOMB        ANDEQ     *BLANK
     C                   MOVE      '002'         XPEOMB
     C*** EOMB 002: "Charges exceed Maximum Allowance"
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM.
     C*
     C     CAKYP         CHAIN     PCA0P                              60
     C     DUPCL2        TAG
     C*
     C     *IN60         IFEQ      '0'
     C*
     C     CARCFL        IFEQ      'X'
     C*
     C     CADSEQ        IFNE      PXDSEQ
     C     CADSEQ        OREQ      PXDSEQ
     C     CAEOB#        ANDNE     PXEOB#
     C*
     C     HCKY2         CHAIN     PHC02                              61
     C     *IN61         IFEQ      '0'
     C                   EXCEPT    RHC02
     C*
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 62
     C  N62              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C                   END                                                      END EOB# EQ *ZERO
     C*
     C                   ELSE                                                    ELSE CARCFL
     C     CAKYP         READE     PCA0P                                  60
     C  N60              GOTO      DUPCL2
     C                   END                                                     END CARCFL
     C*
     C                   ELSE                                                     ELSE 0 *IN61
     C*
     C     CAKYP         READE     PCA0P                                  60
     C  N60              GOTO      DUPCL2
     C                   END                                                      END 0 *IN61
     C                   END                                                      END NE EOB#
     C                   END                                                      END 0 *IN60
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** ONLY PROCESS THE RECORD IF NOT A OVERRIDE TYPE.
     C     CAKY1A        CHAIN     PCA01                              60
     C     CSOVRD        IFNE      'Y'
     C     CACHK#        ANDEQ     *ZERO
     C     CAEOB#        ANDEQ     *ZERO
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPTOTD        CATOTD
     C                   Z-ADD     XPTOTP        CATOTP
     C                   MOVE      XPEOMB        CAEOMB
     C*
     C                   MOVE      Q1USER        CAUSER
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   Z-ADD     DSTIME        CATIME
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C  N60              UPDATE    PCA01
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C* Clear work fields
     C                   MOVE      *BLANKS       XPEOMB
     C                   MOVE      *ZEROS        XPTOTA
     C                   MOVE      *ZEROS        XPTOTP
     C                   MOVE      *ZEROS        XPTOTD
     C*
     C     UPDAE         ENDSR
     *********************************************************************
     OPCA01     E            RCA01
     OPHC01     E            RHC01
     OPHC02     E            RHC02
