     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4041NTX - PHARMACY
     H*    DESCRIPTION: ADJUDICATION (TEXAS RULES - 08/01/91)
     H*   This program is called from F0070R (Claim Adjudication) and will
     H* perform the actual adjudication of the detail claim records writ-
     H* ten by F0070R.  This program was derived from the Texas State
     H* Fee Schedule Reimbursment Manual issued on 08/01/91.
     H* This Sub-program is intended to be used to calculate the amount
     H* that is due a Provder of services, by the Carrier of the Work comp
     H* insurance.  It should be used for any serice provided on or after
     H* 03/01/02.
     H*****************************************************************
     H*  CREATED: 3/1/2002 C:79 Project: TX016
     H*****************************************************************
     H*  CONVERSION FACTORS USED IN THIS PROGRAM:
     H*     XXBPRC = 1.09  (Brand-Name Pharmaceuticals percentage)
     H*     XXGPRC = 1.25  (Generic    Pharmaceuticals percentage)
     H*     XXBDFE = 4.00  (Brand-Name Dispensing Fee set by State of TX)
     H*     XXGDFE = 4.00  (Generic    Dispensing Fee set by State of TX)
     H*****************************************************************
     H*  SPECIALTY CODES USED IN THIS PROGRAM:
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  09/25/01 C:062 PROJECT: FSMNT
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.
     H*
     H*  04/08/99 C:117 Support: MC095 Seq# 17568
     H*                 Added QQCO#, QQSUB#, QQFND to the *ENTRY PARMs
     H*                 and changed all occurences of UPCO#, UPSUB#,
     H*                 UPFND to the above mentioned fields.
     H*
     H*  09/21/98 C:79  Project: MC152                                *
     H*                 1.  Make Dates year 2000 compatible.          *
     H*                 2.  Modify to work with New Provider bills    *
     H*                 3.  Use new FDCAP fields CACHRG, CAPAID, CAMRA*
     H*
     H*  09/04/98 C:31  Project: 95MAL
     H*                 Ntwk Disc:Lesser of FS/Chr
     H*                 1. Add update of CAMRAA
     H*
     H*  08/31/95 C:32  PROJ: TX001
     H*                 CHG EOMB# FROM NUMBERS TO LETTERS
     H*                 AND ERROR MESSAGES FOR THE EOMBS
     H*  11/15/95 C:20  PROJ: TX001
     H*                 1. MODIFICATION TO CALCULATE THE SIGNLE UNIT PRICE
     H*                    BEFORE MULTIPLYING OTHER FACTORS
     H*  11/28/95 C:05  PROJ: TX001
     H*                 1. From RedBook, found out AWP is the PER UNIT price,
     H*                    and we have it in a 9.2 field, but it should be in
     H*                    a 9.5 field.  Therefore, we have to divide   be in
     H*                    by 1000 before use. (XXAWP 9.5)              be in
     H*                 2. Removed RXPSZE field...not needed since we can use
     H*                    actual Unit Price field (RXAWP/RX1AWP div by 1000)
     H*                 3. Replaced certain XXMRA (9.2) workfields with
     H*                    XXMRAT (12.5) in order to properly calculate the
     H*                    cents for payment
     H*                 4. Half-adjusted XXMRA in order to properly
     H*                    calculate cents for payment
     H*****************************************************************
     H*  INDICATOR USAGE:
     H*  54    Use with Primary FDCAP file (i.e. DOWEQ, etc on reading charges)
     H*  55    Use with Primary FDHCP file
     H*  60    General Use
     H*  61    General Use
     H*  62    General Use
     H*****************************************************************
     F*** BEG DEL *** 09/21/99 ****************************************
     F**********FDCAL7  UF  E           K        DISK
     F*** END DEL *** 09/21/99 ****************************************
     FFDCALP    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0P)
     F*** BEG DEL *** 09/21/98 ****************************************
     F*********** FDHCL   UF  E           K        DISK
     F*********** FDHCL1  UF  E           K        DISK
     F*********** PHC01                             KRENAMEPHC02
     F*** END DEL *** 09/21/98 ****************************************
     FFMCML1    IF   E           K DISK
     FFMRXL     IF   E           K DISK
     FFTABLTX   IF   E           K DISK
     FSMUPL     IF   E           K DISK
     F*** BEG ADD *** 09/21/98 ****************************************
     F* Message file logicals
     F*
     FFDMGL1    UF A E           K DISK
     FFDMGL5    IF   E           K DISK
     F                                     RENAME(PMG01:PMG02)
     F*  FDCAL13 is the Primary File Read (PCA01)
     FFDCAL13   UF   E           K DISK
     F                                     RENAME(PCA01:PCA13)
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDDEL4    UF A E           K DISK
     FFAEOL4    IF   E           K DISK
     FFDDPL     UF   E           K DISK
     FFDDPL1    UF   E           K DISK
     F                                     RENAME(PDP01:PDP02)
      *** END ADD *** 09/21/98 ****************************************

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     F*** END ADD *** 09/21/98 ****************************************
     F*
      *** BEG DEL *** 09/21/98 ****************************************

     I******************* QQS        30  5               DETAIL SEQ #
     I******************* QQC        30  2               ERROR MESSAGES
     I******************* QQF        50  6               FROM DATES
      *** END DEL *** 09/21/98 ****************************************
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
     I              CAMRA                       PXTOTD
     I              CAPAID                      PXTOTP
     I              CACHRG                      PXTOTC
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
      *** BEG DEL *** 09/21/98 ****************************************
     I************* PHC02
      *************
     I************* HCCO#                           H2CO#
     I************* HCFND                           H2FND
     I************* HCFYR                           H2FYR
     I************* HCCASE                          H2CASE
     I************* HCDSEQ                          H2DSEQ
     I************* HCDESC                          H2DESC
     I************* HCSTAT                          H2STAT
     I************* HCUSER                          H2USER
     I************* HCPGM                           H2PGM
     I************* HCTIME                          H2TIME
     I************* HCDT                            H2DT
     I************* HCIUSR                          H2IUSR
     I************* HCSUB#                          H2SUB#
     I************* HCPOS                           H2POS
     I************* HCNDC                           H2NDC
     I************* HCRXFD                          H2RXFD
     I************* HCRX#                           H2RX#
     I************* HCRXMQ                          H2RXMQ
     I************* HCRXMM                          H2RXMM
     I************* HCRXDY                          H2RXDY
     I************* HC1A1                           H21A1
     I************* HC1A2                           H21A2
     I************* HC1A3                           H21A3
     I************* HC1A4                           H21A4
     I************* HC1A5                           H21A5
     I************* HC6N1                           H26N1
     I************* HC6N2                           H26N2
     I************* HC6N3                           H26N3
     I************* HC6N4                           H26N4
     I************* HC6N5                           H26N5
     I************* HC52N1                          H252N1
     I************* HC52N2                          H252N2
     I************* HC52N3                          H252N3
     I************* HC52N4                          H252N4
     I************* HC52N5                          H252N5
     I************* HC92N1                          H292N1
     I************* HC92N2                          H292N2
     I************* HC92N3                          H292N3
     I************* HC92N4                          H292N4
     I************* HC92N5                          H292N5
     I************* HC10A1                          H210A1
     I************* HC10A2                          H210A2
     I************* HC10A3                          H210A3
     I************* HC10A4                          H210A4
     I************* HC10A5                          H210A5
     I************* HC40A1                          H240A1
     I************* HCEO01                          H2EO01
     I************* HCEO02                          H2EO02
     I************* HCEO03                          H2EO03
     I************* HCEO04                          H2EO04
     I************* HCEO05                          H2EO05
     I************* HCEO06                          H2EO06
     I************* HCEO07                          H2EO07
     I************* HCEO08                          H2EO08
     I************* HCEO09                          H2EO09
     I************* HCEO10                          H2EO10
     I************* HCBRNC                          H2BRNC
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     IPDP02
     I              DPCO#                       D1CO#
     I              DPSUB#                      D1SUB
     I              DPFND                       D1FND
     I              DPFYR                       D1FYR
     I              DPCASE                      D1CASE
     I              DPSS#                       D1SS#
     I              DPACDT                      D1ACDT
     I              DPEOB#                      D1EOB#
     I              DPDSEQ                      D1DSEQ
     I              DPRX#                       D1RX#
     I              DPNDC                       D1NDC
     I              DPRXMQ                      D1RXMQ
     I              DPRXMM                      D1RXMM
     I              DPRXDY                      D1RXDY
     I              DPSTAT                      D1STAT
     I              DPUSER                      D1USER
     I              DPPGM                       D1PGM
     I              DPTIME                      D1TIME
     I              DPDT                        D1DT
     I              DPIUSR                      D1IUSR
     I              DPIPGM                      D1PGM
     I              DPITIM                      D1ITIM
     I              DPIDT                       D1IDT
     I              DPDESC                      D1DESC
     I              DPRXDI                      D1RXDI
     I              DPRXGB                      D1RXGB
     I              DPRXRF                      D1RXRF
     I              DPRXFD                      D1RXFD
     I              DPUNTS                      D1UNTS

     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***Claim*Detail File
     C*********  CAKY1     KLIST
     C*********            KFLD           QQCO#
     C*********            KFLD           QQSUB#
     C*********            KFLD           QQFND
     C*********            KFLD           QQFYR
     C*********            KFLD           QQCASE
     C*********            KFLD           XXYMD
      *** END DEL *** 09/21/98 ****************************************
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
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** KFLD           SVCHK#
      *** END DEL *** 09/21/98 ****************************************
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
     *** BEG DEL *** 09/21/98 *****************************************
     C***Detail*Charge Auxiliary File
     C********** HCKY      KLIST
     C**********           KFLD           QQCO#
     C**********           KFLD           QQSUB#
     C**********           KFLD           QQFND
     C**********           KFLD           QQFYR
     C**********           KFLD           QQCASE
     C**********           KFLD           CADSEQ
     C**********
     C********** HCKY2     KLIST
     C**********           KFLD           QQCO#
     C**********           KFLD           QQSUB#
     C**********           KFLD           QQFND
     C**********           KFLD           QQFYR
     C**********           KFLD           QQCASE
     C**********           KFLD           PXDSEQ
     C**********           KFLD           HCNDC
     *** END DEL *** 09/21/98 *****************************************
     C*
     *** BEG ADD *** 09/21/98 *****************************************
     C*  Pharmacy auxiliary detail
     C     DPKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    CADSEQ
      *
     C*  Pharmacy auxiliary detail
     C     DPKY2         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    KKDSEQ
     C                   KFLD                    DPNDC
     *** END ADD *** 09/21/98 *****************************************
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
      *** BEG DEL *** 09/21/98 ****************************************
     C********** RXKY      KLIST
     C**********           KFLD           QQCO#
     C**********           KFLD           HCNDC
      *** END DEL *** 09/21/98 ****************************************
     C*
      *** BEG ADD *** 09/21/98 ****************************************
     C*  NDC Drug File
     C     RXKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    DPNDC
      *** END ADD *** 09/21/98 ****************************************
     C*
     C*  User Parameter File
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*** BEG ADD *** 09/21/98 ****************************************
     C*
     C* Adjudication Message File (PMG01)
     C*
     C     MGKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXDSEQ
     C*
     C* Adjudication Message File (PMG02)
     C*
     C     MGKY2         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXDSEQ
     C                   KFLD                    XXMSG#
     C                   KFLD                    XXRCN
      *
     C     CAKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXYMD
     C*
     C     CAKY2         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*
     C     DEKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    QQDSEQ
     C                   KFLD                    QQESRC
     C*
     C     DEKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    QQDSEQ
     C*
     C     EBKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    EBKEY
     C*
     C     EOKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*** END ADD *** 09/21/98 ****************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** PARM           QQFDT 150
     C******************** PARM           QQFDT2150
     C******************** PARM           QQS
     C******************** PARM           QQC
      *** END DEL *** 09/21/98 ****************************************
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQSS#             9
     C                   PARM                    QQACDT            8 0          (MMDDYY ORD)
     C                   PARM                    QQSPCD            3
      *** BEG ADD *** 09/21/98 ****************************************
     C                   PARM                    QQEOB#            9 0
      *** END ADD *** 09/21/98 ****************************************
     C*
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
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** MOVEAQQFDT     QQF,1
     C******************** MOVEAQQFDT2    QQF,26
      *** END DEL *** 09/21/98 ****************************************
     C*** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'XX00000'     XXMSG#
     C*** END ADD *** 09/21/98 ****************************************
     C*
     C* = Initialization Section ======================================
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDATE            7 0
     C*
     C                   Z-ADD     0             Q                 3 0
     C                   Z-ADD     0             S                 3 0
     C                   Z-ADD     *ZERO         XXAWP             9 5          ADD 11/28/95
     C                   Z-ADD     *ZERO         XXMRA             9 2
     C                   Z-ADD     *ZERO         XXMRAT           12 5          ADD 11/28/95
     C                   Z-ADD     1.09          XXBPRC            3 2
     C                   Z-ADD     1.25          XXGPRC            3 2
     C                   Z-ADD     4.00          XXBDFE            3 2
     C                   Z-ADD     4.00          XXGDFE            3 2
     C*** BEGIN CHG *** 09/25/01 **************************************
     C**********           Z-ADD0         XXOVRP  32
     C                   Z-ADD     *ZEROS        XXOVRP            5 4
     C*** END   CHG *** 09/25/01 **************************************
     C*** BEGIN ADD *** 11/15/95 ***********************************************
     C*******************  Z-ADD*ZEROS    XXUNTP 105      DEL 11/28/95
     C** END CHG *** 11/15/95 **************************************************
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** *LIKE     DEFN CACHK#    SVCHK#           7.0
      *** END DEL *** 09/21/98 ****************************************
     C     *LIKE         DEFINE    CADSEQ        SVDSEQ
     C     *LIKE         DEFINE    CAEOB#        SVEOB#
     C     *LIKE         DEFINE    CAFEIN        SVFEIN
     C     *LIKE         DEFINE    CAMOD         SVMOD
     C     *LIKE         DEFINE    CAPRTY        SVPRTY
      *** BEG CHG *** 09/21/98 ****************************************
     C********** *LIKE     DEFN HCNDC     SVNDC
     C     *LIKE         DEFINE    DPNDC         SVNDC
      *** END CHG *** 09/21/98 ****************************************
     C*
     C     *LIKE         DEFINE    CSKEY         TABKEY
     C*
     C     *LIKE         DEFINE    CADIFF        XPDIFF
     C     *LIKE         DEFINE    CABRTC        XPBRTC
     C     *LIKE         DEFINE    CAEOMB        XPEOMB
     C     *LIKE         DEFINE    CATOTA        XPTOTA
     C     *LIKE         DEFINE    CAMRA         XPTOTP
     C     *LIKE         DEFINE    CAPAID        XPTOTD
      *** BEG CHG *** 09/21/98 ****************************************
     C********** *LIKE     DEFN HCRXMQ    XXRXMQ
     C     *LIKE         DEFINE    DPRXMQ        XXRXMQ
      *** END CHG *** 09/21/98 ****************************************
     C*
     C     *LIKE         DEFINE    CAFDAT        XXDATE
     C     *LIKE         DEFINE    CAFDAT        XXYMD
     C*** BEG ADD *** 09/21/98 ****************************************
     C     *LIKE         DEFINE    MGMSG#        XXMSG#
     C     *LIKE         DEFINE    MGMSEQ        XXMSEQ
     C     *LIKE         DEFINE    MGDSEQ        XXDSEQ
     C     *LIKE         DEFINE    DPDSEQ        KKDSEQ
     C     *LIKE         DEFINE    DEESRC        QQESRC
     C     *LIKE         DEFINE    DEDSEQ        QQDSEQ
     C     *LIKE         DEFINE    DENOT1        XXNOT1
     C     *LIKE         DEFINE    DERCN         XXRCN
     C*** END ADD *** 09/21/98 ****************************************
     C*
     C* ===============================================================
     C*
     C*  Get the user parameter file
     C     UPKY          CHAIN     PUP01                              60
     C*
     C*  If the get was unsuccessful, end the program
     C     *IN60         IFEQ      '1'
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** MOVE '99999'   QQS,1
     C******************** MOVE '95'      QQC,1
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '95'          XXMSG#
     C                   MOVE      '99999'       XXDSEQ
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 95: "Invalid User Parameter, See Programmer"
     C                   GOTO      TERM
     C                   END
     C*
     C*  Get the Claim Master Record
     C     CMKY          CHAIN     PCM01                              60
     C*
     C*  If the get was unsuccessful, end the program
     C     *IN60         IFEQ      '1'
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** MOVE '99998'   QQS,1
     C******************** MOVE '96'      QQC,1
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '96'          XXMSG#
     C                   MOVE      '99998'       XXDSEQ
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 96: Invalid Case Number, See Programmer
     C                   GOTO      TERM
     C                   END
     C*
     C*** Do on each date brought in ********************************
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***Do*adjudication for each possible date entered,if after 08/01/91
     C******************   DO   50        S
     C******************   MOVE QQF,S     XXDATE
     C******************   Z-ADDXXDATE    SRDTI
     C******************   CALL 'SRCVTDAT'DTCYMD
     C******************   MOVELSRDTO     XXYMD
     C******************
     C***********QQF,S**   IFNE *ZERO
     C***********XXYMD**   ANDGE910801
     C***********CAKY1**   CHAINPCA01                54
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C*
     C* Find the recon number.  If this is a recon.
     C     EOKY          CHAIN     PEO01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EORCN         XXRCN
     C*
     C                   ELSE
     C                   Z-ADD     *ZERO         XXRCN
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     CAKY2         SETLL     PCA13
     C     CAKY2         READE     PCA13                                  54
      *
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
      *
     C     CAFDAT        IFGE      1020301
     C     XXRCN         ANDEQ     CARCN
      *
     C                   Z-ADD     CAFDAT        XXYMD
      *** END ADD *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ
     C                   MOVE      CAFEIN        SVFEIN
     C                   MOVE      CAMOD         SVMOD
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAEOB#        SVEOB#
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADDCACHK#    SVCHK#
      *** END DEL *** 09/21/98 ****************************************
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C*+*+*+*+*+*+* Do Loop for Each Claim Detail Record +*+*+*+*+*+*+*+*+*+**
     C********** *IN54     DOWEQ'0'                        DO FOR EACH RCD
     C**********           EXCPTRCA01
      *** END DEL *** 09/21/98 ****************************************
     C     CARCFL        IFNE      'X'
     C                   GOTO      NXTRCD
     C                   END
     C*
     C* Get corresponding Detail Charge Auxiliary record (FDHCP)
     *** BEG DEL *** 09/21/98 *****************************************
     C********** HCKY      CHAINPHC01                55
     C********** *IN55     IFEQ '0'
     C**********           MOVE HCNDC     SVNDC
     C**********           Z-ADDHCRXMQ    XXRXMQ
     C**********           EXCPTRHC01
     C**********           END                              END 0 *IN55
     *** END DEL *** 09/21/98 *****************************************
     *** BEG ADD *** 09/21/98 *****************************************
     C     DPKY1         CHAIN     PDP01                              55
     C     *IN55         IFEQ      '0'
     C                   MOVE      DPNDC         SVNDC
     C                   Z-ADD     DPRXMQ        XXRXMQ
     C                   EXCEPT    RDP01
     C                   END                                                     END 0 *IN55
     *** END ADD *** 09/21/98 *****************************************
     C*
     C* If the Service Date is greater than the system date, give err message
     C     CAFDAT        IFGT      SYDATE
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '79'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '79'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg 79: "Service Date entered is greater than today's date
     C*                (system date)."
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END EQ *ZEROS
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END GT SYDATE
     C*
     C*  Get the claim status record.  See if Override Flag is 'Y'.
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C*
     C     *IN60         IFEQ      '1'
      *** BEG DEL *** 09/21/98 ****************************************
     C**********           Z-ADD1         Q
     C********** '00000'   LOKUPQQS,Q                    60
     C**N60*****           Z-ADD30        Q
     C**********           MOVELCADSEQ    QQS,Q
     C**********           MOVE '97'      QQC,Q            INVALID CLM STS CODE
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '97'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
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
      *** BEG CHG *** 09/21/98 ****************************************
     C********** RXNDC     ORNE HCNDC
     C     RXNDC         ORNE      DPNDC
      *** END CHG *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C**********           Z-ADD1         Q
     C********** '00000'   LOKUPQQS,Q                    60
     C**N60*****           Z-ADD30        Q
     C**********           MOVELCADSEQ    QQS,Q
     C**********           MOVE '94'      QQC,Q            INVALID CLM STS CODE
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '94'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg # 94: Invalid Pharmacy NDC Code
     C                   GOTO      TERM
     C                   END
     C     RXSTAT        CABEQ     'I'           NXTRX
     C*
     C* If NDC code is successfully found, then continue with processing
     C*
     C                   Z-ADD     *ZERO         XXMRA
     C                   Z-ADD     *ZERO         XXMRAT
     C* Brand-name Pharmaceuticals
     C     RXPTYP        IFEQ      '01'
     C*
     C* DETERMINE IF FILL DATE IS LESS THAN AWP EFFECTIVE DATE IF SO
     C* USE PREVIOUS AWP UNIT PRICE ELSE USE CURRENT AWP UNIT PRICE
     C*
     C     CAFDAT        IFLT      RXAWPD
     C*** BEGIN CHG *** 11/15/95 ***********************************************
     C     RX1AWP        DIV       1000          XXAWP                          ADD 11/28/95
     C     XXAWP         MULT      XXBPRC        XXMRAT                         CHG 11/28/95
     C     XXMRAT        MULT(H)   XXRXMQ        XXMRA                          CHG 11/28/95
     C     XXMRA         ADD       XXBDFE        XXMRA                          CHG 11/28/95
     C*********  RX1AWP    DIV  RXPSZE    XXUNTP           DEL 11/28/95
     C*********  XXUNTP    MULT XXBPRC    XXMRA            DEL 11/28/95
     C*********  RX1AWP    MULT XXBPRC    XXMRA
     C*********  XXMRA     MULT XXRXMQ    XXMRA            DEL 11/28/95
     C*********  XXMRA     ADD  XXBDFE    XXMRA
     C*** END CHG *** 11/15/95 *************************************************
     C                   ELSE
     C*** BEGIN CHG *** 11/15/95 ***********************************************
     C     RXAWP         DIV       1000          XXAWP                          ADD 11/28/95
     C     XXAWP         MULT      XXBPRC        XXMRAT                         CHG 11/28/95
     C     XXMRAT        MULT(H)   XXRXMQ        XXMRA                          CHG 11/28/95
     C     XXMRA         ADD       XXBDFE        XXMRA                          CHG 11/28/95
     C********** RXAWP     DIV  RXPSZE    XXUNTP           DEL 11/28/95
     C********** XXUNTP    MULT XXBPRC    XXMRA            DEL 11/28/95
     C********** RXAWP     MULT XXBPRC    XXMRA
     C********** XXMRA     MULT XXRXMQ    XXMRA            DEL 11/28/95
     C********** XXMRA     ADD  XXBDFE    XXMRA            DEL 11/28/95
     C*** END CHG *** 11/15/95 *************************************************
     C                   END
     C*
     C                   ELSE                                                   ELSE EQ '01'
     C*
     C* Generic Pharmaceuticals
     C* DETERMINE IF FILL DATE IS LESS THAN AWP EFFECTIVE DATE IF SO
     C* USE PREVIOUS AWP UNIT PRICE ELSE USE CURRENT AWP UNIT PRICE
     C*
     C     CAFDAT        IFLT      RXAWPD
     C*** BEGIN CHG *** 11/15/95 ***********************************************
     C     RX1AWP        DIV       1000          XXAWP                          ADD 11/28/95
     C     XXAWP         MULT      XXGPRC        XXMRAT                         CHG 11/28/95
     C     XXMRAT        MULT(H)   XXRXMQ        XXMRA                          CHG 11/28/95
     C     XXMRA         ADD       XXGDFE        XXMRA                          CHG 11/28/95
     C********** RX1AWP    DIV  RXPSZE    XXUNTP           DEL 11/28/95
     C********** XXUNTP    MULT XXGPRC    XXMRA            DEL 11/28/95
     C********** RX1AWP    MULT XXGPRC    XXMRA
     C********** XXMRA     MULT XXRXMQ    XXMRA            DEL 11/28/95
     C********** XXMRA     ADD  XXGDFE    XXMRA            DEL 11/28/95
     C*** END CHG *** 11/15/95 *************************************************
     C                   ELSE
     C*** BEGIN CHG *** 11/15/95 ***********************************************
     C     RXAWP         DIV       1000          XXAWP                          ADD 11/28/95
     C     XXAWP         MULT      XXGPRC        XXMRAT                         CHG 11/28/95
     C     XXMRAT        MULT(H)   XXRXMQ        XXMRA                          CHG 11/28/95
     C     XXMRA         ADD       XXGDFE        XXMRA                          CHG 11/28/95
     C********** RXAWP     DIV  RXPSZE    XXUNTP           DEL 11/28/95
     C********** XXUNTP    MULT XXGPRC    XXMRA            DEL 11/28/95
     C********** RXAWP     MULT XXGPRC    XXMRA
     C********** XXMRA     MULT XXRXMQ    XXMRA            DEL 11/28/95
     C********** XXMRA     ADD  XXGDFE    XXMRA            DEL 11/28/95
     C*** END CHG *** 11/15/95 *************************************************
     C                   END
     C*
     C                   END                                                    END EQ '01'
     C*
     C*** Determine lesser of calculations
     C     CACHRG        COMP      XXMRA                              60
     C   60              Z-ADD     XXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPBRTC
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
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CADSEQ    OREQ PXDSEQ
     C********** CAEOB#    ANDNEPXEOB#
     C**********
     C********** HCKY2     CHAINPHC02                61
     C********** *IN61     IFEQ '0'
     C**********           EXCPTRHC02
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    61
     C**N61*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '03'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     PXDSEQ        KKDSEQ
     C*
     C     DPKY2         CHAIN     PDP02                              61
     C*
     C     *IN61         IFEQ      '0'
     C     DPNDC         ANDEQ     D1NDC
     C*
     C                   EXCEPT    RDP02
     C                   MOVE      '03'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
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
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAKY1A    CHAINPCA01                60
     C********** *IN60     IFEQ '0'
      *** END DEL *** 09/21/98 ****************************************
     C*   If override type, we must still output the allowed/disallowed
      *** BEG ADD *** 09/21/98 ****************************************
     C     CAEOB#        IFEQ      QQEOB#
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAEOB#    ANDEQ*ZERO
      *** END DEL *** 09/21/98 ****************************************
     C*** BEGIN ADD *** 09/10/93 **************************************
     C     CAMRA         ANDEQ     0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     CANDIS        ANDEQ     0
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CATOTA    ANDEQ0
      *** END DEL *** 09/21/98 ****************************************
     C     CAPAID        ANDEQ     0
     C*** END ADD *** 09/10/93 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     *ZEROS        XPTOTA
      *** END ADD *** 09/21/98 ****************************************
     C*
     C*   If there is an override percentage, calculate
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CACHRG        MULT      XXOVRP        XPTOTA
     C                   END
     C*
     C     CACHRG        SUB       XPTOTA        CAMRA
     C                   Z-ADD     XPTOTA        CAPAID
     C                   END
     C*
     C                   Z-ADD     *ZEROS        CADIFF
     C                   Z-ADD     *ZEROS        CABRTC
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
      *** BEG CHG *** 09/21/98 ****************************************
     C******************** UPDATPCA01
     C                   UPDATE    PCA13
      *** END CHG *** 09/21/98 ****************************************
     C*
     C                   END                                                    END CAEOB# EQ ZERO
     C                   END                                                    END EQ OVRRD
     C*
     C*** Process next record *****************************************
     C     NXTRCD        TAG
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAKY1     READEPCA01                    54  GET NEXT RCD
     C     CAKY2         READE     PCA13                                  54      GET NEXT RCD
      *** END CHG *** 09/21/98 ****************************************
     C*                                                      TO PROCESS
     C*
     C     *IN54         IFEQ      '0'                                          SAVE FIELDS OF
     C                   Z-ADD     CADSEQ        SVDSEQ                         CURRENT REC.
     C                   MOVE      CAFEIN        SVFEIN
     C                   MOVE      CAMOD         SVMOD
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAEOB#        SVEOB#
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADDCACHK#    SVCHK#
     C**********
     C**Get*corresponding Detail Charge Auxiliary record (FDHCP)
     C********** HCKY      CHAINPHC01                55
     C********** *IN55     IFEQ '0'
     C**********           MOVE HCNDC     SVNDC
     C**********           EXCPTRHC01
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C*
     C* Get corresponding Physician Auxiliary record (FDDPP)
     C     DPKY1         CHAIN     PDP01                              55
     C     *IN55         IFEQ      '0'
     C                   MOVE      DPNDC         SVNDC
     C                   EXCEPT    RDP01
     C                   END                                                    END 0 *IN55
      *** END ADD *** 09/21/98 ****************************************
     C*
     C                   END                                                    END *IN54 EQ 0
     C*
     C                   END                                                    END DOWEQ 0 *IN54
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END QQF,S NE *ZERO
     C******************** END                             END DO 50
      *** END DEL *** 09/21/98 ****************************************
     C*
     C*
     C*** End the program *********************************************
     C     TERM          TAG
     C*
     C*  Release all the formats for a save return.
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** DUMMY     CHAINPCA01                54
     C**N54*****           EXCPTRCA01
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   RETURN
     C                   MOVE      '1'           *INLR
     *** BEG ADD *** 09/21/98 *****************************************
     *********************************************************************
     *** UPDATE FADEP WITH EOMB                                E O M B   *
     *********************************************************************
     C     EOMB          BEGSR
     C*
     C* Find the EOMB description
     C                   MOVEL(P)  XPEOMB        EBKEY
     C*
     C     EBKY          CHAIN     PEB01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     EBDESC        XXNOT1
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       XXNOT1
     C*
     C                   ENDIF                                                  *IN60=ON
     C*
     C                   MOVE      'A'           QQESRC
     C                   MOVE      CADSEQ        QQDSEQ
     C*
     C* Determine if an EOMB already exists for this DSEQ number
     C     DEKY          CHAIN     PDE01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      XPEOMB        DEEOB
     C                   MOVEL(P)  XXNOT1        DENOT1
     C*
     C* Update file maintenance fields
     C                   MOVE      Q1USER        DEUSER
     C                   MOVE      Q1PGM         DEPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*
     C                   MOVEL     SRDTO         DEDT
     C                   Z-ADD     DSTIME        DETIME
     C*
     C                   UPDATE    PDE01
     C*
     C                   ELSE
     C*
     C* No EOMB exists so find the next sequence number and add to file
     C     DEKY1         SETGT     PDE02
     C     DEKY1         READPE    PDE02                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C                   ADD       1             DESEQ#
     C*
     C                   ELSE
     C                   Z-ADD     1             DESEQ#
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   MOVE      CACO#         DECO#
     C                   MOVE      CASUB#        DESUB#
     C                   MOVE      CAFND         DEFND
     C                   MOVE      CAFYR         DEFYR
     C                   MOVE      CACASE        DECASE
     C                   MOVE      CASS#         DESS#
     C                   MOVE      CAACDT        DEACDT
     C                   MOVE      CAEOB#        DEEOB#
     C                   MOVE      CADSEQ        DEDSEQ
     C                   MOVE      XPEOMB        DEEOB
     C                   MOVE      XXNOT1        DENOT1
     C                   MOVE      *BLANKS       DENOT2
     C                   MOVE      'A'           DESTAT
     C                   MOVE      'A'           DEESRC
     C                   Z-ADD     XXRCN         DERCN
     C*
     C* Update file maintenance fields
     C                   MOVE      Q1USER        DEUSER
     C                   MOVE      Q1PGM         DEPGM
     C                   MOVE      Q1USER        DEIUSR
     C                   MOVE      Q1PGM         DEIPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*
     C                   MOVEL     SRDTO         DEDT
     C                   MOVEL     SRDTO         DEIDT
     C                   Z-ADD     DSTIME        DETIME
     C                   Z-ADD     DSTIME        DEITIM
     C*
     C                   WRITE     PDE01
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     EOMBE         ENDSR
     *** END ADD *** 09/21/98 *****************************************
     *** BEG ADD *** 09/21/98 *****************************************
     *********************************************************************
     *** ADJUDICATION MESSAGES SUBROUTINE                    M S G       *
     *********************************************************************
     C     MSG           BEGSR
      *
      * Load the Key fields
      *
     C                   MOVE      CADSEQ        XXDSEQ
      *
      * Is this message already in the file for this line item
      *
     C     MGKY2         CHAIN     PMG02                              60
      *
     C     *IN60         IFEQ      *ON
      *
      * Find the next available message sequence number
      *
     C     MGKY1         SETGT     PMG01
     C     MGKY1         READPE(N) PMG01                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     MGMSEQ        ADD       1             XXMSEQ
      *
     C                   ELSE
      *
     C                   Z-ADD     1             XXMSEQ
      *
     C                   ENDIF                                                  *IN60 = OFF
      *
      * Write the new adjudication message to FDMGP
      *
     C                   Z-ADD     QQCO#         MGCO#
     C                   Z-ADD     QQSUB#        MGSUB#
     C                   MOVE      QQFND         MGFND
     C                   MOVE      QQFYR         MGFYR
     C                   MOVE      QQCASE        MGCASE
     C                   MOVE      *ZEROS        MGDCS#
     C                   Z-ADD     QQEOB#        MGEOB#
     C                   Z-ADD     XXDSEQ        MGDSEQ
     C                   Z-ADD     XXMSEQ        MGMSEQ
     C                   MOVE      'S'           MGSRC
     C                   MOVE      *BLANKS       MGCSI
     C                   MOVE      XXMSG#        MGMSG#
     C                   MOVE      'A'           MGSTAT
     C                   Z-ADD     XXRCN         MGRCN
      *
     C                   MOVE      Q1USER        MGUSER
     C                   MOVE      Q1PGM         MGPGM
     C                   MOVE      Q1PGM         MGIPGM
     C                   MOVE      Q1USER        MGIUSR
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSTIME        MGTIME
     C                   Z-ADD     MGTIME        MGITIM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGDT
     C                   MOVEL     SRDTO         MGIDT
      *
     C                   WRITE     PMG01
      *
     C                   ENDIF                                                  *IN60 = OFF
      *
     C     MSGE          ENDSR
     *** END ADD *** 09/21/98 *****************************************
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
     C     XPEOMB        ANDEQ     *BLANK
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '202'     XPEOMB
     C*** EOMB 202: "Charges exceed Maximum Allowance"
     C*
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C                   END
     C*
     *** BEG ADD *** 09/21/98 *****************************************
     C* Call subroutine to write EOMB to FADEP
     C     XPEOMB        IFNE      *BLANKS
      *
     C                   EXSR      EOMB
      *
     C                   ENDIF                                                  XPEOMB NE BLANK
     *** END ADD *** 09/21/98 *****************************************
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
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CADSEQ    OREQ PXDSEQ
     C********** CAEOB#    ANDNEPXEOB#
     C**********
     C********** HCKY2     CHAINPHC02                61
     C********** *IN61     IFEQ '0'
     C**********           EXCPTRHC02
     C**********
     C********** CAEOB#    IFEQ *ZEROS
     C********** CACHK#    ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    61
     C**N61*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '03'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     PXDSEQ        KKDSEQ
     C*
     C     DPKY2         CHAIN     PDP02                              61
     C*
     C     *IN61         IFEQ      '0'
     C     DPNDC         ANDEQ     D1NDC
     C*
     C                   EXCEPT    RDP02
     C                   MOVE      '03'          XXMSG#
     C                   EXSR      MSG
     C*
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                               END EOB# EQ *ZERO
      *** END DEL *** 09/21/98 ****************************************
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
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAKY1A    CHAINPCA01                60
     C********** CSOVRD    IFNE 'Y'
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     CAEOB#        IFEQ      QQEOB#
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CACHK#    ANDEQ*ZERO
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPTOTD        CAMRA
     C                   Z-ADD     XPTOTP        CAPAID
     C                   Z-ADD     XPDIFF        CADIFF
     C                   Z-ADD     XPBRTC        CABRTC
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
      *** BEG CHG *** 09/21/98 ****************************************
     C**N60*************** UPDATPCA01
     C                   UPDATE    PCA13
      *** END CHG *** 09/21/98 ****************************************
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C* Clear work fields
     C                   MOVE      *BLANKS       XPEOMB
     C                   MOVE      *ZEROS        XPTOTA
     C                   MOVE      *ZEROS        XPTOTP
     C                   MOVE      *ZEROS        XPTOTD
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C     UPDAE         ENDSR
     *********************************************************************
      *** BEG DEL *** 09/21/98 ****************************************
     O*********PCA01   E                RCA01
      *** END DEL *** 09/21/98 ****************************************
     *** BEG CHG *** 09/21/98 *****************************************
     ********* OPHC01   E                RHC01
     ********* OPHC02   E                RHC02
     OPDP01     E            RDP01
     OPDP02     E            RDP02
     *** END CHG *** 09/21/98 *****************************************
