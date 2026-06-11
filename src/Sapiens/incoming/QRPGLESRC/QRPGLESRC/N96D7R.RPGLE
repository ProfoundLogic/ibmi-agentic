     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N95D7R - TYPE D7 DISCOUNT CODE CALCULATIONS
     H*****************************************************************
     H* THIS PGM IS CALLED BY N9500R TO CALCULATE THE NETWORK DISCOUNT*
     H* AMT BASED ON THE PROVIDERS DISCOUNT CODE. THE DISCOUNT WILL   *
     H* ONLY BE CALCULATED FOR THE SINGLE SEQUENCE NUMBER PASSED TO   *
     H* THIS PROGRAM.                                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    N9500R                                                     *
     H*                                                               *
     H*  PROGRAMMER: C:20               PROJECT: 4/59MBJ              *
     H*  CREATED: 05/17/94                                            *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  07/18/23 C:342 Project: NY060
     H*                 NY Compliance: CARC/RARC Update
     H*
     H*  02/09/16 C:079 Support: 114/39337
     H*                 For client 114 remove message only if no MRA red
     H*
     H*  02/09/10 C:227 PROJECT: 114MVO
     H*                 UPLOAD/AUTOMATE NJ CONTRACTED ASC RATES
     H*
     H*  01/19/09 C:062 PROJECT: MC288
     H*                 ADD MULTILINE SUPPORT
     H*
     H*  01/13/09 C:079 SUPPORT: 66458
     H*                 REMOVE 114MQE KEEP FS REDUCTION LOGIC
     H*
     H*  10/17/08 C:189 PROJECT: 114MRY
     H*                 NJ NORTH / SOUTH JERSEY UPDATES
     H*
     H*
     H*  07/07/08 C:191 PROJECT: 114MQE
     H*                 MODIFY FOR WORKNET NEW NETWORK ALGORITHM
     H*
     H*  04/19/06 C:163 PROJECT: 114WMF
     H*                 BACK TO BASE - NETWORK DISCOUNT (114MAG 114MEK)
     H*
     H*  11/14/02 C:62  PROJECT:MC114 34884
     H*                 DEFAULT A VALUE FOR XXAM02 TO KEEP FROM
     H*                 GETTING DIVIDE BY ZERO ERROR.
     H*
     H*  09/27/01 C:62  PROJECT:MC246
     H*                 ASSIGN DESEQ#.
     H*
     H*  09/07/01 C:62  Support: 28856 (48MBR)
     H*                 ACCESS NDCOL RECORDS FOR THE SELECTED DISCOUNT
     H*                 CODE.
     H*
     H*  07/27/01 C:62  PROJECT: NW073
     H*                 ONLY DELETE IF CAMRA = 0 BUT NOT MESSAGE
     H*                 XX00003
     H*
     H*  05/18/01 C:79  Support: MC114 27553
     H*                 D7 Discount Alogrithm Issue
     H*                 1. Only delete EOMB code and messages if there
     H*                    is a discount.
     H*
     H*  06/14/00 C:79  Support: 22742 (48MBR)
     H*                 D7 Discount Alogrithm Issue
     H*                 1. Change carve out > logic
     H*                 2. Add FDDHP
     H*                 3. Correct fields names
     H*                 4. Add error if individual consideration
     H*
     H*  03/30/99 C:31  Project: NW011
     H*                 Discount Calculation changes
     H*                 1. Add Provder Bill changes
     H*                 Note: Program created from F95DFR
     H*
     H*  10/30/96    C:45    REQ/PROJ: NW009                          *
     H*                      1. ADD EOMB CODE 398 FOR NETWORK         *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*****************************************************************
     FNMBDLD    IF   E           K DISK
     FFDCALN    UF   E           K DISK
     F*** BEGIN ADD *** 04/19/06 **************************************
     F******* NDCOL1  IF  E           K        DISK
     FNDCOL2    IF   E           K DISK
     F*** END   ADD *** 04/19/06 **************************************
     FFTABL     IF   E           K DISK
     F*** BEGIN ADD *** 06/14/00 **************************************
     FFAEOLG    IF   E           K DISK
     FFDDEL3    UF A E           K DISK
     F*** BEGIN ADD *** 09/27/01 **************************************
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     F*** END   ADD *** 09/27/01 **************************************
     FFDDHL     IF   E           K DISK
     FFDDUL     IF   E           K DISK
     FFDMGL6    UF A E           K DISK
     F*** END ADD *** 06/14/00 ****************************************
     F*** BEGIN ADD *** 01/19/09 **************************************
     FFMPRL     IF   E           K DISK
     F*** END   ADD *** 01/19/09 **************************************
     F*** BEGIN DEL *** 06/14/00 **************************************
     F*** BEGIN ADD *** 03/30/99 **************************************
     F********* FDDEP   O   E                    DISK
     F*** END   DEL *** 06/14/00 **************************************

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D XXNOT1          C                   CONST('NETWORK DISCOUNT AP-
     D                                     PLIED')
     F*** END ADD *** 03/30/99 ****************************************

     C*
     C*   KEY LIST FOR THE CARVE-OUT TRANSACTION FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    QQCODE
     C*** BEG CHG *** 06/14/00 ****************************************
     C******************** KFLD           CACPT4
     C*** BEG CHG *** 02/09/10 ****************************************
     C***********          KFLD           DHPRCD
     C                   KFLD                    KKPRCD
     C*** END CHG *** 02/09/10 ****************************************
     C*** END CHG *** 06/14/00 ****************************************
     C*** BEG ADD *** 04/19/06 ****************************************
     C                   KFLD                    KKPMOD
     C*** END ADD *** 04/19/06 ****************************************
     C*** BEGIN CHG *** 09/07/01 **************************************
     C**********           KFLD           CAFDAT
     C                   KFLD                    BDEFDT
     C*** END   CHG *** 09/07/01 **************************************
     C*
     C*  CASE DETAIL RECORDS
     C*
     C     CAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFUND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXFDAT
     C                   KFLD                    XXDSEQ
     C*** BEGIN ADD *** 01/19/09 **************************************
     C*
     C     PRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CACPT4
     C                   KFLD                    CAFDAT
     C*
     C     PRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CACPT4
     C*** END   ADD *** 01/19/09 **************************************
     C*** BEGIN ADD *** 06/14/00 **************************************
     C*
     C*  EOMB TRANSACTION FILE
     C*
     C     EOKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C*
     C*  EOMB DETAIL EOB/NOTES TRANSACTION FILE
     C*
     C     DEKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    EORCN
     C*
     C*  PROV BILLS SPECIFIC ERRORS
     C*
     C     MGKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    EORCN
     C                   KFLD                    CADSEQ
     C*** BEGIN ADD *** 09/27/01 **************************************
     C*
     C     DEKY1         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C                   KFLD                    KKSEQ#
     C*
     C     DEKY2         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*** END   ADD *** 09/27/01 **************************************
     C*
     C*  EOMB PHYS/HCFA DETAIL FILE
     C*
     C     DHKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*** END ADD *** 06/14/00 ****************************************
     C*
     C*  BILLING CODE TABLE FILE
     C*
     C     BDKEY         KLIST
     C                   KFLD                    QQCODE            3
     C                   KFLD                    CAFDAT
     C*
     C*  CLAIM STATUS CODE TABLE FILE
     C*
     C     TCSKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    TABKEY           10
     C*
     C* INITIALIZE SECTION
     C*
     C     *LIKE         DEFINE    BDAM01        XXAM01
     C     *LIKE         DEFINE    BDAM02        XXAM02
     C     *LIKE         DEFINE    BDDT          XXFDAT
     C*** BEGIN ADD *** 04/19/06 **************************************
     C     *LIKE         DEFINE    COPMOD        KKPMOD
     C*** END   ADD *** 04/19/06 **************************************
     C*** BEGIN ADD *** 09/27/01 **************************************
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C*** END   ADD *** 09/27/01 **************************************
     C*** BEG ADD *** 02/09/10 ****************************************
     C     *LIKE         DEFINE    DHPRCD        KKPRCD
     C     *LIKE         DEFINE    DHPRCD        XXPRCD
     C     *LIKE         DEFINE    DHPOS         XXPOS
     C     *LIKE         DEFINE    DHUNTS        XXUNTS
     C     *LIKE         DEFINE    DHMOD1        XXMOD1
     C*** END ADD *** 02/09/10 ****************************************
     C*
     C                   Z-ADD     0             HLADDT            7 7
     C                   Z-ADD     0             HLANMN           10 7
     C                   Z-ADD     0             HLANTV            3 0
     C                   Z-ADD     0             XXANTV            5 2
     C                   Z-ADD     0             XXWORK           10 7
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFUND            3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQFDAT            8
     C                   PARM                    QQDSEQ            5
     C                   PARM                    QQCODE            3
     C                   PARM                    QQCEDT            8
     C                   PARM                    QQSYS             1
     C                   PARM                    QQFL1             1
     C                   PARM                    QQFL2             1
     C                   PARM                    QQFL3             1
     C                   PARM                    QQFL4             1
     C                   PARM                    QQFL5             1
     C                   PARM                    QQDS1            10
     C                   PARM                    QQDS2            10
     C                   PARM                    QQDS3            10
     C                   PARM                    QQDS4            10
     C                   PARM                    QQDS5            10
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
     C*** BEGIN ADD *** 04/19/06 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*
     C*** END   ADD *** 04/19/06 **************************************
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQFUND        XXFUND            3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   MOVE      QQFEIN        XXFEIN            9 0
     C                   MOVE      QQMOD         XXMOD             3 0
     C                   MOVE      QQFDAT        X1FDAT            8 0
     C*
     C                   Z-ADD     X1FDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C*
     C                   MOVE      QQDSEQ        XXDSEQ            5 0
     C                   MOVE      QQCEDT        XXEDAT            8 0
     C                   Z-ADD     *ZEROS        COUNT             5 0
     C                   Z-ADD     *ZEROS        XXMMRA            9 2
     C*
     C*  GET THE CASE DETAIL RECORDS
     C*
     C     CAKEY         CHAIN     PCA01                              61
     C     *IN61         IFEQ      '0'
     C*
     C*** BEGIN ADD *** 01/19/09 **************************************
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   CLEAR                   PR1A2
     C                   ENDIF
     C*** END   ADD *** 01/19/09 **************************************
     C*** BEG ADD *** 06/14/00 ****************************************
     C*
     C     EOKY          CHAIN     PEO01                              99
     C*** BEGIN ADD *** 01/19/09 **************************************
     C*
     C* FIND OUT IF MULTI-LINE OR NOT
     C*
     C     *IN99         IFEQ      *OFF
     C                   CALL      'SA014R'
1    C                   PARM      CAAST         ##ST              2
2    C                   PARM      EOBTYP        ##BTYP            1
3    C                   PARM      CAFDAT        ##FDAT            7 0
4    C                   PARM      *BLANK        ##SPLT            1
     C                   ENDIF
     C*** END   ADD *** 01/19/09 **************************************
     C*
     C                   SELECT
     C*
     C     EOSYS         WHENEQ    'P'
     C  N99DHKY          CHAIN     PDH01                              99
     C*** BEG ADD *** 04/19/06 ****************************************
     C                   MOVEL(P)  DHMOD1        KKPMOD
     C*** END ADD *** 04/19/06 ****************************************
     C*** BEG ADD *** 02/09/10 ****************************************
     C                   MOVEL(P)  DHPRCD        KKPRCD
     C                   MOVEL(P)  DHPRCD        XXPRCD
     C                   MOVEL(P)  DHPOS         XXPOS
     C                   MOVEL(P)  DHMOD1        XXMOD1
     C                   Z-ADD     DHUNTS        XXUNTS
     C*** END ADD *** 02/09/10 ****************************************
     C*
     C     EOSYS         WHENEQ    'H'
     C  N99DHKY          CHAIN     PDU01                              99
     C*** BEG ADD *** 04/19/06 ****************************************
     C                   MOVEL(P)  *BLANKS       KKPMOD
     C*** END ADD *** 04/19/06 ****************************************
     C*** BEG ADD *** 02/09/10 ****************************************
     C                   MOVEL(P)  DURATE        KKPRCD
     C                   MOVEL(P)  DURATE        XXPRCD
     C                   MOVEL(P)  DUPOS         XXPOS
     C                   MOVEL(P)  DUPMOD        XXMOD1
     C                   Z-ADD     DUUNTS        XXUNTS
     C*** END ADD *** 02/09/10 ****************************************
     C*
     C                   ENDSL                                                  End select
     C*** END ADD *** 06/14/00 ****************************************
     C*
     C* BE SURE CLAIM STATUS CODE IS NOT "C" FOR CAP ON EC/MD
     C*
     C                   MOVE      *BLANKS       TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C     *IN60         IFEQ      '0'
     C     CSDISC        ANDNE     'C'
     C*
     C* FOR EACH DETAIL RECORD READ, FIND THE DISCOUNT MASTER RECORD
     C* THAT APPLYS TO THAT DATE OF SERVICE.
     C*
     C     BDKEY         SETGT     PBD01
     C     NXTBD         TAG
     C                   READP     PBD01                                  60
     C*
     C* VERIFY DISCOUNT CODE AND STATUS
     C*
     C     *IN60         IFEQ      '0'
     C     BDCODE        IFNE      QQCODE
     C     BDSTAT        OREQ      'I'
     C*
     C                   GOTO      NXTBD
     C                   END                                                    BDCODE/BDSTAT
     C                   END                                                    *IN60 OFF
     C*
     C* IF A VALID DISCOUNT RECORD IS NOT FOUND, ZERO THE DISCOUNT
     C*
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZEROS        CANDIS
     C                   GOTO      UPDTCA
     C                   END                                                    *IN60 ON
     C*
     C* IF A VALID DISCOUNT RECORD WAS FOUND GET CONVERSION FACTOR(BDAM01)
     C* AND TIME UNIT FACTOR(BDAM02)
     C*
     C                   Z-ADD     BDAM01        XXAM01
     C                   Z-ADD     BDAM02        XXAM02
     C*** BEGIN ADD *** 11/14/02 **************************************
     C     XXAM02        IFEQ      *ZEROS
     C                   Z-ADD     15            XXAM02
     C                   ENDIF
     C*** END   ADD *** 11/14/02 **************************************
     C*
     C* IF WE DO GET A GOOD HIT IN DISCOUNT CODE FILE
     C*
     C     COKY          SETGT     PCO01
     C     NXTCO         TAG
     C*** BEG CHG ****** 07/07/08 ************************************
     C**********           READPPCO01                    60
     C     COKY          READPE    PCO01                                  60
     C*** END CHG ****** 07/07/08 ************************************
     C     *IN60         IFEQ      '0'
     C*** BEG DEL ****** 07/07/08 ************************************
     C********** QQCODE    ANDEQCOCODE
     C*** BEG CHG *** 06/14/00 ****************************************
     C********** CACPT4    ANDEQCOCPT4
     C********** DHPRCD    ANDEQCOCPT4
     C*** END CHG *** 06/14/00 ****************************************
     C*** BEGIN CHG *** 09/07/01 **************************************
     C********** BDEFDT    ANDEQCOEFDT
     C*** END   CHG *** 09/07/01 **************************************
     C*** END DEL ****** 07/07/08 ************************************
     C*
     C     COSTAT        IFEQ      'I'
     C                   GOTO      NXTCO
     C                   END
     C*
     C* IF COFEE IS ZERO AND COFL03 IS AN 'F' LEAVE AT STATE FEE SCHEDULE
     C*
     C     COFEE         IFEQ      *ZEROS
     C     COFL03        ANDEQ     'F'
     C*** BEGIN CHG *** 03/30/99 **************************************
     C********             Z-ADDCATOTD    CANDIS
     C                   Z-ADD     CAMRA         CANDIS
     C********             Z-ADD*ZEROS    CATOTD
     C                   Z-ADD     *ZEROS        CAMRA
     C*** BEGIN ADD *** 07/27/01 **************************************
     C*** BEGIN CHG *** 04/19/06 **************************************
     C******************** EXSR REMMSG
     C     XXCLT#        IFNE      '114'
     C                   EXSR      REMMSG
     C                   ENDIF                                                  XXCLT#<>114
     C*** END   CHG *** 04/19/06 **************************************
     C*** END   ADD *** 07/27/01 **************************************
     C*** END CHG *** 03/30/99 ****************************************
     C                   GOTO      UPDTCA
     C                   END                                                    END COFEE EQ *ZEROS
     C*
     C* IF COFL03 IS AN 'I' = INDIVIDUAL CONSIDERATION THEN WRITE A    TO CAEOMB
     C* NETWORK MESSAGE TO FDMGP
     C*
     C*** BEGIN DEL *** 06/14/00 **************************************
     C********   COFL03    IFEQ 'I'
     C********   CAPMOD    IFNE '26'
     C********             MOVEL'***'     CAEOMB
     C*** END DEL *** 06/14/00 ****************************************
     C*** BEGIN ADD *** 06/14/00 **************************************
     C     COFL03        IFEQ      'I'
     C*** BEG CHG *** 02/09/10 ****************************************
     C*********  DHMOD1    IFNE '26'
     C     XXMOD1        IFNE      '26'
     C*** END CHG *** 02/09/10 ****************************************
     C                   Z-ADD     CACO#         MGCO#
     C                   Z-ADD     CASUB#        MGSUB#
     C                   Z-ADD     CAFND         MGFND
     C                   Z-ADD     CAFYR         MGFYR
     C                   Z-ADD     CACASE        MGCASE
     C                   Z-ADD     CADCS#        MGDCS#
     C                   Z-ADD     CADSEQ        MGDSEQ
     C                   Z-ADD     1             MGMSEQ
     C                   MOVE      CAEOB#        MGEOB#
     C                   MOVE      'NW00002'     MGMSG#
     C                   MOVE      *BLANKS       MGMSFX
     C                   MOVE      *BLANKS       MGCSI
     C                   MOVE      'N'           MGSRC
     C                   MOVE      'A'           MGSTAT
     C                   MOVE      CAUSER        MGUSER
     C                   MOVE      CAPGM         MGPGM
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        MGTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGDT
     C                   MOVE      MGUSER        MGIUSR
     C                   MOVE      MGPGM         MGIPGM
     C                   Z-ADD     MGTIME        MGITIM
     C                   Z-ADD     MGDT          MGIDT
     C                   MOVE      CARCN         MGRCN
     C                   WRITE     PMG01
     C*** END ADD *** 06/14/00 ****************************************
     C                   Z-ADD     *ZEROS        CANDIS
     C                   GOTO      UPDTCA
     C                   END                                                    END DHMOD1 NE '26'
     C                   END                                                    END COFL03 EQ 'I'
     C*
     C* IF CODE IS AN 'ANE' CODE 00100-01999, THEN CALCULATE TIME UNITS
     C*
     C*** BEG ADD *** 02/09/10 ****************************************
     C     1             SUBST(P)  XXPRCD:5      XXSUFF            1
     C     XXSUFF        IFGE      'A'
     C     XXSUFF        ANDLE     'Z'
     C                   ELSE
     C*** END ADD *** 02/09/10 ****************************************
     C*** BEG DEL *** 06/14/00 ****************************************
     C********** CACPT4    IFGE '00100'
     C********** CACPT4    ANDLE'01999'
     C*** END DEL *** 06/14/00 ****************************************
     C*** BEG ADD *** 06/14/00 ****************************************
     C*** BEG CHG *** 02/09/10 ****************************************
     C*********  DHPRCD    IFGE '00100'
     C*********  DHPRCD    ANDLE'01999'
     C     XXPRCD        IFGE      '00100'
     C     XXPRCD        ANDLE     '01999'
     C*** END CHG *** 02/09/10 ****************************************
     C*** END ADD *** 06/14/00 ****************************************
     C     CAAST         IFEQ      'MA'
     C                   EXSR      MAANE
     C                   ELSE                                                   ELSE CAAST EQ 'MA'
     C                   EXSR      ANE
     C                   END                                                    END CAAST EQ 'MA'
     C*** BEG ADD *** 06/14/00 ****************************************
     C                   EXSR      PMNT
     C*** BEG DEL *** 05/18/01 ****************************************
     C******************** EXSR REMMSG
     C*** END DEL *** 05/18/01 ****************************************
     C*** END ADD *** 06/14/00 ****************************************
     C                   GOTO      UPDTCA
     C                   END                                                    END DHPRCD GE '00100
     C*** BEG ADD *** 02/09/10 ****************************************
     C                   ENDIF                                                  XXSUFF <> A-Z
     C*** END ADD *** 02/09/10 ****************************************
     C*
     C* Initially load XXMMRA with Carve Out Amount
     C*
     C     COFL02        IFEQ      'A'
     C*
     C*** BEG CHG *** 10/17/08 ****************************************
     C******************   Z-ADDCOFEE     XXMMRA
     C     XXCLT#        IFEQ      '114'
     C*** BEG CHG *** 02/09/10 ****************************************
     C*********  DHPOS     IFEQ ' 11'
     C     XXPOS         IFEQ      ' 11'
     C*** END CHG *** 02/09/10 ****************************************
     C                   Z-ADD     COFEE         XXMMRA
     C                   ELSE
     C     COFFEE        IFEQ      0
     C                   Z-ADD     COFEE         XXMMRA
     C                   ELSE
     C                   Z-ADD     COFFEE        XXMMRA
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     COFEE         XXMMRA
     C                   ENDIF
     C*** BEGIN ADD *** 01/19/09 **************************************
     C*
     C* IF NO SPLIT OUT OF LINES THEN MULTIPLY BY UNITS
     C*
     C     ##SPLT        IFEQ      'N'
     C     PR1A1         ANDEQ     *BLANKS
     C     XXMMRA        MULT      CAUNTS        XXMMRA
     C                   ENDIF
     C*** END   ADD *** 01/19/09 **************************************
     C*** END CHG *** 10/17/08 ****************************************
     C*
     C* IF MODIFIER 26 THEN USE THE ADDITIONAL AMOUNT FOR MRA AMOUNT
     C*
     C*** BEG DEL *** 06/14/00 ****************************************
     C********** CACPT4    IFGE '70000'
     C********** CACPT4    ANDLE'97799'
     C********** CAPMOD    IFEQ '26'
     C*** END DEL *** 06/14/00 ****************************************
     C*** BEGIN ADD *** 04/19/06 **************************************
     C     XXCLT#        IFNE      '114'
     C*** END   ADD *** 04/19/06 **************************************
     C*** BEG ADD *** 06/14/00 ****************************************
     C*** BEG CHG *** 02/09/10 ****************************************
     C********   DHPRCD    IFGE '70000'
     C********   DHPRCD    ANDLE'97799'
     C********   DHMOD1    IFEQ '26'
     C     XXPRCD        IFGE      '70000'
     C     XXPRCD        ANDLE     '97799'
     C     XXMOD1        IFEQ      '26'
     C*** END CHG *** 02/09/10 ****************************************
     C*** END ADD *** 06/14/00 ****************************************
     C                   Z-ADD     COAAMT        XXMMRA
     C*** BEGIN ADD *** 01/19/09 **************************************
     C*
     C* IF NO SPLIT OUT OF LINES THEN MULTIPLY BY UNITS
     C*
     C     ##SPLT        IFEQ      'N'
     C     PR1A1         ANDEQ     *BLANKS
     C*** BEG CHG *** 02/09/10 ****************************************
     C********   XXMMRA    MULT DHUNTS    XXMMRA
     C     XXMMRA        MULT      XXUNTS        XXMMRA
     C*** END CHG *** 02/09/10 ****************************************
     C                   ENDIF
     C*** END   ADD *** 01/19/09 **************************************
     C                   END                                                    END DHMOD1 EQ '26'
     C                   END                                                    END DHPRCD GE 70000
     C*** BEGIN ADD *** 04/19/06 **************************************
     C                   ENDIF                                                  XXCLT#<>114
     C*** END   ADD *** 04/19/06 **************************************
     C*
     C*** BEGIN DEL *** 06/14/00 **************************************
     C*** BEGIN CHG *** 03/30/99 **************************************
     C********   XXMMRA    IFGE CATOTC
     C********** XXMMRA    IFGE CACHRG
     C**********           Z-ADDXXMMRA    CATOTA
     C**********           Z-ADDCATOTA    CATOTP
     C**********           Z-ADDXXMMRA    CAPAID
     C**********           Z-ADD*ZEROS    CATOTD
     C**********           Z-ADD*ZEROS    CAMRA
     C********** CATOTC    SUB  XXMMRA    CANDIS
     C********** CACHRG    SUB  XXMMRA    CANDIS
     C****END*CHG *** 03/30/99 ****************************************
     C**********           MOVE *BLANKS   CAEOMB
     C**********
     C**********           ELSE                             ELSE COFEE GE CATOTC
     C**********
     C****BEGIN*CHG *** 03/30/99 **************************************
     C**********           Z-ADDXXMMRA    CATOTA
     C**********           Z-ADDXXMMRA    CATOTP
     C**********           Z-ADDXXMMRA    CAPAID
     C**********           Z-ADD*ZEROS    CATOTD
     C**********           Z-ADD*ZEROS    CAMRA
     C********** CATOTC    SUB  CATOTA    CANDIS
     C********** CACHRG    SUB  CAPAID    CANDIS
     C****END*CHG *** 03/30/99 ****************************************
     C**********
     C**********           END                              END COFEE GE CATOTC
     C*** BEGIN ADD *** 06/14/00 **************************************
     C                   EXSR      PMNT
     C*** BEG DEL *** 05/18/01 ****************************************
     C******************** EXSR REMMSG
     C*** END DEL *** 05/18/01 ****************************************
     C*** END ADD *** 06/14/00 ****************************************
     C*
     C                   ELSE                                                    ELSE COFL02 EQ 'A'
     C*
     C     COFEE         DIV       100           XXPPER            7 4
     C*** BEGIN CHG *** 03/30/99 **************************************
     C********   CATOTC    MULT XXPPER    CATOTA
     C     CACHRG        MULT      XXPPER        CAPAID
     C********             Z-ADDCATOTA    CATOTP
     C********             Z-ADD*ZEROS    CATOTD
     C*** BEG CHG ****** 07/07/08 ************************************
     C*************        Z-ADD*ZEROS    CAMRA
     C*** BEG DEL ****** 01/13/09 ************************************
     C**IF BDLMRA IS Y THEN DO NOT CLEAR CAMRA
     C********   BDLMRA    IFNE 'Y'
     C********             Z-ADD*ZEROS    CAMRA
     C********             ENDIF                           BDLMRA<>'Y'
     C*** END DEL ****** 01/13/09 ************************************
     C*** END CHG ****** 07/07/08 ************************************
     C*** BEG ADD ****** 01/13/09 ************************************
     C                   Z-ADD     *ZEROS        CAMRA
     C*** END ADD ****** 01/13/09 ************************************
     C*** BEGIN ADD *** 07/27/01 **************************************
     C*** BEGIN CHG *** 04/19/06 **************************************
     C******************** EXSR REMMSG
     C     XXCLT#        IFNE      '114'
     C                   EXSR      REMMSG
     C                   ENDIF                                                  XXCLT#<>114
     C*** END   CHG *** 04/19/06 **************************************
     C*** END   ADD *** 07/27/01 **************************************
     C********   CATOTC    SUB  CATOTA    CANDIS
     C     CACHRG        SUB       CAPAID        CANDIS
     C*** BEG CHG ****** 07/07/08 ************************************
     C     CANDIS        SUB       CAMRA         CANDIS
     C*** END CHG ****** 07/07/08 ************************************
     C*** END CHG *** 03/30/99 ****************************************
     C*** BEG ADD *** 08/10/00 ****************************************
     C*** BEG DEL *** 05/18/01 ****************************************
     C******************** EXSR REMMSG
     C*** END DEL *** 05/18/01 ****************************************
     C*** END ADD *** 08/10/00 ****************************************
     C*
     C*
     C                   END                                                     END COFL02 EQ 'A'
     C*
     C                   ELSE                                                    ELSE *IN60 EQ '0'
     C*
     C                   Z-ADD     *ZEROS        CANDIS
     C                   GOTO      UPDTCA
     C*
     C                   END                                                     END *IN60 EQ '0'
     C*
     C* UPDATE THE DETAIL CHARGE RECORD
     C*
     C     UPDTCA        TAG
     C*
     C*** IF CANDIS HAS AN AMOUNT THEN MOVE NEW NETWORK CODE INTO CAEOMB
     C     CANDIS        IFGT      *ZEROS
     C*** BEGIN CHG *** 03/30/99 **************************************
     C********             MOVE '398'     CAEOMB
     C*
     C*** BEGIN DEL *** 07/27/01 **************************************
     C*** BEG ADD *** 05/18/01 ****************************************
     C**********           EXSR REMMSG
     C*** END ADD *** 05/18/01 ****************************************
     C*** END   DEL *** 07/27/01 **************************************
     C*** BEGIN ADD *** 04/19/06 **************************************
     C     XXCLT#        IFEQ      '114'
     C*** Beg Add *** 02/09/16 ****************************************
     C     CAmra         Andeq     0
     C*** End Add *** 02/09/16 ****************************************
     C                   EXSR      REMMSG
     C                   ENDIF                                                  XXCLT#=114
     C*** END   ADD *** 04/19/06 **************************************
     C*** BEGIN ADD *** 09/27/01 **************************************
     C*
     C* GET NEXT DETAIL SEQUENCE NUMBER
     C*
     C                   Z-ADD     *HIVAL        KKSEQ#
     C     DEKY1         SETGT     PDE02
     C     DEKY2         READPE    PDE02                                  99
     C     *IN99         IFEQ      *OFF
     C     DESEQ#        ADD       1             XXSEQ#
     C                   ELSE
     C                   Z-ADD     1             XXSEQ#
     C                   ENDIF
     C*
     C                   CLEAR                   PDE01
     C*** END   ADD *** 09/27/01 **************************************
     C                   MOVE      CACO#         DECO#
     C                   MOVE      CASUB#        DESUB#
     C                   MOVE      CAFND         DEFND
     C                   MOVE      CAFYR         DEFYR
     C                   MOVE      CACASE        DECASE
     C                   MOVE      CASS#         DESS#
     C                   MOVE      CAACDT        DEACDT
     C                   MOVE      CAEOB#        DEEOB#
     C                   MOVE      CADSEQ        DEDSEQ
     C*** BEGIN ADD *** 09/27/01 **************************************
     C                   Z-ADD     XXSEQ#        DESEQ#
     C*** END   ADD *** 09/27/01 **************************************
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      '398'         DEEOB
     C                   MOVEL     '398'         DEEOBCD
     C**** End Chg *** 07/18/23 **************************************
     C                   MOVEL(P)  XXNOT1        DENOT1
     C                   MOVE      *BLANKS       DENOT2
     C                   MOVE      'A'           DESTAT
     C                   MOVE      'N'           DEESRC
     C                   Z-ADD     CARCN         DERCN
     C                   MOVE      Q1USER        DEUSER
     C                   MOVE      Q1PGM         DEPGM
     C                   MOVE      Q1USER        DEIUSR
     C                   MOVE      Q1PGM         DEIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DEDT
     C                   MOVEL     SRDTO         DEIDT
     C                   Z-ADD     DSTIME        DETIME
     C                   Z-ADD     DSTIME        DEITIM
     C                   WRITE     PDE01
     C*** END CHG *** 03/30/99 ****************************************
     C                   ENDIF
     C*
     C                   UPDATE    PCA01
     C*
     C                   END                                                     CLM STS CD NE 'C'
     C*
     C                   END                                                     END *IN61
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     *********************************************************************
     *** Anesthesia                 Codes: 00100 - 01999           A N E *
     *********************************************************************
     C     ANE           BEGSR
     C*
     C* THE TIME UNITS FOR ALL PROCEDURES IS COMPUTED BY ALLOWING 1 UNIT
     C* FOR EACH XXAM02 OF ANESTHESIA TIME.  IN ADDITION EACH FRACTION
     C* OF A QUARTER HOUR WILL BE REIMBURSED AT 1 UNIT.
     C*
     C*  1.0 UNIT FOR EACH XXAM02 OR ANY FRACTION THEREOF
     C*** BEG CHG *** 06/14/00 ****************************************
     C********** CAANMN    DIV  XXAM02    HLANMN
     C*** BEG CHG *** 02/09/10 ****************************************
     C********   DHUNTS    DIV  XXAM02    HLANMN
     C     XXUNTS        DIV       XXAM02        HLANMN
     C*** END CHG *** 02/09/10 ****************************************
     C*** END CHG *** 06/14/00 ****************************************
     C** ROUND UP ALL FRACTIONS OF XXAM02 TO AN ADDITIONAL TIME UNIT
     C                   MVR                     HLADDT
     C                   ADD       HLANMN        HLADDT
     C                   Z-ADD     HLANMN        HLANTV
     C     HLANTV        MULT      XXAM02        XXANTV
     C*** BEG CHG *** 06/14/00 ****************************************
     C********** CAANMN    SUB  XXANTV    XXANTV
     C*** BEG CHG *** 02/09/10 ****************************************
     C********   DHUNTS    SUB  XXANTV    XXANTV
     C     XXUNTS        SUB       XXANTV        XXANTV
     C*** BEG CHG *** 02/09/10 ****************************************
     C*** END CHG *** 06/14/00 ****************************************
     C     XXANTV        IFGT      5
     C                   ADD       1             HLANTV
     C                   END
     C*
     C* BASIC VALUE + TIME UNITS = TOTAL ANESTHESIA VALUE
     C*
     C     COANES        ADD       HLANTV        XXWORK
     C*
     C* TOTAL ANESTHESIA VALUE * CONVERSION FACOTR = FEE
     C*
     C     XXWORK        MULT(H)   XXAM01        XXMMRA
     C*
     C*** BEG DEL *** 06/14/00 ****************************************
     C*** BEGIN CHG *** 03/30/99 **************************************
     C********** XXMMRA    IFGE CATOTC
     C********** XXMMRA    IFGE CACHRG
     C**********           Z-ADDXXMMRA    CATOTA
     C**********           Z-ADDCATOTA    CATOTP
     C**********           Z-ADDXXMMRA    CAPAID
     C**********           Z-ADD*ZEROS    CATOTD
     C**********           Z-ADD*ZEROS    CAMRA
     C********** CATOTC    SUB  XXMMRA    CANDIS
     C********** CACHRG    SUB  XXMMRA    CANDIS
     C****END*CHG *** 03/30/99 ****************************************
     C**********           MOVE *BLANKS   CAEOMB
     C**********
     C**********           ELSE                             ELSE COFEE GE CATOTC
     C**********
     C****BEGIN*CHG *** 03/30/99 **************************************
     C**********           Z-ADDXXMMRA    CATOTA
     C**********           Z-ADDXXMMRA    CATOTP
     C**********           Z-ADDXXMMRA    CAPAID
     C**********           Z-ADD*ZEROS    CATOTD
     C**********           Z-ADD*ZEROS    CAMRA
     C********** CATOTC    SUB  CATOTA    CANDIS
     C********** CACHRG    SUB  CAPAID    CANDIS
     C****END*CHG *** 03/30/99 ****************************************
     C**********
     C**********           END                              END COFEE GE CATOTC
     C*** END DEL *** 06/14/00 ****************************************
     C*
     C                   ENDSR
     *********************************************************************
     *** Anesthesia                 Codes: 00100 - 01999      M  A A N E *
     *********************************************************************
     C     MAANE         BEGSR
     C*
     C* THE TIME UNITS FOR ALL PROCEDURES IS COMPUTED BY ALLOWING 1 UNIT
     C* FOR EACH XXAM02 OF ANESTHESIA TIME.  IN ADDITION EACH FRACTION
     C* OF A QUARTER HOUR GREATER THAN 5 MINUTES WILL BE REIMBURSED AT 1 UNIT.
     C*
     C*  1.0 UNIT FOR EACH XXAM02 OR ANY FRACTION THEREOF GREATER THAN 5 MIN
     C*** BEG CHG *** 06/14/00 ****************************************
     C********** CAANMN    DIV  XXAM02    HLANMN
     C*** BEG CHG *** 02/09/10 ****************************************
     C*******    DHUNTS    DIV  XXAM02    HLANMN
     C     XXUNTS        DIV       XXAM02        HLANMN
     C*** BEG CHG *** 02/09/10 ****************************************
     C*** END CHG *** 06/14/00 ****************************************
     C** ROUND UP ALL FRACTIONS OF XXAM02 TO AN ADDITIONAL TIME UNIT
     C                   MVR                     HLADDT
     C                   ADD       HLANMN        HLADDT
     C                   Z-ADD     HLANMN        HLANTV
     C     HLANTV        MULT      XXAM02        XXANTV
     C*** BEG CHG *** 06/14/00 ****************************************
     C********** CAANMN    SUB  XXANTV    XXANTV
     C*** BEG CHG *** 02/09/10 ****************************************
     C********   DHUNTS    SUB  XXANTV    XXANTV
     C     XXUNTS        SUB       XXANTV        XXANTV
     C*** BEG CHG *** 02/09/10 ****************************************
     C*** END CHG *** 06/14/00 ****************************************
     C     XXANTV        IFGT      5
     C                   ADD       1             HLANTV
     C                   END
     C*
     C* BASIC VALUE + TIME UNITS = TOTAL ANESTHESIA VALUE
     C*
     C     COANES        ADD       HLANTV        XXWORK
     C*
     C* TOTAL ANESTHESIA VALUE * CONVERSION FACOTR = FEE
     C*
     C     XXWORK        MULT(H)   XXAM01        XXMMRA
     C*
     C*** BEG DEL *** 06/14/00 ****************************************
     C*** BEGIN CHG *** 03/30/99 **************************************
     C**********
     C********** XXMMRA    IFGE CATOTC
     C********** XXMMRA    IFGE CACHRG
     C**********           Z-ADDXXMMRA    CATOTA
     C**********           Z-ADDCATOTA    CATOTP
     C**********           Z-ADDXXMMRA    CAPAID
     C**********           Z-ADD*ZEROS    CATOTD
     C**********           Z-ADD*ZEROS    CAMRA
     C********** CATOTC    SUB  XXMMRA    CANDIS
     C********** CACHRG    SUB  XXMMRA    CANDIS
     C****END*CHG *** 03/30/99 ****************************************
     C**********           MOVE *BLANKS   CAEOMB
     C**********
     C**********           ELSE                             ELSE COFEE GE CATOTC
     C**********
     C****BEGIN*CHG *** 03/30/99 **************************************
     C**********           Z-ADDXXMMRA    CATOTA
     C**********           Z-ADDXXMMRA    CATOTP
     C**********           Z-ADDXXMMRA    CAPAID
     C**********           Z-ADD*ZEROS    CATOTD
     C**********           Z-ADD*ZEROS    CAMRA
     C********** CATOTC    SUB  CATOTA    CANDIS
     C********** CACHRG    SUB  CAPAID    CANDIS
     C****END*CHG *** 03/30/99 ****************************************
     C**********
     C**********           END                              END COFEE GE CATOTC
     C*** END DEL *** 06/14/00 ****************************************
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 06/14/00 **************************************
     *********************************************************************
     *** Payment - Compare CO and charged. Write message/EOMB code P M N T
     *********************************************************************
     C     PMNT          BEGSR
     C*** BEGIN ADD *** 04/19/06 **************************************
     C     XXCLT#        IFNE      '114'
     C*** END   ADD *** 04/19/06 **************************************
     C*
     C* Carve out < Charged amount
     C*
     C     XXMMRA        IFLT      CACHRG
     C     CACHRG        SUB       XXMMRA        CANDIS
     C*** BEG CHG ****** 07/07/08 ************************************
     C*************        Z-ADD0         CAMRA
     C*** BEG DEL *** 01/13/09 ***************************************
     C* IF BDLMRA IS Y THEN DO NOT CLEAR CAMRA
     C********   BDLMRA    IFNE 'Y'
     C********             Z-ADD0         CAMRA
     C********             ENDIF                           BDLMRA<>'Y'
     C********   CANDIS    SUB  CAMRA     CANDIS
     C*** END DEL *** 01/13/09 ***************************************
     C*** END CHG ****** 07/07/08 ************************************
     C*** BEG ADD ****** 01/13/09 ************************************
     C                   Z-ADD     0             CAMRA
     C*** END ADD *** 01/13/09 ***************************************
     C*** BEGIN ADD *** 07/27/01 **************************************
     C                   EXSR      REMMSG
     C*** END   ADD *** 07/27/01 **************************************
     C                   Z-ADD     XXMMRA        CAPAID
     C                   ENDIF                                                  MRA < TOTC
     C*
     C* Carve out >= Charged amount
     C*
     C     XXMMRA        IFGE      CACHRG
     C                   Z-ADD     0             CANDIS
     C                   Z-ADD     0             CAMRA
     C*** BEGIN ADD *** 07/27/01 **************************************
     C                   EXSR      REMMSG
     C*** END   ADD *** 07/27/01 **************************************
     C                   Z-ADD     CACHRG        CAPAID
     C                   ENDIF                                                  MRA < TOTC
     C*** BEGIN ADD *** 04/19/06 **************************************
     C                   ENDIF                                                  XXCLT#<>114
     C*** END   ADD *** 04/19/06 **************************************
     C*
     C*** BEGIN ADD *** 04/19/06 **************************************
     C     XXCLT#        IFEQ      '114'
     C*** BEG CHG *** 10/17/08 ****************************************
     C********** COFEE     IFLT CAPAID
     C**********           Z-ADDCOFEE     CAPAID
     C     XXMMRA        IFLT      CAPAID
     C                   Z-ADD     XXMMRA        CAPAID
     C*** END CHG *** 10/17/08 ****************************************
     C                   Z-ADD     *ZEROS        CAOTHR
     C                   Z-ADD     *ZEROS        CAOTH2
     C                   MOVEL     *BLANKS       CAOT2C
     C*** BEG CHG ****** 07/07/08 ************************************
     C*** BEG DEL ****** 01/13/08 ************************************
     C* IF BDLMRA IS Y THEN DO NOT CLEAR CAMRA
     C*******    BDLMRA    IFNE 'Y'
     C*******              Z-ADD0         CAMRA
     C*******              ENDIF                           BDLMRA<>'Y'
     C*** END DEL ****** 01/13/08 ************************************
     C*** END CHG ****** 07/07/08 ************************************
     C     CACHRG        SUB       CAMRA         CANDIS
     C                   SUB       CAPAID        CANDIS
     C                   ENDIF                                                  COFEE<CAPAID
     C                   ENDIF                                                  XXCLT#=114
     C*
     C*** END   ADD *** 04/19/06 **************************************
     C                   ENDSR
     *********************************************************************
     *** Remove  - Remove adjudication messages/EOMB Codes  R E M M S GN T
     *********************************************************************
     C     REMMSG        BEGSR
     C*
     C* Remove adjudication messages from Prov bills errors
     C*
     C     MGKY          CHAIN     PMG01                              99
     C     *IN99         DOWEQ     *OFF
     C     MGSRC         IFEQ      'S'
     C*** BEGIN ADD *** 07/27/01 **************************************
     C                   MOVE      MGMSG#        XXMSG#            5
     C     XXMSG#        IFNE      '00003'
     C*** END   ADD *** 07/27/01 **************************************
     C                   DELETE    PMG01
     C*** BEGIN ADD *** 07/27/01 **************************************
     C                   ENDIF
     C*** END   ADD *** 07/27/01 **************************************
     C                   ENDIF                                                  SRC = 'S'
     C     MGKY          READE     PMG01                                  99
     C                   ENDDO                                                  *IN99 = Off
     C*
     C* Remove adjudication EOMB codes from Detail EOB/NOTES
     C*
     C     DEKY          CHAIN     PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C     DEESRC        IFEQ      'A'
     C     DEDSEQ        ANDEQ     CADSEQ
     C                   DELETE    PDE01
     C                   ENDIF                                                  ESRC = 'A'
     C     DEKY          READE     PDE01                                  99
     C                   ENDDO                                                  *IN99 = Off
     C*
     C                   ENDSR
