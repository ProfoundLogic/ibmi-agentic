     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N96DHR - TYPE DH DISCOUNT CODE CALCULATIONS
     H*****************************************************************
     H* THIS PGM IS CALLED BY N9600R TO CALCULATE THE NETWORK DISCOUNT*
     H* AMT BASED ON THE PROVIDERS DISCOUNT CODE. THE DISCOUNT WILL   *
     H* ONLY BE CALCULATED FOR THE SINGLE SEQUENCE NUMBER PASSED TO   *
     H* THIS PROGRAM.                                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    N9600R                                                     *
     H*                                                               *
     H*  PROGRAMMER: C:62               PROJECT: 114MDE               *
     H*  CREATED: 03/07/02                                            *
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H*  07/18/23 C:342 Project: NY060                                *
     H*                 NY Compliance: CARC/RARC Update               *
     H*                                                               *
     H*  12/08/14 C:079 Support MC114 87533                           *
     H*                 Use 9.2 fields for percents                   *
     H*                                                               *
     H*  10/20/14 C:079 PROJECT 114MMP Correction                     *
     H*                 Use 9.2 fields for percents                   *
     H*                                                               *
     H*  05/27/14 C:274 PROJECT 114MMP                                *
     H*                 Divide discount code by 10 (move decimal      *
     H*                 position to the left by one position).        *
     H*                                                               *
     H*  05/14/04 C:62 PROJECT 114MIC                                 *
     H*                ADD INPATIENT % PROCESSING                     *
     H*                                                               *
     H*  04/15/03 C:62 PROJECT 114MFG                                 *
     H*                ADD PROCESSING FOR % OFF MRA PROCESSING        *
     H*                                                               *
     H*  03/27/03 C:79 SUPPORT MC114 36638                            *
     H*                1.  PROCESS 83X AND CAH LIKE OUTPATIENT        *
     H*                2.  LOAD KKEFDT CORRECTLY                      *
     H*                                                               *
     H*  10/30/02 C:62 PROJECT 114MEV                                 *
     H*                PROCESS REHAB INPATIENT LIKE OUTPATIENT        *
     H*                                                               *
     H*  06/04/02 C:62 PROJECT:114MDE                                 *
     H*                DON'T EXCLUDE '83X' BILL TYPES FROM NETWORK    *
     H*                PROCESSING.                                    *
     H*                                                               *
     H*  05/02/02 C:62 PROJECT:114MDE                                 *
     H*                ADD ADDITIONAL PROCESSING.                     *
     H*                                                               *
     H*****************************************************************
     FNMBDL1    IF   E           K DISK
     FFDCALF    UF   E           K DISK
     FFTABL     IF   E           K DISK
     FFAEOLG    IF   E           K DISK
     FFDDEL3    UF A E           K DISK
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDMGL6    UF   E           K DISK
     FFDDUL     IF   E           K DISK
     FFAEUL     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D XXNOT1          C                   CONST('NETWORK DISCOUNT AP-
     D                                     PLIED')

     D*** Begin Add *** 05/27/14 **************************************
     D*** Begin Del *** 10/20/14 **************************************
     D*** wkBDPC01        s              7  4
     D*** wkBDPC02        s              7  4
     D*** wkBDPC03        s              7  4
     D*** End   Del *** 10/20/14 **************************************
     D*** End Add   *** 05/27/14 **************************************

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@FEIN            9
     C                   PARM                    @@MOD             3
     C                   PARM                    @@FDAT            8
     C                   PARM                    @@DSEQ            5
     C                   PARM                    @@CODE
     C                   PARM                    @@CEDT            8
     C                   PARM                    @@SYS             1
     C                   PARM                    @@FL1             1
     C                   PARM                    @@FL2             1
     C                   PARM                    @@FL3             1
     C                   PARM                    @@FL4             1
     C                   PARM                    @@FL5             1
     C                   PARM                    @@DS1            10
     C                   PARM                    @@DS2            10
     C                   PARM                    @@DS3            10
     C                   PARM                    @@DS4            10
     C                   PARM                    @@DS5            10
     C                   PARM                    @@RTCD            1
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
     C* BILLING CODE TABLE FILE
     C*
     C     BDKY          KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKEFDT
     C*
     C* CASE DETAIL RECORDS
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKDSEQ
     C*
     C* EOMB PHYS/HCFA DETAIL FILE
     C*
     C     DUKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*
     C* EOMB TRANSACTION FILE
     C*
     C     EOKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C*
     C* EOMB HOSP/UB HEADER FILE
     C*
     C     EUKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C*
     C*  CLAIM STATUS CODE TABLE FILE
     C*
     C     TCSKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    TABKEY           10
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
     C*
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    BDCODE        @@CODE
     C     *LIKE         DEFINE    BDCODE        KKCODE
     C     *LIKE         DEFINE    BDTYPE        KKTYPE
     C     *LIKE         DEFINE    BDEFDT        KKEFDT
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C*** BEGIN ADD *** 04/15/03 **************************************
     C*** Beg Chg *** 12/08/14 **************************************
     C***  *LIKE         DEFINE    BDPC02        XXPC02
     C     *LIKE         DEFINE    BDAm13        XXPC02
     C*** End Chg *** 12/08/14 **************************************
     C*** END   ADD *** 04/15/03 **************************************
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@SUB#        KKSUB#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
     C                   MOVE      @@DSEQ        KKDSEQ
     C                   MOVEL(P)  @@CODE        KKCODE
     C                   MOVEL     'DH'          KKTYPE
     C                   MOVEL     @@CEDT        KKEFDT
     C                   Z-ADD     *ZEROS        COUNT             5 0
     C                   Z-ADD     *ZEROS        XXMMRA            9 2
     C                   Z-ADD     0             HLADDT            7 7
     C                   Z-ADD     0             HLANMN           10 7
     C                   Z-ADD     0             HLANTV            3 0
     C                   Z-ADD     0             XXANTV            5 2
     C                   Z-ADD     0             XXWORK           10 7
     C*
     C*  GET THE CASE DETAIL RECORDS
     C*
     C     CAKY          CHAIN     PCA01                              99
     C  N99EOKY          CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   SELECT
     C     EOSYS         WHENEQ    'H'
     C     DUKY          CHAIN     PDU01                              99
     C     EUKY          CHAIN     PEU01                              99
     C                   ENDSL                                                  End select
     C*
     C* BE SURE CLAIM STATUS CODE IS NOT "C" FOR CAP ON EC/MD
     C*
     C                   MOVE      *BLANKS       TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              99
     C     *IN99         IFEQ      '0'
     C     CSDISC        ANDNE     'C'
     C*
     C* GET THE DISCOUNT RECORD BASED UPON THE OVERALL BILLS FROM DATE
     C*
     C     BDKY          CHAIN     PBD01                              99
     C*
     C* IF WE DO GET A GOOD HIT IN DISCOUNT CODE FILE, LET'S DO
     C* SOME DISCOUNTING.
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C*** Begin ADD *** 05/27/14 **************************************
     C*** Begin Del *** 10/20/14 **************************************
     C************       Eval      wkBDPC01 = BDPC01/10
     C************       Eval      wkBDPC02 = BDPC02/10
     C************       Eval      wkBDPC03 = BDPC03/10
     C*** End   Del *** 10/20/14 **************************************
     C*** End   ADD *** 05/27/14 **************************************
     C                   MOVEL(P)  EUBLTY        XXBLTY            2
     C                   SETOFF                                       90
     C*
     C* OUTPATIENT BILL
     C*
     C     XXBLTY        IFEQ      '13'
     C     XXBLTY        OREQ      '14'
     C     XXBLTY        OREQ      '23'
     C     XXBLTY        OREQ      '33'
     C     XXBLTY        OREQ      '74'
     C     XXBLTY        OREQ      '75'
     C     XXBLTY        OREQ      '84'
     C*** BEGIN ADD *** 10/30/02 **************************************
     C     EOSPCD        OREQ      'REH'
     C*** END   ADD *** 10/30/02 **************************************
     C*** BEGIN ADD *** 03/27/03 **************************************
     C     EOSPCD        OREQ      'CAH'
     C     XXBLTY        OREQ      '83'
     C     EOSPCD        ANDEQ     'HOS'
     C*** END   ADD *** 03/27/03 **************************************
     C*
     C*** BEGIN ADD *** 05/02/02 **************************************
     C     EOFLG3        IFEQ      'P'
     C*** END   ADD *** 05/02/02 **************************************
     C*** Begin Chg *** 05/27/14 **************************************
     C**** CACHRG        MULT      BDPC01        CAPAID
     C*** Begin Chg *** 10/20/14 **************************************
     C**** CACHRG        MULT      wkBDPC01      CAPAID
     C                   Eval      capaid = cachrg * (BDAM12/100)
     C*** End   Chg *** 10/20/14 **************************************
     C*** End   Chg *** 05/27/14 **************************************
     C     CACHRG        SUB       CAPAID        CANDIS
     C                   SUB       CAMRA         CANDIS
     C*** BEGIN ADD *** 05/02/02 **************************************
     C                   ELSE
     C*** BEGIN CHG *** 04/15/03 **************************************
     C********** CAPAID    MULT 0.10      CANDIS
     C**********           SUB  CANDIS    CAPAID
     C*** Beg Chg *** 12/08/14 ***************************************
     C**** 1             SUB       BDPC02        XXPC02
     C                   Eval      xxPc02 = 1 - (BDAm13/100)
     C*** End Chg *** 12/08/14 ***************************************
     C*** BEGIN CHG *** 05/27/14 **************************************
     C**** CAPAID        MULT(H)   BDPC02        CANDIS
     C*** Begin Chg *** 10/20/14 **************************************
     C**** CAPAID        MULT(H)   wkBDPC02      CANDIS
     C                   Eval      candis = capaid * (BDAM13/100)
     C*** End   Chg *** 10/20/14 **************************************
     C*** END   CHG *** 05/27/14 **************************************
     C*                    MULT XXPC02    CAPAID    H
     C                   SUB       CANDIS        CAPAID
     C*** END   CHG *** 04/15/03 **************************************
     C                   ENDIF
     C*** END   ADD *** 05/02/02 **************************************
     C*
     C                   ELSE
     C*
     C* INPATIENT BILL
     C*
     C*** BEGIN DEL *** 06/04/02 **************************************
     C********** XXBLTY    IFEQ '83'
     C**********           SETON                     90
     C**********           ELSE
     C*** END   DEL *** 06/04/02 **************************************
     C*
     C*** BEGIN CHG *** 05/02/02 **************************************
     C********** BDAM01    MULT DUUNTS    XXAMT  112
     C********** CACHRG    IFGT XXAMT
     C********** CAPAID    IFGT XXAMT
     C**********           Z-ADDXXAMT     CAPAID
     C********** CACHRG    SUB  CAPAID    CANDIS
     C**********           SUB  CAMRA     CANDIS
     C**********           ENDIF
     C**********           ENDIF
     C*** BEGIN ADD *** 04/15/03 **************************************
     C*** BEGIN CHG *** 05/27/14 **************************************
     C**** BDPC03        IFNE      *ZEROS
     C*** Begin Chg *** 10/20/14 **************************************
     C**** wkBDPC03      IFNE      *ZEROS
     C     BDAM14        IFNE      *ZEROS
     C*** End   Chg *** 10/20/14 **************************************
     C*** END   CHG *** 05/27/14 **************************************
     C*
     C     EOFLG3        IFEQ      'P'
     C*
     C*** BEGIN CHG *** 05/27/14 **************************************
     C**** CACHRG        MULT      BDPC03        CAPAID
     C*** Begin Chg *** 10/20/14 **************************************
     C*****CACHRG        MULT      wkBDPC03      CAPAID
     C                   Eval      capaid = cachrg * (BDAM14/100)
     C*** End   Chg *** 10/20/14 **************************************
     C*** END   CHG *** 05/27/14 **************************************
     C     CACHRG        SUB       CAPAID        CANDIS
     C                   SUB       CAMRA         CANDIS
     C*
     C                   ELSE
     C*
     C*** Beg Chg *** 12/08/14 ***************************************
     C**** 1             SUB       BDPC02        XXPC02
     C                   Eval      xxPc02 = 1 - (BDAm13/100)
     C*** End Chg *** 12/08/14 ***************************************
     C*** BEGIN CHG *** 05/27/14 **************************************
     C**** CAPAID        MULT(H)   BDPC02        CANDIS
     C*** Begin Chg *** 10/20/14 **************************************
     C**** CAPAID        MULT(H)   wkBDPC02      CANDIS
     C                   Eval      candis = capaid * (BDAM13/100)
     C*** End   Chg *** 10/20/14 **************************************
     C*** END   CHG *** 05/27/14 **************************************
     C*                    MULT XXPC02    CAPAID    H
     C                   SUB       CANDIS        CAPAID
     C                   ENDIF
     C*
     C                   ELSE
     C*** END   ADD *** 04/15/03 **************************************
     C     EOFLG3        IFEQ      'P'
     C     BDAM01        MULT      DUUNTS        XXAMT            11 2
     C                   Z-ADD     XXAMT         CAPAID
     C     CACHRG        SUB       CAPAID        CANDIS
     C                   SUB       CAMRA         CANDIS
     C                   ELSE
     C*** BEGIN CHG *** 04/15/03 **************************************
     C********** CAPAID    MULT 0.10      CANDIS
     C**********           SUB  CANDIS    CAPAID
     C*** Beg Chg *** 12/08/14 ***************************************
     C**** 1             SUB       BDPC02        XXPC02
     C                   Eval      xxPc02 = 1 - (BDAm13/100)
     C*** End Chg *** 12/08/14 ***************************************
     C*** BEGIN CHG *** 05/27/14 **************************************
     C**** CAPAID        MULT(H)   BDPC02        CANDIS
     C*** Begin Chg *** 10/20/14 **************************************
     C**** CAPAID        MULT(H)   wkBDPC02      CANDIS
     C                   Eval      candis = capaid * (BDAM13/100)
     C*** End   Chg *** 10/20/14 **************************************
     C*** END   CHG *** 05/27/14 **************************************
     C*                    MULT XXPC02    CAPAID    H
     C                   SUB       CANDIS        CAPAID
     C*** END   CHG *** 04/15/03 **************************************
     C                   ENDIF
     C*** END   CHG *** 05/02/02 **************************************
     C*** BEGIN ADD *** 04/15/03 **************************************
     C                   ENDIF
     C*** END   ADD *** 04/15/03 **************************************
     C*
     C*** BEGIN DEL *** 06/04/02 **************************************
     C**********           ENDIF
     C*** END   DEL *** 06/04/02 **************************************
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C*** IF CANDIS HAS AN AMOUNT THEN MOVE NEW NETWORK CODE INTO CAEOMB
     C*
     C     CANDIS        IFNE      *ZEROS
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
     C                   MOVE      CACO#         DECO#
     C                   MOVE      CASUB#        DESUB#
     C                   MOVE      CAFND         DEFND
     C                   MOVE      CAFYR         DEFYR
     C                   MOVE      CACASE        DECASE
     C                   MOVE      CASS#         DESS#
     C                   MOVE      CAACDT        DEACDT
     C                   MOVE      CAEOB#        DEEOB#
     C                   MOVE      CADSEQ        DEDSEQ
     C                   Z-ADD     XXSEQ#        DESEQ#
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
     C                   ENDIF
     C                   UPDATE    PCA01
     C                   ENDIF                                                   *IN90 = *OFF
     C                   ENDIF                                                   *IN99 = *OFF
     C                   ENDIF                                                   CSDISC = 'C'
     C                   ENDIF                                                   *IN99 = *OFF
     C*
     C                   SETON                                        LR
     C********************************************************************
     C** Remove  - Remove adjudication messages/EOMB Codes  R E M M S GN T
     C********************************************************************
     C     REMMSG        BEGSR
     C*
     C* Remove adjudication messages from Prov bills errors
     C*
     C     MGKY          CHAIN     PMG01                              99
     C     *IN99         DOWEQ     *OFF
     C     MGSRC         IFEQ      'S'
     C                   MOVE      MGMSG#        XXMSG#            5
     C     XXMSG#        IFNE      '00003'
     C                   DELETE    PMG01
     C                   ENDIF
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
