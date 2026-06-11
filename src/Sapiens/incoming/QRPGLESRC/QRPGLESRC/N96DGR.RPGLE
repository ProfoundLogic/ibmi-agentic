     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N96DGR - TYPE DG DISCOUNT CODE CALCULATIONS
     H*****************************************************************
     H* THIS PGM IS CALLED BY N9600R TO CALCULATE THE NETWORK DISCOUNT*
     H* AMT BASED ON THE PROVIDERS DISCOUNT CODE. THE DISCOUNT WILL   *
     H* ONLY BE CALCULATED FOR THE SINGLE SEQUENCE NUMBER PASSED TO   *
     H* THIS PROGRAM.                                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    N9600R                                                     *
     H*                                                               *
     H*  PROGRAMMER: C:62               PROJECT: 114MAP               *
     H*  CREATED: 02/07/01                                            *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  07/18/23 C:342 Project: NY060
     H*                 NY Compliance: CARC/RARC Update
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
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*****************************************************************
     FNMBDLD    IF   E           K DISK
     FFDCALF    UF   E           K DISK
     FNDCOL4    IF   E           K DISK
     FFTABL     IF   E           K DISK
     FFAEOLG    IF   E           K DISK
     F*** BEGIN CHG *** 07/27/01 **************************************
     F**********FDDEL3  O   E           K        DISK
     FFDDEL3    UF A E           K DISK
     F*** END   CHG *** 07/27/01 **************************************
     F*** BEGIN ADD *** 09/27/01 **************************************
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     F*** END   ADD *** 09/27/01 **************************************
     F*** BEGIN ADD *** 07/27/01 **************************************
     FFDMGL6    UF   E           K DISK
     F*** END   ADD *** 07/27/01 **************************************
     FFDDUL     IF   E           K DISK
     FFAEUL     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D XXNOT1          C                   CONST('NETWORK DISCOUNT AP-
     D                                     PLIED')

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
     C* KEY LIST FOR THE CARVE-OUT TRANSACTION FILE
     C*
     C     COKY1         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKFL03
     C                   KFLD                    KK10A1
     C*
     C     COKY2         KLIST
     C                   KFLD                    KKCODE
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
     C*** BEGIN ADD *** 07/27/01 **************************************
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
     C*** END   ADD *** 07/27/01 **************************************
     C*
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    BDCODE        @@CODE
     C     *LIKE         DEFINE    BDCODE        KKCODE
     C     *LIKE         DEFINE    BDEFDT        KKEFDT
     C     *LIKE         DEFINE    CO10A1        KK10A1
     C     *LIKE         DEFINE    COFL03        KKFL03
     C*** BEGIN ADD *** 09/27/01 **************************************
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C*** END   ADD *** 09/27/01 **************************************
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@SUB#        KKSUB#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
     C                   MOVE      @@DSEQ        KKDSEQ
     C                   MOVEL(P)  @@CODE        KKCODE
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
     C     *IN99         IFEQ      '0'
     C*
     C     EOKY          CHAIN     PEO01                              99
     C*
     C                   SELECT
     C     EOSYS         WHENEQ    'H'
     C  N99DUKY          CHAIN     PDU01                              99
     C  N99EUKY          CHAIN     PEU01                              99
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
     C* FOR EACH DETAIL RECORD READ, FIND THE DISCOUNT MASTER RECORD
     C* THAT APPLYS TO THAT DATE OF SERVICE.
     C*
     C                   Z-ADD     CAFDAT        KKEFDT
     C*
     C     BDKY          SETGT     PBD01
     C     NXTBD         TAG
     C                   READP     PBD01                                  99
     C*
     C* VERIFY DISCOUNT CODE AND STATUS
     C*
     C     *IN99         IFEQ      '0'
     C     BDCODE        IFNE      KKCODE
     C     BDSTAT        OREQ      'I'
     C                   GOTO      NXTBD
     C                   ENDIF                                                  BDCODE/BDSTAT
     C                   ENDIF                                                  *IN99 OFF
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C* IF WE DO GET A GOOD HIT IN DISCOUNT CODE FILE
     C*
     C                   MOVEL(P)  EUBLTY        XXBLTY            2
     C                   SETOFF                                       90
     C*
     C     XXBLTY        IFEQ      '13'
     C     XXBLTY        OREQ      '14'
     C     XXBLTY        OREQ      '23'
     C     XXBLTY        OREQ      '33'
     C     XXBLTY        OREQ      '74'
     C     XXBLTY        OREQ      '75'
     C     XXBLTY        OREQ      '84'
     C                   MOVEL(P)  'O'           KKFL03
     C                   MOVEL(P)  DURVCD        KK10A1
     C                   ELSE
     C     XXBLTY        IFEQ      '83'
     C                   SETON                                        90
     C                   ELSE
     C                   MOVEL(P)  'I'           KKFL03
     C                   MOVEL(P)  'INP'         KK10A1
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C*** BEGIN ADD *** 09/07/01 **************************************
     C                   Z-ADD     BDEFDT        KKEFDT
     C*** END   ADD *** 09/07/01 **************************************
     C*
     C     COKY1         SETGT     PCO01
     C     NXTCO         TAG
     C     COKY2         READPE    PCO01                                  99
B1   C     *IN99         IFEQ      '0'
B2   C     COFL03        IFEQ      KKFL03
     C*** BEGIN CHG *** 09/07/01 **************************************
     C     BDEFDT        ANDEQ     COEFDT
     C*** END   CHG *** 09/07/01 **************************************
B3   C     COSTAT        IFEQ      'I'
     C                   GOTO      NXTCO
S3   C                   ELSE
B4   C     KK10A1        IFLT      CO10A1
B4   C     KK10A1        ORGT      CO10A2
     C                   GOTO      NXTCO
E4   C                   ENDIF
E3   C                   ENDIF
S2   C                   ELSE
     C                   GOTO      NXTCO
E2   C                   ENDIF
E1   C                   ENDIF
     C*
     C* IF A VALID DISCOUNT RECORD IS NOT FOUND, ZERO THE DISCOUNT
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C* IF INPATIENT/PER DIEM, THEN PAY PER DIEM FLAT RATE.            LE
     C*
     C     KKFL03        IFEQ      'I'
     C     COFL02        IFEQ      'P'
     C     CACHRG        IFGT      COFEE
     C                   Z-ADD     COFEE         CAPAID
     C     CACHRG        SUB       CAPAID        CANDIS
     C                   ELSE
     C                   Z-ADD     CACHRG        CAPAID
     C                   Z-ADD     *ZEROS        CANDIS
     C                   ENDIF
     C                   ELSE
     C                   ENDIF
     C                   ELSE
     C*
     C* IF OUTPATIENT/% OF CHARGES, THEN PAY PERCENTAGE OF CHARGED.
     C*
     C     COFL02        IFEQ      'C'
     C     COFEE         DIV       100           XXPERC            9 4
     C     CACHRG        MULT      XXPERC        CAPAID
     C     CACHRG        SUB       CAPAID        CANDIS
     C                   ELSE
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        CAMRA
     C*** BEGIN ADD *** 07/27/01 **************************************
     C                   EXSR      REMMSG
     C*** END   ADD *** 07/27/01 **************************************
     C                   Z-ADD     *ZEROS        CAOTHR
     C                   Z-ADD     *ZEROS        CAOTH2
     C*
     C                   ELSE
     C                   Z-ADD     *ZEROS        CANDIS
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     *ZEROS        CANDIS
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     *ZEROS        CANDIS
     C                   ENDIF
     C*
     C*** IF CANDIS HAS AN AMOUNT THEN MOVE NEW NETWORK CODE INTO CAEOMB
     C*
     C     CANDIS        IFGT      *ZEROS
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
     C                   ENDIF
     C*
     C                   UPDATE    PCA01
     C*
     C                   ENDIF                                                   CLM STS CD NE 'C'
     C                   ENDIF                                                   END *IN99
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*** BEGIN ADD *** 07/27/01 **************************************
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
     C*** END   ADD *** 07/27/01 **************************************
