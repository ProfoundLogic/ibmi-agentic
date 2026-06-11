     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE N96DUR - Network Discount Type DU Processing Program
     H*****************************************************************
     H* THIS PGM IS CALLED BY N9600R TO CALCULATE THE NETWORK DISCOUNT*
     H* AMT BASED ON THE PROVIDERS DISCOUNT CODE. THE DISCOUNT WILL   *
     H* ONLY BE CALCULATED FOR THE SINGLE SEQUENCE NUMBER PASSED TO   *
     H* THIS PROGRAM.                                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    N9600R                                                     *
     H*                                                               *
     H*****************************************************************
     H*  Created on 04/30/2013 by C:277 for PROJECT: 147MAS           *
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H*  07/18/23 C:342 Project: NY060                                *
     H*                 NY Compliance: CARC/RARC Update               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*****************************************************************
     FNMBDLD    IF   E           K DISK
     FFDCALN    UF   E           K DISK
     FNDCOL2    IF   E           K DISK
     FNDCOL3    IF   E           K DISK
     F                                     RENAME(PCO01:PCO02)
     FFTABL     IF   E           K DISK
     FFAEOLG    IF   E           K DISK
     FFDDEL3    UF A E           K DISK
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDDHL     IF   E           K DISK
     FFDDUL     IF   E           K DISK
     FFDMGL6    UF A E           K DISK
     FFMPRL     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D XXNOT1          C                   CONST('NETWORK DISCOUNT AP-
     D                                     PLIED')
     D @@DSC1          S             70
     D @@DSC2          S             70
     D @@DSC3          S             70
     D @@DSC4          S             70
     D @@TEXT          s             70
     D @@CO#           S              3  0
     D @@EOB           S              3
     D XXEOB           S              3
     D XXDSEQ          S              5  0

     C*
     C*   KEY LIST FOR THE CARVE-OUT TRANSACTION FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKPMOD
     C                   KFLD                    BDEFDT
     C*
     C     COKY1         KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    BDEFDT
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
     C*
     C     PRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CACPT4
     C                   KFLD                    CAFDAT
     C*
     C     PRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CACPT4
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
     C     *LIKE         DEFINE    COPMOD        KKPMOD
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C     *LIKE         DEFINE    DHPRCD        KKPRCD
     C     *LIKE         DEFINE    DHPRCD        XXPRCD
     C     *LIKE         DEFINE    DHPOS         XXPOS
     C     *LIKE         DEFINE    DHUNTS        XXUNTS
     C     *LIKE         DEFINE    DHMOD1        XXMOD1
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
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*
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
     C                   EVAL      XXDSEQ = %Dec(QQDSEQ:5:0)
     C                   MOVE      QQCEDT        XXEDAT            8 0
     C                   Z-ADD     *ZEROS        COUNT             5 0
     C                   Z-ADD     *ZEROS        XXMMRA            9 2
     C*
     C*  GET THE CASE DETAIL RECORDS
     C*
     C     CAKEY         CHAIN     PCA01                              61
     C     *IN61         IFEQ      '0'
     C*
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   CLEAR                   PR1A2
     C                   ENDIF
     C*
     C     EOKY          CHAIN     PEO01                              99
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
     C*
     C                   SELECT
     C*
     C     EOSYS         WHENEQ    'P'
     C  N99DHKY          CHAIN     PDH01                              99
     C                   MOVEL(P)  DHMOD1        KKPMOD
     C                   MOVEL(P)  DHPRCD        KKPRCD
     C                   MOVEL(P)  DHPRCD        XXPRCD
     C                   MOVEL(P)  DHPOS         XXPOS
     C                   MOVEL(P)  DHMOD1        XXMOD1
     C                   Z-ADD     DHUNTS        XXUNTS
     C*
     C     EOSYS         WHENEQ    'H'
     C  N99DHKY          CHAIN     PDU01                              99
     C                   MOVEL(P)  *BLANKS       KKPMOD
     C                   MOVEL(P)  DURATE        KKPRCD
     C                   MOVEL(P)  DURATE        XXPRCD
     C                   MOVEL(P)  DUPOS         XXPOS
     C                   MOVEL(P)  DUPMOD        XXMOD1
     C                   Z-ADD     DUUNTS        XXUNTS
     C*
     C                   ENDSL                                                  End select
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
     C     XXAM02        IFEQ      *ZEROS
     C                   Z-ADD     15            XXAM02
     C                   ENDIF
     C*
     C* IF WE DO GET A GOOD HIT IN DISCOUNT CODE FILE
     C*
     C     COKY          SETGT     PCO01                              90
     C     NXTCO         TAG
     C     COKY          READPE    PCO01                                  60
     C   60COKY1         SETGT     PCO02
     C     NXTCO1        TAG
     C   60COKY1         READPE    PCO02                                  60
     C     *IN60         IFEQ      '0'
     C*
     C     COSTAT        IFEQ      'I'
     C                   GOTO      NXTCO1
     C                   END
     C                   END
     C     *IN60         IFEQ      '0'
     C*
     C     COSTAT        IFEQ      'I'
     C                   GOTO      NXTCO
     C                   END
     C*
     C* IF COFEE IS ZERO AND COFL03 IS AN 'F' LEAVE AT STATE FEE SCHEDULE
     C*
     C     COFEE         IFEQ      *ZEROS
     C     COFL03        ANDEQ     'F'
     C                   Z-ADD     CAMRA         CANDIS
     C                   Z-ADD     *ZEROS        CAMRA
     C     XXCLT#        IFNE      '114'
     C                   EXSR      REMMSG
     C                   ENDIF                                                  XXCLT#<>114
     C                   GOTO      UPDTCA
     C                   END                                                    END COFEE EQ *ZEROS
     C*
     C* IF COFL03 IS AN 'I' = INDIVIDUAL CONSIDERATION THEN WRITE A    TO CAEOMB
     C* NETWORK MESSAGE TO FDMGP
     C*
     C     COFL03        IFEQ      'I'
     C     XXMOD1        IFNE      '26'
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
     C                   Z-ADD     *ZEROS        CANDIS
     C                   GOTO      UPDTCA
     C                   END                                                    END DHMOD1 NE '26'
     C                   END                                                    END COFL03 EQ 'I'
     C*
     C* IF CODE IS AN 'ANE' CODE 00100-01999, THEN CALCULATE TIME UNITS
     C*
     C     1             SUBST(P)  XXPRCD:5      XXSUFF            1
     C     XXSUFF        IFGE      'A'
     C     XXSUFF        ANDLE     'Z'
     C                   ELSE
     C     XXPRCD        IFGE      '00100'
     C     XXPRCD        ANDLE     '01999'
     C     CAAST         IFEQ      'MA'
     C                   EXSR      MAANE
     C                   ELSE                                                   ELSE CAAST EQ 'MA'
     C                   EXSR      ANE
     C                   END                                                    END CAAST EQ 'MA'
     C                   EXSR      PMNT
     C                   GOTO      UPDTCA
     C                   END                                                    END DHPRCD GE '00100
     C                   ENDIF                                                  XXSUFF <> A-Z
     C*
     C* Initially load XXMMRA with Carve Out Amount
     C*
     C     COFL02        IFEQ      'A'
     C*
     C     XXCLT#        IFEQ      '114'
     C     XXPOS         IFEQ      ' 11'
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
     C*
     C* IF NO SPLIT OUT OF LINES THEN MULTIPLY BY UNITS
     C*
     C     ##SPLT        IFEQ      'N'
     C     PR1A1         ANDEQ     *BLANKS
     C     XXMMRA        MULT      CAUNTS        XXMMRA
     C                   ENDIF
     C*
     C* IF MODIFIER 26 THEN USE THE ADDITIONAL AMOUNT FOR MRA AMOUNT
     C*
     C     XXCLT#        IFNE      '114'
     C     XXPRCD        IFGE      '70000'
     C     XXPRCD        ANDLE     '97799'
     C     XXMOD1        IFEQ      '26'
     C                   Z-ADD     COAAMT        XXMMRA
     C*
     C     ##SPLT        IFEQ      'N'
     C     PR1A1         ANDEQ     *BLANKS
     C     XXMMRA        MULT      XXUNTS        XXMMRA
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C*
     C                   SELECT
     C                   WHEN      XXPRCD = '99245' OR XXPRCD = '99499' OR
     C                             XXPRCD = '99275' OR XXPRCD = '99456'
     C                   Z-ADD     COAAMT        XXMMRA
     C*
     C                   WHEN      (XXPRCD >= '10000' and XXPRCD <= '99607')
     C                             OR (XXPRCD >= 'A0021' and XXPRCD <= 'V5364')
     C                             OR (XXPRCD >= '00100' and XXPRCD <= '01999')
     C                   EVAL      XXMMRA = (BDPC01 * CACHRG)
     C                   ENDSL
     C                   EVAL      CAPAID = XXMMRA
     C*
     C                   ENDIF                                                  XXCLT#<>114
     C*
     C                   EXSR      PMNT
     C*
     C                   ELSE                                                    ELSE COFL02 EQ 'A'
     C*
     C     COFEE         DIV       100           XXPPER            7 4
     C     CACHRG        MULT      XXPPER        CAPAID
     C**IF BDLMRA IS Y THEN DO NOT CLEAR CAMRA
     C                   Z-ADD     *ZEROS        CAMRA
     C     XXCLT#        IFNE      '114'
     C                   EXSR      REMMSG
     C                   ENDIF                                                  XXCLT#<>114
     C     CACHRG        SUB       CAPAID        CANDIS
     C     CANDIS        SUB       CAMRA         CANDIS
     C*
     C*
     C                   END                                                     END COFL02 EQ 'A'
     C*
     C                   ELSE                                                    ELSE *IN60 EQ '0'
     C*
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
     C     BDTYPE        OREQ      'DU'
     C*
     C     XXCLT#        IFEQ      '114'
     C                   EXSR      REMMSG
     C                   ENDIF                                                  XXCLT#=114
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
     C                   MOVEL     ' 93'         XXEOB
     C     CAAST         IFEQ      'FL'
     C*
     C                   MOVE      CACO#         @@CO#
     C                   CALL      'FA783R'
     C                   PARM                    @@CO#
     C                   PARM      XXEOB         @@EOB
     C                   PARM      BDCOM1        @@TEXT
     C                   PARM      *BLANKS       @@DSC1
     C                   PARM      *BLANKS       @@DSC2
     C                   PARM      *BLANKS       @@DSC3
     C                   PARM      *BLANKS       @@DSC4
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  @@DSC1        DENOT1
     C                   MOVEL     @@DSC2        DENOT2
     C                   MOVEL     @@DSC3        DENOT3
     C                   MOVEL     @@DSC4        DENOT4
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
     C****               MOVE      XXEOB         DEEOB
     C                   MOVEL     XXEOB         DEEOBCD
     C**** End Chg *** 07/18/23 **************************************
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
     C                   END                                                     CLM STS CD NE 'C'
     C*
     C                   END                                                     END *IN61
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C     ANE           BEGSR
     C*
     C* THE TIME UNITS FOR ALL PROCEDURES IS COMPUTED BY ALLOWING 1 UNIT
     C* FOR EACH XXAM02 OF ANESTHESIA TIME.  IN ADDITION EACH FRACTION
     C* OF A QUARTER HOUR WILL BE REIMBURSED AT 1 UNIT.
     C*
     C*  1.0 UNIT FOR EACH XXAM02 OR ANY FRACTION THEREOF
     C     XXUNTS        DIV       XXAM02        HLANMN
     C** ROUND UP ALL FRACTIONS OF XXAM02 TO AN ADDITIONAL TIME UNIT
     C                   MVR                     HLADDT
     C                   ADD       HLANMN        HLADDT
     C                   Z-ADD     HLANMN        HLANTV
     C     HLANTV        MULT      XXAM02        XXANTV
     C     XXUNTS        SUB       XXANTV        XXANTV
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
     C*
     C                   ENDSR
     C     MAANE         BEGSR
     C*
     C* THE TIME UNITS FOR ALL PROCEDURES IS COMPUTED BY ALLOWING 1 UNIT
     C* FOR EACH XXAM02 OF ANESTHESIA TIME.  IN ADDITION EACH FRACTION
     C* OF A QUARTER HOUR GREATER THAN 5 MINUTES WILL BE REIMBURSED AT 1 UNIT.
     C*
     C*  1.0 UNIT FOR EACH XXAM02 OR ANY FRACTION THEREOF GREATER THAN 5 MIN
     C     XXUNTS        DIV       XXAM02        HLANMN
     C** ROUND UP ALL FRACTIONS OF XXAM02 TO AN ADDITIONAL TIME UNIT
     C                   MVR                     HLADDT
     C                   ADD       HLANMN        HLADDT
     C                   Z-ADD     HLANMN        HLANTV
     C     HLANTV        MULT      XXAM02        XXANTV
     C     XXUNTS        SUB       XXANTV        XXANTV
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
     C*
     C                   ENDSR
     *********************************************************************
     *** Payment - Compare CO and charged. Write message/EOMB code P M N T
     *********************************************************************
     C     PMNT          BEGSR
     C     XXCLT#        IFNE      '114'
     C*
     C* Carve out < Charged amount
     C*
     C     XXMMRA        IFLT      CACHRG
     C     CACHRG        SUB       XXMMRA        CANDIS
     C* IF BDLMRA IS Y THEN DO NOT CLEAR CAMRA
     C                   Z-ADD     0             CAMRA
     C                   EXSR      REMMSG
     C                   Z-ADD     XXMMRA        CAPAID
     C                   ENDIF                                                  MRA < TOTC
     C*
     C* Carve out >= Charged amount
     C*
     C     XXMMRA        IFGE      CACHRG
     C                   Z-ADD     0             CANDIS
     C                   Z-ADD     0             CAMRA
     C                   EXSR      REMMSG
     C                   Z-ADD     CACHRG        CAPAID
     C                   ENDIF                                                  MRA < TOTC
     C                   ENDIF                                                  XXCLT#<>114
     C*
     C     XXCLT#        IFEQ      '114'
     C     XXMMRA        IFLT      CAPAID
     C                   Z-ADD     XXMMRA        CAPAID
     C                   Z-ADD     *ZEROS        CAOTHR
     C                   Z-ADD     *ZEROS        CAOTH2
     C                   MOVEL     *BLANKS       CAOT2C
     C* IF BDLMRA IS Y THEN DO NOT CLEAR CAMRA
     C     CACHRG        SUB       CAMRA         CANDIS
     C                   SUB       CAPAID        CANDIS
     C                   ENDIF                                                  COFEE<CAPAID
     C                   ENDIF                                                  XXCLT#=114
     C*
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
