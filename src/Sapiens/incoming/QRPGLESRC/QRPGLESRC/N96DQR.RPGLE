     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N96DQR - TYPE DQ DISCOUNT CODE CALCULATIONS
     H*****************************************************************
     H* THIS PGM IS CALLED BY N9500R TO CALCULATE THE NETWORK DISCOUNT*
     H* AMT BASED ON THE PROVIDERS DISCOUNT CODE. THE DISCOUNT WILL   *
     H* ONLY BE CALCULATED FOR THE SINGLE SEQUENCE NUMBER PASSED TO   *
     H* THIS PROGRAM.                                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    N9500R                                                     *
     H*                                                               *
     H*  PROGRAMMER: C:212              PROJECT: C114MQM              *
     H*  CREATED: 09/02/08                                            *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  07/18/23 C:342 Project: NY060                                *
     H*                 NY Compliance: CARC/RARC Update               *
     H*
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*
     H*   INDICATOR      DESCRIPTION                                  *
     H*       45         GENERAL USAGE(SETLL/READ/CHAIN)              *
     H*       51         GENERAL USAGE(SETLL/READ/CHAIN)              *
     H*       60         GENERAL USAGE(SETLL/READ/CHAIN)              *
     H*       61         GENERAL USAGE(SETLL/READ/CHAIN)              *
     H*       99         GENERAL USAGE(SETLL/READ/CHAIN)              *
     H*       LR         LAST RECORD                                  *
     H*                                                               *
     H*****************************************************************
     FNMBDLD    IF   E           K DISK
     FFDCALN    UF   E           K DISK
     FNDCOL7    IF   E           K DISK
     F                                     RENAME(PCO01:PCO07)
     FNDCOL8    IF   E           K DISK
     F                                     RENAME(PCO01:PCO08)
     FFMS4L11   IF   E           K DISK
     FFTABL     IF   E           K DISK
     FFAEOLG    IF   E           K DISK
     FFDDEL3    O    E           K DISK
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDDUL     IF   E           K DISK
     FFDDHL     IF   E           K DISK
     F*

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D XXNOT1          C                   CONST('NETWORK DISCOUNT AP-
     D                                     PLIED')

     C*
     C*   KEY LIST FOR THE CARVE-OUT TRANSACTION FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    XXSVCD           10
     C                   KFLD                    XXMOD1            2
     C                   KFLD                    XXMOD2            2
     C                   KFLD                    XXFDAT
     C*
     C     COKY1         KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    XXSVCD           10
     C                   KFLD                    XXMOD1            2
     C                   KFLD                    XXMOD2            2
     C*
     C     COKY2         KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    XXPRCD           10
     C                   KFLD                    XXMOD1            2
     C                   KFLD                    XXMOD2            2
     C                   KFLD                    XXFDAT
     C*
     C     COKY3         KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    XXPRCD           10
     C                   KFLD                    XXMOD1            2
     C                   KFLD                    XXMOD2            2
     C*
     C     S4KY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXDS1
     C                   KFLD                    QQSYS
     C                   KFLD                    QQCODE
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
     C*  EOMB HOSPITAL DETAIL FILE
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
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
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
     C                   MOVE      *BLANKS       XXTEMP            2
     C                   MOVEL     QQDS1         XXDS1             5
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
     C*
     C     EOKY          CHAIN     PEO01                              99
     C*
     C                   MOVE      *BLANKS       XXPRCD
     C                   SELECT
     C*
     C     EOBTYP        WHENEQ    'H'
     C  N99DUKY          CHAIN     PDU01                              99
     C                   MOVEL(P)  DUPMOD        XXMOD1
     C                   MOVEL(P)  DUMOD2        XXMOD2
     C                   MOVEL(P)  DURATE        XXPRCD
     C                   MOVEL(P)  DURVCD        XXSVCD
     C*
     C     EOBTYP        WHENEQ    'N'
     C  N99DHKY          CHAIN     PDH01                              99
     C                   MOVEL(P)  DHMOD1        XXMOD1
     C                   MOVEL(P)  DHMOD2        XXMOD2
     C                   MOVEL(P)  DHPRCD        XXPRCD
     C                   MOVEL     *BLANKS       XXSVCD
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
     C                   EXSR      CHKHIT
     C*
     C     HITFLG        IFEQ      'N'
     C*
     C                   MOVE      'N'           FLAG1             1
     C*
     C*** IF S4FEE IS NOT ZERO APPLY THAT AMOUNT  ***
     C     S4KY          SETLL     PS401
     C                   READE     PS401                                  45
     C     *IN45         DOWEQ     '0'
     C*
     C     S4STAT        IFEQ      'A'
     C*
     C     XXFDAT        IFGE      S4BGDT
     C                   MOVE      'Y'           FLAG1
     C                   ENDIF
     C*
     C     S4ENDT        IFNE      0
     C     XXFDAT        ANDGT     S4ENDT
     C                   MOVE      'N'           FLAG1
     C                   ENDIF
     C*
     C     FLAG1         IFEQ      'Y'
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF                                                  S4STAT IFEQ 'A'
     C                   READE     PS401                                  45
     C*
     C                   ENDDO                                                  *IN45 DOWEQ '0'
     C*
     C     FLAG1         IFEQ      'Y'
     C*
     C     S4FEE         IFNE      0
     C                   MOVE      S4FEE         COFEE
     C                   ENDIF
     C*
     C                   ELSE                                                   FLAG1 IFEQ 'Y'
     C*
     C                   Z-ADD     *ZEROS        CANDIS
     C                   GOTO      UPDTCA
     C*
     C                   ENDIF                                                  FLAG1 IFEQ 'Y'
     C*
     C                   ENDIF                                                  HITFLG IFEQ 'N'
     C*
     C* IF COFEE IS ZERO
     C*
     C     COFEE         IFEQ      *ZEROS
     C                   Z-ADD     CAMRA         CANDIS
     C                   Z-ADD     *ZEROS        CAMRA
     C                   GOTO      UPDTCA
     C                   END                                                    END COFEE EQ *ZEROS
     C*
     C* Initially load XXMMRA with Carve Out Amount
     C*
     C                   Z-ADD     COFEE         XXMMRA
     C*
     C                   EXSR      PMNT
     C*
     C* UPDATE THE DETAIL CHARGE RECORD
     C*
     C     UPDTCA        TAG
     C*
     C*** IF CANDIS HAS AN AMOUNT THEN MOVE NEW NETWORK CODE INTO CAEOMB
     C     CANDIS        IFGT      *ZEROS
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
     *** Payment - Compare CO and charged. Write message/EOMB code P M N T
     *********************************************************************
     C     PMNT          BEGSR
     C*
     C     COFEE         IFLT      CAPAID
     C                   Z-ADD     COFEE         CAPAID
     C                   Z-ADD     *ZEROS        CAOTHR
     C                   Z-ADD     *ZEROS        CAOTH2
     C                   MOVEL     *BLANKS       CAOT2C
     C     CACHRG        SUB       CAMRA         CANDIS
     C                   SUB       CAPAID        CANDIS
     C                   ENDIF                                                  COFEE<CAPAID
     C*
     C                   ENDSR
     *********************************************************************
     *** CHKHIT  - check if a record is found in discount code file ***
     *********************************************************************
     C     CHKHIT        BEGSR
     C*
     C                   MOVE      'N'           HITFLG            1
     C*
     C     EOBTYP        IFEQ      'H'
     C*
     C*** check if a record exist for service cod/mod1/mod2 ***
     C     COKY          SETGT     PCO07
     C     COKY1         READPE    PCO07                                  51
     C     *IN51         IFEQ      '0'
     C     COSTAT        IFEQ      'A'
     C                   MOVE      'Y'           HITFLG
     C                   ENDIF
     C                   ELSE
     C*** check if a record exist for service cod/mod2/mod1 ***
     C                   MOVE      XXMOD1        XXTEMP
     C                   MOVE      XXMOD2        XXMOD1
     C                   MOVE      XXTEMP        XXMOD2
     C     COKY          SETGT     PCO07
     C     COKY1         READPE    PCO07                                  51
     C     *IN51         IFEQ      '0'
     C     COSTAT        IFEQ      'A'
     C                   MOVE      'Y'           HITFLG
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  *IN51 IFEQ *OFF
     C                   ENDIF
     C*
     C     HITFLG        IFEQ      'N'
     C*
     C*** check if a record exist for procedure cod/mod1/mod2 ***
     C*
     C     COKY2         SETGT     PCO08
     C     COKY3         READPE    PCO08                                  51
     C     *IN51         IFEQ      '0'
     C     COSTAT        IFEQ      'A'
     C                   MOVE      'Y'           HITFLG
     C                   ENDIF
     C                   ELSE
     C*** check if a record exist for procedure cod/mod2/mod1 ***
     C                   MOVE      XXMOD1        XXTEMP
     C                   MOVE      XXMOD2        XXMOD1
     C                   MOVE      XXTEMP        XXMOD2
     C     COKY2         SETGT     PCO08
     C     COKY3         READPE    PCO08                                  51
     C     *IN51         IFEQ      '0'
     C     COSTAT        IFEQ      'A'
     C                   MOVE      'Y'           HITFLG
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  *IN51 IFEQ *OFF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
