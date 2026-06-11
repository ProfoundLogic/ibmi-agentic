     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
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
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  10/30/96    C:45    REQ/PROJ: NW009                          *
     H*                      1. ADD EOMB CODE 398 FOR NETWORK         *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*****************************************************************
     FNMBDLD    IF   E           K DISK
     FFDCALN    UF   E           K DISK
     FNDCOL1    IF   E           K DISK
     FFTABL     IF   E           K DISK

     I*

     C*
     C*   KEY LIST FOR THE CARVE-OUT TRANSACTION FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    CACPT4
     C                   KFLD                    CAFDAT
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
     C*
     C* IF WE DO GET A GOOD HIT IN DISCOUNT CODE FILE
     C*
     C     COKY          SETGT     PCO01
     C     NXTCO         TAG
     C                   READP     PCO01                                  60
     C     *IN60         IFEQ      '0'
     C     QQCODE        ANDEQ     COCODE
     C     CACPT4        ANDEQ     COCPT4
     C*
     C     COSTAT        IFEQ      'I'
     C                   GOTO      NXTCO
     C                   END
     C*
     C* IF COFEE IS ZERO AND COFL03 IS AN 'F' LEAVE AT STATE FEE SCHEDULE
     C*
     C     COFEE         IFEQ      *ZEROS
     C     COFL03        ANDEQ     'F'
     C                   Z-ADD     CATOTD        CANDIS
     C                   Z-ADD     *ZEROS        CATOTD
     C                   GOTO      UPDTCA
     C                   END                                                    END COFEE EQ *ZEROS
     C*
     C* IF COFL03 IS AN 'I' = INDIVIDUAL CONSIDERATION THEN MOVE '***' TO CAEOMB
     C* CODE WILL BE HIGHLIGHTED IN F0070F/R
     C*
     C     COFL03        IFEQ      'I'
     C     CAPMOD        IFNE      '26'
     C                   MOVEL     '***'         CAEOMB
     C                   Z-ADD     *ZEROS        CANDIS
     C                   GOTO      UPDTCA
     C                   END                                                    END CAPMOD NE '26'
     C                   END                                                    END COFL03 EQ 'I'
     C*
     C* IF CODE IS AN 'ANE' CODE 00100-01999, THEN CALCULATE TIME UNITS
     C*
     C     CACPT4        IFGE      '00100'
     C     CACPT4        ANDLE     '01999'
     C     CAAST         IFEQ      'MA'
     C                   EXSR      MAANE
     C                   ELSE                                                   ELSE CAAST EQ 'MA'
     C                   EXSR      ANE
     C                   END                                                    END CAAST EQ 'MA'
     C                   GOTO      UPDTCA
     C                   END                                                    END CACPT4 GE '00100
     C*
     C     COFL02        IFEQ      'A'
     C*
     C                   Z-ADD     COFEE         XXMMRA
     C*
     C* IF MODIFIER 26 THEN USE THE ADDITIONAL AMOUNT FOR MRA AMOUNT
     C*
     C     CACPT4        IFGE      '70000'
     C     CACPT4        ANDLE     '97799'
     C     CAPMOD        IFEQ      '26'
     C                   Z-ADD     COAAMT        XXMMRA
     C                   END                                                    END CAPMOD EQ '26'
     C                   END                                                    END CACPT4 GE 70000
     C*
     C     XXMMRA        IFGE      CATOTC
     C                   Z-ADD     XXMMRA        CATOTA
     C                   Z-ADD     CATOTA        CATOTP
     C                   Z-ADD     *ZEROS        CATOTD
     C     CATOTC        SUB       XXMMRA        CANDIS
     C                   MOVE      *BLANKS       CAEOMB
     C*
     C                   ELSE                                                    ELSE COFEE GE CATOT
     C*
     C                   Z-ADD     XXMMRA        CATOTA
     C                   Z-ADD     XXMMRA        CATOTP
     C                   Z-ADD     *ZEROS        CATOTD
     C     CATOTC        SUB       CATOTA        CANDIS
     C*
     C                   END                                                     END COFEE GE CATOTC
     C*
     C                   ELSE                                                    ELSE COFL02 EQ 'A'
     C*
     C     COFEE         DIV       100           XXPPER            7 4
     C     CATOTC        MULT      XXPPER        CATOTA
     C                   Z-ADD     CATOTA        CATOTP
     C                   Z-ADD     *ZEROS        CATOTD
     C     CATOTC        SUB       CATOTA        CANDIS
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
     C*
     C     CANDIS        IFGT      *ZEROS
     C                   MOVE      '398'         CAEOMB
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
     C     CAANMN        DIV       XXAM02        HLANMN
     C** ROUND UP ALL FRACTIONS OF XXAM02 TO AN ADDITIONAL TIME UNIT
     C                   MVR                     HLADDT
     C                   ADD       HLANMN        HLADDT
     C                   Z-ADD     HLANMN        HLANTV
     C     HLANTV        MULT      XXAM02        XXANTV
     C     CAANMN        SUB       XXANTV        XXANTV
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
     C     XXMMRA        IFGE      CATOTC
     C                   Z-ADD     XXMMRA        CATOTA
     C                   Z-ADD     CATOTA        CATOTP
     C                   Z-ADD     *ZEROS        CATOTD
     C     CATOTC        SUB       XXMMRA        CANDIS
     C                   MOVE      *BLANKS       CAEOMB
     C*
     C                   ELSE                                                    ELSE COFEE GE CATOT
     C*
     C                   Z-ADD     XXMMRA        CATOTA
     C                   Z-ADD     XXMMRA        CATOTP
     C                   Z-ADD     *ZEROS        CATOTD
     C     CATOTC        SUB       CATOTA        CANDIS
     C*
     C                   END                                                     END COFEE GE CATOTC
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
     C     CAANMN        DIV       XXAM02        HLANMN
     C** ROUND UP ALL FRACTIONS OF XXAM02 TO AN ADDITIONAL TIME UNIT
     C                   MVR                     HLADDT
     C                   ADD       HLANMN        HLADDT
     C                   Z-ADD     HLANMN        HLANTV
     C     HLANTV        MULT      XXAM02        XXANTV
     C     CAANMN        SUB       XXANTV        XXANTV
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
     C     XXMMRA        IFGE      CATOTC
     C                   Z-ADD     XXMMRA        CATOTA
     C                   Z-ADD     CATOTA        CATOTP
     C                   Z-ADD     *ZEROS        CATOTD
     C     CATOTC        SUB       XXMMRA        CANDIS
     C                   MOVE      *BLANKS       CAEOMB
     C*
     C                   ELSE                                                    ELSE COFEE GE CATOT
     C*
     C                   Z-ADD     XXMMRA        CATOTA
     C                   Z-ADD     XXMMRA        CATOTP
     C                   Z-ADD     *ZEROS        CATOTD
     C     CATOTC        SUB       CATOTA        CANDIS
     C*
     C                   END                                                     END COFEE GE CATOTC
     C*
     C                   ENDSR
