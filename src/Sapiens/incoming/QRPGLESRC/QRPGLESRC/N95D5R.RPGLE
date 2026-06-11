     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE N95D5R - TYPE D5 DISCOUNT CODE CALCULATIONS
     H*****************************************************************
     H* THIS PGM IS CALLED BY N9500R TO CALCULATE THE NETWORK DISCOUNT*
     H* AMT BASED ON THE PROVIDERS DISCOUNT CODE. THE DISCOUNT WILL   *
     H* ONLY BE CALCULATED FOR THE SINGLE SEQUENCE NUMBER PASSED TO   *
     H* THIS PROGRAM.                                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    N9500R                                                     *
     H*                                                               *
     H*  PROGRAMMER: C:20               PROJECT: 98/NW001             *
     H*  CREATED: 03/21/94                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  02/02/01 C:62  Support: MC044 25964
     H*                 IF CALCULATED PAID AMOUNT IS GREATER THAN
     H*                 CHARGED, CALCULATE USING CHARGED AMOUNT.
     H*
     H*  01/30/01 C:62  Support: MC095 26091
     H*                 INCLUDE FSMRA PROCESSING.
     H*
     H*  03/07/95    C:20    REQ/PROJ: NW007                          *
     H*                      1. PGM HAS BEEN UPDATED TO ACCEPT NEW    *
     H*                         PARMS:                                *
     H*                         SYS  1                                *
     H*                         FL1  1                                *
     H*                         FL2  1                                *
     H*                         FL3  1                                *
     H*                         FL4  1                                *
     H*                         FL5  1                                *
     H*                         DS1 10                                *
     H*                         DS2 10                                *
     H*                         DS3 10                                *
     H*                         DS4 10                                *
     H*                         DS5 10                                *
     H*                                                               *
     H*  10/30/96    C:45    REQ/PROJ: NW009                          *
     H*                      1. ADD EOMB CODE 398 FOR NETWORK         *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*****************************************************************
     FNMBDLD    IF   E           K DISK
     FFDCALN    UF   E           K DISK
     FFTABL     IF   E           K DISK

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
     C     *LIKE         DEFINE    BDDT          XXFDAT
     C*** BEGIN ADD *** 02/02/01 **************************************
     C     *LIKE         DEFINE    CATOTA        XXTOTA
     C*** END   ADD *** 02/02/01 **************************************
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
     C                   MOVE      QQSYS         XXSYS             1
     C*
     C*  GET THE CASE DETAIL RECORDS
     C*
     C     CAKEY         CHAIN     PCA01                              61
     C     *IN61         IFEQ      '0'
     C*** BEGIN ADD *** 01/30/01 **************************************
     C     CACPT4        IFNE      'FSMRA'
     C*** END   ADD *** 01/30/01 **************************************
     C*
     C* BE SURE CLAIM STATUS CODE IS NOT "C" FOR CAP ON EC/MD
     C*
     C                   MOVE      *BLANKS       TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C     *IN60         IFEQ      '0'
     C     CSDISC        ANDNE     'C'
     C*
     C*** FOR EACH DETAIL RECORD READ, FIND THE DISCOUNT MASTER RECORD
     C*** THAT APPLYS TO THAT DATE OF SERVICE.
     C*
     C*
     C*  GET THE DISCOUNT MASTER RECORD
     C*
     C     BDKEY         SETGT     PBD01
     C     NXTBD         TAG
     C                   READP     PBD01                                  60
     C*
     C*** VERIFY DISCOUNT CODE AND STATUS
     C*
     C     *IN60         IFEQ      '0'
     C     BDCODE        IFNE      QQCODE
     C     BDSTAT        OREQ      'I'
     C*
     C                   GOTO      NXTBD
     C                   END                                                    BDCODE/BDSTAT
     C                   END                                                    *IN60 OFF
     C*
     C*** IF A VALID DISCOUNT RECORD IS NOT FOUND, ZERO THE % SO THE
     C*** NETWORK DISCOUNT WILL BE ZERO.
     C*
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZEROS        BDPC01
     C                   Z-ADD     *ZEROS        BDPC02
     C                   Z-ADD     *ZEROS        BDPC03
     C                   END                                                    *IN60 ON
     C                   Z-ADD     *ZEROS        XXNDIS            9 2
     C*
     C*** DETERMINE DISCOUNT TO APPLY -- IF NON-FEE SCHEDULE USE BDPC03
     C*      ... IF FEE SCHEDULE -- HOSPITAL TYPE CHARGES -- USE BDPC01
     C*      ... IF FEE SCHEDULE -- NON/HOSP TYPE CHARGES -- USE BDPC02
     C*      ... IF CPT-4 CODE 70540                      -- USE BDPC04
     C*
     C     CAICD9        IFEQ      'NO-FEE '                                     NOT FEE SCH
     C     CATOTP        MULT(H)   BDPC03        XXNDIS
     C*** BEGIN ADD *** 02/02/01 **************************************
     C     CATOTP        SUB       XXNDIS        XXTOTA
     C*** END   ADD *** 02/02/01 **************************************
     C                   ELSE                                                    ELSE FEE SC
     C*
     C     XXSYS         IFEQ      'H'
     C     CATOTP        MULT(H)   BDPC01        XXNDIS                          HOSPITAL
     C*** BEGIN ADD *** 02/02/01 **************************************
     C     CATOTP        SUB       XXNDIS        XXTOTA
     C*** END   ADD *** 02/02/01 **************************************
     C                   ELSE                                                    ELSE NON-HOSP
     C     CATOTP        MULT(H)   BDPC02        XXNDIS
     C*** BEGIN ADD *** 02/02/01 **************************************
     C     CATOTP        SUB       XXNDIS        XXTOTA
     C*** END   ADD *** 02/02/01 **************************************
     C                   END                                                     END CAPRTY
     C*
     C                   END                                                     END CAICD9
     C*
     C     CACPT4        IFEQ      '70540'
     C     CATOTP        MULT(H)   BDPC04        XXNDIS
     C*** BEGIN ADD *** 02/02/01 **************************************
     C     CATOTP        SUB       XXNDIS        XXTOTA
     C*** END   ADD *** 02/02/01 **************************************
     C                   END                                                     END CACPT4
     C*
     C*** BEGIN ADD *** 02/02/01 **************************************
     C     CATOTC        IFGT      XXTOTA
     C*** END   ADD *** 02/02/01 **************************************
     C                   Z-ADD     XXNDIS        CANDIS
     C     CATOTP        SUB       CANDIS        CATOTP
     C     CATOTA        SUB       CANDIS        CATOTA
     C*** BEGIN ADD *** 02/02/01 **************************************
     C                   ELSE
     C     CATOTP        SUB       CATOTC        CANDIS
     C     CATOTP        SUB       CANDIS        CATOTP
     C     CATOTA        SUB       CANDIS        CATOTA
     C                   ENDIF
     C*** END   ADD *** 02/02/01 **************************************
     C*
     C*** IF CANDIS HAS AN AMOUNT THEN MOVE NEW NETWORK CODE INTO CAEOMB
     C*
     C     CANDIS        IFGT      *ZEROS
     C                   MOVE      '398'         CAEOMB
     C                   ENDIF
     C*
     C                   UPDATE    PCA01
     C*
     C                   END                                                     CLM STS CD
     C*** BEGIN ADD *** 01/30/01 **************************************
     C                   ELSE
     C                   UPDATE    PCA01
     C                   ENDIF
     C*** END   ADD *** 01/30/01 **************************************
     C*
     C                   END                                                     END *IN61
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
