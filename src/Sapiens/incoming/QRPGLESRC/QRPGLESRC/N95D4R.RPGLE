     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N95D4R - TYPE D4 DISCOUNT CODE CALCULATIONS
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
     H** REVISIONS:
     H*
     H*  02/02/01 C:62  Support: MC044 25964
     H*                 IF CALCULATED PAID AMOUNT IS GREATER THAN
     H*                 CHARGED, CALCULATE USING CHARGED AMOUNT.
     H*
     H*  01/30/01 C:62  Support: MC095 26091
     H*                 INCLUDE FSMRA PROCESSING.
     H*
     H*  01/25/01 C:79  Support: MC090 26034
     H*                 1. Only calc using CADIFF if this is OK or AL
     H*
     H*  02/02/00 C:31  Support: 21679
     H*                 Network discount issue
     H*                 1. Remove TOTA < CHRG logic
     H*
     H*  07/01/99 C:31  Support: 18000
     H*                 D4 network discount issue
     H*                 1. Add support for FSMRA
     H*
     H*  09/29/98 C:31  Support: 13964
     H*                 ALAMED network issue
     H*                 1. Removed parm from parm list
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
     C*** BEGIN ADD *** 07/01/99 **************************************
     C     *LIKE         DEFINE    BDPC01        XXPC00
     C     *LIKE         DEFINE    CANDIS        XXNDIS
     C     *LIKE         DEFINE    CATOTA        XXTOTA
     C     *LIKE         DEFINE    CADIFF        XXDIFF
     C     *LIKE         DEFINE    CAMRAA        XXALOW
     C*** END ADD *** 07/01/99 ****************************************
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
     C*** BEGIN DEL *** 09/29/98 **************************************
     C********             PARM           QQTADM  8
     C*** END DEL *** 09/29/98 ****************************************
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
     C*
     C*** DETERMINE DISCOUNT TO APPLY -- IF NON-FEE SCHEDULE USE BDPC03
     C*      ... IF FEE SCHEDULE -- HOSPITAL TYPE CHARGES -- USE BDPC01
     C*      ... IF FEE SCHEDULE -- NON/HOSP TYPE CHARGES -- USE BDPC02
     C*
     C*** BEGIN DEL *** 07/01/99 **************************************
     C********             Z-ADD*ZEROS    XXNDIS  92
     C********   CAICD9    IFEQ 'NO-FEE '                   NOT FEE SCH
     C********   CATOTP    MULT BDPC03    XXNDIS    H
     C********             ELSE                             ELSE FEE SC
     C********
     C**IF*INPATIENT IS ENTERED USE BDPC01, USE BDPC02 FOR ALL OTHERS
     C********   CAIPOP    IFEQ '1'                         10/11/89 ADD
     C********   CATOTP    MULT BDPC01    XXNDIS    H       HOSPITAL
     C********             ELSE                             ELSE NON-HOSP
     C********   CATOTP    MULT BDPC02    XXNDIS    H
     C********             END                              END '1' CAIPOP
     C********
     C********             END                              END CAICD9
     C********
     C********             Z-ADDXXNDIS    CANDIS
     C********   CATOTP    SUB  CANDIS    CATOTP
     C********   CATOTA    SUB  CANDIS    CATOTA
     C********
     C****IF*CANDIS HAS AN AMOUNT THEN MOVE NEW NETWORK CODE INTO CAEOMB
     C********
     C********   CANDIS    IFGT *ZEROS
     C********             MOVE '398'     CAEOMB
     C********             ENDIF
     C********
     C********             UPDATPCA01
     C*** END DEL *** 07/01/99 ****************************************
     C*** BEGIN ADD *** 07/01/99 **************************************
     C*
     C*                Determine % off fee schedule
     C*                ------------------------------------------------
     C                   Z-ADD     *ZEROS        XXNDIS
     C*** BEG ADD *** 01/25/01 ****************************************
     C     CAAST         IFEQ      'OK'
     C     CAAST         OREQ      'AL'
     C*** END ADD *** 01/25/01 ****************************************
     C     CATOTP        ADD       CADIFF        XXALOW
     C*
     C*** BEG ADD *** 01/25/01 ****************************************
     C                   ELSE
     C                   Z-ADD     CATOTP        XXALOW
     C                   ENDIF
     C*** END ADD *** 01/25/01 ****************************************
     C*
     C                   Z-ADD     0             XXNDIS
     C     CAICD9        IFEQ      'NO-FEE'
     C                   Z-ADD     BDPC03        XXPC00
     C                   ELSE                                                   ICD9 = 'NO-FEE'
     C     CAIPOP        IFEQ      '1'
     C                   Z-ADD     BDPC01        XXPC00
     C                   ELSE                                                   IPOP = '1'
     C                   Z-ADD     BDPC02        XXPC00
     C                   ENDIF                                                  IPOP = '1'
     C                   ENDIF                                                  ICD9 = 'NO-FEE'
     C     XXALOW        MULT(H)   XXPC00        XXNDIS
     C     XXALOW        SUB       XXNDIS        XXTOTA
     C*
     C*                Update Paid, Allowed, Discount, and EOMB code
     C*                ------------------------------------------------
     C                   SETOFF                                       80
     C*** BEGIN DEL *** 02/02/00 **************************************
     C********   XXTOTA    IFLT CACHRG
     C********   XXTOTA    ANDNE0
     C********   CATOTP    ANDNE0
     C********   CATOTP    SUB  XXTOTA    CANDIS
     C********   CATOTP    SUB  CANDIS    CATOTP
     C********   CATOTA    SUB  CANDIS    CATOTA
     C********             MOVE '398'     CAEOMB
     C********             SETON                     80
     C********             ENDIF                           TOTA < TOTC
     C*** END DEL *** 02/02/00 ****************************************
     C*                If state = 'OK' then
     C*                   CADIFF = MRA$ - (MRA$ * NTWK%) - Charges
     C*                ------------------------------------------------
     C     CAAST         IFEQ      'OK'
     C     CAAST         OREQ      'AL'
     C*** BEGIN ADD *** 02/02/00 **************************************
     C     XXTOTA        IFLT      CATOTC
     C     XXTOTA        ANDNE     0
     C     CATOTP        ANDNE     0
     C     CATOTP        SUB       XXTOTA        CANDIS
     C     CATOTP        SUB       CANDIS        CATOTP
     C     CATOTA        SUB       CANDIS        CATOTA
     C                   MOVE      '398'         CAEOMB
     C                   SETON                                        80
     C                   ENDIF                                                  TOTA < TOTC
     C*** END ADD *** 02/02/00 ****************************************
     C     XXALOW        MULT(H)   XXPC00        XXDIFF
     C     XXALOW        SUB       XXDIFF        XXDIFF
     C     XXDIFF        SUB       CATOTC        CADIFF
     C     XXALOW        SUB       XXNDIS        CAMNAA
     C     CADIFF        IFLE      0
     C                   Z-ADD     0             CADIFF
     C                   ENDIF                                                  DIFF <= 0
     C                   SETON                                        80
     C*** BEGIN ADD *** 02/02/00 **************************************
     C                   ELSE                                                   AST = 'OK'
     C*** BEGIN ADD *** 02/02/01 **************************************
     C     CATOTC        IFGE      XXTOTA
     C*** END   ADD *** 02/02/01 **************************************
     C     XXTOTA        IFNE      0
     C     CATOTP        ANDNE     0
     C     CATOTP        SUB       XXTOTA        CANDIS
     C     CATOTP        SUB       CANDIS        CATOTP
     C     CATOTA        SUB       CANDIS        CATOTA
     C                   MOVE      '398'         CAEOMB
     C                   SETON                                        80
     C                   ENDIF                                                  TOTA <> 0
     C*** BEGIN ADD *** 02/02/01 **************************************
     C                   ELSE
     C     CATOTC        IFNE      0
     C     CATOTP        ANDNE     0
     C     CATOTP        SUB       CATOTC        CANDIS
     C     CATOTP        SUB       CANDIS        CAPAID
     C     CATOTA        SUB       CANDIS        CATOTA
     C                   ENDIF                                                  TOTA <> 0
     C                   ENDIF
     C*** END ADD *** 02/02/00 ****************************************
     C                   ENDIF                                                  AST = 'OK'
     C*
     C*                If discount applies: update Case Detail
     C*                ------------------------------------------------
     C   80              UPDATE    PCA01
     C*** END ADD *** 07/01/99 ****************************************
     C*
     C                   END                                                     CLM STS CD
     C*                                                     NE "C"
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
