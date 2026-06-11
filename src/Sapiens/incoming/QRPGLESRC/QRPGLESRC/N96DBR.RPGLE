     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N96DBR - TYPE DB DISCOUNT CODE CALCULATIONS
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program is calculates the network discount amount  **
     H**   based on the providers discount code. The discount will   **
     H**   be calculated for the single sequence number passed to    **
     H**   this program.                                             **
     H**   CALLED FROM:                                              **
     H**     N9600R   -  Process Detail Charges to Correct Discount  **
     H**   CALLS TO:                                                 **
     H**     SRCVTDAT - Convert date from one format to another      **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   11/11/99      PROJECT: 48MEH                   **
     H**              (Adopted from N95DBR)                          **
     H*****************************************************************
     H** REVISIONS:
     H*
     H*   7/18/23 C:342 Project: NY060
     H*                 NY Compliance: CARC/RARC Update
     H*
     H*  05/15/18 C:180 Project: 114MJR
     H*                 Add logic to pay MRA even if > billed amount
     H*
     H*  04/30/13 C:79  PROJECT:147MAS
     H*                 Add logic to use EOB code 93
     H*
     H*  10/29/04 C:62  PROJECT:MC010 44335
     H*                 IF ALGORITHM CAUSES PAID TO BE GREATER THAN
     H*                 CHARGED THEN PAY CHARGED
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
     H*  06/14/00 C:31  Support: 22742 (48MBR)
     H*                 DB Discount Alogrithm Issue
     H*                 1. Change carve out > logic
     H*                 2. Add FDDHP
     H*                 3. Correct fields names
     H*                 4. Add error if individual consideration
     H*
     H*  01/03/00 C:31  Support: 20864 (48MBR)
     H*                 DB Discount Alogrithm Issue
     H*                 1. Change carve out > logic
     H*
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*****************************************************************
     FNMBDLD    IF   E           K DISK
     FFDCALN    UF   E           K DISK
     FNDCOL1    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFTABL     IF   E           K DISK
     F*** BEGIN ADD *** 06/14/00 **************************************
     FFAEOLG    IF   E           K DISK
     F*** BEGIN CHG *** 07/27/01 **************************************
     F**********FDDEL3  UF  E           K        DISK
     FFDDEL3    UF A E           K DISK
     F*** END   CHG *** 07/27/01 **************************************
     F*** BEGIN ADD *** 09/27/01 **************************************
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     F*** END   ADD *** 09/27/01 **************************************
     FFDDHL     IF   E           K DISK
     FFDMGL6    UF A E           K DISK

     D*
     D*** BEGIN ADD *** 07/27/01 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** END   ADD *** 07/27/01 **************************************
     D*----------------------------------------------------------------
     D* System date and time
     D*----------------------------------------------------------------
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*** END ADD *** 06/14/00 ****************************************
     D*** BEGIN ADD *** 07/27/01 **************************************
     D XXNOT1          C                   CONST('NETWORK DISCOUNT AP-
     D                                     PLIED')
      **** Begin Add *** 05/15/18 *************************************
     D CltId           S              3A   INZ
     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d                                3
      **** End   Add *** 05/15/18 *************************************
     I*** END   ADD *** 07/27/01 **************************************

     C*
     C*   KEY LIST FOR THE CARVE-OUT TRANSACTION FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    QQCODE
     C*** BEGIN CHG *** 06/14/00 **************************************
     C********             KFLD           CACPT4
     C                   KFLD                    DHPRCD
     C*** END CHG *** 06/14/00 ****************************************
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
     C     *LIKE         DEFINE    CAFDAT        XXFDAT
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
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFUND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
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
     C* INITIALIZE SECTION
     C*
     C     *LIKE         DEFINE    BDPC01        XXPC01
     C     *LIKE         DEFINE    BDPC02        XXPC02
     C     *LIKE         DEFINE    BDAM01        XXAM01
     C     *LIKE         DEFINE    BDAM02        XXAM02
     C*** BEGIN ADD *** 09/27/01 **************************************
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C*** END   ADD *** 09/27/01 **************************************
     C*** Beg Add *** 04/20/13 *********************************************
     C     *LIKE         DEFINE    DENOT1        @@DSC1
     C     *LIKE         DEFINE    DENOT1        @@DSC2
     C     *LIKE         DEFINE    DENOT1        @@DSC3
     C     *LIKE         DEFINE    DENOT1        @@DSC4
     C**** Beg Chg *** 07/18/23 **************************************
     C**** *LIKE         DEFINE    DEEOB         @@EOB
     C**** *LIKE         DEFINE    DEEOB         xxEOB
     C     *LIKE         DEFINE    DEEOBCD       @@EOB
     C     *LIKE         DEFINE    DEEOBCD       xxEOB
     C**** End Chg *** 07/18/23 **************************************
     C     *LIKE         DEFINE    CACO#         @@CO#
     C     *LIKE         DEFINE    BDCOM1        @@TEXT
     C*** End Add *** 04/20/13 *********************************************
     C                   MOVE      *BLANKS       XXMCA             1
     C                   Z-ADD     *ZEROS        T                 5 0
     C                   Z-ADD     0             HLADDT            7 7
     C                   Z-ADD     0             HLANMN           10 7
     C                   Z-ADD     0             HLANTV            3 0
     C                   Z-ADD     0             XXANTV            5 2
     C                   Z-ADD     0             XXWORK           10 7
      /Free
       //**** Begin Add *** 05/15/18 ***************************************
       GetClientId(CltId);
       //**** End   Add *** 05/15/18 ***************************************
      /End-Free
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
     C                   Z-ADD     X1FDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C                   MOVE      QQDSEQ        XXDSEQ            5 0
     C                   MOVE      QQCEDT        XXEDAT            8 0
     C                   Z-ADD     *ZEROS        COUNT             5 0
     C                   Z-ADD     *ZEROS        XXMMRA            9 2
     C*** BEGIN ADD *** 10/29/04 **************************************
     C                   Z-ADD     *ZEROS        XXPAID            9 2
     C*** END   ADD *** 10/29/04 **************************************
     C*
     C*  GET THE CASE DETAIL RECORDS
     C*
     C     CAKEY         CHAIN     PCA01                              61
     C     *IN61         IFEQ      '0'
     C*** BEGIN ADD *** 07/27/01 **************************************
     C*          Get current recon number
     C*          ------------------------------------------------------
     C     EOKY          CHAIN     PEO01                              99
     C*** END   ADD *** 07/27/01 **************************************
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
     C* GET FLAG INDICATING ALL OR MCA ONLY
     C*
     C                   Z-ADD     51            T
     C     1             SUBST     BDCOM2:T      XXMCA
     C*
     C* IF MCA FLAG IS 'Y' CHECK CLAIM RECORD...ELSE PROCEES DISCOUNTING
     C*
     C     XXMCA         IFEQ      'Y'
     C*
     C* CHAIN TO CLAIM MASTER RECORD TO CHECK MCA FLAG
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C     CMMCA         IFEQ      'N'
     C                   GOTO      UPDTCA
     C                   END                                                    END CMMCA EQ 'N'
     C                   ELSE                                                   ELSE *IN60 EQ '0'
     C                   GOTO      UPDTCA
     C                   END                                                    END *IN60 EQ '0'
     C*
     C                   END                                                    END XXMCA EQ 'Y'
     C*
     C* IF A VALID DISCOUNT RECORD WAS FOUND GET DISCOUNT PERCENTAGES
     C* (BDPC01, BDPC02)
     C*
     C                   Z-ADD     BDPC01        XXPC01
     C                   Z-ADD     BDPC02        XXPC02
     C                   Z-ADD     BDAM01        XXAM01
     C                   Z-ADD     BDAM02        XXAM02
     C*** BEGIN ADD *** 06/14/00 **************************************
     C     DHKY          CHAIN     PDH01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       DHMOD1
     C                   Z-ADD     0             DHUNTS
     C                   MOVE      *BLANKS       DHPRCD
     C                   ENDIF                                                  *IN60 = On
     C*** END ADD *** 06/14/00 ****************************************
     C*
     C* IF WE DO GET A GOOD HIT IN DISCOUNT CODE FILE
     C*
     C     COKY          SETGT     PCO01
     C     NXTCO         TAG
     C                   READP     PCO01                                  60
     C     *IN60         IFEQ      '0'
     C     QQCODE        ANDEQ     COCODE
     C*** BEGIN CHG *** 06/14/00 **************************************
     C********   CACPT4    ANDEQCOCPT4
     C     DHPRCD        ANDEQ     COCPT4
     C*** END CHG *** 06/14/00 ****************************************
     C*** BEGIN CHG *** 09/07/01 **************************************
     C     BDEFDT        ANDEQ     COEFDT
     C*** END   CHG *** 09/07/01 **************************************
     C*
     C     COSTAT        IFEQ      'I'
     C                   GOTO      NXTCO
     C                   END
     C*
     C* IF COFEE IS ZERO AND COFL03 IS AN 'F' LEAVE AT STATE FEE SCHEDULE
     C*
     C     COFEE         IFEQ      *ZEROS
     C     COFL03        ANDEQ     'F'
     C*** BEGIN CHG *** 06/14/00 **************************************
     C********             Z-ADDCATOTD    CANDIS
     C                   Z-ADD     CAMRA         CANDIS
     C********             Z-ADD*ZEROS    CATOTD
     C                   Z-ADD     *ZEROS        CAMRA
     C*** BEGIN ADD *** 07/27/01 **************************************
     C                   EXSR      REMMSG
     C*** END   ADD *** 07/27/01 **************************************
     C*** END CHG *** 06/14/00 ****************************************
     C                   GOTO      UPDTCA
     C                   END                                                    END COFEE EQ *ZEROS
     C*
     C* IF COFL03 IS AN 'I' = INDIVIDUAL CONSIDERATION THEN MOVE '***' TO CAEOMB
     C* CODE WILL BE HIGHLIGHTED IN F0070F/R
     C*
     C*** BEGIN DEL *** 06/14/00 **************************************
     C********   COFL03    IFEQ 'I'
     C********   CAPMOD    IFNE '26'
     C********             MOVEL'***'     CAEOMB
     C*** END DEL *** 06/14/00 ****************************************
     C*** BEGIN ADD *** 06/14/00 **************************************
     C     DHKY          CHAIN     PDH01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       DHMOD1
     C                   Z-ADD     0             DHUNTS
     C                   ENDIF                                                  *IN60 = On
     C     COFL03        IFEQ      'I'
     C     DHMOD1        IFNE      '26'
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
     C                   END                                                    END CAPMOD NE '26'
     C                   END                                                    END COFL03 EQ 'I'
     C*
     C* IF CODE IS AN 'ANE' CODE 00100-01999, THEN CALCULATE TIME UNITS
     C*
     C*** BEGIN CHG *** 06/14/00 **************************************
     C********   CACPT4    IFGE '00100'
     C     DHPRCD        IFGE      '00100'
     C********   CACPT4    ANDLE'01999'
     C     DHPRCD        ANDLE     '01999'
     C*** END CHG *** 06/14/00 ****************************************
     C                   EXSR      ANE
     C                   GOTO      UPDTCA
     C                   END                                                    END DHPRCD GE '00100
     C*
     C     COFL02        IFEQ      'A'
     C*
     C                   Z-ADD     COFEE         XXMMRA
     C*
     C* IF MODIFIER 26 THEN USE THE ADDITIONAL AMOUNT FOR MRA AMOUNT
     C*
     C*** BEGIN CHG *** 06/14/00 **************************************
     C********   CACPT4    IFGE '70000'
     C     DHPRCD        IFGE      '70000'
     C********   CACPT4    ANDLE'97799'
     C     DHPRCD        ANDLE     '97799'
     C*** END CHG *** 06/14/00 ****************************************
     C     CAPMOD        IFEQ      '26'
     C                   Z-ADD     COAAMT        XXMMRA
     C                   END                                                    END CAPMOD EQ '26'
     C                   END                                                    END DHPRCD GE 70000
     C*
     C*** BEGIN DEL *** 01/03/00 **************************************
     C********   CAPAID    SUB  XXMMRA    CANDIS
     C********   CAPAID    SUB  CANDIS    CAPAID
     C********   CATOTA    SUB  CANDIS    CATOTA
     C*** END DEL *** 01/03/00 ****************************************
     C*** BEGIN DEL *** 06/14/00 **************************************
     C****BEGIN ADD *** 01/03/00 **************************************
     C********
     C********   Carve out < Charged amount
     C********   ------------------------------------------------------
     C********   XXMMRA    IFLT CACHRG
     C********   CACHRG    SUB  XXMMRA    CANDIS
     C********             Z-ADD0         CAMRA
     C********             Z-ADDXXMMRA    CAPAID
     C********             ENDIF                           MRA < TOTC
     C****END*ADD *** 01/03/00 ****************************************
     C*** END DEL *** 06/14/00 ****************************************
     C*** BEGIN ADD *** 06/14/00 **************************************
     C                   EXSR      DOCO
     C*** END ADD *** 06/14/00 ****************************************
     C*
     C                   ELSE                                                    ELSE COFL02 EQ 'A'
     C*
     C     COFEE         DIV       100           XXPPER            7 4
     C     CACHRG        MULT      XXPPER        XXMMRA
     C                   Z-ADD     XXMMRA        CANDIS
     C     CAPAID        SUB       CANDIS        CAPAID
     C*** BEGIN DEL *** 06/14/00 **************************************
     C********   CATOTA    SUB  CANDIS    CATOTA
     C*** END DEL *** 06/14/00 ****************************************
     C*
     C                   END                                                     END COFL02 EQ 'A'
     C*
     C                   ELSE                                                    ELSE *IN60 EQ '0'
     C*
     C     CARCFL        IFEQ      'Y'
     C     CAPAID        MULT(H)   BDPC02        XXMMRA
     C                   ELSE
     C     CAPAID        MULT(H)   BDPC01        XXMMRA
     C                   END
     C*** BEGIN ADD *** 10/29/04 **************************************
     C     CAPAID        SUB       XXMMRA        XXPAID
     C*** END   ADD *** 10/29/04 **************************************
     C*
     C*** BEGIN ADD *** 10/29/04 **************************************
      *** Begin Add *** 05/15/18 **************************************
      /Free
       Select;
       When CLTID = '114' and EOast = 'FL';
          If xxMMRA <= CApaid;
             CAndis = xxMMRA;
             CApaid = CApaid - CAndis;
          Else;
             CAndis = *Zeros;
          Endif;
       Other;
      /End-Free
      *** End Add *** 05/15/18 **************************************
     C     XXPAID        IFLE      CACHRG
     C*** END   ADD *** 10/29/04 **************************************
     C     XXMMRA        IFLE      CAPAID
     C                   Z-ADD     XXMMRA        CANDIS
     C     CAPAID        SUB       CANDIS        CAPAID
     C*** BEGIN DEL *** 06/14/00 **************************************
     C********   CATOTA    SUB  CANDIS    CATOTA
     C*** END DEL *** 06/14/00 ****************************************
     C                   ELSE                                                    ELSE XXMMRA LT CATO
     C                   Z-ADD     *ZEROS        CANDIS
     C                   END                                                     END XXMMRA LT CACHR
     C*** BEGIN ADD *** 10/29/04 **************************************
     C                   ELSE
     C                   Z-ADD     CACHRG        CAPAID
     C                   Z-ADD     *ZEROS        CANDIS
     C                   SUB       CAMRA         CANDIS
     C                   SUB       CAOTHR        CANDIS
     C                   SUB       CAOTH2        CANDIS
     C                   ENDIF
     C*** END   ADD *** 10/29/04 **************************************
     C                   GOTO      UPDTCA
     C*
      *** Begin Add *** 05/15/18 ************************************
      /Free
       Endsl;
      /End-Free
      *** End Add *** 05/15/18 **************************************
     C                   END                                                     END *IN60 EQ '0'
     C*
     C* UPDATE THE DETAIL CHARGE RECORD
     C*
     C     UPDTCA        TAG
      /Free
       //*** Begin Add *** 05/15/18 ***********************************
       Select;
       When CLTID = '114' and EOast = 'FL';
          *IN75 = *On;
       When CanDis > *Zeros;
          *in75 = *On;
       Other;
          *in75 = *Off;
       Endsl;
       //*** End   Add *** 05/15/18 ***********************************
      /End-Free
     C*** BEGIN ADD *** 07/27/01 **************************************
      *** BEGIN CHG *** 05/15/18 **************************************
     C***  CANDIS        IFGT      *ZEROS
     C     *IN75         IFEQ      *ON
      *** END   CHG *** 05/15/18 **************************************
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
     C*** END   ADD *** 09/27/01 **************************************
     C*
     C                   CLEAR                   PDE01
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
     C*** BEGIN ADD *** 09/27/01 **************************************
     C                   Z-ADD     XXSEQ#        DESEQ#
     C*** END   ADD *** 09/27/01 **************************************
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      '398'         DEEOB
     C                   MOVEL     '398'         DEEOBCD
     C**** End Chg *** 07/18/23 **************************************
     C                   MOVEL(P)  XXNOT1        DENOT1
     C                   MOVE      *BLANKS       DENOT2
      *
     C*** Beg Add *** 04/20/13 ********************************************
      *
     C     CAAST         IFEQ      'FL'
     C*
     C                   MOVEL(P)  ' 93'         XXEOB
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVEL(P)  ' 93'         DEEOB
     C                   MOVEL(P)  ' 93'         DEEOBCD
     C**** End Chg *** 07/18/23 **************************************
     C*
     C                   MOVE      CACO#         @@CO#
     C*
     C                   CALL      'FA783R'
     C                   PARM                    @@CO#
     C                   PARM      XXEOB         @@EOB
     C                   PARM      BDCOM1        @@TEXT
     C                   PARM      *BLANKS       @@DSC1
     C                   PARM      *BLANKS       @@DSC2
     C                   PARM      *BLANKS       @@DSC3
     C                   PARM      *BLANKS       @@DSC4
     C*
     C                   MOVEL(P)  @@DSC1        DENOT1
     C                   MOVEL(P)  @@DSC2        DENOT2
     C                   MOVEL(P)  @@DSC3        DENOT3
     C                   MOVEL(P)  @@DSC4        DENOT4
     C*
     C                   ENDIF
     C*** End Add *** 04/20/13 ********************************************
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
     C*
     C                   ENDIF
     C*** END   ADD *** 07/27/01 **************************************
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
     C*** BEGIN CHG *** 06/14/00 **************************************
     C********   CAANMN    DIV  XXAM02    HLANMN
     C     DHUNTS        DIV       XXAM02        HLANMN
     C*** END CHG *** 06/14/00 ****************************************
     C** ROUND UP ALL FRACTIONS OF XXAM02 TO AN ADDITIONAL TIME UNIT
     C                   MVR                     HLADDT
     C                   ADD       HLANMN        HLADDT
     C                   Z-ADD     HLANMN        HLANTV
     C     HLANTV        MULT      XXAM02        XXANTV
     C*** BEGIN CHG *** 06/14/00 **************************************
     C********   CAANMN    SUB  XXANTV    XXANTV
     C     DHUNTS        SUB       XXANTV        XXANTV
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
     C*** BEGIN CHG *** 06/14/00 **************************************
     C********   XXMMRA    IFLE CACHRG
     C********             Z-ADDXXMMRA    CATOTA
     C********             Z-ADD*ZEROS    CATOTD
     C********   CACHRG    SUB  CATOTA    CANDIS
     C                   EXSR      DOCO
     C********             ELSE                             ELSE XXMMRA LT CATOT
     C********             Z-ADD*ZEROS    CANDIS
     C********             END                              END XXMMRA LT CACHRG
     C*** END CHG *** 06/14/00 ****************************************
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 06/14/00 **************************************
     C********************************************************************
     C** Apply carve out discount
     C********************************************************************
     C     DOCO          BEGSR
     C*
     C*          Carve out < Charged amount
     C*          ------------------------------------------------------
     C     XXMMRA        IFLT      CACHRG
     C     CACHRG        SUB       XXMMRA        CANDIS
     C                   Z-ADD     0             CAMRA
     C*** BEGIN ADD *** 07/27/01 **************************************
     C                   EXSR      REMMSG
     C*** END   ADD *** 07/27/01 **************************************
     C                   Z-ADD     XXMMRA        CAPAID
     C                   ENDIF                                                  MRA < TOTC
     C*
     C*          Carve out >= Charged amount
     C*          ------------------------------------------------------
     C     XXMMRA        IFGE      CACHRG
     C                   Z-ADD     0             CANDIS
     C                   Z-ADD     0             CAMRA
     C*** BEGIN ADD *** 07/27/01 **************************************
     C                   EXSR      REMMSG
     C*** END   ADD *** 07/27/01 **************************************
     C                   Z-ADD     CACHRG        CAPAID
     C                   ENDIF                                                  MRA < TOTC
     C*
     C*** BEGIN DEL *** 07/27/01 **************************************
     C*          Get current recon number
     C*          ------------------------------------------------------
     C********** EOKY      CHAINPEO01                99
     C********** *IN99     IFEQ *OFF
     C*
     C*          Remove adjudication messages from Detail EOB/NOTES
     C*          ------------------------------------------------------
     C********** DEKY      CHAINPDE01                99
     C********** *IN99     DOWEQ*OFF
     C********** DEESRC    IFEQ 'A'
     C********** DEDSEQ    ANDEQCADSEQ
     C**********           DELETPDE01
     C**********           ENDIF                           ESRC = 'A'
     C********** DEKY      READEPDE01                    99
     C**********           ENDDO                           *IN99 = Off
     C*
     C*          Remove adjudication messages from Prov bills errors
     C*          ------------------------------------------------------
     C********** MGKY      CHAINPMG01                99
     C********** *IN99     DOWEQ*OFF
     C********** MGSRC     IFEQ 'S'
     C**********           DELETPMG01
     C**********           ENDIF                           SRC = 'S'
     C********** MGKY      READEPMG01                    99
     C**********           ENDDO                           *IN99 = Off
     C**********           ENDIF                           *IN99 = Off
     C*** END   DEL *** 07/27/01 **************************************
     C                   ENDSR
     C*** END ADD *** 06/14/00 ****************************************
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
