     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE N96D4R - TYPE D4 DISCOUNT CODE CALCULATIONS
     H*****************************************************************
     H* THIS PGM IS CALLED BY N9600R TO CALCULATE THE NETWORK DISCOUNT*
     H* AMT BASED ON THE PROVIDERS DISCOUNT CODE. THE DISCOUNT WILL   *
     H* ONLY BE CALCULATED FOR THE SINGLE SEQUENCE NUMBER PASSED TO   *
     H* THIS PROGRAM.                                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    N9600R                                                     *
     H*                                                               *
     H*  PROGRAMMER: C:20               PROJECT: 98/NW001             *
     H*  CREATED: 03/21/94                                            *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  07/18/23 C:342 Project: NY060
     H*                 NY Compliance: CARC/RARC Update
     H*
     H*  09/25/09 C:198 SUPPORT: 69800
     H*                 CORRECTED PROCESSING FOR DISCOUNTS SO THAT
     H*                 WHENEVER PER DIEM * (100- N/W DISCOUNT) IS
     H*                 MORE THAN BILLED CHARGES, BILLED CHARGES SHOULD
     H*                 BE PAID.
     H*
     H*  03/11/08 C:198 SUPPORT: 61779
     H*                 APPLIED NETWORK DISCOUNT BILLS CHARGED LESS THAN MRA
     H*
     H*  11/27/07 C:62  SUPPORT:MC010 60170
     H*                 IF FLORIDA INPATIENT BILL ALLOW PAID>CHARGED
     H*
     H*  04/11/03 C:79  SUPPORT:MC071 36813
     H*                 USE CAMRAA INSTEAD OF CADIFF. CADIFF IS ONLY
     H*                 POPULATED FOR FSMRA BILLS
     H*
     H*  09/27/01 C:62  PROJECT:MC246
     H*                 ASSIGN DESEQ#.
     H*
     H*  02/14/01 C:79  Support: MC044 26343
     H*                 If discount percent >= 0 use standard calcs
     H*                 If discount < 0 use charged as the limit.
     H*
     H*  02/02/01 C:62  Support: MC044 25964
     H*                 IF CALCULATED PAID AMOUNT IS GREATER THAN
     H*                 CHARGED, CALCULATE USING CHARGED AMOUNT.
     H*
     H*  01/30/01 C:62  Support: MC090 26034
     H*                 Only calc using CADIFF if this is OK or AL
     H*
     H*  01/30/01 C:62  Support: MC095 26091
     H*                 INCLUDE FSMRA PROCESSING.
     H*
     H*  12/18/00 C:62  Support: 24925
     H*                 CONDITION THE USE OF DIFF.
     H*
     H*  02/02/00 C:31  Support: 21679
     H*                 Network discount issue
     H*                 1. Change TOTA < CHRG logic
     H*
     H*  07/01/99 C:31  Support: 18000
     H*                 D4 network discount issue
     H*                 1. Add support for FSMRA
     H*
     H*  03/30/99 C:31  Project: NW011
     H*                 Discount Calculation changes
     H*                 1. Replace CAIPOP with FDDHP and FDDUP
     H*                 2. Add Provder Bill changes
     H*                 Note: Program created from F95DFR
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
     F*** BEGIN ADD *** 03/30/99 **************************************
     FFAEOLG    IF   E           K DISK
     FFDDHL     IF   E           K DISK
     FFDDUL     IF   E           K DISK
     FFDDEP     O    E             DISK
     F*** END ADD *** 03/30/99 ****************************************
     F*** BEGIN ADD *** 09/27/01 **************************************
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     F*** END   ADD *** 09/27/01 **************************************
     FFTABL     IF   E           K DISK

     D*** BEGIN ADD *** 03/30/99 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D XXNOT1          C                   CONST('NETWORK DISCOUNT AP-
     D                                     PLIED')

     I*** END ADD *** 03/30/99 ****************************************

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
     C*** BEGIN ADD *** 03/30/99 **************************************
     C*
     C*          PEO01 - EOMB Transaction File
     C     EOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFUND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    CAEOB#
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
     C*          PDH01 - EOMB Phys/HCFA Detail Transaction File
     C*          PDU01 - EOMB Hosp/UB Detail Transaction File
     C     DHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFUND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    XXDSEQ
     C*** END ADD *** 03/30/99 ****************************************
     C*
     C     *LIKE         DEFINE    BDDT          XXFDAT
     C*** BEGIN ADD *** 07/01/99 **************************************
     C     *LIKE         DEFINE    BDPC01        XXPC00
     C     *LIKE         DEFINE    CANDIS        XXNDIS
     C     *LIKE         DEFINE    CATOTA        XXTOTA
     C     *LIKE         DEFINE    CADIFF        XXDIFF
     C     *LIKE         DEFINE    CAMRAA        XXALOW
     C*** END ADD *** 07/01/99 ****************************************
     C*** BEGIN ADD *** 09/27/01 **************************************
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C*** END   ADD *** 09/27/01 **************************************
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
     C*** BEGIN ADD *** 03/30/99 **************************************
     C     EOKY          CHAIN     PEO01                              60
     C                   SELECT
     C     EOSYS         WHENEQ    'P'
     C  N60DHKY          CHAIN     PDH01                              60
     C  N60              MOVE      DHPOS         XXPOS             2
     C     EOSYS         WHENEQ    'H'
     C  N60DHKY          CHAIN     PDU01                              60
     C  N60              MOVE      DUPOS         XXPOS
     C                   ENDSL                                                  End select
     C*** END ADD *** 03/30/99 ****************************************
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
     C*** BEGIN DEL *** 07/01/99 **************************************
     C********             Z-ADD*ZEROS    XXNDIS  92
     C********
     C****DETERMINE DISCOUNT TO APPLY -- IF NON-FEE SCHEDULE USE BDPC03
     C*******... IF FEE SCHEDULE -- HOSPITAL TYPE CHARGES -- USE BDPC01
     C*******... IF FEE SCHEDULE -- NON/HOSP TYPE CHARGES -- USE BDPC02
     C********
     C********   CAICD9    IFEQ 'NO-FEE '                   NOT FEE SCH
     C****BEGIN CHG *** 03/30/99 **************************************
     C********   CATOTP    MULT BDPC03    XXNDIS    H
     C********   CAPAID    MULT BDPC03    XXNDIS    H
     C********             ELSE                             ELSE FEE SC
     C********
     C**IF*INPATIENT IS ENTERED USE BDPC01, USE BDPC02 FOR ALL OTHERS
     C********   CAIPOP    IFEQ '1'                         10/11/89 ADD
     C********   XXPOS     IFEQ '21'
     C********   CATOTP    MULT BDPC01    XXNDIS    H       HOSPITAL
     C********   CAPAID    MULT BDPC01    XXNDIS    H       HOSPITAL
     C********             ELSE                             ELSE NON-HOSP
     C********             ELSE                             POS = '21'
     C********   CATOTP    MULT BDPC02    XXNDIS    H
     C********   CAPAID    MULT BDPC02    XXNDIS    H
     C********             END                              END '1' CAIPOP
     C********             ENDIF                            POS = '21'
     C****END*CHG *** 03/30/99 ****************************************
     C********
     C********             END                              END CAICD9
     C********
     C********             Z-ADDXXNDIS    CANDIS
     C****BEGIN CHG *** 03/30/99 **************************************
     C********   CATOTP    SUB  CANDIS    CATOTP
     C********   CAPAID    SUB  CANDIS    CAPAID
     C********   CATOTA    SUB  CANDIS    CATOTA
     C****END*CHG *** 03/30/99 ****************************************
     C********
     C****IF*CANDIS HAS AN AMOUNT THEN MOVE NEW NETWORK CODE INTO CAEOMB
     C********
     C********   CANDIS    IFGT *ZEROS
     C****BEGIN CHG *** 03/30/99 **************************************
     C********             MOVE '398'     CAEOMB
     C********             MOVE CACO#     DECO#
     C********             MOVE CASUB#    DESUB#
     C********             MOVE CAFND     DEFND
     C********             MOVE CAFYR     DEFYR
     C********             MOVE CACASE    DECASE
     C********             MOVE CASS#     DESS#
     C********             MOVE CAACDT    DEACDT
     C********             MOVE CAEOB#    DEEOB#
     C********             MOVE CADSEQ    DEDSEQ
     C********             MOVE '398'     DEEOB
     C********             MOVE XXNOT1    DENOT1
     C********             MOVE *BLANKS   DENOT2
     C********             MOVE 'A'       DESTAT
     C********             MOVE 'N'       DEESRC
     C********             Z-ADDCARCN     DERCN
     C********             MOVE Q1USER    DEUSER
     C********             MOVE Q1PGM     DEPGM
     C********             MOVE Q1USER    DEIUSR
     C********             MOVE Q1PGM     DEIPGM
     C********             TIME           DSDTTM
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     DEDT
     C********             MOVELSRDTO     DEIDT
     C********             Z-ADDDSTIME    DETIME
     C********             Z-ADDDSTIME    DEITIM
     C********             WRITEPDE01
     C****END*CHG *** 03/30/99 ****************************************
     C********             ENDIF
     C********
     C********             UPDATPCA01
     C*** END DEL *** 07/01/99 ****************************************
     C*** BEGIN ADD *** 07/01/99 **************************************
     C*
     C*                Determine % off fee schedule
     C*                ------------------------------------------------
     C                   Z-ADD     *ZEROS        XXNDIS
     C*** BEGIN ADD *** 12/18/00 **************************************
     C     CACHRG        IFGE      CAPAID
     C*** END   ADD *** 12/18/00 **************************************
     C*** BEGIN ADD *** 01/30/01 **************************************
     C     CAAST         IFEQ      'OK'
     C     CAAST         OREQ      'AL'
     C*** END   ADD *** 01/30/01 **************************************
     C*** BEG CHG *** 04/11/03 ****************************************
     C********** CAPAID    ADD  CADIFF    XXALOW
     C*** BEG ADD *** 03/11/08 **************************************
     C*** BEG DEL *** 09/25/09 ****************************************
     C*********  CACHRG    IFGE CAMRAA
     C*** END DEL *** 09/25/09 ****************************************
     C*** END ADD *** 03/11/08 **************************************
     C                   Z-ADD     CAMRAA        XXALOW
     C*** EDN CHG *** 04/11/03 ****************************************
     C*** BEG ADD *** 03/11/08 **************************************
     C*** BEG DEL *** 09/25/09 ****************************************
     C***********          ELSE
     C***********          Z-ADDCAPAID    XXALOW
     C***********          ENDIF                           CACHRG >= CAMRAA
     C*** BEG DEL *** 09/25/09 ****************************************
     C*** END ADD *** 03/11/08 **************************************
     C*** BEGIN ADD *** 01/30/01 **************************************
     C                   ELSE
     C                   Z-ADD     CAPAID        XXALOW
     C                   ENDIF
     C*** END   ADD *** 01/30/01 **************************************
     C*** BEGIN ADD *** 12/18/00 **************************************
     C                   ELSE
     C                   Z-ADD     CAPAID        XXALOW
     C                   ENDIF
     C*** END   ADD *** 12/18/00 **************************************
     C                   Z-ADD     0             XXNDIS
     C     CAICD9        IFEQ      'NO-FEE'
     C                   Z-ADD     BDPC03        XXPC00
     C                   ELSE                                                   ICD9 = 'NO-FEE'
     C********   CAIPOP    IFEQ '1'
     C     XXPOS         IFEQ      '21'
     C                   Z-ADD     BDPC01        XXPC00
     C********             ELSE                            IPOP = '1'
     C                   ELSE                                                    POS = '21'
     C                   Z-ADD     BDPC02        XXPC00
     C********             ENDIF                           IPOP = '1'
     C                   ENDIF                                                   POS = '21'
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
     C********   CAPAID    ANDNE0
     C********   CAPAID    SUB  XXTOTA    CANDIS
     C********   CAPAID    SUB  CANDIS    CAPAID
     C********   CATOTA    SUB  CANDIS    CATOTA
     C********             ENDIF                           TOTA < TOTC
     C*** END DEL *** 02/02/00 ****************************************
     C*
     C*                If state = 'OK' then
     C*                   CADIFF = MRA$ - (MRA$ * NTWK%) - Charges
     C*                ------------------------------------------------
     C     CAAST         IFEQ      'OK'
     C     CAAST         OREQ      'AL'
     C*** BEGIN ADD *** 02/02/00 **************************************
     C     XXTOTA        IFLT      CACHRG
     C     XXTOTA        ANDNE     0
     C     CAPAID        ANDNE     0
     C     CAPAID        SUB       XXTOTA        CANDIS
     C     CAPAID        SUB       CANDIS        CAPAID
     C     CATOTA        SUB       CANDIS        CATOTA
     C                   ENDIF                                                  TOTA < TOTC
     C*** END ADD *** 02/02/00 ****************************************
     C     XXALOW        MULT(H)   XXPC00        XXDIFF
     C     XXALOW        SUB       XXDIFF        XXDIFF
     C     XXDIFF        SUB       CACHRG        CADIFF
     C     XXALOW        SUB       XXNDIS        CAMNAA
     C     CADIFF        IFLE      0
     C                   Z-ADD     0             CADIFF
     C                   ENDIF                                                  DIFF <= 0
     C*** BEGIN ADD *** 02/02/00 **************************************
     C                   ELSE                                                   AST = 'OK'
     C*** BEG ADD *** 02/14/01 ****************************************
     C     XXPC00        IFLT      0
     C*** END ADD *** 02/14/01 ****************************************
     C*** BEGIN ADD *** 02/02/01 **************************************
     C     CACHRG        IFGE      XXTOTA
     C*** END   ADD *** 02/02/01 **************************************
     C     XXTOTA        IFNE      0
     C     CAPAID        ANDNE     0
     C     CAPAID        SUB       XXTOTA        CANDIS
     C     CAPAID        SUB       CANDIS        CAPAID
     C     CATOTA        SUB       CANDIS        CATOTA
     C                   ENDIF                                                  TOTA <> 0
     C*** END ADD *** 02/02/00 ****************************************
     C*** BEGIN ADD *** 02/02/01 **************************************
     C                   ELSE
     C*** BEG ADD *** 02/14/01 ****************************************
     C     CAPAID        IFLE      CACHRG
     C*** END ADD *** 02/14/01 ****************************************
     C     CACHRG        IFNE      0
     C     CAPAID        ANDNE     0
     C     CAPAID        SUB       CACHRG        CANDIS
     C     CAPAID        SUB       CANDIS        CAPAID
     C     CAPAID        SUB       CANDIS        CATOTA
     C                   ENDIF                                                  TOTA <> 0
     C*** BEG ADD *** 02/14/01 ****************************************
     C                   ENDIF                                                  CAPAID<=CACHRG
     C*** END ADD *** 02/14/01 ****************************************
     C                   ENDIF
     C*
     C*** BEG ADD *** 02/14/01 ****************************************
     C* Network discount >= 0 *****************************************
     C                   ELSE
     C*
     C     XXTOTA        IFNE      0
     C     CAPAID        ANDNE     0
     C     CAPAID        SUB       XXTOTA        CANDIS
     C     CAPAID        SUB       CANDIS        CAPAID
     C     CATOTA        SUB       CANDIS        CATOTA
     C                   ENDIF                                                  TOTA <> 0
     C*
     C                   ENDIF                                                  XXPC00 < 0
     C*** END ADD *** 02/14/01 ****************************************
     C*** END   ADD *** 02/02/01 **************************************
     C                   ENDIF                                                  AST = 'OK'
     C*** BEGIN ADD *** 11/27/07 **************************************
     C*
     C* FLORIDA INPATIENT BILLS
     C*
     C     CAAST         IFEQ      'FL'
     C     XXPOS         ANDEQ     '21'
     C                   Z-ADD     XXTOTA        CAPAID
     C                   Z-ADD     XXNDIS        CANDIS
     C                   ENDIF
     C*** END   ADD *** 11/27/07 **************************************
     C*
     C*                If discount applies: update Case Detail
     C*                ------------------------------------------------
     C*** BEGIN CHG *** 11/27/07 **************************************
     C********** CANDIS    IFGT *ZEROS
     C     CANDIS        IFNE      *ZEROS
     C*** END   CHG *** 11/27/07 **************************************
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
     C                   ENDIF                                                  NDIS > 0
     C*
     C                   UPDATE    PCA01
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
