     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N96DFR - TYPE DF DISCOUNT CODE CALCULATIONS
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program calculates the CorVel Network discount     **
     H**     amount as follows:                                      **
     H**     . If Outpatient the lessor of                           **
     H**          CAPAID * BDPC01 (Discount % off Fee Schedule)      **
     H**          CACHRG * BDPC02 (Billed Amt Discount %)            **
     H**     . If Inpatient and CAPAID <= CACHRG                     **
     H**          CAPAID * BDPC03 (Inpatient Discount %)             **
     H**          CACHRG * BDPC04 (Inpatient Billed Amt Discount %)  **
     H**     . If Inpatient and CAPAID > CACHRG                      **
     H**          CAPAID * BDPC03 (Inpatient Discount %)             **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     N9600R - Process Detail Charges to Correct Discount     **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**                                                             **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED: 05/21/98         PROJECT: 51MCT                  **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  07/18/23 C:342 Project: NY060
     H*                 NY Compliance: CARC/RARC Update
     H*
     H*  09/27/01 C:62  PROJECT:MC246
     H*                 ASSIGN DESEQ#.
     H*
     H*  11/14/00 C:79  Project: FLMNT 24925
     H*                 Discount Calculation changes
     H*                 1. For inpatient per diem, if CAPAID > CACHRG
     H*                    then take the discount off CAPAID
     H*
     H*  03/30/99 C:31  Project: NW011
     H*                 Discount Calculation changes
     H*                 1. Replace FDHCP with FDDHP and FDDUP
     H*                 2. Add Provder Bill changes
     H*                 Note: Program created from F95DFR
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      60      Case Detail File not found                     **
     H**      61      Billing Rate/Discount Master not found         **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      FDCALN (PCA01): Case Detail File (CO#/SUB#/FND/FYR/    **
     H**                      CASE/FEIN/MOD/FDAT/DSEQ/FLAG <> 'P')   **
     H**      FDHCL  (PHC01): Detail Charge Auxiliary                **
     H**                      (CO#/SUB/FND/FYR/CASE/DSEQ)            **
     H**      NMBDLA (PBD01): Billing Rate/Discount Master           **
     H**                      (CODE/EFDT/STAT <> 'I')                **
     H*****************************************************************
     H*
     FFDCALN    UF   E           K DISK
     F*** BEGIN CHG *** 03/30/99 **************************************
     F********FDHCL   IF  E           K        DISK
     FFAEOLG    IF   E           K DISK
     FFDDHL     IF   E           K DISK
     FFDDUL     IF   E           K DISK
     FFDDEP     O    E             DISK
     F*** END CHG *** 03/30/99 ****************************************
     F*** BEGIN ADD *** 09/27/01 **************************************
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     F*** END   ADD *** 09/27/01 **************************************
     FNMBDLA    IF   E           K DISK

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
     C* ---------------------------------------------------------------
     C*        KEY LISTS
     C* ---------------------------------------------------------------
     C*          PCA01 - Case Detail File
     C     CAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFUND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXFDAT            7 0
     C                   KFLD                    XXDSEQ
     C*** BEGIN CHG *** 03/30/99 **************************************
     C********   *LIKE     DEFN CATOTA    XXTOTA
     C     *LIKE         DEFINE    CAPAID        XXPAID
     C********   *LIKE     DEFN CATOTC    XXTOTC
     C     *LIKE         DEFINE    CACHRG        XXCHRG
     C********   *LIKE     DEFN CANDIS    XXNDIS
     C     *LIKE         DEFINE    CANDIS        XXNDIS
     C*** END CHG *** 03/30/99 ****************************************
     C*
     C*          PBD01 - Billing Rate/Discount Master
     C     BDKY          KLIST
     C                   KFLD                    QQCODE            3
     C                   KFLD                    CAFDAT
     C*** BEGIN DEL *** 03/30/99 **************************************
     C********
     C********   PHC01 - Detail Charge Auxiliary
     C********   HCKY      KLIST
     C********             KFLD           XXCO#
     C********             KFLD           XXSUB#
     C********             KFLD           XXFUND
     C********             KFLD           XXFYR
     C********             KFLD           XXCASE
     C********             KFLD           XXDSEQ
     C*** END DEL *** 03/30/99 ****************************************
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
     C*** BEGIN ADD *** 09/27/01 **************************************
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C*** END   ADD *** 09/27/01 **************************************
     C*
     C* ---------------------------------------------------------------
     C*        PARM LISTS
     C* ---------------------------------------------------------------
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
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*          M A I N L I N E
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*
     C*          Initialize FDCAP key
     C*          ------------------------------------------------------
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQFUND        XXFUND            3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   MOVE      QQFEIN        XXFEIN            9 0
     C                   MOVE      QQMOD         XXMOD             3 0
     C                   MOVE      QQFDAT        XXFDT8            8 0
     C                   Z-ADD     XXFDT8        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C                   MOVE      QQDSEQ        XXDSEQ            5 0
     C                   MOVE      QQCEDT        XXEDAT            8 0
     C*
     C*          Get Case Detail and Detail Auxilary File records
     C*          ------------------------------------------------------
     C     CAKY          CHAIN     PCA01                              60
     C*** BEGIN CHG *** 03/30/99 **************************************
     C**N60****  HCKY      CHAINPHC01                60
     C  N60EOKY          CHAIN     PEO01                              60
     C                   SELECT
     C     EOSYS         WHENEQ    'P'
     C  N60DHKY          CHAIN     PDH01                              60
     C  N60              MOVE      DHPOS         XXPOS             2
     C     EOSYS         WHENEQ    'H'
     C  N60DHKY          CHAIN     PDU01                              60
     C  N60              MOVE      DUPOS         XXPOS
     C                   ENDSL                                                  End select
     C*** END CHG *** 03/30/99 ****************************************
     C*
     C*             Get most current Discount Master record
     C*             ---------------------------------------------------
     C     *IN60         IFEQ      *OFF
     C     BDKY          SETGT     PBD01
     C                   READP     PBD01                                  61
     C     *IN61         IFEQ      *OFF
     C     BDCODE        ANDEQ     QQCODE
     C*** BEGIN DEL *** 03/30/99 **************************************
     C********
     C********         Most current Discount Master record found
     C********         If Inpatient the lessor of
     C********           CATOTA * BDPC03 (Inpatient Discount %)
     C********           CATOTC * BDPC04 (Inpatient Bill Amt Discnt %)
     C********         ------------------------------------------------
     C********   HCPOS     IFEQ '21'
     C********   HCPOS     OREQ '51'
     C********   HCPOS     OREQ '61'
     C********   CATOTA    MULT BDPC03    XXNDIS
     C********   CATOTA    SUB  XXNDIS    XXTOTA
     C********   CATOTC    MULT BDPC04    XXNDIS
     C********   CATOTC    SUB  XXNDIS    XXTOTC
     C********             ELSE                            POS = '21'
     C********
     C********         If Outpatient the lessor of
     C********           CATOTA * BDPC01 (Discount % off Fee Schedule)
     C********           CATOTC * BDPC02 (Billed Amt Discount %)
     C********         ------------------------------------------------
     C********   CATOTA    MULT BDPC01    XXNDIS
     C********   CATOTA    SUB  XXNDIS    XXTOTA
     C********   CATOTC    MULT BDPC02    XXNDIS
     C********   CATOTC    SUB  XXNDIS    XXTOTC
     C********             ENDIF                           POS = '21'
     C********
     C********         Update FDCAL if there is a network discount
     C********         ------------------------------------------------
     C********   XXTOTA    IFLT XXTOTC
     C********   CATOTA    SUB  XXTOTA    CANDIS
     C********             ELSE                            TOTA < TOTC
     C********   CATOTP    SUB  XXTOTC    CANDIS
     C********             ENDIF                           TOTA < TOTC
     C********   CANDIS    IFGT 0
     C********   CATOTA    SUB  CANDIS    CATOTA
     C********   CATOTP    SUB  CANDIS    CATOTP
     C********             MOVE '398'     CAEOMB
     C*** END DEL *** 03/30/99 ****************************************
     C*** BEGIN ADD *** 03/30/99 **************************************
     C*
     C*                Most current Discount Master record found
     C*                If Inpatient and CAPAID <= CACHRG the lessor of
     C*                  CAPAID * BDPC03 (Inpatient Discount %)
     C*                  CACHRG * BDPC04 (Inpatient Bill Amt Discnt %)
     C*                ------------------------------------------------
     C     XXPOS         IFEQ      '21'
     C     XXPOS         OREQ      '51'
     C     XXPOS         OREQ      '61'
     C     CAPAID        MULT      BDPC03        XXNDIS
     C     CAPAID        SUB       XXNDIS        XXPAID
     C     CACHRG        MULT      BDPC04        XXNDIS
     C     CACHRG        SUB       XXNDIS        XXCHRG
     C                   ELSE                                                   POS = ' 21'
     C*
     C*                If Outpatient the lessor of
     C*                  CAPAID * BDPC01 (Discount % off Fee Schedule)
     C*                  CACHRG * BDPC02 (Billed Amt Discount %)
     C*                ------------------------------------------------
     C     CAPAID        MULT      BDPC01        XXNDIS
     C     CAPAID        SUB       XXNDIS        XXPAID
     C     CACHRG        MULT      BDPC02        XXNDIS
     C     CACHRG        SUB       XXNDIS        XXCHRG
     C                   ENDIF                                                  POS = ' 21'
     C*
     C*                Update FDCAL if there is a network discount
     C*                ------------------------------------------------
     C*** BEG ADD *** 11/14/00 ****************************************
     C* If CAPAID <= CACHRG take the lesser of XXPAID and XXCHRG
     C     CAPAID        IFLE      CACHRG
     C*** END ADD *** 11/14/00 ****************************************
     C     XXPAID        IFLT      XXCHRG
     C     CAPAID        SUB       XXPAID        CANDIS
     C                   ELSE                                                   PAID < CHRG
     C     CAPAID        SUB       XXCHRG        CANDIS
     C                   ENDIF                                                  PAID < CHRG
     C*
     C*** BEG ADD *** 11/14/00 ****************************************
     C* If CAPAID > CACHRG take the discount off CAPAID
     C                   ELSE
     C     CAPAID        SUB       XXPAID        CANDIS
     C*
     C                   ENDIF                                                  CAPAID LE CACHRG
     C*** END ADD *** 11/14/00 ****************************************
     C     CANDIS        IFGT      0
     C     CAPAID        SUB       CANDIS        CAPAID
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
     C*** END ADD *** 03/30/99 ****************************************
     C                   UPDATE    PCA01
     C                   ENDIF                                                  NDIS > 0
     C*
     C                   ENDIF                                                  *IN61 = Off
     C                   ENDIF                                                  *IN60 = Off
     C*
     C*          End of Program
     C*          ------------------------------------------------------
     C                   SETON                                        LR
