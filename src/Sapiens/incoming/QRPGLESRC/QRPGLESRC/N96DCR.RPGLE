     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N96DCR - TYPE DC DISCOUNT CODE CALCULATIONS
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program caculates the network discount amount by   **
     H**     taking a percentage off the State Fee Schedule Reim-    **
     H**     bursement Amount and comparing that amount to the bill- **
     H**     ed charges and pay whichever is less.                   **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     N9500R - Process Detail Charges to Correct Discount     **
     H**   CALLS TO:                                                 **
     H**     QCMDEXC - IBM Command                                   **
     H**                                                             **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   10/21/97      PROJECT: 48MAY                   **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  07/18/23 C:342 Project: NY060
     H*                 NY Compliance: CARC/RARC Update
     H*
     H*  12/24/01 C:62  PROJECT:95MBB
     H*                 CHANGE ALGORITHM
     H*
     H*  09/27/01 C:62  PROJECT:MC246
     H*                 ASSIGN DESEQ#.
     H*
     H*  12/18/00 C:62  Support: 24925
     H*                 CONDITION THE USE OF DIFF.
     H*
     H*  06/03/99 C:31  Support: 18284
     H*                 Negative network discount
     H*                 1. Replace with CAMRAA with CAPAID + CADIFF
     H*
     H*  03/30/99 C:31  Project: NW011
     H*                 Discount Calculation changes
     H*                 1. Replace CAIPOP with FDDHP and FDDUP
     H*                 2. Add Provder Bill changes
     H*                 Note: Program created from F95DFR
     H*
     H*  02/19/99 C:31  Project: 95MAR
     H*                 DC discount changes
     H*                 1. Add type 4 provider support
     H*                 2. Add No-Fee support
     H*                 3. Add CAMNAA support
     H*
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      60      General Usage                                  **
     H**      61      Case Detail File not found                     **
     H**      80      Update Case Detail record                      **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      FDCALN (PCA01): Case Detail File (CO#/SUB#/FND/FYR/    **
     H**                      CASE/FEIN/MOD/FDAT/DSEQ/FLAG <> 'P')   **
     H**      FTABL         : Logical over table files (CO#/KEY)     **
     H**             (PCS01): All States Claim Status Code Table     **
     H**      NMBDLD (PBD01): Billing Rate/Discount Master           **
     H**                      (CODE/EFDT/RCDT = 'D')                 **
     H*****************************************************************
     H*
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
     F                                     IGNORE(PRC01)
     FNMBDLD    IF   E           K DISK

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
     C*
     C*          FDCALN (PCA01): Case Detail File
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
     C*
     C*         NMBDLD (PBD01): Billing Rate/Discount Master
     C     BDKY          KLIST
     C                   KFLD                    QQCODE            3
     C                   KFLD                    XXFDAT
     C*
     C*          FTABL  (PCS01): All States Claim Status Code Table
     C     CSKY          KLIST
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
     C* ---------------------------------------------------------------
     C*        DEFINES
     C* ---------------------------------------------------------------
     C     *LIKE         DEFINE    BDPC01        XXPC00
     C     *LIKE         DEFINE    CAAST         XXAST
     C     *LIKE         DEFINE    CANDIS        XXNDIS
     C     *LIKE         DEFINE    CATOTA        XXTOTA
     C     *LIKE         DEFINE    CADIFF        XXDIFF
     C     *LIKE         DEFINE    CAUNTS        X1UNTS
     C     *LIKE         DEFINE    CAMRAA        XXALOW
     C*** BEGIN ADD *** 12/24/01 **************************************
     C     *LIKE         DEFINE    CAMRAA        XXLOWL
     C*** END   ADD *** 12/24/01 **************************************
     C*
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*          M A I N L I N E
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*
     C*          Move parms to keys
     C*          ------------------------------------------------------
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
     C                   MOVE      QQCODE        XXCODE            3
     C*
     C*          Get case detail record
     C*          ------------------------------------------------------
     C     CAKY          CHAIN     PCA01                              61
     C     *IN61         IFEQ      '0'
     C*** BEGIN CHG *** 03/30/99 **************************************
     C*** BEGIN CHG *** 02/19/99 **************************************
     C********   CATOTP    ANDNE0
     C     CAPAID        ANDNE     0
     C*** END CHG *** 02/19/99 ****************************************
     C*** END CHG *** 03/30/99 ****************************************
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
     C*
     C*             Set up adjudication state
     C*             ---------------------------------------------------
     C     CAAST         IFEQ      *BLANKS
     C                   MOVE      'OK'          XXAST
     C                   ELSE                                                   AST = Blanks
     C                   MOVE      CAAST         XXAST
     C                   ENDIF                                                  AST = Blanks
     C*
     C*             Get claim status code - do not process if 'C' (CAP)
     C*             ---------------------------------------------------
     C                   MOVE      *BLANKS       TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     CSKY          CHAIN     PCS01                              60
     C     *IN60         IFEQ      '0'
     C     CSDISC        ANDNE     'C'
     C*
     C*                Get Discount Master for date of service
     C*                ------------------------------------------------
     C                   Z-ADD     0             XXPC00
     C                   Z-ADD     0             XXNDIS
     C                   Z-ADD     0             XXALOW
     C     BDKY          SETGT     PBD01
     C                   READP     PBD01                                  60
     C     *IN60         DOWEQ     *OFF
     C     BDCODE        ANDEQ     QQCODE
     C     BDSTAT        IFNE      'I'
     C                   LEAVE
     C                   ENDIF                                                  STAT = 'I'
     C                   READP     PBD01                                  60
     C                   ENDDO                                                  *IN60 = Off
     C*
     C*                Determine % off fee schedule
     C*                ------------------------------------------------
     C*** BEGIN CHG *** 12/24/01 **************************************
     C*** BEGIN CHG *** 06/03/99 **************************************
     C********             Z-ADDCAMRAA    XXALOW
     C*** BEGIN ADD *** 12/18/00 **************************************
     C********   CACHRG    IFGE CAPAID
     C*** END   ADD *** 12/18/00 **************************************
     C********   CAPAID    ADD  CADIFF    XXALOW
     C*** BEGIN ADD *** 12/18/00 **************************************
     C********             ELSE
     C********             Z-ADDCAPAID    XXALOW
     C********             ENDIF
     C*** END   ADD *** 12/18/00 **************************************
     C*** END CHG *** 06/03/99 ****************************************
     C*** BEGIN CHG *** 02/19/99 **************************************
     C********   CAMRAA    MULT XXPC01    XXNDIS    H
     C********             Z-ADD0         XXNDIS
     C********   CAICD9    IFEQ 'NO-FEE'
     C********             Z-ADDBDPC03    XXPC00
     C********             ELSE                            ICD9 = 'NO-FEE'
     C*** BEGIN CHG *** 03/30/99 **************************************
     C********   CAIPOP    IFEQ '1'
     C********   XXPOS     IFEQ '21'
     C********             Z-ADDBDPC01    XXPC00
     C********             ELSE                            IPOP = '1'
     C********             ELSE                             POS = '21'
     C********             Z-ADDBDPC02    XXPC00
     C********             ENDIF                           IPOP = '1'
     C********             ENDIF                            POS = '21'
     C********             ENDIF                           ICD9 = 'NO-FEE'
     C*** END CHG *** 02/19/99 ****************************************
     C*** BEGIN CHG *** 06/03/99 **************************************
     C********   CAMRAA    MULT XXPC00    XXNDIS    H
     C********   XXALOW    MULT XXPC00    XXNDIS    H
     C********   CAMRAA    SUB  XXNDIS    XXTOTA
     C********   XXALOW    SUB  XXNDIS    XXTOTA
     C*** END CHG *** 06/03/99 ****************************************
     C*
     C*                Update Paid, Allowed, Discount, and EOMB code
     C*                ------------------------------------------------
     C********             SETOF                     80
     C*** BEGIN CHG *** 03/30/99 **************************************
     C********   XXTOTA    IFLT CATOTC
     C********   XXTOTA    IFLT CACHRG
     C********   XXTOTA    ANDNE0
     C********   CATOTP    ANDNE0
     C********   CAPAID    ANDNE0
     C********   CATOTP    SUB  XXTOTA    CANDIS
     C********   CAPAID    SUB  XXTOTA    CANDIS
     C********   CATOTP    SUB  CANDIS    CATOTP
     C********   CAPAID    SUB  CANDIS    CAPAID
     C********   CATOTA    SUB  CANDIS    CATOTA
     C********             MOVE '398'     CAEOMB
     C*
     C     CAICD9        IFEQ      'NO-FEE'
     C                   Z-ADD     BDPC03        XXPC00
     C                   ELSE                                                   ICD9 = 'NO-FEE'
     C     XXPOS         IFEQ      '21'
     C                   Z-ADD     BDPC01        XXPC00
     C                   ELSE                                                    POS = '21'
     C                   Z-ADD     BDPC02        XXPC00
     C                   ENDIF                                                   POS = '21'
     C                   ENDIF                                                  ICD9 = 'NO-FEE'
     C*
     C     CAMRAA        MULT      XXPC00        XXDIFF
     C     CAMRAA        SUB       XXDIFF        XXLOWL
     C*
     C     XXLOWL        IFLT      CACHRG
     C     XXLOWL        ANDNE     *ZEROS
     C     CAPAID        ANDNE     *ZEROS
     C                   Z-ADD     0             XXNDIS
     C*
     C     CAPAID        IFGT      XXLOWL
     C     CAPAID        IFGE      CAMRAA
     C                   Z-ADD     XXDIFF        CANDIS
     C                   SUB       CANDIS        CAPAID
     C                   ELSE
     C     CAPAID        SUB       XXLOWL        CANDIS
     C                   SUB       CANDIS        CAPAID
     C                   ENDIF
     C                   ENDIF
     C*** END   CHG *** 12/24/01 **************************************
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
     C*** END CHG *** 03/30/99 ****************************************
     C                   SETON                                        80
     C                   ENDIF                                                  TOTA < TOTC
     C*
     C*                If state = 'OK' then
     C*                   CADIFF = MRA$ - (MRA$ * NTWK%) - Charges
     C*                ------------------------------------------------
     C     XXAST         IFEQ      'OK'
     C     XXALOW        MULT(H)   XXPC00        XXDIFF
     C     XXALOW        SUB       XXDIFF        XXDIFF
     C*** BEGIN ADD *** 03/30/99 **************************************
     C********   XXDIFF    SUB  CATOTC    CADIFF
     C     XXDIFF        SUB       CACHRG        CADIFF
     C*** END ADD *** 03/30/99 ****************************************
     C*** BEGIN CHG *** 02/19/99 **************************************
     C*** BEGIN CHG *** 06/03/99 **************************************
     C********   CAMRAA    SUB  XXNDIS    CAMNAA    H
     C     XXALOW        SUB(H)    XXNDIS        CAMNAA
     C*** END CHG *** 06/03/99 ****************************************
     C     CADIFF        IFLE      0
     C                   Z-ADD     0             CADIFF
     C                   ENDIF                                                  DIFF <= 0
     C                   SETON                                        80
     C*** END CHG *** 02/19/99 ****************************************
     C                   ENDIF                                                  AST = 'OK'
     C*
     C*** BEGIN ADD *** 01/10/02 **************************************
     C                   Z-ADD     *ZEROS        CADCS#
     C     XXPC00        IFNE      *ZEROS
     C                   MOVE      XXPC00        CADCS#
     C                   ENDIF
     C*** END   ADD *** 01/10/02 **************************************
     C*                If discount applies: update Case Detail
     C*                ------------------------------------------------
     C   80              UPDATE    PCA01
     C*
     C                   ENDIF                                                   *IN60 = Off
     C                   ENDIF                                                   *IN61 = Off
     C*
     C*          End of Program
     C*          ------------------------------------------------------
     C                   SETON                                        LR
