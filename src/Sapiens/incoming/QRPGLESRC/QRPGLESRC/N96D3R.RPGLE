     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE N96D3R - TYPE D3 DISCOUNT CODE CALCULATIONS
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
     H*  09/04/09 C:227 PROJECT:114MUV
     H*                 Discount Code Modifications.
     H*
     H*  09/27/01 C:62  PROJECT:MC246
     H*                 ASSIGN DESEQ#.
     H*
     H*  06/14/00 C:79  Support: 22742 (48MBR)
     H*                 1. Correct fields names to use Pbills fields
     H*
     H*  03/30/99 C:31  Project: NW011
     H*                 Discount Calculation changes
     H*                 1. Replace CAIPOP with FDDHP and FDDUP
     H*                 2. Add Provder Bill changes
     H*                 Note: Program created from F95DFR
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
     FFMSVL1    IF   E           K DISK    USROPN
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

     D QCA             S             56    DIM(2) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*** BEGIN ADD *** 03/30/99 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*** END ADD *** 03/30/99 ****************************************
     D                 DS
     D  @@CMD                  1     55
     D  @@ST                  40     41
     D                 DS
     D  XXCMSV                 1     11
     D  XXRVCD                 1      3  0
     D  XXHCPC                 4      8
     D  XXUNTS                 9     11  0
     D*** BEGIN ADD *** 03/30/99 **************************************
     D XXNOT1          C                   CONST('NETWORK DISCOUNT AP-
     D                                     PLIED')

     I*** END ADD *** 03/30/99 ****************************************

     C*
     C*   KEY LIST FOR THE SERVICE CODE FILE
     C*
     C     SVKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCMSV
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
     C*** END ADD *** 03/07/95 *************************************************
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
     C*  INITIALIZATION SECTION
     C     *LIKE         DEFINE    CAAST         C1AST
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
     C*** BEG ADD *** 09/04/09 ****************************************
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       CLTID             3
     C*** END ADD *** 09/04/09 ****************************************
     C                   Z-ADD     X1FDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C*
     C                   MOVE      QQDSEQ        XXDSEQ            5 0
     C                   MOVE      QQCEDT        XXEDAT            8 0
     C*
     C*  GET THE CASE DETAIL RECORDS
     C*
     C     CAKEY         CHAIN     PCA01                              61
     C     *IN61         IFEQ      '0'
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
     C* GET STATE FIRST
     C     CAAST         IFEQ      *BLANKS
     C                   MOVE      'FL'          C1AST
     C                   ELSE
     C                   MOVE      CAAST         C1AST
     C                   END
     C*
     C*** CLOSE FILE IF OPENED
     C*
     C     STOPN         IFEQ      '1'
     C                   CLOSE     FMSVL1
     C                   MOVE      ' '           STOPN             1
     C                   MOVE      ' '           STOVR
     C                   END
     C*
     C*** DELETE OVERRIDE
     C     STOVR         IFEQ      '1'
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           STOVR             1
     C                   END
     C*
     C*** OVERRIDE FMPRL FILE TO SELECTED STATE CODE
     C     STOVR         IFEQ      ' '
     C                   Z-ADD     55            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     C1AST         @@ST                           EOMB ADJUD STATE
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           STOVR             1
     C                   END
     C*
     C*** OPEN FILE FOR PROCESSING
     C     STOPN         IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMSVL1
     C                   MOVE      '1'           STOPN
     C                   END
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
     C                   Z-ADD     *ZEROS        XXNDIS            9 2
     C*
     C*** RETREIVE SERVICE CODE RECORD TO DETERMINE WHICH FORMULA TO
     C*   USE IN CALC NETWORK DISCOUNT - MRA OR U & C.
     C*
     C*** BEGIN CHG *** 11/10/92 **************************************
     C*** BEG CHG *** 06/14/00 ****************************************
     C******************** Z-ADDCASVCD    XXRVCD  30
     C******************** MOVE CACPT4    XXHCPC  5
     C                   Z-ADD     DURVCD        XXRVCD            3 0
     C                   MOVE      DURATE        XXHCPC            5
     C*** END CHG *** 06/14/00 ****************************************
     C                   Z-ADD     *ZEROS        XXUNTS            3 0          ADD 05/31/92
     C*
     C     SVKEY         SETGT     PSV01
     C     NXTSV1        TAG
     C                   READP     PSV01                                  60
     C     *IN60         IFEQ      '0'
     C     SVCO#         ANDEQ     XXCO#
     C     SVCMSV        ANDEQ     XXCMSV                                       CHG 04/28/92
     C     SVSTAT        IFNE      'I'
     C*
     C* MRA --  TOTAL PAID, LESS 10% IN PATIENT CHARGES -- OR --
     C*         TOTAL PAID, LESS 20% OUT PATIENT CHARGES.
     C*
     C     SVALOW        IFGT      *ZEROS                                        MRA CALC
     C*** BEGIN CHG *** 03/30/99 **************************************
     C********   CAIPOP    IFEQ '1'
     C     XXPOS         IFEQ      '21'
     C********   CATOTP    MULT BDPC01    XXNDIS    H       INPATIENT
     C     CAPAID        MULT(H)   BDPC01        XXNDIS                          INPATIENT
     C********             ELSE                             ELSE CAIPOP
     C                   ELSE                                                    POS = '21'
     C********   CATOTP    MULT BDPC02    XXNDIS    H       OUT PATIENT
     C     CAPAID        MULT(H)   BDPC02        XXNDIS                          OUT PATIENT
     C********             END                              END CAIPOP
     C                   ENDIF                                                   POS = '21'
     C*** END CHG *** 03/30/99 ****************************************
     C*
     C                   ELSE                                                    ELSE SVALOW
     C*
     C* U&C --  TOTAL PAID, LESS 30% IN PATIENT CHARGES -- OR --
     C*         TOTAL PAID, LESS 40% OUT PATIENT CHARGES.
     C*
     C*** BEGIN CHG *** 03/30/99 **************************************
     C********   CAIPOP    IFEQ '1'
     C     XXPOS         IFEQ      '21'
     C********   CATOTP    MULT BDPC03    XXNDIS    H       INPATIENT
     C     CAPAID        MULT(H)   BDPC03        XXNDIS                          INPATIENT
     C********             ELSE                             ELSE CAIPOP
     C                   ELSE                                                    POS = '21'
     C********   CATOTP    MULT BDPC04    XXNDIS    H       OUTPATIENT
     C     CAPAID        MULT(H)   BDPC04        XXNDIS                          OUTPATIENT
     C********             END                              END CAIPOP
     C                   ENDIF                                                   POS = '21'
     C*** END CHG *** 03/30/99 ****************************************
     C*
     C                   END                                                     END SVALOW
     C*
     C                   ELSE
     C                   GOTO      NXTSV1
     C                   END
     C                   END
     C*** BEG ADD *** 09/04/09 ****************************************
     C     CLTID         IFEQ      '114'
     C     XXPOS         IFEQ      '21'
     C     CAPAID        MULT      BDPC01        XXNDIS
     C                   ELSE
     C     CAPAID        MULT      BDPC02        XXNDIS
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 09/04/09 ****************************************
     C*
     C                   Z-ADD     XXNDIS        CANDIS
     C*** BEGIN CHG *** 03/30/99 **************************************
     C********   CATOTP    SUB  CANDIS    CATOTP
     C     CAPAID        SUB       CANDIS        CAPAID
     C********   CATOTA    SUB  CANDIS    CATOTA
     C*** END CHG *** 03/30/99 ****************************************
     C*
     C*** BEGIN ADD *** 10/30/96 ***********************************************
     C*
     C*** IF CANDIS HAS AN AMOUNT THEN MOVE NEW NETWORK CODE INTO CAEOMB
     C*
     C     CANDIS        IFGT      *ZEROS
     C*** BEGIN CHG *** 03/30/99 **************************************
     C********             MOVE '398'     CAEOMB
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
     C                   ENDIF
     C*
     C*** END ADD *** 10/30/96 *************************************************
     C                   UPDATE    PCA01
     C*
     C                   END                                                     CLM STS CD
     C*                                                     NE "C"
     C                   ADD       CANDIS        TOTDIS            9 2           SAVE FOR EOB UPD
     C*
     C                   END                                                     END *IN61
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  QCMDEXC TABLE
DLTOVR FILE(FMSVL1)
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
