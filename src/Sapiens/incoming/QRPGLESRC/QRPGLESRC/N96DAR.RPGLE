     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N96DAR - Network Discount Type DA Processing Program
     H*****************************************************************
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    N9601R                                                     *
     H*                                                               *
     H*  CALLs To:                                                    *
     H*    FA783R                                                     *
     H*                                                               *
     H*  PROGRAMMER: C:284              PROJECT: 147MAS               *
     H*  CREATED: 04/30/2013                                          *
     H*                                                               *
     H*****************************************************************
     H** REVISIONS:
     H*                                                               *
     H*  07/18/23 C:342 Project: NY060                                *
     H*                 NY Compliance: CARC/RARC Update               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*****************************************************************
     FNMBDLD    IF   E           K DISK
     FFMPRL     IF   E           K DISK    USROPN
     FFDCALN    UF   E           K DISK
     FFAEOLG    IF   E           K DISK
     FFDDHL     IF   E           K DISK
     FFDDUL     IF   E           K DISK
     FFDDEP     O    E             DISK
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFTABL     IF   E           K DISK

     D QCA             S             56    DIM(2) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  @@CMD                  1     55
     D  @@ST                  38     39
     D XXNOT1          C                   CONST('NETWORK DISCOUNT AP-
     D                                     PLIED')
     D*================================================================
     D* Statnd Alone Variable
     D*================================================================
     D @@DSC1          s             70
     D @@DSC2          s             70
     D @@DSC3          s             70
     D @@DSC4          s             70
     D @@EOB           s                   like(caeob#)
     D XXCPT4          s                   like(cacpt4)

     C*
     C*   KEY LIST FOR THE SERVICE CODE FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CACPT4
     C                   KFLD                    CAFDAT
     C*
     C     PRKEY1        KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CACPT4
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
     C                   KFLD                    XXFDAT
     C*
     C*  CLAIM STATUS CODE TABLE FILE
     C*
     C     TCSKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    TABKEY           10
     C*
     C*          PEO01 - EOMB Transaction File
     C     EOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFUND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    CAEOB#
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
     C*
     C     *LIKE         DEFINE    CADT          XXFDAT
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
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
     C                   Z-ADD     X1FDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C*
     C                   MOVE      QQDSEQ        XXDSEQ            5 0
     C                   MOVE      QQCODE        XXCODE            3
     C*
     C*  GET THE CASE DETAIL RECORDS
     C*
     C     CAKEY         CHAIN     PCA01                              61
     C     *IN61         IFEQ      '0'
     C     EOKY          CHAIN     PEO01                              60
     C                   SELECT
     C     EOSYS         WHENEQ    'P'
     C  N60DHKY          CHAIN     PDH01                              60
     C  N60              MOVE      DHPOS         XXPOS             2
     C     EOSYS         WHENEQ    'H'
     C  N60DHKY          CHAIN     PDU01                              60
     C  N60              MOVE      DUPOS         XXPOS
     C                   ENDSL                                                  End select
     C*
     C* GET STATE FIRST
     C*
     C     CAAST         IFEQ      *BLANKS
     C                   MOVE      'FL'          C1AST
     C                   ELSE
     C                   MOVE      CAAST         C1AST
     C                   END
     C*
     C*** CLOSE FILE IF OPENED
     C*
     C     STOPN         IFEQ      '1'
     C                   CLOSE     FMPRL
     C                   MOVE      ' '           STOPN             1
     C                   MOVE      ' '           STOVR
     C                   END
     C*
     C*** DELETE OVERRIDE
     C*
     C     STOVR         IFEQ      '1'
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           STOVR             1
     C                   END
     C*
     C*** OVERRIDE FMPRL FILE TO SELECTED STATE CODE
     C*
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
     C*
     C     STOPN         IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMPRL
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
     C*****************************************************************
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
     C*****************************************************************
     C                   Z-ADD     *ZEROS        XXNDIS            9 2
     C                   Z-ADD     *ZEROS        xxmra             9 2
     C*
     C*** RETREIVE SERVICE CODE RECORD TO DETERMINE WHICH FORMULA TO
     C*   USE IN CALC NETWORK DISCOUNT - MRA OR U & C.
     C*
     C                   MOVE      CACPT4        XXCPT4
     C     PRKEY         SETGT     PPR01
     C     NXTPR1        TAG
     C     PRKEY1        READPE    PPR01                                  60
     C     *IN60         IFEQ      '0'
     C     PRCO#         ANDEQ     XXCO#
     C     PRCPT4        ANDEQ     XXCPT4                                       CHG 04/28/92
     C     PRSTAT        IFNE      'I'
     C*
     C     PR92N1        IFGT      *ZEROS                                        MRA CALC
     C*
     C     CAPAID        MULT(H)   BDPC01        xxmra
     C*
     C     CAPAID        SUB       xxmra         xxndis
     C                   Z-ADD     XXNDIS        CANDIS
     C     CAPAID        SUB       CANDIS        CAPAID
     C*
     C                   END                                                     END SVALOW
     C*
     C                   ELSE
     C                   GOTO      NXTPR1
     C                   END
     C                   END
     C*
     C*
     C*** IF CANDIS HAS AN AMOUNT THEN MOVE NEW NETWORK CODE INTO CAEOMB
     C*
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
     C****               MOVE      ' 93'         DEEOB
     C                   MOVEL     ' 93'         DEEOBCD
     C**** End Chg *** 07/18/23 **************************************
     C                   MOVEL(P)  XXNOT1        DENOT1
     C                   MOVE      *BLANKS       DENOT2
     C                   If        CAAST = 'FL'
     C*
     C                   MOVE      CACO#         XXCO#
     C                   MOVE      CAEOB#        @@EOB
     C                   Eval      @@TEXT = 'Ambulatory Surgical Centre.'
     C                   CALL      'FA783R'
     C                   PARM                    XXCO#
     C**** Beg Chg *** 07/18/23 **************************************
     C****               PARM                    DEEOB
     C                   PARM                    DEEOBCD
     C**** End Chg *** 07/18/23 **************************************
     C                   PARM                    @@TEXT           70
     C                   PARM      *BLANKS       @@DSC1
     C                   PARM      *BLANKS       @@DSC2
     C                   PARM      *BLANKS       @@DSC3
     C                   PARM      *BLANKS       @@DSC4
     C*
     C                   MOVEL(P)  @@DSC1        DENOT1
     C                   MOVEL(P)  @@DSC2        DENOT2
     C                   MOVEL(P)  @@DSC3        DENOT3
     C                   MOVEL(P)  @@DSC4        DENOT4
     C                   Endif
     C*
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
     C                   END                                                     CLM STS CD
     C*                                                     NE "C"
     C                   END                                                     END *IN61
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  QCMDEXC TABLE
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
