     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N96DOR - TYPE DO DISCOUNT CODE CALCULATIONS
     H*****************************************************************
     H* THIS PGM IS CALLED BY N9600R TO CALCULATE THE NETWORK DISCOUNT*
     H* AMT BASED ON THE PROVIDERS DISCOUNT CODE. THE DISCOUNT WILL   *
     H* ONLY BE CALCULATED FOR THE SINGLE SEQUENCE NUMBER PASSED TO   *
     H* THIS PROGRAM.                                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    N9600R                                                     *
     H*                                                               *
     H*  PROGRAMMER: C:212              PROJECT: 114MRJ               *
     H*  CREATED: 10/08/08                                            *
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                              **
     H*  07/18/23 C:342 Project: NY060                                *
     H*                 NY Compliance: CARC/RARC Update               *
     H*                                                              **
     H*****************************************************************
     FNMBDL1    IF   E           K DISK
     FFDCALF    UF   E           K DISK
     FFDCAL18   IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     FFTABL     IF   E           K DISK
     FFAEOLG    IF   E           K DISK
     FFDDEL3    UF A E           K DISK
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDMGL6    UF   E           K DISK
     FFDDHL     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D XXNOT1          C                   CONST('DISCOUNT OFF CHARGE-
     D                                      HAS BEEN APPLIED')

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@FEIN            9
     C                   PARM                    @@MOD             3
     C                   PARM                    @@FDAT            8
     C                   PARM                    @@DSEQ            5
     C                   PARM                    @@CODE
     C                   PARM                    @@CEDT            8
     C                   PARM                    @@SYS             1
     C                   PARM                    @@FL1             1
     C                   PARM                    @@FL2             1
     C                   PARM                    @@FL3             1
     C                   PARM                    @@FL4             1
     C                   PARM                    @@FL5             1
     C                   PARM                    @@DS1            10
     C                   PARM                    @@DS2            10
     C                   PARM                    @@DS3            10
     C                   PARM                    @@DS4            10
     C                   PARM                    @@DS5            10
     C                   PARM                    @@RTCD            1
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
     C     PLIST1        PLIST
     C                   PARM                    CACO#
     C                   PARM                    CASUB#
     C                   PARM                    CAFND
     C                   PARM                    CAFYR
     C                   PARM                    CACASE
     C                   PARM                    CAEOB#
     C                   PARM                    CADSEQ
     C                   PARM                    XXPRCD            5
     C                   PARM                    CAFDAT
     C                   PARM                    ##ZIPA            5 0
     C                   PARM                    ##MOD1            2
     C                   PARM                    ##MOD2            2
     C                   PARM                    ##MOD3            2
     C                   PARM                    ##MOD4            2
     C                   PARM                    XXALLW            9 2
     C*
     C* BILLING CODE TABLE FILE
     C*
     C     BDKY1         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKEFDT
     C*
     C     BDKY2         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKTYPE
     C*
     C* CASE DETAIL RECORDS
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKDSEQ
     C*
     C     CAKY1         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    XXPRCD
     C*
     C* EOMB TRANSACTION FILE
     C*
     C     EOKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C*
     C*  CLAIM STATUS CODE TABLE FILE
     C*
     C     CSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    TABKEY           10
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
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    CACPT4        ##PRCD
     C     *LIKE         DEFINE    BDCODE        @@CODE
     C     *LIKE         DEFINE    BDCODE        KKCODE
     C     *LIKE         DEFINE    BDTYPE        KKTYPE
     C     *LIKE         DEFINE    BDEFDT        KKEFDT
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@SUB#        KKSUB#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
     C                   MOVE      @@DSEQ        KKDSEQ
     C                   MOVEL(P)  @@CODE        KKCODE
     C                   MOVEL     'DO'          KKTYPE
     C                   Z-ADD     *HIVAL        KKEFDT
     C*
     C*  GET THE CASE DETAIL RECORDS
     C*
     C     CAKY          CHAIN     PCA01                              99
     C     *IN99         IFEQ      *OFF
     C     EOKY          CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C* BE SURE CLAIM STATUS CODE IS NOT "C" FOR CAP ON EC/MD
     C*
     C                   MOVEL(P)  CACSCD        TABKEY
     C     CSKY          CHAIN     PCS01                              99
     C     *IN99         IFEQ      '0'
     C     CSDISC        ANDNE     'C'
     C*
     C* GET THE DISCOUNT RECORD BASED UPON THE OVERALL BILLS FROM DATE
     C*
     C     BDKY1         SETGT     PBD01
     C     BDKY2         READPE    PBD01                                  99
     C     *IN99         DOWEQ     *OFF
     C     BDSTAT        ANDEQ     'I'
     C     BDKY2         READPE    PBD01                                  99
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C* IF WE DO GET A GOOD HIT IN DISCOUNT CODE FILE, LET'S DO
     C* SOME DISCOUNTING.
     C*
     C** Populate/Initialize reqired fields
     C                   MOVE      CACPT4        ##PRCD
     C                   MOVE      'N'           FLAG1             1
     C*
     C     CAAST         IFEQ      'PA'
     C     EOBTYP        ANDEQ     'N'
     C*
     C     CAPAID        IFNE      0
     C     BDSKPD        ORNE      'Y'
     C*
     C     BDLMRA        IFNE      'Y'
     C                   Z-ADD     *ZEROS        CAMRA
     C                   ENDIF                                                  BDLMRA IFNE 'Y'
     C*
     C     ##PRCD        IFEQ      '99204'
     C     ##PRCD        OREQ      '99214'
     C     ##PRCD        OREQ      '99205'
     C     ##PRCD        OREQ      '99215'
     C*
     C                   MOVE      'Y'           FLAG1
     C** Determine the allowance to be paid
     C                   EXSR      CALALW
     C                   Z-ADD     *ZEROS        XXTMP1           10 2
     C     AVGALW        MULT      BDPC01        XXTMP1
     C     AVGALW        SUB       XXTMP1        CAPAID
     C     CACHRG        SUB       CAMRA         CANDIS
     C                   SUB       CAOTHR        CANDIS
     C                   SUB       CAOTH2        CANDIS
     C                   SUB       CAPAID        CANDIS
     C*
     C                   ENDIF                                                  CD=99204/99214
     C*
     C     ##PRCD        IFEQ      '99203'
     C     ##PRCD        OREQ      '99213'
     C*
     C                   MOVE      'Y'           FLAG1
     C** Determine if the corresponding pair-code (99204/99214) or   ***
     C** (99205/99215) is also billed (FLAG2 = Y in that case)       ***
     C                   EXSR      CKPAIR
     C     FLAG2         IFEQ      'Y'
     C                   Z-ADD     *ZEROS        CAPAID
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXTMP1           10 2
     C     CAPAID        MULT      BDPC01        XXTMP1
     C     CAPAID        SUB       XXTMP1        CAPAID
     C                   ENDIF
     C*
     C     CACHRG        SUB       CAMRA         CANDIS
     C                   SUB       CAOTHR        CANDIS
     C                   SUB       CAOTH2        CANDIS
     C                   SUB       CAPAID        CANDIS
     C*
     C                   ENDIF                                                  CD=99203/99213
     C*
     C** FOR REST OF THE CODES
     C*
     C     FLAG1         IFEQ      'N'
     C** FLAG1 WILL BE 'N' IF ##PRCD IS NOT AMONGST THE CODES BEING
     C** CHEKED ABOVE AND IS A CODE DIFFERENT THAN THOSE.
     C*
     C                   Z-ADD     *ZEROS        XXTMP1           10 2
     C     CAPAID        MULT      BDPC01        XXTMP1
     C     CAPAID        SUB       XXTMP1        CAPAID
     C*
     C     CACHRG        SUB       CAMRA         CANDIS
     C                   SUB       CAOTHR        CANDIS
     C                   SUB       CAOTH2        CANDIS
     C                   SUB       CAPAID        CANDIS
     C                   ENDIF                                                  FLAG1 IFEQ 'N'
     C*
     C                   ELSE                                                   CAPAID=0&BDSKPD
     C** Discount is not applied
     C                   ENDIF                                                  CAPAID=0&BDSKPD
     C*
     C                   ENDIF                                                  CAAST=PA&EOB=N
     C*
     C     BDSKPD        IFEQ      'Y'
     C     CAPAID        ANDNE     *ZEROS
     C     BDSKPD        OREQ      *BLANKS
     C*
     C                   EXSR      REMMSG
     C*
     C                   ENDIF                                                  BDSKPD=Y&CAPAID!=0
     C*
     C* IF CANDIS HAS AN AMOUNT THEN MOVE NEW NETWORK CODE INTO CAEOMB
     C*
     C     CAMRA         IFNE      *ZEROS
     C     CAOTHR        ORNE      *ZEROS
     C     CAOTH2        ORNE      *ZEROS
     C     CANDIS        ORNE      *ZEROS
     C*
     C* GET NEXT DETAIL SEQUENCE NUMBER
     C*
     C                   Z-ADD     *HIVAL        KKSEQ#
     C     DEKY1         SETGT     PDE02
     C     DEKY2         READPE    PDE02                                  99
     C*
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
     C****               MOVE      '396'         DEEOB
     C                   MOVEL     '396'         DEEOBCD
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
     C*
     C                   ENDIF                                                   CANDIS<>0
     C*
     C                   ENDIF                                                  *IN99=*OFF /PBD
     C*
     C                   ENDIF                                                   CSDISC='C'
     C*
     C                   ENDIF                                                  *IN99=*OFF /PEO
     C*
     C                   UPDATE    PCA01
     C*
     C                   ENDIF                                                  *IN99=*OFF /PCA
     C*
     C                   SETON                                        LR
     C*
     C********************************************************************
     C** Remove  - Remove adjudication messages/EOMB Codes  R E M M S G
     C********************************************************************
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
     C********************************************************************
     C** CALALW  - Calculates the allowance that should be  paid in case
     C**           of some special codes.
     C********************************************************************
     C     CALALW        BEGSR
     C*
     C* If the code is 99204/99205 the allowance should be the location
     C* zip total of 99203 & 99204 divided by 2 i.e their average.
     C* Similarly if code is 99214/99215 it is the average of 99213 &99214
     C* Call FA760R to determine the allowance.
     C*
     C** Populate the required parameters
     C*
     C                   MOVEL     EOFLIC        ##ZIPA
     C     DEKY2         CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      DHMOD1        ##MOD1
     C                   MOVE      DHMOD2        ##MOD2
     C                   MOVE      DHMOD3        ##MOD3
     C                   MOVE      DHMOD4        ##MOD4
     C                   ENDIF
     C** Call for first code of the pair
     C     ##PRCD        IFEQ      '99204'
     C     ##PRCD        OREQ      '99205'
     C                   MOVE      '99203'       XXPRCD
     C                   ELSE
     C                   MOVE      '99213'       XXPRCD
     C                   ENDIF
     C*
     C                   CALL      'FA760R'      PLIST1
     C*
     C** Store the allowance calculated
     C                   Z-ADD     XXALLW        TOTALW           10 2
     C*
     C** Call for second code of the pair
     C     ##PRCD        IFEQ      '99204'
     C     ##PRCD        OREQ      '99205'
     C                   MOVE      '99204'       XXPRCD
     C                   ELSE
     C                   MOVE      '99214'       XXPRCD
     C                   ENDIF
     C*
     C                   CALL      'FA760R'      PLIST1
     C*
     C** Calculate total allowance
     C     XXALLW        ADD       TOTALW        TOTALW
     C** Calculate avarage allowance
     C     TOTALW        DIV       2             AVGALW           10 2
     C*
     C                   ENDSR
     C********************************************************************
     C** CKPAIR  - Checks if certain pair of codes exists in the bill.
     C********************************************************************
     C     CKPAIR        BEGSR
     C*
     C** Initialize Flag2
     C                   MOVE      'N'           FLAG2             1
     C*
     C** Determine the code pair to be checked
     C*
     C** Check with first pair
     C     ##PRCD        IFEQ      '99203'
     C                   MOVE      '99204'       XXPRCD
     C                   ELSE
     C                   MOVE      '99214'       XXPRCD
     C                   ENDIF
     C*
     C     CAKY1         SETLL     PCA02                                  98
     C     *IN98         IFEQ      '1'
     C                   MOVE      'Y'           FLAG2
     C                   ENDIF
     C*
     C** If First pair doesn't exist check with next pair if that exists
     C     FLAG2         IFEQ      'N'
     C     ##PRCD        IFEQ      '99203'
     C                   MOVE      '99205'       XXPRCD
     C                   ELSE
     C                   MOVE      '99215'       XXPRCD
     C                   ENDIF
     C*
     C     CAKY1         SETLL     PCA02                                  98
     C     *IN98         IFEQ      '1'
     C                   MOVE      'Y'           FLAG2
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C********************************************************************
