     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N96DJR - TYPE DJ DISCOUNT CODE CALCULATIONS
     H*****************************************************************
     H* THIS PGM IS CALLED BY N9600R TO CALCULATE THE NETWORK DISCOUNT*
     H* AMT BASED ON THE PROVIDERS DISCOUNT CODE. THE DISCOUNT WILL   *
     H* ONLY BE CALCULATED FOR THE SINGLE SEQUENCE NUMBER PASSED TO   *
     H* THIS PROGRAM.                                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    N9600R                                                     *
     H*                                                               *
     H*  PROGRAMMER: C:62               PROJECT: NW074                *
     H*  CREATED: 07/10/02                                            *
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                              **
     H*  07/18/23 C:342 Project: NY060                               **
     H*                 NY Compliance: CARC/RARC Update              **
     H*                                                              **
     H*  10/20/03 C:62 PROJECT NW077                                 **
     H*                ADD SKIP DENIED LINE FLAG LOGIC               **
     H*                                                              **
     H*  10/24/02 C:62 PROJECT NW074                                 **
     H*                ADD LEAVE MRA FLAG LOGIC.                     **
     H*                                                               *
     H*****************************************************************
     FNMBDL1    IF   E           K DISK
     FFDCALF    UF   E           K DISK
     FFTABL     IF   E           K DISK
     FFAEOLG    IF   E           K DISK
     FFDDEL3    UF A E           K DISK
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDMGL6    UF   E           K DISK

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
     C                   MOVEL     'DJ'          KKTYPE
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
     C* IF WE DO GET A GOOD HIT IN DISCOUNT CODE FILE, LET'S DO
     C* SOME DISCOUNTING.
     C*
     C     *IN99         IFEQ      *OFF
     C*** BEGIN ADD *** 10/20/03 **************************************
     C     BDSKPD        IFEQ      'Y'
     C     CAPAID        ANDNE     *ZEROS
     C     BDSKPD        OREQ      *BLANKS
     C*** END   ADD *** 10/20/03 **************************************
     C*
     C                   EXSR      REMMSG
     C*** BEGIN DEL *** 10/24/02 **************************************
     C**********           Z-ADD*ZEROS    CAMRA
     C*** END   DEL *** 10/24/02 **************************************
     C                   Z-ADD     *ZEROS        CAOTHR
     C                   Z-ADD     *ZEROS        CAOTH2
     C                   Z-ADD     *ZEROS        CANDIS
     C                   MOVEL     *BLANKS       CAOT1C
     C                   MOVEL     *BLANKS       CAOT2C
     C*
     C*** BEGIN ADD *** 10/24/02 **************************************
     C     BDLMRA        IFNE      'Y'
     C*
     C                   Z-ADD     *ZEROS        CAMRA
     C*** END   ADD *** 10/24/02 **************************************
     C                   SELECT
     C     BDMRAF        WHENEQ    'Y'
     C     CACHRG        MULT      BDPC01        CAMRA
     C     CACHRG        SUB       CAMRA         CAPAID
     C     BDOT1F        WHENEQ    'Y'
     C     CACHRG        MULT      BDPC01        CAOTHR
     C     CACHRG        SUB       CAOTHR        CAPAID
     C                   MOVEL(P)  BDOT1C        CAOT1C
     C     BDOT2F        WHENEQ    'Y'
     C     CACHRG        MULT      BDPC01        CAOTH2
     C     CACHRG        SUB       CAOTH2        CAPAID
     C                   MOVEL(P)  BDOT2C        CAOT2C
     C     BDNETF        WHENEQ    'Y'
     C     CACHRG        MULT      BDPC01        CANDIS
     C     CACHRG        SUB       CANDIS        CAPAID
     C                   ENDSL
     C*** BEGIN ADD *** 10/24/02 **************************************
     C                   ELSE
     C     CACHRG        MULT      BDPC01        CAPAID
     C                   SELECT
     C     BDMRAF        WHENEQ    'Y'
     C     CACHRG        SUB       CAPAID        CAMRA
     C     BDOT1F        WHENEQ    'Y'
     C     CACHRG        SUB       CAPAID        CAOTHR
     C                   SUB       CAMRA         CAOTHR
     C     CAOTHR        IFNE      *ZEROS
     C                   MOVEL(P)  BDOT1C        CAOT1C
     C                   ENDIF
     C     BDOT2F        WHENEQ    'Y'
     C     CACHRG        SUB       CAPAID        CAOTH2
     C                   SUB       CAMRA         CAOTH2
     C     CAOTH2        IFNE      *ZEROS
     C                   MOVEL(P)  BDOT2C        CAOT2C
     C                   ENDIF
     C     BDNETF        WHENEQ    'Y'
     C     CACHRG        SUB       CAPAID        CANDIS
     C                   SUB       CAMRA         CANDIS
     C                   ENDSL
     C                   ENDIF
     C*** END   ADD *** 10/24/02 **************************************
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
     C                   ENDIF                                                   CANDIS<>0
     C*** BEGIN ADD *** 10/20/03 **************************************
     C                   ENDIF                                                   BDSKPD='Y'
     C*** END   ADD *** 10/20/03 **************************************
     C                   ENDIF                                                   *IN99=*OFF
     C                   ENDIF                                                   CSDISC='C'
     C                   ENDIF                                                   *IN99=*OFF
     C                   UPDATE    PCA01
     C                   ENDIF                                                   *IN99=*OFF
     C*
     C                   SETON                                        LR
     C********************************************************************
     C** Remove  - Remove adjudication messages/EOMB Codes  R E M M S GN T
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
