     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N96D9R - TYPE D9 DISCOUNT CODE CALCULATIONS
     H*****************************************************************
     H* THIS PGM IS CALLED BY N9600R TO CALCULATE THE NETWORK DISCOUNT*
     H* AMT BASED ON THE PROVIDERS DISCOUNT CODE. THE DISCOUNT WILL   *
     H* ONLY BE CALCULATED FOR THE SINGLE SEQUENCE NUMBER PASSED TO   *
     H* THIS PROGRAM.                                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    N9600R                                                     *
     H*                                                               *
     H*  PROGRAMMER: C:62               PROJECT: 71MAL                *
     H*  CREATED: 07/27/00                                            *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  07/18/23 C:342 Project: NY060
     H*                 NY Compliance: CARC/RARC Update
     H*
     H*  10/03/06 C:62  Project: 71WHI
     H*                 RETURN TO BASE
     H*
     H*  12/19/03 C:79  PROJECT:71MEQ
     H*                 LEAVE ALL ADJ MESSAGES AND EOB CODE(Except 202)
     H*
     H*  09/27/01 C:62  PROJECT:MC246
     H*                 ASSIGN DESEQ#.
     H*
     H*  07/27/01 C:62  PROJECT: NW073
     H*                 ONLY DELETE IF CAMRA = 0 BUT NOT MESSAGE
     H*                 XX00003
     H*
     H*  07/09/01 C:62  Support: MC071 28105
     H*                 ADD PROCESS TO WRITE FDDEP RECORD.
     H*
     H*  05/10/00 C:62  Support: 71MAL
     H*                 CHANGE TO PBILLS FIELDS.
     H*
     H*  09/29/98 C:31  Support: 13964
     H*                 ALAMED network issue
     H*                 1. Removed parm from parm list
     H*
     H*  07/20/98 C:31  Project: 71MAJ
     H*                 1. V4 Impl of Alamed (71MAB)
     H*
     H*  12/19/95  C:41  REQ/PROJ: 71MAB                              *
     H*                  FIX PROCESS TO CALC CORRECT DISCOUNT AMOUNT. *
     H*                                                               *
     H*  10/05/95  C:32  REQ/PROJ: 2/71MAB                            *
     H*                  WHEN 'P' ENTERED AS MOD USE PROF COMP AMT    *
     H*                  INSTEAD OF MRA                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  60  - GENERAL CHAIN/READ                                     *
     H*  61  - GENERAL CHAIN/READ                                     *
     H*****************************************************************
     FNMBDLD    IF   E           K DISK
     FNMALL     IF   E           K DISK
     FFDCALN    UF   E           K DISK
     F*** BEGIN ADD *** 05/10/00 **************************************
     FFDDHL     IF   E           K DISK
     F*** END   ADD *** 05/10/00 **************************************
     FFTABL     IF   E           K DISK
     F*** BEGIN ADD *** 07/09/01 **************************************
     FFDDEL3    UF A E           K DISK
     F*** END   ADD *** 07/09/01 **************************************
     F*** BEGIN ADD *** 09/27/01 **************************************
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     F*** END   ADD *** 09/27/01 **************************************
     F*** BEGIN ADD *** 07/27/01 **************************************
     F*** BEG DEL *** 12/19/03 ****************************************
     F****** FDMGL6  UF  E           K        DISK
     F*** END DEL *** 12/19/03 ****************************************
     F*** END   ADD *** 07/27/01 **************************************
     FFAEOL4    IF   E           K DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D XXNOT1          C                   CONST('NETWORK DISCOUNT AP-
     D                                     PLIED')

     I*** END   ADD *** 07/09/01 **************************************

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
     C                   KFLD                    XXFDAT            7 0
     C                   KFLD                    XXDSEQ
     C*
     C*  BILLING CODE TABLE FILE
     C*
     C     BDKEY         KLIST
     C                   KFLD                    QQCODE            3
     C                   KFLD                    CAFDAT
     C*
     C*  ALAMED/PMD CODE FILE
     C*
     C     ALKEY         KLIST
     C                   KFLD                    CACPT4
     C                   KFLD                    CAFDAT
     C*** BEGIN ADD *** 07/09/01 **************************************
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
     C*  EOMB FILE
     C*
     C     EOKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C*** END   ADD *** 07/09/01 **************************************
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
     C*** BEGIN ADD *** 05/10/00 **************************************
     C*
     C     DHKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*** END   ADD *** 05/10/00 **************************************
     C*
     C*  CLAIM STATUS CODE TABLE FILE
     C*
     C     TCSKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    TABKEY           10
     C*** BEGIN ADD *** 07/27/01 **************************************
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
     C*** END   ADD *** 07/27/01 **************************************
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
     C                   Z-ADD     X1FDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C                   MOVE      QQDSEQ        XXDSEQ            5 0
     C*
     C*  GET THE CASE DETAIL RECORDS
     C*
     C     CAKEY         CHAIN     PCA01                              61
     C     *IN61         IFEQ      '0'
     C*** BEGIN ADD *** 07/27/01 **************************************
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
     C*** FOR EACH DETAIL RECORD READ, FIND THE DISCOUNT MASTER RECORD
     C*** THAT APPLIES TO THAT DATE OF SERVICE.
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
     C                   MOVE      *BLANKS       BDCOM2
     C                   END                                                    *IN60 ON
     C                   Z-ADD     *ZEROS        XXNDIS            9 2
     C*
     C*** GET SOURCE AND LESS THAN FLAGS
     C*
     C                   SUBST     BDCOM2:51     XXSRCE            1
     C                   SUBST     BDCOM2:52     XXLESS            1
     C*
     C     XXSRCE        IFEQ      'B'
     C                   EXSR      SRBILL
     C                   ELSE
     C     XXSRCE        IFEQ      'P'
     C                   EXSR      SRPMD
     C                   ELSE
     C     XXSRCE        IFEQ      'F'
     C                   EXSR      SRFEE
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** BEGIN ADD *** 07/09/01 **************************************
     C*
     C*** IF CANDIS HAS AN AMOUNT THEN WRITE FDDEP RECORD
     C*
     C     CANDIS        IFGT      *ZEROS
     C*** BEGIN DEL *** 07/27/01 **************************************
     C********** CAMRA     IFEQ *ZEROS
     C*
     C* Remove adjudication EOMB codes from Detail EOB/NOTES
     C*
     C********** EOKY      CHAINPEO01                99
     C********** DEKY      CHAINPDE01                99
     C********** *IN99     DOWEQ*OFF
     C********** DEESRC    IFEQ 'A'
     C********** DEDSEQ    ANDEQCADSEQ
     C**********           DELETPDE01
     C**********           ENDIF                           ESRC = 'A'
     C********** DEKY      READEPDE01                    99
     C**********           ENDDO                           *IN99 = Off
     C*
     C**********           ENDIF
     C*** END   DEL *** 07/27/01 **************************************
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
     C*** END   ADD *** 07/09/01 **************************************
     C*
     C                   UPDATE    PCA01
     C*
     C                   END                                                     CLM STS CD
     C*                                                     NE "C"
     C                   END                                                     END *IN61
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C* SRBILL - CALC WHEN SOURCE IS 'B' - FOR BILLED CHARGES
     C*****************************************************************
     C     SRBILL        BEGSR
     C                   Z-ADD     *ZEROS        XXPLUS
     C                   Z-ADD     *ZEROS        XXMINS
     C                   Z-ADD     *ZEROS        XXMRA
     C*
     C     BDPC02        IFGT      *ZEROS
     C*** BEGIN CHG *** 05/10/00 **************************************
     C********** CATOTC    MULT BDPC02    XXMINS  93
     C     CACHRG        MULT      BDPC02        XXMINS            9 3
     C*** END   CHG *** 05/10/00 **************************************
     C                   ENDIF
     C*** BEGIN CHG *** 05/10/00 **************************************
     C********** CATOTC    SUB  XXMINS    XXMRA   92H
     C     CACHRG        SUB(H)    XXMINS        XXMRA             9 2
     C*** END   CHG *** 05/10/00 **************************************
     C*
     C*** BEGIN CHG *** 05/10/00 **************************************
     C********** CATOTC    SUB  XXMRA     CANDIS
     C**********           Z-ADDXXMRA     CATOTP
     C********** CATOTP    SUB  CAOTHR    CATOTP
     C**********           Z-ADDCATOTP    CATOTA
     C**********           Z-ADD*ZEROS    CATOTD
     C     CACHRG        SUB       XXMRA         CANDIS
     C                   Z-ADD     XXMRA         CAPAID
     C     CAPAID        SUB       CAOTHR        CAPAID
     C                   SUB       CAOTH2        CAPAID
     C                   Z-ADD     *ZEROS        CAMRA
     C*** BEGIN ADD *** 07/27/01 **************************************
     C                   EXSR      REMMSG
     C*** END   ADD *** 07/27/01 **************************************
     C*** END   CHG *** 05/10/00 **************************************
     C*
     C                   ENDSR
     C*****************************************************************
     C* SRFEE  - CALC WHEN SOURCE IS 'F' - FOR FEE SCHEDULE
     C*****************************************************************
     C     SRFEE         BEGSR
     C*
     C* GET FEE SCHEDULE AMOUNT
     C*
     C********** CATOTC    SUB  CATOTD    XXMRAF  92       DEL 12/19/95
     C                   Z-ADD     CABRTC        XXMRAF            9 2          ADD 12/19/95
     C*
     C     BDPC01        IFGT      *ZEROS
     C     XXMRAF        MULT      BDPC01        XXPLUS            9 3
     C                   ENDIF
     C*
     C     BDPC02        IFGT      *ZEROS
     C     XXMRAF        MULT      BDPC02        XXMINS            9 3
     C                   MULT      -1            XXMINS
     C                   ENDIF
     C*
     C     XXMRAF        ADD(H)    XXPLUS        XXMRA             9 2
     C     XXMRA         ADD(H)    XXMINS        XXMRA             9 2
     C*
     C***
     C*** IF XXLESS EQUAL 'Y' USE THE LESSER AMOUNT
     C*** BILLED CHARGES VS. CALCULATED XXMRA FROM FEE SCHED
     C***
     C     XXLESS        IFEQ      'Y'
     C*** BEGIN CHG *** 05/10/00 **************************************
     C********** XXMRA     IFLT CATOTC
     C**********           Z-ADD*ZEROS    CATOTD
     C********** CATOTC    SUB  XXMRA     XXNDIS
     C********** XXMRA     SUB  CAOTHR    XXMRA
     C**********           Z-ADDXXNDIS    CANDIS
     C**********           Z-ADDXXMRA     CATOTP
     C**********           Z-ADDXXMRA     CATOTA
     C     XXMRA         IFLT      CACHRG
     C                   Z-ADD     *ZEROS        CAMRA
     C*** BEGIN ADD *** 07/27/01 **************************************
     C                   EXSR      REMMSG
     C*** END   ADD *** 07/27/01 **************************************
     C     CACHRG        SUB       XXMRA         XXNDIS
     C     XXMRA         SUB       CAOTHR        XXMRA
     C                   SUB       CAOTH2        XXMRA
     C                   Z-ADD     XXNDIS        CANDIS
     C                   Z-ADD     XXMRA         CAPAID
     C*** END   CHG *** 05/10/00 **************************************
     C*
     C                   ELSE
     C*** BEGIN CHG *** 05/10/00 **************************************
     C**********           Z-ADDCATOTC    XXMRA
     C**********           Z-ADD*ZEROS    CATOTD
     C********** CATOTC    SUB  XXMRA     XXNDIS
     C********** XXMRA     SUB  CAOTHR    XXMRA
     C**********           Z-ADDXXNDIS    CANDIS
     C**********           Z-ADDXXMRA     CATOTP
     C**********           Z-ADDXXMRA     CATOTA
     C                   Z-ADD     CACHRG        XXMRA
     C                   Z-ADD     *ZEROS        CAMRA
     C*** BEGIN ADD *** 07/27/01 **************************************
     C                   EXSR      REMMSG
     C*** END   ADD *** 07/27/01 **************************************
     C     CAPAID        SUB       XXMRA         XXNDIS
     C     XXMRA         SUB       CAOTHR        XXMRA
     C                   SUB       CAOTH2        XXMRA
     C                   Z-ADD     XXNDIS        CANDIS
     C                   Z-ADD     XXMRA         CAPAID
     C*** END   CHG *** 05/10/00 **************************************
     C                   END
     C                   END
     C*
     C*** IF XXLESS NE 'Y', USE THE CALCULATED XXMRA FROM FEE SCHED
     C*** IF XXMRA IS GT BILLED AMOUNT, LOAD THE DIFFERENCE INTO
     C*** CANDIS AS A NEGATIVE NUMBER
     C***
     C*
     C     XXLESS        IFNE      'Y'
     C*** BEGIN CHG *** 05/10/00 **************************************
     C********** XXMRA     IFGT CATOTC
     C**********           Z-ADD*ZEROS    CATOTD
     C********** XXMRA     SUB  CAOTHR    XXMRA
     C**********           Z-ADDXXMRA     CATOTP
     C**********           Z-ADDXXMRA     CATOTA
     C********** XXMRA     SUB  CATOTC    CANDIS
     C**********           MULT -1        CANDIS
     C     XXMRA         IFGT      CACHRG
     C                   Z-ADD     *ZEROS        CAMRA
     C*** BEGIN ADD *** 07/27/01 **************************************
     C                   EXSR      REMMSG
     C*** END   ADD *** 07/27/01 **************************************
     C     XXMRA         SUB       CAOTHR        XXMRA
     C                   SUB       CAOTH2        XXMRA
     C                   Z-ADD     XXMRA         CAPAID
     C     XXMRA         SUB       CACHRG        CANDIS
     C                   MULT      -1            CANDIS
     C*** END   CHG *** 05/10/00 **************************************
     C*
     C                   ELSE
     C*** BEGIN CHG *** 05/10/00 **************************************
     C**********           Z-ADD*ZEROS    CATOTD
     C********** CATOTC    SUB  XXMRA     XXNDIS
     C********** XXMRA     SUB  CAOTHR    XXMRA
     C**********           Z-ADDXXNDIS    CANDIS
     C**********           Z-ADDXXMRA     CATOTP
     C**********           Z-ADDXXMRA     CATOTA
     C                   Z-ADD     *ZEROS        CAMRA
     C*** BEGIN ADD *** 07/27/01 **************************************
     C                   EXSR      REMMSG
     C*** END   ADD *** 07/27/01 **************************************
     C     CACHRG        SUB       XXMRA         XXNDIS
     C     XXMRA         SUB       CAOTHR        XXMRA
     C                   SUB       CAOTH2        XXMRA
     C                   Z-ADD     XXNDIS        CANDIS
     C                   Z-ADD     XXMRA         CAPAID
     C*** END   CHG *** 05/10/00 **************************************
     C                   END
     C                   END
     C                   ENDSR
     C*****************************************************************
     C* SRPMD  - CALC WHEN SOURCE IS 'P' - FOR PMD FILE
     C*****************************************************************
     C     SRPMD         BEGSR
     C*
     C     ALKEY         SETGT     PAL01
     C     NXTAL         TAG
     C                   READP     PAL01                                  60
     C*
     C*** VERIFY CPT4 CODE AND EFFECTIVE DATE
     C*
     C     *IN60         IFEQ      '0'
     C     ALCPT4        IFNE      CACPT4
     C     ALEFDT        ORGT      CAFDAT
     C     ALSTAT        OREQ      'I'
     C*
     C                   GOTO      NXTAL
     C                   END                                                    ALCPT4/ALEFDT
     C                   END                                                    *IN60 OFF
     C*
     C*** IF A VALID PMD RECORD IS NOT FOUND, ZERO THE ALMRA AMOUNT
     C*** SO THE NETWORK DISCOUNT WILL BE ZERO.
     C*
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZEROS        ALMRA
     C                   Z-ADD     *ZEROS        ALPROF                         ADD 10/05/95
     C                   MOVE      'N'           XXLESS
     C                   END                                                    *IN60 ON
     C*** BEGIN ADD *** 10/05/95 **************************************
     C***
     C*** IF THE PROCEDURE MODIFIER IS A 'P', USE PROFESSIONAL
     C*** COMPONENT AMOUNT INSTEAD OF THE MRA
     C***
     C*** BEGIN CHG *** 05/10/00 **************************************
     C********** CAPMOD    IFEQ 'P'
     C**********           Z-ADDALPROF    ALMRA
     C**********           END
     C*
     C     DHKY          CHAIN     PDH01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     DHMOD1        IFEQ      'P'
     C     DHMOD2        OREQ      'P'
     C     DHMOD3        OREQ      'P'
     C     DHMOD4        OREQ      'P'
     C                   Z-ADD     ALPROF        ALMRA
     C                   ENDIF
     C                   ENDIF
     C*** END   CHG *** 05/10/00 **************************************
     C*** END   ADD *** 10/05/95 **************************************
     C*
     C*** GET THE PERCENTAGE AMOUNTS
     C*
     C     BDPC01        IFGT      *ZEROS
     C     ALMRA         MULT      BDPC01        XXPLUS            9 3
     C                   ENDIF
     C*
     C     BDPC02        IFGT      *ZEROS
     C     ALMRA         MULT      BDPC02        XXMINS            9 3
     C                   MULT      -1            XXMINS
     C                   ENDIF
     C*
     C     ALMRA         ADD(H)    XXPLUS        XXMRA             9 2
     C     XXMRA         ADD(H)    XXMINS        XXMRA             9 2
     C***
     C*** IF XXLESS EQUAL 'Y' USE THE LESSER AMOUNT
     C*** BILLED CHARGES VS. CALCULATED XXMRA FROM PDM FILE
     C***
     C     XXLESS        IFEQ      'Y'
     C*** BEGIN CHG *** 05/10/00 **************************************
     C********** XXMRA     IFLT CATOTC
     C**********           Z-ADD*ZEROS    CATOTD
     C********** CATOTC    SUB  XXMRA     XXNDIS
     C********** XXMRA     SUB  CAOTHR    XXMRA
     C**********           Z-ADDXXNDIS    CANDIS
     C**********           Z-ADDXXMRA     CATOTP
     C**********           Z-ADDXXMRA     CATOTA
     C     XXMRA         IFLT      CACHRG
     C                   Z-ADD     *ZEROS        CAMRA
     C*** BEGIN ADD *** 07/27/01 **************************************
     C                   EXSR      REMMSG
     C*** END   ADD *** 07/27/01 **************************************
     C     CACHRG        SUB       XXMRA         XXNDIS
     C     XXMRA         SUB       CAOTHR        XXMRA
     C                   SUB       CAOTH2        XXMRA
     C                   Z-ADD     XXNDIS        CANDIS
     C                   Z-ADD     XXMRA         CAPAID
     C*** END   CHG *** 05/10/00 **************************************
     C*
     C                   ELSE
     C*** BEGIN CHG *** 05/10/00 **************************************
     C**********           Z-ADDCATOTC    XXMRA
     C**********           Z-ADD*ZEROS    CATOTD
     C********** CATOTC    SUB  XXMRA     XXNDIS
     C********** XXMRA     SUB  CAOTHR    XXMRA
     C**********           Z-ADDXXNDIS    CANDIS
     C**********           Z-ADDXXMRA     CATOTP
     C**********           Z-ADDXXMRA     CATOTA
     C                   Z-ADD     CACHRG        XXMRA
     C                   Z-ADD     *ZEROS        CAMRA
     C*** BEGIN ADD *** 07/27/01 **************************************
     C                   EXSR      REMMSG
     C*** END   ADD *** 07/27/01 **************************************
     C     CACHRG        SUB       XXMRA         XXNDIS
     C     XXMRA         SUB       CAOTHR        XXMRA
     C                   SUB       CAOTH2        XXMRA
     C                   Z-ADD     XXNDIS        CANDIS
     C                   Z-ADD     XXMRA         CAPAID
     C*** END   CHG *** 05/10/00 **************************************
     C                   END
     C                   END
     C*
     C*** IF XXLESS NE 'Y', USE THE CALCULATED XXMRA FROM PDM FILE
     C*** IF XXMRA IS GT BILLED AMOUNT, LOAD THE DIFFERENCE INTO
     C*** CANDIS AS A NEGATIVE NUMBER
     C***
     C     XXLESS        IFNE      'Y'
     C*** BEGIN CHG *** 05/10/00 **************************************
     C********** XXMRA     IFGT CATOTC
     C**********           Z-ADD*ZEROS    CATOTD
     C********** XXMRA     SUB  CAOTHR    XXMRA
     C**********           Z-ADDXXMRA     CATOTP
     C**********           Z-ADDXXMRA     CATOTA
     C********** XXMRA     SUB  CATOTC    CANDIS
     C**********           MULT -1        CANDIS
     C     XXMRA         IFGT      CACHRG
     C                   Z-ADD     *ZEROS        CAMRA
     C*** BEGIN ADD *** 07/27/01 **************************************
     C                   EXSR      REMMSG
     C*** END   ADD *** 07/27/01 **************************************
     C     XXMRA         SUB       CAOTHR        XXMRA
     C                   SUB       CAOTH2        XXMRA
     C                   Z-ADD     XXMRA         CAPAID
     C     XXMRA         SUB       CACHRG        CANDIS
     C                   MULT      -1            CANDIS
     C*** END   CHG *** 05/10/00 **************************************
     C*
     C                   ELSE
     C*** BEGIN CHG *** 05/10/00 **************************************
     C**********           Z-ADD*ZEROS    CATOTD
     C********** CATOTC    SUB  XXMRA     XXNDIS
     C********** XXMRA     SUB  CAOTHR    XXMRA
     C**********           Z-ADDXXNDIS    CANDIS
     C**********           Z-ADDXXMRA     CATOTP
     C**********           Z-ADDXXMRA     CATOTA
     C                   Z-ADD     *ZEROS        CAMRA
     C*** BEGIN ADD *** 07/27/01 **************************************
     C                   EXSR      REMMSG
     C*** END   ADD *** 07/27/01 **************************************
     C     CACHRG        SUB       XXMRA         XXNDIS
     C     XXMRA         SUB       CAOTHR        XXMRA
     C                   SUB       CAOTH2        XXMRA
     C                   Z-ADD     XXNDIS        CANDIS
     C                   Z-ADD     XXMRA         CAPAID
     C*** END   CHG *** 05/10/00 **************************************
     C                   END
     C                   END
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 07/27/01 **************************************
     *********************************************************************
     *** Remove  - Remove adjudication messages/EOMB Codes  R E M M S GN T
     *********************************************************************
     C     REMMSG        BEGSR
     C*** BEG DEL *** 12/19/04 ****************************************
     C*********
     C**Remove*adjudication messages from Prov bills errors
     C*********
     C*********  MGKY      CHAINPMG01                99
     C*********  *IN99     DOWEQ*OFF
     C*********  MGSRC     IFEQ 'S'
     C*********            MOVE MGMSG#    XXMSG#  5
     C*********  XXMSG#    IFNE '00003'
     C*********            DELETPMG01
     C*********            ENDIF
     C*********            ENDIF                           SRC = 'S'
     C*********  MGKY      READEPMG01                    99
     C*********            ENDDO                           *IN99 = Off
     C*** END DEL *** 12/19/04 ****************************************
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
