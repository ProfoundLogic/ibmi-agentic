     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N96DKR - TYPE DK DISCOUNT CODE CALCULATIONS
     H*****************************************************************
     H* THIS PGM IS CALLED BY N9600R TO CALCULATE THE NETWORK DISCOUNT*
     H* AMT BASED ON THE PROVIDERS DISCOUNT CODE. THE DISCOUNT WILL   *
     H* ONLY BE CALCULATED FOR THE SINGLE SEQUENCE NUMBER PASSED TO   *
     H* THIS PROGRAM.                                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    N9600R                                                     *
     H*                                                               *
     H*****************************************************************
     H*  Created on 03/21/2005 by c:62 for Project 114MJD             *
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  07/18/23 C:342 Project: NY060
     H*                 NY Compliance: CARC/RARC Update
     H*
     H*  02/25/09 C:227 SUPPORT: 67248
     H*                 WSH Discount Code
     H*
     H*****************************************************************
     FNMBDLD    IF   E           K DISK
     FFDCALN    UF   E           K DISK
     FNDCOL3    IF   E           K DISK
     FFTABL     IF   E           K DISK
     FFAEOLG    IF   E           K DISK
     FFDDEL3    UF A E           K DISK
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDDUL     IF   E           K DISK
     FFDMGL6    UF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D XXNOT1          C                   CONST('NETWORK DISCOUNT AP-
     D                                     PLIED')

     C*
     C*   KEY LIST FOR THE CARVE-OUT TRANSACTION FILE
     C*
     C     COKY1         KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    BDEFDT
     C                   KFLD                    KK10A1
     C*
     C     COKY2         KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    BDEFDT
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
     C*  EOMB HOSPITAL DETAIL FILE
     C*
     C     DUKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*
     C*  BILLING CODE TABLE FILE
     C*
     C     BDKY1         KLIST
     C                   KFLD                    QQCODE            3
     C                   KFLD                    CAFDAT
     C*
     C     BDKY2         KLIST
     C                   KFLD                    QQCODE
     C*
     C*  CLAIM STATUS CODE TABLE FILE
     C*
     C     TCSKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    TABKEY           10
     C*
     C* INITIALIZE SECTION
     C*
     C     *LIKE         DEFINE    BDAM01        XXAM01
     C     *LIKE         DEFINE    BDAM02        XXAM02
     C     *LIKE         DEFINE    BDDT          XXFDAT
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C     *LIKE         DEFINE    CO10A1        KK10A1
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
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
     C*
     C                   Z-ADD     X1FDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C*
     C                   MOVE      QQDSEQ        XXDSEQ
     C*
     C* Get the case detail records
     C*
     C     CAKEY         CHAIN     PCA01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     EOKY          CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C     DUKY          CHAIN     PDU01                              99
     C                   ENDIF
     C*
     C* Be sure claim status code is not "C" for cap on EC/MD
     C*
     C                   MOVE      *BLANKS       TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              99
     C     *IN99         IFEQ      *OFF
     C     CSDISC        ANDNE     'C'
     C*
     C* For each detail record read, find the discount master record
     C* that applys to that date of service.
     C*
     C     BDKY1         SETGT     PBD01
     C     BDKY2         READPE    PBD01                                  90
     C     *IN90         DOWEQ     *OFF
     C     BDSTAT        IFNE      'I'
     C                   LEAVE
     C                   ENDIF                                                  BDCODE/BDSTAT
     C     BDKY2         READPE    PBD01                                  90
     C                   ENDDO                                                  *IN99 OFF
     C*
     C* If we do get a good hit in discount code file
     C*
     C     *IN90         IFEQ      *OFF
     C                   EXSR      GETDSC
     C                   ENDIF
     C*
     C* Calculate network discount
     C*
     C     *IN90         IFEQ      *OFF
     C                   EXSR      CLCDSC
     C                   ENDIF
     C*
     C* If CANDIS has an amount then move new network code into CAEOMB
     C*
     C     CANDIS        IFGT      *ZEROS
     C                   EXSR      WRTDEP
     C                   ENDIF
     C*
     C                   UPDATE    PCA01
     C                   ENDIF                                                   CLM STS CD NE 'C'
     C                   ENDIF                                                   END *IN61
     C*
     C                   SETON                                        LR
     *********************************************************************
     C* CHKCPT - Find discount that CPT and revenue code match         N T
     *********************************************************************
     C     CHKCPT        BEGSR
     C*
     C                   MOVEL(P)  DURVCD        KK10A1
     C*
     C     COKY1         SETGT     PCO01
     C*** BEG CHG 02/25/09 ********************************************
     C********** COKY2     REDPEPCO01                    90
     C********** *IN90     DOWEQ*OFF
     C     COKY2         READPE    PCO01                                  99
     C     *IN99         DOWEQ     *OFF
     C*** END CHG 02/25/09 ********************************************
     C*
     C     CO10A1        IFLE      KK10A1
     C     CO10A2        ANDGE     KK10A1
     C     CO10A3        ANDLE     DURATE
     C     CO10A4        ANDGE     DURATE
     C                   SETOFF                                       90
     C                   LEAVE
     C                   ENDIF
     C*
     C*** BEG CHG 02/25/09 ********************************************
     C********   CO10A1    IFGT KK10A1
     C********   CO10A3    ORGT DURATE
     C     CO10A2        IFLT      KK10A1
     C     CO10A4        ORLT      DURATE
     C*** END CHG 02/25/09 ********************************************
     C                   LEAVE
     C                   ENDIF
     C*
     C*** BEG CHG 02/25/09 ********************************************
     C********** COKY2     REDPEPCO01                    90
     C     COKY2         READPE    PCO01                                  99
     C*** END CHG 02/25/09 ********************************************
     C                   ENDDO
     C*
     C                   ENDSR
     *********************************************************************
     C* CHKREV - Find discount that Revenue code match                 N T
     *********************************************************************
     C     CHKREV        BEGSR
     C*
     C                   MOVEL(P)  DURVCD        KK10A1
     C*
     C     COKY1         SETGT     PCO01
     C     COKY2         READPE    PCO01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     CO10A1        IFLE      KK10A1
     C     CO10A2        ANDGE     KK10A1
     C     CO10A3        ANDEQ     *BLANKS
     C                   SETOFF                                       90
     C                   LEAVE
     C                   ENDIF
     C*
     C*** BEG CHG 02/25/09 ********************************************
     C********** CO10A1    IFGT KK10A1
     C     CO10A2        IFLT      KK10A1
     C*** END CHG 02/25/09 ********************************************
     C                   LEAVE
     C                   ENDIF
     C*
     C     COKY2         READPE    PCO01                                  99
     C                   ENDDO
     C*
     C                   ENDSR
     *********************************************************************
     C* CLCDSC - Calculate the dicsount                                N T
     *********************************************************************
     C     CLCDSC        BEGSR
     C*
     C     COFEE         DIV       100           XXDISC            9 4
     C*
     C     CAPAID        MULT(H)   XXDISC        CANDIS
     C                   SUB       CANDIS        CAPAID
     C*
     C                   ENDSR
     *********************************************************************
     C* GETDSC - Get the discount rate for this line item              N T
     *********************************************************************
     C     GETDSC        BEGSR
     C*
     C* If there is a cpt associated with the line item then let's
     C* find a match based upon revenue code and cpt code
     C*
     C                   SETON                                        90
     C     DURATE        IFNE      *BLANKS
     C                   EXSR      CHKCPT
     C                   ENDIF
     C*
     C* If no hit from above or only revenue code then let's find
     C* match based upon revenue code only
     C*
     C     *IN90         IFEQ      *ON
     C                   EXSR      CHKREV
     C                   ENDIF
     C*
     C                   ENDSR
     *********************************************************************
     C* REMMSG - Remove adjudication messages/EOMB Codes               N T
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
     *********************************************************************
     C* WRTDET - Write the detail note                                 N T
     *********************************************************************
     C     WRTDEP        BEGSR
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
     C*
     C                   ENDSR
