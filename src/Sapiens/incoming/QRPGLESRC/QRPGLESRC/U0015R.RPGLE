     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE U0015R - PRE/RE-CERTIFICATION EDIT PROGRAM
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    AFTER CHARGES ARE ADJUDICATED IF A PRE-CERT WAS SELECTED   *
     H*    AND THE LIABILITY IS 'A' BILL ENTRY WILL CALL THIS PROGRAM *
     H*    TO PROCESS THE CHARGES AGAINST THE PRE-CERT RECORD.        *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*   F0070R                                                      *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*   NONE                                                        *
     H*                                                               *
     H*  PROGRAMMER: KEN RUTHVEN        PROJECT: 99AAJ                *
     H*  CREATED: 12/03/92                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  12/15/97 C:56   Project: FS139                               *
     H*                  1. Add QQDCS# as a entry PARM                *
     H*                  2. Condition the use of QQDCS# based on the  *
     H*                     number of parms passed.                   *
     H*                  3. Use QQDCS# to only process detail charge  *
     H*                     with matching CADCS#.                     *
     H*                                                               *
     H*  02/02/98 C:62   DEMO                                         *
     H*                  1. CHANGE QQF DATES TO 6A MOVE TO 6.0 FIELD  *
     H*                     AND CONVERT TO 7.0 CYMD.                  *
     H*                                                               *
     H*  12/17/96 C:45   REQ/PROJ: UR019                              *
     H*                  1. VALIDATE PRE-CERT FIELDS PRIOR TO DOING   *
     H*                     EDITS SO THAT ERRORS WILL BE BECAUSE OF   *
     H*                     INVALID DATA NOT ZEROS OR BLANKS.         *
     H*                                                               *
     H*  02/21/94 C:20   REQ/PROJ: MCMNT                              *
     H*                  1. ADDED CALL TO SRFUTR TO CALCULATE THE     *
     H*                     FURTURE DATE FROM THE ADDMISSION DATE AND *
     H*                     THE RECOMMENDED DAYS                      *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*****************************************************************
     FUMPCL     IF   E           K DISK
     FUMPCL1    IF   E           K DISK
     F                                     RENAME(PPC01:PPC02)
     FFDCAL7    IF   E           K DISK
     FFDCALD    IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     FFMCML1    IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFADCL     IF   E           K DISK

     D*
     D QQS             S              5    DIM(50)                              DETAIL SEQ #
     D QQC             S              5    DIM(50)                              ERROR MESSAGES
     D*** BEGIN CHG *** 02/02/98 **************************************
     D**********          QQF        50  8               FROM DATES
     D QQF             S              6    DIM(50)                              FROM DATES
     D*** END   CHG *** 02/02/98 **************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  DCDC                   1     20
     D                                     DIM(4)                               DIAG CODES FADCP
     D  DCDIA1                 1      5
     D  DCDIA2                 6     10
     D  DCDIA3                11     15
     D  DCDIA4                16     20
     D*
     D                 DS
     D  PCDC                   1     25
     D                                     DIM(5)                               DIAG CODES UMPCP
     D  PCICD1                 1      5
     D  PCICD2                 6     10
     D  PCICD3                11     15
     D  PCICD4                16     20
     D  PCICD5                21     25
     D*
     D                 DS
     D  PCPR                   1     40
     D                                     DIM(8)                               PROC CODES UMPCP
     D  PCCPT1                 1      5
     D  PCCPT2                 6     10
     D  PCCPT3                11     15
     D  PCCPT4                16     20
     D  PCCPT5                21     25
     D  PCCPT6                26     30
     D  PCCPT7                31     35
     D  PCCPT8                36     40
     D*
     D                 DS
     D  SP                     1    056  0
     D                                     DIM(8)                               PROC FROM SRVS DATES
     D  PCSPD1               001    007  0
     D  PCSPD2               008    014  0
     D  PCSPD3               015    021  0
     D  PCSPD4               022    028  0
     D  PCSPD5               029    035  0
     D  PCSPD6               036    042  0
     D  PCSPD7               043    049  0
     D  PCSPD8               050    056  0
     D*
     D                 DS
     D  EP                     1    056  0
     D                                     DIM(8)                               PROC TO SERV DATES
     D  PCEPD1               001    007  0
     D  PCEPD2               008    014  0
     D  PCEPD3               015    021  0
     D  PCEPD4               022    028  0
     D  PCEPD5               029    035  0
     D  PCEPD6               036    042  0
     D  PCEPD7               043    049  0
     D  PCEPD8               050    056  0
     D*
     D                 DS
     D  VIS                    1     24  0
     D                                     DIM(8)                               # OF AUTH VISITS PC
     D  PCA#V1                 1      3  0
     D  PCA#V2                 4      6  0
     D  PCA#V3                 7      9  0
     D  PCA#V4                10     12  0
     D  PCA#V5                13     15  0
     D  PCA#V6                16     18  0
     D  PCA#V7                19     21  0
     D  PCA#V8                22     24  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     IPCA02
     I              CACPT4                      X2CPT4
     I              CACO#                       X2CO#
     I              CASUB#                      X2SUB#
     I              CAFND                       X2FND
     I              CAFYR                       X2FYR
     I              CACASE                      X2CASE
     I              CACSCD                      X2CSCD
     I              CAFDAT                      X2FDAT
     I              CATDAT                      X2TDAT
     I              CARDAT                      X2RDAT
     I              CAIPOP                      X2IPOP
     I              CAFEIN                      X2FEIN
     I              CAMOD                       X2MOD
     I              CAPRTY                      X2PRTY
     I              CATRCD                      X2TRCD
     I              CASVCD                      X2SVCD
     I              CAICD9                      X2ICD9
     I              CAANMN                      X2ANMN
     I              CAUNTS                      X2UNTS
     I              CATOTA                      X2TOTA
     I              CATOTC                      X2TOTC
     I              CATOTD                      X2TOTD
     I              CATOTP                      X2TOTP
     I              CADSEQ                      X2DSEQ
     I              CADELF                      X2DELF
     I              CAFLAG                      X2FLAG
     I              CASTAT                      X2STAT
     I              CAP1                        X2P1
     I              CAPMOD                      X2PMOD
     I              CAUSER                      X2USER
     I              CAPGM                       X2PGM
     I              CADT                        X2DT
     I              CATIME                      X2TIME
     I              CACHK#                      X2CHK#
     I              CAEOB#                      X2EOB#
     I              CAPART                      X2PART
     I              CAEOMB                      X2EOMB
     I              CARCFL                      X2RCFL
     I              CAOTHR                      X2OTHR
     I              CANDIS                      X2NDIS
     I              CARDTP                      X2RDTP
     I              CABRSV                      X2BRSV
     I              CABRPM                      X2BRPM
     I              CABRPC                      X2BRPC
     I              CABRAM                      X2BRAM
     I              CABRUN                      X2BRUN
     I              CABRTC                      X2BRTC
     I              CAZIPA                      X2ZIPA
     I              CAAST                       X2AST
     I              CADIFF                      X2DIFF
     I*
     IPPC02
     I              PCCO#                       P2CO#
     I              PCSUB#                      P2SUB#
     I              PCFND                       P2FND
     I              PCFYR                       P2FYR
     I              PCCASE                      P2CASE
     I              PCLIAB                      P2LIAB
     I              PCSTAT                      P2STAT
     I              PCADDT                      P2ADDT
     I              PCRVDT                      P2RVDT
     I              PCCERT                      P2CERT
     I              PCEXT                       P2EXT
     I              PCFEIN                      P2FEIN
     I              PCMOD                       P2MOD
     I              PCICD1                      P2ICD1
     I              PCICD2                      P2ICD2
     I              PCICD3                      P2ICD3
     I              PCICD4                      P2ICD4
     I              PCICD5                      P2ICD5
     I              PCCPT1                      P2CPT1
     I              PCCPT2                      P2CPT2
     I              PCCPT3                      P2CPT3
     I              PCCPT4                      P2CPT4
     I              PCCPT5                      P2CPT5
     I              PCCPT6                      P2CPT6
     I              PCCPT7                      P2CPT7
     I              PCCPT8                      P2CPT8
     I              PCSPD1                      P2SPD1
     I              PCSPD2                      P2SPD2
     I              PCSPD3                      P2SPD3
     I              PCSPD4                      P2SPD4
     I              PCSPD5                      P2SPD5
     I              PCSPD6                      P2SPD6
     I              PCSPD7                      P2SPD7
     I              PCSPD8                      P2SPD8
     I              PCEPD1                      P2EPD1
     I              PCEPD2                      P2EPD2
     I              PCEPD3                      P2EPD3
     I              PCEPD4                      P2EPD4
     I              PCEPD5                      P2EPD5
     I              PCEPD6                      P2EPD6
     I              PCEPD7                      P2EPD7
     I              PCEPD8                      P2EPD8
     I              PCA#V1                      P2A#V1
     I              PCA#V2                      P2A#V2
     I              PCA#V3                      P2A#V3
     I              PCA#V4                      P2A#V4
     I              PCA#V5                      P2A#V5
     I              PCA#V6                      P2A#V6
     I              PCA#V7                      P2A#V7
     I              PCA#V8                      P2A#V8
     I              PCTOS1                      PCTOS1
     I              PCTOS2                      P2TOS2
     I              PCTOS3                      P2TOS3
     I              PCTOS4                      P2TOS4
     I              PCTOS5                      P2TOS5
     I              PCTOS6                      P2TOS6
     I              PCTOS7                      P2TOS7
     I              PCTOS8                      P2TOS8
     I              PCINTR                      P2INTR
     I              PCPASG                      P2PASG
     I              PCREQD                      P2REQD
     I              PCCERD                      P2CERD
     I              PCRECD                      P2RECD
     I              PCPOS                       P2POS

     C*
     C*   KEY LIST FOR THE DETAIL CHARGE FILE
     C*
     C     CAKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C*
     C     CAKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*   KEY LIST FOR THE PRE-CERT MASTER FILE
     C*
     C     PCKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C                   KFLD                    XXCERT
     C                   KFLD                    XXEXT
     C*
     C     PCKEY2        KLIST
     C                   KFLD                    XXCERT
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C*
     C*   KEY LIST FOR THE DIAGNOSIS CODE WORKFILE
     C*
     C     DCKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXDCS#
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    UPDT          XXYMD
     C     *LIKE         DEFINE    UPDT          XXMXDY
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
     C*** BEGIN CHG *** 02/02/98 **************************************
     C                   PARM                    QQFDT           150
     C                   PARM                    QQFDT2          150
     C*** END   CHG *** 02/02/98 **************************************
     C                   PARM                    QQS
     C                   PARM                    QQC
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQSS#             9 0
     C                   PARM                    QQACDT            8 0          (MMDDYY ORD)
     C                   PARM                    QQCERT           15
     C                   PARM                    QQEXT             3
     C*** BEGIN ADD *** 12/15/97 *************************************
     C                   PARM                    QQDCS#            5
     C*** END ADD *** 12/15/97 ***************************************
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
     C     *LIKE         DEFINE    CADCS#        XXDCS#
     C     *LIKE         DEFINE    CAFEIN        XXFEIN
     C     *LIKE         DEFINE    CAMOD         XXMOD
     C     *LIKE         DEFINE    PCCERT        XXCERT
     C     *LIKE         DEFINE    PCEXT         XXEXT
     C*** BEGIN ADD *** 12/15/97 *************************************
     C     *LIKE         DEFINE    CADCS#        HDDCS#
     C*** END ADD *** 12/15/97 ***************************************
     C*
     C                   MOVEA     QQFDT         QQF(1)
     C                   MOVEA     QQFDT2        QQF(26)
     C                   MOVE      QQCERT        XXCERT
     C                   MOVE      QQEXT         XXEXT
     C*
     C** GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C** IF THE GET WAS UNSUCCESSFUL END THE PROGRAM.
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '99999'       QQS(1)
     C                   MOVE      '95'          QQC(1)                         INVALID USER PARM
     C                   GOTO      TERM
     C                   END
     C*
     C** GET THE CLAIM MASTER RECORD.
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C*
     C** IF THE GET WAS UNSUCCESSFUL END THE PROGRAM.
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '99998'       QQS(1)
     C                   MOVE      '96'          QQC(1)                         INVALID CASE #
     C                   GOTO      TERM
     C                   END
     C*
     C** DO THE EDIT CHECK FOR EACH POSSIBLE DATE ENTERED
     C*
     C                   DO        42            S                 3 0
     C*** BEGIN CHG *** 02/02/98 **************************************
     C**********           MOVELQQF,S     XXDATE  80
     C                   MOVEL     QQF(S)        XXDATE            6 0
     C*** END   CHG *** 02/02/98 **************************************
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C     QQF(S)        IFNE      *ZERO
     C     CAKEY1        CHAIN     PCA01                              54
     C*                                                                +*+*+**
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
     C*
     C*** BEGIN ADD *** 12/15/97 *************************************
     C*
     C*  IF DSC#s DO NOT MATCH DO NOT EDIT THE DETAIL CHARGE RECORD
     C*
     C     Q1PRMS        IFEQ      12
     C*
     C                   MOVE      QQDCS#        HDDCS#
     C     CADCS#        CABNE     HDDCS#        DCSTAG
     C*
     C                   END
     C*** END ADD *** 12/15/97 ***************************************
     C*
     C** GET THE PRE-CERT RECORD
     C*
     C                   Z-ADD     CAFEIN        XXFEIN
     C                   Z-ADD     CAMOD         XXMOD
     C                   Z-ADD     CADCS#        XXDCS#
     C     PCKEY1        CHAIN     PPC01                              60
     C     *IN60         IFEQ      '0'
     C*
     C** CHECK THE PLACE OF SERVICE CODE FOR A MATCH
     C*
     C     PCPOS         IFNE      *BLANKS                                      PCPOS NE *BLANKS
     C     PCPOS         IFNE      CAIPOP
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     50            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'PC002'       QQC(Q)                         INVALID POS CODE
     C                   END
     C                   ENDIF
     C*
     C* TAKE ADMIT DATE AND THE RECOMMENDED DAYS = MAX DAYS
     C*
     C*
     C                   Z-ADD     PCADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C*
     C                   Z-ADD     PCRECD        SRFUTR            5 0
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD            1
     C*
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXMXDY
     C*
     C                   Z-ADD     *ZEROS        SRFUTR
     C*
     C* IF THIS IS A RE-CERT WE NEED TO ADD ALL RECOMMENDED DAYS FROM
     C* THE PRE-CERT AND ALL RE-CERT PRIOR TO AND INCLUDING THIS ONE
     C* TO GET CORRECT MAX DAYS RANGE
     C*
     C     XXEXT         IFNE      *ZEROS
     C     PCKEY2        CHAIN     PPC02                              60
     C     *IN60         DOWEQ     '0'
     C     P2EXT         ANDLT     XXEXT
     C                   ADD       P2RECD        SRFUTR
     C     PCKEY2        READE     PPC02                                  60
     C                   END
     C*
     C*
     C                   Z-ADD     XXMXDY        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD            1
     C*
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXMXDY
     C*
     C                   END
     C*
     C** NO CHARGES ARE ALLOWED OUT OF THE MAX. DATE RANGE
     C*
     C     PCADDT        IFNE      *ZEROS                                       PCADDT NE *ZEROS
     C     XXYMD         IFLT      PCADDT
     C     XXYMD         ORGT      XXMXDY
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     50            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'PC003'       QQC(Q)                         INVALID POS CODE
     C                   END
     C                   ENDIF
     C*
     C* VERIFY THE DIAGNOSIS CODE OF THE DETAIL CHARGE IS IN THE PRE-
     C* CERT RECORD
     C*
     C     DCKEY         CHAIN     PDC01                              40
     C     *IN40         IFEQ      '0'
     C                   Z-ADD     CADXR         X                 5 0
     C                   Z-ADD     1             Y                 5 0
     C                   MOVEA     PCDC          XXPCDC           25
     C     XXPCDC        IFNE      *BLANKS                                      PCICDX NE *BLANKS
     C     DCDC(X)       LOOKUP    PCDC(Y)                                60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     50            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'PC004'       QQC(Q)
     C                   END                                                    END *IN60
     C                   ENDIF
     C                   END                                                    END *IN40
     C*
     C* VERIFY THAT DETAIL CHARGE PROCEDURE IS IN THE PRE-CERT RECORD
     C*
     C     CAPRTY        IFEQ      '1'
     C                   Z-ADD     1             X                 5 0
     C                   MOVEA     PCPR          XXPCPR           40
     C     XXPCPR        IFNE      *BLANKS                                      PCCPTX NE *BLANKS
     C     CACPT4        LOOKUP    PCPR(X)                                60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     50            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'PC005'       QQC(Q)
     C                   END                                                    END *IN60
     C                   ENDIF
     C*
     C* CHECK TO SEE IF PRE-CERT FROM AND TO SERV DATES WERE ENTERED
     C*
     C     SP(X)         IFNE      *ZEROS
     C     EP(X)         ANDNE     *ZEROS
     C*
     C* IF THE PRE-CERT DATES HAVE BEEN ENTERED MAKE SURE CHARGE IS
     C* WITHIN DATE RANGE
     C*
     C     CAFDAT        IFGE      SP(X)
     C     CAFDAT        ANDLE     EP(X)
     C*
     C* IF CHARGE IS WITHIN PRE-CERT DATES READ DETAIL CHARGE FILE FOR
     C* ANY OTHER CHARGES FOR PROVIDER WITHIN PRE-CERT DATES AND MAKE
     C* SURE THAT TOTAL IS NOT GREATER THAN TOTAL # OF VISITS FROM
     C* PRE-CERT FILE
     C*
     C                   Z-ADD     *ZEROS        XX#VST            5 0
     C     CAKEY2        CHAIN     PCA02                              50
     C     *IN50         DOWEQ     '0'
     C*
     C* IF WE FIND A HIT CPT4 CODE MUST BE SAME AS CURRENT ONE
     C*
     C     CACPT4        IFEQ      X2CPT4
     C*
     C* NOW WE MUST MAKE SURE CHARGE IS WITHIN PRE-CERT FROM AND TO DATES
     C*
     C     X2FDAT        IFGE      SP(X)
     C     X2FDAT        ANDLE     EP(X)
     C                   ADD       1             XX#VST
     C                   END                                                    END X2FDAT
     C                   END                                                    END CACPT4
     C     CAKEY2        READE     PCA02                                  50
     C                   END                                                    END *IN50
     C*
     C* NOW THAT WE HAVE THE TOTAL # OF VISITS WE MUST COMPARE TO
     C* PRE-CERT RECORD AND MAKE SURE THAT IT IS LESS THAN OR EQUAL TO
     C* THE PRE/RE-CERT # OF VISITS
     C*
     C     VIS(X)        IFNE      *ZEROS                                       PCA#VX NE *ZEROS
     C     XX#VST        IFGT      VIS(X)
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     50            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'PC006'       QQC(Q)
     C                   END                                                    END XX#VST
     C                   ENDIF
     C*
     C                   ELSE                                                   ELSE CAFDAT
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     50            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'PC007'       QQC(Q)
     C                   END                                                    END CAFDAT
     C*
     C                   ELSE                                                   ELSE SP/EP
     C*
     C* IF THE FROM AND TO DATES IN PRE-CERT RECORD ARE ZEROS ALLOW
     C* ONLY THE TOTAL NUMBER OF VISITS FOUND IN THE PRE-CERT RECORD
     C* FOR THE LIFE OF THE CLAIM
     C*
     C                   Z-ADD     *ZEROS        XX#VST            5 0
     C     CAKEY2        CHAIN     PCA02                              50
     C     *IN50         DOWEQ     '0'
     C*
     C* IF WE FIND A HIT CPT4 CODE MUST BE SAME AS CURRENT ONE
     C*
     C     CACPT4        IFEQ      X2CPT4
     C                   ADD       1             XX#VST
     C                   END                                                    END CACPT4
     C     CAKEY2        READE     PCA02                                  50
     C                   END                                                    END *IN50
     C*
     C* NOW THAT WE HAVE THE TOTAL # OF VISITS WE MUST COMPARE TO
     C* PRE-CERT RECORD AND MAKE SURE THAT IT IS LESS THAN OR EQUAL TO
     C* THE PRE/RE-CERT # OF VISITS
     C*
     C     VIS(X)        IFNE      *ZEROS                                       PCA#VX NE *ZEROS
     C     XX#VST        IFGT      VIS(X)
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     50            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'PC008'       QQC(Q)
     C                   END                                                    END XX#VST
     C                   ENDIF
     C*
     C                   END                                                    END SP/EP
     C*
     C                   END                                                    END CAPRTY
     C*
     C                   END                                                    END *IN60
     C*** BEGIN ADD *** 12/15/97 *************************************
     C     DCSTAG        TAG
     C*** END ADD *** 12/15/97 ***************************************
     C     CAKEY1        READE     PCA01                                  54
     C                   END                                                    ENDDOW EQ 54
     C                   END                                                    END QQF,S
     C                   END                                                    ENDDO 42
     C*
     C* END PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
