     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4052RTX                                          *
     H*    DESCRIPTION: ADJUDICATION (08/01/97 RULES)                 *
     H*   This program is called from F0070R (Claim Adjudication)     *
     H*   and will perform the actual adjudication of the detail      *
     H*   claim records written by F0070R.                            *
     H*                                                               *
     H*  This Sub-program is intended to be used to calculate the     *
     H*  amount that is due a provider of services, by the carrier    *
     H*  of the Workers' Compensation insurance.                      *
     H*                                                               *
     H*  It should be used for any service provided on or after       *
     H*  08/01/97.                                                    *
     H*****************************************************************
     H*  CREATED: C:59 09/05/97    PROJECT: TX007                     *
     H*****************************************************************
     H*  Specialty Codes used in this program:
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*  04/08/99 C:117 Support: MC095 Seq# 17568
     H*                 Added QQCO#, QQSUB#, QQFND to the *ENTRY PARMs
     H*                 and changed all occurences of UPCO#, UPSUB#,
     H*                 UPFND to the above mentioned fields.
     H*  09/04/98 C:31  Project: 95MAL
     H*                 Ntwk Disc:Lesser of FS/Chr
     H*                 1. Add update of CAMRAA
     H*
     H*****************************************************************
     H*  INDICATOR USAGE:                                            **
     H*****************************************************************
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMSVL1TX  IF   E           K DISK
     FFTABLTX   IF   E           K DISK
     FFDCAL7    UF   E           K DISK
     FFADCL     IF   E           K DISK
     FFDCALO    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0O)
     FFDCALC    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0C)
     FFDHCL     IF   E           K DISK
     F*

     D*
     D QQS             S              5    DIM(30)                              DETAIL SEQ #
     D QQC             S              2    DIM(30)                              ERROR MESSAGES
     D QQF             S              6    DIM(50)                              FROM DATES
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  XXCMSV                 1     11
     D  XXRVCD                 1      3  0
     D  XXHCPC                 4      8
     D  XXUNIT                 9     11  0
     D*
     D*  DATE/TIME DATA STRUCTURE.
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  FSY                    1     10
     D                                     DIM(10)                              GROUP SYS CODE
     D  FNSY01                 1      1
     D  FNSY02                 2      2
     D  FNSY03                 3      3
     D  FNSY04                 4      4
     D  FNSY05                 5      5
     D  FNSY06                 6      6
     D  FNSY07                 7      7
     D  FNSY08                 8      8
     D  FNSY09                 9      9
     D  FNSY10                10     10

     I*
     I*  RENAME THE DUPLICATE FIELD NAMES.
     IPCA0C
     I              CACPT4                      XCCPT4
     I              CACO#                       XCCO#
     I              CASUB#                      XCSUB#
     I              CAFND                       XCFND
     I              CAFYR                       XCFYR
     I              CACASE                      XCCASE
     I              CACSCD                      XCCSCD
     I              CAFDAT                      XCFDAT
     I              CATDAT                      XCTDAT
     I              CARDAT                      XCRDAT
     I              CAIPOP                      XCIPOP
     I              CAFEIN                      XCFEIN
     I              CAMOD                       XCMOD
     I              CAPRTY                      XCPRTY
     I              CATRCD                      XCTRCD
     I              CASVCD                      XCSVCD
     I              CAICD9                      XCICD9
     I              CAANMN                      XCANMN
     I              CAUNTS                      XCUNTS
     I              CATOTA                      XCTOTA
     I              CATOTC                      XCTOTC
     I              CATOTD                      XCTOTD
     I              CATOTP                      XCTOTP
     I              CADSEQ                      XCDSEQ
     I              CADELF                      XCDELF
     I              CAFLAG                      XCFLAG
     I              CASTAT                      XCSTAT
     I              CAP1                        XCP1
     I              CAPMOD                      XCPMOD
     I              CAUSER                      XCUSER
     I              CAPGM                       XCPGM
     I              CADT                        XCDT
     I              CATIME                      XCTIME
     I              CACHK#                      XCCHK#
     I              CAEOB#                      XCEOB#
     I              CAPART                      XCPART
     I              CAEOMB                      XCEOMB
     I              CARCFL                      XCRCFL
     I              CADIFF                      XCDIFF
     I              CAOTHR                      XCOTHR
     I              CANDIS                      XCNDIS
     I              CARDTP                      XCRDTP
     I              CABRSV                      XCBRSV
     I              CABRPM                      XCBRPM
     I              CABRPC                      XCBRPC
     I              CABRAM                      XCBRAM
     I              CABRUN                      XCBRUN
     I              CABRTC                      XCBRTC
     I              CAZIPA                      XCZIPA
     I              CAAST                       XCAST
     I*
     IPCA0O
     I              CASVCD                      OXSVCD
     I              CACO#                       OXCO#
     I              CASUB#                      OXSUB#
     I              CAFND                       OXFND
     I              CAFYR                       OXFYR
     I              CACASE                      OXCASE
     I              CAFDAT                      OXFDAT
     I              CADSEQ                      OXDSEQ
     I              CAFEIN                      OXFEIN
     I              CAMOD                       OXMOD
     I              CATOTA                      OXTOTA
     I              CATOTC                      OXTOTC
     I              CATOTD                      OXTOTD
     I              CATOTP                      OXTOTP
     I              CAPMOD                      OXPMOD
     I              CAUSER                      OXUSER
     I              CAPGM                       OXPGM
     I              CADT                        OXDT
     I              CATIME                      OXTIME
     I              CACHK#                      OXCHK#
     I              CAEOB#                      OXEOB#
     I              CADXR                       OXDXR
     I              CAFLAG                      OXFLAG

     C*
     C*  CLAIM DETAIL FILE
     C     CAKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C*
     C     CAKY1A        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C                   KFLD                    SVPRTY
     C                   KFLD                    SVDSEQ
     C                   KFLD                    SVEOB#
     C                   KFLD                    SVCHK#
     C*
     C     CAKY1Z        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    HLFEIN
     C                   KFLD                    HLMOD
     C                   KFLD                    HLPRTY
     C                   KFLD                    HLDSEQ
     C                   KFLD                    HLEOB#
     C                   KFLD                    HLCHK#
     C*
     C     CAKY4         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    XXSVCD
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXUNTS
     C*
     C     CAKYO         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    XXSVCD
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXUNTS
     C*
     C     CAKYC2        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C*
     C*
     C*  CLAIM MASTER FILE
     C     CMKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C*
     C*  Provider Code File
     C     PDKY          KLIST
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C*
     C*  SERV CODE FILE
     C     SVKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    XXCMSV
     C                   KFLD                    CAFDAT
     C*
     C*  CLAIM STATUS CODE TABLE FILE
     C     TCSKY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    TABKEY
     C*
     C*  DIAGNOSIS CODE/DETAIL CHARGE FILE
     C     DCKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C*
     C*  USER PARAMETER FILE
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  DUMMY KEY USED FOR RELEASE OF ALL CA RECORDS
     C*
     C     DUMMY         KLIST
     C                   KFLD                    QQCO#
     C*
     C*   KEY LIST FOR THE FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C*
     C* DETAIL CHARGE AUXILIARY FILE
     C*
     C     HCKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    CADSEQ
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQFDT           150
     C                   PARM                    QQFDT2          150
     C                   PARM                    QQS
     C                   PARM                    QQC
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQSS#             9 0
     C                   PARM                    QQACDT            8 0          (MMDDYY ORD)
     C                   PARM                    QQSPCD            3
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQSUB#            5 0
     C                   PARM                    QQFND             3 0
     C*** END ADD *** 04/08/99 ****************************************
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
     C***************************************************************
     C* INITIALIZATION SECTION
     C                   Z-ADD     *ZEROS        S                 3 0
     C                   Z-ADD     *ZEROS        Q                 3 0
     C                   Z-ADD     *ZEROS        XXALOW            9 2
     C                   Z-ADD     *ZEROS        XXOVRP            3 2
     C*
     C                   Z-ADD     001.75        XXRADI            5 2           RADI Conversion
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDATE            7 0
     C**
     C*
     C     *LIKE         DEFINE    CADSEQ        HLDSEQ                         5.0
     C     *LIKE         DEFINE    CAFEIN        HLFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         HLMOD                          3.0
     C     *LIKE         DEFINE    CAPRTY        HLPRTY                         1
     C     *LIKE         DEFINE    CAEOB#        HLEOB#                         9.0
     C     *LIKE         DEFINE    CACHK#        HLCHK#                         7.0
     C     *LIKE         DEFINE    CACHK#        SVCHK#                         7.0
     C     *LIKE         DEFINE    CADSEQ        SVDSEQ                         5.0
     C     *LIKE         DEFINE    CAEOB#        SVEOB#                         9.0
     C     *LIKE         DEFINE    CAFEIN        SVFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         SVMOD                          3.0
     C     *LIKE         DEFINE    CAPRTY        SVPRTY                         1
     C     *LIKE         DEFINE    PDSPCD        SVSPCD
     C     *LIKE         DEFINE    CAUNTS        SVUNTS                         5.2
     C     *LIKE         DEFINE    CSKEY         TABKEY                         10
     C     *LIKE         DEFINE    CACPT4        XPCPT4                         5
     C     *LIKE         DEFINE    CAEOMB        XPEOMB                         3
     C     *LIKE         DEFINE    CAPMOD        XPPMOD                         2
     C     *LIKE         DEFINE    CATOTA        XPTOTA                         9.2
     C     *LIKE         DEFINE    CATOTD        XPTOTD                         9.2
     C     *LIKE         DEFINE    CATOTP        XPTOTP                         9.2
     C     *LIKE         DEFINE    CADIFF        XPDIFF                         9.2
     C     *LIKE         DEFINE    CABRTC        XPBRTC
     C     *LIKE         DEFINE    CACPT4        XXCPT4                         5
     C     *LIKE         DEFINE    CAFDAT        XXDATE
     C     *LIKE         DEFINE    CASVCD        XXSVCD                         4.0
     C     *LIKE         DEFINE    CAUNTS        XXUNTS                         5.2
     C     *LIKE         DEFINE    CAFDAT        XXYMD
     C     *LIKE         DEFINE    CATOTA        XXINVC                         9.2
     C     *LIKE         DEFINE    CATOTA        XXDIEM                         9.2
     C     *LIKE         DEFINE    CATOTA        XXMRA                          9.2
     C*
     C     *LIKE         DEFINE    CAFEIN        SXFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         SXMOD                          3.0
     C*
     C***************************************************************
     C*
     C     QQRTCD        CABEQ     'P'           TERM                           PRE-OPEN FILES
     C*
     C                   MOVEA     QQFDT         QQF(1)
     C                   MOVEA     QQFDT2        QQF(26)
     C*
     C** GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
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
     C     CMKY          CHAIN     PCM01                              60
     C*
     C** IF THE GET WAS UNSUCCESSFUL END THE PROGRAM.
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '99998'       QQS(1)
     C                   MOVE      '96'          QQC(1)                         INVALID CASE #
     C                   GOTO      TERM
     C                   END
     C*
     C** ADJUDICATE FOR EACH POSSIBLE DATE ENTERED IF AFTER 08/01/91
     C*
     C                   DO        50            S
     C                   MOVE      QQF(S)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C     QQF(S)        IFNE      *ZERO
     C     XXYMD         ANDGE     970801
     C     CAKY1         CHAIN     PCA01                              54
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAUNTS        SVUNTS
     C                   MOVEL     CACPT4        XPCPT4
     C                   MOVE      CAPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C* --- Get and save the Provider's Specialty Code ----------------
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD
     C                   END
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C*
     C*+*+*+*+*+*+* Do Loop for Each Claim Detail Record +*+*+*+*+*+*+*+*+*+**
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
     C                   EXCEPT    RCA01
     C*
     C* If the Service Date is greater than the system date, give err message
     C     CAFDAT        IFGT      SYDATE
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '79'          QQC(Q)
     C*** Err Msg 79: "Service Date entered is greater than today's date
     C*                (system date)."
     C                   END                                                    END EQ *ZEROS
     C                   END                                                    END GT SYDATE
     C*
     C** GET THE CLAIM STATUS RECORD. SEE IF OVERRIDE FLAG IS 'Y'.
     C*
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C*
     C** A HARD ERROR WILL CAUSE THE PROGRAM TO TERMINATE ASAP.
     C*
     C     *IN60         IFEQ      '1'
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '97'          QQC(Q)                         INVALID CLM STS CODE
     C*** Err Msg 97: "Invalid Claim Status Code, See Programmer"
     C                   END
     C                   END
     C*
     C* * * IF OVERRIDE FLAG IS NOT EQUAL TO 'Y' THEN PROCESS THIS RECORD
     C* * * ELSE DO NOT ADJUDICATE.
     C*
     C     CSOVRD        IFNE      'Y'
     C*
     C* ===== NON-OVERRIDE TYPE ======================================
     C*** CHECK FOR LEVEL BREAK (PROVIDER)
     C     SXFEIN        IFNE      CAFEIN
     C     SXMOD         ORNE      CAMOD
     C                   EXSR      NEWPRO
     C                   END
     C*
     C* ===== NON-HOSPITAL TYPE ======================================
     C     CAPRTY        IFNE      '4'
     C                   GOTO      NXTRCD
     C                   END
     C*
     C* ===== HOSPITAL TYPE ==========================================
     C                   EXSR      HOSP
     C*
     C                   ELSE                                                   OVERRIDE TYPE
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM ON HOSPITAL.
     C*
     C     CAEOB#        IFEQ      *ZERO
     C                   MOVE      CASVCD        XXSVCD
     C                   MOVE      CACPT4        XXCPT4
     C                   MOVE      CAUNTS        XXUNTS
     C*
     C     CAKYO         CHAIN     PCA0O                              54
     C*
     C     DUPCL3        TAG
     C*
     C     *IN54         IFEQ      '0'
     C*
     C     CADSEQ        IFNE      OXDSEQ
     C     CADSEQ        OREQ      OXDSEQ
     C     CAEOB#        ANDNE     OXEOB#
     C*
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C                   END
     C                   ELSE
     C*
     C     CAKYO         READE     PCA0O                                  54                3
     C  N54              GOTO      DUPCL3
     C                   END
     C                   END
     C                   END
     C*
     C* ===== OVERRIDE TYPE ==========================================
     C     CAKY1A        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C*** IF OVERRIDE TYPE WE MUST STILL OUTPUT THE ALLOWED/DISALLOWED ETC.
     C     CAEOB#        IFEQ      *ZERO
     C     CATOTD        ANDEQ     0
     C     CAOTHR        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CATOTA        ANDEQ     0
     C     CATOTP        ANDEQ     0
     C*
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CATOTC        MULT      XXOVRP        CATOTA
     C                   END
     C*
     C                   Z-ADD     *ZEROS        CABRTC
     C     CATOTC        SUB       CATOTA        CATOTD
     C                   Z-ADD     CATOTA        CATOTP
     C                   END
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XCMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA01
     C                   END
     C*
     C                   END
     C*
     C     NXTRCD        TAG
     C*
     C*
     C*+*+*+*+*+*+*+*+*+*+ Read Next Claim Detail Record +*+*+*+*+*+*+*+*+*+**
     C     CAKY1         READE     PCA01                                  54      GET NEXT RCD
     C*                                                      TO PROCESS
     C*
     C     *IN54         IFEQ      '0'                                          SAVE FIELDS OF
     C                   Z-ADD     CADSEQ        SVDSEQ                         CURRENT REC.
     C                   MOVE      CAFEIN        SVFEIN
     C                   MOVE      CAMOD         SVMOD
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAUNTS        SVUNTS
     C                   MOVEL     CACPT4        XPCPT4
     C                   MOVE      CAPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C*
     C*----------------------------------------------------------------
     C* --- Get and save the Provider's Specialty Code ----------------
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD
     C                   END
     C                   END
     C*
     C                   END
     C*
     C                   END
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C** RELEASE ALL THE FORMATS FOR A SAVE RETURN.
     C*
     C     DUMMY         CHAIN     PCA01                              54
     C  N54              EXCEPT    RCA01
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     0             XCMRA             9 2
     C*** END ADD *** 09/04/98 ****************************************
     C                   RETURN
     C                   MOVE      '1'           *INLR
     *********************************************************************
     *** NEW PROVIDER SUBROUTINE (LEVEL BREAK)               N E W P R O *
     *********************************************************************
     C     NEWPRO        BEGSR
     C*
     C                   Z-ADD     CADSEQ        HLDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        HLFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         HLMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        HLPRTY
     C                   Z-ADD     CAEOB#        HLEOB#
     C                   Z-ADD     CACHK#        HLCHK#
     C*
     C**** NOT NEEDED *************
     C                   Z-ADD     HLDSEQ        CADSEQ                         SAVE FIELD VALS
     C                   MOVE      HLFEIN        CAFEIN                         FOR RECORD WE
     C                   MOVE      HLMOD         CAMOD                          ARE PROCESSING
     C                   MOVE      HLPRTY        CAPRTY
     C                   Z-ADD     HLEOB#        CAEOB#
     C                   Z-ADD     HLCHK#        CACHK#
     C*** REGET RECORD.
     C     CAKY1Z        CHAIN     PCA01                              54
     C*
     C  N54              EXCEPT    RCA01
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ
     C                   MOVE      CAFEIN        SVFEIN                            CURR REC.
     C                   MOVE      CAMOD         SVMOD
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C*
     C*----------------------------------------------------------------
     C* Get and save the Provider's Specialty Code
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD
     C                   END
     C*
     C                   MOVE      CAFEIN        SXFEIN
     C                   MOVE      CAMOD         SXMOD
     C*
     C     NEWPRE        ENDSR
     *********************************************************************
     *** HOSPITAL SUBROUTINE   SERVICE CODES                     H O S P *
     *********************************************************************
     C     HOSP          BEGSR
     C*
     C*----------------------------------------------------------------
     C* Get INPATIENT / OUTPATIENT FLAG
     C                   MOVE      *BLANKS       XXOUT
     C     DCKY          CHAIN     PDC01                              60
     C     *IN60         IFEQ      '0'
     C                   SUBST     DC10A1:6      XXOUT             3
     C                   SUBST     DC10A3:1      XXTOS             2
     C                   END
     C*
     C*-- GET INVOICE AMOUNT FOR IMPLANTABLES -----------------------------
     C*
     C*--------------------------------------------------------------------
     C*
     C*** USE STANDARD (000xxxxx000) AS SVKY (I.E. 000ROOMM000)
     C*** TO RETRIEVE SERVICE CODE RECS (FOR PER DIEM RATES)
     C                   Z-ADD     CASVCD        XXRVCD
     C*** BEGIN CHG *** 08/29/97 **************************************
     C** ALLOWS USER TO INPUT BOTH CPT AND REVENUE WITH DESC FOR REV
     C     CACPT4        IFGE      '70000'
     C     CACPT4        ANDLE     '76400'
     C                   MOVE      '000'         XXRVCD
     C                   MOVE      CACPT4        XXHCPC            5
     C                   ELSE
     C                   MOVE      *BLANKS       XXHCPC            5
     C                   ENDIF
     C*** END CHG ***** 08/29/97 **************************************
     C                   Z-ADD     *ZEROS        XXUNIT
     C*
     C     SVKY          SETGT     PSV01                                        GET SERV CODE
     C     NXTSV1        TAG
     C                   READP     PSV01                                  60     RECORD
     C     *IN60         IFEQ      '0'
     C     SVCO#         ANDEQ     CACO#
     C     SVCMSV        ANDEQ     XXCMSV
     C     SVEFDT        ANDLE     CAFDAT                                       ADD 08/24/95
     C     SVSTAT        CABEQ     'I'           NXTSV1
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     SVALOW        XCMRA
     C*** END ADD *** 09/04/98 ****************************************
      *
      * USED TO ADD 10% TO ANY CODE WITH SERVICE TYPE EQUAL TO '6'
      *
     C                   MOVEL     SVTYPE        XXTYPE            1
      *
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C     SVCO#         ORNE      CACO#
     C     SVCMSV        ORNE      XXCMSV
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZERO         XPDIFF
     C                   MOVE      'G  '         XPEOMB
     C*** EOMB G:   "INCLUDED IN GLOBAL CHARGE"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C                   END                                                    END *IN60=1
     C****************************************************************
      *  Page 66, #5  if any of the following ICD9 codes are billed
      *  as the #1 spot, diagnosis 800 95950, 940-9499, 042-0449 pay
      *  100% with error message
     C*
     C     CAICD9        IFGE      '800  '
     C     CAICD9        ANDLE     '95950'
      *
     C     CAICD9        ORGE      '042  '
     C     CAICD9        ANDLE     '0449 '
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GI'          QQC(Q)
     C*** Err Msg GI: "Carrier to set payment at fair and reasonable  te
     C*                rate."
      *
      *    Uses CATOTC to pay in full in calculation
      *
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     CATOTC        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C                   ENDIF                                                  END CASVCD
     C*
      *
      ****************************************************************
      *  Priority 2
      *  Page 71, If the total billed amount is > $40,000.00 pay the
      *  billed amount X 75%
      *
     C*  Stop-Loss Method.  Stop-loss is an independent reimbursement
     C*  methodology that will reimburse that hospital for unusually
     C*  costly services rendered during treatment to an injured
     C*  worker. To be eligible for the stop-loss payment, the
     C*  total charges (audited) for the hospital admission must
     C*  exceed $40,000.
     C*  The total charges for the admission are mult by 75%
     C*  to determine the MRA.
     C*
     C                   Z-ADD     *ZEROS        XXCHRG            9 2          ADD 01/20/96
     C     CAKYC2        CHAIN     PCA0C                              60
     C*
     C     *IN60         DOWEQ     '0'
     C***        XCCPT4    IFEQ CACPT4
     C***        XCDSEQ    ANDNECADSEQ
     C***        XCTOTP    ANDGT*ZEROS                     IN TESTING
     C***        XCCPT4    ORNE CACPT4
     C***        XCTOTP    ANDGT*ZEROS                     IN TESTING
     C*   ACCUMULATE TOTAL CHARGED AMOUNT
     C                   ADD       XCTOTC        XXCHRG
     C***                  ENDIF
     C     CAKYC2        READE     PCA0C                                  60
     C                   ENDDO                                                  END DOWEQ
     C     XXCHRG        IFGT      40000.00
     C     CATOTC        MULT      .75           XXMRA                                      sion
      *
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GJ'          QQC(Q)
     C*** Err Msg GJ: "Stop Loss reached, billed is greater than $40,000.00
      *
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C                   ENDIF
      ****************************************************************
      *  Page 70A, any service code with service type equal to 6, add
      *  10% to CHARGED AMOUNT
      *
     C     XXTYPE        IFEQ      '6'
     C     XXTOS         IFEQ      'IR'
     C     XXTOS         OREQ      'IP'
     C     XXTOS         OREQ      'IM'
     C     XXTOS         OREQ      'IS'
      *
      * CHAIN TO THE DETAIL CHARGE AUXILIARY FILE TO GET INVOICE AMT
      *
     C     HCKY          CHAIN     PHC01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     XXRVCD        IFGE      274
     C     XXRVCD        ANDLE     276
     C     XXRVCD        OREQ      278
     C     HC92N2        IFGE      250
     C     HC92N2        MULT      1.10          XXINVC
     C                   ELSE
     C                   Z-ADD     HC92N2        XXINVC
     C                   ENDIF
      *
     C     CATOTC        IFLE      XXINVC
     C*
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     CATOTC        XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXINVC        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   END                                                    END CATOTC
     C                   ENDIF                                                  END XXRVCD
     C*
     C*
     C* Update charge with payment
     C*
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C*
     C                   END                                                    END *IN60 EQ '0'
     C                   END                                                    END XXTOS
     C                   END                                                    END SVTYPE EQ 6
     C*
      ****************************************************************
      *  Page 70A, any service code with service type equal to 6, add
      *  If IR, IP, IM, IS with 250-269  no pay
      *  If HS, HO, AS, AO with 250-269  Pay in full
      *
     C     XXRVCD        IFGE      250
     C     XXRVCD        ANDLE     269
      *
     C     XXTOS         IFEQ      'IR'
     C     XXTOS         OREQ      'IP'
     C     XXTOS         OREQ      'IM'
     C     XXTOS         OREQ      'IS'
     C*
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   MOVE      'G  '         XPEOMB
     C*** EOMB G:   "INCLUDED IN GLOBAL CHARGE"
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        XPDIFF
     C     CATOTC        COMP      XXMRA                              60
     C   60              Z-ADD     XXMRA         XPTOTA
     C  N60              Z-ADD     CATOTC        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   END                                                    END XXTOS
     C*
     C* Update charge with payment
     C*
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C                   ENDIF                                                  END XXRVCD
     C*
      ****************************************************************
      *  REVISION -- SUPPORT 9314
     C*
     C     CACPT4        IFGE      '70000'
     C     CACPT4        ANDLE     '79999'
      *****************************************************************
      *
      * DETERMINES INPATIENT OR OUTPATIENT AND GETS FLAG ACCORDINGLY
      *
     C     XXTOS         IFEQ      'IR'
     C     XXTOS         OREQ      'IP'
     C     XXTOS         OREQ      'IM'
     C     XXTOS         OREQ      'IS'
      *
     C                   MOVEL     SVFL01        XXFL              1
     C                   ELSE
     C                   MOVEL     SVFL02        XXFL
     C                   ENDIF
      *****************************************************************
      *
     C     XXFL          IFNE      'C'
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZERO         XPDIFF
     C                   MOVE      'G  '         XPEOMB
     C*** EOMB G:   "INCLUDED IN GLOBAL CHARGE"
     C*
     C                   ELSE
     C                   Z-ADD     *ZEROS        XPDIFF
     C     CATOTC        COMP      SVALOW                             60
     C   60              Z-ADD     SVALOW        XPTOTA
     C  N60              Z-ADD     CATOTC        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
      *
     C                   ENDIF                                                  END XXTOS
      *
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C                   ENDIF                                                  END CACPT4
      *
      ****************************************************************
      *  PAGE 68.
      *  Revenue codes 100-169 need to caulculate the number of units
      *  for the line item by either 870.00 (IM) or 1118.00(IS)
      *
     C     XXTOS         IFEQ      'IM'
     C     XXTOS         OREQ      'IS'
     C                   MOVE      'Y'           XXFLG             1
      *
     C     CASVCD        IFGE      200
     C     CASVCD        ANDLE     214
     C     CAUNTS        MULT      SVALOW        XXMRA
     C                   MOVE      'N'           XXFLG
     C                   ENDIF                                                  END XXTOS IM
      *
     C     XXTOS         IFEQ      'IM'
     C     CASVCD        ANDGE     100
     C     CASVCD        ANDLE     169
     C     CAUNTS        MULT      870.00        XXMRA
     C                   MOVE      'N'           XXFLG
     C                   ENDIF                                                  END XXTOS IM
      *
     C     XXTOS         IFEQ      'IS'
     C     CASVCD        ANDGE     100
     C     CASVCD        ANDLE     169
     C     CAUNTS        MULT      1118.00       XXMRA
     C                   MOVE      'N'           XXFLG
     C                   ENDIF                                                  END XXTOS IM
     C*
     C     CASVCD        IFGE      380
     C     CASVCD        ANDLE     399
     C     CASVCD        OREQ      413
     C     CASVCD        OREQ      545
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GI'          QQC(Q)
     C*** Err Msg GI: "Carrier to set payment at fair and reasonable  te
     C*                rate."
     C                   MOVE      'N'           XXFLG
     C     CAUNTS        MULT      SVALOW        XXMRA
     C                   ENDIF                                                  END CASVCD
      *
     C     XXFLG         IFEQ      'Y'
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZERO         XPDIFF
     C                   MOVE      'G  '         XPEOMB
     C*** EOMB G:   "INCLUDED IN GLOBAL CHARGE"
     C*
     C                   ELSE
     C                   Z-ADD     *ZEROS        XPDIFF
     C     CATOTC        COMP      XXMRA                              60
     C   60              Z-ADD     XXMRA         XPTOTA
     C  N60              Z-ADD     CATOTC        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   ENDIF                                                  END CASVCD
     C*
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C                   ENDIF                                                  END XXTOS
     C*
      ****************************************************************
      *  Page 66, If type of service IR, IP, HS, HO, AS or AO is billed
      *  pay charged amount with error message.
     C*
     C     XXTOS         IFEQ      'IR'
     C     XXTOS         OREQ      'IP'
     C     XXTOS         OREQ      'HS'
     C     XXTOS         OREQ      'HO'
     C     XXTOS         OREQ      'AS'
     C     XXTOS         OREQ      'AP'
     C     XXTOS         OREQ      'IM'
     C     XXTOS         OREQ      'IS'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GI'          QQC(Q)
     C*** Err Msg GI: "Carrier to set payment at fair and reasonable
     C*                rate."
      *
     C     CAUNTS        MULT      SVALOW        XXMRA
      *
      *    Uses CATOTC to pay in full in calculation
     C*
     C                   Z-ADD     *ZEROS        XPDIFF
     C     CATOTC        COMP      XXMRA                              60
     C   60              Z-ADD     XXMRA         XPTOTA
     C  N60              Z-ADD     CATOTC        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C                   ENDIF                                                  END CASVCD
     C*
      ****************************************************************
     C*
     C     CACPT4        IFEQ      'OUT'
     C     CATOTC        MULT      .90           XXMRA
     C*
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C*
     C                   END                                                    END CACPT4
     C*
     C*
     C* If Code is found in the file and it has an MRA amount, pay
     C*
     C*
     C     CAUNTS        IFGT      1
     C     SVALOW        MULT(H)   CAUNTS        XXDIEM
     C                   ELSE
     C                   Z-ADD     SVALOW        XXDIEM
     C                   END
     C*
     C*
     C***  RADIOLOGY CPT4 CODES FOR CAT SCANS AND MRI CODES
     C***    HAVE BEEN ADDED TO THE SERVICE CODE MASTER FILE
     C***    WITH THESE CODES PAY THE LESSER OF BILLED CHARGES
     C***    VS. MRA (WHICH IS FEE SCHEDULE) AMOUNT
     C***
     C                   SUBST     CACPT4:1      XXRAD7            1            RADIOLOGY CPT4
     C     XXRAD7        IFEQ      '7'
     C     CAFDAT        IFLT      960401                                       ADD 03/27/96
     C     XXDIEM        MULT      XXRADI        XXDIEM                         RADI CONV   sion
     C                   ENDIF                                                  ADD 03/27/96
     C                   ENDIF
     C     XXRAD7        IFEQ      '7'
     C     CATOTC        ANDLT     XXDIEM
     C                   Z-ADD     CATOTC        XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTD
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   ELSE                                                   ELSE XXRAD7
     C*
     C*
     C     CATOTC        IFLE      XXDIEM
     C     XXDIEM        SUB       CATOTC        XPDIFF
     C                   Z-ADD     *ZEROS        XPTOTD
     C                   Z-ADD     CATOTC        XPTOTA
     C                   Z-ADD     CATOTC        XPTOTP
     C                   Z-ADD     CATOTC        XPBRTC
     C*
     C                   ELSE
     C                   Z-ADD     *ZEROS        XPDIFF
     C     CATOTC        SUB       XXDIEM        XPTOTD
     C                   Z-ADD     XXDIEM        XPTOTA
     C                   Z-ADD     XXDIEM        XPTOTP
     C                   Z-ADD     XXDIEM        XPBRTC
     C*
     C                   END                                                    END CATOTC
     C                   END                                                    END XXRAD7
     C*
     C     CACPT4        IFEQ      'MISC'
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   END
     C*
     C                   EXSR      UPDAT
     C*
     C     HOSPE         ENDSR
     *********************************************************************
     *** UPDATE SUBROUTINE                                     U P D A T *
     *********************************************************************
     C     UPDAT         BEGSR
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** CHARGES EXCEED MAXIMUM ALLOWANCE FOUND IN REIMBURSEMENT MANUAL
     C*
     C     XPTOTD        IFGT      *ZERO
     C     XPEOMB        ANDEQ     *BLANK
     C     XPTOTA        ANDGT     *ZEROS
     C*
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*** IF REDUCED BY TO .90 USE 'M' INSTEAD OF 'F' FOR EOMB
     C***
     C     CACPT4        IFEQ      'IR'
     C     CACPT4        OREQ      'OUT'
     C*
     C                   MOVE      'M  '         XPEOMB
     C*** EOMB M:   "REDUCED TO FAIR AND REASONABLE"
     C                   END
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM ON HOSPITAL.
     C*
     C     CAEOB#        IFEQ      *ZERO
     C                   MOVE      CASVCD        XXSVCD
     C                   MOVE      CACPT4        XXCPT4
     C                   MOVE      CAUNTS        XXUNTS
     C*
     C     CAKYO         CHAIN     PCA0O                              54
     C*
     C     DUPCL2        TAG
     C*
     C     *IN54         IFEQ      '0'
     C*
     C     CADSEQ        IFNE      OXDSEQ
     C     CADSEQ        OREQ      OXDSEQ
     C     CAEOB#        ANDNE     OXEOB#
     C*
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C                   END
     C                   ELSE
     C*
     C     CAKYO         READE     PCA0O                                  54
     C  N54              GOTO      DUPCL2
     C                   END
     C                   END
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** ONLY PROCESS THE RECORD IF NOT A OVERRIDE TYPE.
     C*
     C*
     C     CAKY1A        CHAIN     PCA01                              54
     C     CSOVRD        IFNE      'Y'
     C     CACHK#        ANDEQ     *ZERO
     C     CAEOB#        ANDEQ     *ZERO
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPTOTD        CATOTD
     C                   Z-ADD     XPTOTP        CATOTP
     C                   Z-ADD     XPDIFF        CADIFF
     C                   Z-ADD     XPBRTC        CABRTC
     C                   MOVE      XPPMOD        CAPMOD
     C                   MOVE      XPEOMB        CAEOMB
     C*
     C                   MOVE      Q1USER        CAUSER
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   Z-ADD     DSTIME        CATIME
     C*
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XCMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C  N54              UPDATE    PCA01
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** CLEAR THE WORK SAVE FIELDS.
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZERO         XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      *BLANKS       XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE         ENDSR
     OPCA01     E            RCA01
