     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4051ROK                                          *
     H*    DESCRIPTION: ADJUDICATION (08/01/91 RULES)                 *
     H*   This program is called from F0070R (Claim Adjudication)     *
     H*   and will perform the actual adjudication of the detail      *
     H*   claim records written by F0070R.                            *
     H*                                                               *
     H*  This Sub-program is intended to be used to calculate the     *
     H*  amount that is due a provider of services, by the carrier    *
     H*  of the Workers' Compensation insurance.                      *
     H*                                                               *
     H*  It should be used for any service provided on or after       *
     H*  08/01/91.                                                    *
     H*****************************************************************
     H*                                   Conversion
     H*  Subroutines/Codes                  Factor
     H*      HOSP     Hospital            $ 920.00   Unit Value Charge
     H*                                   $ 450.00   Per Day Charge
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  09/25/01 C:062 PROJECT: FSMNT
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.
     H*
     H*  04/08/99 C:117 Support: MC095 Seq# 17568
     H*                 Added QQCO#, QQSUB#, QQFND to the *ENTRY PARMs
     H*                 and changed all occurences of UPCO#, UPSUB#,
     H*                 UPFND to the above mentioned fields.
     H*  09/04/98 C:31  Project: 95MAL
     H*                 Ntwk Disc:Lesser of FS/Chr
     H*                 1. Add update of CAMRAA
     H*
     H*  05/31/92 LJD    PROJ:FS001  (per TLB)                        *
     H*                  1.Chg SVKEY keyfield UNITS to always chain   *
     H*                     with *ZEROS                               *
     H*                                                               *
     H*  11/06/92 C:20   PROJ: 99AAD                                  *
     H*                  1. ADDED PROCESSING FOR MEDICAL DEVICE ERROR *
     H*                  2. ADDED PROCESSING FOR ELEVATED CARE ERROR  *
     H*                                                               *
     H*  12/28/92 C:05   PROJ: FS081                                  *
     H*                  1. Added new parm to *ENTRY: Specialty Code  *
     H*                  2. Changed usage of SPCD from PDSPCD to QQSPCD
     H*                                                               *
     H*  01/21/93 C:20   PROJ: FS075                                  *
     H*                  1. CHANGED ADJUDICATION DO LOOP FROM 42 TO 50*
     H*
     H*  01/25/93 C:20  Project: FS054
     H*                 1. ADDED CODE TO CHECK FOR POSSIBLE DUPLICATE
     H*                    CHARGE IF THE CLAIM STATUS CODE IS AN
     H*                    OVERRIDE TYPE
     H*
     H*  03/19/93 C:05  Project: FS102
     H*                 1. Remove Provider Modifier from key that checks for
     H*                    "Possible Duplicate".  Required adding new logical
     H*                    FDCALO
     H*                 2. Rmvd reference to FDCAL4/PCA04 (wasn't used anymore)
     H*
     H*  03/19/93 C:05  Project: FS104
     H*                 1. Added Err Msg# 79 for Service Date > System Date
     H*
     H*  09/10/93 C:20  REQ/PROJ: FSMNT
     H*                 1. OVR  - FIX TO SECTION OF CODE THAT CALCULATES
     H*                    THE TOTAL PAID AMOUNT FOR CODES THAT ARE OVERRIDE
     H*                    CLAIM STATUS, ONLY CALCULATE AMOUNT IF CATOTD,
     H*                    CAOTHR, CANDIS, CATOTA, CATOTP ALL EQUAL 0
     H*
     H*  12/21/95 C:32  REQ/PROJ: OK003
     H*                 1. ADDED DATE RANGE DUE TO 01/01/96 FEE SCHEDULE
     H*****************************************************************
     H*  INDICATOR USAGE:                                            **
     H*****************************************************************
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMSVL1OK  IF   E           K DISK
     FFTABLOK   IF   E           K DISK
     FFDCAL7    UF   E           K DISK
     F*** BEGIN ADD *** 11/06/92 **************************************
     FFMFNL     IF   E           K DISK
     FNMHSL     IF   E           K DISK
     F*** END ADD *** 11/06/92 ****************************************
     F*** BEGIN ADD *** 03/19/93 **************************************
     FFDCALO    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0O)
     F*** END ADD ***** 03/19/93 **************************************
     F*

     D*
     D QQS             S              5    DIM(30)                              DETAIL SEQ #
     D QQC             S              2    DIM(30)                              ERROR MESSAGES
     D QQF             S              6    DIM(50)                              FROM DATES
     D*** BEGIN ADD *** 11/06/92 *************************************
     D*** END ADD ***** 03/19/93 **************************************
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
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*** BEGIN ADD *** 11/06/92 *************************************
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

     I*** END ADD ***** 11/06/92 *************************************
     I*
     I*  RENAME THE DUPLICATE FIELD NAMES.
     I*** BEGIN ADD *** 03/19/93 **************************************
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
     I*** END ADD ***** 11/06/92 *************************************

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
     C*** BEGIN ADD *** 03/19/93 *****************************************
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
     C*** END ADD ***** 03/19/93 **************************************
     C*
     C*  CLAIM MASTER FILE
     C     CMKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
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
     C*  USER PARAMETER FILE
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  DUMMY KEY USED FOR RELEASE OF ALL CA RECORDS
     C*
     C     DUMMY         KLIST
     C                   KFLD                    QQCO#
     C*** BEGIN ADD *** 11/06/92 *************************************
     C*   KEY LIST FOR THE FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C*
     C*  HOSPITAL PROVIDER FILE
     C*
     C     HSKEY         KLIST
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C*** END ADD ***** 11/06/92 *************************************
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
     C                   PARM                    QQSPCD            3            ADD 12/28/92
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
     C*** BEGIN CHG *** 09/25/01 **************************************
     C**********           Z-ADD*ZEROS    XXOVRP  32
     C                   Z-ADD     *ZEROS        XXOVRP            5 4
     C*** END   CHG *** 09/25/01 **************************************
     C*
     C*** BEGIN ADD *** 03/19/93 *****************************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDATE            7 0
     C*** END ADD ***** 03/19/93 *****************************************
     C**
     C*
     C     *LIKE         DEFINE    CADSEQ        HLDSEQ                         5.0
     C     *LIKE         DEFINE    CAFEIN        HLFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         HLMOD                          3.0
     C     *LIKE         DEFINE    CAPRTY        HLPRTY                         1
     C     *LIKE         DEFINE    CAEOB#        HLEOB#                         9.0
     C     *LIKE         DEFINE    CACHK#        HLCHK#                         7.0
     C*
     C     *LIKE         DEFINE    CACHK#        PTCHK#                         7.0
     C     *LIKE         DEFINE    CADSEQ        PTDSEQ                         5.0
     C     *LIKE         DEFINE    CAEOB#        PTEOB#                         9.0
     C     *LIKE         DEFINE    CAFEIN        PTFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         PTMOD                          3.0
     C     *LIKE         DEFINE    CAPRTY        PTPRTY                         1
     C*
     C     *LIKE         DEFINE    CACHK#        SVCHK#                         7.0
     C     *LIKE         DEFINE    CADSEQ        SVDSEQ                         5.0
     C     *LIKE         DEFINE    CAEOB#        SVEOB#                         9.0
     C     *LIKE         DEFINE    CAFEIN        SVFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         SVMOD                          3.0
     C     *LIKE         DEFINE    CAPRTY        SVPRTY                         1
     C     *LIKE         DEFINE    CAUNTS        SVUNTS                         5.2
     C*
     C     *LIKE         DEFINE    CSKEY         TABKEY                         10
     C*
     C     *LIKE         DEFINE    CACPT4        XPCPT4                         5
     C     *LIKE         DEFINE    CAEOMB        XPEOMB                         3
     C     *LIKE         DEFINE    CAPMOD        XPPMOD                         2
     C     *LIKE         DEFINE    CATOTA        XPTOTA                         9.2
     C     *LIKE         DEFINE    CATOTD        XPTOTD                         9.2
     C     *LIKE         DEFINE    CATOTP        XPTOTP                         9.2
     C*
     C     *LIKE         DEFINE    CACPT4        XXCPT4                         5
     C     *LIKE         DEFINE    CAFDAT        XXDATE                         6.0
     C     *LIKE         DEFINE    CASVCD        XXSVCD                         4.0
     C     *LIKE         DEFINE    CAUNTS        XXUNTS                         5.2
     C     *LIKE         DEFINE    CAFDAT        XXYMD                          6.0
     C*
     C     *LIKE         DEFINE    CAFEIN        SXFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         SXMOD                          3.0
     C*
     C                   Z-ADD     920.00        XXHOSC            7 2
     C                   Z-ADD     450.00        XXHOSD            7 2
     C                   Z-ADD     *ZEROS        XTHOSC            9 2
     C                   Z-ADD     *ZEROS        XTHOSD            9 2
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
     C** DO THE ADJUDICATION FOR EACH POSSIBLE DATE ENTERED IF AFTER 01/01/01
     C*
     C*** BEGIN CHG *** 01/21/93 **************************************
     C**********           DO   42        S
     C                   DO        50            S
     C*** END CHG *** 01/21/93 ****************************************
     C                   MOVE      QQF(S)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C     QQF(S)        IFNE      *ZERO
     C     XXYMD         ANDGE     910801
     C     XXYMD         ANDLT     960101                                       ADD 12/21/95
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
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C*
     C*+*+*+*+*+*+* Do Loop for Each Claim Detail Record +*+*+*+*+*+*+*+*+*+**
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
     C                   EXCEPT    RCA01
     C*
     C*** BEGIN ADD *** 03/19/93 *************************************
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
     C*** END ADD ***** 03/19/93 *************************************
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
     C     CSOVRD        IFNE      'Y'
     C*
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
     C*
     C                   EXSR      HOSP
     C*
     C                   ELSE                                                   OVERRIDE TYPE
     C*** BEGIN ADD *** 01/25/93 **************************************
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM ON HOSPITAL.
     C*
     C     CAEOB#        IFEQ      *ZERO
     C                   MOVE      CASVCD        XXSVCD
     C                   MOVE      CACPT4        XXCPT4
     C                   MOVE      CAUNTS        XXUNTS
     C*
     C********** CAKY4     CHAINPCA04                54    DEL 03/19/93
     C     CAKYO         CHAIN     PCA0O                              54        ADD 03/19/93
     C*
     C     DUPCL3        TAG
     C*
     C     *IN54         IFEQ      '0'
     C*
     C*** BEGIN CHG *** 03/19/93 ****************************************
     C********** CADSEQ    IFNE X4DSEQ
     C********** CADSEQ    OREQ X4DSEQ
     C********** CAEOB#    ANDNEX4EOB#
     C     CADSEQ        IFNE      OXDSEQ
     C     CADSEQ        OREQ      OXDSEQ
     C     CAEOB#        ANDNE     OXEOB#
     C*** END CHG ***** 03/19/93 ****************************************
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
     C********** CAKY4     READEPCA04                    54 DEL 03/19/93
     C     CAKYO         READE     PCA0O                                  54     ADD 03/19/93
     C  N54              GOTO      DUPCL3
     C                   END
     C                   END
     C                   END
     C*** END ADD *** 01/25/93 ****************************************
     C*
     C     CAKY1A        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C*** IF OVERRIDE TYPE WE MUST STILL OUTPUT THE ALLOWED/DISALLOWED ETC.
     C     CAEOB#        IFEQ      *ZERO
     C*** BEGIN ADD *** 09/10/93 **************************************
     C     CATOTD        ANDEQ     0
     C     CAOTHR        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CATOTA        ANDEQ     0
     C     CATOTP        ANDEQ     0
     C*** END ADD *** 09/10/93 ****************************************
     C*
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CATOTC        MULT      XXOVRP        CATOTA
     C                   END
     C*
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
     C*** BEFORE NEXT RECORD IS READ, SAVE THE KEY FIELDS NEEDED TO
     C*** RETRIEVE THE PHYSICAL THERAPY RECORDS TO BE UPDATED IN THE
     C*** PHYS2 ROUTINE WHICH IS EXECUTED ON A PROVIDER LEVEL BREAK...
     C**
     C                   Z-ADD     CADSEQ        PTDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        PTFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         PTMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        PTPRTY
     C                   Z-ADD     CAEOB#        PTEOB#
     C                   Z-ADD     CACHK#        PTCHK#
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
     C                   MOVE      CAFEIN        SXFEIN
     C                   MOVE      CAMOD         SXMOD
     C*
     C     NEWPRE        ENDSR
     *********************************************************************
     *** HOSPITAL SUBROUTINE   SERVICE CODES                     H O S P *
     *********************************************************************
     C     HOSP          BEGSR
     C*
     C                   Z-ADD     CASVCD        XXRVCD
     C                   MOVE      CACPT4        XXHCPC
     C*******************  Z-ADDCAUNTS    XXUNIT           DEL 05/31/92
     C                   Z-ADD     *ZEROS        XXUNIT                         ADD 05/31/92
     C*
     C     SVKY          SETGT     PSV01                                        GET SERV CODE
     C     NXTSV1        TAG
     C                   READP     PSV01                                  60     RECORD
     C     *IN60         IFEQ      '0'
     C     SVCO#         ANDEQ     CACO#
     C     SVCMSV        ANDEQ     XXCMSV
     C     SVSTAT        CABEQ     'I'           NXTSV1
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     SVALOW        XCMRA
     C*** END ADD *** 09/04/98 ****************************************
     C*** BEGIN ADD *** 11/06/92 *************************************
     C*
     C* CHECK SERVICE CODE FOR ELEVATED CARE
     C*
     C     SVTSRV        IFEQ      'EC'
     C*
     C     FNKEY         CHAIN     PFN01                              80
     C     *IN80         IFEQ      '0'
     C     CAFDAT        IFGE      FNEFDT
     C                   Z-ADD     1             X                 5 0
     C     'N'           LOOKUP    FSY(X)                                 80
     C     *IN80         IFEQ      '1'
     C*
     C     HSKEY         CHAIN     PHS01                              80
     C     *IN80         IFEQ      '0'
     C     CAFDAT        IFGE      HSCEDT
     C     CAFDAT        ANDLE     HSTMDT
     C     HSECFL        ANDEQ     'Y'
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '73'          QQC(Q)                         EC ERR MSG
     C*** Err Msg 72: "THIS CHARGE IS FOR ELEVATED CARE, PLEASE REVIEW"
     C                   END                                                    END GE/LE/EQ
     C                   END                                                    END *IN80 HS
     C                   END                                                    END *IN80 NET
     C                   END                                                    END GE FNEFDT
     C                   END                                                    END *IN80 FN
     C                   END                                                    END EQ EC
     C*
     C* CHECK SERVICE CODE FOR MEDICAL DEVICES
     C*
     C     SVTSRV        IFEQ      'MD'
     C*
     C     FNKEY         CHAIN     PFN01                              80
     C     *IN80         IFEQ      '0'
     C     CAFDAT        IFGE      FNEFDT
     C                   Z-ADD     1             X                 5 0
     C     'N'           LOOKUP    FSY(X)                                 80
     C     *IN80         IFEQ      '1'
     C*
     C     HSKEY         CHAIN     PHS01                              80
     C     *IN80         IFEQ      '0'
     C     CAFDAT        IFGE      HSCEDT
     C     CAFDAT        ANDLE     HSTMDT
     C     HSMDFL        ANDEQ     'Y'
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '72'          QQC(Q)                         MD ERR MSG
     C*** Err Msg 72: "THIS CHARGE IS FOR A MEDICAL DEVICE, PLEASE REVIEW"
     C                   END                                                    END GE/LE/EQ
     C                   END                                                    END *IN80 HS
     C                   END                                                    END *IN80 NET
     C                   END                                                    END GE FNEFDT
     C                   END                                                    END *IN80 FN
     C                   END                                                    END EQ MD
     C*** END ADD ***** 10/10/89 *************************************
     C*
     C** If code is in the FMSVP File and
     C     SVALOW        IFNE      *ZERO
     C*
     C**      If there is an MRA or Unit Value:
     C*  Per page H-1:
     C** DRG Unit Value X Conversion Factor (XXHOSC)
     C**     +    #Days X Per Day Charge (XXHOSD)
     C**     =    MRA per hospital stay
     C     SVALOW        MULT      XXHOSC        XTHOSC
     C     CAUNTS        MULT      XXHOSD        XTHOSD
     C     XTHOSC        ADD       XTHOSD        XPTOTA
     C*
     C     XPTOTA        IFLT      CATOTC
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   ELSE                                                   ELS LT TOTC
     C                   Z-ADD     CATOTC        XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   END                                                    END LT TOTC
     C*
     C                   ELSE                                                   ELS NE *ZERO
     C*
     C**      If there is NOT an MRA or Unit Value:
     C                   Z-ADD     CATOTC        XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C*
     C                   END                                                    END NE *ZERO
     C*
     C                   ELSE                                                   ELS 0 *IN60
     C*
     C** If code is NOT in the FMSVP File:
     C                   Z-ADD     CATOTC        XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C*
     C                   END                                                    END 0 *IN60
     C*
     C                   Z-ADD     XPTOTA        XPTOTP
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
     C                   MOVE      '002'         XPEOMB
     C*** EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
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
     C********** CAKY4     CHAINPCA04                54     DEL 03/19/93
     C     CAKYO         CHAIN     PCA0O                              54         ADD 03/19/93
     C*
     C     DUPCL2        TAG
     C*
     C     *IN54         IFEQ      '0'
     C*
     C*** BEGIN ADD *** 03/19/93 **************************************
     C********** CADSEQ    IFNE X4DSEQ
     C********** CADSEQ    OREQ X4DSEQ
     C********** CAEOB#    ANDNEX4EOB#
     C     CADSEQ        IFNE      OXDSEQ
     C     CADSEQ        OREQ      OXDSEQ
     C     CAEOB#        ANDNE     OXEOB#
     C*** END ADD ***** 03/19/93 **************************************
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
     C*********  CAKY4     READEPCA04                    54 DEL 03/19/93
     C     CAKYO         READE     PCA0O                                  54     ADD 03/19/93
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
     C                   MOVE      *BLANKS       XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE         ENDSR
     OPCA01     E            RCA01
