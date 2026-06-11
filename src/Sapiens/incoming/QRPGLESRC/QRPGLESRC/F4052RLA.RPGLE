     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4052RLA                                          *
     H*    DESCRIPTION: ADJUDICATION (08/01/94 RULES)                 *
     H*   This program is called from F0070R (Claim Adjudication)     *
     H*   and will perform the actual adjudication of the detail      *
     H*   claim records written by F0070R.                            *
     H*                                                               *
     H*  This Sub-program is intended to be used to calculate the     *
     H*  amount that is due a provider of services, by the carrier    *
     H*  of the Workers' Compensation insurance.                      *
     H*                                                               *
     H*  It should be used for any service provided on or after       *
     H*  08/01/94.                                                    *
     H*****************************************************************
     H*  CREATED: 9/15/94   C:05   PROJECT: LA008
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*  04/08/99 C:117 Support: MC095 Seq# 17568
     H*                 Added QQCO#, QQSUB#, QQFND to the *ENTRY PARMs
     H*                 and changed all occurences of UPCO#, UPSUB#,
     H*                 UPFND to the above mentioned fields.
     H*
     H*  09/04/98 C:31  Project: 95MAL
     H*                 Ntwk Disc:Lesser of FS/Chr
     H*                 1. Add update of CAMRAA
     H*
     H*****************************************************************
     H*  INDICATOR USAGE:                                            **
     H*****************************************************************
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMSVL1LA  IF   E           K DISK
     FFTABLLA   IF   E           K DISK
     FFDCAL7    UF   E           K DISK
     FFDCALO    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0O)
     FFACAPLA   O    E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK1)
     FFACAL3LA  IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK2)
     F*

     D*
     D QQS             S              5    DIM(30)                              DETAIL SEQ #
     D QQC             S              2    DIM(30)                              ERROR MESSAGES
     D QQF             S              6    DIM(50)                              FROM DATES
     D QCA             S             55    DIM(4) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
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

     I*
     I*  RENAME THE DUPLICATE FIELD NAMES.
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
     I*
     I* Record Format for HOSP2 Workfile FACAL3LA
     IPCAWK2
     I              CACO#                       P2CO#
     I              CASUB#                      P2SUB#
     I              CAFND                       P2FND
     I              CAFYR                       P2FYR
     I              CACASE                      P2CASE
     I              CACSCD                      P2CSCD
     I              CAFDAT                      P2FDAT
     I              CATDAT                      P2TDAT
     I              CARDAT                      P2RDAT
     I              CAIPOP                      P2IPOP
     I              CAFEIN                      P2FEIN
     I              CAMOD                       P2MOD
     I              CAPRTY                      P2PRTY
     I              CATRCD                      P2TRCD
     I              CASVCD                      P2SVCD
     I              CAPMOD                      P2PMOD
     I              CACPT4                      P2CPT4
     I              CAICD9                      P2ICD9
     I              CAANMN                      P2ANMN
     I              CAUNTS                      P2UNTS
     I              CATOTC                      P2TOTC
     I              CATOTA                      P2TOTA
     I              CATOTD                      P2TOTD
     I              CAEOMB                      P2EOMB
     I              CATOTP                      P2TOTP
     I              CADSEQ                      P2DSEQ
     I              CADELF                      P2DELF
     I              CAFLAG                      P2FLAG
     I              CACHK#                      P2CHK#
     I              CAEOB#                      P2EOB#
     I              CAPART                      P2PART
     I              CAP1                        P2P1
     I              CASS#                       P2SS#
     I              CAACDT                      P2ACDT
     I              CASTAT                      P2STAT
     I              CAUSER                      P2USER
     I              CAPGM                       P2PGM
     I              CATIME                      P2TIME
     I              CADT                        P2DT
     I              CAIUSR                      P2IUSR
     I              CAIDT                       P2IDT
     I              CADXR                       P2DXR
     I              CADCS#                      P2DCS#
     I              CALTR#                      P2LTR#
     I              CARCFL                      P2RCFL
     I              CAOTHR                      P2OTHR
     I              CANDIS                      P2NDIS
     I              CARDTP                      P2RDTP
     I              CABRSV                      P2BRSV
     I              CABRPM                      P2BRPM
     I              CABRPC                      P2BRPC
     I              CABRAM                      P2BRAM
     I              CABRUN                      P2BRUN
     I              CABRTC                      P2BRTC
     I              CAZIPA                      P2ZIPA
     I              CAAST                       P2AST
     I              CADIFF                      P2DIFF
     I              CABRPT                      P2BRPT
     I              CADAFL                      P2DAFL
     I              CAMRA                       P2MRA
     I              CABREC                      P2BREC
     I              CABRP                       P2BRP
     I              CATYPE                      P2TYPE

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
     C     CAKY1B        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    P2FEIN
     C                   KFLD                    P2MOD
     C                   KFLD                    P2PRTY
     C                   KFLD                    P2DSEQ
     C                   KFLD                    P2EOB#
     C                   KFLD                    P2CHK#
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
     C*** BEG ADD *** 04/08/99 ***************************************
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQSUB#            5 0
     C                   PARM                    QQFND             3 0
     C*** END ADD *** 04/08/99 ***************************************
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
     C                   Z-ADD     *ZEROS        PRDIEM            9 2
     C* PRDIEM is the variable field used to PER DIEM AMOUNT
     C* It will be used for both Per Diem codes and Per Hour codes
     C                   Z-ADD     *ZERO         XXBILL            9 2          Billed Charges
     C                   Z-ADD     *ZERO         XXNONC            9 2          Non Covered Charges
     C                   Z-ADD     *ZEROS        XXOVRP            3 2          Override Percentage
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDATE            7 0
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
     C     *LIKE         DEFINE    CABRTC        XPBRTC                         9.2
     C     *LIKE         DEFINE    CACPT4        XPCPT4                         5
     C     *LIKE         DEFINE    CADIFF        XPDIFF                         9.2
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
     C                   MOVE      ' '           XFIRST            1
     C                   MOVE      *BLANKS       QQTYPE            1
     C                   MOVE      *BLANKS       XXALOW            1
     C                   Z-ADD     *ZEROS        XXWORK            5 2
     C                   Z-ADD     *ZEROS        XXHOUR            5 2
     C                   MOVE      ' '           OPNFLG            1            File Open Flag
     C                   MOVE      ' '           OVRFLG            1            File Override Flag
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
     C** DO THE ADJUDICATION FOR EACH POSSIBLE DATE ENTERED IF AFTER 08/01/94
     C*
     C                   DO        50            S
     C                   MOVE      QQF(S)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C     QQF(S)        IFNE      *ZERO
     C     XXYMD         ANDGE     940801                                       ADD 09/15/94
     C     CAKY1         CHAIN     PCA01                              54
     C*
     C*+*+*+*+*+*+* Do Loop for Each Claim Detail Record +*+*+*+*+*+*+*+*+*+**
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
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
     C                   END                                                    END 1 *IN60
     C*
     C* * * IF OVERRIDE FLAG IS NOT EQUAL TO 'Y' THEN PROCESS THIS RECORD
     C* * * ELSE DO NOT ADJUDICATE.
     C     CSOVRD        IFNE      'Y'
     C*
     C*** CHECK FOR LEVEL BREAK (PROVIDER)
     C     SVFEIN        IFNE      CAFEIN
     C     SVMOD         ORNE      CAMOD
     C* Complete process of the bill: calculate payment on the covered charge
     C                   EXSR      HOSP2
     C                   Z-ADD     *ZERO         XXBILL
     C                   Z-ADD     *ZERO         XXNONC
     C                   END
     C*
     C* ===== NON-HOSPITAL TYPE ======================================
     C     CAPRTY        IFNE      '4'
     C                   GOTO      NXTRCD
     C                   END
     C*
     C* ===== HOSPITAL TYPE ==========================================
     C*
     C* If first time into program,
     C* or the first code after the workfile has been used
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the workfile is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACAL3LA
     C                   CLOSE     FACAPLA
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACAL3LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for MEDI/SURG Codes
     C                   CALL      'CRTFACLA'
     C                   PARM      'H'           QQTYPE
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACAL3LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACAL3LA
     C                   OPEN      FACAPLA
     C                   MOVE      '1'           OPNFLG
     C                   END
     C*
     C                   END                                                    END XFIRST EQ ' '
     C                   EXSR      HOSP1
     C*
     C                   ELSE                                                   ELSE CSOVRD NE Y
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM ON HOSPITAL.
     C*
     C     CAEOB#        IFEQ      *ZERO
     C                   MOVE      CASVCD        XXSVCD
     C                   MOVE      CACPT4        XXCPT4
     C                   MOVE      CAUNTS        XXUNTS
     C*
     C     CAKYO         CHAIN     PCA0O                              60
     C*
     C     DUPCL3        TAG
     C*
     C     *IN60         IFEQ      '0'
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
     C     CAKYO         READE     PCA0O                                  60
     C  N60              GOTO      DUPCL3
     C                   END                                                    END DSEQ NE OXDSEQ
     C                   END                                                    END 0 *IN60
     C                   END                                                    END CAEOB# = 0
     C*
     C     CAKY1A        CHAIN     PCA01                              60
     C     *IN60         IFEQ      '0'
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
     C     CATOTC        SUB       CATOTA        CATOTD
     C                   Z-ADD     CATOTA        CATOTP
     C                   END
     C                   Z-ADD     *ZEROS        CADIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        CABRTC                         ADD 09/15/94
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XCMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA01
     C                   END                                                    END 0 *IN60
     C*
     C                   END                                                    END CSOVRD NE Y
     C*
     C     NXTRCD        TAG
     C*
     C*+*+*+*+*+*+*+*+*+*+ Read Next Claim Detail Record +*+*+*+*+*+*+*+*+*+**
     C     CAKY1         READE     PCA01                                  54
     C                   END                                                    END DOWEQ 0 *IN54
     C*
     C                   END                                                    END QQF,S NE *ZERO
     C*
     C* Complete process of the bill: calculate payment on the covered charge
     C     OPNFLG        IFEQ      '1'
     C                   EXSR      HOSP2
     C                   Z-ADD     *ZERO         XXBILL
     C                   Z-ADD     *ZERO         XXNONC
     C                   MOVE      ' '           XFIRST
     C                   END                                                    END OPNFLG EQ '1'
     C*
     C                   END                                                    END DO 50
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
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACAL3LA
     C                   CLOSE     FACAPLA
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACAL3LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     0             XCMRA             9 2
     C*** END ADD *** 09/04/98 ****************************************
     C                   RETURN
     C                   MOVE      '1'           *INLR
     *********************************************************************
     *** HOSPITAL SUBROUTINE   SERVICE CODES                   H O S P 1 *
     *********************************************************************
     C     HOSP1         BEGSR
     C*
     C*-------------------------------------------------------------------
     C*-- Get the Service Code record ------------------------------------
     C                   Z-ADD     CASVCD        XXRVCD
     C                   MOVE      CACPT4        XXHCPC
     C                   Z-ADD     *ZEROS        XXUNIT
     C*
     C* If a hit in Service Code file, then this is a covered charge
     C     SVKY          SETGT     PSV01
     C     NXTSV1        TAG
     C                   READP     PSV01                                  60
     C     *IN60         IFEQ      '0'
     C     SVCO#         ANDEQ     CACO#
     C     SVCMSV        ANDEQ     XXCMSV
     C     SVSTAT        CABEQ     'I'           NXTSV1
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     SVALOW        XCMRA
     C*** END ADD *** 09/04/98 ****************************************
     C*
     C*    Keep running total of Billed charges
     C                   ADD       CATOTC        XXBILL
     C*
     C* Write record to workfile for further processing
     C                   MOVE      'H'           CATYPE
     C                   Z-ADD(H)  SVALOW        CAMRA
     C                   WRITE     PCAWK1
     C*
     C                   ELSE                                                    ELSE 0 *IN60
     C*
     C* If no hit in Service Code file, then this is a noncovered charge
     C*    Keep running total of Billed charges
     C                   ADD       CATOTC        XXBILL
     C*    Keep running total of noncovered charges
     C                   ADD       CATOTC        XXNONC
     C*
     C*    and will disallow this charge
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C     XXRVCD        IFEQ      274
     C     XXRVCD        OREQ      277
     C     XXRVCD        OREQ      299
     C     XXRVCD        ORGE      290
     C     XXRVCD        ANDLE     292
     C     XXRVCD        ORGE      500
     C     XXRVCD        ANDLE     539
     C     XXRVCD        ORGE      570
     C     XXRVCD        ANDLE     599
     C     XXRVCD        ORGE      540
     C     XXRVCD        ANDLE     559
     C     XXRVCD        ORGE      820
     C     XXRVCD        ANDLE     889
     C     XXRVCD        ORGE      900
     C     XXRVCD        ANDLE     919
     C     XXRVCD        ORGE      940
     C     XXRVCD        ANDLE     949
     C     XXRVCD        ORGE      960
     C     XXRVCD        ANDLE     989
     C                   MOVE      '212'         XPEOMB
     C* EOMB Code 212: This charge should be submitted on HCFA-1500 for reimb"
     C                   ELSE
     C                   MOVE      '242'         XPEOMB
     C* EOMB Code 242: This is a non-covered charge"
     C                   END
     C                   EXSR      UPDAT
     C*
     C                   END                                                     END 0 *IN60
     C*
     C*
     C     HOSP1E        ENDSR
     *********************************************************************
     *** UPDATE HOSPITAL CHARGES                               H O S P 2 *
     *********************************************************************
     C     HOSP2         BEGSR
     C*
     C                   Z-ADD     *ZEROS        XXWORK
     C                   Z-ADD     *ZEROS        XXHOUR
     C     *LOVAL        SETLL     PCAWK2
     C                   READ      PCAWK2                                 60
     C     *IN60         DOWEQ     '0'
     C*
     C     P2CPT4        IFEQ      'HCHBC'
     C     P2CPT4        OREQ      'HCFSC'
     C     P2UNTS        DIV(H)    60            XXWORK
     C                   ADD       XXWORK        XXHOUR
     C                   END                                                    END EQ 'HCHBC'
     C*
     C                   READ      PCAWK2                                 60
     C                   END
     C*
     C                   MOVE      *BLANKS       XXALOW
     C     XXHOUR        IFGT      0
     C     XXHOUR        ANDLT     8
     C                   MOVE      'Y'           XXALOW
     C                   END                                                    END GT 0
     C*
     C     *LOVAL        SETLL     PCAWK2
     C                   READ      PCAWK2                                 60
     C     *IN60         DOWEQ     '0'
     C*
     C*
     C*  Apply rule based on Inpatient vs. Outpatient
     C*
     C* Inpatient (POS EQ 1)
     C* =====================
     C     P2IPOP        IFEQ      '1'
     C     P2IPOP        OREQ      '6'
     C     P2IPOP        OREQ      '8'
     C     P2IPOP        OREQ      'D'
     C     P2IPOP        OREQ      'O'
     C     P2IPOP        OREQ      'R'
     C*
     C* IF CONTINUOUS HOME CARE(HCHBC OR HCFSC) UNITS MUST BE A MIN
     C* OF 8 HOURS
     C     P2CPT4        IFEQ      'HCHBC'
     C     P2CPT4        OREQ      'HCFSC'
     C     XXALOW        IFEQ      'Y'
     C                   Z-ADD     P2TOTC        XPTOTD
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '213'         XPEOMB
     C* EOMB Code 213: Continuous home care must be provided for a min of 8hrs
     C*                for reimbursement"
     C                   EXSR      UPDAT2
     C                   GOTO      HOSP2E
     C                   END                                                    END XXALOW =Y
     C                   END                                                    END = HCHBC
     C*
     C* IF AUTOMATIC OUTLIERS PAY COVERED BILLED CHARGES AT A 15%
     C* DISCOUNT
     C     P2CPT4        IFEQ      'OUTLR'
     C     P2TOTC        MULT      .85           XPTOTA
     C     P2TOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     XPTOTA        XPBRTC                         ADD 09/15/94
     C                   MOVE      '210'         XPEOMB
     C* EOMB Code 210: This procedure code is used for all outliers"
     C                   EXSR      UPDAT2
     C                   GOTO      HOSP2E
     C                   END                                                    END = OUTLR
     C*
     C     P2CPT4        IFEQ      'HCHBC'
     C     P2CPT4        OREQ      'HCFSC'
     C     P2UNTS        IFGT      1
     C     P2UNTS        DIV(H)    60            XXHOUR
     C     P2MRA         MULT(H)   XXHOUR        PRDIEM
     C                   ELSE                                                   ELSE SVUNTS GT 1
     C                   Z-ADD     P2MRA         PRDIEM
     C                   END                                                    END SVUNTS GT 1
     C                   ELSE
     C     P2MRA         MULT      P2UNTS        PRDIEM
     C                   END
     C*
     C*      If billed charges > per diem amount,
     C*           Per diem amount - Noncovered charges = Payment amount
     C*      If billed charges < per diem amount,
     C*           Billed Charges - Noncovered charges = Payment amount
     C     XXBILL        IFGT      PRDIEM
     C     P2CPT4        IFEQ      'HCHBC'
     C     P2CPT4        OREQ      'HCFSC'
     C                   Z-ADD     PRDIEM        XPTOTA
     C                   ELSE
     C     PRDIEM        SUB       XXNONC        XPTOTA
     C                   END
     C     XPTOTA        IFGE      0
     C                   Z-ADD     0             XXNONC
     C                   END
     C     XPTOTA        IFLT      0
     C                   Z-ADD     0             XPTOTA
     C                   END
     C     P2TOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     XPTOTA        XPBRTC                         ADD 09/15/94
     C*
     C                   ELSE                                                   ELSE TOTC GT PRDIEM
     C*
     C     P2CPT4        IFEQ      'HCHBC'
     C     P2CPT4        OREQ      'HCFSC'
     C                   Z-ADD     P2TOTC        XPTOTA
     C                   ELSE
     C     XXBILL        SUB       XXNONC        XPTOTA
     C                   END
     C     XPTOTA        IFGE      0
     C                   Z-ADD     0             XXNONC
     C                   END
     C     XPTOTA        IFLT      0
     C                   Z-ADD     0             XPTOTA
     C                   END
     C     P2TOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     XPTOTA        XPBRTC                         ADD 09/15/94
     C                   END                                                    END TOTC GT PRDIEM
     C*
     C*
     C                   ELSE                                                   ELSE CAIPOP = 1
     C*
     C*
     C* Outpatient (POS NE 1)
     C* =====================
     C*     (Billed Charges - Noncovered Charges) * .90 = Payment amount
     C     P2TOTC        MULT      .90           XPTOTA
     C     P2TOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     XPTOTA        XPBRTC                         ADD 09/15/94
     C                   END                                                    END CAIPOP = 1
     C*
     C*
     C                   EXSR      UPDAT2
     C                   READ      PCAWK2                                 60
     C                   END
     C*
     C     HOSP2E        ENDSR
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
     C     CAKYO         CHAIN     PCA0O                              60
     C*
     C     DUPCL2        TAG
     C*
     C     *IN60         IFEQ      '0'
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
     C     CAKYO         READE     PCA0O                                  60
     C  N60              GOTO      DUPCL2
     C                   END
     C                   END
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** ONLY PROCESS THE RECORD IF NOT A OVERRIDE TYPE.
     C*
     C*
     C     CAKY1A        CHAIN     PCA01                              60
     C     CSOVRD        IFNE      'Y'
     C     CACHK#        ANDEQ     *ZERO
     C     CAEOB#        ANDEQ     *ZERO
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPTOTD        CATOTD
     C                   Z-ADD     XPTOTP        CATOTP
     C                   Z-ADD     XPDIFF        CADIFF                         ADD 09/15/94
     C                   Z-ADD     XPBRTC        CABRTC                         ADD 09/15/94
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
     C  N60              UPDATE    PCA01
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** CLEAR THE WORK SAVE FIELDS.
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      *BLANKS       XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE         ENDSR
     *********************************************************************
     *** UPDATE2 SUBROUTINE                                     U P D AT2*
     *********************************************************************
     C     UPDAT2        BEGSR
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
     C     P2EOB#        IFEQ      *ZERO
     C                   Z-ADD     P2FEIN        CAFEIN
     C                   MOVE      P2SVCD        XXSVCD
     C                   MOVE      P2CPT4        XXCPT4
     C                   MOVE      P2UNTS        XXUNTS
     C*
     C     CAKYO         CHAIN     PCA0O                              60
     C*
     C     DUPCL4        TAG
     C*
     C     *IN60         IFEQ      '0'
     C*
     C     P2DSEQ        IFNE      OXDSEQ
     C     P2DSEQ        OREQ      OXDSEQ
     C     P2EOB#        ANDNE     OXEOB#
     C*
     C     P2EOB#        IFEQ      *ZEROS
     C     P2CHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     P2DSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C                   END
     C                   ELSE
     C*
     C     CAKYO         READE     PCA0O                                  60
     C  N60              GOTO      DUPCL4
     C                   END
     C                   END
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*  Using FACAPLA record, chain to FDCAP record to update
     C*  Only process the record if not an override type
     C*
     C     CAKY1B        CHAIN     PCA01                              60
     C     CSOVRD        IFNE      'Y'
     C     CACHK#        ANDEQ     *ZERO
     C     CAEOB#        ANDEQ     *ZERO
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPTOTD        CATOTD
     C                   Z-ADD     XPTOTP        CATOTP
     C                   Z-ADD     XPDIFF        CADIFF                         ADD 09/15/94
     C                   Z-ADD     XPBRTC        CABRTC                         ADD 09/15/94
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
     C  N60              UPDATE    PCA01
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** CLEAR THE WORK SAVE FIELDS.
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      *BLANKS       XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE2        ENDSR
     OPCA01     E            RCA01
**  QCA QCMDEXC TABLE
DLTOVR FILE(FACAL3LA)
OVRDBF FILE(FACAL3LA)  TOFILE(QTEMP/FACAL3LA)
DLTOVR FILE(FACAPLA)
OVRDBF FILE(FACAPLA)  TOFILE(QTEMP/FACAPLA)
