     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*  PROGRAM: F4052RGA                                            *
     H*  DESCRIPTION: ADJUDICATION (07/01/95 RULES)                   *
     H*  This program is called from F0070R (Claim Adjudication)      *
     H*  and will perform the actual adjudication of the detail       *
     H*  claim records written by F0070R.                             *
     H*                                                               *
     H*  This Sub-program is intended to be used to calculate the     *
     H*  amount that is due a provider of services, by the carrier    *
     H*  of the Workers' Compensation insurance.                      *
     H*                                                               *
     H*  It should be used for any service provided on or after       *
     H*  07/01/95.                                                    *
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
     H*  07/23/96  C:32 PROJ: GA008                                   *
     H*                 ADDED ENDING DATE RANGE FOR RECORDS PROCESSED *
     H*
     H*  03/27/97  C:59 PROJ: GA007                                   *
     H*                 ADDED ICD9 CODES WITH PREFIX 'P'              *
     H*                 ADDED NEW SUBROUTINE HOSP2 TO DEAL WITH ICD9  *
     H*                 CODES, RADIOLOGY CODES AND PHYSICAL MEDICINE  *
     H*****************************************************************
     H*  INDICATOR USAGE:                                            **
     H*****************************************************************
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMSVL1GA  IF   E           K DISK
     FFTABLGA   IF   E           K DISK
     FFDCAL7    UF   E           K DISK
     FFDCALO    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0O)
     F*

     D*
     D QQS             S              5    DIM(30)                              DETAIL SEQ #
     D QQC             S              2    DIM(30)                              ERROR MESSAGES
     D QQF             S              6    DIM(50)                              FROM DATES
     D OUT1            S              5    DIM(11) CTDATA PERRCD(1)             Outlier/LOS Threshol
     D OUT2            S              3  0 DIM(11) ALT(OUT1)
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

     I* NOTE:  Made OUT1 (DRG) a 5A field to match LOKUP by CACPT4 a 5A field
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
     I              CACPT4                      OXCPT4

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
     C*** BEGIN ADD *** 03/27/97 ***************************************
     C*
     C     CAKYO1        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    XXSVCD
     C*
     C*** END ADD ***** 03/27/97 ***************************************
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
     C                   Z-ADD     *ZEROS        OUTCHG            9 2          Outlier Charge
     C                   Z-ADD     *ZEROS        OUTDAY            3 0          Outlier Days
     C                   Z-ADD     *ZEROS        OUTPAY            9 2          Outlier Payment
     C                   Z-ADD     *ZEROS        PRDIEM            9 2          Per Diem
     C                   Z-ADD     *ZEROS        A                 3 0
     C                   Z-ADD     *ZEROS        Q                 3 0
     C                   Z-ADD     *ZEROS        S                 3 0
     C                   Z-ADD     *ZEROS        XXALOW            9 2
     C                   Z-ADD     *ZEROS        XXLOST            3 0          Outlier LOS Threshol
     C                   Z-ADD     *ZEROS        XXOTPC            5 4
     C                   Z-ADD     *ZEROS        XXOVRP            3 2
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
     C     *LIKE         DEFINE    CACHK#        PTCHK#                         7.0
     C     *LIKE         DEFINE    CADSEQ        PTDSEQ                         5.0
     C     *LIKE         DEFINE    CAEOB#        PTEOB#                         9.0
     C     *LIKE         DEFINE    CAFEIN        PTFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         PTMOD                          3.0
     C     *LIKE         DEFINE    CAPRTY        PTPRTY                         1
     C     *LIKE         DEFINE    CACHK#        SVCHK#                         7.0
     C     *LIKE         DEFINE    CADSEQ        SVDSEQ                         5.0
     C     *LIKE         DEFINE    CAEOB#        SVEOB#                         9.0
     C     *LIKE         DEFINE    CAFEIN        SVFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         SVMOD                          3.0
     C     *LIKE         DEFINE    CAPRTY        SVPRTY                         1
     C     *LIKE         DEFINE    CAUNTS        SVUNTS                         5.2
     C     *LIKE         DEFINE    CSKEY         TABKEY                         10
     C     *LIKE         DEFINE    CABRTC        XPBRTC                         9.2
     C     *LIKE         DEFINE    CACPT4        XPCPT4                         5
     C     *LIKE         DEFINE    CADIFF        XPDIFF                         9.2
     C     *LIKE         DEFINE    CAEOMB        XPEOMB                         3
     C     *LIKE         DEFINE    CAPMOD        XPPMOD                         2
     C     *LIKE         DEFINE    CATOTA        XPTOTA                         9.2
     C     *LIKE         DEFINE    CATOTD        XPTOTD                         9.2
     C     *LIKE         DEFINE    CATOTP        XPTOTP                         9.2
     C     *LIKE         DEFINE    CACPT4        XXCPT4                         5
     C     *LIKE         DEFINE    CAFDAT        XXDATE                         6.0
     C     *LIKE         DEFINE    CATOTP        XXMRA                          9.2
     C     *LIKE         DEFINE    CASVCD        XXSVCD                         4.0
     C     *LIKE         DEFINE    CAUNTS        XXUNTS                         5.2
     C     *LIKE         DEFINE    CAFDAT        XXYMD                          6.0
     C*
     C     *LIKE         DEFINE    CAFEIN        SXFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         SXMOD                          3.0
     C*
     C     *LIKE         DEFINE    CAUNTS        ZZUNTS                         5.2
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
     C** DO ADJUDICATION FOR EACH POSSIBLE DATE ENTERED IF AFTER 07/01/95
     C*
     C                   DO        50            S
     C                   MOVE      QQF(S)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C     QQF(S)        IFNE      *ZERO
     C     XXYMD         ANDGE     950701
     C     XXYMD         ANDLT     960701                                       ADD 07/23/96
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
     C*** BEGIN ADD *** 03/27/97 *************************************
     C*  IF CPT4 EQUALS ICD9 CODES, RADIOLOGY CODES, PHYSICAL MEDICINE
     C*  CALL HOSP2 ELSE CALLS HOSP
     C*
     C     QQSPCD        IFEQ      'ASC'
     C     CACPT4        IFGE      'P0000'
     C     CACPT4        ANDLE     'P9999'
     C     CACPT4        ORGE      '70010'
     C     CACPT4        ANDLE     '78999'
     C     CACPT4        ORGE      '97010'
     C     CACPT4        ANDLE     '97999'
     C                   EXSR      HOSP2
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '98'          QQC(Q)                         INVALID CLM STS CODE
     C*** Err Msg 98: "Invalid poocedure code, see data processing'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   MOVE      '001'         XPEOMB
     C*** EOMB 001: "Service is not reimburseable under the Workers'
     C***            Compensation Program."
     C*
     C                   EXSR      UPDAT
     C                   ENDIF
     C                   ELSE
     C*
     C*** END ADD ***** 03/27/97 *************************************
     C                   EXSR      HOSP
     C                   ENDIF                                                  ADD 3/27/97
     C*
     C                   ELSE                                                   OVERRIDE TYPE
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM ON HOSPITAL.
     C*
     C     CAEOB#        IFEQ      *ZERO
     C                   Z-ADD     CASVCD        XXSVCD
     C                   MOVE      CACPT4        XXCPT4
     C                   Z-ADD     CAUNTS        XXUNTS
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
     C     CAKYO         READE     PCA0O                                  54
     C  N54              GOTO      DUPCL3
     C                   END
     C                   END
     C                   END
     C*
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
     C     CATOTC        SUB       CATOTA        CATOTD
     C                   Z-ADD     CATOTA        CATOTP
     C                   END
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
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
     C                   Z-ADD     0             XXMRA
     C*** END ADD *** 09/04/98 ****************************************
     C                   RETURN
     C                   MOVE      '1'           *INLR
     *********************************************************************
     *** HOSPITAL SUBROUTINE   SERVICE CODES                     H O S P *
     *********************************************************************
     C     HOSP          BEGSR
     C*
     C* For Inpatient Services (POS = 1)
     C     CAIPOP        IFEQ      '1'
     C*
     C                   Z-ADD     CASVCD        XXRVCD
     C                   MOVEL     CACPT4        XXHCPC
     C                   Z-ADD     *ZEROS        XXUNIT
     C*
     C* Get DRG (Service Code) value
     C     SVKY          SETGT     PSV01                                        GET SERV CODE
     C     NXTSV1        TAG
     C                   READP     PSV01                                  60     RECORD
     C     *IN60         IFEQ      '0'
     C     SVCO#         ANDEQ     CACO#
     C     SVCMSV        ANDEQ     XXCMSV
     C     SVSTAT        CABEQ     'I'           NXTSV1
     C*
     C* Determine if this is an Outlier DRG
     C                   Z-ADD     1             A
     C     CACPT4        LOOKUP    OUT1(A)                                60
     C*
     C* Outlier DRG
     C     *IN60         IFEQ      '1'
     C                   MOVE      OUT2(A)       XXLOST                         LOS Threshold
     C*
     C*  Step 1:  Pg. 101 - PER DIEM
     C*      Divide the total amount of billed charges
     C*      by the number of days covered by the bill.
     C                   Z-ADD     *ZEROS        PRDIEM
     C*
     C* If no Units is indicated, assume 1 unit/day
     C     CAUNTS        IFEQ      0
     C                   Z-ADD     1             ZZUNTS
     C                   ELSE
     C                   Z-ADD     CAUNTS        ZZUNTS
     C                   END
     C*
     C     CATOTC        DIV       ZZUNTS        PRDIEM
     C*
     C*  Step 2:  Pg. 101 - OUTLIER DAYS
     C*      Subtract the outlier length of stay threshold for that DRG
     C*                by the number of days covered by the bill.
     C     ZZUNTS        SUB       XXLOST        OUTDAY
     C*
     C*  Step 3:  Pg. 101 - OUTLIER CHARGES
     C*      Multiply the number of Outlier Days from Step 2 times the
     C*                Per Diem for the bill from Step 1
     C     OUTDAY        MULT      PRDIEM        OUTCHG
     C*
     C*  Step 4:  Pg. 101 - OUTLIER PAYMENT
     C*      Multiply the Outlier Charges from Step 3 by forty percent (40%)
     C*                Then, add the 40% to the DRG average.  Pay that amount.
     C     OUTCHG        MULT      .40           OUTPAY
     C*
     C*  We will pay up to the Charge amount in the normal charge's paid amount
     C*  based on the sum of the DRG average in the FMSVP file + the 40%
     C*  calculated.  We will handle any extra by using the FSMRA detail
     C*  charge record which will be created in F0070R from the XPDIFF field
     C*  in the normal charge's record:
     C     SVALOW        ADD       OUTPAY        XXMRA
     C     CATOTC        IFLT      XXMRA
     C                   Z-ADD     CATOTC        XPTOTP
     C     XXMRA         SUB       CATOTC        XPDIFF
     C                   ELSE
     C                   Z-ADD     XXMRA         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   END
     C                   Z-ADD     XXMRA         XPBRTC
     C*
     C* Else not an Outlier DRG
     C                   ELSE                                                        ELSE OUTLIER
     C*
     C     CATOTC        IFLT      SVALOW
     C                   Z-ADD     CATOTC        XPTOTP
     C                   ELSE
     C                   Z-ADD     SVALOW        XPTOTP
     C                   END
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     SVALOW        XPBRTC
     C*
     C                   END                                                         END OUTLIER
     C*
     C*
     C*** IF NOT FOUND IN FMSVP...PAY AT 0.00
     C                   ELSE
     C     CATOTC        MULT      0.00          XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '001'         XPEOMB
     C*** EOMB 001: "Service is not reimburseable under the Workers'
     C***            Compensation Program."
     C*
     C** END OF GOOD CHAIN OF SERVICE CODE
     C*
     C                   END
     C* For Outpatient Services (POS ne 1)
     C                   ELSE                                                   ELSE CAIPOP = '1'
     C*
     C* Get Outpatient Service code value
     C                   Z-ADD     CASVCD        XXRVCD
     C                   MOVEL     'OUTPT'       XXHCPC
     C                   Z-ADD     *ZEROS        XXUNIT
     C     SVKY          SETGT     PSV01                                        GET SERV CODE
     C     NXTSV2        TAG
     C                   READP     PSV01                                  60     RECORD
     C     *IN60         IFEQ      '0'
     C     SVCO#         ANDEQ     CACO#
     C     SVCMSV        ANDEQ     XXCMSV
     C     SVSTAT        CABEQ     'I'           NXTSV2
     C*
     C     SVOTPC        DIV       100           XXOTPC
     C     CATOTC        MULT      XXOTPC        XPTOTP
     C                   Z-ADD     XPTOTP        XPBRTC
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   END                                                     END *IN60 = 0
     C                   END                                                    END CAIPOP = '1'
     C*
     C*** MUST UPDATE RECORD HERE
     C                   Z-ADD     XPTOTP        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   EXSR      UPDAT
     C*
     C     HOSPE         ENDSR
     C*
     C*** BEGIN ADD *** 03/27/97 *****************************************
     *********************************************************************
     *** HOSPITAL SUBROUTINE   ICD9 CODES                        H O S P *
     *********************************************************************
     C     HOSP2         BEGSR
     C*
     C* PAGE 112 ,#A
     C* PAYMENT FOR OUTPATIENT SURGICAL SERVICES AND ASSOCIATED GOODS
     C* RENDERED ON OR AFTER JULY 1, 1995 BY A HOSPITAL, ANBULATORY
     C* SURGICAL CENTER, OR OTHER OUTPATIENT FACILITY WHICH BILLS FOR ITS
     C* SERVICES ON HCFA FORM UB-92.
     C*
     C                   Z-ADD     *ZEROS        XXMRA
     C                   Z-ADD     CASVCD        XXRVCD
     C                   MOVEL     CACPT4        XXHCPC
     C                   Z-ADD     *ZEROS        XXUNIT
     C*
     C     SVKY          SETGT     PSV01                                        GET SERV CODE
     C     NXTSV3        TAG
     C                   READP     PSV01                                  60     RECORD
     C     *IN60         IFEQ      '0'
     C     SVCO#         ANDEQ     CACO#
     C     SVCMSV        ANDEQ     XXCMSV
     C     SVSTAT        CABEQ     'I'           NXTSV3
     C                   Z-ADD     SVALOW        XXMRA
     C                   ENDIF
     C*
     C** RADIOLOGY CODES PAYS 110% OF MRA
     C*
     C     CACPT4        IFGE      '70000'
     C     CACPT4        ANDLE     '79999'
     C*
     C* NON-SURGICAL RADIOLOGY SERVICES -- RANGE HAVING (-27) TECH COMPONENT
     C* TECHNICAL COMPONENT IS ENTERED IN MRA AMOUNT
     C*
     C     XXMRA         MULT      1.1           XXMRA
     C*
     C                   ENDIF                                                  END CPT4 70010
     C* ====================================================================
     C* PAGE 113, #6
     C*      Other billing and Payment Requirements:
     C* All facilities billiing for services on UB-92 forms are required
     C* to include all appropriate CPT and HCPCS codes in Field 44 of the
     C* form.  The payment amounts described here in above shall cover all
     C* goods and services reflected on the bill.
     C*
     C     CACPT4        IFGE      '70010'
     C     CACPT4        ANDLE     '78320'
     C     CACPT4        ORGE      '97010'
     C     CACPT4        ANDLE     '97750'
     C*
     C                   MOVEL     'P0000'       XXCPT4
     C     CAKYO         SETLL     PCA0O
     C     CAKYO1        READE     PCA0O                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C     OXCPT4        IFGE      'P0000'
     C     OXCPT4        ANDLE     'P9999'
     C     CAFDAT        ANDEQ     OXFDAT
     C     CADSEQ        ANDNE     OXDSEQ
     C     OXTOTP        ANDGE     *ZEROS
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   MOVE      '005'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      HOSP2E
     C*
     C                   ENDIF                                                  END P0000
     C     CAKYO1        READE     PCA0O                                  54
     C                   ENDDO                                                  END *IN54
     C*
     C                   ENDIF                                                  END CACPT4
     C*
     C* ====================================================================
     C*  Page 112, #3
     C*  No additional payment shall be required even if other procedures
     C*  are listed in field 81 of form UB-92, except as follows.  Facilities
     C*  may receive additional payment in excess ofhte fees listed in the
     C*  ICD-9 Listing if (1) more than one principal procedure is performed
     C*  during the operative session.
     C*
     C*  Surgical Services -- Only pay first 'P' code billed per date of service
     C*
     C     CACPT4        IFGE      'P0000'
     C     CACPT4        ANDLE     'P9999'
     C*
     C                   MOVEL     'P0339'       XXCPT4
     C     CAKYO         SETLL     PCA0O
     C*
     C     CAKYO1        READE     PCA0O                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C     OXCPT4        IFGE      'P0000'
     C     OXCPT4        ANDLE     'P9999'
     C     OXDSEQ        ANDNE     CADSEQ
     C     OXTOTP        ANDNE     *ZEROS
     C     CAFDAT        ANDEQ     OXFDAT
     C*
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FN'          QQC(Q)                         INVALID CLM STS CODE
     C*** Err Msg FN: "Multiple procedures have been billed, adjustor to review'
     C                   END
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceds number of allowable procedures in reimbursement
     C***            manual".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      HOSP2E
     C                   ENDIF                                                  END P0000
     C*
     C     CAKYO1        READE     PCA0O                                  54
     C                   ENDDO                                                  END *IN54
     C*
     C                   ENDIF                                                  END CACPT4
     C*
     C* ====================================================================
     C*
     C*** MUST UPDATE RECORD HERE
     C*
     C     CATOTC        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTA
     C  N54              Z-ADD     CATOTC        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XPTOTA        XPTOTP
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   EXSR      UPDAT
     C*
     C     HOSP2E        ENDSR
     C*
     C*** END ADD ***** 03/27/97 *****************************************
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
     C     CAKYO         CHAIN     PCA0O                              54         ADD
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
     C                   Z-ADD     XPBRTC        CABRTC
     C                   Z-ADD     XPDIFF        CADIFF
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
     C                   Z-ADD     XXMRA         CAMRAA
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
     C                   Z-ADD     *ZERO         XPBRTC
     C                   Z-ADD     *ZERO         XPDIFF
     C                   MOVE      *BLANKS       XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE         ENDSR
     OPCA01     E            RCA01
**  OUTLIER DIAGNOSTIC RELATED GROUPS (DRGS)
009  127
217  014
236  021
249  011
415  023
458  032
460  010
462  055
468  020
486  067
487  052
