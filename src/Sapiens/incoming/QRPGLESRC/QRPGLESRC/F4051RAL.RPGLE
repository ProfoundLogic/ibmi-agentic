     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4051RAL                                          *
     H*    DESCRIPTION: ADJUDICATION (03/01/93 RULES)                 *
     H*   This program is called from F0070R (Claim Adjudication)     *
     H*   and will perform the actual adjudication of the detail      *
     H*   claim records written by F0070R.                            *
     H*                                                               *
     H*  This Sub-program is intended to be used to calculate the     *
     H*  amount that is due a provider of services, by the carrier    *
     H*  of the Workers' Compensation insurance.                      *
     H*                                                               *
     H*  It should be used for any service provided on or after       *
     H*  03/01/93.                                                    *
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
     H*  11/18/97 C:86  Project: MC145
     H*         CHECK ALL DATE FIELDS AND ROUTINES TO SEE IF THEY ARE
     H*         CORRECT FOR 7 DIGIT FILE FIELDS & 8 DIGIT SCREEN FIELDS
     H*                                                               *
     H*  04/18/95 C:20  REQ/PROJ: ALMNT
     H*                 1. IF A HOSPITAL IS MARKED WIT AN '*': REIMBURSEMENT
     H*                    WILL BE FOR THE TOTAL CHARGED IF THIS AMOUNT IS
     H*                    LESS THAN THE PER DIEM.  IF TOTAL CHARGES ARE
     H*                    GREATER THAN THE PER DIEM: IF A CAP AMOUNT EXISTS
     H*                    PAY THE GREATER OF THE PER DIEM OR THE CAP AMOUNT
     H*                    IF NO CAP AMOUNT PAY THE PER DIEM.
     H*  03/21/95 C:20  REQ/PROJ: ALMNT
     H*                 1. MODIFICATION TO NOT PAY THE CHARGED AMOUNT IF IT
     H*                    IS GREATER THAN THE PER DIEM AMOUNT AND P2FL02 =*
     H*
     H*  12/14/94 C:20  REQ/PROJ: 133/AL015
     H*                 1. IF HOSPITAL IS A STARRED HOSPITAL. (PSFL02 EQ *).
     H*                    PAY THE LESSER OF CHARGE OR PER DIEM
     H*
     H*  04/19/94 C:20  REQ/PROJ: ALMNT
     H*                 WHEN GIVING ERROR MESSAGE THAT NO PERCENTAGE IFFORMATION
     H*                 HAS BEEN SET UP DISALLOW CHARGES
     H*
     H*  12/21/93 C:20  REQ/PROJ: ALMNT
     H*                 IF NO PERCENTAGE INFORMATION HAS BEEN SET UP FOR THIS
     H*                 PROVIDER GIVE MESSAGE
     H*
     H*  12/02/93 C:20  REQ/PROJ: 50040/AL011
     H*                 FIX TO PAY HOSPITAL UP TO THE PER DIEM FOR INPATIENT
     H*                 HOSPITAL.  CODES WILL BE PAID AS FOLLOWS:
     H*                  A) (PER DIEM * UNITS) > CHARGES
     H*                     PAY CHARGES AND PUT DIFFERENCE IN CADIFF
     H*                  B) CHARGES > (PER DIEM * UNITS)
     H*                     1) NO CAP
     H*                        PAY PER DIEM * UNITS
     H*                     2) CAP
     H*                        PAY GREATER OF (PER DIEM *UNITS) OR CAP
     H*
     H*  10/20/93 C:20  REQ/PROJ: ALMNT
     H*                 Fix to initialize field XXTOTP
     H*
     H*  09/28/93 C:20  REQ/PROJ: FSMNT
     H*                 1. HOSP - FIX TO PAY LESSER OF MRA OR PROVIDER CHARGE
     H*                    FOR OUTPATIENT CODES
     H*
     H*  09/10/93 C:20  REQ/PROJ: FSMNT
     H*                 1. OVR  - FIX TO SECTION OF CODE THAT CALCULATES
     H*                    THE TOTAL PAID AMOUNT FOR CODES THAT ARE OVERRIDE
     H*                    CLAIM STATUS, ONLY CALCULATE AMOUNT IF CATOTD,
     H*                    CAOTHR, CANDIS, CATOTA, CATOTP ALL EQUAL 0
     H*
     H*  07/29/93 C:20  Project: 47AAC
     H*                 1. CHANGE TO LOAD STATES MRA VALUE INTO CABRTC
     H*                    TO BE USED IN DISCOUNTING PROGRAM
     H*
     H*  07/22/93 C:05  Project: AL006
     H*                 1. ADDED CODE TO CHECK FOR PROVIDER/SERVICE
     H*                    CODE IN ALABAMA PROVIDER AUXILIARY FILE.
     H*                    IF FOUND THEN PROCESS:
     H*                    P2FL01 = A (PER DIEM)
     H*                    P2AM02 NE ZEROS CALCULATE PAYMENT FROM 'CAP'
     H*                    P2AM02 EQ ZEROS CALCULATE PER DIEM
     H*                    P2FL01 = P (OUT-PATIENT PERCENTAGE)
     H*                    CALCULATE ON OUT-PATIENT PERCENTAGE
     H*
     H*  03/19/93 C:05  Project: FS104
     H*                 1. Added Err Msg# 79 for Service Date > System Date
     H*
     H*  03/19/93 C:05  Project: FS102
     H*                 1. Remove Provider Modifier from key that checks for
     H*                    "Possible Duplicate".  Required adding new logical
     H*                    FDCALO
     H*                 2. Rmvd reference to FDCAL4/PCA04 (wasn't used anymore)
     H*
     H*  01/25/93 C:20  Project: FS054
     H*                 1. ADDED CODE TO CHECK FOR POSSIBLE DUPLICATE
     H*                    CHARGE IF THE CLAIM STATUS CODE IS AN
     H*                    OVERRIDE TYPE
     H*
     H*  01/21/93 C:20   PROJ: FS075                                  *
     H*                  1. CHANGED ADJUDICATION DO LOOP FROM 42 TO 50*
     H*
     H*  12/28/92 C:05   PROJ: FS081                                  *
     H*                  1. Added new parm to *ENTRY: Specialty Code  *
     H*                  2. Changed usage of SPCD from FMPDP to passed parm
     H*                                                               *
     H*  12/11/92C:20    PROJ:                                        *
     H*                  1.IF NO HIT IN FMSVP PAY AT 100% NOT 80%     *
     H*                                                               *
     H*  07/16/92 LJD   Proj#: FS010                                  *
     H*                 1.  Revised effective date selection to be a  *
     H*                     range of 01/01/01 - 07/31/91 so that      *
     H*                     only charges with effective date within   *
     H*                     this range will processed thru this program
     H*                                                               *
     H*  05/31/92 LJD    PROJ:FS001  (per TLB)                        *
     H*                  1.Chg SVKEY keyfield UNITS to always chain   *
     H*                     with *ZEROS                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                            **
     H*****************************************************************
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMSVL1AL  IF   E           K DISK
     FFTABLAL   IF   E           K DISK
     FFDCAL7    UF   E           K DISK
     F*** BEGIN ADD *** 03/19/93 **************************************
     FFDCALO    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0O)
     F*** END ADD ***** 03/19/93 **************************************
     F*** BEGIN ADD *** 07/22/93 **************************************
     FFMP2LAL   IF   E           K DISK
     FFMP2L1AL  IF   E           K DISK
     F                                     RENAME(PP201:PP202)
     F*** END ADD *** 07/22/93 ****************************************
     F*

     D*
     D QQS             S              5    DIM(30)                              DETAIL SEQ #
     D QQC             S              2    DIM(30)                              ERROR MESSAGES
     D QQF             S              6    DIM(50)                              FROM DATES
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

     I*
     I*  RENAME THE DUPLICATE FIELD NAMES.
     I*
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
     C*** BEGIN ADD *** 07/22/93 **************************************
     C*
     C*  ALABAMA PROVIDER AUXILIARY FILE
     C     P2KY          KLIST
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    XXCMSV
     C                   KFLD                    CAFDAT
     C*
     C     P2KY1         KLIST
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C*** END ADD *** 07/22/93 ****************************************
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
     C                   PARM                    QQSPCD            3            ADD 12/28/92
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
     C                   Z-ADD     *ZEROS        XXALOW            9 2
     C                   Z-ADD     *ZEROS        XXOVRP            3 2
     C*
     C*** BEGIN ADD *** 03/19/93 *****************************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*** BEGIN CHANGE ********* 11/18/97 ********************************
     C**********           MOVELSRDTO     SYDATE  60
     C                   MOVEL     SRDTO         SYDATE            7 0
     C*** END   CHANGE ********* 11/18/97 ********************************
     C*** END ADD ***** 03/19/93 *****************************************
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
     C     *LIKE         DEFINE    CACPT4        XPCPT4                         5
     C     *LIKE         DEFINE    CAEOMB        XPEOMB                         3
     C     *LIKE         DEFINE    CAPMOD        XPPMOD                         2
     C     *LIKE         DEFINE    CATOTA        XPTOTA                         9.2
     C     *LIKE         DEFINE    CATOTD        XPTOTD                         9.2
     C     *LIKE         DEFINE    CATOTP        XPTOTP                         9.2
     C     *LIKE         DEFINE    CADIFF        XPDIFF                         9.2 ADD 12/02/93
     C     *LIKE         DEFINE    CABRTC        XPBRTC                         9.2
     C     *LIKE         DEFINE    CACPT4        XXCPT4                         5
     C     *LIKE         DEFINE    CAFDAT        XXDATE                         6.0
     C     *LIKE         DEFINE    CASVCD        XXSVCD                         4.0
     C     *LIKE         DEFINE    CAUNTS        XXUNTS                         5.2
     C     *LIKE         DEFINE    CAFDAT        XXYMD                          6.0
     C*
     C     *LIKE         DEFINE    CAFEIN        SXFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         SXMOD                          3.0
     C                   Z-ADD     *ZEROS        XXDEIM            9 2          ADD 12/02/93
     C                   Z-ADD     *ZEROS        XXCAP             9 2          ADD 12/02/93
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
     C*** BEGIN CHANGE ********* 11/18/97 ********************************
     C***********          MOVELQQF,S     XXDATE
     C                   MOVE      QQF(S)        XXDATE
     C*** END   CHANGE ********* 11/18/97 ********************************
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C     QQF(S)        IFNE      *ZERO
     C     XXYMD         ANDGE     930301
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
     C                   Z-ADD     1             Q                 3 0
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
     C                   MOVE      *ZEROS        CADIFF                          ADD 12/02/93
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
     C*** BEGIN ADD *** 07/21/93 **************************************
     C*
     C* VERIFY SERVICE DATE AGAINST PROVIDER EFFECTIVE DATE
     C*
     C     P2KY1         CHAIN     PP202                              61
     C     *IN61         IFEQ      '0'
     C     CAFDAT        IFGE      P2EFDT
     C*
     C* CHECK ALABAMA PROV. AUX. FILE FOR SERVICE CODE
     C*
     C     P2KY          SETGT     PP201
     C                   READP     PP201                                  60
     C     *IN60         IFEQ      '0'
     C     CAFEIN        ANDEQ     P2FEIN
     C     CAMOD         ANDEQ     P2MOD
     C     XXCMSV        ANDEQ     P2SV01
     C*
     C* IF SERVICE CODE MARKED A = AMOUNT PER DIEM
     C*
     C     P2FL01        IFEQ      'A'
     C*** BEGIN CHG *** 12/14/94 ***********************************************
     C     P2FL02        IFEQ      '*'
     C*** BEGIN ADD *** 03/21/95 ***********************************************
     C     SVUNTS        IFGT      1
     C     P2AM01        MULT      SVUNTS        XXDEIM
     C                   ELSE
     C                   Z-ADD     P2AM01        XXDEIM
     C                   END
     C*
     C     CATOTC        IFLE      XXDEIM
     C*** END ADD *** 03/21/95 *************************************************
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     CATOTC        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXDEIM        XPBRTC
     C*** BEGIN ADD *** 03/21/95 ***********************************************
     C                   ELSE
     C*
     C*** BEGIN ADD *** 04/18/95 ***********************************************
     C* PER VERNON DUKE AT AL WORKERS' COMPENSATION
     C* 1. IF TOTAL CHARGES ARE GREATER THAN THE PER DIEM
     C*    A. IF STOP LOSS CAP EXISTS PAY THE STOP LOSS CAP AS LONG AS DOES
     C*       NOT GO BELOW PER DIEM
     C*    B. IF NO STOP LOSS CAP EXISTS PAY THE PER DEIM
     C*
     C* IF NO CAP AMOUNT PAY PER DIEM * UNITS
     C*
     C     P2AM02        IFEQ      *ZEROS
     C*
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXDEIM        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXDEIM        XPBRTC
     C*
     C                   ELSE                                                   ELSE P2AM02 EQ 0
     C*
     C* IF THE CAP AMOUNT IS NOT ZERO PAY THE GREATER OF PER DIEM * UNITS OR CAP
     C*
     C     100           SUB       P2AM02        XXCAMT            7 4
     C     XXCAMT        DIV       100           XXCAMT
     C     CATOTC        MULT      XXCAMT        XXCAP
     C*
     C     XXDEIM        IFGT      XXCAP
     C                   Z-ADD     XXDEIM        XPTOTA
     C                   ELSE
     C                   Z-ADD     XXCAP         XPTOTA
     C                   END
     C*
     C                   Z-ADD     *ZEROS        XPDIFF
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   END                                                    END P2AM02
     C**********           Z-ADD*ZEROS    XPDIFF
     C**********           Z-ADDXXDEIM    XPTOTA
     C********** CATOTC    SUB  XPTOTA    XPTOTD
     C**********           Z-ADDXPTOTA    XPTOTP
     C**********           Z-ADDXXDEIM    XPBRTC
     C*** END ADD *** 04/18/95 *************************************************
     C*
     C                   END
     C*** END ADD *** 03/21/95 *************************************************
     C                   ELSE
     C*** END ADD *** 12/14/94 *************************************************
     C*** BEGIN CHG *** 12/02/93 **************************************
     C*
     C* CHECK TO SEE IF PRE DIEM * UNITS IS GREATER THAN CHARGES
     C*
     C     SVUNTS        IFGT      1
     C     P2AM01        MULT      SVUNTS        XXDEIM
     C                   ELSE
     C                   Z-ADD     P2AM01        XXDEIM
     C                   END
     C*
     C     CATOTC        IFLE      XXDEIM
     C*
     C     XXDEIM        SUB       CATOTC        XPDIFF
     C                   Z-ADD     CATOTC        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXDEIM        XPBRTC
     C*
     C                   ELSE                                                   ELSE CATOTC LE
     C*
     C* IF NO CAP AMOUNT PAY PER DIEM * UNITS
     C*
     C     P2AM02        IFEQ      *ZEROS
     C*
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXDEIM        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXDEIM        XPBRTC
     C*
     C                   ELSE                                                   ELSE P2AM02 EQ 0
     C*
     C* IF THE CAP AMOUNT IS NOT ZERO PAY THE GREATER OF PER DIEM * UNITS OR CAP
     C*
     C     100           SUB       P2AM02        XXCAMT            7 4
     C     XXCAMT        DIV       100           XXCAMT
     C     CATOTC        MULT      XXCAMT        XXCAP
     C*
     C     XXDEIM        IFGT      XXCAP
     C                   Z-ADD     XXDEIM        XPTOTA
     C                   ELSE
     C                   Z-ADD     XXCAP         XPTOTA
     C                   END
     C*
     C                   Z-ADD     *ZEROS        XPDIFF
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   END                                                    END P2AM02
     C*
     C                   END                                                    END CATOTC LE
     C*** BEGIN DEL *** 12/02/93 **************************************
     C*
     C* P2AM02 IFNE ZERO CALCULATE PAYMENT FROM 'CAP' AMOUNT
     C*
     C**********           Z-ADD*ZEROS    XXTOTP           ADD 10/20/93
     C********** P2AM02    IFNE *ZEROS
     C********** 100       SUB  P2AM02    XXCAMT  74
     C********** XXCAMT    DIV  100       XXCAMT
     C********** CATOTC    MULT XXCAMT    XXTOTP  92
     C**********           END
     C*
     C* IF P2AM02 EQ ZEROS CALCULATE PER DIEM AMOUNT
     C*
     C********** SVUNTS    IFGT 1
     C********** P2AM01    MULT SVUNTS    XXPAMT  92
     C**********           ELSE
     C**********           Z-ADDP2AM01    XXPAMT
     C**********           END
     C********** XXPAMT    IFLT XXTOTP
     C**********           Z-ADDXXTOTP    XXPAMT
     C**********           END
     C********** CATOTC    IFLT XXPAMT
     C**********           Z-ADDCATOTC    XPTOTA
     C**********           ELSE
     C**********           Z-ADDXXPAMT    XPTOTA
     C**********           END
     C********** CATOTC    SUB  XPTOTA    XPTOTD
     C**********           Z-ADDXPTOTA    XPTOTP
     C**********           Z-ADDXXPAMT    XPBRTC
     C*** END DEL *** 12/02/93 ****************************************
     C                   END                                                    END P2FL02 EQ '*'
     C                   ELSE                                                   ELSE EQ 'A'
     C*
     C* IF P2FL01 EQ P (OUTPATIENT %) CALCULATE PAYMENT ON PERCENTAGE
     C*
     C     P2FL01        IFEQ      'P'
     C     P2AM01        DIV       100           XXPERC            7 4
     C     CATOTC        MULT      XXPERC        XXTOTP            9 2
     C     SVUNTS        IFGT      1
     C     XXTOTP        MULT      SVUNTS        XXTOTP
     C                   END
     C*** BEGIN ADD *** 09/28/93 **************************************
     C     CATOTC        IFLT      XXTOTP
     C                   Z-ADD     CATOTC        XXTOTP
     C                   END
     C*** END ADD *** 09/28/93 ****************************************
     C                   Z-ADD     XXTOTP        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTP        XPBRTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 12/02/93
     C                   END
     C                   END
     C*
     C* IF SERVICE CODE NOT FOUND IN AL PROV. AUX. FILE PROCESS
     C* PAY ZERO
     C                   ELSE
     C*** END ADD *** 07/21/93 ****************************************
     C     CATOTC        MULT      0.00          XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 12/02/93
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another."
     C                   END                                                    END *IN60
     C                   ELSE                                                   ELSE P2EFDT
     C     CATOTC        MULT      0.00          XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTP        XPBRTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 12/02/93
     C                   END
     C*** BEGIN ADD *** 12/21/93 **************************************
     C                   ELSE                                                   ELSE NO RECORD
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AL'          QQC(Q)
     C*** Err Msg AL: "No CODE information has been found for this
     C*                provider."
     C                   END                                                    END EQ *ZEROS
     C*** BEGIN ADD *** 04/19/94 ***********************************************
     C     CATOTC        MULT      0.00          XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   Z-ADD     *ZEROS        XPDIFF
     C*** END ADD *** 04/19/94 *************************************************
     C*** END ADD *** 12/21/93 ****************************************
     C                   END
     C*
     C*** IF NOT FOUND IN FMSVP...PAY AT 0%
     C                   ELSE
     C***********CATOTC    MULT .80       XPTOTA           DEL 12/11/92
     C     CATOTC        MULT      0.00          XPTOTA                         ADD 12/11/92
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 12/02/93
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 202: "This procedure is included in the basic allowance
     C***            of another."
     C*
     C** END OF GOOD CHAIN OF SERVICE CODE
     C*
     C                   END
     C*
     C*** MUST UPDATE RECORD HERE
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
     C     XPTOTP        ANDGT     *ZEROS
     C     XPEOMB        ANDEQ     *BLANK
     C                   MOVE      '202'         XPEOMB
     C*** EOMB 202: "Charges exceed Maximum Allowance found in reimburse manual"
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
     C                   Z-ADD     XPBRTC        CABRTC
     C                   Z-ADD     XPDIFF        CADIFF                         ADD 12/02/93
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
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      *BLANKS       XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE         ENDSR
     OPCA01     E            RCA01
