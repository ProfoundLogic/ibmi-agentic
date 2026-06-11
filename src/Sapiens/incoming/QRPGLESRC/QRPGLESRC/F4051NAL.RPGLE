     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
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
     H*
     H*  04/29/05 C:079 SUPPORT: MC025 46607
     H*                 ADD CACODE TO RENAME OF FDCAP TO PREVENT
     H*                 LOADING THE WRONG VALUE
     H*                                                               *
     H*  09/14/04 C:079 SUPPORT: MC071 43816                          *
     H*                 EXEC STOP LOSS ONLY FOR PAY LESSER FACILITIES *
     H*                                                               *
     H*  09/30/03 C:079 SUPPORT: MC072 39109                          *
     H*                 EXEC STOP LOSS FOR ALL PER DIEM BILLS         *
     H*                                                               *
     H*  08/14/03 C:079 SUPPORT: MC072 38540                          *
     H*                 1. LOAD CORRECT AMOUNT INTO XCMRA             *
     H*                                                               *
     H*  10/21/02 C:079 PROJECT: MC010 34605                          *
     H*                 1. CALCULATE PER DIEM FOR TOTAL BILL          *
     H*                                                               *
     H*  08/08/02 C:079 PROJECT: ALMNT 32464                          *
     H*                 1. CHANGE EOB CODE TO BE CORRECT WHEN SERV    *
     H*                    CODE NOT FOUND                             *
     H*                 2. ADD SUB TO GET TOTAL CHARGES IF CHANGED    *
     H*                                                               *
     H*  04/12/02 C:062 SUPPORT: MC071 31852                          *
     H*                 CALCULATE CADIFF.                             *
     H*                                                               *
     H*  04/02/02 C:079 SUPPORT: MC071 31852                          *
     H*                 USE ALLOWANCE FROM FMP2PAL FOR CAMRAA WHEN    *
     H*                 APPROPRIATE INSTEAD OF FMSVPAL                *
     H*                                                               *
     H*  09/25/01 C:062 PROJECT: FSMNT                                *
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.   *
     H*                                                               *
     H*  12/27/99 C:117 Project: MC010  Sup Seq#: 21094               *
     H*                 Taking DUUNTS out of DUKY1; Poss Duplicate    *
     H*                 problem.                                      *
     H*                                                               *
     H*  06/08/99 C:79 PROJ: AL035                                    *
     H*                Modified to work with Provider Bills           *
     H*                                                               *
     H*  04/08/99 C:117 Support: MC095 Seq# 17568                     *
     H*                 Added QQCO#, QQSUB#, QQFND to the *ENTRY PARMs*
     H*                 and changed all occurences of UPCO#, UPSUB#,  *
     H*                 UPFND to the above mentioned fields.          *
     H*                                                               *
     H*  09/04/98 C:31  Project: 95MAL                                *
     H*                 Ntwk Disc:Lesser of FS/Chr                    *
     H*                 1. Add update of CAMRAA                       *
     H*                                                               *
     H*  11/18/97 C:86  Project: MC145                                *
     H*         CHECK ALL DATE FIELDS AND ROUTINES TO SEE IF THEY ARE *
     H*         CORRECT FOR 7 DIGIT FILE FIELDS & 8 DIGIT SCREEN FIELDS
     H*                                                               *
     H*  04/18/95 C:20  REQ/PROJ: ALMNT                               *
     H*                 1. IF A HOSPITAL IS MARKED WIT AN '*': REIMBURSEMENT
     H*                    WILL BE FOR THE TOTAL CHARGED IF THIS AMOUNT IS
     H*                    LESS THAN THE PER DIEM.  IF TOTAL CHARGES ARE
     H*                    GREATER THAN THE PER DIEM: IF A CAP AMOUNT EXISTS
     H*                    PAY THE GREATER OF THE PER DIEM OR THE CAP AMOUNT
     H*                    IF NO CAP AMOUNT PAY THE PER DIEM.         *
     H*  03/21/95 C:20  REQ/PROJ: ALMNT                               *
     H*                 1. MODIFICATION TO NOT PAY THE CHARGED AMOUNT IF IT
     H*                    IS GREATER THAN THE PER DIEM AMOUNT AND P2FL02 =*
     H*                                                               *
     H*  12/14/94 C:20  REQ/PROJ: 133/AL015                           *
     H*                 1. IF HOSPITAL IS A STARRED HOSPITAL. (PSFL02 EQ *).
     H*                    PAY THE LESSER OF CHARGE OR PER DIEM       *
     H*                                                               *
     H*  04/19/94 C:20  REQ/PROJ: ALMNT                               *
     H*                 WHEN GIVING ERROR MESSAGE THAT NO PERCENTAGE IFFORMATION
     H*                 HAS BEEN SET UP DISALLOW CHARGES              *
     H*                                                               *
     H*  12/21/93 C:20  REQ/PROJ: ALMNT                               *
     H*                 IF NO PERCENTAGE INFORMATION HAS BEEN SET UP FOR THIS
     H*                 PROVIDER GIVE MESSAGE                         *
     H*                                                               *
     H*  12/02/93 C:20  REQ/PROJ: 50040/AL011                         *
     H*                 FIX TO PAY HOSPITAL UP TO THE PER DIEM FOR INPATIENT
     H*                 HOSPITAL.  CODES WILL BE PAID AS FOLLOWS:     *
     H*                  A) (PER DIEM * UNITS) > CHARGES              *
     H*                     PAY CHARGES AND PUT DIFFERENCE IN CADIFF  *
     H*                  B) CHARGES > (PER DIEM * UNITS)              *
     H*                     1) NO CAP                                 *
     H*                        PAY PER DIEM * UNITS                   *
     H*                     2) CAP                                    *
     H*                        PAY GREATER OF (PER DIEM *UNITS) OR CAP*
     H*                                                               *
     H*  10/20/93 C:20  REQ/PROJ: ALMNT                               *
     H*                 Fix to initialize field XXTOTP                *
     H*                                                               *
     H*  09/28/93 C:20  REQ/PROJ: FSMNT                               *
     H*                 1. HOSP - FIX TO PAY LESSER OF MRA OR PROVIDER*CHARGE
     H*                    FOR OUTPATIENT CODES                       *
     H*                                                               *
     H*  09/10/93 C:20  REQ/PROJ: FSMNT                               *
     H*                 1. OVR  - FIX TO SECTION OF CODE THAT CALCULATES
     H*                    THE TOTAL PAID AMOUNT FOR CODES THAT ARE OVERRIDE
     H*                    CLAIM STATUS, ONLY CALCULATE AMOUNT IF CAMRA ,
     H*                    CAOTHR, CANDIS, CATOTA, CAPAID ALL EQUAL 0 *
     H*                                                               *
     H*  07/29/93 C:20  Project: 47AAC                                *
     H*                 1. CHANGE TO LOAD STATES MRA VALUE INTO CABRTC*
     H*                    TO BE USED IN DISCOUNTING PROGRAM          *
     H*                                                               *
     H*  07/22/93 C:05  Project: AL006                                *
     H*                 1. ADDED CODE TO CHECK FOR PROVIDER/SERVICE   *
     H*                    CODE IN ALABAMA PROVIDER AUXILIARY FILE.   *
     H*                    IF FOUND THEN PROCESS:                     *
     H*                    P2FL01 = A (PER DIEM)                      *
     H*                    P2AM02 NE ZEROS CALCULATE PAYMENT FROM 'CAP'
     H*                    P2AM02 EQ ZEROS CALCULATE PER DIEM         *
     H*                    P2FL01 = P (OUT-PATIENT PERCENTAGE)        *
     H*                    CALCULATE ON OUT-PATIENT PERCENTAGE        *
     H*                                                               *
     H*  03/19/93 C:05  Project: FS104                                *
     H*                 1. Added Err Msg# 79 for Service Date > System*Date
     H*                                                               *
     H*  03/19/93 C:05  Project: FS102                                *
     H*                 1. Remove Provider Modifier from key that checks for
     H*                    "Possible Duplicate".  Required adding new logical
     H*                    FDCALO                                     *
     H*                 2. Rmvd reference to FDCAL4/PCA04 (wasn't used*anymore)
     H*                                                               *
     H*  01/25/93 C:20  Project: FS054                                *
     H*                 1. ADDED CODE TO CHECK FOR POSSIBLE DUPLICATE *
     H*                    CHARGE IF THE CLAIM STATUS CODE IS AN      *
     H*                    OVERRIDE TYPE                              *
     H*                                                               *
     H*  01/21/93 C:20   PROJ: FS075                                  *
     H*                  1. CHANGED ADJUDICATION DO LOOP FROM 42 TO 50*
     H*                                                               *
     H*  12/28/92 C:05   PROJ: FS081                                  *
     H*                  1. Added new parm to *ENTRY: Specialty Code  *
     H*                  2. Changed usage of SPCD from FMPDP to passed*parm
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
     FFDCALH    UF   E           K DISK
     F*** BEGIN ADD *** 03/19/93 **************************************
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0O)
     F*** END ADD ***** 03/19/93 **************************************
     F*** BEGIN ADD *** 07/22/93 **************************************
     FFMP2LAL   IF   E           K DISK
     FFMP2L1AL  IF   E           K DISK
     F                                     RENAME(PP201:PP202)
     FFDDUL     IF   E           K DISK
     FFDDUL1    IF   E           K DISK
     F                                     RENAME(PDU01:PDU02)
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDDEL4    UF A E           K DISK
     FFAEOL4    IF   E           K DISK
     FFDMGL1    UF A E           K DISK
     FFDMGL5    IF   E           K DISK
     F                                     RENAME(PMG01:PMG02)
     F*** END ADD *** 07/22/93 ****************************************
     F*

     D*
     D QQS             S              5    DIM(999)                             DETAIL SEQ #
     D QQC             S              2    DIM(999)                             ERROR MESSAGES
     D*
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
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     I*
     I*  RENAME THE DUPLICATE FIELD NAMES.
     I*
     I*** BEGIN ADD *** 03/19/93 **************************************
     IPCA0O
     I              CACO#                       OXCO#
     I              CAFND                       OXFND
     I              CAFYR                       OXFYR
     I              CACASE                      OXCASE
     I              CACSCD                      OXCSCD
     I              CAFDAT                      OXFDAT
     I              CATDAT                      OXTDAT
     I              CARDAT                      OXRDAT
     I              CAIPOP                      OXIPOP
     I              CAFEIN                      OXFEIN
     I              CAMOD                       OXMOD
     I              CAPRTY                      OXPRTY
     I              CATRCD                      OXTRCD
     I              CASVCD                      OXSVCD
     I              CAPMOD                      OXPMOD
     I              CACPT4                      OXCPT4
     I              CAICD9                      OXICD9
     I              CATOTA                      OXTOTA
     I              CAEOMB                      OXEOMB
     I              CADSEQ                      OXDSEQ
     I              CADELF                      OXDELF
     I              CAFLAG                      OXFLAG
     I              CACHK#                      OXCHK#
     I              CAEOB#                      OXEOB#
     I              CAPART                      OXPART
     I              CAP1                        OXP1
     I              CASS#                       OXSS#
     I              CAACDT                      OXACDT
     I              CASTAT                      OXSTAT
     I              CAUSER                      OXUSER
     I              CAPGM                       OXPGM
     I              CATIME                      OXTIME
     I              CADT                        OXDT
     I              CAIUSR                      OXIUSR
     I              CAIDT                       OXIDT
     I              CADXR                       OXDXR
     I              CADCS#                      OXDCS#
     I              CALTR#                      OXLTR#
     I              CARCFL                      OXRCFL
     I              CASUB#                      OXSUB#
     I              CAOTHR                      OXOTHR
     I              CANDIS                      OXNDIS
     I              CARDTP                      OXRDTP
     I              CABRSV                      OXBRSV
     I              CABRPM                      OXBRPM
     I              CABRPC                      OXBRPC
     I              CABRAM                      OXBRAM
     I              CABRUN                      OXBRUN
     I              CABRTC                      OXBRTC
     I              CAZIPA                      OXZIPA
     I              CAAST                       OXAST
     I              CADIFF                      OXDIFF
     I              CAIPGM                      OXIPGM
     I              CACHRG                      OXTOTC
     I              CAMRA                       OXTOTD
     I              CAOTH2                      OXOTH2
     I              CAPAID                      OXTOTP
     I              CARCN                       OXRCN
     I              CALIN#                      OXLIN#
     I              CARULE                      OXRULE
     I              CAOT1C                      OXOT1C
     I              CAOT2C                      OXOT2C
     I              CAMRAA                      OXMRAA
     I              CAITIM                      OXITIM
     I              CAMNAA                      OXMNAA
     I*** BEG ADD *** 04/29/05 ****************************************
     I              CACODE                      OXCODE
     I*** END ADD *** 04/29/05 ****************************************
     I*
     IPDU02
     I              DUCO#                       D1CO#
     I              DUSUB#                      D1SUB#
     I              DUFND                       D1FND
     I              DUFYR                       D1FYR
     I              DUCASE                      D1CASE
     I              DUSS#                       D1SS#
     I              DUACDT                      D1ACDT
     I              DUEOB#                      D1EOB#
     I              DUDSEQ                      D1DSEQ
     I              DURVCD                      D1RVCD
     I              DUDESC                      D1DESC
     I              DURATE                      D1RATE
     I              DUUNTS                      D1UNTS
     I              DUNOCH                      D1NOCH
     I              DUSTAT                      D1STAT
     I              DUUSER                      D1USER
     I              DUPGM                       D1PGM
     I              DUTIME                      D1TIME
     I              DUDT                        D1DT
     I              DUIUSR                      D1IUSR
     I              DUIPGM                      D1IPGM
     I              DUITIM                      D1ITIM
     I              DUIDT                       D1IDT
     I              DUPOS                       D1POS

     C*** BEG ADD *** 08/08/02 ****************************************
     C*
     C     CAKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*** END ADD *** 08/08/02 ****************************************
     C*
     C     CAKY1A        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXYMD
     C                   KFLD                    SVDSEQ
     C*
     C     CAKY1Z        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXYMD
     C                   KFLD                    HLDSEQ
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
     C                   KFLD                    KKDSEQ
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
     C*** BEG ADD *** 08/08/02 ****************************************
     C     P2KY2         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C                   KFLD                    XXCMSV
     C                   KFLD                    EOFRDT
     C*
     C     P2KY3         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C                   KFLD                    XXCMSV
     C*** END ADD *** 08/08/02 ****************************************
     C*
     C*  DUMMY KEY USED FOR RELEASE OF ALL CA RECORDS
     C*
     C     DUMMY         KLIST
     C                   KFLD                    QQCO#
     C*
     C     CAKYH         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    KKYMD
     C*
     C     CAKYH1        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*
     C     DUKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     DUKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    DURVCD
     C                   KFLD                    DURATE
     C*** BEG DEL *** 12/27/99 ****************************************
     C***********          KFLD           DUUNTS
     C*** END DEL *** 12/27/99 ****************************************
     C*
     C     DEKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    KKDSEQ
     C                   KFLD                    KKESRC
     C*
     C*  EOMB CODE FILE MASTER (FDDEL4)
     C     DEKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     EOKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*
     C     EBKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    EBKEY
     C* Adjudication Message File (PMG01)
     C*
     C     MGKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    KKDSEQ
     C*
     C* Adjudication Message File (PMG02)
     C*
     C     MGKY2         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    KKDSEQ
     C                   KFLD                    XXMSG#
     C                   KFLD                    XXRCN
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQSS#             9 0
     C                   PARM                    QQACDT            8 0          (MMDDYY ORD)
     C                   PARM                    QQSPCD            3            ADD 12/28/92
     C                   PARM                    QQEOB#            9 0
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
     C*** BEGIN CHG *** 09/25/01 **************************************
     C**********           Z-ADD*ZEROS    XXOVRP  32
     C                   Z-ADD     *ZEROS        XXOVRP            5 4
     C*** END   CHG *** 09/25/01 **************************************
     C                   MOVE      *ZEROS        QQC
     C                   MOVE      *ZEROS        QQS
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
     C     *LIKE         DEFINE    DUUNTS        SVUNTS                         5.2
     C     *LIKE         DEFINE    CSKEY         TABKEY                         10
     C     *LIKE         DEFINE    DURATE        XPCPT4                         5
     C     *LIKE         DEFINE    CAEOMB        XPEOMB                         3
     C     *LIKE         DEFINE    CAPMOD        XPPMOD                         2
     C     *LIKE         DEFINE    CATOTA        XPTOTA                         9.2
     C     *LIKE         DEFINE    CAMRA         XPTOTD                         9.2
     C     *LIKE         DEFINE    CAPAID        XPTOTP                         9.2
     C     *LIKE         DEFINE    CADIFF        XPDIFF                         9.2 ADD 12/02/93
     C     *LIKE         DEFINE    CABRTC        XPBRTC                         9.2
     C     *LIKE         DEFINE    DURATE        XXCPT4                         5
     C     *LIKE         DEFINE    CAFDAT        XXDATE                         6.0
     C     *LIKE         DEFINE    DURVCD        XXSVCD                         4.0
     C     *LIKE         DEFINE    DUUNTS        XXUNTS                         5.2
     C     *LIKE         DEFINE    CAFDAT        XXYMD                          6.0
     C     *LIKE         DEFINE    CARCN         XXRCN
     C     *LIKE         DEFINE    DENOT1        XXNOT1
     C     *LIKE         DEFINE    MGMSG#        XXMSG#
     C     *LIKE         DEFINE    MGMSEQ        XXMSEQ
     C     *LIKE         DEFINE    CAEOB#        XXEOB#
     C     *LIKE         DEFINE    CAACDT        XXACDT
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C*** BEG ADD *** 10/21/02 ****************************************
     C     *LIKE         DEFINE    CAPAID        X1DIEM
     C     *LIKE         DEFINE    CAPAID        SMDIEM
     C*** END ADD *** 10/21/02 ****************************************
     C*
     C     *LIKE         DEFINE    CAFEIN        SXFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         SXMOD                          3.0
     C*
     C     *LIKE         DEFINE    CAFDAT        KKYMD
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    DEESRC        KKESRC
     C*
     C*** BEG ADD *** 08/08/02 ****************************************
     C     *LIKE         DEFINE    EOCHRG        SMCHRG
     C*** END ADD *** 08/08/02 ****************************************
     C*** BEGIN ADD *** 09/14/04 ***********************************************
     C     *LIKE         DEFINE    EOCHRG        X1LOSS
     C*** END   ADD *** 09/14/04 ***********************************************
     C*
     C                   Z-ADD     *ZEROS        XXDEIM            9 2          ADD 12/02/93
     C                   Z-ADD     *ZEROS        XXCAP             9 2          ADD 12/02/93
     C*** BEG ADD *** 10/21/02 ****************************************
     C                   Z-ADD     *ZEROS        X1CAP             9 2
     C*** END ADD *** 10/21/02 ****************************************
     C*
     C***************************************************************
     C*
     C     QQRTCD        CABEQ     'P'           TERM                           PRE-OPEN FILES
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
     C*
     C* Find the recon number.  If this is a recon.
     C     EOKY          CHAIN     PEO01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EORCN         XXRCN
     C*
     C                   ELSE
     C                   Z-ADD     *ZERO         XXRCN
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*** BEG ADD *** 10/21/02 ****************************************
     C*
     C* SUM UP CHARGES FOR ALL RECONS AND COMPARE TO THRESHOLD
     C*
     C                   EXSR      TOTCHG
     C*** END DEL *** 10/21/02 ****************************************
     C*** BEG ADD *** 08/08/02 ****************************************
     C*
     C* IS THIS A THRESHOLD PROVIDER
     C*
     C                   Z-ADD     *ZEROS        XXRVCD
     C                   MOVEL     'THRSH'       XXHCPC
     C                   MOVE      *ZEROS        XXUNIT
     C*** BEG DEL *** 10/21/02 ****************************************
     C*
     C********************
     C**SUM*UP*CHARGES*FOR ALL RECONS AND COMPARE TO THRESHOLD
     C********************
     C******************** EXSR TOTCHG
     C*** END DEL *** 10/21/02 ****************************************
     C*
     C                   SETOFF                                       16
     C     P2KY2         SETGT     PP201
     C     P2KY3         READPE    PP201                                  99
     C     *IN99         IFEQ      *OFF
     C     SMCHRG        ANDGT     P2AM03
     C                   SETON                                        16
     C     100           SUB       P2AM02        P2AM02
     C     P2AM02        DIV       100           ##PERC            9 4
     C                   ENDIF
     C*** END ADD *** 08/08/02 ****************************************
     C*
     C                   Z-ADD     *ZEROS        KKYMD
     C*
     C     CAKYH         SETLL     PCA01
     C     CAKYH1        READE     PCA01                                  54
     C*
     C*+*+*+*+*+*+* Do Loop for Each Claim Detail Record +*+*+*+*+*+*+*+*+*+**
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
     C*
     C                   EXCEPT    RCA01
     C*
     C     CAFDAT        IFGE      930301
     C     CARCN         ANDEQ     XXRCN
     C*
     C                   Z-ADD     CAFDAT        XXYMD
     C                   Z-ADD     CADSEQ        KKDSEQ
     C*
     C     DUKY          CHAIN     PDU01                              60
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     DUUNTS        SVUNTS
     C                   MOVEL     DURATE        XPCPT4
     C                   MOVE      CAPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C*
     C*** BEGIN ADD *** 03/19/93 *************************************
     C* If the Service Date is greater than the system date, give err message
     C     CAFDAT        IFGT      SYDATE
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '79'          QQC(Q)
     C*** Err Msg 79: "Service Date entered is greater than today's date
     C*                (system date)."
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
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '97'          QQC(Q)                         INVALID CLM STS CODE
     C*** Err Msg 97: "Invalid Claim Status Code, See Programmer"
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
     C     DUKY1         CHAIN     PDU02                              54
      *
     C     *IN54         DOWEQ     '0'
      *
     C     DUDSEQ        IFNE      D1DSEQ
      *
     C                   Z-ADD     D1DSEQ        KKDSEQ
      *
     C     CAKYO         CHAIN     PCA0O                              55
      *
     C     *IN55         IFEQ      '0'
     C     CAFDAT        ANDEQ     OXFDAT
     C     CAFEIN        ANDEQ     OXFEIN
     C     OXFLAG        ANDNE     'P'
      *
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C                   MOVE      *ON           *IN54
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C*
     C                   END
     C*
     C                   END
     C*
     C  N54DUKY1         READE     PDU02                                  54                3
     C*
     C                   ENDDO                                                  *IN54 DOWEQ
     C*
     C     CAKY1A        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C*** IF OVERRIDE TYPE WE MUST STILL OUTPUT THE ALLOWED/DISALLOWED ETC.
     C*** BEGIN ADD *** 09/10/93 **************************************
     C     CAMRA         IFEQ      0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CATOTA        ANDEQ     0
     C     CAPAID        ANDEQ     0
     C*** END ADD *** 09/10/93 ****************************************
     C                   Z-ADD     0             XPTOTA
     C*
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CACHRG        MULT      XXOVRP        XPTOTA
     C                   END
     C*
     C     CACHRG        SUB       XPTOTA        CAMRA
     C                   Z-ADD     XPTOTA        CAPAID
     C                   END
     C*                    MOVE *ZEROS    CADIFF            ADD 12/02/93
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XCMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C*** BEGIN ADD *** 04/12/02 **************************************
     C                   Z-ADD     XPDIFF        CADIFF
     C*** END   ADD *** 04/12/02 **************************************
     C                   UPDATE    PCA01
     C                   END
     C*
     C                   END
     C*
     C                   ENDIF                                                  CAFDAT IFGE
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
     C     CAKYH1        READE     PCA01                                  54      GET NEXT RCD
     C*                                                      TO PROCESS
     C                   ENDDO
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
     C* Write adjudication messages to FDMGP
     C                   EXSR      MSG
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     0             XCMRA             9 2
     C*** END ADD *** 09/04/98 ****************************************
     C                   RETURN
     C                   MOVE      '1'           *INLR
     *********************************************************************
     *** UPDATE FDDEP WITH EOMB                                E O M B   *
     *********************************************************************
     C     EOMB          BEGSR
     C*
     C* Find the EOMB description
     C                   MOVEL(P)  XPEOMB        EBKEY
     C*
     C     EBKY          CHAIN     PEB01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     EBDESC        XXNOT1
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       XXNOT1
     C*
     C                   ENDIF                                                  *IN60=ON
     C*
     C                   MOVE      'A'           KKESRC
     C*
     C* Determine if an EOMB already exists for this DSEQ number
     C     DEKY          CHAIN     PDE01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      XPEOMB        DEEOB
     C                   MOVEL(P)  XXNOT1        DENOT1
     C*
     C* Update file maintenance fields
     C                   MOVE      Q1USER        DEUSER
     C                   MOVE      Q1PGM         DEPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*
     C                   MOVEL     SRDTO         DEDT
     C                   Z-ADD     DSTIME        DETIME
     C*
     C                   UPDATE    PDE01
     C*
     C                   ELSE
     C*
     C* No EOMB exists so find the next sequence number and add to file
     C     DEKY1         SETGT     PDE02
     C     DEKY1         READPE    PDE02                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C                   ADD       1             DESEQ#
     C*
     C                   ELSE
     C                   Z-ADD     1             DESEQ#
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   MOVE      QQCO#         DECO#
     C                   MOVE      QQSUB#        DESUB#
     C                   MOVE      QQFND         DEFND
     C                   MOVE      QQFYR         DEFYR
     C                   MOVE      QQCASE        DECASE
     C                   MOVE      QQSS#         DESS#
     C                   MOVE      XXACDT        DEACDT
     C                   MOVE      QQEOB#        DEEOB#
     C                   MOVE      KKDSEQ        DEDSEQ
     C                   MOVE      XPEOMB        DEEOB
     C                   MOVE      XXNOT1        DENOT1
     C                   MOVE      *BLANKS       DENOT2
     C                   MOVE      'A'           DESTAT
     C                   MOVE      'A'           DEESRC
     C                   Z-ADD     XXRCN         DERCN
     C*
     C* Update file maintenance fields
     C                   MOVE      Q1USER        DEUSER
     C                   MOVE      Q1PGM         DEPGM
     C                   MOVE      Q1USER        DEIUSR
     C                   MOVE      Q1PGM         DEIPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*
     C                   MOVEL     SRDTO         DEDT
     C                   MOVEL     SRDTO         DEIDT
     C                   Z-ADD     DSTIME        DETIME
     C                   Z-ADD     DSTIME        DEITIM
     C*
     C                   WRITE     PDE01
     C* Restore the sequence number key field to the current record
     C                   Z-ADD     SVDSEQ        KKDSEQ
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     EOMBE         ENDSR
     *********************************************************************
     *** ADJUDICATION MESSAGES SUBROUTINE                    M S G       *
     *********************************************************************
     C     MSG           BEGSR
      *
      * Load the template
      *
     C                   MOVEL     'XX00000'     XXMSG#
      *
      * Is this message already in the file for this line item
      *
     C                   Z-ADD     1             N                 3 0
      *
     C     N             DOWLE     999
      *
     C     QQC(N)        IFEQ      '00'
      *
     C                   LEAVE
      *
     C                   ENDIF
      *
     C                   MOVE(P)   QQS(N)        KKDSEQ
     C                   MOVE      QQC(N)        XXMSG#
      *
     C     MGKY2         CHAIN     PMG02                              60
      *
     C     *IN60         IFEQ      *ON
      *
      * Find the next available message sequence number
      *
     C     MGKY1         SETGT     PMG01
     C     MGKY1         READPE(N) PMG01                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     MGMSEQ        ADD       1             XXMSEQ
      *
     C                   ELSE
      *
     C                   Z-ADD     1             XXMSEQ
      *
     C                   ENDIF                                                  *IN60 = OFF
      *
      * Write the new adjudication message to FDMGP
      *
     C                   Z-ADD     QQCO#         MGCO#
     C                   Z-ADD     QQSUB#        MGSUB#
     C                   MOVE      QQFND         MGFND
     C                   MOVE      QQFYR         MGFYR
     C                   MOVE      QQCASE        MGCASE
     C                   MOVE      *ZEROS        MGDCS#
     C                   Z-ADD     QQEOB#        MGEOB#
     C                   Z-ADD     KKDSEQ        MGDSEQ
     C                   Z-ADD     XXMSEQ        MGMSEQ
     C                   MOVE      'S'           MGSRC
     C                   MOVE      *BLANKS       MGCSI
     C                   MOVE      XXMSG#        MGMSG#
     C                   MOVE      'A'           MGSTAT
     C                   Z-ADD     XXRCN         MGRCN
      *
     C                   MOVE      Q1USER        MGUSER
     C                   MOVE      Q1PGM         MGPGM
     C                   MOVE      Q1PGM         MGIPGM
     C                   MOVE      Q1USER        MGIUSR
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSTIME        MGTIME
     C                   Z-ADD     MGTIME        MGITIM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGDT
     C                   MOVEL     SRDTO         MGIDT
      *
     C                   WRITE     PMG01
      *
     C                   ENDIF                                                  *IN60 = OFF
      *
     C                   ADD       1             N
      *
     C                   ENDDO                                                  N LE 999
      *
     C     MSGE          ENDSR
     C*
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
     C                   Z-ADD     DURVCD        XXRVCD
     C                   MOVE      DURATE        XXHCPC
     C*******************  Z-ADDDUUNTS    XXUNIT           DEL 05/31/92
     C                   Z-ADD     *ZEROS        XXUNIT                         ADD 05/31/92
     C*** BEG ADD *** 04/02/02 ****************************************
     C                   Z-ADD     0             XCMRA
     C*** END ADD *** 04/02/02 ****************************************
     C*
     C     SVKY          SETGT     PSV01                                        GET SERV CODE
     C     NXTSV1        TAG
     C                   READP     PSV01                                  60     RECORD
     C     *IN60         IFEQ      '0'
     C     SVCO#         ANDEQ     CACO#
     C     SVCMSV        ANDEQ     XXCMSV
     C     SVSTAT        CABEQ     'I'           NXTSV1
     C*** BEGIN ADD *** 09/04/98 **************************************
     C*** BEG DEL *** 04/02/02 ****************************************
     C******************** Z-ADDSVALOW    XCMRA
     C*** END DEL *** 04/02/02 ****************************************
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
     C*** BEG ADD *** 04/02/02 ****************************************
     C                   Z-ADD     XXDEIM        XCMRA
     C*** END ADD *** 04/02/02 ****************************************
     C*
     C*** BEG CHG *** 10/21/02 ****************************************
     C********** CACHRG    IFLE XXDEIM
     C     SMCHRG        IFLE      SMDIEM
     C*** END CHG *** 10/21/02 ****************************************
     C*** END ADD *** 03/21/95 *************************************************
     C*** BEGIN CHG *** 04/11/02 **************************************
     C**********           Z-ADD*ZEROS    XPDIFF
     C     XXDEIM        SUB       CACHRG        XPDIFF
     C*** END   CHG *** 04/11/02 **************************************
     C                   Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
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
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXDEIM        XPBRTC
     C*
     C                   ELSE                                                   ELSE P2AM02 EQ 0
     C*
     C* IF THE CAP AMOUNT IS NOT ZERO PAY THE GREATER OF PER DIEM * UNITS OR CAP
     C*
     C     100           SUB       P2AM02        XXCAMT            7 4
     C     XXCAMT        DIV       100           XXCAMT
     C     CACHRG        MULT      XXCAMT        XXCAP
     C*
     C*** BEG CHG *** 10/21/02 ****************************************
     C********** XXDEIM    IFGT XXCAP
     C     SMDIEM        IFGT      X1CAP
     C*** END CHG *** 10/21/02 ****************************************
     C*** BEG ADD *** 10/21/02 ****************************************
     C     XXDEIM        IFGT      CACHRG
     C                   Z-ADD     CACHRG        XPTOTA
     C                   ELSE
     C*** END ADD *** 10/21/02 ****************************************
     C                   Z-ADD     XXDEIM        XPTOTA
     C*** BEG ADD *** 10/21/02 ****************************************
     C                   ENDIF
     C*** END ADD *** 10/21/02 ****************************************
     C                   ELSE
     C                   Z-ADD     XXCAP         XPTOTA
     C*** BEG ADD *** 04/02/02 ****************************************
     C                   Z-ADD     XXCAP         XCMRA
     C*** END ADD *** 04/02/02 ****************************************
     C                   END
     C*
     C                   Z-ADD     *ZEROS        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   END                                                    END P2AM02
     C**********           Z-ADD*ZEROS    XPDIFF
     C**********           Z-ADDXXDEIM    XPTOTA
     C********** CACHRG    SUB  XPTOTA    XPTOTD
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
     C* CHECK TO SEE IF PER DIEM * UNITS IS GREATER THAN CHARGES
     C*
     C     SVUNTS        IFGT      1
     C     P2AM01        MULT      SVUNTS        XXDEIM
     C                   ELSE
     C                   Z-ADD     P2AM01        XXDEIM
     C                   END
     C*** BEG ADD *** 08/14/03 ****************************************
     C                   Z-ADD     XXDEIM        XCMRA
     C*** END ADD *** 08/14/03 ****************************************
     C*
     C*** BEG CHG *** 10/21/02 ****************************************
     C********** CACHRG    IFLE XXDEIM
     C     SMCHRG        IFLE      SMDIEM
     C*** END CHG *** 10/21/02 ****************************************
     C*
     C     XXDEIM        SUB       CACHRG        XPDIFF
     C                   Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXDEIM        XPBRTC
     C*
     C                   ELSE                                                   ELSE CACHRG LE
     C*
     C* IF NO CAP AMOUNT PAY PER DIEM * UNITS
     C*
     C     P2AM02        IFEQ      *ZEROS
     C*
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXDEIM        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XXDEIM        XPBRTC
     C*
     C                   ELSE                                                   ELSE P2AM02 EQ 0
     C*
     C* IF THE CAP AMOUNT IS NOT ZERO PAY THE GREATER OF PER DIEM * UNITS OR CAP
     C*
     C     100           SUB       P2AM02        XXCAMT            7 4
     C     XXCAMT        DIV       100           XXCAMT
     C     CACHRG        MULT      XXCAMT        XXCAP
     C*
     C*** BEG CHG *** 10/21/02 ****************************************
     C********** XXDEIM    IFGT XXCAP
     C     SMDIEM        IFGT      X1CAP
     C*** END CHG *** 10/21/02 ****************************************
     C*** BEG ADD *** 10/22/02 ****************************************
     C     XXDEIM        IFLE      CACHRG
     C*** END ADD *** 10/22/02 ****************************************
     C                   Z-ADD     XXDEIM        XPTOTA
     C*** BEG ADD *** 10/22/02 ****************************************
     C                   ELSE
     C                   Z-ADD     CACHRG        XPTOTA
     C                   ENDIF
     C*** END ADD *** 10/22/02 ****************************************
     C*** BEG ADD *** 08/14/03 ****************************************
     C                   Z-ADD     XXDEIM        XCMRA
     C*** END ADD *** 08/14/03 ****************************************
     C                   ELSE
     C                   Z-ADD     XXCAP         XPTOTA
     C*** BEG ADD *** 08/14/03 ****************************************
     C                   Z-ADD     XXCAP         XCMRA
     C*** END ADD *** 08/14/03 ****************************************
     C                   END
     C*
     C                   Z-ADD     *ZEROS        XPDIFF
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   END                                                    END P2AM02
     C*
     C                   END                                                    END CACHRG LE
     C*** BEGIN DEL *** 12/02/93 **************************************
     C*
     C* P2AM02 IFNE ZERO CALCULATE PAYMENT FROM 'CAP' AMOUNT
     C*
     C**********           Z-ADD*ZEROS    XXTOTP           ADD 10/20/93
     C********** P2AM02    IFNE *ZEROS
     C********** 100       SUB  P2AM02    XXCAMT  74
     C********** XXCAMT    DIV  100       XXCAMT
     C********** CACHRG    MULT XXCAMT    XXTOTP  92
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
     C********** CACHRG    IFLT XXPAMT
     C**********           Z-ADDCACHRG    XPTOTA
     C**********           ELSE
     C**********           Z-ADDXXPAMT    XPTOTA
     C**********           END
     C********** CACHRG    SUB  XPTOTA    XPTOTD
     C**********           Z-ADDXPTOTA    XPTOTP
     C**********           Z-ADDXXPAMT    XPBRTC
     C*** END DEL *** 12/02/93 ****************************************
     C*** BEG DEL *** 09/30/03 ****************************************
     C**********
     C*** BEG ADD *** 08/08/02 ****************************************
     C********** *IN16     IFEQ *ON
     C********** CACHRG    MULT ##PERC    XPTOTA
     C********** CACHRG    SUB  XPTOTA    XPTOTD
     C**********           Z-ADDXPTOTA    XPTOTP
     C**********           Z-ADDXPTOTA    XPBRTC
     C****BEG*ADD *** 08/14/03 ****************************************
     C**********           Z-ADDXPTOTA    XCMRA
     C****END*ADD *** 08/14/03 ****************************************
     C**********           Z-ADD*ZEROS    XPDIFF
     C**********           MOVE '224'     XPEOMB
     C****EOMB*224: "Stop loss met. Paid per the stop loss percent.."
     C**********           ENDIF
     C*** END ADD *** 08/08/02 ****************************************
     C*** END DEL *** 09/30/03 ****************************************
     C                   END                                                    END P2FL02 EQ '*'
     C*** BEG ADD *** 09/30/03 ****************************************
     C*** BEGIN ADD *** 09/14/04 ***********************************************
     C     EOCHRG        MULT      ##PERC        X1LOSS
     C     SMDIEM        IFLE      X1LOSS
     C*** END   ADD *** 09/14/04 ***********************************************
     C     *IN16         IFEQ      *ON
     C     CACHRG        MULT      ##PERC        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C                   Z-ADD     XPTOTA        XCMRA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '224'         XPEOMB
     C*   EOMB 224: "Stop loss met. Paid per the stop loss percent.."
     C                   ENDIF
     C*** END ADD *** 09/30/03 ****************************************
     C*** BEGIN ADD *** 09/14/04 ***********************************************
     C                   ENDIF                                                  EOCHRG MULT
     C*** END   ADD *** 09/14/04 ***********************************************
     C                   ELSE                                                   ELSE EQ 'A'
     C*
     C* IF P2FL01 EQ P (OUTPATIENT %) CALCULATE PAYMENT ON PERCENTAGE
     C*
     C     P2FL01        IFEQ      'P'
     C     P2AM01        DIV       100           XXPERC            7 4
     C     CACHRG        MULT      XXPERC        XXTOTP            9 2
     C     SVUNTS        IFGT      1
     C     XXTOTP        MULT      SVUNTS        XXTOTP
     C                   END
     C*** BEG ADD *** 04/02/02 ****************************************
     C*** BEG CHG *** 08/14/03 ****************************************
     C******************** Z-ADDXPTOTP    XCMRA
     C                   Z-ADD     XXTOTP        XCMRA
     C*** END CHG *** 08/14/03 ****************************************
     C*** END ADD *** 04/02/02 ****************************************
     C*** BEGIN ADD *** 09/28/93 **************************************
     C     CACHRG        IFLT      XXTOTP
     C                   Z-ADD     CACHRG        XXTOTP
     C                   END
     C*** END ADD *** 09/28/93 ****************************************
     C                   Z-ADD     XXTOTP        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
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
     C     CACHRG        MULT      0.00          XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 12/02/93
     C*** BEG ADD *** 08/08/02 ****************************************
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AL'          QQC(Q)
     C*** Err Msg AL: "No CODE information has been found for this
     C*                provider."
     C*** END ADD *** 08/08/02 ****************************************
     C*** BEG DEL *** 08/08/02 ****************************************
     C*************        MOVE '205'     XPEOMB
     C****EOMB*205: "This procedure is included in the basic allowance
     C*************  of another."
     C*** END DEL *** 08/08/02 ****************************************
     C*
     C                   END                                                    END *IN60
     C*
     C                   ELSE                                                   ELSE P2EFDT
     C     CACHRG        MULT      0.00          XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTP        XPBRTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 12/02/93
     C                   END
     C*** BEGIN ADD *** 12/21/93 **************************************
     C                   ELSE                                                   ELSE NO RECORD
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AL'          QQC(Q)
     C*** Err Msg AL: "No CODE information has been found for this
     C*                provider."
     C*** BEGIN ADD *** 04/19/94 ***********************************************
     C     CACHRG        MULT      0.00          XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   Z-ADD     *ZEROS        XPDIFF
     C*** END ADD *** 04/19/94 *************************************************
     C*** END ADD *** 12/21/93 ****************************************
     C                   END
     C*
     C*** IF NOT FOUND IN FMSVP...PAY AT 0%
     C                   ELSE
     C***********CACHRG    MULT .80       XPTOTA           DEL 12/11/92
     C     CACHRG        MULT      0.00          XPTOTA                         ADD 12/11/92
     C     CACHRG        SUB       XPTOTA        XPTOTD
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
     *** TOTCHG SUBROUTINE                                  T O T C H G  *
     *********************************************************************
     C     TOTCHG        BEGSR
     C*
     C                   Z-ADD     0             SMCHRG
     C*** BEG ADD *** 10/21/02 ****************************************
     C                   Z-ADD     0             SMDIEM
     C*** END ADD *** 10/21/02 ****************************************
     C     CAKY          CHAIN(N)  PCA01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C*** BEG ADD *** 10/21/02 ****************************************
     C                   Z-ADD     CADSEQ        KKDSEQ
     C*
     C     DUKY          CHAIN     PDU01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     DURVCD        XXRVCD
     C                   MOVE      DURATE        XXHCPC
     C                   Z-ADD     *ZEROS        XXUNIT
     C*** END ADD *** 10/21/02 ****************************************
     C*
     C                   ADD       CACHRG        SMCHRG
     C*** BEG ADD *** 10/21/02 ****************************************
     C*
     C* SUM UP THE PER DIEM FOR THE ENTIRE BILL
     C*
     C* CHECK ALABAMA PROV. AUX. FILE FOR SERVICE CODE
     C*
     C     P2KY          SETGT     PP201
     C                   READP     PP201                                  99
     C*
     C     *IN99         IFEQ      '0'
     C     CAFDAT        ANDGE     P2EFDT
     C     CAFEIN        ANDEQ     P2FEIN
     C     CAMOD         ANDEQ     P2MOD
     C     XXCMSV        ANDEQ     P2SV01
     C*
     C* IF SERVICE CODE MARKED A = AMOUNT PER DIEM
     C*
     C     P2FL01        IFEQ      'A'
     C*
     C     DUUNTS        IFGT      1
     C*
     C     P2AM01        MULT      DUUNTS        X1DIEM
     C                   ELSE
     C*
     C                   Z-ADD     P2AM01        X1DIEM
     C*
     C                   END                                                    SVUNTS GT 1
     C*
     C                   ADD       X1DIEM        SMDIEM
     C*
     C     P2AM02        IFNE      0
     C     100           SUB       P2AM02        XXCAP
     C                   END                                                    P2FL01 = A
     C*
     C                   END                                                    P2FL01 = A
     C*
     C                   END                                                    *IN99 = 0
     C*
     C                   END                                                    *IN99 = 0
     C*** END ADD *** 10/21/02 ****************************************
     C*
     C     CAKY          READE     PCA01                                  99
     C*
     C                   ENDDO                                                  *IN99 DOWEQ 0
     C*
     C     100           SUB       P2AM02        X1CAMT            7 4
     C     X1CAMT        DIV       100           X1CAMT
     C     SMCHRG        MULT      X1CAMT        X1CAP
     C*
     C     TOTCHE        ENDSR
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
     C     XPEOMB        IFNE      *BLANKS
     C*
     C                   Z-ADD     SVDSEQ        KKDSEQ
     C                   EXSR      EOMB
     C*
     C                   ENDIF
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM ON HOSPITAL.
     C*
     C     DUKY1         CHAIN     PDU02                              54
      *
     C     *IN54         DOWEQ     '0'
      *
     C     DUDSEQ        IFNE      D1DSEQ
      *
     C                   Z-ADD     D1DSEQ        KKDSEQ
      *
     C     CAKYO         CHAIN     PCA0O                              55
      *
     C     *IN55         IFEQ      '0'
     C     CAFDAT        ANDEQ     OXFDAT
     C     CAFEIN        ANDEQ     OXFEIN
     C     OXFLAG        ANDNE     'P'
      *
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C                   MOVE      *ON           *IN54
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C*
     C                   END
     C*
     C                   END
     C*
     C  N54DUKY1         READE     PDU02                                  54                3
     C*
     C                   ENDDO                                                  *IN54 DOWEQ
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** ONLY PROCESS THE RECORD IF NOT A OVERRIDE TYPE.
     C*
     C*
     C     CAKY1A        CHAIN     PCA01                              54
     C     CSOVRD        IFNE      'Y'
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPTOTD        CAMRA
     C                   Z-ADD     XPTOTP        CAPAID
     C                   Z-ADD     XPBRTC        CABRTC
     C*                    Z-ADDXPDIFF    CADIFF           ADD 12/02/93
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
     C*** BEGIN ADD *** 04/12/02 **************************************
     C                   Z-ADD     XPDIFF        CADIFF
     C*** END   ADD *** 04/12/02 **************************************
     C*
     C  N54              UPDATE    PCA01
     C*
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
