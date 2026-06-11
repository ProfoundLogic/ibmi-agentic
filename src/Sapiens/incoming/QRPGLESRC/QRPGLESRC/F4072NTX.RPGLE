     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4072NTX
     H*    DESCRIPTION: ADJUDICATION (TEXAS RULES - 04/01/96)
     H*   This program is called from F0070R (Claim Adjudication) and will
     H* perform the actual adjudication of the detail claim records writ-
     H* ten by F0070R.  This program was derived from the Texas State
     H* Fee Schedule Reimbursment Manual issued on 08/01/91.
     H* This Sub-program is intended to be used to calculate the amount
     H* that is due a Provder of services, by the Carrier of the Work comp
     H* insurance.  It should be used for any service provided on or after
     H* 04/01/96.
     H*****************************************************************
     H*  CREATED: 3/07/96   C:32   PROJECT: TX005
     H****************************************************************F
     H*
     H* PROGRAMMER NOTE: THE MAIN SECTION HAS BEEN DUPLICATED
     H* IN ORDER TO MAKE TWO PASSES OF THE DATA
     H*
     H* Information needed for each year's adjudication program rules:
     H*    1.  Update the effective date range for reading the detail charges
     H*        (found in the mainline section)
     H*    2.  Update Conversion factors:
     H*        (found in *LIKE section)
     H*        ANESTHESIA              XXANES
     H*        CERT ANESTHESIOLOGISTS  XXCANE
     H*****************************************************************
     H*  SPECIALTY CODES USED IN THIS PROGRAM:
     H*  OT  - OCCUPATIONAL THERAPISTS
     H*  OCT - OCCUPATIONAL THERAPISTS
     H*  PHT - PHYSICAL THERAPISTS
     H*  RNA - CERTIFIED REGISTERED NURSE ANESTHETISTS
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  08/09/02 C:062 PROJECT: FS244
     H*                 USE CAZIPA.
     H*
     H*  09/25/01 C:062 PROJECT: FSMNT
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.
     H*                                                               *
     H*  11/16/00 C:79  Support: 25098                                *
     H*                 For PT codes if anes flag=T dont mult by anes *
     H*                 amount, use MRA.                              *
     H*                                                               *
     H*  09/27/00 C:79  Support: 24397                                *
     H*                 Remove code that denies 97010-97546 with      *
     H*                 99202-99204 for Phys med.                     *
     H*                                                               *
     H*  04/20/99 C:79  Support: 17689                                *
     H*                 Move execution of Follow up day subroutine    *
     H*                 outside of main loop so it only executes once *
     H*                 each time the program is called               *
     H*
     H*  04/08/99 C:117 Support: MC095 Seq# 17568
     H*                 Added QQCO#, QQSUB#, QQFND to the *ENTRY PARMs
     H*                 and changed all occurences of UPCO#, UPSUB#,
     H*                 UPFND to the above mentioned fields.
     H*
     H*
     H*  09/21/98 C:79  PROJ: MC152
     H*                 MAKE DATES YEAR 2000 COMPATIBLE.  MODIFY TO
     H*                 WORK WITH PROVIDER BILLS
     H*                 1.  Scan replace CATOTC with CACHRG.
     H*                 2.  Scan replace CATOTD with CAMRA.
     H*                 2.  Scan replace CATOTP with CAPAID
     H*
     H*  05/20/98 C:31  Support: 11599
     H*                 MedView Discount Issue
     H*                 1. Change definition of XPBRTC
     H*
     H*  10/21/97 C:59 PROJ:TX007
     H*                CHG LOGICALS TO ONLY SEARCH AGAINST PHYSICIAN
     H*                BILLS
     H*
     H*  05/03/96 C:32 PROJ:TX005
     H*                INCREASE ARRAY PDAT, MDAT DUE TO REQUIREMENT >30
     H*  05/13/96 C:32 PROJ:TX005
     H*                SETUP FOR L1-L5 99455, 99456, 99499
     H*                DID NOT DO REQ BEFORE DUE TO NEED FOR 2 MODIFIERS
     H*                THIS CHG DOES NOT ALLOW FOR MODS -26, 27, WP
     H*  08/28/96 C:32 PROJ:TXMNT
     H*            1.  ADD EDIT TO SKIP PHARM FDCAP RECORDS WHEN
     H*                READING ALL RECS FOR A SPECIFIC DATE
     H*            2.  REMOVE MAJOR/MINOR FOR 99362, AND 99376
     H*  04/03/97 C:32 PROJ:TXMNT
     H*            1.  CHG TO LOAD CABRTC IF CATOTA HAS AMOUNT LOADED
     H*                EXCLUDING BY-REPORT FLAGS (I,A)
     H*****************************************************************
     H*  INDICATOR USAGE:
     H*  54    Use with Primary FDCAP file (i.e. DOWEQ, etc on reading charges)
     H*  60    General Use
     H*  61    General Use
     H*  62    General Use
     H*****************************************************************
     H*  SECTION                                  SUBR         CODES
     H*  Anesthesia                               ANES         00100-01999
     H** Durable Medical Equipment                DME          A0000-A9999
     H*                                                        E0000-E9999
     H*                                                        K0000-K9999
     H*                                                        L0000-L9999
     H** Medical                                  MEDI         90000-99199
     H*  Office Visits                            OFFI         99201-99499
     H** Pathology                                PATH         80002-89399
     H** Radiology                                RADI         70010-79999
     H*  Surgery                                  SURG         10000-69979
     H*                                           SURG2        10000-69979
     H*****************************************************************
      *** BEG DEL *** 09/21/99 ****************************************
     F**********FDCAL7 is the Primary File Read (PCA01)
     F**********FDCAL7  UF  E           K        DISK
      *** END DEL *** 09/21/99 ****************************************
     F*
     FFACAPTX   O    E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK1)
     FFACALTX   IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK2)
     F*** BEGIN CHG *** 10/21/97 **************************************
     F*   THE LOGICALS WITH POSTFIX OF 'P' ONLY SEARCH AGAINST PHYSICIAN
     F*   TYPE BILLS
     F*********FDCAL6  IF  E           K        DISK
     FFDCAL6P   IF   E           K DISK
     F*** BEGIN CHG *** 10/21/97 **************************************
     F                                     RENAME(PCA01:PCA06)
     F*** BEGIN CHG *** 10/21/97 **************************************
     F*** BEG DEL *** 09/21/98 ****************************************
     F*********FDCAL8  UF  E           K        DISK
     F*********FDCAL8P UF  E           K        DISK
     F*** BEGIN CHG *** 10/21/97 **************************************
     F*********** PCA01                             KRENAMEPCA08
     F*** END DEL *** 09/21/98 ****************************************
     F*** BEGIN CHG *** 10/21/97 **************************************
     F*********FDCAL9  IF  E           K        DISK
     FFDCAL9P   IF   E           K DISK
     F*** BEGIN CHG *** 10/21/97 **************************************
     F                                     RENAME(PCA01:PCA09)
     F*** BEGIN CHG *** 10/21/97 **************************************
     F*********FDCALA  IF  E           K        DISK
     FFDCALAP   IF   E           K DISK
     F*** BEGIN CHG *** 10/21/97 **************************************
     F                                     RENAME(PCA01:PCA0A)
     F*** BEGIN CHG *** 10/21/97 **************************************
     F*********FDCALP  IF  E           K        DISK
     FFDCALPP   IF   E           K DISK
     F*** END   CHG *** 10/21/97 **************************************
     F                                     RENAME(PCA01:PCA0P)
     F*** BEGIN CHG *** 10/21/97 **************************************
     F*********FDCALC  IF  E           K        DISK
     FFDCALCP   IF   E           K DISK
     F*** BEGIN CHG *** 10/21/97 **************************************
     F                                     RENAME(PCA01:PCA0C)
     FFMCML1    IF   E           K DISK
     F*** BEGIN DEL *** 08/09/02 **************************************
     F**********FMGZL   IF  E           K        DISK
     F*** END   DEL *** 08/09/02 **************************************
     FFMPDL     IF   E           K DISK
     FFMPRLTX   IF   E           K DISK
     FFMRCL     IF   E           K DISK
     FFTABLTX   IF   E           K DISK
     F                                     IGNORE(PRC01)
     FSMUPL     IF   E           K DISK
     F*** BEG ADD *** 09/21/98 ****************************************
     F* Message file logicals
     F*
     FFDMGL1    UF A E           K DISK
     FFDMGL5    IF   E           K DISK
     F                                     RENAME(PMG01:PMG02)
     F*  FDCAL13 is the Primary File Read (PCA01)
     FFDCAL13   UF   E           K DISK
     F                                     RENAME(PCA01:PCA13)
     F*  FDCAL5 is the File to be updated in SURG2 subroutine
     FFDCAL5    UF   E           K DISK
     F                                     RENAME(PCA01:PCA05)
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDDEL4    UF A E           K DISK
     FFAEOL4    IF   E           K DISK
     FFDDHL     IF   E           K DISK
     FFDDHL1    IF   E           K DISK
     F                                     RENAME(PDH01:PDH02)
     F*** END ADD *** 09/21/98 ****************************************

     D*
     D* "Entry" Arrays
      *** BEG DEL *** 09/21/98 ****************************************
     D******************* QQS        30  5               DETAIL SEQ #
     D******************* QQC        30  2               ERROR MESSAGES
     D******************* QQF        50  6               FROM DATES
      *** END DEL *** 09/21/98 ****************************************
     D*
      *** BEG CHG *** 09/21/98 ****************************************
     D******************* FUF       500  6 0             FOLLOW UP FRM DY
     D******************* FUT       500  6 0             FOLLOW UP TO DAY
     D FUF             S              7  0 DIM(500)                             FOLLOW UP FRM DY
     D FUT             S              7  0 DIM(500)                             FOLLOW UP TO DAY
      *** END CHG *** 09/21/98 ****************************************
     D* SAVE ENTRY ARRAYS
      *** BEG DEL *** 09/21/98 ****************************************
     D******************* SVQQS      30  5               DETAIL SEQ #
     D******************* SVQQC      30  2               ERROR MESSAGES
      *** END DEL *** 09/21/98 ****************************************
     D*
     D* Compile-time Arrays
     D QCA             S             55    DIM(4) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D ANEPM1          S              2    DIM(6) CTDATA PERRCD(1)              ANES Px Modifiers
     D ANEPM2          S              1  0 DIM(6) ALT(ANEPM1)
     D NOCT            S              5    DIM(102) CTDATA PERRCD(1)            NO CUT SURG
     D SUR1            S              5    DIM(189) CTDATA PERRCD(1)            CUT/MAJOR-MIN
     D SUR2            S              5    DIM(189) ALT(SUR1)
     D BODY            S              6  0 DIM(13)                              BODY PART CNTR
      *** BEG CHG *** 09/21/98 ****************************************
     D******************* FDAT       12  6 0             FUTURE DATE ARRAY
     D FDAT            S              7  0 DIM(12)                              FUTURE DATE ARRAY
      *** END CHG *** 09/21/98 ****************************************
     D FAMT            S              7  2 DIM(12)                              FUTURE AMT  ARRAY
     D CPT4            S              5    DIM(30)                              CPT4 ARRAY
     D PRT             S              2    DIM(20)                              PART TBL
     D CNT             S              7  0 DIM(20)                              PART CNT
     D CNTU            S              7  0 DIM(20)                              PART CNT UNITS
     D RADLC           S              5    DIM(32) CTDATA PERRCD(1)             (L) RADI CPT4/MAR
     D RADL            S              5  0 DIM(32) ALT(RADLC)
     D RADLP           S              5  0 DIM(32) CTDATA PERRCD(1)             (L) RADI PROF/TECH
     D RADLT           S              5  0 DIM(32) ALT(RADLP)
     D RADEC           S              5    DIM(32) CTDATA PERRCD(1)             (E) RADI CPT4/MAR
     D RADE            S              5  0 DIM(32) ALT(RADEC)
     D RADEP           S              5  0 DIM(32) CTDATA PERRCD(1)             (E) RADI PROF/TECH
     D RADET           S              5  0 DIM(32) ALT(RADEP)
     D SEPP            S              5    DIM(259) CTDATA PERRCD(1)            SEPARATE PROCEDURE
     D*** BEGIN CHG *** 05/03/96 **************************************
     D*************       MDAT       30  6 0             DATE ARRAY
     D*************       PDAT       30  6 0             DATE ARRAY
     D MDAT            S              7  0 DIM(200)                             DATE ARRAY
     D PDAT            S              7  0 DIM(200)                             DATE ARRAY
     D*** END   CHG *** 05/03/96 **************************************
      *** BEG ADD *** 09/21/98 ****************************************
     D MOD             S              2    DIM(4) ASCEND                        MODIFIER ARRAY
      *** END ADD *** 09/21/98 ****************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*  Date Data Structure.
     D                 DS
     D  XXYMD2                 1      6  0
     D  XXYM2                  1      4  0
     D                 DS
     D  XXYMD3                 1      6  0
     D  XXYM3                  1      4  0
      *** END ADD *** 09/21/98 ****************************************

     I*
     I*  Rename the duplicate field names.
     IPCA06
     I              CACO#                       X6CO#
     I              CASUB#                      X6SUB#
     I              CAFND                       X6FND
     I              CAFYR                       X6FYR
     I              CACASE                      X6CASE
     I              CAFDAT                      X6FDAT
     I              CADSEQ                      X6DSEQ
     I              CAFEIN                      X6FEIN
     I              CAMOD                       X6MOD
     I              CACPT4                      X6CPT4
     I              CATOTA                      X6TOTA
     I              CACHRG                      X6TOTC
     I              CAMRA                       X6TOTD
     I              CAPAID                      X6TOTP
     I              CAPMOD                      X6PMOD
     I              CAUSER                      X6USER
     I              CAPGM                       X6PGM
     I              CADT                        X6DT
     I              CATIME                      X6TIME
     I              CACHK#                      X6CHK#
     I              CAEOB#                      X6EOB#
     I              CADIFF                      X6DIFF
     I              CAPART                      X6PART
     I              CADXR                       X6DXR
     I              CAFLAG                      X6FLAG
     I*
      *** BEG CHG *** 09/21/98 ****************************************
     I************PCA08
     IPCA05
      *** END CHG *** 09/21/98 ****************************************
     I              CACO#                       XRCO#
     I              CAFND                       XRFND
     I              CAFYR                       XRFYR
     I              CACASE                      XRCASE
     I              CACSCD                      XRCSCD
     I              CAFDAT                      XRFDAT
     I              CATDAT                      XRTDAT
     I              CARDAT                      XRRDAT
     I              CAIPOP                      XRIPOP
     I              CAFEIN                      XRFEIN
     I              CAMOD                       XRMOD
     I              CAPRTY                      XRPRTY
     I              CATRCD                      XRTRCD
     I              CASVCD                      XRSVCD
     I              CAPMOD                      XRPMOD
     I              CACPT4                      XRCPT4
     I              CAICD9                      XRICD9
     I              CATOTA                      XRTOTA
     I              CAEOMB                      XREOMB
     I              CADSEQ                      XRDSEQ
     I              CADELF                      XRDELF
     I              CAFLAG                      XRFLAG
     I              CACHK#                      XRCHK#
     I              CAEOB#                      XREOB#
     I              CAPART                      XRPART
     I              CAP1                        XRP1
     I              CASS#                       XRSS#
     I              CAACDT                      XRACDT
     I              CASTAT                      XRSTAT
     I              CAUSER                      XRUSER
     I              CAPGM                       XRPGM
     I              CATIME                      XRTIME
     I              CADT                        XRDT
     I              CAIUSR                      XRIUSR
     I              CAIDT                       XRIDT
     I              CADXR                       XRDXR
     I              CADCS#                      XRDCS#
     I              CALTR#                      XRLTR#
     I              CARCFL                      XRRCFL
     I              CASUB#                      XRSUB#
     I              CAOTHR                      XROTHR
     I              CANDIS                      XRNDIS
     I              CARDTP                      XRRDTP
     I              CABRSV                      XRBRSV
     I              CABRPM                      XRBRPM
     I              CABRPC                      XRBRPC
     I              CABRAM                      XRBRAM
     I              CABRUN                      XRBRUN
     I              CABRTC                      XRBRTC
     I              CAZIPA                      XRZIPA
     I              CAAST                       XRAST
     I              CADIFF                      XRDIFF
     I              CAIPGM                      XRIPGM
     I              CACHRG                      XRTOTC
     I              CAMRA                       XRTOTD
     I              CAOTH2                      XROTH2
     I              CAPAID                      XRTOTP
     I              CARCN                       XRRCN
     I              CALIN#                      XRLIN#
     I              CARULE                      XRRULE
     I              CAOT1C                      XROT1C
     I              CAOT2C                      XROT2C
     I              CAMRAA                      XRMRAA
     I              CAITIM                      XRITIM
     I              CAMNAA                      XRMNAA
     I*
     IPCA0A
     I              CACO#                       XACO#
     I              CAFND                       XAFND
     I              CAFYR                       XAFYR
     I              CACASE                      XACASE
     I              CACSCD                      XACSCD
     I              CAFDAT                      XAFDAT
     I              CATDAT                      XATDAT
     I              CARDAT                      XARDAT
     I              CAIPOP                      XAIPOP
     I              CAFEIN                      XAFEIN
     I              CAMOD                       XAMOD
     I              CAPRTY                      XAPRTY
     I              CATRCD                      XATRCD
     I              CASVCD                      XASVCD
     I              CAPMOD                      XAPMOD
     I              CACPT4                      XACPT4
     I              CAICD9                      XAICD9
     I              CATOTA                      XATOTA
     I              CAEOMB                      XAEOMB
     I              CADSEQ                      XADSEQ
     I              CADELF                      XADELF
     I              CAFLAG                      XAFLAG
     I              CACHK#                      XACHK#
     I              CAEOB#                      XAEOB#
     I              CAPART                      XAPART
     I              CAP1                        XAP1
     I              CASS#                       XASS#
     I              CAACDT                      XAACDT
     I              CASTAT                      XASTAT
     I              CAUSER                      XAUSER
     I              CAPGM                       XAPGM
     I              CATIME                      XATIME
     I              CADT                        XADT
     I              CAIUSR                      XAIUSR
     I              CAIDT                       XAIDT
     I              CADXR                       XADXR
     I              CADCS#                      XADCS#
     I              CALTR#                      XALTR#
     I              CARCFL                      XARCFL
     I              CASUB#                      XASUB#
     I              CAOTHR                      XAOTHR
     I              CANDIS                      XANDIS
     I              CARDTP                      XARDTP
     I              CABRSV                      XABRSV
     I              CABRPM                      XABRPM
     I              CABRPC                      XABRPC
     I              CABRAM                      XABRAM
     I              CABRUN                      XABRUN
     I              CABRTC                      XABRTC
     I              CAZIPA                      XAZIPA
     I              CAAST                       XAAST
     I              CADIFF                      XADIFF
     I              CAIPGM                      XAIPGM
     I              CACHRG                      XATOTC
     I              CAMRA                       XATOTD
     I              CAOTH2                      XAOTH2
     I              CAPAID                      XATOTP
     I              CARCN                       XARCN
     I              CALIN#                      XALIN#
     I              CARULE                      XARULE
     I              CAOT1C                      XAOT1C
     I              CAOT2C                      XAOT2C
     I              CAMRAA                      XAMRAA
     I              CAITIM                      XAITIM
     I              CAMNAA                      XAMNAA
     I*
     IPCA09
     I              CACO#                       XRCO#
     I              CASUB#                      XRSUB#
     I              CAFND                       XRFND
     I              CAFYR                       XRFYR
     I              CACASE                      XRCASE
     I              CAFDAT                      XRFDAT
     I              CADSEQ                      XRDSEQ
     I              CAFEIN                      XRFEIN
     I              CAMOD                       XRMOD
     I              CATOTA                      XRTOTA
     I              CACHRG                      XRTOTC
     I              CAMRA                       XRTOTD
     I              CAPAID                      XRTOTP
     I              CAPMOD                      XRPMOD
     I              CACPT4                      DXPRCD
     I              CAUSER                      XRUSER
     I              CAPGM                       XRPGM
     I              CADT                        XRDT
     I              CATIME                      XRTIME
     I              CACHK#                      XRCHK#
     I              CAEOB#                      XREOB#
     I              CADXR                       XRDXR
     I              CAPART                      XRPART
     I              CAEOMB                      XREOMB
     I              CADIFF                      XRDIFF
     I              CABRPC                      XRBRPC
     I              CARCFL                      XRRCFL
     I*
     IPCA0P
     I              CACO#                       PXCO#
     I              CASUB#                      PXSUB#
     I              CAFND                       PXFND
     I              CAFYR                       PXFYR
     I              CACASE                      PXCASE
     I              CAFDAT                      PXFDAT
     I              CADSEQ                      PXDSEQ
     I              CAFEIN                      PXFEIN
     I              CAMOD                       PXMOD
     I              CACPT4                      PXCPT4
     I              CAPMOD                      PXPMOD
     I              CAUSER                      PXUSER
     I              CAPGM                       PXPGM
     I              CATIME                      PXTIME
     I              CADT                        PXDT
     I              CACHK#                      PXCHK#
     I              CAEOB#                      PXEOB#
     I              CAPART                      PXPART
     I              CADXR                       PXDXR
     I              CAFLAG                      PXFLAG
     I              CADIFF                      PXDIFF
     I              CARCFL                      PXRCFL
     I              CACHRG                      PXTOTC
     I              CAMRA                       PXTOTD
     I              CAPAID                      PXTOTP
     I*
     IPCA0C
     I              CACO#                       XCCO#
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
     I              CAPMOD                      XCPMOD
     I              CACPT4                      XCCPT4
     I              CAICD9                      XCICD9
     I              CATOTA                      XCTOTA
     I              CAEOMB                      XCEOMB
     I              CADSEQ                      XCDSEQ
     I              CADELF                      XCDELF
     I              CAFLAG                      XCFLAG
     I              CACHK#                      XCCHK#
     I              CAEOB#                      XCEOB#
     I              CAPART                      XCPART
     I              CAP1                        XCP1
     I              CASS#                       XCSS#
     I              CAACDT                      XCACDT
     I              CASTAT                      XCSTAT
     I              CAUSER                      XCUSER
     I              CAPGM                       XCPGM
     I              CATIME                      XCTIME
     I              CADT                        XCDT
     I              CAIUSR                      XCIUSR
     I              CAIDT                       XCIDT
     I              CADXR                       XCDXR
     I              CADCS#                      XCDCS#
     I              CALTR#                      XCLTR#
     I              CARCFL                      XCRCFL
     I              CASUB#                      XCSUB#
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
     I              CADIFF                      XCDIFF
     I              CAIPGM                      XCIPGM
     I              CACHRG                      XCTOTC
     I              CAMRA                       XCTOTD
     I              CAOTH2                      XCOTH2
     I              CAPAID                      XCTOTP
     I              CARCN                       XCRCN
     I              CALIN#                      XCLIN#
     I              CARULE                      XCRULE
     I              CAOT1C                      XCOT1C
     I              CAOT2C                      XCOT2C
     I              CAMRAA                      XCMRAA
     I              CAITIM                      XCITIM
     I              CAMNAA                      XCMNAA
     I*
     I* Record Format for SURG2 Workfile FACALTX
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
      *** BEG ADD *** 09/21/98 ****************************************
     I*  Rename the duplicate field names.
     IPDH02
     I              DHCO#                       DXCO#
     I              DHSUB#                      DXSUB#
     I              DHFND                       DXFND
     I              DHFYR                       DXFYR
     I              DHCASE                      DXCASE
     I              DHSS#                       DXSS#
     I              DHACDT                      DXACDT
     I              DHEOB#                      DXEOB#
     I              DHDSEQ                      DXDSEQ
     I              DHPOS                       DXPOS
     I              DHTOS                       DXTOS
     I              DHPRCD                      DXPRCD
     I              DHPRDS                      DXPRDS
     I              DHMOD1                      DXMOD1
     I              DHMOD2                      DXMOD2
     I              DHMOD3                      DXMOD3
     I              DHMOD4                      DXMOD4
     I              DHDXR1                      DXDXR1
     I              DHDXR2                      DXDXR2
     I              DHDXR3                      DXDXR3
     I              DHDXR4                      DXDXR4
     I              DHUNTS                      DXUNTS
     I              DHAREA                      DXAREA
     I              DHPPLN                      DXPPLN
     I              DHSTAT                      DXSTAT
     I              DHUSER                      DXUSER
     I              DHPGM                       DXPGM
     I              DHTIME                      DXTIME
     I              DHDT                        DXDT
     I              DHIUSR                      DXIUSR
     I              DHIPGM                      DXIPGM
     I              DHITIM                      DXITIM
     I              DHIDT                       DXIDT
     I              DHRCFL                      DXRCFL
      *** BEG DEL *** 09/21/98 ****************************************

     C********************
     C*  Claim Detail File
     C***********CAKY1**** KLIST
     C******************** KFLD           QQCO#
     C******************** KFLD           QQSUB#
     C******************** KFLD           QQFND
     C******************** KFLD           QQFYR
     C******************** KFLD           QQCASE
     C******************** KFLD           XXYMD
      *** END DEL *** 09/21/98 ****************************************
     C*
     C*  CLAIM DETAIL FILE
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
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** KFLD           SVCHK#
      *** END DEL *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
      *** END DEL *** 09/21/98 ****************************************
     C*
     C     CAKY6         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXPMOD
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C*** Used with PCA08 (FDCAL8) in SURG2
     C*******************
     C***********CAKY8A**  KLIST
     C*******************  KFLD           QQCO#
     C*******************  KFLD           QQSUB#
     C*******************  KFLD           QQFND
     C*******************  KFLD           QQFYR
     C*******************  KFLD           QQCASE
     C*******************  KFLD           PPFDAT
     C*******************  KFLD           PPFEIN
     C*******************  KFLD           PPMOD
     C*******************  KFLD           PPCPT4
     C*******************  KFLD           PPDSEQ
      *** END DEL *** 09/21/98 ****************************************
     C*
     C     CAKY9         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    XXCPT4
     C*
     C     CAKY9A        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C*
     C*** Used with PCA09 (FDCAL9) in SURG2
     C     CAKY9C        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    PPFDAT
     C                   KFLD                    PPFEIN
     C                   KFLD                    PPMOD
     C                   KFLD                    XXCPT4
     C*
     C*** Used with PCA09 (FDCAL9) in SURG2
     C     CAKY9D        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    PPFDAT
     C                   KFLD                    PPFEIN
     C                   KFLD                    PPMOD
     C*
     C*** Used with PCA09 (FDCAL9) in MEDI
     C     CAKY9E        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C*
     C     CAKYA         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    XXYMD
     C                   KFLD                    XXCPT4
     C*
     C     CAKYA1        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C*
     C     CAKYA2        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    XXYMD
     C*
     C     CAKYP2        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXPMOD
     C*
     C     CAKYP1        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXFDAT
     C                   KFLD                    CAFEIN
     C                   KFLD                    XXCPT4
     C*
     C     CAKYZ         KLIST
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
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** KFLD           HLCHK#
      *** BEG DEL *** 09/21/98 ****************************************
     C*
     C     CAKY8         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXPMOD
     C*
     C     CAKYC         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    XXCPT4
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
     C     CAKYC3        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C*
     C*  Claim Master File
     C     CMKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C*
     C*  Procedure Code Modifier File
     C     PMKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    TABKEY
     C*
     C     PRKEY         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXEFDT
     C*
     C*
     C*  Claim Status Code Table File
     C     TCSKY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    TABKEY
     C*
     C*  Dummy key used for release of all CA records
     C     DUMMY         KLIST
     C                   KFLD                    QQCO#
     C*
     C*** Geographical Zip Area FILE
     C*** BEGIN DEL *** 08/09/02 **************************************
     C********** GZKY      KLIST
     C**********           KFLD           QQCO#
     C**********           KFLD           XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C*
     C*  Procedure Code file
     C     PRKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXEFDT
     C*
     C*  Provider Code File
     C     PDKY          KLIST
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C*
     C*  PROVIDER AUXILIARY FILE
     C     P1KY          KLIST
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C*
     C* Reasonable & Customary File
     C     RCKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    XXZIPA
     C                   KFLD                    XXCPCD
     C                   KFLD                    XXYMD
     C*** BEG ADD *** 09/21/98 ****************************************
     C*
     C* Adjudication Message File (PMG01)
     C*
     C     MGKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXDSEQ
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
     C                   KFLD                    XXDSEQ
     C                   KFLD                    XXMSG#
     C                   KFLD                    XXRCN
      *
     C     CAKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXYMD
     C*
     C     CAKY2         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*
     C     DEKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    QQDSEQ
     C                   KFLD                    QQESRC
     C*
     C     DEKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    QQDSEQ
     C*
     C     EBKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    EBKEY
     C*
     C     EOKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*
     C     DHKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXDSEQ
     C*
     C     CAKY5         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    PPDSEQ
     C*** END ADD *** 09/21/98 ****************************************
     C*
     C*  User Parameter File
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** PARM           QQFDT 150
     C******************** PARM           QQFDT2150
     C******************** PARM           QQS
     C******************** PARM           QQC
      *** END DEL *** 09/21/98 ****************************************
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQSS#             9 0          CHAIN TO EE
     C                   PARM                    QQACDT            8 0          (MMDDYY ORD)
     C                   PARM                    QQSPCD            3
      *** BEG ADD *** 09/21/98 ****************************************
     C                   PARM                    QQEOB#            9 0
      *** END ADD *** 09/21/98 ****************************************
     C*
     C*** BEG ADD *** 04/08/99 ***************************************
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQSUB#            5 0
     C                   PARM                    QQFND             3 0
     C*** END ADD *** 04/08/99 ***************************************
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
     C     QQRTCD        CABEQ     'P'           TERM                           PRE-OPEN FILES
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** MOVEAQQFDT     QQF,1
     C******************** MOVEAQQFDT2    QQF,26
      *** END DEL *** 09/21/98 ****************************************
     C*
     C* = Initialization Section ======================================
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** MOVEAQQS       SVQQS
     C******************** MOVEAQQC       SVQQC
      *** END DEL *** 09/21/98 ****************************************
     C*
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDATE            7 0
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** *LIKE     DEFN CACHK#    HLCHK#
      *** BEG DEL *** 09/21/98 ****************************************
     C     *LIKE         DEFINE    CADSEQ        HLDSEQ
     C     *LIKE         DEFINE    CAEOB#        HLEOB#
     C     *LIKE         DEFINE    CAFEIN        HLFEIN
     C     *LIKE         DEFINE    CAFEIN        HDFEIN
     C     *LIKE         DEFINE    CAFDAT        HLFDAT
     C     *LIKE         DEFINE    CAFDAT        HDYMD
     C     *LIKE         DEFINE    CAFDAT        SVYMD
     C     *LIKE         DEFINE    CAMOD         HLMOD
     C     *LIKE         DEFINE    CAMOD         HDMOD
     C     *LIKE         DEFINE    CAPRTY        HLPRTY
     C*
     C     *LIKE         DEFINE    CAUNTS        PHOURS
     C     *LIKE         DEFINE    CAUNTS        LHOURS
     C*
     C     *LIKE         DEFINE    DHPRCD        PPCPT4
     C     *LIKE         DEFINE    CADSEQ        PPDSEQ
     C     *LIKE         DEFINE    CAFDAT        PPFDAT
     C     *LIKE         DEFINE    CAFEIN        PPFEIN
     C     *LIKE         DEFINE    CAMOD         PPMOD
     C*
     C     *LIKE         DEFINE    PRANAM        SXANAM
     C     *LIKE         DEFINE    CAFEIN        SXFEIN
     C     *LIKE         DEFINE    CAMOD         SXMOD
     C     *LIKE         DEFINE    PRMRA         SXMRA
     C     *LIKE         DEFINE    PRMRA         HLMRA
     C     *LIKE         DEFINE    PRPROF        SXPROF
     C     *LIKE         DEFINE    PRRPT         SXRPT
     C     *LIKE         DEFINE    PRSUPP        SXSUPP
     C     *LIKE         DEFINE    PRSURG        SXSURG
     C     *LIKE         DEFINE    PR92N1        SX92N1
     C     *LIKE         DEFINE    PR1A1         SX1A1
     C     *LIKE         DEFINE    PR1A2         SX1A2
     C     *LIKE         DEFINE    PR1A3         SX1A3
     C     *LIKE         DEFINE    PR1A4         SX1A4
     C     *LIKE         DEFINE    PR1A5         SX1A5
     C*
     C     *LIKE         DEFINE    CACHK#        SVCHK#
     C     *LIKE         DEFINE    CADSEQ        SVDSEQ
     C     *LIKE         DEFINE    CAFDAT        SVDT
     C     *LIKE         DEFINE    CAEOB#        SVEOB#
     C     *LIKE         DEFINE    CAFEIN        SVFEIN
     C     *LIKE         DEFINE    CAMOD         SVMOD
     C     *LIKE         DEFINE    CAPRTY        SVPRTY
     C     *LIKE         DEFINE    CAUNTS        SVUNTS
     C*
     C     *LIKE         DEFINE    PMKEY         TABKEY
     C*
     C     *LIKE         DEFINE    DHPRCD        XPCPT4
     C     *LIKE         DEFINE    CAEOMB        XPEOMB
     C     *LIKE         DEFINE    CAPMOD        XPPMOD
     C     *LIKE         DEFINE    CATOTA        XPTOTA
     C     *LIKE         DEFINE    CACHRG        XPTOTC
     C     *LIKE         DEFINE    CAMRA         XPTOTD
     C     *LIKE         DEFINE    CAPAID        XPTOTP
     C*** BEGIN CHG *** 05/20/98 **************************************
     C********   *LIKE     DEFN CABRUN    XPBRTC
     C     *LIKE         DEFINE    CABRTC        XPBRTC
     C*** END CHG *** 05/20/98 ****************************************
     C     *LIKE         DEFINE    CAPAID        XXTOTP
     C*
     C     *LIKE         DEFINE    DHPRCD        XXCPT4
     C     *LIKE         DEFINE    DHPRCD        XXCPCD
     C     *LIKE         DEFINE    CAFDAT        XXDATE
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    CAFDAT        XXEFDT
     C*** BEGIN DEL *** 08/09/02 **************************************
     C********** *LIKE     DEFN GZFRZP    XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C     *LIKE         DEFINE    PRMRA         XXMRA
     C     *LIKE         DEFINE    DHAREA        XXPART
     C     *LIKE         DEFINE    CAPMOD        XXPMOD
     C     *LIKE         DEFINE    CAANMN        XXUNIT
     C     *LIKE         DEFINE    CAANMN        CNTMIN
     C     *LIKE         DEFINE    CAFDAT        XXYMD
     C     *LIKE         DEFINE    CAFDAT        XXFDAT
     C*** BEGIN CHG *** 08/09/02 **************************************
     C********** *LIKE     DEFN GZZIPA    XXZIPA
     C     *LIKE         DEFINE    RCZIPA        XXZIPA
     C*** END   CHG *** 08/09/02 **************************************
     C*
     C*** BEG ADD *** 09/21/98 ****************************************
     C     *LIKE         DEFINE    MGMSG#        XXMSG#
     C     *LIKE         DEFINE    MGMSEQ        XXMSEQ
     C     *LIKE         DEFINE    DEESRC        QQESRC
     C     *LIKE         DEFINE    DEDSEQ        QQDSEQ
     C     *LIKE         DEFINE    DENOT1        XXNOT1
     C     *LIKE         DEFINE    DERCN         XXRCN
     C     *LIKE         DEFINE    DHUNTS        XXUNTS
     C*** END ADD *** 09/21/98 ****************************************
     C                   Z-ADD     0             Q                 3 0
     C                   Z-ADD     0             S                 3 0
     C                   Z-ADD     0             T                 5 0
     C                   Z-ADD     0             X                 5 0
     C                   Z-ADD     0             COUNT             5 0
     C                   Z-ADD     0             COUNT2            5 0
     C                   Z-ADD     0             COUNT3            5 0
     C                   Z-ADD     0             FUTRCT            5 0
     C                   Z-ADD     0             PASTCT            5 0
     C                   Z-ADD     0             CNTB1             5 0
     C                   Z-ADD     0             CNTB2             5 0
     C                   Z-ADD     0             XXAMT             7 2
     C                   Z-ADD     0             XXDIFF            7 2
     C                   Z-ADD     0             HLADDT            7 7          To hold MVR
     C                   Z-ADD     0             HLANMN           10 7          CAANMN for 15 minute
     C                   Z-ADD     0             HLANTV            3 0          Anes Time Value
     C                   Z-ADD     0             XXANTV            5 2          Anes WORKFIELD
     C                   MOVE      ' '           OPNFLG            1            File Open Flag
     C                   MOVE      '0'           OFCPD             1            CLEAR TMP IN(NOT USD
     C                   MOVE      ' '           OVRFLG            1            File Override Flag
     C                   Z-ADD     0             SRFUT             5 0
     C                   Z-ADD     0             SRPST             5 0
      *** BEG CHG *** 09/21/98 ****************************************
     C******************** MOVE *BLANKS   SRDTE   8
     C                   MOVE      *ZEROS        SRDTE             8 0
      *** BEG CHG *** 09/21/98 ****************************************
     C                   MOVE      *BLANKS       SRERR             1
     C                   MOVE      *BLANKS       SRTERM            1
     C                   MOVE      *BLANKS       SVSPCD            3
     C                   MOVE      ' '           XFIRST            1            First time Wrkf buil
     C                   Z-ADD     0             XXANMN            5 2
     C                   MOVE      *BLANKS       XXEND             2
     C*** BEGIN CHG *** 09/25/01 **************************************
     C**********           Z-ADD0         XXOVRP  32
     C                   Z-ADD     0             XXOVRP            5 4
     C*** END   CHG *** 09/25/01 **************************************
     C                   Z-ADD     0             XXPERC            5 2
     C                   Z-ADD     0             XXWORK           10 7
     C                   Z-ADD     0             TTANMN           10 7
     C                   MOVE      '0'           XX25EM            1
     C                   MOVE      *BLANKS       QQTYPE            1
     C                   Z-ADD     0             XVUNTS            7 4
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD0         XXUNTS  74
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     0             X1ANMN            7 4
     C                   Z-ADD     0             XXUNT             7 4
     C                   Z-ADD     0             PRADIO           10 2
     C                   Z-ADD     0             LRADIO           10 2
     C                   Z-ADD     0             CNTADD            5 0
     C*** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'XX00000'     XXMSG#
     C*** END ADD *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     040.00        XXANES            5 2           ANES Conversion
     C                   Z-ADD     035.00        XXCANE            5 2           CERT ANES Conv
     C* ===============================================================
     C*
     C*  Get the user parameter file
     C     UPKY          CHAIN     PUP01                              60
     C*
     C*  If the get was unsuccessful, end the program
     C     *IN60         IFEQ      '1'
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** MOVE '99999'   QQS,1
     C******************** MOVE '95'      QQC,1
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '95'          XXMSG#
     C                   MOVE      '99999'       XXDSEQ
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 95: "Invalid User Parameter, See Programmer"
     C                   GOTO      TERM
     C                   END
     C*
     C*  Get the Claim Master Record
     C     CMKY          CHAIN     PCM01                              60
     C*
     C*  If the get was unsuccessful, end the program
     C     *IN60         IFEQ      '1'
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** MOVE '99998'   QQS,1
     C******************** MOVE '96'      QQC,1
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '96'          XXMSG#
     C                   MOVE      '99998'       XXDSEQ
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 96: Invalid Case Number, See Programmer
     C                   GOTO      TERM
     C                   END
     C*
     C*** XXPASS - USED AS FLAG TO ZERO OUT PAID FIELDS WHEN ERRORS
     C*** OCCUR ON FIRST PASS OF DATA
     C*** XXPASS, 1 = FIRST PASS;  2 = SECOND PASS
     C*
     C                   MOVE      '1'           XXPASS            1
      *** BEG DEL *** 09/21/98 ****************************************
      *******************
     C*******************
     C****Do*on*each*date brought in ********************************
     C*******************
     C***Do*the*adjudication for each date entered,if after 04/01/96
     C*******************  DO   50        S
     C*******************  MOVE QQF,S     XXDATE
     C*******************  Z-ADDXXDATE    SRDTI
     C*******************  CALL 'SRCVTDAT'DTCYMD
     C*******************  MOVELSRDTO     XXYMD
     C*******************
     C***********QQF,S***  IFNE *ZERO
     C***********XXYMD***  ANDGE960401
     C*******************  MOVE '0'       OFCPD
     C********** CAKY1***  CHAINPCA01                54
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
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
     C*
     C     CAKY2         SETLL     PCA13
     C     CAKY2         READE     PCA13                                  54
      *
     *** BEG ADD *** 04/20/99 *****************************************
     C                   Z-ADD     CADSEQ        SVDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAUNTS        SVUNTS
     C                   MOVEL     DHPRCD        XPCPT4
     C                   MOVEL     CAPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C*
     C                   MOVE      QQSPCD        SVSPCD
     C*
     C                   EXSR      LODFU                                        LOAD FOLLOW-UP
     C*
     *** END ADD *** 04/20/99 *****************************************
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
      *
     C     CAFDAT        IFGE      960401
     C     XXRCN         ANDEQ     CARCN
      *
     C                   Z-ADD     CAFDAT        XXYMD
     C                   MOVE      CADSEQ        XXDSEQ
     C                   MOVE      *BLANKS       XPEOMB
      *** END ADD *** 09/21/98 ****************************************
     C*
     *** BEG DEL *** 04/20/99 *****************************************
     C******************** Z-ADDCADSEQ    SVDSEQ           SAVE FIELD VALS
     C******************** MOVE CAFEIN    SVFEIN           FOR RECORD WE
     C******************** MOVE CAMOD     SVMOD            ARE PROCESSING
     C******************** MOVE CAPRTY    SVPRTY
     C******************** Z-ADDCAUNTS    SVUNTS
     C******************** MOVELDHPRCD    XPCPT4
     C******************** MOVELCAPMOD    XPPMOD
     C******************** Z-ADDCAEOB#    SVEOB#
      ****BEG*DEL*****09/21/98 ****************************************
     C******************** Z-ADDCACHK#    SVCHK#
      ****BEG*DEL*****09/21/98 ****************************************
     C********************
     C******************** MOVE CAFEIN    SXFEIN           SET PROVIDER WE
     C******************** MOVE CAMOD     SXMOD            ARE PROCESSING
     C********************
     C******************** MOVE QQSPCD    SVSPCD
     C********************
     C********************
     C******************** EXSR LODFU                      LOAD FOLLOW-UP
     *** END DEL *** 04/20/99 *****************************************
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C*** DOWEQ on FDCAP file****************************************
     C********************
     C********** *IN54 *** DOWEQ'0'                        DO FOR EACH RCD
     C******************** EXCPTECA01
      *** END DEL *** 09/21/98 ****************************************
     C*
     C* If the Service Date is greater than the system date, give err message
     C     CAFDAT        IFGT      SYDATE
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '79'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '79'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg 79: "Service Date entered is greater than today's date
     C*                (system date)."
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END EQ *ZEROS
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END GT SYDATE
     C*
     C                   MOVE      '0'           XX25EM
     C*
     C*  Get the claim status record.  See if Override Flag is 'Y'.
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C*
     C     *IN60         IFEQ      '1'
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '97'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '97'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 97: "Invalid Claim Status Code, See Programmer"
     C                   END
      *** BEG ADD *** 09/21/98 ****************************************
     C*
     C* Load modifiers into an array to be processed later.  Load units
     C* and place of service into holding fields
     C                   CLEAR                   MOD
     C                   Z-ADD     CADSEQ        XXDSEQ
     C*
     C     DHKY          CHAIN     PDH01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     DHMOD1        MOD(1)
     C                   MOVEL     DHMOD2        MOD(2)
     C                   MOVEL     DHMOD3        MOD(3)
     C                   MOVEL     DHMOD4        MOD(4)
     C*
     C                   MOVEA     MOD           MOD1              8
     C*
     C                   ENDIF                                                  *IN60=OFF
      *** END ADD *** 09/21/98 ****************************************
     C*
     C*** NOT OVERRIDE ************************************************
     C*
     C* If override flag is not equal to 'Y', then process this record
     C*    Else do not adjudicate.
     C*
     C     CSOVRD        IFNE      'Y'
     C*
     C*== R&C =========================================================
     C* Check to see if this record is a Reasonable/Customary record
     C     CARCFL        IFEQ      'Y'                                           REAS/CUST
     C*
     C                   MOVEL     DHPRCD        XXCPCD
     C                   MOVE      *BLANKS       XPEOMB
     C                   MOVE      *ZEROS        XPTOTA
     C                   MOVE      *ZEROS        XPTOTP
     C                   MOVE      *ZEROS        XPTOTD
     C                   MOVE      *ZEROS        XPBRTC
     C*
     C*** BEGIN DEL *** 08/09/02 **************************************
     C**********           MOVE *BLANKS   XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C                   MOVE      *BLANKS       XXZIPA
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C* Now that the Provider is established, get its Geographic Zip Area
     C* 1st - establish the first 3-bytes of the zip code
     C*** BEGIN DEL *** 08/09/02 **************************************
     C**********           MOVELPDZIP     XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C                   END                                                    END 0 *IN60
     C*
     C* 2nd - Get the corresponding Geographic Zip Area record:
     C*** BEGIN CHG *** 08/09/02 **************************************
     C********** GZKY      SETGTPGZ01
     C**********           READPPGZ01                    60
     C********** *IN60     IFEQ '0'
     C********** XXFRZP    ANDLEGZTOZP
     C**********           MOVE GZZIPA    XXZIPA
     C**********           END                             END 0 *IN60
     C                   MOVE      CAZIPA        XXZIPA
     C*** END   CHG *** 08/09/02 **************************************
     C*
     C     RCKY          SETGT     PRC01
     C     NXRC1         TAG
     C                   READP     PRC01                                  60
     C     *IN60         IFEQ      '0'
     C     RCCO#         ANDEQ     QQCO#
     C     RCZIPA        ANDEQ     XXZIPA
     C     RCCPT4        ANDEQ     XXCPCD
     C     RCSTAT        IFNE      'I'
     C*
     C*** IF PROCEDURE IS "BY-REPORT" FLAG IT AS SUCH AND UPDATE RECORD.
     C     RCRPT         IFEQ      'Y'
     C*** IF MODIFIER EXISTS CHECK THE CODE THROUGH THE MODIFIER SEC.
      *** BEG CHG *** 09/21/99 ***************************************
     C********** CAPMOD    IFNE *BLANK
     C     MOD1          IFNE      *BLANK
      *** END CHG *** 09/21/99 ***************************************
     C*
     C                   EXSR      PMOD
     C     CACHRG        COMP      XXMRA                              60
     C   60              Z-ADD     XXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   ELSE                                                   ELSE CAPMOD NE *BLAN
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CACHK#    IFEQ *ZERO
     C********** CAEOB#    ANDEQ*ZERO
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '91'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '91'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   END                                                    END CAPMOD NE *BLANK
     C*
     C                   ELSE                                                   ELSE RCRPT EQ 'Y'
     C*
     C     CACHRG        COMP      RCMRA                              61
     C   61              MOVE      'M  '         XPEOMB
     C* EOMB CODE M:   "REDUCED TO FAIR AND REASONABLE"
     C   61              Z-ADD     RCMRA         XPTOTA
     C  N61              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   END                                                    END RCRPT EQ 'Y'
     C*
     C                   ELSE                                                    ELSE RCSTAT NE "I"
     C                   GOTO      NXRC1
     C                   END                                                     END RCSTAT NE "I"
     C                   END                                                     END 0 *IN60
     C*
      *** BEG DEL *** 09/21/99 ****************************************
     C********** CAKY1A    CHAINPCA01                54
     C********** *IN54     IFEQ '0'
      *** END DEL *** 09/21/99 ****************************************
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
      *** BEG DEL *** 09/21/99 ****************************************
     C******************** END
      *** END DEL *** 09/21/99 ****************************************
     C*
     C                   END                                                     END CARCFL = Y
     C*
     C*================================================================
     C*
     C*   Check for level break (Provider)
     C     SXFEIN        IFNE      CAFEIN
     C     SXMOD         ORNE      CAMOD
     C                   EXSR      NEWPRO
     C                   END
     C*
     C*== Hospital ====================================================
     C*
     C*   Hospital Subroutine or Regular Providers or PHARMACY
     C     CARCFL        IFEQ      'S'
     C     CARCFL        OREQ      'N'
     C*** BEGIN ADD *** 08/28/96 **************************************
     C     CARCFL        OREQ      'X'
     C*** END   ADD *** 08/28/96 **************************************
     C                   GOTO      NXTRCD
     C*
     C*== Non-Hospital ================================================
     C*
     C                   ELSE                                                   Else eq 'N,S'
     C*
     C*  Test for Fee Schedule Reimbursement = 'Y'
     C                   MOVEL     DHPRCD        XXCPT4
     C                   MOVE      CAFDAT        XXEFDT
     C*
     C     PRKY          SETGT     PPR01                                         GET PROC CODE
     C     NXTPR2        TAG
     C                   READP     PPR01                                  60     RECORD
     C*
     C*  A hard error will cause the program to terminate ASAP.
     C     *IN60         IFEQ      '1'
     C     PRCPT4        ORNE      DHPRCD
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '98'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '98'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 98: "Invalid Procedure Code, See Programmer"
     C                   GOTO      TERM
     C                   END
     C     PRSTAT        CABEQ     'I'           NXTPR2
     C*
     C*== FS Reimbursement = "N" ======================================
     C*
     C*  If fee schedule reimbursement equals 'N', but the MRA is equal
     C*     to zero, take the information as is.
     C     PRFSR         IFEQ      'N'
     C*
     C*  Flag to display Fee Schedule non-reimburseable Error Message
     C                   MOVE      '1'           XX25EM
     C*
     C     PRMRA         IFEQ      *ZEROS
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     CACHRG        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   ELSE
     C*
     C*  If fee schedule reimbursement equals 'N' But the MRA
     C*    is greater than zero, this CPT4 Code has been assigned a
     C*    specific MRA by the insurance company.
     C     CACHRG        COMP      PRMRA                              60
     C   60              MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C   60              Z-ADD     PRMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   END
     C                   END
     C*
     C*====================================================================
     C*== FS Reimbursement = "Y" ==========================================
     C*
     C*  If Fee Schedule Reimbursement equals 'Y', use MRA
     C*
     C*  Save the procedure code file values.
     C     PRFSR         IFEQ      'Y'
     C                   Z-ADD     PRMRA         SXMRA
     C                   Z-ADD     PRANAM        SXANAM
     C                   Z-ADD     PRPROF        SXPROF
     C                   MOVE      PRRPT         SXRPT
     C                   MOVE      PRSUPP        SXSUPP
     C                   MOVE      PRSURG        SXSURG
     C                   MOVE      PR92N1        SX92N1
     C                   MOVE      PR1A1         SX1A1
     C                   MOVE      PR1A2         SX1A2
     C                   MOVE      PR1A3         SX1A3
     C                   MOVE      PR1A4         SX1A4
     C                   MOVE      PR1A5         SX1A5
     C*
     C* IF 'ASC' AND 'ASC GROUP' > 0 THEN DO NOT LOOK AT BY REPORT FLAG
     C*
     C     SVSPCD        IFEQ      'ASC'
     C     SX92N1        ANDGT     *ZEROS
     C                   GOTO      PASRPT
     C                   ELSE
     C*
     C*
     C*--------------------------------------------------------------------
     C*-- By Report -------------------------------------------------------
     C     PRRPT         IFEQ      'Y'
     C*
     C**** BEGIN ADD *** 05/13/96 *************************************
     C     DHPRCD        IFEQ      '99455'
     C     DHPRCD        OREQ      '99456'
     C     DHPRCD        OREQ      '99499'
     C* Check if CODE has already been paid
     C                   MOVE      DHPRCD        XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9         READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRDSEQ        IFNE      CADSEQ
     C     XRTOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DV'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DV'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DV: "Only one of these procedure codes is allowed,
     C*             per date (adjust part code)."
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END XRDSEQ N
     C     CAKY9         READE     PCA09                                  60
     C                   END                                                    END *IN60
     C                   END                                                    END DHPRCD
     C*
     C* PER P. 16 Specialty areas reimbursement 99456
     C*    and shall be reimbursed at $450.00.
     C* PER P. 17 Specialty Areas Reimbursement 99499
     C*    and shall be reimbursed at $350.00.
     C     DHAREA        IFEQ      'SA'
     C     DHPRCD        ANDEQ     '99456'
     C     DHAREA        OREQ      'SA'
     C     DHPRCD        ANDEQ     '99499'
     C     DHPRCD        IFEQ      '99456'
     C                   Z-ADD     450           SXMRA
     C                   END
     C     DHPRCD        IFEQ      '99499'
     C                   Z-ADD     350           SXMRA
     C                   END
     C*
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*** BEGIN CHG *** 04/03/97 **************************************
     C*****************    Z-ADD*ZEROS    XPBRTC
     C                   Z-ADD     XPTOTA        XPBRTC
     C*** END   CHG *** 04/03/97 **************************************
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   END                                                    END DHAREA
     C**** END   ADD *** 05/13/96 *************************************
     C*   If modifier exists check the code through the Modifier Subr
      *** BEG CHG *** 09/21/99 ***************************************
     C********** CAPMOD    IFNE *BLANK
     C     MOD1          IFNE      *BLANK
      *** END CHG *** 09/21/99 ***************************************
     C                   EXSR      PMOD
     C*
     C* PER P. 15 Review of a report only = 50.00
     C     DHPRCD        IFEQ      '99455'
      *** BEG CHG *** 09/21/99 ***************************************
     C********** CAPMOD    ANDEQ'RP'
     C**********           Z-ADD50        SXMRA
     C     'RP'          LOOKUP    MOD(1)                                 99
     C   99              Z-ADD     50            SXMRA
      *** END CHG *** 09/21/99 ***************************************
     C                   END
     C*
     C**** BEGIN ADD *** 05/13/96 *************************************
     C*
     C* PER P. 14 Maximum Medical Improvement and/or Impairment Rating
     C     DHPRCD        IFEQ      '99455'
      *** BEG DEL *** 09/21/99 ***************************************
     C********** CAPMOD    ANDNE'RP'
     C* Load corresponding office visit code, depending on MOD
     C******************** SELEC
      *** END DEL *** 09/21/99 ***************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C*
     C* Load corresponding office visit code, depending on MOD
     C     'RP'          LOOKUP    MOD(1)                                 99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVE      *OFF          XXMDFL            1
     C*
     C     'L1'          LOOKUP    MOD(1)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      '99211'       XXCPT4
     C                   MOVE      *ON           XXMDFL
     C*
     C                   ENDIF                                                  *IN99=ON
     C*
     C     'L2'          LOOKUP    MOD(1)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      '99212'       XXCPT4
     C                   MOVE      *ON           XXMDFL
     C*
     C                   ENDIF                                                  *IN99=ON
     C*
     C     'L3'          LOOKUP    MOD(1)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      '99213'       XXCPT4
     C                   MOVE      *ON           XXMDFL
     C*
     C                   ENDIF                                                  *IN99=ON
     C*
     C     'L4'          LOOKUP    MOD(1)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      '99214'       XXCPT4
     C                   MOVE      *ON           XXMDFL
     C*
     C                   ENDIF                                                  *IN99=ON
     C*
     C     'L5'          LOOKUP    MOD(1)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      '99215'       XXCPT4
     C                   MOVE      *ON           XXMDFL
     C*
     C                   ENDIF                                                  *IN99=ON
      *** END ADD *** 09/21/98 ****************************************
     C*
      *** BEG DEL *** 09/21/99 ***************************************
     C********** CAPMOD    WHEQ 'L1'
     C**********           MOVE '99211'   XXCPT4
     C********** CAPMOD    WHEQ 'L2'
     C**********           MOVE '99212'   XXCPT4
     C********** CAPMOD    WHEQ 'L3'
     C**********           MOVE '99213'   XXCPT4
     C********** CAPMOD    WHEQ 'L4'
     C**********           MOVE '99214'   XXCPT4
     C********** CAPMOD    WHEQ 'L5'
     C**********           MOVE '99215'   XXCPT4
     C**********           OTHER
      *** END DEL *** 09/21/99 ***************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '10'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     XXMDFL        IFEQ      *OFF
     C*
      *** END ADD *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '10'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ENDIF                                                  XXMDFL=OFF
      *** END ADD *** 09/21/98 ****************************************
     C*
      *** BEG DEL *** 09/21/98 ***************************************
     C******************** ENDSL
      *** END DEL *** 09/21/98 ***************************************
     C*
     C* Get Office visit rate
     C                   MOVE      CAFDAT        XXEFDT
     C                   Z-ADD     *ZEROS        XXMRA
     C*
     C     PRKY          SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         DOWEQ     '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        IFEQ      'A'
     C                   Z-ADD     PRMRA         XXMRA
     C                   LEAVE
     C                   ENDIF                                                  END PRSTAT
     C*
     C                   READP     PPR01                                  54
     C                   ENDDO                                                  END DOW 54
     C*
     C*
     C* Find the rate to apply to the base rate
     C                   SELECT
     C     DHAREA        WHENEQ    '1B'
     C                   ADD       300           XXMRA
     C     DHAREA        WHENEQ    '1S'
     C                   ADD       350           XXMRA
     C     DHAREA        WHENEQ    '2B'
     C                   ADD       450           XXMRA
     C     DHAREA        WHENEQ    '2S'
     C                   ADD       500           XXMRA
     C     DHAREA        WHENEQ    '3B'
     C                   ADD       600           XXMRA
     C     DHAREA        WHENEQ    '3S'
     C                   ADD       650           XXMRA
     C                   OTHER
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DW'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DW'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DW: "Invalid part code used with this item.
     C*             Re-enter item w/valid part code."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   ENDSL
      *** BEG ADD *** 09/21/98 ****************************************
     C*** Per Pg. 16 99455 with modifier 26 pays 80%. Modifier 27 pays
     C*** 20%.
     C     '26'          LOOKUP    MOD(1)                                 99
     C   99              MULT      .80           XXMRA
      *
     C     '27'          LOOKUP    MOD(1)                                 99
     C   99              MULT      .20           XXMRA
      *** END ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XXMRA         SXMRA
     C*
      *** BEG ADD *** 09/21/99 ***************************************
     C                   ENDIF                                                  *IN99=OFF
      *** END ADD *** 09/21/99 ***************************************
     C*
     C                   ENDIF                                                  END 99455
     C**** END   ADD *** 05/13/96 *************************************
     C*
     C* PER P. 17 Broken appointments coded as 99499-BA
     C*    and shall be reimbursed at $100.00.
     C     DHPRCD        IFEQ      '99499'
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    ANDEQ'BA'
     C******************** Z-ADD100       SXMRA
     C     'BA'          LOOKUP    MOD(1)                                 99
     C   99              Z-ADD     100           SXMRA
      *** END CHG *** 09/21/98 ****************************************
     C                   END
     C*
     C**** BEGIN ADD *** 05/13/96 *************************************
     C* PER P. 16 Broken appointments coded as 99456-BA
     C*    and shall be reimbursed at $100.00.
     C     DHPRCD        IFEQ      '99456'
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    ANDEQ'BA'
     C******************** Z-ADD100       SXMRA
     C     'BA'          LOOKUP    MOD(1)                                 99
     C   99              Z-ADD     100           SXMRA
      *** END CHG *** 09/21/98 ****************************************
     C                   END
     C* PER P. 15 Maximum Medical Improvement and/or Impairment Rating
     C     DHPRCD        IFEQ      '99456'
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAPMOD    ANDNE'BA'
      *** END DEL *** 09/21/98 ****************************************
     C*
     C* Load Rate of modifier
      *** BEG ADD *** 09/21/98 ****************************************
     C     'BA'          LOOKUP    MOD(1)                                 99
      *
     C     *IN99         IFEQ      *OFF
      *
     C                   MOVE      *OFF          XXMDFL
      *
     C     'L1'          LOOKUP    MOD(1)                                 99
      *
     C     *IN99         IFEQ      *ON
      *
     C                   MOVE      *ON           XXMDFL
     C                   Z-ADD     400           XXMRA
      *
     C                   ENDIF                                                  *IN99=ON
      *
     C     'L2'          LOOKUP    MOD(1)                                 99
      *
     C     *IN99         IFEQ      *ON
      *
     C                   MOVE      *ON           XXMDFL
     C                   Z-ADD     300           XXMRA
      *
     C                   ENDIF                                                  *IN99=ON
      *
     C     'L3'          LOOKUP    MOD(1)                                 99
      *
     C     *IN99         IFEQ      *ON
      *
     C                   MOVE      *ON           XXMDFL
     C                   Z-ADD     200           XXMRA
      *
     C                   ENDIF                                                  *IN99=ON
     C* ==============================================================
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C**********           SELEC
     C********** CAPMOD    WHEQ 'L1'
     C**********           Z-ADD400       XXMRA
     C********** CAPMOD    WHEQ 'L2'
     C**********           Z-ADD300       XXMRA
     C********** CAPMOD    WHEQ 'L3'
     C**********           Z-ADD200       XXMRA
     C**********           OTHER
      *** END DEL *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '10'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     XXMDFL        IFEQ      *OFF
      *** END ADD *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '10'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C******************** ENDSL
     C                   ENDIF                                                  XXMDFL=ON
      *** END CHG *** 09/21/98 ****************************************
     C*
     C* Find the rate to apply to the base rate
     C                   SELECT
     C     DHAREA        WHENEQ    '1B'
     C                   ADD       300           XXMRA
     C     DHAREA        WHENEQ    '1S'
     C                   ADD       350           XXMRA
     C     DHAREA        WHENEQ    '2B'
     C                   ADD       450           XXMRA
     C     DHAREA        WHENEQ    '2S'
     C                   ADD       500           XXMRA
     C     DHAREA        WHENEQ    '3B'
     C                   ADD       600           XXMRA
     C     DHAREA        WHENEQ    '3S'
     C                   ADD       650           XXMRA
     C                   OTHER
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DW'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DW'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DW: "Invalid part code used with this item.
     C*             Re-enter item w/valid part code."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   ENDSL
      *** BEG ADD *** 09/21/98 ****************************************
     C*** Per Pg. 16 99456 with modifier 26 pays 80%. Modifier 27 pays
     C*** 20%.
     C     '26'          LOOKUP    MOD(1)                                 99
     C   99              MULT      .80           XXMRA
      *
     C     '27'          LOOKUP    MOD(1)                                 99
     C   99              MULT      .20           XXMRA
      *** END ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XXMRA         SXMRA
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ENDIF                                                  *IN99=OFF
      *** END ADD *** 09/21/98 ****************************************
      *
     C                   ENDIF                                                  END 99456
     C***
     C* PER P. 16 Required Medical Examination RME (99499)
     C     DHPRCD        IFEQ      '99499'
     C*
     C* Load Rate of modifier
      *** BEG ADD *** 09/21/98 ****************************************
     C     'BA'          LOOKUP    MOD(1)                                 99
      *
     C     *IN99         IFEQ      *OFF
      *
     C                   MOVE      *OFF          XXMDFL
      *
     C     'L1'          LOOKUP    MOD(1)                                 99
      *
     C     *IN99         IFEQ      *ON
      *
     C                   MOVE      *ON           XXMDFL
     C                   Z-ADD     300           XXMRA
      *
     C                   ENDIF                                                  *IN99=ON
      *
     C     'L2'          LOOKUP    MOD(1)                                 99
      *
     C     *IN99         IFEQ      *ON
      *
     C                   MOVE      *ON           XXMDFL
     C                   Z-ADD     200           XXMRA
      *
     C                   ENDIF                                                  *IN99=ON
      *
     C     'L3'          LOOKUP    MOD(1)                                 99
      *
     C     *IN99         IFEQ      *ON
      *
     C                   MOVE      *ON           XXMDFL
     C                   Z-ADD     100           XXMRA
      *
     C                   ENDIF                                                  *IN99=ON
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAPMOD    ANDNE'BA'
     C**********           SELEC
     C********** CAPMOD    WHEQ 'L1'
     C**********           Z-ADD300       XXMRA
     C********** CAPMOD    WHEQ 'L2'
     C**********           Z-ADD200       XXMRA
     C********** CAPMOD    WHEQ 'L3'
     C**********           Z-ADD100       XXMRA
     C**********           OTHER
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '10'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     XXMDFL        IFEQ      *OFF
      *
     C                   MOVE      '10'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C******************** ENDSL
     C                   ENDIF                                                  XXMDFL=OFF
      *** END CHG *** 09/21/98 ****************************************
     C*
     C* Find the rate to apply to the base rate
     C                   SELECT
     C     DHAREA        WHENEQ    '1B'
     C                   ADD       300           XXMRA
     C     DHAREA        WHENEQ    '1S'
     C                   ADD       350           XXMRA
     C     DHAREA        WHENEQ    '2B'
     C                   ADD       450           XXMRA
     C     DHAREA        WHENEQ    '2S'
     C                   ADD       500           XXMRA
     C     DHAREA        WHENEQ    '3B'
     C                   ADD       600           XXMRA
     C     DHAREA        WHENEQ    '3S'
     C                   ADD       650           XXMRA
     C                   OTHER
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DW'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DW'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DW: "Invalid part code used with this item.
     C*             Re-enter item w/valid part code."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   ENDSL
      *** BEG ADD *** 09/21/98 ****************************************
     C*** Per Pg. 16 99455 with modifier 26 pays 80%. Modifier 27 pays
     C*** 20%.
     C     '26'          LOOKUP    MOD(1)                                 99
     C   99              MULT      .80           XXMRA
      *
     C     '27'          LOOKUP    MOD(1)                                 99
     C   99              MULT      .20           XXMRA
      *** END ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XXMRA         SXMRA
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ENDIF                                                  *IN99=OFF
      *** END ADD *** 09/21/98 ****************************************
     C                   ENDIF                                                  END 99499
     C**** END   ADD *** 05/13/96 *************************************
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*** BEGIN CHG *** 04/03/97 **************************************
     C*****************    Z-ADD*ZEROS    XPBRTC
     C                   Z-ADD     XPTOTA        XPBRTC
     C*** END   CHG *** 04/03/97 **************************************
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CACHK#    IFEQ *ZERO
     C********** CAEOB#    ANDEQ*ZERO
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '91'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '91'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 91: "Carrier to set payment on individual case basis
     C*             based on documentation"
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END                                                    END PRRPT EQ 'Y'
     C*
     C*--------------------------------------------------------------------
     C*-- ANE/Amount Not Established --------------------------------------
     C     PRRPT         IFEQ      'A'
     C     DHAREA        IFEQ      'IA'
     C     PRANAM        ANDGT     *ZERO
     C                   GOTO      PASRPT
     C                   END
     C*
     C*   If modifier exists check the code through the Modifier Subr
      *** BEG CHG *** 09/21/99 ***************************************
     C********** CAPMOD    IFNE *BLANK
     C     MOD1          IFNE      *BLANK
      *** END CHG *** 09/21/99 ***************************************
     C                   EXSR      PMOD
     C*
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CACHK#    IFEQ *ZERO
     C********** CAEOB#    ANDEQ*ZERO
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '24'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '24'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 24: "The item is ANE - requires further inspection
     C*             by an adjuster"
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END                                                    END PRRPT EQ 'A'
     C*
     C*--------------------------------------------------------------------
     C*-- INDIVIDUAL CONSIDERATION ----------------------------------------
     C     PRRPT         IFEQ      'I'
     C*
     C*   If modifier exists check the code through the Modifier Subr
      *** BEG CHG *** 09/21/99 ***************************************
     C********** CAPMOD    IFNE *BLANK
     C     MOD1          IFNE      *BLANK
      *** END CHG *** 09/21/99 ***************************************
     C                   EXSR      PMOD
     C*
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CACHK#    IFEQ *ZERO
     C********** CAEOB#    ANDEQ*ZERO
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '83'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '82'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 82: "This item is I.C. - requires further inspection
     C*             by an adjuster"
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '83'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '82'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 82: "This item is I.C. - requires further inspection
     C*             by an adjuster"
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END                                                    END PRRPT EQ 'I'
     C                   END                                                    END SVSPCD
     C     PASRPT        TAG
     C*
     C*================================================================
     C*== Normal Processing of Procedure Code =========================
     C*
     C*-----------------------------------------------------------------
     C*-- Execute all Subroutines here ---------------------------------
     C*
     C     DHPRCD        IFGE      '00100'                                      ANESTHESIA
     C     DHPRCD        ANDLE     '01999'
     C     DHPRCD        OREQ      '99100'
     C     DHPRCD        OREQ      '99116'
     C     DHPRCD        OREQ      '99135'
     C     DHPRCD        OREQ      '99140'
     C                   EXSR      ANES
     C*
     C                   ELSE                                                      ELSE ANES
     C*
     C     DHPRCD        IFGE      '10000'                                      SURGERY
     C     DHPRCD        ANDLE     '69979'
     C*-------------------------------------------------------------------
     C* If first time into program,
     C* or the first code after the workfile has been used
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the workfile is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALTX
     C                   CLOSE     FACAPTX
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALTX
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPTX
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACTX'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALTX
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPTX
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALTX
     C                   OPEN      FACAPTX
     C                   MOVE      '1'           OPNFLG
     C                   END
     C*
     C*     then, reset hold YMD field and provider/mod
     C                   Z-ADD     XXYMD         HDYMD
     C                   Z-ADD     CAFEIN        HDFEIN
     C                   Z-ADD     CAMOD         HDMOD
     C*
     C                   ELSE                                                   ELS XFIRST EQ ' '
     C*
     C* If this is a new date or new provider/modifier
     C     XXYMD         IFNE      HDYMD
     C     CAFEIN        ORNE      HDFEIN
     C     CAMOD         ORNE      HDMOD
     C*     then, Process all Codes for the existing date/provider
     C                   EXSR      SURG2
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALTX
     C                   CLOSE     FACAPTX
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALTX
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPTX
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACTX'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALTX
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPTX
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALTX
     C                   OPEN      FACAPTX
     C                   MOVE      '1'           OPNFLG
     C                   END
     C*
     C*     then, reset hold YMD field and provider/mod
     C                   Z-ADD     XXYMD         HDYMD
     C                   Z-ADD     CAFEIN        HDFEIN
     C                   Z-ADD     CAMOD         HDMOD
     C*
     C                   END
     C                   END                                                    END XFIRST EQ ' '
     C*-------------------------------------------------------------------
     C                   EXSR      SURG
     C*
     C                   ELSE                                                      ELSE SURG
     C*
     C     DHPRCD        IFGE      '70010'                                      RADIOLOGY
     C     DHPRCD        ANDLE     '79999'
     C                   EXSR      RADI
     C*
     C                   ELSE                                                      ELSE RADI
     C*
     C     DHPRCD        IFGE      '80002'                                      PATHOLOGY
     C     DHPRCD        ANDLE     '89399'
     C                   EXSR      PATH
     C*
     C                   ELSE                                                      ELSE PATH
     C****
     C     DHPRCD        IFGE      '90700'                                      MEDICAL
     C     DHPRCD        ANDLE     '99199'
     C     DHPRCD        ANDNE     '99100'
     C     DHPRCD        ANDNE     '99116'
     C     DHPRCD        ANDNE     '99135'
     C     DHPRCD        ANDNE     '99140'
     C                   EXSR      MEDI
     C*
     C                   ELSE                                                      ELSE MEDI
     C     DHPRCD        IFGE      '99201'                                      EVAL & MGMT
     C     DHPRCD        ANDLE     '99499'
     C                   EXSR      OFFI
     C*
     C                   ELSE                                                      ELSE EVAL
     C*
     C     DHPRCD        IFGE      'A0000'                                                       QUI
     C     DHPRCD        ANDLE     'A9999'
     C     DHPRCD        ORGE      'E0000'                                                       QUI
     C     DHPRCD        ANDLE     'E9999'
     C     DHPRCD        ORGE      'K0000'                                                       QUI
     C     DHPRCD        ANDLE     'K9999'
     C     DHPRCD        ORGE      'L0000'                                                       QUI
     C     DHPRCD        ANDLE     'L9999'
     C                   EXSR      DME
     C*
     C                   ELSE                                                      ELSE DME
     C*
     C                   EXSR      OTHE                                         OTHER CODES
     C*
     C                   END                                                       END DME
     C                   END                                                       END OFFI
     C                   END                                                       END MEDI
     C                   END                                                       END PATH
     C                   END                                                       END RADI
     C                   END                                                       END SURG
     C                   END                                                       END ANES
     C*
     C                   END
     C                   END
     C*
     C*** ELSE IS OVERRIDE ********************************************
     C                   ELSE                                                   OVERRIDE TYPE
     C*
     C*  Test for a possible duplicate claim.
     C     DHPRCD        IFNE      *BLANK
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAEOB#    ANDEQ*ZERO
      *** END DEL *** 09/21/98 ****************************************
     C                   MOVE      DHPRCD        XXCPT4
     C******************   MOVELCAPMOD    XXPMOD
     C     CAKYP2        CHAIN     PCA0P                              60
     C*
     C     DUPCL1        TAG
     C     *IN60         IFEQ      '0'
     C*
     C     CADSEQ        IFNE      PXDSEQ
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CADSEQ ** OREQ PXDSEQ
     C********** CAEOB# ** ANDNEPXEOB#
     C*
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    61
     C**N61*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '03'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '03'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C                   ELSE
     C*
     C     CAKYP2        READE     PCA0P                                  60
     C  N60              GOTO      DUPCL1
     C                   END
     C                   END
     C                   END
     C*
      *** BEG DEL *** 09/21/99 ****************************************
     C********** CAKY1A    CHAINPCA01                60
     C********** *IN60     IFEQ '0'
      *** END DEL *** 09/21/99 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     CAEOB#        IFEQ      QQEOB#
      *** END ADD *** 09/21/98 ****************************************
     C*   If override type, we must still output the allowed/disallowed
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAEOB#    IFEQ *ZERO
      *** END DEL *** 09/21/98 ****************************************
     C     CAMRA         ANDEQ     0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CAPAID        ANDEQ     0
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CATOTA    ANDEQ0
      *** END DEL *** 09/21/98 ****************************************
     C*
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     *ZEROS        XPTOTA
      *** END ADD *** 09/21/98 ****************************************
     C*   If there is an override percentage, calculate
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CACHRG        MULT      XXOVRP        XPTOTA
     C                   END
     C*
     C     CACHRG        SUB       XPTOTA        CAMRA
     C                   Z-ADD     XPTOTA        CAPAID
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*
      *** BEG CHG *** 09/21/99 ****************************************
     C******************** UPDATPCA01
     C                   UPDATE    PCA13
      *** END DEL *** 09/21/99 ****************************************
     C*
     C                   END
     C*
     C                   END                                                    END EQ OVRRD
     C*
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ENDIF                                                  CAFDAT GE
      *** END ADD *** 09/21/98 ****************************************
     C*
     C*** Process next record *****************************************
     C     NXTRCD        TAG
     C*
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAKY1     READEPCA01                    54  GET NEXT RCD
     C     CAKY2         READE     PCA13                                  54      GET NEXT RCD
      *** END CHG *** 09/21/98 ****************************************
     C*                                                      TO PROCESS
     C*
     C     *IN54         IFEQ      '0'                                          SAVE FIELDS OF
     C                   Z-ADD     CADSEQ        SVDSEQ                         CURRENT REC.
     C                   MOVE      CAFEIN        SVFEIN
     C                   MOVE      CAMOD         SVMOD
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAUNTS        SVUNTS
     C                   MOVEL     DHPRCD        XPCPT4
     C                   MOVEL     CAPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C                   ENDIF
     C*
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ENDDO                                                  *IN54=OFF
      *** END ADD *** 09/21/98 ****************************************
     C*
     C*** Execute SURG2 if a SURG Code has been processed
     C     OPNFLG        IFEQ      '1'
     C                   EXSR      SURG2                                        Process the SURG
     C                   END                                                    END OPNFLG '1'
     C*
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
     C******************** ENDDO                           *IN54=OFF
      *** END DEL *** 09/21/98 ****************************************
     C*****************************************************************
     C* THE FOLLOWING CODE WAS DUPLICATED TO CATCH ERRORS NOT FOUND
     C* ON FIRST PASS OF DATA.
     C*****************************************************************
     C*  DUP OF MAIN CODE !!
     C*
     C                   MOVE      ' '           XFIRST
     C*** XXPASS - USED AS FLAG TO ZERO OUT PAID FIELDS WHEN ERRORS
     C*** OCCUR ON FIRST PASS OF DATA
     C*** XXPASS, 1 = FIRST PASS;  2 = SECOND PASS
     C*
     C                   MOVE      '2'           XXPASS            1
     C*
     C*
     C* RELOAD VALUES FROM ENTRY QQS, AND QQC
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** MOVEASVQQS     QQS
     C******************** MOVEASVQQC     QQC
      *** END DEL *** 09/21/98 ****************************************
     C*
      *** BEG DEL *** 09/21/98 ****************************************
      *******************
     C***Do*the*adjudication for each date entered,if after 04/01/96
     C*******************  DO   50        S
     C*******************  MOVE QQF,S     XXDATE
     C*******************  Z-ADDXXDATE    SRDTI
     C*******************  CALL 'SRCVTDAT'DTCYMD
     C*******************  MOVELSRDTO     XXYMD
     C*******************
     C***********QQF,S***  IFNE *ZERO
     C***********XXYMD***  ANDGE960401
     C*******************  MOVE '0'       OFCPD
     C********** CAKY1 **  CHAINPCA01                54
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     CAKY2         SETLL     PCA13
     C     CAKY2         READE     PCA13                                  54
      *
     *** BEG ADD *** 04/20/99 *****************************************
     C                   Z-ADD     CADSEQ        SVDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAUNTS        SVUNTS
     C                   MOVEL     DHPRCD        XPCPT4
     C                   MOVEL     CAPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C*
     C                   MOVE      QQSPCD        SVSPCD
     C*
     C                   EXSR      LODFU                                        LOAD FOLLOW-UP
     C*
     *** END ADD *** 04/20/99 *****************************************
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
     C*
     C     CAFDAT        IFGE      960401
     C     XXRCN         ANDEQ     CARCN
     C*
     C                   Z-ADD     CAFDAT        XXYMD                                      ALS
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      *BLANKS       XPEOMB
     C*
     *** BEG DEL *** 04/20/99 *****************************************
     C******************** Z-ADDCADSEQ    SVDSEQ           SAVE FIELD VALS
     C******************** MOVE CAFEIN    SVFEIN           FOR RECORD WE
     C******************** MOVE CAMOD     SVMOD            ARE PROCESSING
     C******************** MOVE CAPRTY    SVPRTY
     C******************** Z-ADDCAUNTS    SVUNTS
     C******************** MOVELDHPRCD    XPCPT4
     C******************** MOVELCAPMOD    XPPMOD
     C******************** Z-ADDCAEOB#    SVEOB#
      ****BEG*DEL*****09/21/98 ****************************************
     C******************** Z-ADDCACHK#    SVCHK#
      ****END*DEL*****09/21/98 ****************************************
     C********************
     C******************** MOVE CAFEIN    SXFEIN           SET PROVIDER WE
     C******************** MOVE CAMOD     SXMOD            ARE PROCESSING
     C********************
     C******************** MOVE QQSPCD    SVSPCD
     C********************
     C******************** EXSR LODFU
     *** END DEL *** 04/20/99 *****************************************
     C*
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C*** DOWEQ on FDCAP file****************************************
      *******************
     C*******************
     C************IN54***  DOWEQ'0'                        DO FOR EACH RCD
     C*******************  EXCPTECA01
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      CADSEQ        XXDSEQ
     C                   MOVE      'XX00000'     XXMSG#
      *** END ADD *** 09/21/98 ****************************************
     C*
     C* If the Service Date is greater than the system date, give err message
     C     CAFDAT        IFGT      SYDATE
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '79'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '79'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg 79: "Service Date entered is greater than today's date
     C*                (system date)."
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END EQ *ZEROS
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END GT SYDATE
     C*
     C                   MOVE      '0'           XX25EM
     C*
     C*  Get the claim status record.  See if Override Flag is 'Y'.
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C*
     C     *IN60         IFEQ      '1'
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '97'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '97'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 97: "Invalid Claim Status Code, See Programmer"
     C                   END
      *** BEG ADD *** 09/21/98 ****************************************
     C*
     C* Load modifiers into an array to be processed later
     C                   CLEAR                   MOD
     C                   Z-ADD     CADSEQ        XXDSEQ
     C*
     C     DHKY          CHAIN     PDH01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     DHMOD1        MOD(1)
     C                   MOVEL     DHMOD2        MOD(2)
     C                   MOVEL     DHMOD3        MOD(3)
     C                   MOVEL     DHMOD4        MOD(4)
     C*
     C                   MOVEA     MOD           MOD1              8
     C*
     C                   ENDIF                                                  *IN60=OFF
      *** END ADD *** 09/21/98 ****************************************
     C*
     C*** NOT OVERRIDE ************************************************
     C*
     C* If override flag is not equal to 'Y', then process this record
     C*    Else do not adjudicate.
     C*
     C     CSOVRD        IFNE      'Y'
     C*
     C*== R&C =========================================================
     C* Check to see if this record is a Reasonable/Customary record
     C     CARCFL        IFEQ      'Y'                                           REAS/CUST
     C*
     C                   MOVEL     DHPRCD        XXCPCD
     C                   MOVE      *BLANKS       XPEOMB
     C                   MOVE      *ZEROS        XPTOTA
     C                   MOVE      *ZEROS        XPTOTP
     C                   MOVE      *ZEROS        XPTOTD
     C                   MOVE      *ZEROS        XPBRTC
     C*
     C*** BEGIN DEL *** 08/09/02 **************************************
     C**********           MOVE *BLANKS   XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C                   MOVE      *BLANKS       XXZIPA
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C* Now that the Provider is established, get its Geographic Zip Area
     C* 1st - establish the first 3-bytes of the zip code
     C*** BEGIN DEL *** 08/09/02 **************************************
     C**********           MOVELPDZIP     XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C                   END                                                    END 0 *IN60
     C*
     C* 2nd - Get the corresponding Geographic Zip Area record:
     C*** BEGIN CHG *** 08/09/02 **************************************
     C********** GZKY      SETGTPGZ01
     C**********           READPPGZ01                    60
     C********** *IN60     IFEQ '0'
     C********** XXFRZP    ANDLEGZTOZP
     C**********           MOVE GZZIPA    XXZIPA
     C**********           END                             END 0 *IN60
     C                   MOVE      CAZIPA        XXZIPA
     C*** END   CHG *** 08/09/02 **************************************
     C*
     C     RCKY          SETGT     PRC01
     C     N2RC1         TAG
     C                   READP     PRC01                                  60
     C     *IN60         IFEQ      '0'
     C     RCCO#         ANDEQ     QQCO#
     C     RCZIPA        ANDEQ     XXZIPA
     C     RCCPT4        ANDEQ     XXCPCD
     C     RCSTAT        IFNE      'I'
     C*
     C*** IF PROCEDURE IS "BY-REPORT" FLAG IT AS SUCH AND UPDATE RECORD.
     C     RCRPT         IFEQ      'Y'
     C*** IF MODIFIER EXISTS CHECK THE CODE THROUGH THE MODIFIER SEC.
      *** BEG CHG *** 09/21/99 ***************************************
     C********** CAPMOD    IFNE *BLANK
     C     MOD1          IFNE      *BLANK
      *** END CHG *** 09/21/99 ***************************************
     C*
     C                   EXSR      PMOD
     C     CACHRG        COMP      XXMRA                              60
     C   60              Z-ADD     XXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD
     C*
     C                   ELSE                                                   ELSE CAPMOD NE *BLAN
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '91'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '91'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD
     C                   END                                                    END CAPMOD NE *BLANK
     C*
     C                   ELSE                                                   ELSE RCRPT EQ 'Y'
     C*
     C     CACHRG        COMP      RCMRA                              61
     C   61              MOVE      'M  '         XPEOMB
     C* EOMB CODE M:   "REDUCED TO FAIR AND REASONABLE"
     C   61              Z-ADD     RCMRA         XPTOTA
     C  N61              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   END                                                    END RCRPT EQ 'Y'
     C*
     C                   ELSE                                                    ELSE RCSTAT NE "I"
     C                   GOTO      N2RC1
     C                   END                                                     END RCSTAT NE "I"
     C                   END                                                     END 0 *IN60
     C*
      *** BEG DEL *** 09/21/99 ****************************************
     C********** CAKY1A    CHAINPCA01                54
     C********** *IN54     IFEQ '0'
      *** END DEL *** 09/21/99 ****************************************
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD
      *** BEG DEL *** 09/21/99 ****************************************
     C******************** END
      *** END DEL *** 09/21/99 ****************************************
     C*
     C                   END                                                     END CARCFL = Y
     C*
     C*================================================================
     C*
     C*   Check for level break (Provider)
     C     SXFEIN        IFNE      CAFEIN
     C     SXMOD         ORNE      CAMOD
     C                   EXSR      NEWPRO
     C                   END
     C*
     C*== Hospital ====================================================
     C*
     C*   Hospital Subroutine or Regular Providers or PHARMACY
     C     CARCFL        IFEQ      'S'
     C     CARCFL        OREQ      'N'
     C*** BEGIN ADD *** 08/28/96 **************************************
     C     CARCFL        OREQ      'X'
     C*** END   ADD *** 08/28/96 **************************************
     C                   GOTO      N2TRCD
     C*
     C*== Non-Hospital ================================================
     C*
     C                   ELSE                                                   Else eq 'N,S'
     C*
     C*  Test for Fee Schedule Reimbursement = 'Y'
     C                   MOVEL     DHPRCD        XXCPT4
     C                   MOVE      CAFDAT        XXEFDT
     C*
     C     PRKY          SETGT     PPR01                                         GET PROC CODE
     C     N2TPR2        TAG
     C                   READP     PPR01                                  60     RECORD
     C*
     C*  A hard error will cause the program to terminate ASAP.
     C     *IN60         IFEQ      '1'
     C     PRCPT4        ORNE      DHPRCD
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '98'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '98'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 98: "Invalid Procedure Code, See Programmer"
     C                   GOTO      TERM
     C                   END
     C     PRSTAT        CABEQ     'I'           N2TPR2
     C*
     C*== FS Reimbursement = "N" ======================================
     C*
     C*  If fee schedule reimbursement equals 'N', but the MRA is equal
     C*     to zero, take the information as is.
     C     PRFSR         IFEQ      'N'
     C*
     C*  Flag to display Fee Schedule non-reimburseable Error Message
     C                   MOVE      '1'           XX25EM
     C*
     C     PRMRA         IFEQ      *ZEROS
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     CACHRG        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD                                       GET NEXT RECORD
     C*
     C                   ELSE
     C*
     C*  If fee schedule reimbursement equals 'N' But the MRA
     C*    is greater than zero, this CPT4 Code has been assigned a
     C*    specific MRA by the insurance company.
     C     CACHRG        COMP      PRMRA                              60
     C   60              MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C   60              Z-ADD     PRMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD                                       GET NEXT RECORD
     C                   END
     C                   END
     C*
     C*====================================================================
     C*== FS Reimbursement = "Y" ==========================================
     C*
     C*  If Fee Schedule Reimbursement equals 'Y', use MRA
     C*
     C*  Save the procedure code file values.
     C     PRFSR         IFEQ      'Y'
     C                   Z-ADD     PRMRA         SXMRA
     C                   Z-ADD     PRANAM        SXANAM
     C                   Z-ADD     PRPROF        SXPROF
     C                   MOVE      PRRPT         SXRPT
     C                   MOVE      PRSUPP        SXSUPP
     C                   MOVE      PRSURG        SXSURG
     C                   MOVE      PR92N1        SX92N1
     C                   MOVE      PR1A1         SX1A1
     C                   MOVE      PR1A2         SX1A2
     C                   MOVE      PR1A3         SX1A3
     C                   MOVE      PR1A4         SX1A4
     C                   MOVE      PR1A5         SX1A5
     C*
     C* IF 'ASC' AND 'ASC GROUP' > 0 THEN DO NOT LOOK AT BY REPORT FLAG
     C*
     C     SVSPCD        IFEQ      'ASC'
     C     SX92N1        ANDGT     *ZEROS
     C                   GOTO      P2SRPT
     C                   ELSE
     C*
     C*
     C*--------------------------------------------------------------------
     C*-- By Report -------------------------------------------------------
     C     PRRPT         IFEQ      'Y'
     C*
     C**** BEGIN ADD *** 05/13/96 *************************************
     C     DHPRCD        IFEQ      '99455'
     C     DHPRCD        OREQ      '99456'
     C     DHPRCD        OREQ      '99499'
     C* Check if CODE has already been paid
     C                   MOVE      DHPRCD        XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9         READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRDSEQ        IFNE      CADSEQ
     C     XRTOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DV'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DV'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DV: "Only one of these procedure codes is allowed,
     C*             per date (adjust part code)."
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD
     C*
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END XRDSEQ N
     C     CAKY9         READE     PCA09                                  60
     C                   END                                                    END *IN60
     C                   END                                                    END DHPRCD
     C*
     C* PER P. 16 Specialty areas reimbursement 99456
     C*    and shall be reimbursed at $450.00.
     C* PER P. 17 Specialty Areas Reimbursement 99499
     C*    and shall be reimbursed at $350.00.
     C     DHAREA        IFEQ      'SA'
     C     DHPRCD        ANDEQ     '99456'
     C     DHAREA        OREQ      'SA'
     C     DHPRCD        ANDEQ     '99499'
     C     DHPRCD        IFEQ      '99456'
     C                   Z-ADD     450           SXMRA
     C                   END
     C     DHPRCD        IFEQ      '99499'
     C                   Z-ADD     350           SXMRA
     C                   END
     C*
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*** BEGIN CHG *** 04/03/97 **************************************
     C*****************    Z-ADD*ZEROS    XPBRTC
     C                   Z-ADD     XPTOTA        XPBRTC
     C*** END   CHG *** 04/03/97 **************************************
     C*
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD                                       GET NEXT RECORD
     C                   END                                                    END DHAREA
     C**** END   ADD *** 05/13/96 *************************************
     C*   If modifier exists check the code through the Modifier Subr
      *** BEG CHG *** 09/21/99 ***************************************
     C********** CAPMOD    IFNE *BLANK
     C     MOD1          IFNE      *BLANK
      *** END CHG *** 09/21/99 ***************************************
     C                   EXSR      PMOD
     C*
     C* PER P. 15 Review of a report only = 50.00
     C     DHPRCD        IFEQ      '99455'
      *** BEG DEL *** 09/21/99 ***************************************
     C********** CAPMOD    ANDEQ'RP'
     C**********           Z-ADD50        SXMRA
      *** END DEL *** 09/21/99 ***************************************
      *** BEG ADD *** 09/21/99 ***************************************
     C     'RP'          LOOKUP    MOD(1)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   Z-ADD     50            SXMRA
     C*
     C                   ENDIF                                                  *IN99=ON
      *** END ADD *** 09/21/99 ***************************************
     C                   END
     C*
     C**** BEGIN ADD *** 05/13/96 *************************************
     C*
     C* PER P. 14 Maximum Medical Improvement and/or Impairment Rating
     C     DHPRCD        IFEQ      '99455'
      *** BEG ADD *** 09/21/98 ****************************************
     C*
     C* Load corresponding office visit code, depending on MOD
     C     'RP'          LOOKUP    MOD(1)                                 99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVE      *OFF          XXMDFL            1
     C*
     C     'L1'          LOOKUP    MOD(1)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      '99211'       XXCPT4
     C                   MOVE      *ON           XXMDFL
     C*
     C                   ENDIF                                                  *IN99=ON
     C*
     C     'L2'          LOOKUP    MOD(1)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      '99212'       XXCPT4
     C                   MOVE      *ON           XXMDFL
     C*
     C                   ENDIF                                                  *IN99=ON
     C*
     C     'L3'          LOOKUP    MOD(1)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      '99213'       XXCPT4
     C                   MOVE      *ON           XXMDFL
     C*
     C                   ENDIF                                                  *IN99=ON
     C*
     C     'L4'          LOOKUP    MOD(1)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      '99214'       XXCPT4
     C                   MOVE      *ON           XXMDFL
     C*
     C                   ENDIF                                                  *IN99=ON
     C*
     C     'L5'          LOOKUP    MOD(1)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      '99215'       XXCPT4
     C                   MOVE      *ON           XXMDFL
     C*
     C                   ENDIF                                                  *IN99=ON
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAPMOD    ANDNE'RP'
     C**********
     C**Load*corresponding office visit code, depending on MOD
     C**********           SELEC
     C********** CAPMOD    WHEQ 'L1'
     C**********           MOVE '99211'   XXCPT4
     C********** CAPMOD    WHEQ 'L2'
     C**********           MOVE '99212'   XXCPT4
     C********** CAPMOD    WHEQ 'L3'
     C**********           MOVE '99213'   XXCPT4
     C********** CAPMOD    WHEQ 'L4'
     C**********           MOVE '99214'   XXCPT4
     C********** CAPMOD    WHEQ 'L5'
     C**********           MOVE '99215'   XXCPT4
     C**********           OTHER
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '10'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     XXMDFL        IFEQ      *OFF
      *
     C                   MOVE      '10'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C******************** ENDSL
     C                   ENDIF                                                  XXMDFL=OFF
      *** END CHG *** 09/21/98 ****************************************
     C*
     C* Get Office visit rate
     C                   MOVE      CAFDAT        XXEFDT
     C                   Z-ADD     *ZEROS        XXMRA
     C*
     C     PRKY          SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         DOWEQ     '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        IFEQ      'A'
     C                   Z-ADD     PRMRA         XXMRA
     C                   LEAVE
     C                   ENDIF                                                  END PRSTAT
     C*
     C                   READP     PPR01                                  54
     C                   ENDDO                                                  END DOW 54
     C*
     C*
     C* Find the rate to apply to the base rate
     C                   SELECT
     C     DHAREA        WHENEQ    '1B'
     C                   ADD       300           XXMRA
     C     DHAREA        WHENEQ    '1S'
     C                   ADD       350           XXMRA
     C     DHAREA        WHENEQ    '2B'
     C                   ADD       450           XXMRA
     C     DHAREA        WHENEQ    '2S'
     C                   ADD       500           XXMRA
     C     DHAREA        WHENEQ    '3B'
     C                   ADD       600           XXMRA
     C     DHAREA        WHENEQ    '3S'
     C                   ADD       650           XXMRA
     C                   OTHER
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DW'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DW'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DW: "Invalid part code used with this item.
     C*             Re-enter item w/valid part code."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD
     C*
     C                   ENDSL
      *** BEG ADD *** 09/21/98 ****************************************
     C*** Per Pg. 16 99455 with modifier 26 pays 80%. Modifier 27 pays
     C*** 20%.
     C     '26'          LOOKUP    MOD(1)                                 99
     C   99              MULT      .80           XXMRA
      *
     C     '27'          LOOKUP    MOD(1)                                 99
     C   99              MULT      .20           XXMRA
      *** END ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XXMRA         SXMRA
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ENDIF                                                  *IN99=OFF
      *** END ADD *** 09/21/98 ****************************************
     C                   ENDIF                                                  END 99455
     C**** END   ADD *** 05/13/96 *************************************
     C* PER P. 17 Broken appointments coded as 99499-BA
     C*    and shall be reimbursed at $100.00.
     C     DHPRCD        IFEQ      '99499'
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    ANDEQ'BA'
     C**********           Z-ADD100       SXMRA
     C     'BA'          LOOKUP    MOD(1)                                 99
     C   99              Z-ADD     100           SXMRA
      *** END CHG *** 09/21/98 ****************************************
     C                   END
     C*
     C**** BEGIN ADD *** 05/13/96 *************************************
     C* PER P. 16 Broken appointments coded as 99456-BA
     C*    and shall be reimbursed at $100.00.
     C     DHPRCD        IFEQ      '99456'
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    ANDEQ'BA'
     C**********           Z-ADD100       SXMRA
     C     'BA'          LOOKUP    MOD(1)                                 99
     C   99              Z-ADD     100           SXMRA
      *** END CHG *** 09/21/98 ****************************************
     C                   END
     C* PER P. 15 Maximum Medical Improvement and/or Impairment Rating
     C     DHPRCD        IFEQ      '99456'
      *** BEG ADD *** 09/21/98 ****************************************
     C     'BA'          LOOKUP    MOD(1)                                 99
      *
     C     *IN99         IFEQ      *OFF
      *
     C                   MOVE      *OFF          XXMDFL
      *
     C     'L1'          LOOKUP    MOD(1)                                 99
      *
     C     *IN99         IFEQ      *ON
      *
     C                   MOVE      *ON           XXMDFL
     C                   Z-ADD     400           XXMRA
      *
     C                   ENDIF                                                  *IN99=ON
      *
     C     'L2'          LOOKUP    MOD(1)                                 99
      *
     C     *IN99         IFEQ      *ON
      *
     C                   MOVE      *ON           XXMDFL
     C                   Z-ADD     300           XXMRA
      *
     C                   ENDIF                                                  *IN99=ON
      *
     C     'L3'          LOOKUP    MOD(1)                                 99
      *
     C     *IN99         IFEQ      *ON
      *
     C                   MOVE      *ON           XXMDFL
     C                   Z-ADD     200           XXMRA
      *
     C                   ENDIF                                                  *IN99=ON
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
      **********
     C********** CAPMOD    ANDNE'BA'
     C**********
     C**Load*Rate of modifier
     C**********           SELEC
     C********** CAPMOD    WHEQ 'L1'
     C**********           Z-ADD400       XXMRA
     C********** CAPMOD    WHEQ 'L2'
     C**********           Z-ADD300       XXMRA
     C********** CAPMOD    WHEQ 'L3'
     C**********           Z-ADD200       XXMRA
     C**********           OTHER
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '10'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     XXMDFL        IFEQ      *OFF
      *
     C                   MOVE      '10'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C******************** ENDSL
     C                   ENDIF                                                  XXMDFL=OFF
      *** END CHG *** 09/21/98 ****************************************
     C*
     C* Find the rate to apply to the base rate
     C                   SELECT
     C     DHAREA        WHENEQ    '1B'
     C                   ADD       300           XXMRA
     C     DHAREA        WHENEQ    '1S'
     C                   ADD       350           XXMRA
     C     DHAREA        WHENEQ    '2B'
     C                   ADD       450           XXMRA
     C     DHAREA        WHENEQ    '2S'
     C                   ADD       500           XXMRA
     C     DHAREA        WHENEQ    '3B'
     C                   ADD       600           XXMRA
     C     DHAREA        WHENEQ    '3S'
     C                   ADD       650           XXMRA
     C                   OTHER
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DW'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DW'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DW: "Invalid part code used with this item.
     C*             Re-enter item w/valid part code."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD
     C*
     C                   ENDSL
      *** BEG ADD *** 09/21/98 ****************************************
     C*** Per Pg. 16 99456 with modifier 26 pays 80%. Modifier 27 pays
     C*** 20%.
     C     '26'          LOOKUP    MOD(1)                                 99
     C   99              MULT      .80           XXMRA
      *
     C     '27'          LOOKUP    MOD(1)                                 99
     C   99              MULT      .20           XXMRA
      *** END ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XXMRA         SXMRA
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ENDIF                                                  *IN99=OFF
      *** END ADD *** 09/21/98 ****************************************
     C                   ENDIF                                                  END 99456
     C***
     C***
     C* PER P. 16 Required Medical Examination RME (99499)
     C     DHPRCD        IFEQ      '99499'
      *** BEG ADD *** 09/21/98 ****************************************
     C     'BA'          LOOKUP    MOD(1)                                 99
      *
     C     *IN99         IFEQ      *OFF
      *
     C                   MOVE      *OFF          XXMDFL
      *
     C     'L1'          LOOKUP    MOD(1)                                 99
      *
     C     *IN99         IFEQ      *ON
      *
     C                   MOVE      *ON           XXMDFL
     C                   Z-ADD     300           XXMRA
      *
     C                   ENDIF                                                  *IN99=ON
      *
     C     'L2'          LOOKUP    MOD(1)                                 99
      *
     C     *IN99         IFEQ      *ON
      *
     C                   MOVE      *ON           XXMDFL
     C                   Z-ADD     200           XXMRA
      *
     C                   ENDIF                                                  *IN99=ON
      *
     C     'L3'          LOOKUP    MOD(1)                                 99
      *
     C     *IN99         IFEQ      *ON
      *
     C                   MOVE      *ON           XXMDFL
     C                   Z-ADD     100           XXMRA
      *
     C                   ENDIF                                                  *IN99=ON
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
      **********
     C********** CAPMOD    ANDNE'BA'
     C**********
     C**Load*Rate of modifier
     C**********           SELEC
     C********** CAPMOD    WHEQ 'L1'
     C**********           Z-ADD300       XXMRA
     C********** CAPMOD    WHEQ 'L2'
     C**********           Z-ADD200       XXMRA
     C********** CAPMOD    WHEQ 'L3'
     C**********           Z-ADD100       XXMRA
     C**********           OTHER
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '10'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     XXMDFL        IFEQ      *OFF
      *
     C                   MOVE      '10'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C******************** ENDSL
     C                   ENDIF                                                  XXMDFL=OFF
      *** END CHG *** 09/21/98 ****************************************
     C*
     C* Find the rate to apply to the base rate
     C                   SELECT
     C     DHAREA        WHENEQ    '1B'
     C                   ADD       300           XXMRA
     C     DHAREA        WHENEQ    '1S'
     C                   ADD       350           XXMRA
     C     DHAREA        WHENEQ    '2B'
     C                   ADD       450           XXMRA
     C     DHAREA        WHENEQ    '2S'
     C                   ADD       500           XXMRA
     C     DHAREA        WHENEQ    '3B'
     C                   ADD       600           XXMRA
     C     DHAREA        WHENEQ    '3S'
     C                   ADD       650           XXMRA
     C                   OTHER
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DW'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DW'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DW: "Invalid part code used with this item.
     C*             Re-enter item w/valid part code."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD
     C*
     C                   ENDSL
      *** BEG ADD *** 09/21/98 ****************************************
     C*** Per Pg. 16 99455 with modifier 26 pays 80%. Modifier 27 pays
     C*** 20%.
     C     '26'          LOOKUP    MOD(1)                                 99
     C   99              MULT      .80           XXMRA
      *
     C     '27'          LOOKUP    MOD(1)                                 99
     C   99              MULT      .20           XXMRA
      *** END ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XXMRA         SXMRA
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ENDIF                                                  *IN99=OFF
      *** END ADD *** 09/21/98 ****************************************
     C                   ENDIF                                                  END 99499
     C**** END   ADD *** 05/13/96 *************************************
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*** BEGIN CHG *** 04/03/97 **************************************
     C*****************    Z-ADD*ZEROS    XPBRTC
     C                   Z-ADD     XPTOTA        XPBRTC
     C*** END   CHG *** 04/03/97 **************************************
     C*
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '91'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '91'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 91: "Carrier to set payment on individual case basis
     C*             based on documentation"
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END                                                    END PRRPT EQ 'Y'
     C*
     C*--------------------------------------------------------------------
     C*-- ANE/Amount Not Established --------------------------------------
     C     PRRPT         IFEQ      'A'
     C     DHAREA        IFEQ      'IA'
     C     PRANAM        ANDGT     *ZERO
     C                   GOTO      P2SRPT
     C                   END
     C*
     C*   If modifier exists check the code through the Modifier Subr
      *** BEG CHG *** 09/21/99 ***************************************
     C********** CAPMOD    IFNE *BLANK
     C     MOD1          IFNE      *BLANK
      *** END CHG *** 09/21/99 ***************************************
     C                   EXSR      PMOD
     C*
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '24'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '24'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 24: "The item is ANE - requires further inspection
     C*             by an adjuster"
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END                                                    END PRRPT EQ 'A'
     C*
     C*--------------------------------------------------------------------
     C*-- INDIVIDUAL CONSIDERATION ----------------------------------------
     C     PRRPT         IFEQ      'I'
     C*
     C*   If modifier exists check the code through the Modifier Subr
      *** BEG CHG *** 09/21/99 ***************************************
     C********** CAPMOD    IFNE *BLANK
     C     MOD1          IFNE      *BLANK
      *** END CHG *** 09/21/99 ***************************************
     C                   EXSR      PMOD
     C*
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '83'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '82'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 82: "This item is I.C. - requires further inspection
     C*             by an adjuster"
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '83'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '82'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 82: "This item is I.C. - requires further inspection
     C*             by an adjuster"
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END                                                    END PRRPT EQ 'I'
     C                   END                                                    END SVSPCD
     C     P2SRPT        TAG
     C*
     C*================================================================
     C*== Normal Processing of Procedure Code =========================
     C*
     C*-----------------------------------------------------------------
     C*-- Execute all Subroutines here ---------------------------------
     C*
     C     DHPRCD        IFGE      '00100'                                      ANESTHESIA
     C     DHPRCD        ANDLE     '01999'
     C     DHPRCD        OREQ      '99100'
     C     DHPRCD        OREQ      '99116'
     C     DHPRCD        OREQ      '99135'
     C     DHPRCD        OREQ      '99140'
     C                   EXSR      ANES
     C*
     C                   ELSE                                                      ELSE ANES
     C*
     C     DHPRCD        IFGE      '10000'                                      SURGERY
     C     DHPRCD        ANDLE     '69979'
     C*-------------------------------------------------------------------
     C* If first time into program,
     C* or the first code after the workfile has been used
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the workfile is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALTX
     C                   CLOSE     FACAPTX
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALTX
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPTX
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACTX'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALTX
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPTX
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALTX
     C                   OPEN      FACAPTX
     C                   MOVE      '1'           OPNFLG
     C                   END
     C*
     C*     then, reset hold YMD field and provider/mod
     C                   Z-ADD     XXYMD         HDYMD
     C                   Z-ADD     CAFEIN        HDFEIN
     C                   Z-ADD     CAMOD         HDMOD
     C*
     C                   ELSE                                                   ELS XFIRST EQ ' '
     C*
     C* If this is a new date or new provider/modifier
     C     XXYMD         IFNE      HDYMD
     C     CAFEIN        ORNE      HDFEIN
     C     CAMOD         ORNE      HDMOD
     C*     then, Process all Codes for the existing date/provider
     C                   EXSR      SURG2
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALTX
     C                   CLOSE     FACAPTX
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALTX
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPTX
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACTX'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALTX
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPTX
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALTX
     C                   OPEN      FACAPTX
     C                   MOVE      '1'           OPNFLG
     C                   END
     C*
     C*     then, reset hold YMD field and provider/mod
     C                   Z-ADD     XXYMD         HDYMD
     C                   Z-ADD     CAFEIN        HDFEIN
     C                   Z-ADD     CAMOD         HDMOD
     C*
     C                   END
     C                   END                                                    END XFIRST EQ ' '
     C*-------------------------------------------------------------------
     C                   EXSR      SURG
     C*
     C                   ELSE                                                      ELSE SURG
     C*
     C     DHPRCD        IFGE      '70010'                                      RADIOLOGY
     C     DHPRCD        ANDLE     '79999'
     C                   EXSR      RADI
     C*
     C                   ELSE                                                      ELSE RADI
     C*
     C     DHPRCD        IFGE      '80002'                                      PATHOLOGY
     C     DHPRCD        ANDLE     '89399'
     C                   EXSR      PATH
     C*
     C                   ELSE                                                      ELSE PATH
     C*
     C     DHPRCD        IFGE      '90700'                                      MEDICAL
     C     DHPRCD        ANDLE     '99199'
     C     DHPRCD        ANDNE     '99100'
     C     DHPRCD        ANDNE     '99116'
     C     DHPRCD        ANDNE     '99135'
     C     DHPRCD        ANDNE     '99140'
     C                   EXSR      MEDI
     C*
     C                   ELSE                                                      ELSE MEDI
     C     DHPRCD        IFGE      '99201'                                      EVAL & MGMT
     C     DHPRCD        ANDLE     '99499'
     C                   EXSR      OFFI
     C*
     C                   ELSE                                                      ELSE EVAL
     C*
     C     DHPRCD        IFGE      'A0000'                                                       QUI
     C     DHPRCD        ANDLE     'A9999'
     C     DHPRCD        ORGE      'E0000'                                                       QUI
     C     DHPRCD        ANDLE     'E9999'
     C     DHPRCD        ORGE      'K0000'                                                       QUI
     C     DHPRCD        ANDLE     'K9999'
     C     DHPRCD        ORGE      'L0000'                                                       QUI
     C     DHPRCD        ANDLE     'L9999'
     C                   EXSR      DME
     C*
     C                   ELSE                                                      ELSE DME
     C*
     C                   EXSR      OTHE                                         OTHER CODES
     C*
     C                   END                                                       END DME
     C                   END                                                       END OFFI
     C                   END                                                       END MEDI
     C                   END                                                       END PATH
     C                   END                                                       END RADI
     C                   END                                                       END SURG
     C                   END                                                       END ANES
     C*
     C                   END
     C                   END
     C*
     C*** ELSE IS OVERRIDE ********************************************
     C                   ELSE                                                   OVERRIDE TYPE
     C*
     C*  Test for a possible duplicate claim.
     C     DHPRCD        IFNE      *BLANK
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAEOB#    ANDEQ*ZERO
      *** END DEL *** 09/21/98 ****************************************
     C                   MOVE      DHPRCD        XXCPT4
     C*******************  MOVELCAPMOD    XXPMOD
     C     CAKYP2        CHAIN     PCA0P                              60
     C*
     C     D2PCL1        TAG
     C     *IN60         IFEQ      '0'
     C*
     C     CADSEQ        IFNE      PXDSEQ
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CADSEQ    OREQ PXDSEQ
     C********** CAEOB#    ANDNEPXEOB#
      *** END DEL *** 09/21/98 ****************************************
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '03'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '03'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C                   ELSE
     C*
     C     CAKYP2        READE     PCA0P                                  60
     C  N60              GOTO      D2PCL1
     C                   END
     C                   END
     C                   END
     C*
      *** BEG DEL *** 09/21/99 ****************************************
     C********** CAKY1A    CHAINPCA01                60
     C********** *IN60     IFEQ '0'
      *** END DEL *** 09/21/99 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     CAEOB#        IFEQ      QQEOB#
      *** END ADD *** 09/21/98 ****************************************
     C*   If override type, we must still output the allowed/disallowed
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAEOB#    IFEQ *ZERO
      *** END DEL *** 09/21/98 ****************************************
     C     CAMRA         ANDEQ     0
     C     CAOTHR        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CAPAID        ANDEQ     0
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CATOTA    ANDEQ0
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     *ZEROS        XPTOTA
      *** END ADD *** 09/21/98 ****************************************
     C*
     C*   If there is an override percentage, calculate
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CACHRG        MULT      XXOVRP        XPTOTA
     C                   END
     C*
     C     CACHRG        SUB       XPTOTA        CAMRA
     C                   Z-ADD     XPTOTA        CAPAID
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*
      *** BEG CHG *** 09/21/99 ****************************************
     C******************** UPDATPCA01
     C                   UPDATE    PCA13
      *** END DEL *** 09/21/99 ****************************************
     C*
     C                   END
     C*
     C                   END                                                    END EQ OVRRD
     C*
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ENDIF                                                  CAFDAT GE
      *** END ADD *** 09/21/98 ****************************************
     C*
     C*** Process next record *****************************************
     C     N2TRCD        TAG
     C*
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAKY1     READEPCA01                    54  GET NEXT RCD
     C     CAKY2         READE     PCA13                                  54      GET NEXT RCD
      *** END CHG *** 09/21/98 ****************************************
     C*                                                      TO PROCESS
     C*
     C     *IN54         IFEQ      '0'                                          SAVE FIELDS OF
     C                   Z-ADD     CADSEQ        SVDSEQ                         CURRENT REC.
     C                   MOVE      CAFEIN        SVFEIN
     C                   MOVE      CAMOD         SVMOD
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAUNTS        SVUNTS
     C                   MOVEL     DHPRCD        XPCPT4
     C                   MOVEL     CAPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C                   END
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ENDDO                                                  *IN54=OFF
      *** END ADD *** 09/21/98 ****************************************
     C*
     C*** Execute SURG2 if a SURG Code has been processed
     C     OPNFLG        IFEQ      '1'
     C                   EXSR      SURG2                                        Process the SURG cod
     C                   END                                                    END OPNFLG EQ '1'
     C*
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*  END DUP OF MAIN CODE
     C*****************************************************************
     C*  End the program
     C     TERM          TAG
     C*
     C*  Release all the formats for a save return.
     C*
      *** BEG DEL *** 09/21/99 ****************************************
     C********** DUMMY     CHAINPCA01                54
     C**N54********        EXCPTECA01
      *** BEG DEL *** 09/21/99 ****************************************
      *** BEG CHG *** 09/21/98 ****************************************
     C********** DUMMY *** CHAINPCA08                54
     C* N54 ************** EXCPTECA08
     C     DUMMY         CHAIN     PCA05                              54
     C  N54              EXCEPT    ECA05
      *** END CHG *** 09/21/98 ****************************************
     C*
     C*----------------------------------------------------------------------
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALTX
     C                   CLOSE     FACAPTX
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALTX
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPTX
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*---------------------------------------------------------------------
     C*
     C                   RETURN
     C                   MOVE      '1'           *INLR
     *********************************************************************
     *** Anesthesia                 Codes: 00100 - 01999         A N E S *
     *********************************************************************
     C     ANES          BEGSR
     C*
     C*=====================================================================
     C*=== Procedure Codes with modifiers:
      *** BEG DEL *** 09/21/99 ***************************************
     C********** CAPMOD    IFNE *BLANK
     C********** CAPMOD    IFNE '22'
     C********** CAPMOD    ANDNE'23'
     C********** CAPMOD    ANDNE'41'
     C********** CAPMOD    ANDNE'42'
     C********** CAPMOD    ANDNE'43'
     C********** CAPMOD    ANDNE'44'
     C********** CAPMOD    ANDNE'46'
     C********** CAPMOD    ANDNE'P1'
     C********** CAPMOD    ANDNE'P2'
     C********** CAPMOD    ANDNE'P3'
     C********** CAPMOD    ANDNE'P4'
     C********** CAPMOD    ANDNE'P5'
     C********** CAPMOD    ANDNE'P6'
     C*
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '10'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C*----------------------------------------------------------------
     C* Per p. 196 ANESTHESIA
      *
     C                   Z-ADD     1             Y
      *
     C     Y             DOWLE     4
      *
     C     MOD(Y)        IFNE      *BLANK
     C     MOD(Y)        ANDNE     '22'
     C     MOD(Y)        ANDNE     '23'
     C     MOD(Y)        ANDNE     '41'
     C     MOD(Y)        ANDNE     '42'
     C     MOD(Y)        ANDNE     '43'
     C     MOD(Y)        ANDNE     '44'
     C     MOD(Y)        ANDNE     '46'
     C     MOD(Y)        ANDNE     'P1'
     C     MOD(Y)        ANDNE     'P2'
     C     MOD(Y)        ANDNE     'P3'
     C     MOD(Y)        ANDNE     'P4'
     C     MOD(Y)        ANDNE     'P5'
     C     MOD(Y)        ANDNE     'P6'
     C                   MOVE      '10'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C*
     C                   ELSE                                                   ELSE EQ 22...
     C*----------------------------------------------------------------
     C* Per P. 193 Anesthesia - Physical status modifiers
     C* See table in Fee Sched manual                                  ion
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ 'P1'
     C********** CAPMOD    OREQ 'P2'
     C********** CAPMOD    OREQ 'P3'
     C********** CAPMOD    OREQ 'P4'
     C********** CAPMOD    OREQ 'P5'
     C********** CAPMOD    OREQ 'P6'
      *** END DEL *** 09/21/98 ****************************************
     C*
     C     MOD(Y)        IFEQ      'P1'
     C     MOD(Y)        OREQ      'P2'
     C     MOD(Y)        OREQ      'P3'
     C     MOD(Y)        OREQ      'P4'
     C     MOD(Y)        OREQ      'P5'
     C     MOD(Y)        OREQ      'P6'
     C                   Z-ADD     1             X
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    LOKUPANEPM1,X                 60
     C     MOD(Y)        LOOKUP    ANEPM1(X)                              60
      *** END CHG *** 09/21/98 ****************************************
     C     *IN60         IFEQ      '1'
     C     ANEPM2(X)     ADD       SXANAM        SXANAM
     C                   END                                                    END 1 *IN60
     C                   END                                                    END EQ 'P1'
     C*
     C                   EXSR      PMOD
     C*
     C                   END                                                    MOD,Y NE BLANK
     C*
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END NE *BLANK
      *** END DEL *** 09/21/98 ****************************************
     C*
     C*
     C***  USING UNUSUAL SERVICE / UNUSUAL ANESTHESIA MODIFIER
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '22'
     C********** CAPMOD    OREQ '23'
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'BQ'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     MOD(Y)        IFEQ      '22'
     C     MOD(Y)        OREQ      '23'
      *
     C                   MOVE      'BQ'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg BQ: "Documentation is required for reimbursement"
     C                   END
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ADD       1             Y
     C*
     C                   ENDDO                                                  X DOWLE 4
      *** END ADD *** 09/21/98 ****************************************
     C*
     C*================================================================
     C*=== Calculating Time Units =====================================
     C*
     C* The Time Units for all procedures is computed by allowing 1 unit
     C* for each 15 minutes of anesthesia time.  In each instance, 5 minutes
     C* or greater is considered a significant portion of a time unit.
     C*
     C*  1.0 Unit for each 15 minutes or any fraction thereof
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAANMN    DIV  15        HLANMN
     C     DHUNTS        DIV       15            HLANMN
      *** END CHG *** 09/21/98 ****************************************
     C** Round up all fractions of 15 minutes to an additional Time Unit
     C                   MVR                     HLADDT
     C                   ADD       HLANMN        HLADDT
     C                   Z-ADD     HLANMN        HLANTV
     C     HLADDT        IFGT      *ZEROS
     C                   ADD       1             HLANTV
     C                   END
     C*
     C* PER P. 194-195 THESE CPT-4 CODES DO NOT HAVE A TIME FACTOR
     C*
     C     DHPRCD        IFEQ      '99100'
     C     DHPRCD        OREQ      '99116'
     C     DHPRCD        OREQ      '99135'
     C     DHPRCD        OREQ      '99140'
     C     DHPRCD        OREQ      '01997'
     C                   Z-ADD     *ZEROS        HLANTV
     C                   ENDIF
     C*
     C** Basic Value + Time Units = Total Anesthesia Value
     C     SXANAM        ADD       HLANTV        XXWORK
     C*
     C*
     C** Total Anesthesia Value * Conversion Factor = Fee
     C     XXWORK        MULT(H)   XXANES        SXMRA
     C*
     C* ===============================================================
     C* Modifier -46
     C* ANESTHESIA BY CRNA
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '46'
     C     '46'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
      *** END CHG *** 09/21/98 ****************************************
     C     SVSPCD        OREQ      'RNA'
     C     XXWORK        MULT      XXCANE        SXMRA
     C                   END
     C*
     C* ===============================================================
     C* Modifier -42
     C* REIMBURSEMENT FOR CONCURRENT SUPERVISION - ANESTHESIA
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '42'
     C     '42'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
      *** END CHG *** 09/21/98 ****************************************
     C*
     C* ALLOW 90% OF THE ALLOWABLE FEE
     C     SXMRA         MULT      .90           SXMRA
     C*
     C                   END                                                    END CAPMOD
     C* ===============================================================
     C* Modifier -43
     C* REIMBURSEMENT FOR CONCURRENT SUPERVISION - ANESTHESIA
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '43'
     C     '43'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
      *** END CHG *** 09/21/98 ****************************************
     C*
     C* ALLOW 85% OF THE ALLOWABLE FEE
     C     SXMRA         MULT      .85           SXMRA
     C*
     C                   END                                                    END CAPMOD
     C*
     C* ===============================================================
     C* Modifier -44
     C* REIMBURSEMENT FOR CONCURRENT SUPERVISION - ANESTHESIA
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '44'
     C     '44'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
      *** END CHG *** 09/21/98 ****************************************
     C*
     C* ALLOW 80% OF THE ALLOWABLE FEE
     C     SXMRA         MULT      .80           SXMRA
     C*
     C                   END                                                    END CAPMOD
     C*
     C*===============================================================
     C* Calculate Payment
     C*
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C* Update charge with payment
     C                   EXSR      UPDAT
     C*
     C     ANESE         ENDSR
     *********************************************************************
     C*  Durable Medical Equip      Codes:                       D M E   *
     C*                                   A0000 - A9999
     C*                                   E0000 - E9999
     C*                                   K0000 - K9999
     C*                                   L0000 - L9999
     *********************************************************************
     C     DME           BEGSR
     C*
     C* ===============================================================
     C* Rental Equipment will be identified by PMOD = RE
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAPMOD    IFNE *BLANK
     C**********
     C********** CAPMOD    IFNE 'LE'
     C********** CAPMOD    ORNE 'SP'
     C********** CAPMOD    ORNE 'UE'
     C********** CAPMOD    ORNE 'RE'
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '10'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     1             Y
      *
     C     Y             DOWLE     4
      *
     C     MOD(Y)        IFNE      *BLANK
     C     MOD(Y)        ANDNE     'LE'
     C     MOD(Y)        ANDNE     'SP'
     C     MOD(Y)        ANDNE     'UE'
     C     MOD(Y)        ANDNE     'RE'
     C                   MOVE      '10'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg 10: "Invalid Modifier used with this Item., Re-enter Item
     C***              with valid modifier"
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      DMEE
     C*
     C                   END                                                    MOD,Y NE BLANKS
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C******************** END                             Y DOWLE 4
     C                   ADD       1             Y                              Y DOWLE 4
     C*
     C                   ENDDO                                                  Y DOWLE 4
      *** END CHG *** 09/21/98 ****************************************
     C*
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     DMEE          ENDSR
     *** BEG ADD *** 09/21/98 *****************************************
     *********************************************************************
     *** UPDATE FADEP WITH EOMB                                E O M B   *
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
     C                   MOVE      'A'           QQESRC
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
     C* if XPEOMB is not blank
     C     XPEOMB        IFNE      *BLANKS
     C*
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
     C                   SELECT
     C*
     C     QQFILE        WHENEQ    'CA'
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
     C                   MOVE      XPEOMB        DEEOB
     C                   MOVE      XXNOT1        DENOT1
     C                   MOVE      *BLANKS       DENOT2
     C                   MOVE      'A'           DESTAT
     C                   MOVE      'A'           DEESRC
     C                   Z-ADD     XXRCN         DERCN
     C*
     C     QQFILE        WHENEQ    'P2'
     C*
     C                   MOVE      P2CO#         DECO#
     C                   MOVE      P2SUB#        DESUB#
     C                   MOVE      P2FND         DEFND
     C                   MOVE      P2FYR         DEFYR
     C                   MOVE      P2CASE        DECASE
     C                   MOVE      P2SS#         DESS#
     C                   MOVE      P2ACDT        DEACDT
     C                   MOVE      QQEOB#        DEEOB#
     C                   MOVE      P2DSEQ        DEDSEQ
     C                   MOVE      XPEOMB        DEEOB
     C                   MOVE      XXNOT1        DENOT1
     C                   MOVE      *BLANKS       DENOT2
     C                   MOVE      'A'           DESTAT
     C                   MOVE      'A'           DEESRC
     C                   Z-ADD     XXRCN         DERCN
      *
     C                   ENDSL
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
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  XPEOMB=BLANK
     C*
     C     EOMBE         ENDSR
     *** END ADD *** 09/21/98 *****************************************
     *********************************************************************
     *** LOAD FOLLOW UP ARRAYS SUBROUTINE                      L O D F U *
     *********************************************************************
     C     LODFU         BEGSR
     C*
     C*** LOAD ARRAYS WITH FOLLOW UP DAYS
     C*** THE FOLLOWING WILL COMPUTE THE FOLLOW UP DAYS FOR ALL
     C*** CPT4 SERVICES FOR THIS CASE WHICH HAVE A VALUE IN FUD.
     C*** ALSO, IT WILL COMPUTE FOLLOW UP DAYS FOR ALL ANES CODES
     C*** WITH A DEFAULT OF 30 DAYS (PER SH).
     C*
     C                   CLEAR                   FUF
     C                   CLEAR                   FUT
     C                   Z-ADD     1             U                 5 0
     C*
     C     CAKYA1        CHAIN     PCA0A                              60
     C     *IN60         DOWEQ     '0'
     C     U             ANDLE     500
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XADSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C*
     C* DEFAULT 30 DAYS FOR ANES CODES FOR FUD
     C*
     C     DXPRCD        IFGE      '00100'
     C     DXPRCD        ANDLE     '01999'
     C                   Z-ADD     XAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C                   Z-ADD     30            SRFUTR            5 0
     C                   MOVE      ' '           SRRTCD            1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLDDAT            7 0
     C                   Z-ADD     XAFDAT        FUF(U)
     C                   Z-ADD     HLDDAT        FUT(U)
     C                   ADD       1             U
     C*
     C                   ELSE
     C                   MOVE      DXPRCD        XXCPT4
     C                   MOVE      XAFDAT        XXEFDT
     C     PRKY          SETGT     PPR01
     C                   READP     PPR01                                  61
     C     *IN61         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C     PRFOLU        ANDGT     *ZERO
     C                   Z-ADD     XAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C                   Z-ADD     PRFOLU        SRFUTR            5 0
     C                   MOVE      ' '           SRRTCD            1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLDDAT            7 0
     C                   Z-ADD     XAFDAT        FUF(U)
     C                   Z-ADD     HLDDAT        FUT(U)
     C                   ADD       1             U
     C*
     C                   END                                                    END *IN61
     C                   END                                                    END DXPRCD
     C     CAKYA1        READE     PCA0A                                  60
     C                   END
     C*
     C     LODFUE        ENDSR
     *********************************************************************
     C*  Medical                    Codes: 90000 - 99199         M E D I *
     *********************************************************************
     C     MEDI          BEGSR
     C*
     C*=====================================================================
     C* Procedure Codes with modifiers:
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAPMOD    IFNE *BLANK
     C*----------------------------------------------------------------
     C**Per*P.*43 MEDICINE
     C********** CAPMOD    IFNE '22'
     C********** CAPMOD    ANDNE'26'
     C********** CAPMOD    ANDNE'27'
     C********** CAPMOD    ANDNE'29'
     C********** CAPMOD    ANDNE'51'
     C********** CAPMOD    ANDNE'52'
     C********** CAPMOD    ANDNE'55'
     C********** CAPMOD    ANDNE'56'
     C********** CAPMOD    ANDNE'75'
     C********** CAPMOD    ANDNE'76'
     C********** CAPMOD    ANDNE'77'
     C********** CAPMOD    ANDNE'90'
     C********** CAPMOD    ANDNE'99'
     C********** CAPMOD    ANDNE'AC'
     C********** CAPMOD    ANDNE'AP'
     C********** CAPMOD    ANDNE'AT'
     C********** CAPMOD    ANDNE'CM'
     C********** CAPMOD    ANDNE'CP'
     C********** CAPMOD    ANDNE'DB'
     C********** CAPMOD    ANDNE'DC'
     C********** CAPMOD    ANDNE'EC'
     C********** CAPMOD    ANDNE'EM'
     C********** CAPMOD    ANDNE'EU'
     C********** CAPMOD    ANDNE'FC'
     C********** CAPMOD    ANDNE'FT'
     C********** CAPMOD    ANDNE'HE'
     C********** CAPMOD    ANDNE'JA'
     C********** CAPMOD    ANDNE'ME'
     C********** CAPMOD    ANDNE'MP'
     C********** CAPMOD    ANDNE'MR'
     C********** CAPMOD    ANDNE'MT'
     C********** CAPMOD    ANDNE'PH'
     C********** CAPMOD    ANDNE'PO'
     C********** CAPMOD    ANDNE'SS'
     C********** CAPMOD    ANDNE'TN'
     C********** CAPMOD    ANDNE'TT'
     C********** CAPMOD    ANDNE'WC'
     C********** CAPMOD    ANDNE'WH'
     C**********
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '10'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C* Per P. 43 MEDICINE
     C                   Z-ADD     1             Y
      *
     C     Y             DOWLE     4
      *
     C     MOD(Y)        IFNE      *BLANKS
     C     MOD(Y)        IFNE      '22'
     C     MOD(Y)        ANDNE     '26'
     C     MOD(Y)        ANDNE     '27'
     C     MOD(Y)        ANDNE     '29'
     C     MOD(Y)        ANDNE     '51'
     C     MOD(Y)        ANDNE     '52'
     C     MOD(Y)        ANDNE     '55'
     C     MOD(Y)        ANDNE     '56'
     C     MOD(Y)        ANDNE     '75'
     C     MOD(Y)        ANDNE     '76'
     C     MOD(Y)        ANDNE     '77'
     C     MOD(Y)        ANDNE     '90'
     C     MOD(Y)        ANDNE     '99'
     C     MOD(Y)        ANDNE     'AC'
     C     MOD(Y)        ANDNE     'AP'
     C     MOD(Y)        ANDNE     'AT'
     C     MOD(Y)        ANDNE     'CM'
     C     MOD(Y)        ANDNE     'CP'
     C     MOD(Y)        ANDNE     'DB'
     C     MOD(Y)        ANDNE     'DC'
     C     MOD(Y)        ANDNE     'EC'
     C     MOD(Y)        ANDNE     'EM'
     C     MOD(Y)        ANDNE     'EU'
     C     MOD(Y)        ANDNE     'FC'
     C     MOD(Y)        ANDNE     'FT'
     C     MOD(Y)        ANDNE     'HE'
     C     MOD(Y)        ANDNE     'JA'
     C     MOD(Y)        ANDNE     'ME'
     C     MOD(Y)        ANDNE     'MP'
     C     MOD(Y)        ANDNE     'MR'
     C     MOD(Y)        ANDNE     'MT'
     C     MOD(Y)        ANDNE     'PH'
     C     MOD(Y)        ANDNE     'PO'
     C     MOD(Y)        ANDNE     'SS'
     C     MOD(Y)        ANDNE     'TN'
     C     MOD(Y)        ANDNE     'TT'
     C     MOD(Y)        ANDNE     'WC'
     C     MOD(Y)        ANDNE     'WH'
     C                   MOVE      '10'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   ELSE                                                   ELSE EQ 22...
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '55'
     C     MOD(Y)        IFEQ      '55'
      *** END CHG *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '64'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '64'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 64: "The item indicates Postoper Mgmt Only mod-55
     C*             Refer to fee schedule for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END *IN55
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '56'
     C     MOD(Y)        IFEQ      '56'
      *** END CHG *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'BR'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'BR'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg BR: "The item indicates Pre-oper Mgmt Only mod-56
     C*             Refer to fee schedule for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END *IN56
     C*
     C                   EXSR      PMOD
     C*
     C                   END                                                    MOD,Y NE 22
     C*
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ENDIF                                                  MOD,Y NE BLANKS
      *** END ADD *** 09/21/98 ****************************************
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C******************** END                             Y DOWLE 4
     C                   ADD       1             Y
     C*
     C                   ENDDO                                                  Y DOWLE 4
      *** END CHG *** 09/21/98 ****************************************
     C*
     C*
     C* THE FOLLOWING WILL HANDLE THE PER MIN CODES
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAANMN    IFNE *ZEROS
     C********** SXANAM    MULT CAANMN    SXMRA
     C     SX1A2         IFNE      *BLANK
     C*** BEG ADD *** 11/16/00 ****************************************
     C     SX1A2         ANDNE     'T'
     C*** END ADD *** 11/16/00 ****************************************
     C     SXANAM        MULT      DHUNTS        SXMRA
      *** END CHG *** 09/21/98 ****************************************
     C                   ENDIF
     C*
     C*** BEG DEL *** 09/27/00 ****************************************
     C* ====================================================================
     C* PER P. 31 I. A. 7.
     C* The physical or occupation therapist's initial eval
     C* is limited to codes 99202, 99203, 99204 depending on the
     C* level of service. Treatment may be performed on the same
     C* day as initial evaluation.
     C*********
     C*********  SVSPCD    IFEQ 'OT'
     C*********  SVSPCD    OREQ 'OCT'
     C*********  SVSPCD    OREQ 'PHT'
     C*********  DHPRCD    IFGE '97010'
     C*********  DHPRCD    ANDLE'97546'
     C*********
     C*********            MOVE '99202'   XXCPT4
     C*********  CAKY9     SETLLPCA09
     C*********  CAKY9A    READEPCA09                    60
     C*********  *IN60     DOWEQ'0'
     C****BEG*ADD *** 09/21/98 ****************************************
     C*********            Z-ADDXRDSEQ    XXDSEQ
     C*********
     C*********  DHKY      CHAINPDH02                61
     C*********
     C****END*ADD *** 09/21/98 ****************************************
     C*********  DXPRCD    IFGE '99202'
     C*********  DXPRCD    ANDLE'99204'
     C*********  XRTOTP    IFGT 0
     C*********            Z-ADD*ZERO     XPTOTA
     C*********            Z-ADD*ZERO     XPTOTP
     C*********            Z-ADD*ZEROS    XPBRTC
     C*********  CACHRG    SUB  XPTOTA    XPTOTD
     C*********            Z-ADDCACHRG    XPTOTC
     C*********            MOVE '205'     XPEOMB
     C****EOMB*205: "This procedure is included in the basic allowance
     C*********      of another procedure"
     C*********
     C*********            EXSR UPDAT
     C*********            GOTO MEDIE
     C*********            END                             END XRTOTP > 0
     C*********            END                             END DXPRCD
     C*********  CAKY9A    READEPCA09                    60
     C*********            END                             END *IN60
     C*********
     C*********            END                             END DHPRCD 97010
     C*********            END                             END SVSPCD
     C*** END DEL *** 09/27/00 ****************************************
     C*
     C* ====================================================================
     C* PER P. 31 I. A. 8. NOTE
     C* Range of motion measurement and muscle testing as
     C* performed by the physical or occupation therapist during
     C* re-evaluation are included in this code and shall not be
     C* reimbursed separately.
     C*
     C     SVSPCD        IFEQ      'OT'
     C     SVSPCD        OREQ      'OCT'
     C     SVSPCD        OREQ      'PHT'
     C     DHPRCD        IFEQ      '95831'
     C     DHPRCD        OREQ      '95832'
     C     DHPRCD        OREQ      '95833'
     C     DHPRCD        OREQ      '95834'
     C     DHPRCD        OREQ      '95851'
     C     DHPRCD        OREQ      '95852'
     C*
     C                   MOVE      '99213'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9         READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRTOTP        IFGT      0
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END XRTOTP > 0
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD 95831
     C                   END                                                    END SVSPCD
     C*
     C* ====================================================================
     C*** Per P. 12 XIII. C.
     C*** The following services are included in reporting critical
     C*** care when performed during the critical period by the
     C*** doctor providing critical care and shall not be
     C*** billed separately:
     C***
     C     DHPRCD        IFEQ      '93561'
     C     DHPRCD        OREQ      '93562'
     C     DHPRCD        OREQ      '99090'
     C     DHPRCD        OREQ      '91105'
     C     DHPRCD        OREQ      '92953'
     C     DHPRCD        OREQ      '94656'
     C     DHPRCD        OREQ      '94657'
     C     DHPRCD        OREQ      '94660'
     C     DHPRCD        OREQ      '94662'
     C*
     C                   MOVE      '99291'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFGE      '99291'                                      CRITICAL CARE
     C     DXPRCD        ANDLE     '99292'                                      SERVICES
     C     XRTOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'CQ'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'CQ'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg CQ: "This service is included under Critical
     C*              Care Services (99291-99292)".
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END DXPRCD
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD 99221
     C*
     C* ====================================================================
     C*** Per P. 203 I. B. - RADIOLOGY - INJECTION PROCEDURES
     C*** IV injection of contrast material is included in the CAT
     C*** procedure and shall not be reimbursed separately.
     C***
     C     DHPRCD        IFEQ      '90784'
     C*
     C                   MOVE      '70460'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFEQ      '70460'
     C     DXPRCD        OREQ      '70481'
     C     DXPRCD        OREQ      '70487'
     C     DXPRCD        OREQ      '70490'
     C     DXPRCD        OREQ      '70491'
     C     DXPRCD        OREQ      '71260'
     C     DXPRCD        OREQ      '72126'
     C     DXPRCD        OREQ      '72129'
     C     DXPRCD        OREQ      '72132'
     C     DXPRCD        OREQ      '72193'
     C     DXPRCD        OREQ      '73201'
     C     DXPRCD        OREQ      '73701'
     C     DXPRCD        OREQ      '74160'
     C*
     C     XRTOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DC'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DC'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DC: "IV injection of contrast material is included
     C*              in the CAT proc".
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END DXPRCD
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD 90784
     C*
     C* =================================================================
     C***
     C*** Per P. 32 I. A. 10. a.
     C*** A physical medicine session is defined as any combination of
     C*** modalities (97010-97039), procedures (97110-97150) and/or
     C*** physical medicine activities and training (97220-97541).
     C*** Two sessions are allowed per day for the first week of
     C*** the acute phase of the injury. Thereafter, only one session
     C*** per day is allowed.
     C***
     C*** EACH BODY PART WILL GET 8 LIMIT (1ST WEEK), AND 4 THEREAFTER
     C*** 2 HOUR SESSION MAX WAS ALSO ADDED ACCORDING TO FS
     C*** CPT4 CODES 97260 AND 97261 EXCLUDED FROM RANGE (PER TASB)
     C***
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97039'
     C     DHPRCD        ORGE      '97110'
     C     DHPRCD        ANDLE     '97150'                                                  ED
     C     DHPRCD        ORGE      '97220'
     C     DHPRCD        ANDLE     '97250'
     C     DHPRCD        ORGE      '97265'
     C     DHPRCD        ANDLE     '97541'
     C*
     C* ZERO OUT AMTS ON FIRST PASS
     C* FOR THIS EDIT TO WORK ONLY ONE PASS IS REQUIRED
     C* IF TWO PASSES ARE MADE IT WILL PAY A 15 MIN CODE OVER
     C* A 30-60 MIN CODE IF AT THE BREAK POINT (OF TIME LIMIT).
     C*
     C     XXPASS        IFEQ      '1'
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    XXPASS
     C*
     C* GET TIME UNITS (15 MIN = 1 UNIT) FOR MAX OF 2 HOURS PER SESSION
     C*
     C                   Z-ADD     1             XYTIME
     C     DHPRCD        IFEQ      '97150'
     C                   Z-ADD     4             XYTIME            5 0
     C                   ELSE
     C     DHPRCD        IFEQ      '97500'
     C     DHPRCD        OREQ      '97520'
     C     DHPRCD        OREQ      '97540'
     C                   Z-ADD     2             XYTIME
     C                   ENDIF                                                  END DHPRCD
     C                   ENDIF                                                  END DHPRCD
     C*
     C**** COMPUTE 1 WEEK FROM ACCIDENT DATE
     C*
     C                   Z-ADD     CAACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C                   Z-ADD     7             SRFUTR            5 0
     C                   MOVE      ' '           SRRTCD            1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKDAT             7 0
     C*
     C                   Z-ADD     *ZEROS        COUNT
     C                   Z-ADD     *ZEROS        COUNT2
     C                   CLEAR                   CPT4
     C                   CLEAR                   CNTU
     C                   CLEAR                   PRT
     C                   CLEAR                   CNT
     C                   Z-ADD     *ZEROS        SVFDAT            6 0
     C*
     C                   MOVE      '97010'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C*
     C*** READ SAME DATE FOR THIS CASE/FEIN
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      XRDSEQ        XXDSEQ
     C     DHKY          CHAIN     PDH02                              61
      *** END ADD *** 09/21/98 ****************************************
     C     XRDSEQ        IFNE      CADSEQ
     C     XRTOTP        ANDGT     *ZEROS
     C     DXPRCD        IFGE      '97010'
     C     DXPRCD        ANDLE     '97039'
     C     DXPRCD        ORGE      '97110'
     C     DXPRCD        ANDLE     '97150'
     C     DXPRCD        ORGE      '97220'
     C     DXPRCD        ANDLE     '97250'
     C     DXPRCD        ORGE      '97265'
     C     DXPRCD        ANDLE     '97541'
     C*
     C* MOVE IN DUMMY VALUE FOR BLANK PART
      *** BEG CHG *** 09/21/98 ****************************************
     C********** DXAREA    IFEQ *BLANKS
     C**********           MOVE '&&'      DXAREA
     C     DXAREA        IFEQ      *BLANKS
     C                   MOVE      '&&'          DXAREA
      *** END CHG *** 09/21/98 ****************************************
     C                   ENDIF
     C*
     C* GET TIME UNITS (15 MIN = 1 UNIT) FOR MAX OF 2 HOURS PER SESSION
     C*
     C                   Z-ADD     1             XXTIME            5 0
     C     DXPRCD        IFEQ      '97150'
     C                   Z-ADD     4             XXTIME
     C                   ELSE
     C     DXPRCD        IFEQ      '97500'
     C     DXPRCD        OREQ      '97520'
     C     DXPRCD        OREQ      '97540'
     C                   Z-ADD     2             XXTIME
     C                   ENDIF                                                  END DXPRCD
     C                   ENDIF                                                  END DXPRCD
     C*
     C     DXPRCD        IFEQ      '97010'
     C                   ADD       1             COUNT
     C                   Z-ADD     1             XXUNIT
     C                   ENDIF
     C     DXPRCD        IFGT      '97010'
     C                   Z-ADD     1             X
     C     DXPRCD        LOOKUP    CPT4(X)                                61
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    CPT4(X)                                61
     C  N61              Z-ADD     30            X
     C                   MOVE      DXPRCD        CPT4(X)
     C                   Z-ADD     1             XXUNIT                         UNIT = 1
     C                   ELSE
     C                   Z-ADD     0             XXUNIT                         ALLOW MULTI
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     1             X
     C     DXAREA        LOOKUP    PRT(X)                                 61
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    PRT(X)                                 61
     C  N61              Z-ADD     20            X
     C                   MOVE      DXAREA        PRT(X)
     C                   Z-ADD     XXUNIT        CNT(X)
     C                   Z-ADD     XXTIME        CNTU(X)
     C                   ELSE
     C                   ADD       XXUNIT        CNT(X)
     C                   ADD       XXTIME        CNTU(X)
     C                   END                                                    END *IN61
     C*
     C                   END                                                    END GE 97110...
     C                   END                                                    END NE DSEQ ...
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C*
     C*
     C* MOVE IN DUMMY VALUE FOR BLANK PART
     C     DHAREA        IFEQ      *BLANKS
     C                   MOVE      '&&'          XXPART
     C                   ELSE
     C                   MOVE      DHAREA        XXPART
     C                   ENDIF
     C*
     C**  ONLY TWO 97010 CODES ARE ALLOWED
     C     DHPRCD        IFEQ      '97010'
     C     COUNT         IFGE      2
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '54'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '54'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 54: "Exceeds number of procedures allowed in time
     C*              period".
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END
     C                   END
     C*
     C**  ONLY ONE 97012-97028 IS ALLOWED
     C     DHPRCD        IFGE      '97012'
     C     DHPRCD        ANDLE     '97028'
     C                   Z-ADD     1             X
     C     DHPRCD        LOOKUP    CPT4(X)                                61
     C* IF FOUND, DISALLOW
     C     *IN61         IFEQ      '1'
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '54'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '54'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 54: "Exceeds number of procedures allowed in time
     C*              period".
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END
     C                   END
     C*
     C**  MULTI CODES ARE ALLOWED IF OVER 97032
     C     DHPRCD        IFGE      '97032'
     C                   Z-ADD     1             X
     C     DHPRCD        LOOKUP    CPT4(X)                                61
     C* IF FOUND, ALLOW MULTIPLE
     C     *IN61         IFEQ      '1'
     C                   Z-ADD     1             X
     C     XXPART        LOOKUP    PRT(X)                                 61
     C     *IN61         IFEQ      '1'
     C                   Z-ADD     *ZEROS        CNT(X)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C**  Only allow 8 per body area  (per day)
     C**  In the 1st week of acute phase of treatment
     C     CAFDAT        IFLT      WKDAT
     C                   Z-ADD     1             X
     C     XXPART        LOOKUP    PRT(X)                                 61
     C     *IN61         IFEQ      '1'
     C     CNT(X)        IFGE      8
     C     CNTU(X)       ORGE      16                                           TIME UNITS 4HRS
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB#    ANDEQ*ZERO
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DJ'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DJ'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DJ: "PT service over 8 per day or 4 hr time limit
     C*              (1st wk trmnt).
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   ELSE
     C                   ADD       XYTIME        CNTU(X)
     C     CNTU(X)       IFGT      16                                           TIME UNITS 2HRS
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB#    ANDEQ*ZERO
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '54'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '54'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 54: "Exceeds number of procedures in time period"
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END CNTU
     C                   END                                                    END CNT
     C                   END                                                    END *IN61
     C                   END                                                    END CAFDAT
     C*
     C**  Only allow 4 per body area  (per day)
     C**  After the 1st week of acute phase of treatment
     C*
     C     CAFDAT        IFGE      WKDAT
     C                   Z-ADD     1             X
     C     XXPART        LOOKUP    PRT(X)                                 61
     C     *IN61         IFEQ      '1'
     C     CNT(X)        IFGE      4
     C     CNTU(X)       ORGE      8                                            TIME UNITS 2HRS
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DK'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DK'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DK: "PT service over 4 per day or 2 hr time limit
     C*              aft 1st wk trmnt".
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   ELSE
     C                   ADD       XYTIME        CNTU(X)
     C     CNTU(X)       IFGT      8                                            TIME UNITS 2HRS
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '54'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '54'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 54: "Exceeds number of procedures in time period"
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END CNTU
     C                   END                                                    END CNT
     C                   END                                                    END *IN61
     C                   END                                                    END XXPART
     C*
     C                   END                                                    END GE 97110...
     C*
     C*
     C* ====================================================================
     C*** Per P. 32 I. A. 10. b.
     C*** The exclusive use of physical medicine modalities
     C*** (97010-97039) is limited to a maximum of 2 wks
     C*** unless doc is provided.
     C***
     C*** NEED TO FIND RANGE OF DATES FROM MODALITY TO NON-MODALITY
     C*** ALSO, A NEW 2 WK RANGE IS ALLOWED IF NON-MODALITY CODES USED
     C*** NOTE: XXPASS USED TO ZERO OUT AMOUNTS ON 1ST PASS OF DATA
     C***
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97039'
     C*
     C                   Z-ADD     *ZEROS        COUNT
     C                   Z-ADD     0             XXPAST
      *** BEG CHG *** 09/21/98 ****************************************
     C******************** Z-ADD0         XXFUTR  60
     C                   Z-ADD     0             XXFUTR            7 0
      *** END CHG *** 09/21/98 ****************************************
     C                   CLEAR                   MDAT
     C                   CLEAR                   PDAT
     C                   Z-ADD     0             X
     C                   MOVE      'Y'           XXMOVE            1
     C*
     C* START WITH MODALITIES ON FILE
     C                   MOVE      '97010'       XXCPT4
     C*
     C     CAKYA1        SETLL     PCA0A                              60
     C     CAKYA1        READE     PCA0A                                  60
     C*
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XADSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFEQ      DHPRCD
     C     XADSEQ        ANDNE     CADSEQ
     C     DXPRCD        ORNE      DHPRCD
     C     XATOTP        IFGT      *ZERO
     C*
     C*  LOAD DATE OF FIRST MODALITY, OR DATE AFTER A NON-MODALITY
     C     DXPRCD        IFGE      '97010'
     C     DXPRCD        ANDLE     '97039'
     C     XXMOVE        IFEQ      'Y'
     C*** BEGIN ADD *** 05/03/96 **************************************
     C*** IN CASE COUNTER GOES OVER 200 LIMIT, RESTART DATE TABLES
     C     X             IFEQ      200
     C                   CLEAR                   MDAT
     C                   CLEAR                   PDAT
     C                   Z-ADD     0             X
     C                   END
     C*** END   ADD *** 05/03/96 **************************************
     C                   ADD       1             X
     C                   MOVE      XAFDAT        MDAT(X)
     C                   MOVE      'N'           XXMOVE
     C                   END                                                    END XXMOVE
     C                   END                                                    END DXPRCD
     C*
     C* LOAD DATE OF CODE (OTHER THAN A MODALITY)
     C     DXPRCD        IFLT      '97010'
     C     DXPRCD        ORGT      '97039'
     C                   MOVE      'Y'           XXMOVE
     C     X             IFGT      *ZEROS
     C     PDAT(X)       IFEQ      *ZEROS
     C                   MOVE      XAFDAT        PDAT(X)
     C                   END                                                    END PDAT
     C                   END                                                    END X
     C                   END                                                    END DXPRCD
     C                   END                                                    END XATOTP
     C                   END                                                    END DXPRCD
     C*
     C     CAKYA1        READE     PCA0A                                  60
     C                   END                                                    END DO *IN60
     C*
     C*  FIND THE DATE OF THE FIRST MODALITY PRIOR TO CURRENT DATE
     C                   Z-ADD     *ZEROS        XXFDAT
     C                   Z-ADD     *ZEROS        XXPDAT
     C     MDAT(1)       IFNE      *ZEROS
     C     1             DO        X             Y                 5 0
     C     CAFDAT        IFGE      MDAT(Y)
     C                   MOVE      MDAT(Y)       XXFDAT
     C                   MOVE      PDAT(Y)       XXPDAT            6 0
     C                   ENDIF                                                  END CAFDAT
     C                   ENDDO
     C                   ENDIF                                                  END MDAT
     C*
     C* FIND DATE 14 DAYS FUTURE
     C     XXFDAT        IFNE      *ZEROS
     C                   Z-ADD     14            SRFUT
     C                   Z-ADD     XXFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT            8 0
     C                   CALL      'SRFUTR8'
     C                   PARM                    WKFDAT
     C                   PARM                    SRFUT
     C                   PARM                    SRTERM
     C*
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFUTR
     C*
     C*
     C     CAFDAT        IFLT      XXPDAT
     C     XXPDAT        OREQ      *ZEROS
     C     CAFDAT        IFGT      XXFUTR
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'CR'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'CR'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg CR: "If only modalities performed, limit 2 weeks,
     C**               unless DOP provided".
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C     XXPASS        IFEQ      '1'                                          ZERO ON FIRST
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END
     C                   END                                                    END CAFDAT
     C*
     C                   END                                                    END CAFDAT
     C*
     C                   END                                                    END XXFDAT
     C                   END                                                    END DHPRCD
     C*
     C* ====================================================================
     C*** Per P. 33 I. B. 1. a.
     C*** New Patient Visit: The Dr. shall select the office visit
     C*** which most accurately reflects the level of service provided
     C*** and the FIRST MANIPULATION, if performed, is included in
     C*** the office visit.
     C***
     C*
     C     SVSPCD        IFNE      'DO'
     C     DHPRCD        IFEQ      '97260'                                      MANIPULATION
     C                   MOVE      '99201'       XXCPT4
     C                   Z-ADD     *ZEROS        COUNT
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFGE      '99201'
     C     DXPRCD        ANDLE     '99205'
     C     XRTOTP        IFGT      0
     C                   ADD       1             COUNT
     C                   END                                                    END XRTOTP
     C                   END                                                    END DXPRCD
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFGE      1
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CACHK#    IFEQ *ZERO
     C********** CAEOB#    ANDEQ*ZERO
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END COUNT GE 1
     C*
     C                   END                                                    END DHPRCD 97260
     C                   END                                                    END SVSPCD
     C*
     C* ====================================================================
     C*** Per P. 33 I. B. 2. a.
     C*** Any office visit exceeding 99213 on established patients,
     C*** and performed for re-evaluation, is limited to once every
     C*** 30 days and shall include the FIRST MANIPULATION, when
     C*** provided.
     C***
     C*** NOTE: CODE RANGE 99211 - 99215 INCLUDED IN THIS RANGE
     C*** (PER SH)
     C*
     C     SVSPCD        IFNE      'DO'
     C     DHPRCD        IFEQ      '97260'                                      MANIPULATION
     C                   MOVE      '99211'       XXCPT4
     C                   Z-ADD     *ZEROS        COUNT
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFGE      '99211'
     C     DXPRCD        ANDLE     '99215'
     C     XRTOTP        IFGT      0
     C                   ADD       1             COUNT
     C                   END                                                    END XRTOTP
     C                   END                                                    END DXPRCD
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFGE      1
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CACHK#    IFEQ *ZERO
     C********** CAEOB#    ANDEQ*ZERO
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END COUNT GE 1
     C*
     C                   END                                                    END DHPRCD 97260
     C                   END                                                    END SVSPCD
     C*
     C* =================================================================
     C*** Per P. 35 I. E. 2
     C*** FCE(s) are allowed a maximum of three times for each
     C*** injured worker. (Billed as 97750-FC). Max $700.(Total $2100)
     C***
     C     DHPRCD        IFEQ      '97750'
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    ANDEQ'FC'
     C     'FC'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
      *** END CHG *** 09/21/98 ****************************************
     C*
     C                   CLEAR                   PDAT
     C                   Z-ADD     *ZEROS        COUNT
     C                   Z-ADD     *ZEROS        SVFDAT            6 0
     C                   Z-ADD     *ZEROS        XXTOTP
     C*
     C     CAKY9E        SETLL     PCA09                              60
     C     CAKY9E        READE     PCA09                                  60
     C*
     C* FIND ALL OTHER 97750-FC(S) ALREADY PAID
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     XRFEIN        IFEQ      CAFEIN
     C     XRMOD         ANDEQ     CAMOD
     C     DXPRCD        IFEQ      DHPRCD
     C     XRPMOD        ANDEQ     CAPMOD
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRTOTP        IFGT      *ZERO
     C*
     C*  ACCUMULATE NUMBER OF VISITS
     C     XRFDAT        IFNE      SVFDAT
     C                   ADD       1             COUNT
     C                   MOVE      XRFDAT        SVFDAT
     C** LOAD TO VISIT TABLE
     C                   ADD       1             X
     C     X             IFEQ      31
     C                   Z-ADD     30            X
     C                   ENDIF
     C                   Z-ADD     XRFDAT        PDAT(X)
     C                   END                                                    END XRFDAT
     C*  ACCUMULATE TOTAL PAID ON SAME DATE OF CURRENT REC
     C     XRFDAT        IFEQ      CAFDAT
     C                   ADD       XRTOTP        XXTOTP
     C                   END                                                    END XRFDAT
     C                   END                                                    END XRTOTP
     C                   END                                                    END DXPRCD
     C                   END                                                    END XRFEIN
     C*
     C     CAKY9E        READE     PCA09                                  60
     C                   END                                                    END DO *IN60
     C*
     C*  CHECK IF VISITS > 3, OR THIS DATE IS A ANOTHER VISIT
     C*  AND THE MAX AMOUNT HAS BEEN MET
     C                   Z-ADD     1             X
     C     CAFDAT        LOOKUP    PDAT(X)                                60
     C     *IN60         IFEQ      '1'
     C     XXTOTP        ANDGE     700
     C     *IN60         OREQ      '0'
     C     COUNT         ANDGE     3
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'CS'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'CS'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg CS: "FCE(s) allowed max 3 times for a total of $700
     C*              per visit"
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END *IN60
     C*
     C                   Z-ADD     100           SXMRA
      *** BEG ADD *** 09/21/98 ***************************************
     C                   ENDIF                                                  *IN99=ON
      *** END ADD *** 09/21/98 ***************************************
     C                   END                                                    END DHPRCD
     C* =================================================================
     C*** Per P. 35 I. E. 3
     C*** Muscle testing shall not be reimbursed in addition to a
     C*** functional capacity evaluation (FCE).
     C***
     C     DHPRCD        IFEQ      '97750'
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    ANDEQ'MT'
     C     'MT'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
      *** END CHG *** 09/21/98 ****************************************
     C                   MOVE      '97750'       XXCPT4
     C                   Z-ADD     *ZEROS        COUNT
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     XRPMOD        IFEQ      'FC'
     C     DXPRCD        IFEQ      '97750'
     C     XRTOTP        IFGT      0
     C                   ADD       1             COUNT
     C                   END                                                    END XRTOTP
     C                   END                                                    END DXPRCD
     C                   END                                                    END XRPMOD
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFGE      1
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'BQ'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'BQ'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg BQ: "Documentation is required for reimbursement"
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END COUNT GE 1
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C                   ENDIF                                                  *IN99=ON
      *** END CHG *** 09/21/98 ****************************************
     C                   END                                                    END DHPRCD 97750 00
     C*
     C* =================================================================
     C* Per P. 42 IV. B. 1.
     C* Reflex studies: Code 95933 reimbursed once per entire study
     C*                 Code 95935 - A max of six CPT codes can be
     C*                 can be reimbursed for "H" and "F" studies
     C*                 performed per patient on same date of service.
     C*
     C     DHPRCD        IFEQ      '95933'
     C                   Z-ADD     *ZEROS        COUNT
     C                   MOVE      '95933'       XXCPT4
     C     CAKYC         CHAIN     PCA0C                              60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCDSEQ        IFNE      CADSEQ
     C     XCTOTP        ANDGT     *ZEROS
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '54'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '54'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg 54: "Exceeds number of procedures allowed
     C***              in time period"
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END EOB#/CHK# =  0
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END NE DSEQ ...
     C     CAKYC         READE     PCA0C                                  60
     C*
     C                   END                                                    END *IN60
     C                   END                                                    END DHPRCD EQ..
     C*
     C*
     C     DHPRCD        IFEQ      '95935'
     C                   Z-ADD     *ZEROS        COUNT
     C                   MOVE      '95935'       XXCPT4
     C     CAKY9         CHAIN     PCA09                              60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRDSEQ        IFNE      CADSEQ
     C     XRTOTP        ANDGT     *ZEROS
     C*
     C                   ADD       1             COUNT
     C                   END                                                    END NE DSEQ ...
     C     CAKY9         READE     PCA09                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFGE      6
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '54'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '54'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg 54: "Exceeds number of procedures allowed
     C***              in time period"
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END EOB#/CHK# =  0
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END COUNT GE 6
     C                   END                                                    END DHPRCD EQ..
     C* =================================================================
     C*** A MAJOR CODE MUST BE PRESENT ON SAME DAY FOR REIMBURSEMENT
     C***
     C     DHPRCD        IFEQ      '97221'
     C     DHPRCD        OREQ      '97241'
     C     DHPRCD        OREQ      '97501'
     C     DHPRCD        OREQ      '97521'
     C     DHPRCD        OREQ      '97531'
     C     DHPRCD        OREQ      '97541'
     C     DHPRCD        OREQ      '97701'
     C     DHPRCD        OREQ      '97721'
     C*
     C     DHPRCD        IFEQ      '97221'
     C                   MOVE      '97220'       XXCPT4
     C                   END
     C*
     C     DHPRCD        IFEQ      '97241'
     C                   MOVE      '97240'       XXCPT4
     C                   END
     C*
     C     DHPRCD        IFEQ      '97501'
     C                   MOVE      '97500'       XXCPT4
     C                   END
     C*
     C     DHPRCD        IFEQ      '97521'
     C                   MOVE      '97520'       XXCPT4
     C                   END
     C*
     C     DHPRCD        IFEQ      '97531'
     C                   MOVE      '97530'       XXCPT4
     C                   END
     C*
     C     DHPRCD        IFEQ      '97541'
     C                   MOVE      '97540'       XXCPT4
     C                   END
     C*
     C     DHPRCD        IFEQ      '97701'
     C                   MOVE      '97700'       XXCPT4
     C                   END
     C*
     C     DHPRCD        IFEQ      '97721'
     C                   MOVE      '97720'       XXCPT4
     C                   END
     C*
     C     CAKY9         CHAIN     PCA09                              60
     C     *IN60         IFEQ      '1'
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '16'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '16'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 16: "A major code must be present to pay this item".
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '215'         XPEOMB
     C*** EOMB 215: "A major code must be present to pay this item"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END                                                    END 1 *IN60
     C                   END                                                    END DHPRCD 97221
     C*
     C* ====================================================================
     C*** Per P. 63 I. A. 1.
     C*** PACKAGE OR GLOBAL FEE CONCEPT: A single fee is billed
     C*** paid for all nec. services normally performed by the
     C*** surgeon before, during, and after the surgical proc.
     C*** Includes: the pre-oper care nec for the specific proc.
     C*** completion of hosp. records, initiation of treatment,
     C*** and local anesthesia.
     C*
     C     DHPRCD        IFEQ      '93737'
     C     DHPRCD        OREQ      '93738'
     C     DHPRCD        OREQ      '93740'
     C     DHPRCD        OREQ      '93760'
     C     DHPRCD        OREQ      '93762'
     C     DHPRCD        OREQ      '93770'
     C     DHPRCD        OREQ      '93784'
     C     DHPRCD        OREQ      '93786'
     C     DHPRCD        OREQ      '93788'
     C     DHPRCD        OREQ      '93790'
     C     DHPRCD        OREQ      '94760'
     C     DHPRCD        OREQ      '94762'
     C     DHPRCD        OREQ      '94770'
     C*
     C                   MOVE      '00100'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFGE      '00100'                                      ANES CPT4
     C     DXPRCD        ANDLE     '01996'
     C     XRTOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '29'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '29'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END DXPRCD
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD 93737
     C*
     C*================================================================
     C** Per P. 34 C. 7.
     C** Sterile whirlpool is billed as code 97022 with mod -22
     C** and shall be reimbursed at $40.00
     C     DHPRCD        IFEQ      '97022'
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    ANDEQ'22'
     C     '22'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
      *** END CHG *** 09/21/98 ****************************************
     C                   Z-ADD     40            SXMRA
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ENDIF                                                  *IN99=ON
      *** END ADD *** 09/21/98 ****************************************
     C                   END
     C*
     C*
     C*================================================================
     C** Per P. 37 4.
     C** Reimbursement for work conditioning shall be $36 per hour.
     C**
      *** BEG DEL *** 09/21/98 ****************************************
     C********** DHPRCD    IFEQ '97545'
     C********** CAPMOD    ANDEQ'WC'
     C********** DHPRCD    OREQ '97546'
     C********** CAPMOD    ANDEQ'WC'
     C**********           Z-ADD36        SXMRA
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     DHPRCD        IFEQ      '97545'
     C     DHPRCD        OREQ      '97546'
      *
     C     'WC'          LOOKUP    MOD(1)                                 99
      *
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     36            SXMRA
     C                   ENDIF                                                  *IN99=ON
      *** END ADD *** 09/21/98 ****************************************
      *
     C                   END
     C*
     C*
     C** Per P. 38 5.
     C** Reimbursement for work hardening shall be $64 per hour.
     C**
      *** BEG DEL *** 09/21/98 ****************************************
     C********** DHPRCD    IFEQ '97545'
     C********** CAPMOD    ANDEQ'WH'
     C********** DHPRCD    OREQ '97546'
     C********** CAPMOD    ANDEQ'WH'
     C*********************Z-ADD64        SXMRA
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     DHPRCD        IFEQ      '97545'
     C     DHPRCD        OREQ      '97546'
      *
     C     'WH'          LOOKUP    MOD(1)                                 99
      *
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     64            SXMRA
      *** END ADD *** 09/21/98 ****************************************
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'CT'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'CT'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg CT: "If accreditation is NOT CARF,
     C*              reduce payment by 20%".
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ENDIF                                                  *IN99=ON
      *** END ADD *** 09/21/98 ****************************************
     C                   END
     C*
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     MEDIE         ENDSR
     *** BEG ADD *** 09/21/98 *****************************************
     *********************************************************************
     *** ADJUDICATION MESSAGES SUBROUTINE                    M S G       *
     *********************************************************************
     C     MSG           BEGSR
      *
      * Load the Key fields
      *
     C                   MOVE      CADSEQ        XXDSEQ
      *
      * Is this message already in the file for this line item
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
     C                   Z-ADD     XXDSEQ        MGDSEQ
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
     C     MSGE          ENDSR
     *** END ADD *** 09/21/98 *****************************************
      *
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
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADDCACHK#    HLCHK#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     HLDSEQ        CADSEQ                         SAVE FIELD VALS
     C                   MOVE      HLFEIN        CAFEIN                         FOR RECORD WE
     C                   MOVE      HLMOD         CAMOD                          ARE PROCESSING
     C                   MOVE      HLPRTY        CAPRTY
     C                   Z-ADD     HLEOB#        CAEOB#
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADDHLCHK#    CACHK#
     C****Reget*record.***
     C***********CAKYZ**** CHAINPCA01                54
     C********************
     C**N54*************** EXCPTECA01
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ
     C                   MOVE      CAFEIN        SVFEIN                            CURR REC.
     C                   MOVE      CAMOD         SVMOD
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAEOB#        SVEOB#
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADDCACHK#    SVCHK#
      *** END DEL *** 09/21/98 ****************************************
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
     C                   EXSR      LODFU
     C*
     C* ===============================================================
     C*
     C     NEWPRE        ENDSR
     *********************************************************************
     *** OFFICE VISITS     SUBROUTINE CODES 99201 - 99499        O F F I *
     *** EVALUATION AND MANAGEMENT
     *********************************************************************
     C*
     C     OFFI          BEGSR
     C*=====================================================================
     C* Procedure Codes with modifiers:
      *** BEG DEL *** 09/21/99 ***************************************
     C********** CAPMOD    IFNE *BLANK
     C*----------------------------------------------------------------
     C**Per*P.*18 EVALUATION AND MANAGEMENT
     C********** CAPMOD    IFNE '21'
     C********** CAPMOD    ANDNE'22'
     C********** CAPMOD    ANDNE'24'
     C********** CAPMOD    ANDNE'25'
     C********** CAPMOD    ANDNE'26'
     C********** CAPMOD    ANDNE'27'
     C********** CAPMOD    ANDNE'29'
     C********** CAPMOD    ANDNE'34'
     C********** CAPMOD    ANDNE'35'
     C********** CAPMOD    ANDNE'52'
     C********** CAPMOD    ANDNE'75'
     C********** CAPMOD    ANDNE'76'
     C********** CAPMOD    ANDNE'77'
     C********** CAPMOD    ANDNE'90'
     C********** CAPMOD    ANDNE'99'
     C********** CAPMOD    ANDNE'BA'
     C********** CAPMOD    ANDNE'H1'
     C********** CAPMOD    ANDNE'H2'
     C********** CAPMOD    ANDNE'H3'
     C********** CAPMOD    ANDNE'H4'
     C********** CAPMOD    ANDNE'MP'
     C********** CAPMOD    ANDNE'WP'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     1             Y
      *
     C     Y             DOWLE     4
      *
     C     MOD(Y)        IFNE      *BLANK
     C     MOD(Y)        ANDNE     '21'
     C     MOD(Y)        ANDNE     '22'
     C     MOD(Y)        ANDNE     '24'
     C     MOD(Y)        ANDNE     '25'
     C     MOD(Y)        ANDNE     '26'
     C     MOD(Y)        ANDNE     '27'
     C     MOD(Y)        ANDNE     '28'
     C     MOD(Y)        ANDNE     '29'
     C     MOD(Y)        ANDNE     '34'
     C     MOD(Y)        ANDNE     '35'
     C     MOD(Y)        ANDNE     '52'
     C     MOD(Y)        ANDNE     '75'
     C     MOD(Y)        ANDNE     '76'
     C     MOD(Y)        ANDNE     '77'
     C     MOD(Y)        ANDNE     '90'
     C     MOD(Y)        ANDNE     '99'
     C     MOD(Y)        ANDNE     'BA'
     C     MOD(Y)        ANDNE     'H1'
     C     MOD(Y)        ANDNE     'H2'
     C     MOD(Y)        ANDNE     'H3'
     C     MOD(Y)        ANDNE     'H4'
     C     MOD(Y)        ANDNE     'MP'
     C     MOD(Y)        ANDNE     'WP'
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '10'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '10'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END                                                    END NE *BLANK
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END NE *BLANK
      *** END DEL *** 09/21/98 ****************************************
     C*
     C* =================================================================
     C*
     C***  USING MODIFIERS WHICH REQUIRE 'DOP'
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '21'
     C********** CAPMOD    OREQ '22'
     C********** CAPMOD    OREQ '24'
     C********** CAPMOD    OREQ '25'
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'BQ'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     MOD(Y)        IFEQ      '21'
     C     MOD(Y)        OREQ      '22'
     C     MOD(Y)        OREQ      '24'
     C     MOD(Y)        OREQ      '25'
     C                   MOVE      'BQ'          XXMSG#
     C                   EXSR      MSG
     C                   END
     C*** Err Msg BQ: "Documentation is required for reimbursement"
      *** END ADD *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ADD       1             Y
     C*
     C                   ENDDO                                                  Y DOWLE 4
      *** END ADD *** 09/21/98 ****************************************
     C*
     C* ==============================================================
     C*** A MAJOR CODE MUST BE PRESENT ON SAME DAY FOR REIMBURSEMENT
     C***
     C     DHPRCD        IFEQ      '99292'
     C     DHPRCD        OREQ      '99355'
     C     DHPRCD        OREQ      '99357'
     C     DHPRCD        OREQ      '99359'
     C*** BEGIN DEL *** 08/28/96 **************************************
     C*********  DHPRCD    OREQ '99362'
     C*********  DHPRCD    OREQ '99376'
     C*** END   DEL *** 08/28/96 **************************************
     C*
     C     DHPRCD        IFEQ      '99292'
     C                   MOVE      '99291'       XXCPT4
     C                   END
     C*
     C     DHPRCD        IFEQ      '99355'
     C                   MOVE      '99354'       XXCPT4
     C                   END
     C*
     C     DHPRCD        IFEQ      '99357'
     C                   MOVE      '99356'       XXCPT4
     C                   END
     C*
     C     DHPRCD        IFEQ      '99359'
     C                   MOVE      '99358'       XXCPT4
     C                   END
     C*
     C*** BEGIN DEL *** 08/28/96 **************************************
     C*********  DHPRCD    IFEQ '99362'
     C*********            MOVE '99361'   XXCPT4
     C*********            END
     C*********
     C*********  DHPRCD    IFEQ '99376'
     C*********            MOVE '99375'   XXCPT4
     C*********            END
     C*** END   DEL *** 08/28/96 **************************************
     C*
     C     CAKY9         CHAIN     PCA09                              60
     C     *IN60         IFEQ      '1'
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '16'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '16'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 16: "A major code must be present to pay this item".
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '215'         XPEOMB
     C*** EOMB 215: "A major code must be present to pay this item"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END                                                    END 1 *IN60
     C                   END                                                    END DHPRCD 99291
     C*
     C* ==============================================================
     C* Per P. 12-13 'DOP' is required for the following CPT-4 codes
     C*  Neonatal Intensive Care, Nursing Facility, etc..
     C*
     C     DHPRCD        IFGE      '99295'
     C     DHPRCD        ANDLE     '99297'
     C     DHPRCD        ORGE      '99301'
     C     DHPRCD        ANDLE     '99313'
     C     DHPRCD        ORGE      '99321'
     C     DHPRCD        ANDLE     '99323'
     C     DHPRCD        ORGE      '99331'
     C     DHPRCD        ANDLE     '99333'
     C     DHPRCD        ORGE      '99341'
     C     DHPRCD        ANDLE     '99343'
     C     DHPRCD        ORGE      '99351'
     C     DHPRCD        ANDLE     '99353'
     C     DHPRCD        ORGE      '99371'
     C     DHPRCD        ANDLE     '99373'
     C     DHPRCD        ORGE      '99431'
     C     DHPRCD        ANDLE     '99440'
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'BQ'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'BQ'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg BQ: "Documentation is required for reimbursement"
     C                   END
     C*
     C* ===============================================================
     C* PER P. 13 XVIII A. CASE MANAGEMENT
     C* Billing for these services is allowed except in those instances
     C* where a patient encounter occurs on the same date of service.
     C*
     C     DHPRCD        IFGE      '99361'
     C     DHPRCD        ANDLE     '99373'
     C                   MOVE      '00000'       XXCPT4
     C                   Z-ADD     *ZEROS        COUNT
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRTOTP        IFGT      0
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DB'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DB'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg DB: "If billed with a patient encounter on same date,
     C*                disallow payment".
     C*
     C                   LEAVE
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END XRTOTP
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD 99361 00
     C* ====================================================================
     C*** Per P. 41 III. Osteopathic Manipulation
     C*** When manipulation is provided by a doctor of osteopathy
     C*** codes 9825 and 98929 shall be utilized.  The Dr. of osteo.
     C*** may bill 99212 for an established patient office visit.
     C***
     C     DHPRCD        IFEQ      '99213'
     C     DHPRCD        OREQ      '99214'
     C     DHPRCD        OREQ      '99215'
     C*
     C                   MOVE      '98925'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFGE      '98925'
     C     DXPRCD        ANDLE     '98929'
     C     XRTOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DG'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DG'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DG: "Review notes, may need to downcode to code 99212"
     C*
     C                   LEAVE
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END DXPRCD
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD 99213
     C*
     C* ====================================================================
     C*** Per P. 33 I. B. 2. a.
     C*** Any office visit exceeding 99213 on established patients,
     C*** and performed for re-evaluation, is limited to once every
     C*** 30 days and shall include the first manipulation, when
     C*** provided.
     C***
     C     DHPRCD        IFEQ      '99214'
     C     DHPRCD        OREQ      '99215'
     C*
     C                   MOVE      '97260'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9         READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DI'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DI'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DI: "Review notes, may need to DOWNCODE to code 99213"
     C*
     C                   LEAVE
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C     CAKY9         READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD 99214
     C*
     C*
     C* ====================================================================
     C*** Per P. 33 I. B. 1. b. Manipulation
     C*** Established patient visit: The DR. shall use the code
     C*** 99213 with the modifier -MP when providing an off. visit
     C*** in combo with a manipulation on the day of service.
     C***
     C     SVSPCD        IFNE      'DO'
     C     DHPRCD        IFEQ      '99211'
     C     DHPRCD        OREQ      '99212'
     C*
     C                   MOVE      '97260'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9         READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DH'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DH'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DH: "Review notes, may need to UPCODE to code 99213"
     C*
     C                   LEAVE
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C     CAKY9         READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD 99211
     C                   END                                                    END SVSPCD
     C*
     C* ====================================================================
     C* Per P. 204 - Radiation Oncology
     C* The procedure codes for radiation oncology provide for
     C* teletherapy and brachytherapy to include the following
     C* (see p. 204) and normal follow-up care for up to 3 months
     C* following its completion.
     C*
     C                   MOVE      '77305'       XXCPT4
     C     CAKYC         SETLL     PCA0C
     C*
     C*** READ FILE IN FROM DATE SEQUENCE   FOR THIS CASE/FEIN
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XCDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFGE      '77305'
     C     DXPRCD        ANDLE     '77321'
     C     DXPRCD        ORGE      '77326'
     C     DXPRCD        ANDLE     '77328'                                                  ED
     C     DXPRCD        ORGE      '77761'
     C     DXPRCD        ANDLE     '77789'
     C     DXPRCD        OREQ      '77799'
     C     XCTOTP        IFGT      *ZEROS
     C*
     C                   Z-ADD     XCFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C                   Z-ADD     90            SRFUTR            5 0
     C                   MOVE      ' '           SRRTCD            1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKDAT             7 0
     C*
     C     CAFDAT        IFLT      WKDAT
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '29'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '29'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   END                                                    END CAFDAT LT..
     C                   END                                                    END XCTOTP ....
     C                   END                                                    END DXPRCD  ...
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C* Per P. 206 - HYPERTHERMIA
     C* The listed treatments include mgnt during the course of
     C* therapy and follow-up care for 3 months after completion.
     C*
     C     DHPRCD        IFGE      '99211'
     C     DHPRCD        ANDLE     '99215'
     C                   MOVE      '77600'       XXCPT4
     C     CAKYC         SETLL     PCA0C
     C*
     C*** READ FILE IN FROM DATE SEQUENCE   FOR THIS CASE/FEIN
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XCDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFGE      '77600'
     C     DXPRCD        ANDLE     '77620'
     C     XCTOTP        IFGT      *ZEROS
     C*
     C                   Z-ADD     XCFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C                   Z-ADD     90            SRFUTR            5 0
     C                   MOVE      ' '           SRRTCD            1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKDAT             7 0
     C*
     C     CAFDAT        IFLT      WKDAT
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '29'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '29'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   END                                                    END CAFDAT LT..
     C                   END                                                    END XCTOTP ....
     C                   END                                                    END DXPRCD  ...
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHAREA
     C*
     C* ================================================================
     C*** Per P. 31 I. A. 8.
     C*** Re-evaluation of the patient by the physical or occupational
     C*** therapist is limited to code 99213.  Allowed to one every
     C*** 2 weeks.  DOP is required if this eval is performed more
     C*** frequently.
     C***
     C*
     C     SVSPCD        IFEQ      'OT'
     C     SVSPCD        OREQ      'OCT'
     C     SVSPCD        OREQ      'PHT'
     C     DHPRCD        IFEQ      '99213'
     C                   Z-ADD     0             COUNT
     C                   Z-ADD     0             XXPAST
     C                   Z-ADD     0             XXFUTR
     C*
     C*
     C* FIND DATE 2 WEEKS PRIOR
     C                   Z-ADD     14            SRPST
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C                   MOVE      WKFDAT        SRDTE
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C*
     C                   MOVE      SRDTE         XXPAST
     C                   Z-ADD     XXPAST        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPAST
     C*
     C* FIND DATE 2 WEEKS FUTURE
     C                   Z-ADD     14            SRFUT
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C                   CALL      'SRFUTR8'
     C                   PARM                    WKFDAT
     C                   PARM                    SRFUT
     C                   PARM                    SRTERM
     C*
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFUTR
     C*
     C                   MOVE      '99213'       XXCPT4
     C*
     C     CAKYC         CHAIN     PCA0C                              60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCFDAT        IFGE      XXPAST
     C     XCFDAT        ANDLE     XXFUTR
     C     XCTOTP        ANDGT     *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C*
     C                   ADD       1             COUNT
     C                   END                                                    END XCFDAT
     C*
     C     CAKYC         READE     PCA0C                                  60
     C                   END                                                    END DO *IN60
     C*
     C     COUNT         IFGE      1
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'CU'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'CU'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg CU: "Exceeds number of procedures allowed
     C***              in time period, DOP Required."
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END EOB#/CHK# =  0
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END COUNT GE 1
     C                   END                                                    END DHPRCD 99213
     C*
     C                   END                                                    END SVSPCD
     C*
     C* ===============================================================
     C*** Per P. 33 I. B. 2. a.
     C*** Any office visit exceeding 99213 on established patients,
     C*** and performed for re-evaluation, is limited to ONCE EVERY
     C*** 30 DAYS and shall include the first manipulation, when
     C*** provided.
     C***
     C*
     C     DHPRCD        IFEQ      '99214'
     C     DHPRCD        OREQ      '99215'
     C                   Z-ADD     0             COUNT
     C                   Z-ADD     0             XXPAST
     C                   Z-ADD     0             XXFUTR
     C*
     C*
     C* FIND DATE 30 DAYS PRIOR
     C                   Z-ADD     30            SRPST
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C                   MOVE      WKFDAT        SRDTE
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C*
     C                   MOVE      SRDTE         XXPAST            7 0
     C                   Z-ADD     XXPAST        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPAST
     C*
     C* FIND DATE 30 DAYS FUTURE
     C                   Z-ADD     30            SRFUT
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C                   CALL      'SRFUTR8'
     C                   PARM                    WKFDAT
     C                   PARM                    SRFUT
     C                   PARM                    SRTERM
     C*
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFUTR
     C*
     C                   MOVE      '99214'       XXCPT4
     C*
     C     CAKYC         SETLL     PCA0C                              60
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XCDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFEQ      DHPRCD
     C     XCDSEQ        ANDNE     CADSEQ
     C     XCFDAT        IFGE      XXPAST
     C     XCFDAT        ANDLE     XXFUTR
     C     XCTOTP        ANDGT     *ZERO
     C*
     C                   ADD       1             COUNT
     C                   END                                                    END XCFDAT
     C                   END                                                    END DXPRCD
     C*
     C     CAKYC2        READE     PCA0C                                  60
     C                   END                                                    END DO *IN60
     C*
     C*
     C     COUNT         IFGE      1
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'CV'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'CV'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg CV: "This procedure is limited to once every 30
     C***              days"
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END COUNT GE 1
     C*
     C                   END                                                    END DHPRCD 99214
     C* ====================================================================
     C*** Per P. 9 VII. A. & C.
     C*** Hospital Observation Services:
     C*** E/M services provided on the same
     C*** date in sites other that the hosp, related to the admission
     C*** shall not be billed separately.
     C***
     C*** NOTE:  (This edit includes all other fein#.)
     C     DHPRCD        IFLT      '99218'
     C     DHPRCD        ORGT      '99220'
     C*
     C     CAKYC3        SETLL     PCA0C
     C     CAKYC3        READE     PCA0C                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD 09/21/98 ********************************************
      * Get Place of Service from auxiliary detail file
     C                   MOVE      XCDSEQ        XXDSEQ
     C*
     C     DHKY          CHAIN     PDH02                              61
      *** END ADD 09/21/98 ********************************************
     C     XCFDAT        IFEQ      CAFDAT
     C     DXPRCD        IFGE      '99218'
     C     DXPRCD        ANDLE     '99220'
      *** BEG CHG 09/21/98 ********************************************
     C********** XCIPOP    IFNE CAIPOP
     C     DXPOS         IFNE      DHPOS
      *** END CHG 09/21/98 ********************************************
     C     XCTOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '86'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '86'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 86: "A visit/Consultant charge exists for this day,
     C*              please review".
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END XCTOTP > 0
     C                   END                                                    END XCIPOP
     C                   END                                                    END DXPRCD
     C                   END                                                    END XCFDAT
     C     CAKYC3        READE     PCA0C                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD 99218
     C*
     C* ====================================================================
     C*** Per P. 66 II. 2. a.
     C*** When a starred procedures is performed at the time of the initial
     C*** office visit or emergency room visit (new patient) and this
     C*** procedure constitutes the major service
     C*** rendered during the initial visit, Procedure Code 99025 is billed
     C*** in lieu of the usual initial visit.
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99205'
     C     DHPRCD        ORGE      '99281'
     C     DHPRCD        ANDLE     '99285'
     C*
     C* SETLL AND READ THE FILE TESTING FOR AN '*' PROCEDURE
     C*
     C                   MOVE      '10000'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     AGAIN         TAG
     C*
     C     CAKY9A        READE     PCA09                                  54
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C*
     C  N54DXPRCD        IFGE      '10000'
     C     DXPRCD        ANDLE     '69979'
     C     XRTOTP        ANDGT     *ZEROS
     C*
     C                   MOVE      DXPRCD        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKY          SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C     PRSUPP        ANDEQ     'Y'
     C                   MOVE      '99025'       XXCPT4
     C     PRKY          SETGT     PPR01
     C     NXTPRB        TAG
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTPRB
     C                   ELSE
     C                   Z-ADD     *ZERO         SXMRA
     C                   Z-ADD     *ZERO         SXANAM
     C                   Z-ADD     *ZERO         SXPROF
     C                   MOVE      *BLANK        SXRPT
     C                   MOVE      *BLANK        SXSUPP
     C                   MOVE      *BLANK        SXSURG
     C                   MOVE      *BLANK        SX1A1
     C                   END
     C*
     C** SAVE THE PROCEDURE CODE FILE VALUES
     C*
     C                   Z-ADD     PRMRA         SXMRA
     C                   Z-ADD     PRANAM        SXANAM
     C                   Z-ADD     PRPROF        SXPROF
     C                   MOVE      PRRPT         SXRPT
     C                   MOVE      PRSUPP        SXSUPP
     C                   MOVE      PRSURG        SXSURG
     C                   MOVE      PR1A1         SX1A1
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'CY'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'CY'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg CY: "Office visit, reimbursed at code 99025 amount'
     C*
     C                   MOVE      '216'         XPEOMB
     C*** EOMB 216: "Payment is made for code 99025 instead of listed
     C***            Office Visit"
     C*
     C                   ELSE
     C                   GOTO      AGAIN
     C                   END
     C*
     C                   END
     C*
     C                   END
     C* ====================================================================
     C*** Per P. 66 II. B. 2. c.
     C*** When a starred
     C*** procedure performed at the time of a follow-up (established
     C*** patient) visit and this procedure constitutes the major            for
     C*** service at that visit, the follow-up visit shall not be
     C*** billed or reimbursed.
     C*
     C     DHPRCD        IFEQ      '99211'
     C     DHPRCD        OREQ      '99212'
     C     DHPRCD        OREQ      '99213'
     C     DHPRCD        OREQ      '99214'
     C     DHPRCD        OREQ      '99215'
     C*
     C* SETLL AND READ THE FILE TESTING FOR AN '*' PROCEDURE.
     C*
     C                   MOVE      '10020'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     AGAIN2        TAG
     C*
     C     CAKY9A        READE     PCA09                                  54
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C*
     C  N54DXPRCD        IFGE      '10020'
     C     DXPRCD        ANDLE     '69979'
     C     XRTOTP        ANDGT     *ZEROS
     C*
     C                   MOVE      DXPRCD        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C     PRSUPP        ANDEQ     'Y'
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'CZ'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'CZ'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg CZ: "Followup visit included in the charge of a
     C***              starred procedure".
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   ELSE
     C                   GOTO      AGAIN2
     C                   END
     C*
     C                   END
     C*
     C                   END
     C* ===============================================================
     C*** Per p. 67 II. B. 2. d.
     C*** When a starred procedure requires hospitilization, an appropriate
     C*** hospital visit is listed in addition to the starred procedure
     C*** and its follow-up care.
     C*
     C     DHPRCD        IFGE      '99231'
     C     DHPRCD        ANDLE     '99233'
     C     DHPRCD        ORGE      '99281'
     C     DHPRCD        ANDLE     '99285'
     C*
     C* SETLL AND READ THE FILE TESTING FOR AN '*' PROCEDURE.
     C*
     C                   MOVE      '10020'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  54
     C*
     C     *IN54         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFGE      '10020'
     C     DXPRCD        ANDLE     '69979'
     C     XRTOTP        ANDGT     *ZEROS
     C*
     C                   MOVE      DXPRCD        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C     PRSUPP        ANDEQ     'Y'
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'BQ'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'BQ'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg BQ: "Documentation is required for reimbursement"
     C                   LEAVE
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   END
     C                   END
     C     CAKY9A        READE     PCA09                                  54
     C  N54DXPRCD        IFGT      '69979'
     C                   LEAVE
     C                   END
     C*
     C                   END
     C                   END
     C*
     C* ===============================================================
     C*
     C*** Per P. 10 VIII. D.
     C*** Initial hosp care..(99221-99223):  These codes are used to
     C*** report the first hosp inpatient encounter with the patient
     C*** by the admitting DR. ....E/M services provided on the same
     C*** date in sites other that the hosp, related to the admission
     C*** shall not be billed separately.
     C***
     C*** NOTE: 99918-99220 included in range to exclude, (per SH)
     C     DHPRCD        IFLT      '99218'
     C     DHPRCD        ORGT      '99223'
     C*
     C                   MOVE      '99221'       XXCPT4
     C     CAKYA         SETLL     PCA0A
     C     CAKYA2        READE     PCA0A                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD 09/21/98 ********************************************
      * Get Place of Service from auxiliary detail file
     C                   MOVE      XADSEQ        XXDSEQ
     C*
     C     DHKY          CHAIN     PDH02                              61
      *** END ADD 09/21/98 ********************************************
     C     DXPRCD        IFGE      '99221'
     C     DXPRCD        ANDLE     '99223'
      *** BEG CHG 09/21/98 ********************************************
     C********** XAIPOP    IFNE CAIPOP
     C     DXPOS         IFNE      DHPOS
      *** END CHG 09/21/98 ********************************************
     C     XATOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '86'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '86'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 86: "A visit/Consultant charge exists for this day,
     C*              please review".
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END XATOTP > 0
     C                   END                                                    END XAIPOP
     C                   END                                                    END DXPRCD
     C     CAKYA2        READE     PCA0A                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD 99221
     C*
     C* ====================================================================
     C* Per P. 11 IX. D. 2.
     C* Initial Inpatient Consultants, New or Established Patient
     C* (99251-99255): ... Only one initial consultation shall be
     C* reported by a consulting doctor per admission.
     C*
     C     DHPRCD        IFGE      '99251'
     C     DHPRCD        ANDLE     '99255'
     C                   MOVE      '99251'       XXCPT4
     C     CAKYC         SETLL     PCA0C
     C*
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XCDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     XCDSEQ        IFNE      CADSEQ
     C     XCTOTP        ANDGT     *ZEROS
     C     DXPRCD        IFGE      '99251'
     C     DXPRCD        ANDLE     '99255'
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '54'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '54'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg 54: "Exceeds number of procedures allowed
     C***              in time period"
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END EOB#/CHK# =  0
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C                   END                                                    END DXPRCD GE..
     C                   END                                                    END NE DSEQ ...
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD GE..
     C* ===============================================================
     C*** Per P. 63 I. A. 1.
     C*** PACKAGE OR GLOBAL FEE CONCEPT: A single fee is billed
     C*** paid for all nec. services normally performed by the
     C*** surgeon before, during, and after the surgical proc.
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99499'
     C*
     C                   Z-ADD     1             X
     C     X             DOWLE     500
     C     FUF(X)        ANDGT     *ZERO
     C*
     C     CAFDAT        IFGE      FUF(X)
     C     CAFDAT        ANDLE     FUT(X)
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   Z-ADD     CACHRG        XPTOTD
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '29'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '29'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END
     C*
     C                   ADD       1             X
     C*
     C                   END
     C*
     C                   END
     C*
     C* ===============================================================
     C* Per P. 63 I. A. 1. b.   SURGERY
     C* Included in the global period for surgery are all preoperative
     C* visits beginning with the day prior to surgery.
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99499'
     C*
     C                   Z-ADD     1             SRFUT
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C                   CALL      'SRFUTR8'
     C                   PARM                    WKFDAT
     C                   PARM                    SRFUT
     C                   PARM                    SRTERM
     C*
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKFDAT
     C*
     C                   MOVE      XXYMD         SVYMD
     C                   MOVE      WKFDAT        XXYMD
     C                   MOVE      '00100'       XXCPT4
     C*
     C* SETLL AND READ THE FILE TESTING FOR AN '*' PROCEDURE
     C*   OR AN ANESTHESIA CODE WHICH IS CONSIDERED GLOBAL
     C*
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C*
     C     DXPRCD        IFGE      '00100'
     C     DXPRCD        ANDLE     '01999'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   Z-ADD     CACHRG        XPTOTD
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '29'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '29'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CAEOB#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   MOVE      SVYMD         XXYMD
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C                   ENDIF                                                  END DXPRCD
     C*
     C     DXPRCD        IFGE      '10000'
     C     DXPRCD        ANDLE     '69979'
     C*
     C                   MOVE      DXPRCD        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKY          SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         DOWEQ     '0'
     C     CACO#         IFEQ      PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C     PRSUPP        ANDNE     'Y'                                          NOT STARRED
     C     PRSTAT        ANDEQ     'A'
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   Z-ADD     CACHRG        XPTOTD
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '29'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '29'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CAEOB#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   MOVE      SVYMD         XXYMD
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   ENDIF                                                  END CACO#
     C                   READP     PPR01                                  54
     C     DXPRCD        IFNE      PRCPT4
     C                   LEAVE
     C                   ENDIF                                                  DXPRCD
     C                   ENDDO                                                  END LOOP
     C*
     C                   ENDIF                                                  END DXPRCD
     C     CAKY9A        READE     PCA09                                  60
     C                   ENDDO                                                  END LOOP
     C                   MOVE      SVYMD         XXYMD
     C                   END                                                    END GE 99201
     C*
     C* ===============================================================
     C*
     C*   If modifier exists, execute the mod check subroutine
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    CASGT*BLANK    PMOD
     C     MOD1          CASGT     *BLANK        PMOD
      *** END CHG *** 09/21/98 ****************************************
     C                   END
     C*
     C     CACHRG        COMP      SXMRA                              54
     C   54              Z-ADD     SXMRA         XPTOTP
     C  N54              Z-ADD     CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   EXSR      UPDAT
     C*
     C     OFFIE         ENDSR
     *********************************************************************
     *** HANDLE ALL OTHER CODES   XXXXX-XXXXX                    O T H E *
     *********************************************************************
     C     OTHE          BEGSR
     C*
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     OTHEE         ENDSR
     *********************************************************************
     C** Pathology                  Codes: 82009 - 89399         P A T H *
     *********************************************************************
     C     PATH          BEGSR
     C*
     C*
     C* ===============================================================
     C* VALID MODIFIERS ARE: 22, 26, 27, 90, WP
     C*
     C* Procedure Codes with modifiers:
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAPMOD    IFNE *BLANK
     C*----------------------------------------------------------------
     C**Per*P.*246 PATHOLOGY
     C********** CAPMOD    IFNE '22'
     C********** CAPMOD    ANDNE'26'
     C********** CAPMOD    ANDNE'27'
     C********** CAPMOD    ANDNE'90'
     C********** CAPMOD    ANDNE'WP'
     C**********
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '10'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     1             Y
      *
     C     Y             DOWLE     4
      *
     C*----------------------------------------------------------------
     C* Per P. 246 PATHOLOGY
     C     MOD(Y)        IFNE      *BLANK
     C     MOD(Y)        ANDNE     '22'
     C     MOD(Y)        ANDNE     '26'
     C     MOD(Y)        ANDNE     '27'
     C     MOD(Y)        ANDNE     '90'
     C     MOD(Y)        ANDNE     'WP'
     C                   MOVE      '10'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END EQ 22
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   END                                                    END NE *BLANK
     C*
     C***  USING UNUSUAL SERVICE MODIFIER
     C     CAPMOD        IFEQ      '22'
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'BQ'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'BQ'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg BQ: "Documentation is required for reimbursement"
     C*
     C                   END
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ADD       1             Y
     C*
     C                   ENDDO
      *** END ADD *** 09/21/98 ****************************************
     C*
     C     DHPRCD        IFGE      '80002'                                      AUTOMATED
     C     DHPRCD        ANDLE     '80019'                                      MULTICHANNEL
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'BQ'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'BQ'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg BQ: "Documentation is required for reimbursement"
     C*
     C                   END
     C*
     C* ===============================================================
     C*** Per P. 231 - PATHOLOGY
     C*** NOTE:  Only allow one test per group - (THE HIGH NUMBER ONE)
     C***
     C     DHPRCD        IFGE      '80002'                                      AUTOMATED
     C     DHPRCD        ANDLE     '80019'                                      MULTICHANNEL
     C*
     C                   MOVE      '80002'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFGE      '80002'                                                  E
     C     DXPRCD        ANDLE     '80019'
     C     DXPRCD        IFGT      DHPRCD
     C     XRTOTP        IFGT      0
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END DXPRCD
     C                   END                                                    END DXPRCD
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD 80002
     C*
     C*
     C* ===============================================================
     C*** Per P. 234 - PATHOLOGY
     C*** NOTE:  DISPLAY ERROR MESSAGE FOR PANEL TESTS
     C***
     C     DHPRCD        IFGE      '80400'
     C     DHPRCD        ANDLE     '80440'
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DE'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DE'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg DE: "Panel Testing, Read report for verification
     C*                of services".
     C                   END                                                    END DHPRCD 80400
     C*
     C* ===============================================================
     C*** Per P. 231 PANEL TESTING
     C***
     C     DHPRCD        IFGE      '80012'
     C     DHPRCD        ANDLE     '80019'
     C     DHPRCD        OREQ      '85022'
     C     DHPRCD        OREQ      '85025'
     C     DHPRCD        OREQ      '84443'
     C*
     C                   MOVE      '80050'       XXCPT4
     C     CAKY9         CHAIN     PCA09                              60
     C     *IN60         DOWEQ     '0'
     C     XRTOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '29'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '29'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END XRTOTP > 0
     C     CAKY9         READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C* ====================================================================
     C*** Per P. 231 PANEL TESTING
     C***
     C     DHPRCD        IFEQ      '85022'
     C     DHPRCD        OREQ      '85025'
     C     DHPRCD        OREQ      '86287'
     C     DHPRCD        OREQ      '86762'
     C     DHPRCD        OREQ      '86592'
     C     DHPRCD        OREQ      '86850'
     C     DHPRCD        OREQ      '86900'
     C     DHPRCD        OREQ      '86901'
     C*
     C                   MOVE      '80055'       XXCPT4
     C     CAKY9         CHAIN     PCA09                              60
     C     *IN60         DOWEQ     '0'
     C     XRTOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '29'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '29'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END XRTOTP > 0
     C     CAKY9         READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C*
     C* ====================================================================
     C*** Per P. 231 PANEL TESTING
     C***
     C     DHPRCD        IFEQ      '82040'
     C     DHPRCD        OREQ      '82250'
     C     DHPRCD        OREQ      '84075'
     C     DHPRCD        OREQ      '84450'
     C     DHPRCD        OREQ      '84460'
     C*
     C                   MOVE      '80058'       XXCPT4
     C     CAKY9         CHAIN     PCA09                              60
     C     *IN60         DOWEQ     '0'
     C     XRTOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '29'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '29'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END XRTOTP > 0
     C     CAKY9         READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C* ====================================================================
     C*** Per P. 231 PANEL TESTING
     C***
     C     DHPRCD        IFEQ      '86287'
     C     DHPRCD        OREQ      '86291'
     C     DHPRCD        OREQ      '86289'
     C     DHPRCD        OREQ      '86296'
     C     DHPRCD        OREQ      '86302'
     C*
     C                   MOVE      '80059'       XXCPT4
     C     CAKY9         CHAIN     PCA09                              60
     C     *IN60         DOWEQ     '0'
     C     XRTOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '29'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '29'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END XRTOTP > 0
     C     CAKY9         READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C* ====================================================================
     C*** Per P. 231 PANEL TESTING
     C***
     C     DHPRCD        IFEQ      '82465'
     C     DHPRCD        OREQ      '83718'
     C     DHPRCD        OREQ      '84478'
     C*
     C                   MOVE      '80061'       XXCPT4
     C     CAKY9         CHAIN     PCA09                              60
     C     *IN60         DOWEQ     '0'
     C     XRTOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '29'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '29'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END XRTOTP > 0
     C     CAKY9         READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C* ====================================================================
     C*** Per P. 231 PANEL TESTING
     C***
     C     DHPRCD        IFEQ      '84550'
     C     DHPRCD        OREQ      '85651'
     C     DHPRCD        OREQ      '86255'
     C     DHPRCD        OREQ      '86430'
     C*
     C                   MOVE      '80072'       XXCPT4
     C     CAKY9         CHAIN     PCA09                              60
     C     *IN60         DOWEQ     '0'
     C     XRTOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '29'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '29'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END XRTOTP > 0
     C     CAKY9         READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C* ====================================================================
     C*** Per P. 231 PANEL TESTING
     C***
     C     DHPRCD        IFEQ      '86644'
     C     DHPRCD        OREQ      '86694'
     C     DHPRCD        OREQ      '86762'
     C     DHPRCD        OREQ      '86777'
     C*
     C                   MOVE      '80090'       XXCPT4
     C     CAKY9         CHAIN     PCA09                              60
     C     *IN60         DOWEQ     '0'
     C     XRTOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '29'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '29'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END XRTOTP > 0
     C     CAKY9         READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C* ====================================================================
     C*** Per P. 231 PANEL TESTING
     C***
     C     DHPRCD        IFEQ      '84436'
     C     DHPRCD        OREQ      '84479'
     C*
     C                   MOVE      '80091'       XXCPT4
     C     CAKY9         CHAIN     PCA09                              60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     XRTOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '29'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '29'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END XRTOTP > 0
     C     CAKY9         READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C* ====================================================================
     C*** Per P. 231 PANEL TESTING
     C***
     C     DHPRCD        IFEQ      '84436'
     C     DHPRCD        OREQ      '84479'
     C     DHPRCD        OREQ      '84443'
     C*
     C                   MOVE      '80092'       XXCPT4
     C     CAKY9         CHAIN     PCA09                              60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     XRTOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '29'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '29'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END XRTOTP > 0
     C     CAKY9         READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C*
     C* ====================================================================
     C***  Per p. 245 Pathology
     C***  IF CAPMOD = *BLANKS OR 'WP',
     C***  SXMRA WILL BE THE TOTAL OF THE PROF & COMPONENT AMTS
     C*
     C***  USING PROFESSIONAL COMPONENT ONLY
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '26'
     C******************** Z-ADDPRPROF    SXMRA
     C******************** END
     C     '26'          LOOKUP    MOD(1)                                 99
     C   99              Z-ADD     PRPROF        SXMRA
      *** END CHG *** 09/21/98 ****************************************
     C*
     C***  USING TECHNICAL COMPONENT ONLY
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '27'
     C******************** Z-ADDPR92N1    SXMRA
     C******************** END
     C     '27'          LOOKUP    MOD(1)                                 99
     C   99              Z-ADD     PR92N1        SXMRA
      *** END CHG *** 09/21/98 ****************************************
     C*
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     PATHE         ENDSR
     *********************************************************************
     *** Procedure Code Modifier                                 P M O D *
     *********************************************************************
     C     PMOD          BEGSR
     C*
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     CAPMOD        TABKEY
     C*
     C     PMKY          CHAIN     PPM01                              64
     C*
     C     PMPERC        IFNE      *ZERO
     C     PMPERC        DIV(H)    100           XXPERC            5 2
     C                   END
     C*
     C*=====================================================================
     C*
     C********   CAEOB#    IFEQ *ZEROS
     C********   CACHK#    ANDEQ*ZEROS
     C********             Z-ADD1         Q
     C********   '00000'   LOKUPQQS,Q                    60
     C**N60                Z-ADD30        Q
     C********             MOVELCADSEQ    QQS,Q
     C********             MOVE '76'      QQC,Q
     C*** Err Msg 76: "CPT modifier used with this charge.  Verify use
     C***              and payment."
     C********             END
     C*
     C* ===============================================================
     C* Modifier -26
      *** BEG CHG *** 09/21/99 ***************************************
     C********** CAPMOD    IFEQ '26'
     C     '26'          LOOKUP    MOD(1)                                 60
     C     *IN60         IFEQ      *ON
      *** END CHG *** 09/21/99 ***************************************
     C*
     C* PER P. 42
     C* EEG STUDIES, 95812-95822, SHOULD BE PAID AT 30% OF ALLOWABLE FEE
     C* EEG STUDIES, 95950-95958
     C* EMG STUDIES, 90911, 95860-95872, 92265, 51784, 51785
     C* NCV STUDIES, 95900, 95904
     C*
     C     DHPRCD        IFGE      '95812'
     C     DHPRCD        ANDLE     '95822'
     C     DHPRCD        ORGE      '95950'
     C     DHPRCD        ANDLE     '95958'
     C     DHPRCD        OREQ      '90911'
     C     DHPRCD        ORGE      '95860'
     C     DHPRCD        ANDLE     '95872'
     C     DHPRCD        OREQ      '92265'
     C     DHPRCD        OREQ      '51784'
     C     DHPRCD        OREQ      '51785'
     C     DHPRCD        OREQ      '95900'
     C     DHPRCD        OREQ      '95904'
     C*
     C     SXMRA         MULT      .30           SXMRA
     C*
     C                   END                                                    END DHPRCD GE 95812
     C                   END                                                    END PMOD EQ 26
     C*
     C*
     C* ===============================================================
     C* Modifier -27
      *** BEG CHG *** 09/21/99 ***************************************
     C********** CAPMOD    IFEQ '27'
     C     '27'          LOOKUP    MOD(1)                                 60
     C     *IN60         IFEQ      *ON
      *** END CHG *** 09/21/99 ***************************************
     C*
     C* PER P. 42
     C* EEG STUDIES, 95812-95822, SHOULD BE PAID AT 70% OF ALLOWABLE FEE
     C* EEG STUDIES, 95950-95958
     C* EMG STUDIES, 90911, 95860-95872, 92265, 51784, 51785
     C* NCV STUDIES, 95900, 95904
     C*
     C     DHPRCD        IFGE      '95812'
     C     DHPRCD        ANDLE     '95822'
     C     DHPRCD        ORGE      '95950'
     C     DHPRCD        ANDLE     '95958'
     C     DHPRCD        OREQ      '90911'
     C     DHPRCD        ORGE      '95860'
     C     DHPRCD        ANDLE     '95872'
     C     DHPRCD        OREQ      '92265'
     C     DHPRCD        OREQ      '51784'
     C     DHPRCD        OREQ      '51785'
     C     DHPRCD        OREQ      '95900'
     C     DHPRCD        OREQ      '95904'
     C*
     C     SXMRA         MULT      .70           SXMRA
     C*
     C                   END                                                    END DHPRCD GE 95812
     C                   END                                                    END PMOD EQ 27
     C*
     C*
     C* ===============================================================
     C* Modifier -80 ASSISTANT SURGEON
     C********   CAPMOD    IFEQ '80'
     C********
     C* REDUCE 20%
     C********   SXMRA     MULT .20       SXMRA
     C********
     C********             END                             END CAPMOD
     C*
     C* ===============================================================
     C* Modifier -85 PHYSICIAN ASSISTANT (PA) AT ASSISTANT TO SURGEON
     C********   CAPMOD    IFEQ '85'
     C********
     C* REDUCE 10%
     C********   SXMRA     MULT .10       SXMRA
     C********
     C********             END                             END CAPMOD
     C********
     C* ===============================================================
     C*
     C     PMODE         ENDSR
     *********************************************************************
     C** Radiology                  Codes: 70010 - 79999         R A D I *
     *********************************************************************
     C     RADI          BEGSR
     C*
     C* ===============================================================
     C* VALID MODIFIERS ARE: 22, 26, 27, 52, 99, WP
     C*
     C* Procedure Codes with modifiers:
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAPMOD    IFNE *BLANK
     C*----------------------------------------------------------------
     C**Per*Pg.*222 RADIOLOGY
     C********** CAPMOD    IFNE '22'
     C********** CAPMOD    ANDNE'26'
     C********** CAPMOD    ANDNE'27'
     C********** CAPMOD    ANDNE'52'
     C********** CAPMOD    ANDNE'99'
     C********** CAPMOD    ANDNE'WP'
     C**********
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '10'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     1             Y
      *
     C     Y             DOWLE     4
      *
     C     MOD(Y)        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per Pg. 222 RADIOLOGY
     C     MOD(Y)        ANDNE     '22'
     C     MOD(Y)        ANDNE     '26'
     C     MOD(Y)        ANDNE     '27'
     C     MOD(Y)        ANDNE     '52'
     C     MOD(Y)        ANDNE     '99'
     C     MOD(Y)        ANDNE     'WP'
     C                   MOVE      '10'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
     C                   ENDIF                                                  MOD,Y NE BLANK
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END NE *BLANK
      *** END DEL *** 09/21/98 ****************************************
     C*
     C***  USING UNUSUAL SERVICE MODIFIER
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '22'
     C     MOD(Y)        IFEQ      '22'
      *** END CHG *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'BQ'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'BQ'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg BQ: "Documentation is required for reimbursement"
     C*
     C                   END
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ADD       1             Y
     C*
     C                   ENDDO                                                  Y DOWLE 4
      *** END ADD *** 09/21/98 ****************************************
     C*
     C***  Per p. 217 Radiology
     C***  IF CAPMOD = *BLANKS OR 'WP',
     C***  SXMRA WILL BE THE TOTAL OF THE PROF & COMPONENT AMTS
     C*
     C*
     C***  USING PROFESSIONAL COMPONENT ONLY
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '26'
     C**********           Z-ADDPRPROF    SXMRA
     C**********           END
     C     '26'          LOOKUP    MOD(1)                                 99
     C   99              Z-ADD     PRPROF        SXMRA
      *** BEG CHG *** 09/21/98 ****************************************
     C*
     C***  USING TECHNICAL COMPONENT ONLY
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '27'
     C**********           Z-ADDPR92N1    SXMRA
     C**********           END
     C     '27'          LOOKUP    MOD(1)                                 99
     C   99              Z-ADD     PR92N1        SXMRA
      *** BEG CHG *** 09/21/98 ****************************************
     C*
     C*
     C*** Per P. 12 XIII. C. - EVALUATION AND MANAGEMENT
     C*** The following services are included in reporting critical
     C*** care when performed during the critical period by the
     C*** doctor providing critical care and shall not be
     C*** billed separately:
     C***
     C     DHPRCD        IFEQ      '71010'
     C     DHPRCD        OREQ      '71020'
     C*
     C                   MOVE      '99291'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFGE      '99291'                                      CRITICAL CARE
     C     DXPRCD        ANDLE     '99292'                                      SERVICES
     C     XRTOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'CQ'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'CQ'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg CQ: "This service is included under Critical
     C*              Care Services (99291-99292)".
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END DXPRCD
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD 99221
     C*
     C* ====================================================================
     C*** Per P. 204 I. D. - RADIOLOGY - VIDEOFLUOROSCOPY
     C*** When billing for either a myelogram, or injections,
     C*** videofluoroscopy shall not be billed separately.
     C***
     C     DHPRCD        IFEQ      '76120'
     C     DHPRCD        OREQ      '76125'
     C     DHPRCD        OREQ      '76000'
     C*
     C                   MOVE      '70000'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFEQ      '70010'
     C     DXPRCD        OREQ      '72240'
     C     DXPRCD        OREQ      '72255'
     C     DXPRCD        OREQ      '72265'
     C     DXPRCD        OREQ      '72270'
     C     DXPRCD        OREQ      '72285'
     C     DXPRCD        OREQ      '72295'
     C     DXPRCD        OREQ      '75746'
     C*
     C     XRTOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DD'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DD'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DD: "If billing for videofluoroscopy, no reimbursement
     C*              is allowed".
     C*
     C                   LEAVE
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END DXPRCD
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD 76120
     C*
     C* =================================================================
     C* Per p. 218 RADIOLOGY
     C* Magnetic resonance imaging: This procedure shall be billed
     C* according to the following 3 categories:
     C*   1. Limited  (L) < 12 slices  (mod -52)
     C*   2. Standard (S) 13 - 24
     C*   3. Extended (E) > 24 slices  (mod -22)
     C*
     C* Since we do not have the capability to process 2 procedure
     C* modifiers. The part code is used instead with the letters
     C* L, S, and E.
     C* MRI:  LIMITED less than 12 slices
      *** BEG CHG *** 09/21/98 ****************************************
     C********** DHAREA    IFEQ 'L'
     C     '52'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
      *** END CHG *** 09/21/98 ****************************************
     C                   Z-ADD     1             X
     C     DHPRCD        LOOKUP    RADLC(X)                               70
     C     *IN70         IFEQ      '1'
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '  '
     C********** CAPMOD    OREQ 'WP'
     C     'WP'          LOOKUP    MOD(1)                                 99
     C     MOD1          IFEQ      *BLANKS
     C     *IN99         OREQ      *ON
      *** END CHG *** 09/21/98 ****************************************
     C                   Z-ADD     RADL(X)       SXMRA
     C                   END
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '26'
     C**********           Z-ADDRADLP,X   SXMRA
     C**********           END
     C********** CAPMOD    IFEQ '27'
     C**********           Z-ADDRADLT,X   SXMRA
     C**********           END
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     '26'          LOOKUP    MOD(1)                                 99
     C   99              Z-ADD     RADLP(X)      SXMRA
     C     '27'          LOOKUP    MOD(1)                                 99
     C   99              Z-ADD     RADLT(X)      SXMRA
      *** END ADD *** 09/21/98 ****************************************
     C*
     C                   ELSE                                                   *IN70 = 0
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'CW'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'CW'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg CW: "This CPT-4 code does NOT have the corresponding
     C*             L, S, or E value".
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
     C                   END                                                    END *IN70
     C                   END                                                    END DHAREA EQ 'L'
     C*
     C* MRI:  EXTENDED greater than 24 slices
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C********** DHAREA    IFEQ 'E'
     C     '22'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
      *** END CHG *** 09/21/98 ****************************************
     C                   Z-ADD     1             X
     C     DHPRCD        LOOKUP    RADEC(X)                               70
     C     *IN70         IFEQ      '1'
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '  '
     C********** CAPMOD    OREQ 'WP'
     C**********           Z-ADDRADE,X    SXMRA
     C**********           END
     C********** CAPMOD    IFEQ '26'
     C**********           Z-ADDRADEP,X   SXMRA
     C**********           END
     C********** CAPMOD    IFEQ '27'
     C**********           Z-ADDRADET,X   SXMRA
     C**********           END
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     'WP'          LOOKUP    MOD(1)                                 99
      *
     C     MOD1          IFEQ      *BLANKS
     C     *IN99         OREQ      *ON
      *
     C                   Z-ADD     RADE(X)       SXMRA
      *
     C                   END
      *
     C     '26'          LOOKUP    MOD(1)                                 99
     C   99              Z-ADD     RADEP(X)      SXMRA
      *
     C     '27'          LOOKUP    MOD(1)                                 99
     C   99              Z-ADD     RADET(X)      SXMRA
      *** END ADD *** 09/21/98 ****************************************
     C*
     C                   ELSE                                                   *IN70 = 0
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'CW'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'CW'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg CW: "This CPT-4 code does NOT have the corresponding
     C*             L, S, or E value".
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
     C                   END                                                    END *IN70
     C                   END                                                    END DHAREA EQ 'E'
     C*
     C* Per P. 204 - Radiation Oncology
     C* The procedure codes for radiation oncology provide for
     C* teletherapy and brachytherapy to include the following
     C* (see p. 204) and normal follow-up care for up to 3 months
     C* following its completion.
     C*
     C     DHPRCD        IFGE      '77331'
     C     DHPRCD        ANDLE     '77399'
     C     DHPRCD        ORGE      '77419'
     C     DHPRCD        ANDLE     '77499'                                                  ED
     C                   MOVE      '77305'       XXCPT4
     C     CAKYC         SETLL     PCA0C
     C*
     C*** READ FILE IN FROM DATE SEQUENCE   FOR THIS CASE/FEIN
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XCDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFGE      '77305'
     C     DXPRCD        ANDLE     '77321'
     C     DXPRCD        ORGE      '77326'
     C     DXPRCD        ANDLE     '77328'                                                  ED
     C     DXPRCD        ORGE      '77761'
     C     DXPRCD        ANDLE     '77789'
     C     DXPRCD        OREQ      '77799'
     C     XCTOTP        IFGT      *ZEROS
     C*
     C                   Z-ADD     XCFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C                   Z-ADD     90            SRFUTR            5 0
     C                   MOVE      ' '           SRRTCD            1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKDAT             7 0
     C*
     C     CAFDAT        IFLT      WKDAT
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '29'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '29'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   END                                                    END CAFDAT LT..
     C                   END                                                    END XCTOTP ....
     C                   END                                                    END DXPRCD  ...
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHAREA
     C*
     C*
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     RADIE         ENDSR
     *********************************************************************
     *** Surgery                    Codes: 10000 - 69979         S U R G *
     *********************************************************************
     C     SURG          BEGSR
     C*
     C* ===============================================================
     C* PER P. 68 MODIFIER -20 INVALID WITH THE FOLLOWING CPT-4 CODES
     C*           MICROSURGERY
     C*
     C* NOTE: PERFORM THIS EDIT FIRST, TO AVOID GETTING ADDITIONAL MSGS
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '20'
     C     '20'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
      *** END CHG *** 09/21/98 ****************************************
     C     DHPRCD        IFGE      '61304'
     C     DHPRCD        ANDLE     '61711'
     C     DHPRCD        ORGE      '62010'
     C     DHPRCD        ANDLE     '62100'
     C     DHPRCD        ORGE      '63081'
     C     DHPRCD        ANDLE     '63308'
     C     DHPRCD        ORGE      '63704'
     C     DHPRCD        ANDLE     '63710'
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '10'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '10'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END                                                    END DHPRCD
     C*
     C                   END                                                    *IN99=OFF
     C* ===============================================================
     C*
     C*
      *** BEG DEL *** 09/21/99 ***************************************
     C********** CAPMOD    IFNE *BLANK
     C**********
     C********** CAPMOD    IFEQ '20'
     C********** CAPMOD    OREQ '22'
     C********** CAPMOD    OREQ '26'
     C********** CAPMOD    OREQ '27'
     C********** CAPMOD    OREQ '47'
     C********** CAPMOD    OREQ '50'
     C********** CAPMOD    OREQ '51'
     C********** CAPMOD    OREQ '52'
     C********** CAPMOD    OREQ '54'
     C********** CAPMOD    OREQ '55'
     C********** CAPMOD    OREQ '56'
     C********** CAPMOD    OREQ '58'
     C********** CAPMOD    OREQ '62'
     C********** CAPMOD    OREQ '65'
     C********** CAPMOD    OREQ '66'
     C********** CAPMOD    OREQ '76'
     C********** CAPMOD    OREQ '78'
     C********** CAPMOD    OREQ '79'
     C********** CAPMOD    OREQ '80'
     C********** CAPMOD    OREQ '85'
     C********** CAPMOD    OREQ '99'
     C********** CAPMOD    OREQ 'AP'
     C**********
     C********** CAPMOD    IFEQ '54'
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '70'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     1             Y
      *
     C     Y             DOWLE     4
      *
     C     MOD(Y)        IFNE      *BLANK
     C*
     C     MOD(Y)        IFEQ      '20'
     C     MOD(Y)        OREQ      '22'
     C     MOD(Y)        OREQ      '26'
     C     MOD(Y)        OREQ      '27'
     C     MOD(Y)        OREQ      '47'
     C     MOD(Y)        OREQ      '50'
     C     MOD(Y)        OREQ      '51'
     C     MOD(Y)        OREQ      '52'
     C     MOD(Y)        OREQ      '54'
     C     MOD(Y)        OREQ      '55'
     C     MOD(Y)        OREQ      '56'
     C     MOD(Y)        OREQ      '58'
     C     MOD(Y)        OREQ      '62'
     C     MOD(Y)        OREQ      '65'
     C     MOD(Y)        OREQ      '66'
     C     MOD(Y)        OREQ      '76'
     C     MOD(Y)        OREQ      '78'
     C     MOD(Y)        OREQ      '79'
     C     MOD(Y)        OREQ      '80'
     C     MOD(Y)        OREQ      '85'
     C     MOD(Y)        OREQ      '99'
     C     MOD(Y)        OREQ      'AP'
     C*
     C     MOD(Y)        IFEQ      '54'
     C                   MOVE      '70'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 70: "The item indicates Surgical Care Only mod-54
     C*             Refer to fee schedule for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END CAPMOD EQ 54
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '55'
     C     MOD(Y)        IFEQ      '55'
      *** END CHG *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '64'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '64'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 64: "The item indicates Postoper Mgmt Only mod-55
     C*             Refer to fee schedule for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END CAPMOD EQ 55
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '56'
     C     MOD(Y)        IFEQ      '56'
      *** END CHG *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'BR'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'BR'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg BR: "The item indicates Pre-oper Mgmt Only mod-56
     C*             Refer to fee schedule for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END CAPMOD EQ 56
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '66'
     C     MOD(Y)        IFEQ      '66'
      *** END CHG *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '66'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '66'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 66: "Item is surgical team mod-66,
     C*             Refer to fee schedule for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END CAPMOD EQ 66
     C*
     C******************** EXSR PMOD
     C*
     C                   ELSE                                                   ELSE CAPMOD
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '10'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '10'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END                                                    END CAPMOD EQ 20
     C*
     C                   END                                                    END CAPMOD
     C*
     C* ===============================================================
     C* PER P. 68-69 SURGERY
     C***  MODIFIERS WHICH REQUIRE DOCUMENTATION OF PROCEDURE
     C***  Note: Modifiers 54, 55, 56, 66 already excluded
     C***
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '20'
     C********** CAPMOD    OREQ '22'
     C********** CAPMOD    OREQ '52'
     C********** CAPMOD    OREQ '62'
     C********** CAPMOD    OREQ '65'
     C********** CAPMOD    OREQ '85'
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'BQ'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     MOD(Y)        IFEQ      '20'
     C     MOD(Y)        OREQ      '22'
     C     MOD(Y)        OREQ      '52'
     C     MOD(Y)        OREQ      '62'
     C     MOD(Y)        OREQ      '65'
     C     MOD(Y)        OREQ      '85'
     C                   MOVE      'BQ'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg BQ: "Documentation is required for reimbursement"
     C*
     C                   END
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ADD       1             Y                              Y DOWLE 4
     C*
     C                   ENDDO                                                  Y DOWLE 4
      *** END ADD *** 09/21/98 ****************************************
     C*
     C*
     C* ===============================================================
     C**
     C*** Per P. 66 I. E. 4. C.
     C*** Epidural steroid injections shall be billed using code
     C*** 62289 only.
     C***
     C     DHPRCD        IFEQ      '62281'
     C     DHPRCD        OREQ      '62282'
     C     DHPRCD        OREQ      '62298'
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DF'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DF'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DF: "Epidural injections should be billed using
     C*              code 62289".
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END                                                    END DHPRCD 62281
     C*
     C* ====================================================================
     C* PER P. 66 I. E. 7. Separate procedures.
     C* The unbundling of integral parts of a total procedure with
     C* a separate charge for each shall not be reimbursed.
     C* NOTE: If a separate proc is coded w/another SURG code,
     C*       display error message ONLY.
     C*
     C                   Z-ADD     1             R                 5 0
     C     DHPRCD        LOOKUP    SEPP(R)                                60
     C     *IN60         IFEQ      '1'
     C*
     C                   MOVE      '10040'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFGE      '10040'                                      SURG CODES
     C     DXPRCD        ANDLE     '69979'
     C     DXPRCD        IFEQ      DHPRCD
     C     XRDSEQ        ANDNE     CADSEQ
     C     DXPRCD        ORNE      DHPRCD
     C     XRTOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'CX'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'CX'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg CX: "Possible unbundling of a total proc,
     C*              separate procedure coded W/SURG".
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END DXPRCD
     C                   END                                                    END DXPRCD
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END DOW *IN60
     C*
     C                   END                                                    END *IN60 EQ 1
     C*
     C* ===============================================================
     C*   If current code exists in table, must have associated code else
     C*     error has occurred, MAJOR/MINOR TABLE
     C*
     C                   Z-ADD     1             R                 5 0
     C                   MOVE      '0'           *IN54
     C     SURLOK        TAG
     C     DHPRCD        LOOKUP    SUR1(R)                                60
     C     *IN60         IFEQ      '1'
     C                   MOVE      SUR2(R)       XXCPT4
     C     CAKY9         CHAIN     PCA09                              54
     C     *IN54         IFEQ      '1'
     C     R             IFLT      264
     C                   ADD       1             R
     C                   GOTO      SURLOK
     C                   ELSE
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      '215'         XPEOMB
     C*** EOMB 215: "A major code must be present to pay this item"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C*
     C                   END
     C                   ELSE
     C     *IN54         IFEQ      '1'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      '215'         XPEOMB
     C*** EOMB 215: "A major code must be present to pay this item"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C                   END
     C* ===============================================================
     C**
     C*** Per P. 12 XIII. C. - EVALUATION AND MANAGEMENT
     C*** The following services are included in reporting critical
     C*** care when performed during the critical period by the
     C*** doctor providing critical care and shall not be
     C*** billed separately:
     C***
     C     DHPRCD        IFEQ      '36000'
     C     DHPRCD        OREQ      '36410'
     C     DHPRCD        OREQ      '36415'
     C     DHPRCD        OREQ      '36600'
     C*
     C                   MOVE      '99291'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFEQ      '99291'                                      CRITICAL CARE
     C     DXPRCD        OREQ      '99292'                                      SERVICES
     C     XRTOTP        IFGT      0
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'CQ'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'CQ'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg CQ: "This service is included under Critical
     C*              Care Services (99291-99292)".
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CHK#/EOB#
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END DXPRCD
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD 36000
     C*
     C* ===============================================================
     C*** Per P. 66 I. E. 6. b. SURGERY
     C*** Manipulation Under Anesthesia (MUA)
     C*** Manipulation of a joint under anes preceded or followed
     C*** by a surgical proc. on the same day does not warrant reimb.
     C***
     C     DHPRCD        IFEQ      '22505'
     C*
     C                   MOVE      '22100'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFGE      '22100'
     C     DXPRCD        ANDLE     '22855'
     C     DXPRCD        IFEQ      DHPRCD
     C     XRDSEQ        ANDNE     CADSEQ
     C     DXPRCD        ORNE      DHPRCD
     C     XRTOTP        IFGT      0
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DA'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DA'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DA: "Manipulation under ANES of a joint, w/SURG
     C*              same day, (disallowed)".
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** ENDIF                           END CACHK#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END DXPRCD
     C                   END                                                    END DXPRCD
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C* ====================================================================
     C     DHPRCD        IFEQ      '23700'
     C*
     C                   MOVE      '23000'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFGE      '23000'
     C     DXPRCD        ANDLE     '23921'
     C     DXPRCD        IFEQ      DHPRCD
     C     XRDSEQ        ANDNE     CADSEQ
     C     DXPRCD        ORNE      DHPRCD
     C     XRTOTP        IFGT      0
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DA'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DA'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DA: "Manipulation under ANES of a joint, w/SURG
     C*              same day, (disallowed)".
     C*
      *** END DEL *** 09/21/98 ****************************************
     C******************** ENDIF                           END CACHK#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END DXPRCD
     C                   END                                                    END DXPRCD
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C* ====================================================================
     C     DHPRCD        IFEQ      '27275'
     C*
     C                   MOVE      '26990'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFGE      '26990'
     C     DXPRCD        ANDLE     '27299'
     C     DXPRCD        IFEQ      DHPRCD
     C     XRDSEQ        ANDNE     CADSEQ
     C     DXPRCD        ORNE      DHPRCD
     C     XRTOTP        IFGT      0
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DA'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DA'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DA: "Manipulation under ANES of a joint, w/SURG
     C*              same day, (disallowed)".
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** ENDIF                           END CACHK#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END DXPRCD
     C                   END                                                    END DXPRCD
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C* ====================================================================
     C     DHPRCD        IFEQ      '27570'
     C*
     C                   MOVE      '27301'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFGE      '27301'
     C     DXPRCD        ANDLE     '27599'
     C     DXPRCD        IFEQ      DHPRCD
     C     XRDSEQ        ANDNE     CADSEQ
     C     DXPRCD        ORNE      DHPRCD
     C     XRTOTP        IFGT      0
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DA'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DA'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DA: "Manipulation under ANES of a joint, w/SURG
     C*              same day, (disallowed)".
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** ENDIF                           END CACHK#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END DXPRCD
     C                   END                                                    END DXPRCD
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C* ====================================================================
     C     DHPRCD        IFEQ      '27860'
     C*
     C                   MOVE      '27600'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFGE      '27600'
     C     DXPRCD        ANDLE     '27889'
     C     DXPRCD        IFEQ      DHPRCD
     C     XRDSEQ        ANDNE     CADSEQ
     C     DXPRCD        ORNE      DHPRCD
     C     XRTOTP        IFGT      0
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'DA'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'DA'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg DA: "Manipulation under ANES of a joint, w/SURG
     C*              same day, (disallowed)".
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** ENDIF                           END CACHK#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END DXPRCD
     C                   END                                                    END DXPRCD
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C* ===============================================================
     C*** Per P. 195 V. D. ANES
     C*** For diagnostic or therapeutic nerve blocks performed
     C*** by the surgeon, anes, or CRNA, only 1 reimbursement
     C*** per procedure shall be allowed.
     C***
     C     DHPRCD        IFGE      '62274'
     C     DHPRCD        ANDLE     '62279'
     C     DHPRCD        ORGE      '64400'
     C     DHPRCD        ANDLE     '64530'
     C*
     C                   MOVE      '62274'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              61
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFGE      '62274'
     C     DXPRCD        ANDLE     '62279'
     C     DXPRCD        ORGE      '64400'
     C     DXPRCD        ANDLE     '64530'
     C     DXPRCD        IFEQ      DHPRCD
     C     XRDSEQ        ANDNE     CADSEQ
     C     DXPRCD        ORNE      DHPRCD
     C     XRTOTP        IFGT      0
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '54'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '54'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg 54: "Exceeds number of procedures allowed
     C***              in time period"
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** ENDIF                           END CACHK#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END DXPRCD
     C                   END                                                    END DXPRCD
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C* ===============================================================
     C*
     C* NOTE: ALL SURGERY PROCEDURES ARE WRITTEN TO THE WORKFILE
     C*       EXCEPT THOSE WITH MODIFIER -50
     C*       AND THOSE WHICH ARE IN THE NOCUT TABLE
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAPMOD    IFNE '50'
     C     '50'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *OFF
      *** END CHG *** 09/21/98 ****************************************
     C* ===============================================================
     C* Write record to Workfile
     C* Code has passed all edits - Write to the FACAPTX Workfile
     C* Only load "current charges" into the FACAPTX Workfile
     C*
     C* Exceptions are found in NOCT table and cannot be cut
     C*
     C                   Z-ADD     1             T
     C     DHPRCD        LOOKUP    NOCT(T)                                60
     C     *IN60         CABEQ     '1'           NOCUT
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAEOB#    IFEQ *ZEROS
     C********** CACHK#    ANDEQ*ZEROS
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     SXMRA         CAMRA
     C                   MOVE      'S'           CATYPE
      *** BEG ADD *** 09/21/98 ****************************************
      * SURG2 processes using CATOTC (from workfile FACALTX). Load it.
     C                   Z-ADD     CACHRG        CATOTC
      *** END ADD *** 09/21/98 ****************************************
     C                   WRITE     PCAWK1
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END CAEOB# EQ *ZERO
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   END                                                    END CAPMOD EQ ' '42
     C*
     C     NOCUT         TAG
     C*
     C* =================================================================
     C* PER P. 68-85 MODIFIERS WHICH CAUSE A CHANGE IN THE MAR
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAPMOD    IFEQ '20'
     C**********           MULT 1.25      SXMRA
     C**********           ENDIF
     C**********
     C********** CAPMOD    IFEQ '65'
     C**********           MULT .75       SXMRA
     C**********           ENDIF
     C**********
     C********** CAPMOD    IFEQ '80'
     C**********           MULT .25       SXMRA
     C**********           ENDIF
     C**********
     C********** CAPMOD    IFEQ '85'
     C**********           MULT .10       SXMRA
     C**********           ENDIF
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     '20'          LOOKUP    MOD(1)                                 99
     C   99              MULT      1.25          SXMRA
     C*
     C     '65'          LOOKUP    MOD(1)                                 99
     C   99              MULT      .75           SXMRA
     C*
     C     '80'          LOOKUP    MOD(1)                                 99
     C   99              MULT      .25           SXMRA
     C*
     C     '85'          LOOKUP    MOD(1)                                 99
     C   99              MULT      .10           SXMRA
      *** END ADD *** 09/21/98 ****************************************
     C*
     C* =================================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     SURGE         ENDSR
     C********************************************************************
     C** Process SURG Codes for 100%, 50%                      S U R G 2 *
     C*  This subroutine is called at each date, provider level break    *
     C********************************************************************
     C*----------------------------------------------------------------
     C     SURG2         BEGSR
     C*
     C*----------------------------------------------------------------
     C*--- Reading the SURG Detail Charge Workfile --------------------
     C*
     C* ===============================================================
     C* Need to determine the number of surgical procedures for day
     C*
     C                   Z-ADD     0             COUNT
     C                   Z-ADD     0             CNTB1
     C                   Z-ADD     0             CNTB2
     C     *LOVAL        SETLL     PCAWK2
     C                   READ      PCAWK2                                 60
     C     *IN60         IFEQ      '0'
     C* Read history to determine if a SURG code has already been paid
     C                   Z-ADD     P2FDAT        PPFDAT
     C                   Z-ADD     P2FEIN        PPFEIN
     C                   Z-ADD     P2MOD         PPMOD
     C                   MOVE      '10000'       PPCPT4
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      *BLANKS       P2EOMB
      *** END ADD *** 09/21/98 ****************************************
     C     CAKY9C        SETLL     PCA09
     C     CAKY9D        READE     PCA09                                  61
     C     *IN61         DOWEQ     '0'
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     XRDSEQ        XXDSEQ
      *
     C     DHKY          CHAIN     PDH02                              62
      *
      *** END ADD *** 09/21/98 ****************************************
     C     DXPRCD        IFLE      '69979'
      *** BEG CHG *** 09/21/98 ****************************************
     C********** XREOB#    IFNE *ZEROS
     C     XREOB#        IFNE      QQEOB#
      *** END CHG *** 09/21/98 ****************************************
     C     DXAREA        IFEQ      'O'
     C                   ADD       1             COUNT
     C                   ELSE
     C     DXAREA        IFEQ      'O1'
     C                   ADD       1             CNTB1
     C                   ELSE
     C     DXAREA        IFEQ      'O2'
     C                   ADD       1             CNTB2
     C                   END                                                    END EQ 'O1'
     C                   END                                                    END EQ 'O2'
     C                   END                                                    END EQ 'O'
     C                   END                                                    END NE *ZEROS
     C                   END                                                    END LE 69979
     C     CAKY9D        READE     PCA09                                  61
     C                   END                                                    END 0 *IN61
     C*
     C                   END                                                    END 0 *IN60
     C*
     C     *IN60         DOWEQ     '0'
     C*
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      *BLANKS       P2EOMB
     C*
     C* Load modifiers into an array to be processed later
     C                   CLEAR                   MOD
     C*
     C                   Z-ADD     P2DSEQ        XXDSEQ
     C*
     C     DHKY          CHAIN     PDH01                              61
     C*
     C     *IN61         IFEQ      *OFF
     C*
     C                   MOVEL     DHMOD1        MOD(1)
     C                   MOVEL     DHMOD2        MOD(2)
     C                   MOVEL     DHMOD3        MOD(3)
     C                   MOVEL     DHMOD4        MOD(4)
     C*
     C                   MOVEA     MOD           MOD1              8
     C                   MOVE      DHAREA        P2PART
     C                   MOVE      DHPRCD        P2CPT4
     C*
     C                   ENDIF                                                  *IN60=OFF
      *** END ADD *** 09/21/98 ****************************************
     C*
     C* ===============================================================
     C* PER P. 64 MULTIPLE PROCEDURES:  100% of the MAR for the
     C*  primary procedure, (major procedure reflecting the
     C*  greatest value).
     C*  50% of the MAR for secondary or subsequent procedure(s)
     C*
     C* One operative site: Body Part "O"
     C*  Highest value 100%
     C*  after 1st      50%
     C*
     C     P2PART        IFEQ      'O'
     C     COUNT         IFLE      0
     C                   Z-ADD     1             P2BRP                          1st: 100%
     C                   ADD       1             COUNT
     C                   ELSE                                                   ELSE EQ 0
     C     COUNT         IFGE      1
     C                   Z-ADD     .50           P2BRP                          2nd: 50%
     C                   ADD       1             COUNT
     C                   END                                                    END  EQ 1
     C                   END                                                    END  LE 0
     C*
     C                   ELSE                                                   ELSE EQ 'O'
     C*
     C* ===============================================================
     C* Bilateral Site/Primary: Body Part "O1"
     C*  Highest value 100%
     C*  after 1st      50%
     C*
     C     P2PART        IFEQ      'O1'
     C     CNTB1         IFLE      0
     C                   Z-ADD     1             P2BRP                          1st: 100%
     C                   ADD       1             CNTB1
     C                   ELSE                                                   ELSE EQ 0
     C     CNTB1         IFGE      1
     C                   Z-ADD     .50           P2BRP                          2nd: 50%
     C                   ADD       1             CNTB1
     C                   END                                                    END  GE 1
     C                   END                                                    END  LE 0
     C*
     C                   ELSE                                                   ELSE EQ 'O1'
     C*
     C* Bilateral Site/Remote: Body Part "O2"
     C*  Highest value 100%
     C*  after 1st      50%
     C*
     C     P2PART        IFEQ      'O2'
     C     CNTB2         IFLE      0
     C                   Z-ADD     1             P2BRP                          1st: 100%
     C                   ADD       1             CNTB2
     C                   ELSE                                                   ELSE LE 0
     C     CNTB2         IFGE      1
     C                   Z-ADD     .50           P2BRP                          2nd: 50%
     C                   ADD       1             CNTB2
     C                   END                                                    END  GE 1
     C                   END                                                    END  LE 0
     C*
     C                   END                                                    END EQ 'O2'
     C                   END                                                    END EQ 'O1'
     C                   END                                                    END EQ 'O'
     C*
     C     P2MRA         MULT      P2BRP         P2MRA
     C*
     C* =================================================================
     C* PER P. 68-85 MODIFIERS WHICH CAUSE A CHANGE IN THE MAR
     C* NOTE: THESE CHANGES OCCUR AFTER THE PRIMARY/SECONDARY
     C* PROCEDURES ARE FOUND (PRIMARY = HIGHEST MRA)
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** P2PMOD    IFEQ '20'
     C**********           MULT 1.25      P2MRA
     C**********           ENDIF
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     '20'          LOOKUP    MOD(1)                                 99
     C   99              MULT      1.25          P2MRA
      *** END ADD *** 09/21/98 ****************************************
     C*
     C* ===============================================================
      *** BEG CHG *** 09/21/98 ****************************************
     C********** P2PMOD    IFEQ '26'
     C     '26'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
      *** END CHG *** 09/21/98 ****************************************
     C* PER P. 42
     C* EMG STUDIES, 90911, 95860-95872, 92265, 51784, 51785
     C*
     C     P2CPT4        IFEQ      '51784'
     C     P2CPT4        OREQ      '51785'
     C                   MULT      .30           P2MRA
     C                   END                                                    END P2CPT4       12
     C                   END                                                    *IN99=ON
     C*
     C* ===============================================================
      *** BEG CHG *** 09/21/98 ****************************************
     C********** P2PMOD    IFEQ '27'
     C     '27'          LOOKUP    MOD(1)                                 99
     C     *IN99         IFEQ      *ON
      *** END CHG *** 09/21/98 ****************************************
     C* PER P. 42
     C* EMG STUDIES, 90911, 95860-95872, 92265, 51784, 51785
     C*
     C     P2CPT4        IFEQ      '51784'
     C     P2CPT4        OREQ      '51785'
     C                   MULT      .70           P2MRA
     C                   END                                                    END P2CPT4       12
     C                   END                                                    END PMOD EQ 27
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** P2PMOD    IFEQ '65'
     C**********           MULT .75       P2MRA
     C**********           ENDIF
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     '65'          LOOKUP    MOD(1)                                 99
     C   99              MULT      .75           P2MRA
      *** END ADD *** 09/21/98 ****************************************
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** P2PMOD    IFEQ '80'
     C**********           MULT .25       P2MRA
     C**********           ENDIF
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     '80'          LOOKUP    MOD(1)                                 99
     C   99              MULT      .25           P2MRA
      *** END ADD *** 09/21/98 ****************************************
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** P2PMOD    IFEQ '85'
     C**********           MULT .10       P2MRA
     C**********           ENDIF
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     '85'          LOOKUP    MOD(1)                                 99
     C   99              MULT      .10           P2MRA
      *** END ADD *** 09/21/98 ****************************************
     C*
     C* =================================================================
     C*
     C     P2TOTC        COMP      P2MRA                              54        >>>>>
     C   54              Z-ADD     P2MRA         P2TOTA
     C   54              Z-ADD     *ZEROS        P2DIFF
     C  N54              Z-ADD     P2TOTC        P2TOTA
     C  N54P2MRA         SUB       P2TOTA        P2DIFF
     C     P2TOTC        SUB       P2TOTA        P2TOTD
     C                   Z-ADD     P2TOTA        P2TOTP
     C*** BEGIN CHG *** 04/03/97 **************************************
     C*****************    Z-ADDP2MRA     P2BRTC
     C                   Z-ADD     P2TOTA        P2BRTC
     C*** END   CHG *** 04/03/97 **************************************
     C*
     C*  Using FACAPLA record, chain to FDCAP record to update
     C*  Only process the record if not an override type
     C                   Z-ADD     P2FDAT        PPFDAT
     C                   Z-ADD     P2FEIN        PPFEIN
     C                   Z-ADD     P2MOD         PPMOD
     C                   MOVE      P2CPT4        PPCPT4
     C                   Z-ADD     P2DSEQ        PPDSEQ
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAKY8A    CHAINPCA08                61
     C     CAKY5         CHAIN     PCA05                              61
      *
     C     *IN61         IFEQ      *OFF
      *** END CHG *** 09/21/98 ****************************************
     C*
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     XRCSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              62
      *** BEG ADD *** 09/21/98 ****************************************
     C*
     C     *IN62         IFEQ      *OFF
     C*
      *** END ADD *** 09/21/98 ****************************************
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CSOVRD    IFNE 'Y'
     C     CSOVRD        ANDNE     'Y'
      *** END CHG *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C********** XRCHK#    ANDEQ*ZERO
     C********** XREOB#    ANDEQ*ZERO
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     P2TOTA        XRTOTA
     C                   Z-ADD     P2TOTD        XRTOTD
     C                   Z-ADD     P2TOTP        XRTOTP
     C                   Z-ADD     P2DIFF        XRDIFF
     C                   Z-ADD     P2BRTC        XRBRTC
     C                   MOVEL     P2PMOD        XRPMOD
     C     P2TOTD        IFGT      *ZERO
     C     P2EOMB        ANDEQ     *BLANKS
     C                   MOVE      'F  '         P2EOMB
     C*
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C                   END                                                    END P2TOTD GT *ZERO
     *** BEG ADD *** 09/21/98 *****************************************
     C* Call subroutine to write EOMB to FADEP. Set QQFILE so EOMB
     C* subroutine will use the P2 fields for updating.
     C                   MOVE      P2EOMB        XPEOMB
      *
     C                   MOVE      P2DSEQ        QQDSEQ
      *
     C                   MOVE      'P2'          QQFILE            2
      *
     C     XXPASS        IFEQ      '2'
     C                   EXSR      EOMB
     C                   ENDIF
      *
     C* If a reduction was made and no EOMB code assigned then write
     C* a message for the user to assign an EOMB code.
     C     P2TOTP        IFNE      P2TOTC
     C     P2EOMB        ANDEQ     *BLANKS
      *
     C                   MOVE      'BU'          XXMSG#
     C                   EXSR      MSG
      *
     C                   ENDIF                                                  XPEOMB NE BLANK
     *** END ADD *** 09/21/98 *****************************************
     C*
     C                   MOVE      P2EOMB        XREOMB
     C*
     C                   MOVE      Q1USER        XRUSER
     C                   MOVE      Q1PGM         XRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XRDT
     C                   Z-ADD     DSTIME        XRTIME
      *** BEG CHG *** 09/21/98 ****************************************
     C* N61 ************** UPDATPCA08
     C  N61              UPDATE    PCA05
      *** END CHG *** 09/21/98 ****************************************
     C*
     C                   ENDIF                                                  *IN62=OFF
      *** BEG ADD *** 09/21/98 ****************************************
     C*
     C                   ENDIF                                                  *IN61=OFF
      *** END ADD *** 09/21/98 ****************************************
     C*
     C                   READ      PCAWK2                                 60
      *
     C                   END                                                    END DOWEQ 0 *IN60
     C*
     C*
     C     SURG2E        ENDSR
     *********************************************************************
     *** Update                                                U P D A T *
     *********************************************************************
     C     UPDAT         BEGSR
     C*
     C* ===============================================================
     C* Establish default EOMB code, if necessary
     C     XPTOTD        IFGT      *ZERO
     C     XPEOMB        ANDEQ     *BLANK
     C     XPTOTP        ANDGT     *ZEROS
     C*
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C                   END
     C*
     *** BEG ADD *** 09/21/98 *****************************************
     C* Call subroutine to write EOMB to FADEP. Set QQFILE so the EOMB
     C* subroutine will use the CA fields for updating.
      *
     C                   MOVE      CADSEQ        QQDSEQ
     C                   MOVE      'CA'          QQFILE
      *
     C     XXPASS        IFEQ      '2'
     C                   EXSR      EOMB
     C                   ENDIF
      *
     C* If a reduction was made and no EOMB code assigned then write
     C* a message for the user to assign an EOMB code.
     C     XPTOTP        IFNE      CACHRG
     C     XPEOMB        ANDEQ     *BLANKS
      *
     C                   MOVE      'BU'          XXMSG#
     C                   EXSR      MSG
      *
     C                   ENDIF                                                  XPEOMB NE BLANK
     *** END ADD *** 09/21/98 *****************************************
     C*
     C* ===============================================================
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM.
     C** REMOVED MODIFIER FROM DUPLICATE CLAIM EDIT
     C**
     C*
     C     DHPRCD        IFNE      *BLANK
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB#    ANDEQ*ZERO
      *** END DEL *** 09/21/98 ****************************************
     C                   MOVE      DHPRCD        XXCPT4
     C******************** MOVE CAPMOD    XXPMOD
     C     CAKYP2        CHAIN     PCA0P                              60
     C     D2PCL2        TAG
     C*
     C     *IN60         IFEQ      '0'
     C     CADSEQ        IFNE      PXDSEQ
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CADSEQ    OREQ PXDSEQ
     C********** CAEOB#    ANDNEPXEOB#
     C*
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '03'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '03'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C                   ELSE
     C*
     C     CAKYP2        READE     PCA0P                                  60
     C  N60              GOTO      D2PCL2
     C                   END
     C                   END
     C                   END
     C*
     C* ===============================================================
     C** Only process the record if not an override type
     C*
      *** BEG DEL *** 09/21/99 ****************************************
     C********** CAKY1A    CHAINPCA01                54
      *** END DEL *** 09/21/99 ****************************************
     C     CSOVRD        IFNE      'Y'
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CACHK#    ANDEQ*ZERO
     C********** CAEOB#    ANDEQ*ZERO
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPTOTD        CAMRA
     C                   Z-ADD     XPTOTP        CAPAID
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
      *** BEG CHG *** 09/21/99 ****************************************
     C**N54*************** UPDATPCA01
     C                   UPDATE    PCA13
      *** END DEL *** 09/21/99 ****************************************
     C* If Fee Schedule non-reimburseable ... display error message
     C* XX25EM Flag is set at beginning of program where PRFSR eq 'N'
     C     XX25EM        IFEQ      '1'
     C                   MOVE      DHPRCD        XXCPT4
     C     CAKY6         CHAIN     PCA06                              60
     C     *IN60         IFEQ      '0'
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '19'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '19'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 19: *WARNING* Need Letter of Medical Necessity to pay this item
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             ENDIF CAEOB = 0
      *** END DEL *** 09/21/98 ****************************************
     C                   MOVE      '0'           XX25EM
     C                   END                                                    ENDIF *IN60
     C                   END                                                    ENDEQ '1'
     C                   END
     C*
     C* ===============================================================
     C*
     C** Clear the work fields
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      *BLANKS       XPPMOD
     C                   MOVE      *BLANK        XPEOMB
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'XX00000'     XXMSG#
      *** END ADD *** 09/21/98 ****************************************
     C* ===============================================================
     C*
     C     UPDAE         ENDSR
     *********************************************************************
      *** BEG DEL *** 09/21/99 ****************************************
     O************PCA01   E                ECA01
      *** END DEL *** 09/21/99 ****************************************
      *** BEG CHG *** 09/21/98 ****************************************
     O***** PCA08   E                ECA08
     OPCA05     E            ECA05
      *** END CHG *** 09/21/98 ****************************************
     *********************************************************************
**  QCA QCMDEXC TABLE
DLTOVR FILE(FACALTX)
OVRDBF FILE(FACALTX)  TOFILE(QTEMP/FACALTX)
DLTOVR FILE(FACAPTX)
OVRDBF FILE(FACAPTX)  TOFILE(QTEMP/FACAPTX)
**  ANEPM1/ANEPM2 - Anesthesia Px Physical Status Modifiers/Addtl Unit Values
P10
P20
P31
P42
P53
P60
**  NOCT - NOCUT TABLE THESE CODES WILL NOT BE CUT TO 50%
11001
11101
11201
11701
11711
11731
11732
11922
15101
15121
15201
15221
15241
15261
15787
17001
17002
17101
17102
17104
17201
19001
19126
19340
19291
20690
20692
20974
20975
22145
22148
22230
22585
22650
22820
22840
22842
22845
26125
26861
26863
27692
33517
33518
33519
33521
33522
33523
33530
33572
35390
35681
35700
36522
37206
37208
38102
38746
38747
43635
44121
44139
44950
44955
47001
49568
49905
59412
61106
61130
61609
61610
61611
61612
61712
63035
63048
63057
63066
63076
63078
63082
63086
63088
63091
63308
64443
64623
64727
64778
64783
64787
64830
64832
64837
64859
64872
64874
64876
64901
64902
67335
**  SUR1/SUR2: A MUST HAVE B AND A WILL NOT BE CUT
1100111000
1110111100
1120111200
1170111700
1171111710
1173111730
1173211730
1192211920
1192211921
1510115100
1512115120
1522115220
1520115200
1524115240
1526115260
1578715786
1604216041
1700117000
1700217000
1710117100
1710217100
1720117200
1730717304
1900119000
1912619125
1929119290
2214522140
2214822140
2214522141
2214822141
2214522142
2214822142
2223022210
2223022212
2223022214
2223022220
2223022222
2223022224
2258522554
2258522556
2258522558
2265022590
2265022595
2265022600
2265022610
2265022612
2265022625
2265022630
2686126860
2686326862
2735827355
2735827356
2735827357
2769227690
2769227691
6303563030
6303563020
6304863045
6304863046
6304863047
6305763055
6305763056
6306663064
6307663075
6307863077
6308263081
6308663085
6308863087
6309163090
6330863300
6330863301
6330863302
6330863303
6330863304
6330863305
6330863306
6330863307
6444364442
6462364622
6477864776
6483264831
6483764834
6483764835
6483764836
6485964856
6485964857
6490164890
6490164891
6490164892
6490164893
**  RADLC LIMITED MAR
7033600672
7054000672
7054100730
7055100672
7055200789
7055300907
7155000672
7155500730
7214100672
7214200789
7214600672
7214700789
7214800672
7214900789
7215600907
7215700907
7215800907
7215900730
7219600672
7219800730
7322000672
7322100672
7322500730
7372000672
7372100672
7372500730
7418100672
7418500730
7555200672
7555300789
7555600672
7640000672
**  RADLP/RADLT LIMIT
0011800554
0011800554
0013400596
0011800554
0015100638
0018500722
0011800554
0013400596
0011800554
0015100638
0011800554
0015100638
0011800554
0015100638
0018500722
0018500722
0018500722
0013400596
0011800554
0013400596
0011800554
0011800554
0013400596
0011800554
0011800554
0013400596
0011800554
0013400596
0011800554
0015100638
0011800554
0011800554
**  RADEC EXTENDED MAR
7033600924
7054000924
7054100983
7055100924
7055201041
7055301175
7155000924
7155500983
7214100924
7214201041
7214600924
7214701041
7214800924
7214901041
7215601175
7215701175
7215801175
7215900983
7219600924
7219800983
7322000924
7322100924
7322500983
7372000924
7372100924
7372500983
7418100924
7418500983
7555200924
7555301041
7555600924
7640000924
**  RADEP/RADET LIMIT
0016800756
0016800756
0018500798
0016800756
0020100840
0025200923
0016800756
0018500798
0016800756
0020100840
0016800756
0020100840
0016800756
0020100840
0025200923
0025200923
0025200923
0018500798
0016800756
0018500798
0016800756
0016800756
0018500798
0016800756
0016800756
0018500798
0016800756
0018500798
0016800756
0020100840
0016800756
0016800756
**  SEPP - SEPARTE PROCEDURES
11100
11101
11755
20500
20650
20660
20670
21050
21060
21070
21100
21280
21440
21445
21610
21750
24006
24340
25230
25250
26170
26180
26500
26502
26504
27000
27005
27006
27090
27140
27306
27307
27605
27685
28060
28062
28110
28230
28232
28250
28260
28270
28272
28310
28315
29800
29815
29830
29840
29870
29875
29884
30801
30802
31231
31237
31505
31510
31511
31512
31513
31595
31600
31601
31720
31725
32002
32020
32220
32225
32310
32601
32602
32603
32604
32605
32606
33210
33211
33800
36410
36620
36625
36800
36810
36815
36821
36822
36825
36830
36832
36834
36835
36860
36861
37780
38100
38101
38500
38530
38562
38564
38760
38765
38770
38780
42870
43200
43234
43235
43260
43830
43848
44005
44130
44300
44310
44312
44314
44316
44320
44322
44340
44345
44346
44360
44376
44380
44385
44388
44500
44680
44820
44850
45300
45330
45378
45560
45900
45905
45910
45915
46040
46080
46211
46220
46600
46940
46942
47460
47480
47552
49000
49010
49250
49255
49400
49570
50020
50100
50340
50600
50650
50900
51045
51520
51525
51570
51880
52000
52500
53000
53010
53020
53025
53080
53230
53235
54000
54001
54100
54500
54505
54620
54660
55200
55250
55450
55500
55520
55530
56300
56350
56810
57020
57100
57180
57268
57270
57415
57452
57500
57530
57800
58100
58140
58145
58400
58605
58700
58720
58800
58805
58900
58974
59050
59051
59160
59200
59414
59430
60520
60521
60522
60540
61440
61535
64508
65125
65125
65805
65810
65815
65860
65865
65870
65875
65880
66020
66030
66500
66505
66625
66630
66635
66682
66825
67025
67028
67250
67255
67343
67715
68360
68770
69210
69310
69670
69700
