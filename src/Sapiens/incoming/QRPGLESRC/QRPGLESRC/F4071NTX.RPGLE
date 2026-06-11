     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4071RTX
     H*    DESCRIPTION: ADJUDICATION (TEXAS RULES - 08/01/91)
     H*                    (INCLUDES: TEXAS RULES - 12/11/91)
     H*   This program is called from F0070R (Claim Adjudication) and will
     H* perform the actual adjudication of the detail claim records writ-
     H* ten by F0070R.  This program was derived from the Texas State
     H* Fee Schedule Reimbursment Manual issued on 08/01/91.
     H* This Sub-program is intended to be used to calculate the amount
     H* that is due a Provder of services, by the Carrier of the Work comp
     H* insurance.  It should be used for any serice provided on or after
     H* 08/01/91.
     H*****************************************************************
     H*  CREATED: 7/10/95   C:32   PROJECT: TX001
     H*****************************************************************
     H* Information needed for each year's adjudication program rules:
     H*    1.  Update the effective date range for reading the detail charges
     H*        (found in the mainline section)
     H*    2.  Update Conversion factors:
     H*        (found in *LIKE section)
     H*        MEDICINE                XXMEDI
     H*        SURGERY                 XXSURG
     H*        ANESTHESIA              XXANES
     H*        CERT ANESTHESIOLOGISTS  XXCANE    FOR 12/11/91 UPDATE
     H*        RADIOLOGY               XXRADI
     H*        PATHOLOGY               XXPATH
     H*****************************************************************
     H*  SPECIALTY CODES USED IN THIS PROGRAM:
     H*  RNA - CERTIFIED REGISTERED NURSE ANESTHETISTS
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  08/09/02 C:062 PROJECT: FS244
     H*                 USE CAZIPA.
     H*
     H*  09/25/01 C:062 PROJECT: FSMNT
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.
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
     H*
     H*  09/21/98 C:79  PROJ: MC152
     H*                 MAKE DATES YEAR 2000 COMPATIBLE
     H*  08/25/95 C:32  PROJ: TX001
     H*                 CHGD RANGE FOR CAT SCAN (ADDED 73702)
     H*  08/31/95 C:32  PROJ: TX001
     H*                 CHG EOMB# FROM NUMBERS TO LETTERS
     H*                 AND ERROR MESSAGES FOR THE EOMBS
     H*  09/05/95 C:32  PROJ: TX001
     H*                 FIX KEY IN PHYSICAL THERAPY (4 MODALITY CHECK)
     H*                 ADDED SERVICE DATE TO (IF STMT)
     H*  09/14/95 C:32  PROJ: TX001
     H*                 CHG CAT SCAN/MRI PROCESSING
     H*  11/30/95 C:32  PROJ: TX001                                   *
     H*                 1. CHG ANES CONVERSION RATE IF MOD 46 IS USED *
     H*                 2. CHG CODE WHICH DISALLOWED 90060-90080      *
     H*                    IF WITHIN 30 DAYS, GIVE ERR MSG & CALC AMT *
     H*                 3. CHG 8/4 MODALITY LIMIT EDIT:               *
     H*                    A) USE FEIN/MOD IN KEY                     *
     H*                    B) ADD BODY PART ACCUMULATOR               *
     H*                 4. PT LIMIT WITH OFFICE VISIT GIVE ERROR MSG  *
     H*                    AND CALC PAID AMOUNT                       *
     H*  03/15/96 C:32  PROJ: TX005                                   *
     H*              1. ADDED ENDING DATE FOR RECORD PROCESSING RANGE *
     H*              2. CHGD MOD 'RT' TO 'RE' (RENTAL)                *
     H*              3. CHGD ERROR W/OVERPAYMENT OF OFFICE VISITS     *
     H*                 THIS OCCURRED IF DURING PRE-OPER (MSG 'AJ')   *
     H*  08/28/96 C:32 PROJ:TXMNT
     H*                ADD EDIT TO SKIP PHARM FDCAP RECORDS WHEN
     H*                READING ALL RECS FOR A SPECIFIC DATE
     H*****************************************************************
     H*  INDICATOR USAGE:
     H*  54    Use with Primary FDCAP file (i.e. DOWEQ, etc on reading charges)
     H*  60    General Use
     H*  61    General Use
     H*  62    General Use
     H*****************************************************************
     H*  SECTION                                  SUBR         CODES
     H*  Anesthesia                               ANES         00100-01999
     H** Durable Medical Equipment                DME          D0101-D0957
     H*                                                        99070
     H** Medical                                  MEDI         90000-99199
     H*  Office Visits                            OFFI         99201-99499
     H** Pathology                                PATH         80002-89399
     H** Radiology                                RADI         70010-79999
     H*  Surgery                                  SURG         10000-69979
     H*                                           SURG2        10000-69979
     H*****************************************************************
     F*  FDCAL7 is the Primary File Read (PCA01)
      *** BEG DEL *** 09/21/98 ****************************************
     F************FDCAL7  UF  E           K        DISK
      *** END DEL *** 09/21/98 ****************************************
     F*
     FFACAPTX   O    E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK1)
     FFACALTX   IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK2)
     FFDCAL6    IF   E           K DISK
     F                                     RENAME(PCA01:PCA06)
     F*** BEG DEL *** 09/21/98 ****************************************
     F*********** FDCAL8  UF  E           K        DISK
     F*********** PCA01                             KRENAMEPCA08
     F*** END DEL *** 09/21/98 ****************************************
     FFDCAL9    IF   E           K DISK
     F                                     RENAME(PCA01:PCA09)
     FFDCALA    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0A)
     FFDCALP    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0P)
     FFDCALC    IF   E           K DISK
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
     F*** END ADD *** 09/21/98 ****************************************

     D*
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
     D*
     D* Compile-time Arrays
     D QCA             S             55    DIM(4) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D ANEPM1          S              2    DIM(6) CTDATA PERRCD(1)              ANES Px Modifiers
     D ANEPM2          S              1  0 DIM(6) ALT(ANEPM1)
     D SUR1            S              5    DIM(264) CTDATA PERRCD(1)            CUT/MAJOR-MIN
     D SUR2            S              5    DIM(264) ALT(SUR1)
     D PART            S              2    DIM(13) CTDATA PERRCD(1)             PHYS PART VS. MRA
     D PMRA            S              9  2 DIM(13) ALT(PART)
     D BODY            S              6  0 DIM(13)                              BODY PART CNTR
      *** BEG CHG *** 09/21/98 ****************************************
     D******************* FDAT       12  6 0             FUTURE DATE ARRAY
     D FDAT            S              7  0 DIM(12)                              FUTURE DATE ARRAY
      *** END CHG *** 09/21/98 ****************************************
     D FAMT            S              7  2 DIM(12)                              FUTURE AMT  ARRAY
     D*** BEGIN ADD 11/30/95 ******************************************
     D PRT8            S              2    DIM(20)                              PART TBL 8 LIMIT
     D CNT8            S              7  0 DIM(20)                              PART CNT 8 LIMIT
     D PRT4            S              2    DIM(20)                              PART TBL 4 LIMIT
     D CNT4            S              7  0 DIM(20)                              PART CNT 4 LIMIT
     D*
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

     I*** END   ADD 11/30/95 ******************************************
     I*
     I*  Rename the duplicate field names.
     IPCA06
     I              CACPT4                      X6CPT4
     I              CACO#                       X6CO#
     I              CASUB#                      X6SUB#
     I              CAFND                       X6FND
     I              CAFYR                       X6FYR
     I              CACASE                      X6CASE
     I              CAFDAT                      X6FDAT
     I              CADSEQ                      X6DSEQ
     I              CAFEIN                      X6FEIN
     I              CAMOD                       X6MOD
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
      *** END CHG *** 09/21/98 ****************************************
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
     I              CACPT4                      XRPRCD
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
     I              CAFLAG                      XRFLAG
     I              CAIPOP                      XRIPOP
     I*
     IPCA0P
     I*
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
     I              CATOTA                      PXTOTA
     I              CATOTD                      PXTOTD
     I              CATOTP                      PXTOTP
     I              CATOTC                      PXTOTC
     I              CAPMOD                      PXPMOD
     I              CAUSER                      PXUSER
     I              CAPGM                       PXPGM
     I              CADT                        PXDT
     I              CATIME                      PXTIME
     I              CACHK#                      PXCHK#
     I              CAEOB#                      PXEOB#
     I              CADIFF                      PXDIFF
     I              CAPART                      PXPART
     I              CADXR                       PXDXR
     I              CAFLAG                      PXFLAG
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
     I              CAUNTS                      XCUNTS
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

     C*
      *** BEG DEL *** 09/21/98 ****************************************
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
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C     CAKY5         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    PPDSEQ
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
      *** END ADD *** 09/21/98 ****************************************
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
     C     CAKEY8        KLIST
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
     C*
     C     CAKYP         KLIST
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
     C*** BEGIN ADD *** 11/30/95 **************************************
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
     C*** END   ADD *** 11/30/95 **************************************
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
     C*
     C*  User Parameter File
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
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
     C*** END ADD *** 09/21/98 ****************************************
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
     C     QQRTCD        CABEQ     'P'           TERM                           PRE-OPEN FILES
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** MOVEAQQFDT     QQF,1
     C******************** MOVEAQQFDT2    QQF,26
      *** END DEL *** 09/21/98 ****************************************
     C*
     C* = Initialization Section ======================================
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDATE            7 0
     C*
     C     *LIKE         DEFINE    CACHK#        HLCHK#
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
     C     *LIKE         DEFINE    CACPT4        PPCPT4
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
     C     *LIKE         DEFINE    CACPT4        XPCPT4
     C     *LIKE         DEFINE    CAEOMB        XPEOMB
     C     *LIKE         DEFINE    CAPMOD        XPPMOD
     C     *LIKE         DEFINE    CATOTA        XPTOTA
     C     *LIKE         DEFINE    CACHRG        XPTOTC
     C     *LIKE         DEFINE    CAMRA         XPTOTD
     C     *LIKE         DEFINE    CAPAID        XPTOTP
     C*
     C     *LIKE         DEFINE    CACPT4        XXCPT4
     C     *LIKE         DEFINE    CACPT4        XXCPCD
     C     *LIKE         DEFINE    CAFDAT        XXDATE
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    CAFDAT        XXEFDT
     C*** BEGIN DEL *** 08/09/02 **************************************
     C********** *LIKE     DEFN GZFRZP    XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C     *LIKE         DEFINE    PRMRA         XXMRA
     C     *LIKE         DEFINE    CAPART        XXPART
     C     *LIKE         DEFINE    CAPMOD        XXPMOD
     C     *LIKE         DEFINE    CAANMN        XXUNIT
     C     *LIKE         DEFINE    CAANMN        CNTMIN
     C     *LIKE         DEFINE    CAFDAT        XXYMD
     C     *LIKE         DEFINE    CAFDAT        XXFDAT                         ADD 11/30/95
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
     C*** END ADD *** 09/21/98 ****************************************
     C*
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
     C                   Z-ADD     *ZEROS        XXOVRP            5 4
     C*** END   CHG *** 09/25/01 **************************************
     C                   Z-ADD     0             XXPERC            5 2
     C                   Z-ADD     0             XXWORK           10 7
     C                   Z-ADD     0             TTANMN           10 7
     C                   MOVE      '0'           XX25EM            1
     C                   MOVE      *BLANKS       QQTYPE            1
     C                   Z-ADD     0             XVUNTS            7 4
     C                   Z-ADD     0             XXUNTS            7 4
     C                   Z-ADD     0             X1ANMN            7 4
     C                   Z-ADD     0             XXUNT             7 4
     C                   Z-ADD     0             PRADIO           10 2
     C                   Z-ADD     0             LRADIO           10 2
     C                   Z-ADD     0             CNTADD            5 0
     C*
     C*** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'XX00000'     XXMSG#
     C*** END ADD *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     006.75        XXMEDI            5 2           MEDI Conversion
     C                   Z-ADD     185.00        XXSURG            5 2           SURG Conversion
     C                   Z-ADD     037.00        XXANES            5 2           ANES Conversion
     C                   Z-ADD     032.00        XXCANE            5 2           CERT ANES Conv
     C                   Z-ADD     001.75        XXRADI            5 2           RADI Conversion
     C                   Z-ADD     000.95        XXPATH            5 2           PATH Conversion
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
     C***********XXYMD***  ANDGE910801
     C***********XXYMD***  ANDLT960401
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
     C     CAKY2         SETLL     PCA13
     C     CAKY2         READE     PCA13                                  54
      *
     *** BEG ADD *** 04/20/99 *****************************************
     C                   Z-ADD     CADSEQ        SVDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAUNTS        SVUNTS
     C                   MOVEL     CACPT4        XPCPT4
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
     C     CAFDAT        IFGE      910801
     C     CAFDAT        ANDLT     960401
     C     XXRCN         ANDEQ     CARCN
      *
     C                   Z-ADD     CAFDAT        XXYMD
     C                   MOVE      *BLANKS       XPEOMB
      *** END ADD *** 09/21/98 ****************************************
     *** BEG DEL *** 04/20/99 *****************************************
     C******************** Z-ADDCADSEQ    SVDSEQ           SAVE FIELD VALS
     C******************** MOVE CAFEIN    SVFEIN           FOR RECORD WE
     C******************** MOVE CAMOD     SVMOD            ARE PROCESSING
     C******************** MOVE CAPRTY    SVPRTY
     C******************** Z-ADDCAUNTS    SVUNTS
     C******************** MOVELCACPT4    XPCPT4
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
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '97'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '97'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 97: "Invalid Claim Status Code, See Programmer"
     C                   END
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
     C                   MOVEL     CACPT4        XXCPCD
     C                   MOVE      *BLANKS       XPEOMB
     C                   MOVE      *ZEROS        XPTOTA
     C                   MOVE      *ZEROS        XPTOTP
     C                   MOVE      *ZEROS        XPTOTD
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
     C     CAPMOD        IFNE      *BLANK
     C*
     C                   EXSR      PMOD
     C     CACHRG        COMP      XXMRA                              60
     C   60              Z-ADD     XXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   ELSE                                                   ELSE CAPMOD NE *BLAN
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '91'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '91'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C* Err Msg 91: "Carrier to set payment on individual case basis
     C*             based on documentation"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   END                                                    END CAPMOD NE *BLANK
     C*
     C                   ELSE                                                   ELSE RCRPT EQ 'Y'
     C*
     C     CACHRG        COMP      RCMRA                              61
     C*** BEGIN CHG *** 08/31/95 *************************************
     C***61                MOVE '206'     XPEOMB
     C* EOMB Code 206: "This charge has been paid per R&C fees"
     C   61              MOVE      'M  '         XPEOMB
     C* EOMB CODE M:   "REDUCED TO FAIR AND REASONABLE"
     C*** END   CHG *** 08/31/95 *************************************
     C   61              Z-ADD     RCMRA         XPTOTA
     C  N61              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C                   END                                                    END RCRPT EQ 'Y'
     C*
     C                   ELSE                                                    ELSE RCSTAT NE "I"
     C                   GOTO      NXRC1
     C                   END                                                     END RCSTAT NE "I"
     C                   END                                                     END 0 *IN60
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAKY1A    CHAINPCA01                54
     C********** *IN54     IFEQ '0'
      *** END DEL *** 09/21/98 ****************************************
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   END                                                     END CARCFL = Y
     C*
     C*== =============================================================
     C*
     C*   Check for level break (Provider)
     C     SXFEIN        IFNE      CAFEIN
     C     SXMOD         ORNE      CAMOD
     C                   EXSR      NEWPRO
     C                   END
     C*
     C*== Hospital ====================================================
     C*
     C*   Hospital Subroutine or Regular Providers.
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
     C                   MOVEL     CACPT4        XXCPT4
     C                   MOVE      CAFDAT        XXEFDT
     C*
     C     PRKY          SETGT     PPR01                                         GET PROC CODE
     C     NXTPR2        TAG
     C                   READP     PPR01                                  60     RECORD
     C*
     C*  A hard error will cause the program to terminate ASAP.
     C     *IN60         IFEQ      '1'
     C     PRCPT4        ORNE      CACPT4
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '98'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
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
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   ELSE
     C*
     C*  If fee schedule reimbursement equals 'N' But the MRA
     C*    is greater than zero, this CPT4 Code has been assigned a
     C*    specific MRA by the insurance company.
     C     CACHRG        COMP      PRMRA                              60
     C*** BEGIN CHG *** 08/31/95 *************************************
     C***60                MOVE '202'     XPEOMB
     C* EOMB Code 202: "Charges exceed maximum allowance found in
     C*                 Reimbursement guidelines"
     C*
     C   60              MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C   60              Z-ADD     PRMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
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
     C*   If modifier exists check the code through the Modifier Subr
     C     CAPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C*
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '91'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '91'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C* Err Msg 91: "Carrier to set payment on individual case basis
     C*             based on documentation"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
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
     C     CAPART        IFEQ      'IA'
     C     PRANAM        ANDGT     *ZERO
     C                   GOTO      PASRPT
     C                   END
     C*
     C*   If modifier exists check the code through the Modifier Subr
     C     CAPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C*
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '24'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '24'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C* Err Msg 24: "The item is ANE - requires further inspection
     C*             by an adjuster"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
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
     C     CAPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C*
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '82'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '82'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C* Err Msg 82: "This item is I.C. - requires further inspection
     C*             by an adjuster"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '82'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '82'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C* Err Msg 82: "This item is I.C. - requires further inspection
     C*             by an adjuster"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
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
     C     CACPT4        IFGE      '00100'                                      ANESTHESIA
     C     CACPT4        ANDLE     '01999'
     C                   EXSR      ANES
     C*
     C                   ELSE                                                      ELSE ANES
     C*
     C     CACPT4        IFGE      '10000'                                      SURGERY
     C     CACPT4        ANDLE     '69979'
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
     C     CACPT4        IFGE      '70010'                                      RADIOLOGY
     C     CACPT4        ANDLE     '79999'
     C                   EXSR      RADI
     C*
     C                   ELSE                                                      ELSE RADI
     C*
     C     CACPT4        IFGE      '80002'                                      PATHOLOGY
     C     CACPT4        ANDLE     '89399'
     C                   EXSR      PATH
     C*
     C                   ELSE                                                      ELSE PATH
     C*
     C     CACPT4        IFGE      '90000'                                      MEDICAL
     C     CACPT4        ANDLE     '99199'
     C                   EXSR      MEDI
     C*
     C                   ELSE                                                      ELSE MEDI
     C*
     C     CACPT4        IFEQ      '99070'                                      DURABLE MEDICAL EQUI
     C     CACPT4        ORGE      'D0101'
     C     CACPT4        ANDLE     'D0957'
     C                   EXSR      DME
     C*
     C                   ELSE                                                      ELSE DME
     C*
     C                   EXSR      OTHE                                         OTHER CODES
     C*
     C                   END                                                       END DME
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
     C     CACPT4        IFNE      *BLANK
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAEOB#    ANDEQ*ZERO
      *** END DEL *** 09/21/98 ****************************************
     C                   MOVE      CACPT4        XXCPT4
     C                   MOVEL     CAPMOD        XXPMOD
     C     CAKYP         CHAIN     PCA0P                              60
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
     C     CAKYP         READE     PCA0P                                  60
     C  N60              GOTO      DUPCL1
     C                   END
     C                   END
     C                   END
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAKY1A    CHAINPCA01                60
     C********** *IN60     IFEQ '0'
      *** END DEL *** 09/21/98 ****************************************
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
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CATOTA    ANDEQ0
      *** END DEL *** 09/21/98 ****************************************
     C     CAPAID        ANDEQ     0
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
      *** BEG CHG *** 09/21/98 ****************************************
     C*********************UPDATPCA01
     C                   UPDATE    PCA13
      *** END CHG *** 09/21/98 ****************************************
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
     C*
     C     *IN54         IFEQ      '0'                                          SAVE FIELDS OF
     C                   Z-ADD     CADSEQ        SVDSEQ                         CURRENT REC.
     C                   MOVE      CAFEIN        SVFEIN
     C                   MOVE      CAMOD         SVMOD
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAUNTS        SVUNTS
     C                   MOVEL     CACPT4        XPCPT4
     C                   MOVEL     CAPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C                   ENDIF
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** ENDIF                           CAFDAT GE
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
     C******************** ENDDO                           *IN54=OFF
      *** END DEL *** 09/21/98 ****************************************
     C*
     C*  End the program
     C     TERM          TAG
     C*
     C*  Release all the formats for a save return.
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** DUMMY     CHAINPCA01                54
     C**N54*****           EXCPTECA01
      *** END DEL *** 09/21/98 ****************************************
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
     C     CAPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per ANESTHESIA GR P. 6 - 7
     C     CAPMOD        IFNE      '22'
     C     CAPMOD        ANDNE     '23'
     C     CAPMOD        ANDNE     '41'
     C     CAPMOD        ANDNE     '42'
     C     CAPMOD        ANDNE     '43'
     C     CAPMOD        ANDNE     '44'
     C     CAPMOD        ANDNE     '46'
     C     CAPMOD        ANDNE     '75'
     C     CAPMOD        ANDNE     'P1'
     C     CAPMOD        ANDNE     'P2'
     C     CAPMOD        ANDNE     'P3'
     C     CAPMOD        ANDNE     'P4'
     C     CAPMOD        ANDNE     'P5'
     C     CAPMOD        ANDNE     'P6'
     C*
     C                   Z-ADD     1             Q
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '10'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '10'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '201'     XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C*
     C                   ELSE                                                   ELSE EQ 22...
     C*----------------------------------------------------------------
     C* Per ANESTHESIA GR p. 2 - Physical status modifiers
     C* If modifier - Px is used, add additional Time Units to calculation
     C     CAPMOD        IFEQ      'P1'
     C     CAPMOD        OREQ      'P2'
     C     CAPMOD        OREQ      'P3'
     C     CAPMOD        OREQ      'P4'
     C     CAPMOD        OREQ      'P5'
     C     CAPMOD        OREQ      'P6'
     C*
     C                   Z-ADD     1             X
     C     CAPMOD        LOOKUP    ANEPM1(X)                              60
     C     *IN60         IFEQ      '1'
     C     ANEPM2(X)     ADD       SXANAM        SXANAM
     C                   END                                                    END 1 *IN60
     C*
     C                   ELSE                                                   ELSE EQ P1...
     C                   EXSR      PMOD
     C*
     C                   END                                                    END EQ 'P1'
     C*
     C                   END                                                    END EQ '22'
     C*
     C                   END                                                    END NE *BLANK
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
     C     CAUNTS        DIV       15            HLANMN
      *** END CHG *** 09/21/98 ****************************************
     C** Round up all fractions of 15 minutes to an additional Time Unit
     C                   MVR                     HLADDT
     C                   ADD       HLANMN        HLADDT
     C                   Z-ADD     HLANMN        HLANTV
     C     HLADDT        IFGT      *ZEROS
     C                   ADD       1             HLANTV
     C                   END
     C*
     C*
     C** Basic Value + Time Units = Total Anesthesia Value
     C     SXANAM        ADD       HLANTV        XXWORK
     C*
     C*  Per 12/11/91 update, CRNA (Cert Registered Nurse Anesthetists)
     C*  Anesthesia has 2 conversions factors
     C*
     C** Total Anesthesia Value * Conversion Factor = Fee
     C*** BEGIN CHG *** 11/30/95 **************************************
     C********** QQSPCD    IFEQ 'RNA'
     C********** XXYMD     ANDGE911211
     C********** XXWORK    MULT XXCANE    SXMRA     H
     C**********           ELSE
     C     XXWORK        MULT(H)   XXANES        SXMRA
     C**********           END
     C*
     C***  IF MOD 46, IF PRIOR TO 911211 RATE = 37.00 * .80
     C***  IF MOD 46, AFTER 911210 USE RATE = 32.00
     C     CAPMOD        IFEQ      '46'
     C     XXYMD         IFGE      910801
     C     XXYMD         ANDLE     911210
     C     XXWORK        MULT(H)   XXANES        SXMRA
     C     SXMRA         MULT      .80           SXMRA
     C                   ELSE
     C     XXWORK        MULT      XXCANE        SXMRA
     C                   END
     C                   END
     C*** END CHG *** 11/30/95 ****************************************
     C*
     C* ===============================================================
     C* Modifier -42
     C* REIMBURSEMENT FOR CONCURRENT SUPERVISION - ANESTHESIA
     C*
     C     CAPMOD        IFEQ      '42'
     C*
     C* ALLOW 90% OF THE ALLOWABLE FEE
     C     SXMRA         MULT      .90           SXMRA
     C*
     C                   END                                                    END CAPMOD
     C* ===============================================================
     C* Modifier -43
     C* REIMBURSEMENT FOR CONCURRENT SUPERVISION - ANESTHESIA
     C*
     C     CAPMOD        IFEQ      '43'
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
     C     CAPMOD        IFEQ      '44'
     C*
     C* ALLOW 80% OF THE ALLOWABLE FEE
     C     SXMRA         MULT      .80           SXMRA
     C*
     C                   END                                                    END CAPMOD
     C*
     C* ===============================================================
     C* Modifier -46
     C* REIMBURSEMENT FOR CONCURRENT SUPERVISION - ANESTHESIA
     C*
     C*** BEGIN DEL *** 11/30/95 **************************************
     C********** CAPMOD    IFEQ '46'
     C**********
     C* ALLOW 80% OF THE ALLOWABLE FEE
     C********** SXMRA     MULT .80       SXMRA
     C**********
     C**********           END                             END CAPMOD
     C*** END   DEL *** 11/30/95 **************************************
     C*
     C*===============================================================
     C* Calculate Payment
     C*
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C* Update charge with payment
     C                   EXSR      UPDAT
     C*
     C     ANESE         ENDSR
     *********************************************************************
     C*  Durable Medical Equip      Codes: D0300 - D0957         D M E   *
     *********************************************************************
     C     DME           BEGSR
     C*
     C* ===============================================================
     C* Rental Equipment will be identified by PMOD = RE
     C     CAPMOD        IFNE      *BLANK
     C*
     C*** BEGIN CHG *** 03/15/96 **************************************
     C********** CAPMOD    IFNE 'RT'
     C     CAPMOD        IFNE      'RE'
     C*** END   CHG *** 03/15/96 **************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '10'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '10'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*** Err Msg 10: "Invalid Modifier used with this Item., Re-enter Item
     C***              with valid modifier"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '201'     XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C                   EXSR      UPDAT
     C                   GOTO      DMEE
     C*
     C                   END                                                    END CAPMOD EQ RT
     C*
     C                   END                                                    END PMOD NE BLKS
     C*
     C* ===============================================================
     C* IF CODE IS A PURCHASE CODE AND IS SUBMITTED WITH RENTAL
     C* MODIFIER SEND MESSAGE
     C*
     C     SX1A2         IFNE      'R'
     C*
     C*** BEGIN CHG *** 03/15/96 **************************************
     C********** CAPMOD    IFEQ 'RT'
     C     CAPMOD        IFEQ      'RE'
     C*** END   CHG *** 03/15/96 **************************************
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '84'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '84'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C* Err Msg 84: "The item is considered "PURCHASE" only.
     C*              Please verify usage."
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '207'     XPEOMB
     C*** EOMB 207: "Incorrect submission of charge.  Purchase only item
     C*              submitted as rental item."
     C                   MOVE      'T  '         XPEOMB
     C*** EOMB T:   "NOT ACCORDING TO TREATMENT GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C                   EXSR      UPDAT
     C                   GOTO      DMEE
     C*
     C                   END                                                    END CAPMOD EQ RT
     C*
     C                   END                                                    END SX1A2 NE 'R'
     C*
     C* ===============================================================
     C* PER DME PG.2 - CONTINUOUS PASSIVE MOTION
     C*
     C     CACPT4        IFEQ      'D0540'
     C     CACPT4        OREQ      'D0541'
     C*
     C                   MOVE      CAFDAT        XXYM2
     C                   Z-ADD     *ZEROS        COUNT
     C                   Z-ADD     *ZEROS        COUNT2
     C                   Z-ADD     *ZEROS        COUNT3
     C                   Z-ADD     *ZEROS        XXAMT
     C                   CLEAR                   FDAT
     C                   CLEAR                   FAMT
     C                   MOVE      'Y'           XXFRST            1
     C                   MOVE      'D0540'       XXCPT4
     C     CAKYC         SETLL     PCA0C
     C     CAKYC2        READE     PCA0C                                  60
     C     *IN60         DOWEQ     '0'
     C***
     C***   COUNT HOW MANY DAYS OF RENTAL PAID
     C     CACPT4        IFEQ      'D0540'
     C     XCCPT4        IFEQ      'D0540'
     C*** BEGIN CHG *** 03/15/96 **************************************
     C********** XCPMOD    ANDEQ'RT'
     C     XCPMOD        ANDEQ     'RE'
     C*** END   CHG *** 03/15/96 **************************************
     C     XCDSEQ        IFNE      CADSEQ
     C     XCTOTP        IFGT      0
     C***
     C     XCUNTS        IFGT      *ZEROS
     C                   ADD       XCUNTS        COUNT
     C*
     C                   ELSE
     C*
      *** BEG CHG *** 09/21/98 ****************************************
     C******************** ADD  XCANMN    COUNT
     C                   ADD       XCUNTS        COUNT
      *** END CHG *** 09/21/98 ****************************************
     C                   END                                                    END CAUNTS GT 0
     C*
     C                   END                                                    END XCTOTP GT 0
     C                   END
     C                   END
     C                   END
     C***
     C***   ACCUM TOTAL PAID IN 4 WEEK TIME SPANS
     C     CACPT4        IFEQ      'D0540'
     C     XCCPT4        IFEQ      'D0540'
     C*** BEGIN CHG *** 03/15/96 **************************************
     C********** XCPMOD    ANDEQ'RT'
     C     XCPMOD        ANDEQ     'RE'
     C*** END   CHG *** 03/15/96 **************************************
     C*** GET (6) 30 DAY TIME SPAN DATES (FROM ACCIDENT DATE)
     C     XXFRST        IFEQ      'Y'
     C                   MOVE      'N'           XXFRST
     C                   Z-ADD     CAACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C     1             DO        12            X
     C                   Z-ADD     30            SRFUTR            5 0
     C                   MOVE      ' '           SRRTCD            1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FDAT(X)
     C                   ENDDO
     C                   END
     C*
     C***   ACCUM TOTAL PAID INTO THE CORRECT DATE RANGE
     C     XCDSEQ        IFNE      CADSEQ
     C     XCTOTP        IFGT      0
     C     1             DO        12            X
     C     XCFDAT        IFLE      FDAT(X)
     C                   ADD       XCTOTP        FAMT(X)
     C                   LEAVE
     C                   ENDIF                                                  END XCFDAT LE
     C                   ENDDO                                                  ENDDO
     C*
     C                   END                                                    END XCTOTP GT 0
     C                   END                                                    END XCDSEQ NE
     C                   END                                                    END XCCPT4 =D0540
     C                   END                                                    END CACPT4 =D0540
     C***
     C*** COUNT OF SOFT GOODS PER CLAIM
     C     CACPT4        IFEQ      'D0541'
     C     XCCPT4        IFEQ      'D0541'
     C     XCDSEQ        IFNE      CADSEQ
     C     XCTOTP        IFGT      0
     C                   ADD       1             COUNT3
     C                   END                                                    END XCTOTP GT 0
     C                   END                                                    END XCDSEQ NE
     C                   END                                                    END XCCPT4 =D0541
     C                   END                                                    END CACPT4 =D0541
     C***
     C     CAKYC2        READE     PCA0C                                  60
     C                   END                                                    END DOW *IN60
     C*
     C* PER DME PG.2 - CONTINUOUS PASSIVE MOTION, USE OF THIS UNIT IN
     C* EXCESS OF 30 DAYS REQ DOCUMENTATION OF MED NECESSITY BY THE DOCTOR
     C*
     C     CAUNTS        IFGT      *ZEROS
     C                   ADD       CAUNTS        COUNT
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** ELSE
     C******************** ADD  CAANMN    COUNT
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END CAUNTS GT 0
     C*
     C*
     C* PER PG DME P.2 - CONTINUOUS PASSIVE MOTION,ONLY 1 SET OF SOFT GOODS
     C*
     C     COUNT3        IFGE      1
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '209'     XPEOMB
     C*** EOMB 209: "Exceeds reimbursement limitations"
     C*
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C*
     C                   EXSR      UPDAT
     C                   GOTO      DMEE
     C                   END                                                    END COUNT GT 4
     C*
     C                   END                                                    END CACPT4 EQ 'D0540
     C*
     C*
     C* ===============================================================
     C* IF CAPMOD IS RT USE THE AMOUNT FOR THE RENTAL ELSE USE THE
     C* PURCHASE AMOUNT
     C*
     C*** BEGIN CHG *** 03/15/96 **************************************
     C********** CAPMOD    IFEQ 'RT'
     C     CAPMOD        IFEQ      'RE'
     C*** END   CHG *** 03/15/96 **************************************
     C*
     C*** BEG DEL *** 09/21/98 ****************************************
     C********** CAUNTS    IFGT *ZEROS
     C********** CAUNTS    MULT SXMRA     SXMRA
     C*
     C**********           ELSE
     C********** CAANMN    MULT SXANAM    SXMRA
     C*** END DEL *** 09/21/98 ****************************************
     C*** BEG ADD *** 09/21/98 ****************************************
     C     PR1A2         IFEQ      'R'
     C*
     C     CAUNTS        MULT      SXANAM        SXMRA
     C*
     C                   ELSE
     C     CAUNTS        MULT      SXMRA         SXMRA
     C*** END ADD *** 09/21/98 ****************************************
     C*
     C                   END                                                    END CAUNTS GT 0
     C*
     C                   END                                                    END CAPMOD EQ RT
     C*
     C*
     C* PER DME PG .2 - CONTINUOUS PASSIVE MOTION, (FOR RENTAL USAGE)
     C* A MAX OF $995.00 IS ALLOWABLE PER MONTH, AFTER THE 1ST MONTH
     C*
     C                   Z-ADD     *ZEROS        XXAMT
     C                   Z-ADD     *ZEROS        XXDIFF
     C     CACPT4        IFEQ      'D0540'
     C*** BEGIN CHG *** 03/15/96 **************************************
     C********** CAPMOD    ANDEQ'RT'
     C     CAPMOD        ANDEQ     'RE'
     C*** END   CHG *** 03/15/96 **************************************
     C     CAFDAT        ANDGT     FDAT(1)
     C     2             DO        12            X
     C     CAFDAT        IFLE      FDAT(X)
     C     FDAT(X)       OREQ      *ZEROS
     C     SXMRA         ADD       FAMT(X)       XXAMT
     C     XXAMT         IFLE      995
     C                   LEAVE
     C                   ELSE
     C     XXAMT         SUB       995           XXDIFF
     C     SXMRA         SUB       XXDIFF        SXMRA
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '210'     XPEOMB
     C*** EOMB 210: "Max of $995 is allowed per month after 1st
     C***            4 weeks"
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C*
     C                   LEAVE
     C                   END
     C                   END
     C                   ENDDO
     C                   END                                                     END CACPT4
     C*
     C     CACPT4        IFEQ      'D0540'
     C     COUNT         IFGT      30
     C*
     C                   Z-ADD     1             Q
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'BS'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'BS'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg BS: "Documentation is required for reimbursement
     C*              after 30 days of usage".
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '209'     XPEOMB
     C*** EOMB 209: "Exceeds reimbursement limitations"
     C*
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C                   END                                                    END COUNT GT 30
     C                   END                                                    END CACPT4   30
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
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
     C* as long as XPEOMB is not blank
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
     C                   ENDIF                                                  XPEOMB NE BLANK
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     EOMBE         ENDSR
     *** END ADD *** 09/21/98 *****************************************
     *********************************************************************
     *** LOAD FOLLOW UP ARRAYS SUBROUTINE                      L O D F U *
     *********************************************************************
     C     LODFU         BEGSR
     C*
     C*** LOAD ARRRAYS WITH FOLLOW UP DAYS
     C*
     C                   MOVE      *ZERO         FUF
     C                   MOVE      *ZERO         FUT
     C                   Z-ADD     1             U                 5 0
     C*
     C     CAKYA1        CHAIN     PCA0A                              60
     C     *IN60         DOWEQ     '0'
     C     U             ANDLE     500
     C*
     C                   MOVE      XACPT4        XXCPT4
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
     C                   END
     C     CAKYA1        READE     PCA0A                                  60
     C                   END
     C*
     C     LODFUE        ENDSR
     *********************************************************************
     H*  Medical                    Codes: 90000 - 99199         M E D I *
     *********************************************************************
     C     MEDI          BEGSR
     C*
     C* ===============================================================
     C* VALID MODIFIERS ARE: 22, 26, 29, 51, 52, 55, 56, 75, 76, 77, 90, 99
     C*
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     CAPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per PG. MEDICINE GR P.13-16
     C     CAPMOD        IFNE      '22'
     C     CAPMOD        ANDNE     '26'
     C     CAPMOD        ANDNE     '29'
     C     CAPMOD        ANDNE     '51'
     C     CAPMOD        ANDNE     '52'
     C     CAPMOD        ANDNE     '55'
     C     CAPMOD        ANDNE     '56'
     C     CAPMOD        ANDNE     '75'
     C     CAPMOD        ANDNE     '76'
     C     CAPMOD        ANDNE     '77'
     C     CAPMOD        ANDNE     '90'
     C     CAPMOD        ANDNE     '99'
     C*
     C                   Z-ADD     1             Q
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '10'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '10'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '201'     XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   ELSE                                                   ELSE EQ 22...
     C*
     C     CAPMOD        IFEQ      '55'
     C                   Z-ADD     1             Q
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '64'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '64'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 64: "The item indicates Postoper Mgmt Only mod-55
     C*             Refer to fee schedule for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END *IN55
     C*
     C     CAPMOD        IFEQ      '56'
     C                   Z-ADD     1             Q
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'BR'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'BR'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg BR: "The item indicates Pre-oper Mgmt Only mod-56
     C*             Refer to fee schedule for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END *IN56
     C*
     C                   EXSR      PMOD
     C*
     C                   END                                                    END EQ 22
     C*
     C                   END                                                    END NE *BLANK
     C*
     C* =================================================================
     C*
     C*** BEGIN DEL *** 11/30/95 **************************************
     C*** Per MEDICINE GR P. 6
     C*** Any combination of 8 modalities, procedures, or physical medicine
     C*** codes are allowable, but only for the first week of acute phase of
     C*** the injury.  After the first week, only 4 per day will be allowed.
     C**********
     C********** CACPT4    IFGE '97010'
     C********** CACPT4    ANDLE'97039'
     C********** CACPT4    ORGE '97110'
     C********** CACPT4    ANDLE'97144'                    97145 EXCLUDED
     C********** CACPT4    ORGE '97220'
     C********** CACPT4    ANDLE'97799'
     C**********
     C**** COMPUTE 1 WEEK FROM ACCIDENT DATE
     C**********
     C********** 100.0001  MULT CAACDT    SRDATE  60
     C**********           Z-ADD7         SRFUTR  50
     C**********           MOVE ' '       SRRTCD  1
     C**********           CALL 'SRFUTR'
     C**********           PARM           SRDATE
     C**********           PARM           SRFUTR
     C**********           PARM           SRRTCD
     C********** 10000.01  MULT SRDATE    WKDAT   60
     C**********
     C**********           Z-ADD*ZEROS    COUNT
     C**********           Z-ADD*ZEROS    COUNT2
     C**********           Z-ADD*ZEROS    SVFDAT  60
     C**********
     C********** CAKY9E    SETLLPCA09
     C**********
     C*** READ FILE IN FROM DATE SEQUENCE
     C********** CAKY9E    READEPCA09                    60
     C**********
     C********** *IN60     DOWEQ'0'
     C********** XRDSEQ    IFNE CADSEQ
     C********** XRTOTP    ANDGT*ZEROS
     C********** XRCPT4    IFGE '97010'
     C********** XRCPT4    ANDLE'97039'
     C********** XRCPT4    ORGE '97110'
     C********** XRCPT4    ANDLE'97144'                    97145 EXCLUDED
     C********** XRCPT4    ORGE '97220'
     C********** XRCPT4    ANDLE'97799'
     C**********
     C********** XRFDAT    IFLT WKDAT
     C*** BEGIN ADD *** 09/05/95 **************************************
     C********** XRFDAT    IFEQ CAFDAT
     C*** END   ADD *** 09/05/95 **************************************
     C********** XRFDAT    IFEQ SVFDAT
     C**********           ADD  1         COUNT
     C**********           ELSE
     C**********           Z-ADD1         COUNT
     C**********           MOVE XRFDAT    SVFDAT
     C**********           END
     C*** BEGIN ADD *** 09/05/95 **************************************
     C**********           END
     C*** END   ADD *** 09/05/95 **************************************
     C********** CAFDAT    IFLT WKDAT
     C********** COUNT     IFGE 8
     C**********           Z-ADD*ZERO     XPTOTA
     C**********           Z-ADD*ZERO     XPTOTP
     C********** CATOTC    SUB  XPTOTA    XPTOTD
     C**********           Z-ADDCATOTC    XPTOTC
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '209'     XPEOMB
     C*** EOMB 209: "Exceeds reimbursement limitations"
     C**********
     C**********           MOVE 'F  '     XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C**********           EXSR UPDAT
     C**********           GOTO MEDIE
     C**********           END
     C**********           END
     C**********
     C**********           ELSE                                 ELSE XRFDAT
     C*** BEGIN ADD *** 09/05/95 **************************************
     C********** XRFDAT    IFEQ CAFDAT
     C*** END   ADD *** 09/05/95 **************************************
     C********** XRFDAT    IFEQ SVFDAT
     C**********           ADD  1         COUNT2
     C**********           ELSE
     C**********           Z-ADD1         COUNT2
     C**********           MOVE XRFDAT    SVFDAT
     C**********           END
     C****BEGIN*ADD *** 09/05/95 **************************************
     C**********           END
     C*** END   ADD *** 09/05/95 **************************************
     C********** CAFDAT    IFGE WKDAT
     C********** COUNT2    IFGE 4
     C**********           Z-ADD*ZERO     XPTOTA
     C**********           Z-ADD*ZERO     XPTOTP
     C********** CATOTC    SUB  XPTOTA    XPTOTD
     C**********           Z-ADDCATOTC    XPTOTC
     C****BEGIN*CHG *** 08/31/95 *************************************
     C*****************    MOVE '209'     XPEOMB
     C****EOMB*209: "Exceeds reimbursement limitations"
     C**********
     C**********           MOVE 'F  '     XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C**********           EXSR UPDAT
     C**********           GOTO MEDIE
     C**********           END                             END COUNT2 GE 4
     C**********           END                             END CAFDAT GE WKDAT
     C**********           END                             END XRFDAT LT WKDAT
     C**********           END                             END GE 97110...
     C**********           END                             END NE DSEQ ...
     C********** CAKY9E    READEPCA09                    60
     C**********
     C**********           END                             END *IN60
     C**********           END                             END GE 97110...
     C**********
     C*** END   DEL *** 11/30/95 **************************************
     C***
     C*** BEGIN ADD *** 11/30/95 **************************************
     C*** Per MEDICINE GR P. 6
     C*** Any combination of 8 modalities, procedures, or physical medicine
     C*** codes are allowable, but only for the first week of acute phase of
     C*** the injury.  After the first week, only 4 per day will be allowed.
     C*
     C*** 11/30/95 CHGS EXCLUDE 97530-97541, USE LOGICAL WITH FEIN/MOD
     C*** ADD BODY PART ACCUM FOR 8 LIMIT (1ST WEEK), AND FOR 4 LIMIT
     C*** WHICH IS AFTER THE 1ST WEEK OF THE ACCIDENT
     C***
     C     CACPT4        IFGE      '97010'
     C     CACPT4        ANDLE     '97039'
     C     CACPT4        ORGE      '97110'
     C     CACPT4        ANDLE     '97144'                                      97145 EXCLUDED
     C     CACPT4        ORGE      '97220'                                      EXCLUDE
     C     CACPT4        ANDLE     '97529'                                      97530-97541
     C     CACPT4        ORGE      '97542'
     C     CACPT4        ANDLE     '97799'
     C*
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
     C                   CLEAR                   PRT8
     C                   CLEAR                   CNT8
     C                   CLEAR                   PRT4
     C                   CLEAR                   CNT4
     C                   Z-ADD     *ZEROS        SVFDAT            6 0
     C*
     C                   MOVE      '97010'       XXCPT4
     C     CAKYC         SETLL     PCA0C
     C*
     C*** READ FILE IN FROM DATE SEQUENCE   FOR THIS CASE/FEIN
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCDSEQ        IFNE      CADSEQ
     C     XCTOTP        ANDGT     *ZEROS
     C     XCCPT4        IFGE      '97010'
     C     XCCPT4        ANDLE     '97039'
     C     XCCPT4        ORGE      '97110'
     C     XCCPT4        ANDLE     '97144'                                      97145 EXCLUDED
     C     XCCPT4        ORGE      '97220'                                      EXCLUDE
     C     XCCPT4        ANDLE     '97529'                                      97530-97541
     C     XCCPT4        ORGE      '97542'
     C     XCCPT4        ANDLE     '97799'
     C*
     C     XCFDAT        IFLT      WKDAT
     C*** BEGIN ADD *** 09/05/95 **************************************
     C     XCFDAT        IFEQ      CAFDAT
     C*** END   ADD *** 09/05/95 **************************************
     C*
     C                   Z-ADD     1             X
     C     XCPART        LOOKUP    PRT8(X)                                61
     C     *IN61         IFEQ      '0'
     C     *BLANKS       LOOKUP    PRT8(X)                                61
     C  N61              Z-ADD     20            X
     C                   MOVE      XCPART        PRT8(X)
     C                   Z-ADD     1             CNT8(X)
     C                   ELSE
     C                   ADD       1             CNT8(X)
     C                   END                                                    END *IN61
     C*** BEGIN ADD *** 09/05/95 **************************************
     C                   END
     C*** END   ADD *** 09/05/95 **************************************
     C*
     C                   ELSE                                                        ELSE XCFDAT
     C*** BEGIN ADD *** 09/05/95 **************************************
     C     XCFDAT        IFEQ      CAFDAT
     C*** END   ADD *** 09/05/95 **************************************
     C*
     C                   Z-ADD     1             X
     C     XCPART        LOOKUP    PRT4(X)                                61
     C     *IN61         IFEQ      '0'
     C     *BLANKS       LOOKUP    PRT4(X)                                61
     C  N61              Z-ADD     20            X
     C                   MOVE      XCPART        PRT4(X)
     C                   Z-ADD     1             CNT4(X)
     C                   ELSE
     C                   ADD       1             CNT4(X)
     C                   END                                                    END *IN61
     C*** BEGIN ADD *** 09/05/95 **************************************
     C                   END
     C*** END   ADD *** 09/05/95 **************************************
     C                   END                                                    END XCFDAT LT..
     C                   END                                                    END GE 97110...
     C                   END                                                    END NE DSEQ ...
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C*
     C**  Only allow 8 per body area  (per day)
     C**  In the 1st week of acute phase of treatment
     C     CAFDAT        IFLT      WKDAT
     C                   Z-ADD     1             X
     C     CAPART        LOOKUP    PRT8(X)                                61
     C     *IN61         IFEQ      '1'
     C     CNT8(X)       IFGE      8
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'CD'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'CD'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C* Err Msg CD: "PT service over the 8 per day (per body area)
     C*              limit"
     C                   END                                                    END CNT8
     C                   END                                                    END *IN61
     C                   END                                                    END CAPART
     C*
     C**  Only allow 4 per body area  (per day)
     C**  After the 1st week of acute phase of treatment
     C*
     C     CAFDAT        IFGE      WKDAT
     C                   Z-ADD     1             X
     C     CAPART        LOOKUP    PRT4(X)                                61
     C     *IN61         IFEQ      '1'
     C     CNT4(X)       IFGE      4
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'CF'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'CF'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C* Err Msg CF: "PT service over the 4 per day (per body area)
     C*              limit"
     C                   END                                                    END CNT4
     C                   END                                                    END *IN61
     C                   END                                                    END CAPART
     C*
     C                   END                                                    END GE 97110...
     C*
     C*** END   ADD *** 11/30/95 **************************************
     C*
     C* =================================================================
     C*
     C*** Per MEDICINE GR P. 6 - 7
     C*** New patient visit:  Only 4 modalities, procedures, manipulations,
     C*** or phys med activies are allowed when a doctor's office visit is
     C*** also billed. This GR also applies to established patients.
     C*
     C     CACPT4        IFGE      '97010'
     C     CACPT4        ANDLE     '97039'
     C     CACPT4        ORGE      '97110'
     C     CACPT4        ANDLE     '97145'
     C     CACPT4        ORGE      '97220'
     C     CACPT4        ANDLE     '97799'
     C*
     C                   MOVE      '90000'       XXCPT4
     C                   Z-ADD     *ZEROS        COUNT
     C                   Z-ADD     *ZEROS        COUNT2
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '90000'                                      ACCUM OFFICE
     C     XRCPT4        ANDLE     '90020'                                      VISITS CODES
     C     XRCPT4        ORGE      '90100'                                      PER DAY
     C     XRCPT4        ANDLE     '90117'
     C     XRCPT4        ORGE      '90500'
     C     XRCPT4        ANDLE     '90520'
     C     XRCPT4        OREQ      '90030'
     C     XRCPT4        OREQ      '90050'
     C*********  XRTOTP    IFGT 0
     C                   ADD       1             COUNT
     C*********            END
     C                   END                                                    END GE '90000'
     C     XRCPT4        IFGE      '97010'
     C     XRCPT4        ANDLE     '97039'
     C     XRCPT4        ORGE      '97110'
     C     XRCPT4        ANDLE     '97145'
     C     XRCPT4        ORGE      '97220'
     C     XRCPT4        ANDLE     '97799'
     C     XRDSEQ        IFNE      CADSEQ
     C     XRTOTP        IFGT      0
     C                   ADD       1             COUNT2
     C                   END
     C                   END                                                    END GE '97010'
     C                   END                                                    END NE DSEQ 0'
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFGE      1                                            IF OFFICE VISIT
     C     COUNT2        IFGE      4                                            IF MOD/PROC >= 4
     C*
     C*** BEGIN ADD *** 11/30/95 **************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'CG'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'CG'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C* Err Msg CG: "Only 4 modalities, procedures, manipulations,
     C*              allowed when office visit billed"
     C*** END   ADD *** 11/30/95 **************************************
     C*** BEGIN DEL *** 11/30/95 **************************************
     C**********           Z-ADD*ZERO     XPTOTA
     C**********           Z-ADD*ZERO     XPTOTP
     C********** CATOTC    SUB  XPTOTA    XPTOTD
     C**********           Z-ADDCATOTC    XPTOTC
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '209'     XPEOMB
     C*** EOMB 209: "Exceeds reimbursement limitations"
     C**********
     C**********           MOVE 'F  '     XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C**********
     C**********           EXSR UPDAT
     C**********           GOTO MEDIE
     C*** END   DEL *** 11/30/95 **************************************
     C                   END                                                    END COUNT GE 1
     C                   END                                                    END COUNT2 GE 4
     C*
     C                   END                                                    END CACPT4 GE 90000
     C*
     C*** BEGIN DEL *** 11/30/95 *******************************************
     C* Per MEDICINE GR P. 7
     C*** Any office visit exceeding 90050 on established patients and perf
     C*** for re-evaluation  is limited to 1 every 30 days.
     C**********
     C********** CACPT4    IFEQ '90060'
     C********** CACPT4    OREQ '90070'
     C********** CACPT4    OREQ '90080'
     C**********
     C**********           Z-ADD30        SRPST
     C********** CAFDAT    MULT 100.0001  WKFDAT
     C**********           MOVE WKFDAT    SRDTE
     C**********           CALL 'SRPAST'
     C**********           PARM           SRDTE
     C**********           PARM           SRPST
     C**********           PARM           SRERR
     C**********           PARM           SRTERM
     C**********
     C**********           MOVE SRDTE     WKFDAT
     C********** WKFDAT    MULT 10000.01  WKFDAT
     C**********
     C**********           MOVE '90060'   XXCPT4
     C**********
     C********** CAKYC     SETLLPCA0C
     C********** CAKYC2    READEPCA0C                    60
     C**********           Z-ADD0         PASTCT
     C**********
     C********** *IN60     DOWEQ'0'
     C********** XCCPT4    IFEQ '90060'
     C********** XCCPT4    OREQ '90070'
     C********** XCCPT4    OREQ '90080'
     C********** XCFDAT    IFGE WKFDAT
     C********** XCFDAT    ANDLECAFDAT
     C********** XCTOTP    ANDGT*ZERO
     C********** CADSEQ    ANDNEXCDSEQ
     C**********           ADD  1         PASTCT
     C**********           END                             END XCFDAT
     C**********           END                             END 90060...
     C********** CAKYC2    READEPCA0C                    60
     C**********           END                             END DO *IN60
     C**********
     C**********           Z-ADD30        SRFUT
     C********** CAFDAT    MULT 100.0001  WKFDAT
     C**********           CALL 'SRFUTR'
     C**********           PARM           WKFDAT
     C**********           PARM           SRFUT
     C**********           PARM           SRTERM
     C**********
     C********** WKFDAT    MULT 10000.01  WKFDAT
     C**********
     C**********           MOVE '90060'   XXCPT4
     C**********
     C********** CAKYC     SETLLPCA0C
     C********** CAKYC2    READEPCA0C                    60
     C**********           Z-ADD0         FUTRCT
     C**********
     C********** *IN60     DOWEQ'0'
     C********** XCCPT4    IFEQ '90060'
     C********** XCCPT4    OREQ '90070'
     C********** XCCPT4    OREQ '90080'
     C********** XCFDAT    IFLE WKFDAT
     C********** XCFDAT    ANDGECAFDAT
     C********** XCTOTP    ANDGT*ZERO
     C********** CADSEQ    ANDNEXCDSEQ
     C**********           ADD  1         FUTRCT
     C**********           END                             END XCCPT4
     C**********           END                             END 90060...
     C********** CAKYC2    READEPCA0C                    60
     C**********           END                             END DO *IN60
     C**********
     C********** PASTCT    IFGE 1
     C********** FUTRCT    ORGE 1
     C****DISALLOW CHARGES
     C**********
     C**********           Z-ADD*ZERO     XPTOTA
     C**********           Z-ADD*ZERO     XPTOTP
     C********** CATOTC    SUB  XPTOTA    XPTOTD
     C**********           Z-ADDCATOTC    XPTOTC
     C****BEGIN*CHG *** 08/31/95 *************************************
     C*****************    MOVE '209'     XPEOMB
     C****EOMB*209: "Exceeds reimbursement limitations"
     C**********
     C**********           MOVE 'F  '     XPEOMB
     C****EOMB*F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C****END***CHG *** 08/31/95 *************************************
     C**********
     C**********           EXSR UPDAT
     C**********           GOTO MEDIE
     C**********
     C**********           END
     C**********
     C**********           END                             END EQ 90060...
     C**********
     C*** END   DEL *** 11/30/95 *************************************
     C*** BEGIN ADD *** 11/30/95 *************************************
     C* Per MEDICINE GR P. 7
     C*** Any office visit exceeding 90050 on established patients and perf
     C*** for re-evaluation  is limited to 1 every 30 days.
     C*   The first manipulation '97260' is included in office visit.
     C*
     C*   If 97260 and 90060 on same day, if within 30 days of last
     C*   97260 and 90060 display error message.
     C*
     C     CACPT4        IFEQ      '97260'
     C                   Z-ADD     0             PASTCT
     C                   Z-ADD     0             FUTRCT
     C*
     C                   Z-ADD     CAFDAT        XXFDAT
     C                   MOVE      '90060'       XXCPT4
     C     CAKYP1        CHAIN     PCA0P                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     30            SRPST
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT            8 0
     C                   MOVE      WKFDAT        SRDTE
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C*
     C                   MOVE      SRDTE         WKFDAT
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKFDAT
     C*
     C                   MOVE      '90060'       XXCPT4
     C*
     C     CAKYC         CHAIN     PCA0C                              60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        IFEQ      '90060'
     C     XCFDAT        IFGE      WKFDAT
     C     XCFDAT        ANDLE     CAFDAT
     C     XCTOTP        ANDGT     *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C*
     C                   Z-ADD     XCFDAT        XXFDAT
     C                   MOVE      '97260'       XXCPT4
     C     CAKYP1        CHAIN     PCA0P                              60
     C     *IN60         IFEQ      '0'
     C     PXTOTP        IFGT      *ZEROS
     C*
     C                   ADD       1             PASTCT
     C                   END                                                    END PXTOTP
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END XCFDAT
     C                   END                                                    END 90060...
     C     CAKYC         READE     PCA0C                                  60
     C                   END                                                    END DO *IN60
     C*
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
     C                   MOVEL     SRDTO         WKFDAT
     C*
     C                   MOVE      '90060'       XXCPT4
     C*
     C     CAKYC         CHAIN     PCA0C                              60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        IFEQ      '90060'
     C     XCFDAT        IFLE      WKFDAT
     C     XCFDAT        ANDGE     CAFDAT
     C     XCTOTP        ANDGT     *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C*
     C                   Z-ADD     XCFDAT        XXFDAT
     C                   MOVE      '97260'       XXCPT4
     C     CAKYP1        CHAIN     PCA0P                              60
     C     *IN60         IFEQ      '0'
     C     PXTOTP        IFGT      *ZEROS
     C*
     C                   ADD       1             FUTRCT
     C                   END                                                    END PXTOTP
     C                   END                                                    END *IN60
     C                   END                                                    END XCFDAT
     C                   END                                                    END 90060
     C     CAKYC2        READE     PCA0C                                  60
     C                   END                                                    END  *IN60
     C*
     C     PASTCT        IFGE      1
     C     FUTRCT        ORGE      1
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '54'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '54'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*** Err Msg 54: "Exceeds number of procedures allowed
     C***              in time period"
     C                   END
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END EQ 97260
     C*
     C*   If 97260 and 90070 on same day, if within 30 days of last
     C*   97260 and 90070 display error message.
     C*
     C     CACPT4        IFEQ      '97260'
     C                   Z-ADD     0             PASTCT
     C                   Z-ADD     0             FUTRCT
     C*
     C                   Z-ADD     CAFDAT        XXFDAT
     C                   MOVE      '90070'       XXCPT4
     C     CAKYP1        CHAIN     PCA0P                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     30            SRPST
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C                   MOVE      WKFDAT        SRDTE
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C*
     C                   MOVE      SRDTE         WKFDAT
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKFDAT
     C*
     C                   MOVE      '90070'       XXCPT4
     C*
     C     CAKYC         CHAIN     PCA0C                              60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        IFEQ      '90070'
     C     XCFDAT        IFGE      WKFDAT
     C     XCFDAT        ANDLE     CAFDAT
     C     XCTOTP        ANDGT     *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C*
     C                   Z-ADD     XCFDAT        XXFDAT
     C                   MOVE      '97260'       XXCPT4
     C     CAKYP1        CHAIN     PCA0P                              60
     C     *IN60         IFEQ      '0'
     C     PXTOTP        IFGT      *ZEROS
     C*
     C                   ADD       1             PASTCT
     C                   END                                                    END PXTOTP
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END XCFDAT
     C                   END                                                    END 90070...
     C     CAKYC         READE     PCA0C                                  60
     C                   END                                                    END DO *IN60
     C*
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
     C                   MOVEL     SRDTO         WKFDAT
     C*
     C                   MOVE      '90070'       XXCPT4
     C*
     C     CAKYC         CHAIN     PCA0C                              60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        IFEQ      '90070'
     C     XCFDAT        IFLE      WKFDAT
     C     XCFDAT        ANDGE     CAFDAT
     C     XCTOTP        ANDGT     *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C*
     C                   Z-ADD     XCFDAT        XXFDAT
     C                   MOVE      '97260'       XXCPT4
     C     CAKYP1        CHAIN     PCA0P                              60
     C     *IN60         IFEQ      '0'
     C     PXTOTP        IFGT      *ZEROS
     C*
     C                   ADD       1             FUTRCT
     C                   END                                                    END PXTOTP
     C                   END                                                    END *IN60
     C                   END                                                    END XCFDAT
     C                   END                                                    END 90070
     C     CAKYC2        READE     PCA0C                                  60
     C                   END                                                    END  *IN60
     C*
     C     PASTCT        IFGE      1
     C     FUTRCT        ORGE      1
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '54'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '54'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*** Err Msg 54: "Exceeds number of procedures allowed
     C***              in time period"
     C                   END
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END EQ 97260
     C*
     C*   If 97260 and 90080 on same day, if within 30 days of last
     C*   97260 and 90080 display error message.
     C*
     C     CACPT4        IFEQ      '97260'
     C                   Z-ADD     0             PASTCT
     C                   Z-ADD     0             FUTRCT
     C*
     C                   Z-ADD     CAFDAT        XXFDAT
     C                   MOVE      '90080'       XXCPT4
     C     CAKYP1        CHAIN     PCA0P                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     30            SRPST
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C                   MOVE      WKFDAT        SRDTE
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C*
     C                   MOVE      SRDTE         WKFDAT
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKFDAT
     C*
     C                   MOVE      '90080'       XXCPT4
     C*
     C     CAKYC         CHAIN     PCA0C                              60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        IFEQ      '90080'
     C     XCFDAT        IFGE      WKFDAT
     C     XCFDAT        ANDLE     CAFDAT
     C     XCTOTP        ANDGT     *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C*
     C                   Z-ADD     XCFDAT        XXFDAT
     C                   MOVE      '97260'       XXCPT4
     C     CAKYP1        CHAIN     PCA0P                              60
     C     *IN60         IFEQ      '0'
     C     PXTOTP        IFGT      *ZEROS
     C*
     C                   ADD       1             PASTCT
     C                   END                                                    END PXTOTP
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END XCFDAT
     C                   END                                                    END 90080...
     C     CAKYC         READE     PCA0C                                  60
     C                   END                                                    END DO *IN60
     C*
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
     C                   MOVEL     SRDTO         WKFDAT
     C*
     C                   MOVE      '90080'       XXCPT4
     C*
     C     CAKYC         CHAIN     PCA0C                              60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        IFEQ      '90080'
     C     XCFDAT        IFLE      WKFDAT
     C     XCFDAT        ANDGE     CAFDAT
     C     XCTOTP        ANDGT     *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C*
     C                   Z-ADD     XCFDAT        XXFDAT
     C                   MOVE      '97260'       XXCPT4
     C     CAKYP1        CHAIN     PCA0P                              60
     C     *IN60         IFEQ      '0'
     C     PXTOTP        IFGT      *ZEROS
     C*
     C                   ADD       1             FUTRCT
     C                   END                                                    END PXTOTP
     C                   END                                                    END *IN60
     C                   END                                                    END XCFDAT
     C                   END                                                    END 90080
     C     CAKYC2        READE     PCA0C                                  60
     C                   END                                                    END  *IN60
     C*
     C     PASTCT        IFGE      1
     C     FUTRCT        ORGE      1
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '54'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '54'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*** Err Msg 54: "Exceeds number of procedures allowed
     C***              in time period"
     C                   END
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END EQ 97260
     C*
     C*** END   ADD *** 11/30/95 *************************************
     C*
     C*** A MAJOR CODE MUST BE PRESENT ON SAME DAY FOR REIMBURSEMENT
     C***
     C*
     C     CACPT4        IFEQ      '97221'
     C     CACPT4        OREQ      '97241'
     C     CACPT4        OREQ      '97501'
     C     CACPT4        OREQ      '97521'
     C     CACPT4        OREQ      '97531'
     C     CACPT4        OREQ      '97541'
     C     CACPT4        OREQ      '97701'
     C     CACPT4        OREQ      '97721'
     C*
     C     CACPT4        IFEQ      '97221'
     C                   MOVE      '97220'       XXCPT4
     C                   END
     C*
     C     CACPT4        IFEQ      '97241'
     C                   MOVE      '97240'       XXCPT4
     C                   END
     C*
     C     CACPT4        IFEQ      '97501'
     C                   MOVE      '97500'       XXCPT4
     C                   END
     C*
     C     CACPT4        IFEQ      '97521'
     C                   MOVE      '97520'       XXCPT4
     C                   END
     C*
     C     CACPT4        IFEQ      '97531'
     C                   MOVE      '97530'       XXCPT4
     C                   END
     C*
     C     CACPT4        IFEQ      '97541'
     C                   MOVE      '97540'       XXCPT4
     C                   END
     C*
     C     CACPT4        IFEQ      '97701'
     C                   MOVE      '97700'       XXCPT4
     C                   END
     C*
     C     CACPT4        IFEQ      '97721'
     C                   MOVE      '97720'       XXCPT4
     C                   END
     C*
     C     CAKEY8        CHAIN     PCA09                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '215'     XPEOMB
     C*** EOMB 215: "A major code must be present to pay this item"
     C*
     C                   MOVE      'T  '         XPEOMB
     C*** EOMB T:   "NOT ACCORDING TO TREATMENT GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END                                                    END 1 *IN60
     C                   END                                                    END CACPT4 97501
     C*
     C*
     C*** Per MEDICINE GR P. 8
     C*** 'DOP' is required to justify the use of kinetic activities
     C*** codes (97530 and 97531) and activities of daily living
     C*** codes (97540 and 97541), on the same day.
     C*
     C     CACPT4        IFEQ      '97530'
     C     CACPT4        OREQ      '97531'
     C     CACPT4        OREQ      '97540'
     C     CACPT4        OREQ      '97541'
     C*
     C                   MOVE      '97530'       XXCPT4
     C                   Z-ADD     *ZEROS        COUNT
     C                   Z-ADD     *ZEROS        COUNT2
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFEQ      '97530'
     C     XRCPT4        OREQ      '97531'
     C     XRTOTP        IFGT      0
     C                   ADD       1             COUNT
     C                   END
     C                   END                                                    END EQ '97530'...
     C     XRCPT4        IFEQ      '97540'
     C     XRCPT4        OREQ      '97541'
     C     XRTOTP        IFGT      0
     C                   ADD       1             COUNT2
     C                   END
     C                   END                                                    END EQ '97540'...
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END *IN60
     C*
     C     CACPT4        IFEQ      '97530'
     C     CACPT4        OREQ      '97531'
     C     COUNT2        IFGE      1
     C*
     C*** BEGIN DEL *** 11/30/95 *******************************************
     C*** REMOVED CODE TO REDUCE AMT TO ZERO
     C**********           Z-ADD*ZERO     XPTOTA
     C**********           Z-ADD*ZERO     XPTOTP
     C********** CATOTC    SUB  XPTOTA    XPTOTD
     C**********           Z-ADDCATOTC    XPTOTC
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '209'     XPEOMB
     C*** EOMB 209: "Exceeds reimbursement limitations"
     C**********
     C**********           MOVE 'F  '     XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C*** END   DEL *** 11/30/95 *******************************************
     C*
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'BQ'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'BQ'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*** Err Msg BQ: "Documentation is required for reimbursement"
     C*
     C*** BEGIN DEL *** 11/30/95 **************************************
     C******************   EXSR UPDAT
     C******************   GOTO MEDIE
     C*** END   DEL *** 11/30/95 **************************************
     C                   END                                                    END COUNT2 GE 1
     C                   END                                                    END CACPT4 EQ 97530.
     C*
     C*
     C     CACPT4        IFEQ      '97540'
     C     CACPT4        OREQ      '97541'
     C     COUNT         IFGE      1
     C*
     C*** BEGIN DEL *** 11/30/95 *******************************************
     C*** REMOVED CODE TO REDUCE AMT TO ZERO
     C**********           Z-ADD*ZERO     XPTOTA
     C**********           Z-ADD*ZERO     XPTOTP
     C********** CATOTC    SUB  XPTOTA    XPTOTD
     C**********           Z-ADDCATOTC    XPTOTC
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '209'     XPEOMB
     C*** EOMB 209: "Exceeds reimbursement limitations"
     C**********
     C**********           MOVE 'F  '     XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C*** END   DEL *** 11/30/95 *******************************************
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'BQ'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'BQ'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*** Err Msg BQ: "Documentation is required for reimbursement"
     C*
     C*** BEGIN DEL *** 11/30/95 **************************************
     C******************   EXSR UPDAT
     C******************   GOTO MEDIE
     C*** END   DEL *** 11/30/95 **************************************
     C                   END                                                    END COUNT GE 1
     C                   END                                                    END CACPT4 EQ 97540.
     C*
     C                   END                                                    END CACPT4 EQ 97530.
     C*
     C* PER MEDICINE GR P. 9
     C* MUSCLE TESTING CODE 97752 REQUIRES A REPORT ID THE SERVICE PROVIDED
     C* REIMBURSEMENT IS PER BODY AREA, ONLY 2 AREAS ARE ALLOWED.
     C*
     C                   CLEAR                   BODY
     C     CACPT4        IFEQ      '97752'
     C*
     C                   Z-ADD     1             O                 5 0
     C     CAPART        LOOKUP    PART(O)                                70
     C     *IN70         IFEQ      '1'
     C                   Z-ADD     PMRA(O)       SXMRA
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '55'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '55'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C* Err Msg 55: "A copy of the justification report is required
     C*             for reimbursement"
     C*
     C                   END                                                    END *IN70
     C*
     C* MAXIMUM OF 2 AREAS UNLESS DOCUMENTED MULTIPLE INJURIES
     C*
     C                   MOVE      *BLANKS       XXCPT4
     C                   MOVEL     CACPT4        XXCPT4
     C                   Z-ADD     *ZEROS        COUNT
     C     CAKYC         CHAIN     PCA0C                              60
     C     *IN60         DOWEQ     '0'
     C     XCTOTP        IFGT      *ZEROS
     C                   Z-ADD     1             O                 5 0
     C     XCPART        LOOKUP    PART(O)                                70
     C     *IN70         IFEQ      '1'
     C                   Z-ADD     1             BODY(O)
     C                   END
     C*
     C                   END                                                    END XRTOTP
     C*
     C     CAKYC         READE     PCA0C                                  60
     C                   END                                                    END DOWEQ '0'
     C***
     C*** ADD CURRENT SERVICE TO TABLE
     C                   Z-ADD     1             O                 5 0
     C     CAPART        LOOKUP    PART(O)                                70
     C     *IN70         IFEQ      '1'
     C                   Z-ADD     1             BODY(O)
     C                   END
     C*
     C     1             DO        13            O
     C     BODY(O)       IFGT      *ZEROS
     C                   ADD       1             COUNT
     C                   END
     C                   ENDDO
     C     COUNT         IFGT      2
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'AM'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'AM'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C* Err Msg AM: "Only two areas allowed unless documentation of
     C*             multiple injuries"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END                                                    END COUNT
     C*
     C                   END                                                    END CAFDAT
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per SURGERY GR P. 4
     C*** When a starred procedures is performed at the time of the initial
     C*** office visit or emergency room visit (new patient) and this
     C*** procedure constitutes the major service
     C*** rendered during the initial visit, Procedure Code 99025 must be
     C*** used instead of the usual initial office visit code.
     C*
     C     CACPT4        IFEQ      '90000'
     C     CACPT4        OREQ      '90010'
     C     CACPT4        OREQ      '90015'
     C     CACPT4        OREQ      '90017'
     C     CACPT4        OREQ      '90020'
     C     CACPT4        OREQ      '90100'
     C     CACPT4        OREQ      '90110'
     C     CACPT4        OREQ      '90115'
     C     CACPT4        OREQ      '90117'
     C     CACPT4        OREQ      '90200'
     C     CACPT4        OREQ      '90215'
     C     CACPT4        OREQ      '90220'
     C     CACPT4        OREQ      '90300'
     C     CACPT4        OREQ      '90315'
     C     CACPT4        OREQ      '90320'
     C     CACPT4        OREQ      '90400'
     C     CACPT4        OREQ      '90410'
     C     CACPT4        OREQ      '90415'
     C     CACPT4        OREQ      '90420'
     C     CACPT4        OREQ      '90500'
     C     CACPT4        OREQ      '90505'
     C     CACPT4        OREQ      '90510'
     C     CACPT4        OREQ      '90515'
     C     CACPT4        OREQ      '90517'
     C     CACPT4        OREQ      '90520'
     C     CACPT4        OREQ      '90600'
     C     CACPT4        OREQ      '90610'
     C     CACPT4        OREQ      '90620'
     C     CACPT4        OREQ      '90630'
     C*
     C* SETLL AND READ THE FILE TESTING FOR AN '*' PROCEDURE
     C*
     C                   MOVE      '10000'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     AGAIN         TAG
     C*
     C     CAKY9A        READE     PCA09                                  54
     C*
     C  N54XRCPT4        IFGE      '10000'
     C     XRCPT4        ANDLE     '69979'
     C*
     C                   MOVE      XRCPT4        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKY          SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C     PRSUPP        ANDEQ     'Y'
     C                   MOVE      '99025'       XXCPT4
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '216'     XPEOMB
     C*** EOMB 216: "Payment is made for code 99025 instead of O/V per Fee Sche"
     C*
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
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
     C* ALSO DISPLAY ERROR MESSAGE
     C                   Z-ADD     1             Q
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'BQ'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'BQ'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg BQ: "Documentation is required for reimbursement".
     C*
     C                   ELSE
     C                   GOTO      AGAIN
     C                   END
     C*
     C                   END
     C*
     C                   END
     C* ===============================================================
     C* Per SURGERY GR p. 1
     C* IMMEDIATE PRE-OPERATIVE VISITS AND OTHER SERVICES BY THE SURGEON
     C* PAY OFFICE VISIT BUT GIVE ERROR MESSAGE THAT SERVICE
     C* FALLS WITHIN PRE-OPERATIVE POLICY
     C     CACPT4        IFGE      '90000'
     C     CACPT4        ANDLE     '90654'
     C*
     C                   Z-ADD     2             SRFUT
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
     C                   MOVE      '10000'       XXCPT4
     C     CAKYC         SETLL     PCA0C
     C*
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XCCPT4        IFGE      '10000'
     C     XCCPT4        ANDLE     '69979'
     C     XCFDAT        ANDLE     WKFDAT
     C     XCFDAT        ANDGE     CAFDAT
     C     XCTOTP        ANDGT     *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C*
     C                   Z-ADD     1             Q
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'AJ'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'AJ'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg AJ: "Service falls within pre-operative policy."
     C*** BEGIN CHG *** 03/15/96 *******************************************
     C     SXMRA         MULT(H)   XXMEDI        SXMRA
     C     CACHRG        IFGT      SXMRA
     C********** SXMRA     MULT XXMEDI    SXMRA     H
     C*** END   CHG *** 03/15/96 *******************************************
     C                   Z-ADD     SXMRA         XPTOTA
     C                   ELSE
     C                   Z-ADD     CACHRG        XPTOTA
     C                   END
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END
     C*
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C                   END                                                    END 0 DOWEQ 60
     C                   END                                                    END GE '90000'
     C*
     C*
     C* ===============================================================
     C*
     C** Total Medicine  Value * Conversion Factor = Fee
     C     CACPT4        IFNE      '97752'                                      MUSCLE TSTF
     C     SXMRA         MULT(H)   XXMEDI        SXMRA
     C                   END
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
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
      *** END DEL *** 09/21/98 ****************************************
     C*   Reget record.
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAKYZ     CHAINPCA01                54
     C***********
     C**N54******          EXCPTECA01
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
     C* VALID MODIFIERS ARE: 22, 26, 27, 90
     C*
     C* Procedure Codes with modifiers:
     C     CAPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per PG. PATHOLOGY P. 3
     C     CAPMOD        IFNE      '22'
     C     CAPMOD        ANDNE     '26'
     C     CAPMOD        ANDNE     '27'
     C     CAPMOD        ANDNE     '90'
     C*
     C                   Z-ADD     1             Q
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '10'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '10'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '201'     XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
     C                   END                                                    END EQ 22
     C*
     C                   END                                                    END NE *BLANK
     C*
     C***  IF CAPMOD = *BLANKS, SXMRA WILL TOTAL THE PROF & COMPONENT AMTS
     C***  Per p. 1 Pathology
     C*
     C***  USING UNUSUAL SERVICE MODIFIER
     C     CAPMOD        IFEQ      '22'
     C                   Z-ADD     1             Q
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '55'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '55'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 55: "A copy of the justification report is required for
     C*             reimbursement."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '201'     XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
     C                   END
     C*
     C     CACPT4        IFGE      '80050'                                      PANEL TESTS
     C     CACPT4        ANDLE     '80099'
     C                   Z-ADD     1             Q
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '55'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '55'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 55: "A copy of the justification report is required for
     C*             reimbursement."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '201'     XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
     C                   END
     C*
     C*
     C***  USING PROFESSIONAL COMPONENT ONLY
     C     CAPMOD        IFEQ      '26'
     C                   Z-ADD     PRPROF        SXMRA
     C                   END
     C*
     C***  USING TECHNICAL COMPONENT ONLY
     C     CAPMOD        IFEQ      '27'
     C                   Z-ADD     PR92N1        SXMRA
     C                   END
     C*
     C*
     C** Total Pathology  Value * Conversion Factor = Fee
     C     SXMRA         MULT(H)   XXPATH        SXMRA
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
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
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '76'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '76'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*** Err Msg 76: "CPT modifier used with this charge.  Verify use
     C***              and payment."
     C*
     C* ===============================================================
     C* Modifier -26
     C     CAPMOD        IFEQ      '26'
     C*
     C* EEG STUDIES, 95819-95823, SHOULD BE PAID AT 30% OF ALLOWABLE FEE
     C*
     C     CACPT4        IFGE      '95819'
     C     CACPT4        ANDLE     '95823'
     C*
     C     SXMRA         MULT      .30           SXMRA
     C*
     C                   END                                                    END CACPT4 GE 95819
     C                   END                                                    END PMOD EQ 26
     C*
     C*
     C* ===============================================================
     C* Modifier -80 ASSISTANT SURGEON
     C     CAPMOD        IFEQ      '80'
     C*
     C* REDUCE 20%
     C     SXMRA         MULT      .20           SXMRA
     C*
     C                   END                                                    END CAPMOD
     C*
     C* ===============================================================
     C* Modifier -85 PHYSICIAN ASSISTANT (PA) AT ASSISTANT TO SURGEON
     C     CAPMOD        IFEQ      '85'
     C*
     C* REDUCE 10%
     C     SXMRA         MULT      .10           SXMRA
     C*
     C                   END                                                    END CAPMOD
     C*
     C* ===============================================================
     C*
     C     PMODE         ENDSR
     *********************************************************************
     C** Radiology                  Codes: 70010 - 79999         R A D I *
     *********************************************************************
     C     RADI          BEGSR
     C*
     C* ===============================================================
     C* VALID MODIFIERS ARE: 22, 26, 27, 99
     C*
     C* Procedure Codes with modifiers:
     C     CAPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per Pg. RADIOLOGY p. 5
     C     CAPMOD        IFNE      '22'
     C     CAPMOD        ANDNE     '26'
     C     CAPMOD        ANDNE     '27'
     C     CAPMOD        ANDNE     '99'
     C*
     C                   Z-ADD     1             Q
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '10'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '10'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '201'     XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
     C                   END                                                    END EQ 22
     C*
     C                   END                                                    END NE *BLANK
     C*
     C***  IF CAPMOD = *BLANKS, SXMRA WILL TOTAL THE PROF & COMPONENT AMTS
     C***  Per p. 1 Radiology
     C***  BODY PARTS C2, C3, M2, M3 EXCLUDED (HANDLED IN CAT & MRI)
     C*
     C***  USING UNUSUAL SERVICE MODIFIER
     C     CAPMOD        IFEQ      '22'
     C*** BEGIN DEL *** 09/14/95 **************************************
     C********** CAPART    ANDNE'C2'
     C********** CAPART    ANDNE'C3'
     C********** CAPART    ANDNE'M2'
     C********** CAPART    ANDNE'M3'
     C*** END   DEL *** 09/14/95 **************************************
     C                   Z-ADD     1             Q
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '55'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '55'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 55: "A copy of the justification report is required for
     C*             reimbursement."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '201'     XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
     C                   END
     C*
     C*
     C***  USING PROFESSIONAL COMPONENT ONLY
     C     CAPMOD        IFEQ      '26'
     C                   Z-ADD     PRPROF        SXMRA
     C                   END
     C*
     C***  USING TECHNICAL COMPONENT ONLY
     C     CAPMOD        IFEQ      '27'
     C                   Z-ADD     PR92N1        SXMRA
     C                   END
     C*
     C*
     C* Per Pg. RADIOLOGY p. 3
     C*** BEGIN CHG *** 09/14/95 *************************************
     C* CAT Scan and MRI codes will use units as number of 'SLICES'
     C* to determine reimbursement rates.
     C* CAT SCAN CPT4 CODES:
     C     CACPT4        IFGE      '70450'
     C     CACPT4        ANDLE     '70492'
     C     CACPT4        ORGE      '71250'
     C     CACPT4        ANDLE     '71270'
     C     CACPT4        ORGE      '72125'
     C     CACPT4        ANDLE     '72133'
     C     CACPT4        ORGE      '72192'
     C     CACPT4        ANDLE     '72194'
     C     CACPT4        ORGE      '73200'
     C     CACPT4        ANDLE     '73202'
     C     CACPT4        ORGE      '73700'
     C*********  CACPT4    ANDLE'73701'                    DEL 08/25/95
     C     CACPT4        ANDLE     '73702'                                      ADD 08/25/95
     C     CACPT4        ORGE      '74150'
     C     CACPT4        ANDLE     '74170'
     C     CAUNTS        IFGE      36
     C     CAUNTS        ANDLE     45
     C     CAUNTS        SUB       35            XXDIFF
     C*
     C     CAPMOD        IFEQ      '26'
     C     XXDIFF        MULT      2             XXAMT
     C     XXAMT         ADD       SXMRA         SXMRA
     C                   END
     C*
     C     CAPMOD        IFEQ      '27'
     C     XXDIFF        MULT      6.1           XXAMT
     C     XXAMT         ADD       SXMRA         SXMRA
     C                   END
     C*
     C     CAPMOD        IFEQ      *BLANKS
     C     XXDIFF        MULT      8.1           XXAMT
     C     XXAMT         ADD       SXMRA         SXMRA
     C                   END
     C                   END
     C*
     C     CAUNTS        IFGT      45
     C* LOAD 10 SLICES (FOR SLICES 36-45)
     C                   Z-ADD     10            XXDIFF
     C     CAPMOD        IFEQ      '26'
     C     XXDIFF        MULT      2             XXAMT
     C     XXAMT         ADD       SXMRA         SXMRA
     C                   END
     C*
     C     CAPMOD        IFEQ      '27'
     C     XXDIFF        MULT      6.1           XXAMT
     C     XXAMT         ADD       SXMRA         SXMRA
     C                   END
     C*
     C     CAPMOD        IFEQ      *BLANKS
     C     XXDIFF        MULT      8.1           XXAMT
     C     XXAMT         ADD       SXMRA         SXMRA
     C                   END
     C* GET NUMBER OVER 45 SLICES
     C     CAUNTS        SUB       45            XXDIFF
     C     CAPMOD        IFEQ      '26'
     C     XXDIFF        MULT      1.2           XXAMT
     C     XXAMT         ADD       SXMRA         SXMRA
     C                   END
     C*
     C     CAPMOD        IFEQ      '27'
     C     XXDIFF        MULT      4.1           XXAMT
     C     XXAMT         ADD       SXMRA         SXMRA
     C                   END
     C*
     C     CAPMOD        IFEQ      *BLANKS
     C     XXDIFF        MULT      5.3           XXAMT
     C     XXAMT         ADD       SXMRA         SXMRA
     C                   END
     C*
     C     CAUNTS        IFGT      35
     C                   Z-ADD     1             Q
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '55'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '55'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 55: "A copy of the justification report is required for
     C*             reimbursement."
     C                   END                                                    END CAUNTS>3
     C                   END                                                    END CAUNTS>45
     C                   END                                                    END CACPT4...
     C*
     C* MRI CPT4 CODES:
     C     CACPT4        IFEQ      '70336'
     C     CACPT4        ORGE      '70540'
     C     CACPT4        ANDLE     '70552'
     C     CACPT4        OREQ      '71550'
     C     CACPT4        ORGE      '72141'
     C     CACPT4        ANDLE     '72149'
     C     CACPT4        OREQ      '72196'
     C     CACPT4        ORGE      '73220'
     C     CACPT4        ANDLE     '73221'
     C     CACPT4        ORGE      '73720'
     C     CACPT4        ANDLE     '73721'
     C     CACPT4        OREQ      '74181'
     C     CACPT4        OREQ      '75552'
     C     CACPT4        OREQ      '76400'
     C*
     C     CAUNTS        IFGE      26
     C     CAUNTS        ANDLE     39
     C     CAUNTS        SUB       25            XXDIFF
     C*
     C     CAPMOD        IFEQ      '26'
     C     XXDIFF        MULT      2.4           XXAMT
     C     XXAMT         ADD       SXMRA         SXMRA
     C                   END
     C*
     C     CAPMOD        IFEQ      '27'
     C     XXDIFF        MULT      7.1           XXAMT
     C     XXAMT         ADD       SXMRA         SXMRA
     C                   END
     C*
     C     CAPMOD        IFEQ      *BLANKS
     C     XXDIFF        MULT      9.5           XXAMT
     C     XXAMT         ADD       SXMRA         SXMRA
     C                   END
     C                   END
     C*
     C     CAUNTS        IFGT      39
     C* LOAD 14 SLICES (FOR SLICES 26-39)
     C                   Z-ADD     14            XXDIFF
     C     CAPMOD        IFEQ      '26'
     C     XXDIFF        MULT      2.4           XXAMT
     C     XXAMT         ADD       SXMRA         SXMRA
     C                   END
     C*
     C     CAPMOD        IFEQ      '27'
     C     XXDIFF        MULT      7.1           XXAMT
     C     XXAMT         ADD       SXMRA         SXMRA
     C                   END
     C*
     C     CAPMOD        IFEQ      *BLANKS
     C     XXDIFF        MULT      9.5           XXAMT
     C     XXAMT         ADD       SXMRA         SXMRA
     C                   END
     C* GET NUMBER OVER 39 SLICES
     C     CAUNTS        SUB       39            XXDIFF
     C     CAPMOD        IFEQ      '26'
     C     XXDIFF        MULT      2.0           XXAMT
     C     XXAMT         ADD       SXMRA         SXMRA
     C                   END
     C*
     C     CAPMOD        IFEQ      '27'
     C     XXDIFF        MULT      4.1           XXAMT
     C     XXAMT         ADD       SXMRA         SXMRA
     C                   END
     C*
     C     CAPMOD        IFEQ      *BLANKS
     C     XXDIFF        MULT      6.1           XXAMT
     C     XXAMT         ADD       SXMRA         SXMRA
     C                   END
     C*
     C     CAUNTS        IFGT      26
     C                   Z-ADD     1             Q
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '55'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '55'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 55: "A copy of the justification report is required for
     C*             reimbursement."
     C                   END                                                    END CAUNTS>26
     C                   END                                                    END CAUNTS>39
     C                   END                                                    END CACPT4...
     C*
     C*** END   CHG *** 09/14/95 **************************************
     C*** BEGIN DEL *** 09/14/95 **************************************
     C* Per Pg. RADIOLOGY p. 3
     C* CAT Scan procedure codes require body part, this will be used
     C* to determine the reimbursement factor rates.
     C*********
     C*********  CAPART    IFEQ 'C2'
     C*********  CAPART    OREQ 'C3'
     C*********  CAPART    OREQ 'M2'
     C*********  CAPART    OREQ 'M3'
     C*********            Z-ADD1         Q
     C*********  '00000'   LOKUPQQS,Q                    60
     C**N60****            Z-ADD30        Q
     C*********            MOVELCADSEQ    QQS,Q
     C*********            MOVE '55'      QQC,Q
     C* Err Msg 55: "A copy of the justification report is required for
     C*             reimbursement."
     C*********            END
     C*********
     C*********  CAPART    IFEQ 'C2'
     C*********  CAPMOD    IFEQ *BLANKS
     C*********            Z-ADD8.1       SXMRA
     C*********            ELSE
     C*********  CAPMOD    IFEQ '26'
     C*********            Z-ADD2.0       SXMRA
     C*********            ELSE
     C*********  CAPMOD    IFEQ '27'
     C*********            Z-ADD6.1       SXMRA
     C*********            END                             END CAPMOD = 27
     C*********            END                             END CAPMOD = 26
     C*********            END                             END CAPMOD = ' '
     C*********            END                             END CAPART = C2
     C*********
     C*********  CAPART    IFEQ 'C3'
     C*********  CAPMOD    IFEQ *BLANKS
     C*********            Z-ADD5.3       SXMRA
     C*********            ELSE
     C*********  CAPMOD    IFEQ '26'
     C*********            Z-ADD1.2       SXMRA
     C*********            ELSE
     C*********  CAPMOD    IFEQ '27'
     C*********            Z-ADD4.1       SXMRA
     C*********            END                             END CAPMOD = 27
     C*********            END                             END CAPMOD = 26
     C*********            END                             END CAPMOD = ' '
     C*********            END                             END CAPART = C3
     C*
     C* Per Pg. RADIOLOGY p. 3
     C* MRI procedure codes require body
     C* part, this will be used to determine the reimbursement factor
     C* rates.
     C*
     C*********  CAPART    IFEQ 'M2'
     C*********  CAPMOD    IFEQ *BLANKS
     C*********            Z-ADD9.5       SXMRA
     C*********            ELSE
     C*********  CAPMOD    IFEQ '26'
     C*********            Z-ADD2.4       SXMRA
     C*********            ELSE
     C*********  CAPMOD    IFEQ '27'
     C*********            Z-ADD7.1       SXMRA
     C*********            END                             END CAPMOD = 27
     C*********            END                             END CAPMOD = 26
     C*********            END                             END CAPMOD = ' '
     C*********            END                             END CAPART = M2
     C*********
     C*********  CAPART    IFEQ 'M3'
     C*********  CAPMOD    IFEQ *BLANKS
     C*********            Z-ADD6.1       SXMRA
     C*********            ELSE
     C*********  CAPMOD    IFEQ '26'
     C*********            Z-ADD2.0       SXMRA
     C*********            ELSE
     C*********  CAPMOD    IFEQ '27'
     C*********            Z-ADD4.1       SXMRA
     C*********            END                             END CAPMOD = 27
     C*********            END                             END CAPMOD = 26
     C*********            END                             END CAPMOD = ' '
     C*********            END                             END CAPART = M3
     C*********            END                             END CACPT4 70450
     C*** END   DEL *** 09/14/95 **************************************
     C*
     C***Total*Radiology  Value * Conversion Factor = Fee
     C     SXMRA         MULT(H)   XXRADI        SXMRA
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
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
     C     CAPMOD        IFNE      *BLANK
     C*
     C     CAPMOD        IFEQ      '20'
     C     CAPMOD        OREQ      '22'
     C     CAPMOD        OREQ      '26'
     C     CAPMOD        OREQ      '32'
     C     CAPMOD        OREQ      '47'
     C     CAPMOD        OREQ      '50'
     C     CAPMOD        OREQ      '51'
     C     CAPMOD        OREQ      '52'
     C     CAPMOD        OREQ      '54'
     C     CAPMOD        OREQ      '55'
     C     CAPMOD        OREQ      '56'
     C     CAPMOD        OREQ      '62'
     C     CAPMOD        OREQ      '66'
     C     CAPMOD        OREQ      '75'
     C     CAPMOD        OREQ      '76'
     C     CAPMOD        OREQ      '77'
     C     CAPMOD        OREQ      '80'
     C     CAPMOD        OREQ      '85'
     C     CAPMOD        OREQ      '90'
     C     CAPMOD        OREQ      '99'
     C*
     C     CAPMOD        IFEQ      '54'
     C                   Z-ADD     1             Q
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '70'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '70'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 70: "The item indicates Surgical Care Only mod-54
     C*             Refer to fee schedule for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END CAPMOD EQ 54
     C*
     C     CAPMOD        IFEQ      '55'
     C                   Z-ADD     1             Q
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '64'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '64'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 64: "The item indicates Postoper Mgmt Only mod-55
     C*             Refer to fee schedule for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END CAPMOD EQ 55
     C*
     C     CAPMOD        IFEQ      '56'
     C                   Z-ADD     1             Q
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'BR'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'BR'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg BR: "The item indicates Pre-oper Mgmt Only mod-56
     C*             Refer to fee schedule for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END CAPMOD EQ 56
     C*
     C                   EXSR      PMOD
     C*
     C                   ELSE                                                   ELSE CAPMOD
     C*
     C                   Z-ADD     1             Q
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '10'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '10'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '201'     XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C*
     C                   MOVE      'T  '         XPEOMB
     C* EOMB T:  "NOT ACCORDING TO TREATMENT GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END                                                    END CAPMOD EQ 20
     C*
     C                   END                                                    END CAPMOD
     C*
     C*   If current code exists in table, must have associated code else
     C*     error has occurred
     C*
     C                   Z-ADD     1             R                 5 0
     C                   MOVE      '0'           *IN54
     C     SURLOK        TAG
     C     CACPT4        LOOKUP    SUR1(R)                                60
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
     C*
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '215'     XPEOMB
     C*** EOMB 215: "A major code must be present to pay this item"
     C*
     C                   MOVE      'T  '         XPEOMB
     C*** EOMB T:   "NOT ACCORDING TO TREATMENT GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
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
     C*
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '215'     XPEOMB
     C*** EOMB 215: "A major code must be present to pay this item"
     C*
     C                   MOVE      'T  '         XPEOMB
     C*** EOMB T:   "NOT ACCORDING TO TREATMENT GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C                   END
     C*
     C* Per SURGERY GR p. 3 - When a co-surgeon acts in the role of
     C* an assistant surgeon doc from the operative report must
     C* substantiate the need (modifier 80 must be used).
     C     CAPMOD        IFEQ      '80'
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '55'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '55'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C* Err Msg 55: "A copy of the justification report is required
     C*             for reimbursement"
     C                   END
     C*
     C*
     C* Per SURGERY GR p. 12 - The services of a PA in lieu of an
     C* assistant surgeon will require doc which supports the
     C* specific need for an assistant surgeon (modifier 85).
     C     CAPMOD        IFEQ      '85'
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '55'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '55'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C* Err Msg 55: "A copy of the justification report is required
     C*             for reimbursement"
     C                   END
     C*
     C*
     C* Per SURGERY GR p. 3 - Codes 22842 & 22845 with modifer 52
     C* are to be paid at 4.0 RVU per level
     C*
     C     CACPT4        IFEQ      '22842'
     C     CACPT4        OREQ      '22845'
     C     CAPMOD        IFEQ      '52'
     C*
     C                   Z-ADD     4             SXMRA
     C*
     C                   ELSE
     C* Per Surgery p. 3 UPDATE 12/11/91
     C* Codes 22842 and 22845 are considered bilateral procedures
     C* and will always be paid at 50% as secondary procedures.
     C*
     C     XXYMD         IFGE      911211
     C                   MULT      .50           SXMRA
     C                   END                                                    END XXYMD
     C*
     C                   END                                                    END CAPMOD
     C                   END                                                    END CACPT4
     C*
     C* Per SURGERY GR p. 3 - Codes 63020 & 63030 with modifer 52
     C* are to be paid at an additional 2 RVUs
     C*
     C     CAPMOD        IFEQ      '52'
     C     CACPT4        IFEQ      '63020'
     C     CACPT4        OREQ      '63030'
     C     CACPT4        OREQ      '63035'
     C*
     C                   ADD       2             SXMRA
     C*
     C                   END                                                    END CACPT4
     C                   END                                                    END CAPMOD
     C*
     C* NOTE: ALL SURGERY PROCEDURES ARE WRITTEN TO THE WORKFILE
     C* EXCEPT - Modifiers: 52, 80, 85
     C*          Procedure Codes: 22842 and 22845
     C*
     C     CACPT4        IFNE      '22842'
     C     CACPT4        ANDNE     '22845'
     C     CAPMOD        IFNE      '52'
     C     CAPMOD        ANDNE     '80'
     C     CAPMOD        ANDNE     '85'
     C* ===============================================================
     C* Write record to Workfile
     C* Code has passed all edits - Write to the FACAPTX Workfile
     C* Only load "current charges" into the FACAPTX Workfile
     C*
     C* Exceptions are found in SUR1/SUR2 table and cannot be cut
     C*
     C                   Z-ADD     1             T
     C     CACPT4        LOOKUP    SUR1(T)                                60
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
     C                   END                                                    END CAPMOD EQ '51'
     C                   END                                                    END CACPT4 NE 22842
     C*
     C     NOCUT         TAG
     C*
     C* ===============================================================
     C*
     C** Total SURGERY   Value * Conversion Factor = Fee
     C     SXMRA         MULT(H)   XXSURG        SXMRA
     C*
     C* ===============================================================
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
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
      *** BEG ADD *** 09/21/99 ****************************************
     C                   MOVE      *BLANKS       P2EOMB
      *** END ADD *** 09/21/99 ****************************************
      *
      * Look thru FDCAP-Non pended charges
      *
     C     CAKY9C        SETLL     PCA09
     C     CAKY9D        READE     PCA09                                  61
      *
     C     *IN61         DOWEQ     '0'
      *
     C     XRCPT4        IFLE      '69979'
      *** BEG CHG *** 09/21/98 ****************************************
     C********** XREOB#    IFNE *ZEROS
      *
      * Incr. counter only if different EOB# (ie payment has been set)
      *
     C     XREOB#        IFNE      QQEOB#
      *** END CHG *** 09/21/98 ****************************************
     C     XRPART        IFEQ      'O'
     C                   ADD       1             COUNT
     C                   ELSE
     C     XRPART        IFEQ      'O1'
     C                   ADD       1             CNTB1
     C                   ELSE
     C     XRPART        IFEQ      'O2'
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
      *** BEG ADD *** 09/21/99 ****************************************
     C                   MOVE      *BLANKS       P2EOMB
      *** END ADD *** 09/21/99 ****************************************
     C*
     C* ===============================================================
     C* Per SURGERY GR Pg. 3:  Multiple Procedures (O/O1/O2)
     C* One operative site: Body Part "O"
     C*  Highest value 100%
     C*  after 1st      50%
     C*  PMOD 62 - Co-Surgeons - Highest value 75%, thereafter 50%
     C*
     C     P2PART        IFEQ      'O'
     C     COUNT         IFLE      0
     C                   Z-ADD     1             P2BRP                          1st: 100%
     C                   ADD       1             COUNT
     C     P2PMOD        IFEQ      '62'
     C                   Z-ADD     .75           P2BRP
     C                   END
     C                   ELSE                                                   ELSE EQ 0
     C     COUNT         IFGE      1
     C                   Z-ADD     .50           P2BRP                          2nd: 50%
     C                   ADD       1             COUNT
     C                   END                                                    END  EQ 1
     C                   END                                                    END  LE 0
     C*
     C*
     C                   ELSE                                                   ELSE EQ 'O'
     C*
     C* ===============================================================
     C* Bilateral Site/Primary: Body Part "O1"
     C*  Highest value 100%
     C*  after 1st      50%
     C*  PMOD 62 - Co-Surgeons - Highest value 75%, thereafter 50%
     C*
     C     P2PART        IFEQ      'O1'
     C     CNTB1         IFLE      0
     C                   Z-ADD     1             P2BRP                          1st: 100%
     C                   ADD       1             CNTB1
     C     P2PMOD        IFEQ      '62'
     C                   Z-ADD     .75           P2BRP
     C                   END
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
     C*  PMOD 62 - Co-Surgeons - Highest value 75%, thereafter 50%
     C*
     C     P2PART        IFEQ      'O2'
     C     CNTB2         IFLE      0
     C                   Z-ADD     1             P2BRP                          1st: 100%
     C                   ADD       1             CNTB2
     C     P2PMOD        IFEQ      '62'
     C                   Z-ADD     .75           P2BRP
     C                   END
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
     C     P2MRA         MULT      P2BRP         XXWRK1            9 3
     C*
     C** Total SURGERY   Value * Conversion Factor = Fee
     C     XXWRK1        MULT(H)   XXSURG        P2MRA
     C*
     C     P2TOTC        COMP      P2MRA                              54        >>>>>
     C   54              Z-ADD     P2MRA         P2TOTA
     C   54              Z-ADD     *ZEROS        P2DIFF
     C  N54              Z-ADD     P2TOTC        P2TOTA
     C  N54P2MRA         SUB       P2TOTA        P2DIFF
     C     P2TOTC        SUB       P2TOTA        P2TOTD
     C                   Z-ADD     P2TOTA        P2TOTP
     C                   Z-ADD     P2MRA         P2BRTC                         ADD 09/15/94
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
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '202'     P2EOMB
     C* EOMB Code 202: "Charges exceed maximum allowance found in
     C*                 Reimbursement guidelines"
     C*
     C                   MOVE      'F  '         P2EOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C                   END                                                    END P2TOTD GT *ZERO
     C*
     *** BEG ADD *** 09/21/98 *****************************************
     C* Call subroutine to write EOMB to FDDEP. Set QQFILE so EOMB
     C* subroutine will use the P2 fields for updating.
     C                   MOVE      P2EOMB        XPEOMB
      *
     C                   MOVE      P2DSEQ        QQDSEQ
      *
     C                   MOVE      'P2'          QQFILE            2
      *
     C                   EXSR      EOMB
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
     C                   END                                                    END NE 'Y'
     C*
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ENDIF                                                  *IN61=OFF
      *** END ADD *** 09/21/98 ****************************************
     C*
     C                   READ      PCAWK2                                 60
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
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '202'     XPEOMB
     C* EOMB Code 202: "Charges exceed maximum allowance found in
     C*                 Reimbursement guidelines"
     C*
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C                   END
     C*
      *** BEG ADD *** 09/21/98 ****************************************
     C* Call subroutine to write EOMB to FDDEP. Set QQFILE so the EOMB
     C* subroutine will use the CA fields for updating.
     C                   MOVE      CADSEQ        QQDSEQ
     C                   MOVE      'CA'          QQFILE
      *
     C                   EXSR      EOMB
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
      *** END ADD *** 09/21/98 ****************************************
     C*
     C* ===============================================================
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM.
     C*
     C     CACPT4        IFNE      *BLANK
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB#    ANDEQ*ZERO
      *** END DEL *** 09/21/98 ****************************************
     C                   MOVE      CACPT4        XXCPT4
     C                   MOVE      CAPMOD        XXPMOD
     C     CAKYP         CHAIN     PCA0P                              60
     C     DUPCL2        TAG
     C*
     C     *IN60         IFEQ      '0'
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
     C*
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C                   ELSE
     C*
     C     CAKYP         READE     PCA0P                                  60
     C  N60              GOTO      DUPCL2
     C                   END
     C                   END
     C                   END
     C*
     C* ===============================================================
     C** Only process the record if not an override type
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAKY1A    CHAINPCA01                54
      *** END DEL *** 09/21/98 ****************************************
     C     CSOVRD        IFNE      'Y'
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CACHK#    ANDEQ*ZERO
     C********** CAEOB#    ANDEQ*ZERO
     C******************** Z-ADDXPTOTA    CATOTA
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     XPTOTD        CAMRA
     C                   Z-ADD     XPTOTP        CAPAID
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
      *** BEG CHG *** 09/21/98 ****************************************
     C**N54*************** UPDATPCA01
     C                   UPDATE    PCA13
      *** END CHG *** 09/21/98 ****************************************
     C* If Fee Schedule non-reimburseable ... display error message
     C* XX25EM Flag is set at beginning of program where PRFSR eq 'N'
     C     XX25EM        IFEQ      '1'
     C                   MOVE      CACPT4        XXCPT4
     C     CAKY6         CHAIN     PCA06                              60
     C     *IN60         IFEQ      '0'
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '19'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '19'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C* Err Msg 19: *WARNING* Need Letter of Medical Necessity to pay this item
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
     C                   MOVE      *BLANKS       XPPMOD
     C                   MOVE      *BLANK        XPEOMB
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'XX00000'     XXMSG#
      *** END ADD *** 09/21/98 ****************************************
     C* ===============================================================
     C*
     C     UPDAE         ENDSR
     *********************************************************************
      *** BEG DEL *** 09/21/98 ****************************************
     O***********PCA01   E                ECA01
      *** END DEL *** 09/21/98 ****************************************
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
**  SUR1/SUR2: A MUST HAVE B AND A WILL NOT BE CUT
1100111000
1110111100
1120111200
1170111700
1171111710
1173111730
1173211730
1192211920
1510115100
1512115120
1522115220
1520115200
1524115240
1526115260
1578715786
1700117000
1700217000
1710117100
1710217100
1720117200
1900119000
1730717304
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
2612526121
2686126860
2686326862
2735827355
2735827356
2735827357
2769227691
3330033305
3330033310
3330033315
3330033320
3330033322
3330033330
3330033335
3330033350
3353033510
3353033511
3353033512
3353033513
3353033514
3353033516
3383033802
3383033803
3383033813
3383033814
3383033820
3383033822
3383033824
3383033840
3383033845
3383033851
3383033852
3383033855
3568135601
3568135606
3568135612
3568135616
3568135621
3568135626
3568135631
3568135636
3568135637
3568135638
3568135641
3568135642
3568135645
3568135646
3568135650
3568135651
3568135654
3568135656
3568135661
3568135663
3568135665
3568135666
3568135671
3621836215
3621836216
3621836217
3624836245
3624836246
3624836247
4111441112
4111441113
4495544950
4700147000
5861158611
5941259400
5941259510
5952559510
5952559515
6110661253
6171261680
6171261682
6171261684
6171261686
6171261690
6171261692
6171261700
6171261702
6171261703
6171261705
6171261708
6171261710
6171261711
6179561720
6179561735
6179561750
6179561751
6179561770
6179561780
6179561790
6179561791
6179561793
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
6444364440
6444364442
6462364622
6472764702
6472764704
6472764708
6472764712
6472764713
6472764714
6472764716
6472764718
6472764719
6472764721
6472764722
6472764726
6477864776
6478364782
6478764774
6478764776
6478764778
6478764782
6478764783
6478764784
6478764786
6483064831
6483064834
6483064835
6483064836
6483064840
6483064856
6483064857
6483064858
6483064861
6483064862
6483064864
6483064865
6483064866
6483064868
6483064870
6483064872
6483064874
6483064876
6483264831
6483764834
6483764835
6483764836
6485964858
6487264831
6487264834
6487264835
6487264836
6487264837
6487264840
6487264856
6487264858
6487264859
6487264861
6487264862
6487264864
6487264885
6487264886
6487264890
6487264891
6487264892
6487264893
6487264895
6487264896
6487264897
6487264898
6487264901
6487264902
6487264905
6487264907
6487464872
6487464876
6490164885
6490164886
6490164890
6490164891
6490164892
6490164893
6490164895
6409164896
6409164897
6409164898
6490264885
6490264886
6490264890
6490264891
6490264892
6490264893
6490264895
6490264896
6490264897
6490264898
6733567311
6733567312
6733567314
6733567316
6733567318
6733567320
6733567331
6733567332
6733567334
**  PART VS. MRA
01000007500
02000010000
03000010000
04000025000
05000010000
06000010000
7A000025000
7B000010000
7C000010000
7D000025000
7E000010000
7F000010000
7G000010000
