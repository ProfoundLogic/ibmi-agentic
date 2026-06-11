     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H* PROGRAM: F4072RKS
     H* DESCRIPTION: ADJUDICATION (KANSAS RULES - 09/01/97)
     H*   This program is called from F0070R (Claim Adjudication) and will
     H* perform the actual adjudication of the detail claim records writ-
     H* ten by F0070R.  This program was derived from the Kansas
     H* State Fee Schedule Reimbursment Manual issued on 09/01/97.
     H* This Sub-program is intended to be used to calculate the amount
     H* that is due a Provider of services, by the Carrier of the Work comp
     H* insurance.  It should be used for any service provided on or after
     H* 09/01/97.
     H*****************************************************************
     H*  CREATED:10/10/97   C:59   PROJECT: KS003
     H****************************************************************F
     H*
     H* PROGRAMMER NOTE: THE MAIN SECTION HAS BEEN DUPLICATED
     H* IN ORDER TO MAKE TWO PASSES OF THE DATA
     H*
     H* Information needed for each year's adjudication program rules:
     H*    1.  Update the effective date range for reading the detail charges
     H*        (found in the mainline section)
     H*    2.  Update Conversion Factors:
     H*        (found in *LIKE section)
     H*        XXANES  - ANESTHESIOLOGIST
     H*        XXDENT  - DENTISTRY
     H*        XXSURG  - SURGERY
     H*        XXRADI  - RADIOLOGY
     H*        XXPATH  - PATHOLOGY
     H*        XXMEDI  - MEDICINE
     H*        XXSPEC  - SPECIAL SERVICES
     H*****************************************************************
     H*  SPECIALTY CODES USED IN THIS PROGRAM:
     H*  ANE - ANESTHESIOLOGISTS
     H*  RNA - REGISTERED NURSE ANESTHESIOLOGIST
     H*  CHI - CHIROPRACTOR
     H*  DO  - DOCTOR OF OSTEOPATHY
     H*  DDS -
     H*  DMD -
     H*  MD  - MEDICAL DOCTOR
     H*  OCT -
     H*  PHT - PHYSICAL THERAPY
     H*  PHY - PHYSICIAN
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  08/09/02 C:062 PROJECT: FS244
     H*                 USE CAZIPA.
     H*
     H*  09/25/01 C:062 PROJECT: FSMNT
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.
     H*                                                               *
     H*  11/13/99 C:79  Project: KS012                                *
     H*                 Change service date range to 10/01/99         *
     H*                                                               *
     H*  06/15/99 C:79 PROJ: KS004
     H*                Modified to work with Provider Bills
     H*                                                               *
     H*  04/08/99 C:117 Support: MC095 Seq# 17568
     H*                 Added QQCO#, QQSUB#, QQFND to the *ENTRY PARMs
     H*                 and changed all occurences of UPCO#, UPSUB#,
     H*                 UPFND to the above mentioned fields.
     H*                                                               *
     H*  12/10/98 C:79  Project: MC095 Supp: 15433                    *
     H*                 Program will go to N2TRCD instead of NXTRCD   *
     H*                 if inside 2nd main do-loop                    *
     H*                                                               *
     H*  11/05/98 C:79  Project: MC095 Supp: 14320  (C:91)            *
     H*                 Program will go to the next record instead of *
     H*                 terminating if there is no hit FMPRPOK        *
     H*                                                               *
     H*  09/04/98 C:31  Project: 95MAL                                *
     H*                 Ntwk Disc:Lesser of FS/Chr                    *
     H*                 1. Add update of CAMRAA                       *
     H*                                                               *
     H*  02/24/99 C:117 Project: MC095 Supp: 15658                    *
     H*                 Conversion factor incorrect for XXSPEC & the  *
     H*                 wrong factor is used for *PROC                *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:
     H*  54    Use with Primary FDCAP file (i.e. DOWEQ, etc on reading charges)
     H*  60    General Use
     H*  61    General Use
     H*  62    General Use
     H*****************************************************************
     H*  SECTION                                  SUBR         CODES
     H*  Anesthesia                               ANES         00100-01999
     H*  Dental                                   DENT         D0000-D9999
     H*  Depositions                              DEPO   CNCDT,DEPOS,IMESE,MEDRC
     H*  Medical/Physical                         MEDI         90000-99199
     H*  Office Visits                            OFFI         99201-99499
     H*  Pathology                                PATH         80002-89399
     H*  Vocational Rehabilitation                REHB         QRPPS,QRPNP
     H*  Radiology                                RADI         70010-79999
     H*  Radiology2                               RADI2  7261-77799,79000-79999
     H*  Surgery                                  SURG         10000-69979
     H*                                           SURG2        10000-69979
     H*****************************************************************
     F*  FDCAL7 is the Primary File Read (PCA01)
     FFDCALH    UF   E           K DISK
     F*
     FFACAPKS   O    E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK1)
     FFACALKS   IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK2)
     FFACAL1KS  IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK3)
     FFDCAL6    IF   E           K DISK
     F                                     RENAME(PCA01:PCA06)
     FFDCAL5    UF   E           K DISK
     F                                     RENAME(PCA01:PCA08)
     FFDCAL9    IF   E           K DISK
     F                                     RENAME(PCA01:PCA09)
     FFDCALA    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0A)
     FFDCALC    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0C)
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0P)
     FFMCML1    IF   E           K DISK
     F*** BEGIN DEL *** 08/09/02 **************************************
     F**********FMGZL   IF  E           K        DISK
     F*** END   DEL *** 08/09/02 **************************************
     FFMPDL     IF   E           K DISK
     FFMPRLKS   IF   E           K DISK
     FFMRCL     IF   E           K DISK
     FFTABLKS   IF   E           K DISK
     F                                     IGNORE(PRC01)
     FSMUPL     IF   E           K DISK
     FFDDHL     IF   E           K DISK
     FFDDHL2    IF   E           K DISK
     F                                     RENAME(PDH01:PDH03)
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDDEL4    UF A E           K DISK
     FFAEOL4    IF   E           K DISK
     FFDMGL1    UF A E           K DISK
     FFDMGL5    IF   E           K DISK
     F                                     RENAME(PMG01:PMG02)

     D*
     D* "Entry" Arrays
     D QQS             S              5    DIM(999)                             DETAIL SEQ #
     D QQC             S              2    DIM(999)                             ERROR MESSAGES
     D*
     D* Physical Therapy Array
     D PT              S              7  0 DIM(22)                              physical dt cntr.
     D*
     D FUF             S              7  0 DIM(500)                             FOLLOW UP FRM DY
     D FUT             S              7  0 DIM(500)                             FOLLOW UP TO DAY
     D*
     D* Compile-time Arrays
     D QCA             S             55    DIM(6) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D ANEPM1          S              2    DIM(6) CTDATA PERRCD(1)              ANES PX MODIFI
     D ANEPM2          S              1  0 DIM(6) ALT(ANEPM1)
     D SUR1            S              5    DIM(90) CTDATA PERRCD(1)             CUT/MAJOR-MIN
     D SUR2            S              5    DIM(90) ALT(SUR1)
     D SEPP            S              5    DIM(275) CTDATA PERRCD(1)            SEPARATE PROCEDURE
     D NOCT            S              5    DIM(117) CTDATA PERRCD(1)            NO CUT SURGERY
     D* Run-time Arrays
     D CPT4            S              5    DIM(30)                              CPT4 ARRAY
     D PRT             S              2    DIM(20)                              PART TBL
     D MDAT            S              6  0 DIM(30)                              DATE ARRAY
     D PDAT            S              6  0 DIM(30)                              DATE ARRAY
     D** MODIFIER ARRAY
     D MOD             S              2    DIM(100)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*  Date Data Structure.
     D                 DS
     D  XXYMD2                 1      6  0
     D  XXYM2                  1      4  0
     D                 DS
     D  XXYMD3                 1      6  0
     D  XXYM3                  1      4  0

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
     IPCA08
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
     I              CAANMN                      XAANMN
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
     I              CACPT4                      XRCPT4
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
     I* Record Format for SURG2 Workfile FACALKS
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
     I*
     I* Record Format for RADI2 Workfile FACAL1KS
     IPCAWK3
     I              CACO#                       P3CO#
     I              CASUB#                      P3SUB#
     I              CAFND                       P3FND
     I              CAFYR                       P3FYR
     I              CACASE                      P3CASE
     I              CACSCD                      P3CSCD
     I              CAFDAT                      P3FDAT
     I              CATDAT                      P3TDAT
     I              CARDAT                      P3RDAT
     I              CAIPOP                      P3IPOP
     I              CAFEIN                      P3FEIN
     I              CAMOD                       P3MOD
     I              CAPRTY                      P3PRTY
     I              CATRCD                      P3TRCD
     I              CASVCD                      P3SVCD
     I              CAPMOD                      P3PMOD
     I              CACPT4                      P3CPT4
     I              CAICD9                      P3ICD9
     I              CAANMN                      P3ANMN
     I              CAUNTS                      P3UNTS
     I              CATOTC                      P3TOTC
     I              CATOTA                      P3TOTA
     I              CATOTD                      P3TOTD
     I              CAEOMB                      P3EOMB
     I              CATOTP                      P3TOTP
     I              CADSEQ                      P3DSEQ
     I              CADELF                      P3DELF
     I              CAFLAG                      P3FLAG
     I              CACHK#                      P3CHK#
     I              CAEOB#                      P3EOB#
     I              CAPART                      P3PART
     I              CAP1                        P3P1
     I              CASS#                       P3SS#
     I              CAACDT                      P3ACDT
     I              CASTAT                      P3STAT
     I              CAUSER                      P3USER
     I              CAPGM                       P2PGM
     I              CATIME                      P3TIME
     I              CADT                        P3DT
     I              CAIUSR                      P3IUSR
     I              CAIDT                       P3IDT
     I              CADXR                       P3DXR
     I              CADCS#                      P3DCS#
     I              CALTR#                      P3LTR#
     I              CARCFL                      P3RCFL
     I              CAOTHR                      P3OTHR
     I              CANDIS                      P3NDIS
     I              CARDTP                      P3RDTP
     I              CABRSV                      P3BRSV
     I              CABRPM                      P3BRPM
     I              CABRPC                      P3BRPC
     I              CABRAM                      P3BRAM
     I              CABRUN                      P3BRUN
     I              CABRTC                      P3BRTC
     I              CAZIPA                      P3ZIPA
     I              CAAST                       P3AST
     I              CADIFF                      P3DIFF
     I              CABRPT                      P3BRPT
     I              CADAFL                      P3DAFL
     I              CAMRA                       P3MRA
     I              CABREC                      P3BREC
     I              CABRP                       P3BRP
     I              CATYPE                      P3TYPE

     C*
     C*  CLAIM DETAIL FILE
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
     C*** Used with PCA08 (FDCAL8) in SURG2
     C     CAKY8A        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    PPDSEQ
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
     C*** Used with PCA09 (FDCAL9) in MEDI, SURG
     C     CAKY9E        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C*
     C     CAKY9F        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
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
     C     CAKYA3        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    XYYMD             7 0
     C*
     C     CAKYP2        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    KKDSEQ
     C*
     C     CAKYZ         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXYMD
     C                   KFLD                    HLDSEQ
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
     C*  Claim Status Code Table File
     C     TCSKY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    TABKEY
     C*
     C*  Dummy key used for release of all CA records
     C     DUMMY         KLIST
     C                   KFLD                    QQCO#
     C*** BEGIN DEL *** 08/09/02 **************************************
     C*
     C*** Geographical Zip Area FILE
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
     C*  Provider Auxiliary File
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
      *
     C*  CLAIM DETAIL FILE
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
     C     DHKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     DHKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     DHKY2         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXCPT4
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
     C                   PARM                    QQSS#             9 0          CHAIN TO EE
     C                   PARM                    QQACDT            8 0          (MMDDYY ORD)
     C                   PARM                    QQSPCD            3
     C                   PARM                    QQEOB#            9 0
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
     C     QQRTCD        CABEQ     'P'           TERM                           PRE-OPEN FILES
     C*
     C*== Initialization Section ======================================
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDATE            7 0
     C*
     C     *LIKE         DEFINE    CADSEQ        XXSEQ
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
     C     *LIKE         DEFINE    PR92N2        SX92N2
     C     *LIKE         DEFINE    PR92N3        SX92N3
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
     C     *LIKE         DEFINE    CABRUN        XPBRTC
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
     C     *LIKE         DEFINE    CAPART        XXPART
     C     *LIKE         DEFINE    XPPMOD        XXPMOD
     C     *LIKE         DEFINE    CAFDAT        XXYMD
     C     *LIKE         DEFINE    CAFDAT        XXFDAT
     C*** BEGIN CHG *** 08/09/02 **************************************
     C********** *LIKE     DEFN GZZIPA    XXZIPA
     C     *LIKE         DEFINE    RCZIPA        XXZIPA
     C*** END   CHG *** 08/09/02 **************************************
     C     *LIKE         DEFINE    CARCN         XXRCN
     C     *LIKE         DEFINE    DENOT1        XXNOT1
     C     *LIKE         DEFINE    MGMSG#        XXMSG#
     C     *LIKE         DEFINE    MGMSEQ        XXMSEQ
     C     *LIKE         DEFINE    CAEOB#        XXEOB#
     C     *LIKE         DEFINE    CAACDT        XXACDT
     C*
     C     *LIKE         DEFINE    CAFDAT        KKYMD
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    DEESRC        KKESRC
     C*
     C                   Z-ADD     042.50        XXANES            5 2           ANES Conversion
     C                   Z-ADD     100.00        XXSURG            5 2           SURG Conversion
     C                   Z-ADD     100.00        XXRADI            5 2           RADI Conversion
     C                   Z-ADD     100.00        XXPATH            5 2           PATH Conversion
     C                   Z-ADD     075.00        XXMEDI            5 2           MEDI Conversion
     C*
     C*** BEG CHG *** 02/24/1999 **************************************
     C******************   Z-ADD050.00    XXSPEC  52        SPEC Conversion
     C                   Z-ADD     030.00        XXSPEC            5 2           SPEC Conversion
     C*** END CHG *** 02/24/1999 **************************************
     C*
     C                   Z-ADD     025.00        XXEVAL            5 2           EVAL Conversion
     C                   Z-ADD     030.00        XXDENT            5 2           DENT Conversion
     C                   Z-ADD     *ZERO         WKFDAT            8 0           MM/DD/CCYY
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
     C                   Z-ADD     0             HLADDT            7 0          To hold MVR
     C                   Z-ADD     0             HLANMN            9 0          CAANMN for 60 minute
     C                   Z-ADD     0             HLANTV            7 0          Anes Time Value
     C                   Z-ADD     0             XXANTV            5 2          Anes WORKFIELD
     C                   MOVE      ' '           OPNFLG            1            File Open Flag
     C                   MOVE      '0'           OFCPD             1            CLEAR TMP IN(NOT USD
     C                   MOVE      ' '           OVRFLG            1            File Override Flag
     C                   Z-ADD     0             SRFUT             5 0
     C                   Z-ADD     0             SRPST             5 0
     C                   Z-ADD     *ZERO         SRDTE             8 0
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
     C                   MOVE      '0'           XX25EM            1
     C                   MOVE      *BLANKS       QQTYPE            1
     C                   Z-ADD     0             XXUNTS            7 4
     C                   Z-ADD     0             XXUNT             7 4
     C*
     C* ===============================================================
     C*
     C*  Get the user parameter file
     C     UPKY          CHAIN     PUP01                              60
     C*
     C*  If the get was unsuccessful, end the program
     C     *IN60         IFEQ      '1'
     C                   MOVE      '99999'       QQS(1)
     C                   MOVE      '95'          QQC(1)
     C* Err Msg 95: "Invalid User Parameter, See Programmer"
     C                   GOTO      TERM
     C                   END
     C*
     C*  Get the Claim Master Record
     C     CMKY          CHAIN     PCM01                              60
     C*
     C*  If the get was unsuccessful, end the program
     C     *IN60         IFEQ      '1'
     C                   MOVE      '99998'       QQS(1)
     C                   MOVE      '96'          QQC(1)
     C* Err Msg 96: Invalid Case Number, See Programmer
     C                   GOTO      TERM
     C                   END
     C*
     C*** XXPASS - USED AS FLAG TO ZERO OUT PAID FIELDS WHEN ERRORS
     C*** OCCUR ON FIRST PASS OF DATA
     C*** XXPASS, 1 = FIRST PASS;  2 = SECOND PASS
     C*
     C                   MOVE      '1'           XXPASS            1
     C*
     C                   EXSR      LODFU                                        LOAD FOLLOW-UP
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
     C                   Z-ADD     *ZEROS        KKYMD
     C*
     C     CAKYH         SETLL     PCA01
     C     CAKYH1        READE     PCA01                                  54
     C*
     C*+*+*+*+*+*+* Do Loop for Each Claim Detail Record +*+*+*+*+*+*+*+*+*+**
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
     C*
     C                   EXCEPT    ECA01
     C*
     C     CAFDAT        IFGE      970901
     C*** BEG ADD *** 11/13/99 ****************************************
     C     CAFDAT        ANDLT     991001
     C*** END ADD *** 11/13/99 ****************************************
     C     XXRCN         ANDEQ     CARCN
     C*
     C                   MOVE      '0'           OFCPD
     C*
     C                   Z-ADD     CAFDAT        XXYMD
     C                   Z-ADD     CADSEQ        KKDSEQ
     C*
     C     DHKY          CHAIN     PDH01                              60
     C*
     C                   SELECT
      *
     C     DHMOD1        WHENNE    *BLANKS
     C                   MOVEL     DHMOD1        XPPMOD
      *
     C     DHMOD2        WHENNE    *BLANKS
     C                   MOVEL     DHMOD2        XPPMOD
      *
     C     DHMOD3        WHENNE    *BLANKS
     C                   MOVEL     DHMOD3        XPPMOD
      *
     C     DHMOD4        WHENNE    *BLANKS
     C                   MOVEL     DHMOD4        XPPMOD
      *
     C                   ENDSL
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     DHUNTS        SVUNTS
     C                   MOVEL     DHPRCD        XPCPT4
     C                   MOVEL     XPPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C*
     C                   MOVE      QQSPCD        SVSPCD
     C*
     C* If the Service Date is greater than the system date, give err message
     C     CAFDAT        IFGT      SYDATE
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '79'          QQC(Q)
     C*** Err Msg 79: "Service Date entered is greater than today's date
     C*                (system date)."
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
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '97'          QQC(Q)
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
     C*** BEGIN CHG *** 08/09/02 **************************************
     C*
     C* 2nd - Get the corresponding Geographic Zip Area record:
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
     C     XPPMOD        IFNE      *BLANK
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
     C                   ELSE                                                   ELSE XPPMOD NE *BLAN
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '91'          QQC(Q)
     C* Err Msg 91: "Carrier to set payment on individual case basis
     C*             based on documentation"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   END                                                    END XPPMOD NE *BLANK
     C*
     C                   ELSE                                                   ELSE RCRPT EQ 'Y'
     C*
     C     CACHRG        COMP      RCMRA                              61
     C   61              MOVE      '206'         XPEOMB
     C* EOMB Code 206: "This charge has been paid per R&C fees"
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
     C     CAKY1A        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   END
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
     C*   Hospital Subroutine or Regular Providers or pharmacy records
     C     CARCFL        IFEQ      'S'
     C     CARCFL        OREQ      'N'
     C     CARCFL        OREQ      'X'
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
      *** BEG CHG *** 11/05/98 ****************************************
     C**********           Z-ADD1         Q
     C********** '00000'   LOKUPQQS,Q                    60
     C**N60*****           Z-ADD30        Q
     C**********           MOVELCADSEQ    QQS,Q
     C**********           MOVE '98'      QQC,Q
     C********** Err Msg 98: "Invalid Procedure Code, See Programmer"
     C**********           GOTO TERM
     C                   GOTO      NXTRCD
      *** END CHG *** 11/05/98 ****************************************
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
     C   60              MOVE      '206'         XPEOMB
     C* EOMB Code 206: "This charge has been paid per resonable
     C*                 and customary fees"
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
     C                   MOVE      PR92N2        SX92N2
     C                   MOVE      PR92N3        SX92N3
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
     C* ===============================================================
     C* PER P. 191 MEDICINE  - MATERIALS SUPPLIED BY HEALTH CARE PROV.
     C* Supplies and materials provided by the health care provider
     C* over and above those usually included with the office visits or
     C* other services rendered may be charged for seperately.  Code
     C* 99070 will return error message.
     C*
     C     DHPRCD        IFEQ      '99070'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EW'          QQC(Q)
     C* Err Msg EW: "SUPPLIES = invoice plus 25% or $15.00, MEDS = AWP
     C*              + $6.00 whichever is less"
     C*
     C                   ENDIF                                                  END DHPRCD
     C*
     C* PER P. 220 RADIOLOGY
     C* Payment for prof services (mod -26) will not be made when X-ray
     C* taken elsewhere are reviewed during an IME, medical visit or
     C* consultation.  (or as X-Ray Consultation CPT 76140)
     C*
     C     DHPRCD        IFEQ      '76140'
     C*
     C                   MOVE      '99201'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '99201'                                      OFFICE VIS
     C     XRCPT4        ANDLE     '99285'
     C     XRCPT4        ORGE      '99354'                                      OFFICE VIS
     C     XRCPT4        ANDLE     '99358'
     C     XRCPT4        OREQ      '99499'                                      OFFICE VIS
     C     XRTOTP        IFGT      0
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '29'          QQC(Q)
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '215'         XPEOMB
     C*** EOMB 215: "This procedure was paid under a complete code".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END XRCPT4
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END DOW *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '91'          QQC(Q)
     C* Err Msg 91: "Carrier to set payment on individual case basis
     C*             based on documentation"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
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
     C     XPPMOD        IFNE      *BLANK
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
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '24'          QQC(Q)                         ANE MESSAGE
     C* Err Msg 24: "The item is ANE - requires further inspection
     C*             by an adjuster"
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
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C*
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '83'          QQC(Q)                         ANE MESSAGE
     C* Err Msg 83: "This item is I.C. - requires further inspection
     C*             by an adjuster"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '83'          QQC(Q)                         ANE MESSAGE
     C* Err Msg 83: "This item is I.C. - requires further inspection
     C*             by an adjuster"
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
     C*================================================================
     C* If modifiers -26, -27 is billed, the same date, the opposite
     C* mod will not pay
     C*
     C                   Z-ADD     0             X
     C     XPPMOD        IFEQ      '26'
     C     XPPMOD        OREQ      '27'
     C                   MOVEL     DHPRCD        XXCPT4
     C*
     C     CAKY9         SETLL     PCA09
     C     CAKY9         READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XRPMOD        IFEQ      '26'
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRTOTP        ANDGT     0
     C     XRPMOD        OREQ      '27'
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRTOTP        ANDGT     0
     C                   ADD       1             X
     C                   ENDIF                                                  END XRPMOD
     C*****************************************************************
     C     XRCPT4        IFEQ      DHPRCD
     C     XRFDAT        ANDEQ     CAFDAT
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRFLAG        ANDNE     'P'
     C     XRTOTP        ANDGT     0
     C*
     C     X             IFGE      2
     C     XRPMOD        OREQ      XPPMOD
     C     XRPMOD        ORNE      '26'
     C     XRPMOD        ANDNE     '27'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 61
     C  N61              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '205'         XPEOMB
     C*   EOMB 205: "This procedure is inclueded in the basic allowance "
     C*             "of another code"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   END                                                    END X
     C                   END                                                    END XRCPT4
     C*
     C     CAKY9         READE     PCA09                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C                   END                                                    END XPPMOD
     C*****************************************************************
     C*================================================================
     C* If modifiers -26, -27 is billed previously do not pay
     C*
     C     XPPMOD        IFEQ      '  '
     C                   MOVEL     DHPRCD        XXCPT4
     C*
     C     CAKY9         SETLL     PCA09
     C     CAKY9         READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XRPMOD        IFEQ      '26'
     C     XRTOTP        ANDGT     0
     C     XRFLAG        ANDNE     'P'
     C     XRPMOD        OREQ      '27'
     C     XRTOTP        ANDGT     0
     C     XRFLAG        ANDNE     'P'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 61
     C  N61              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '205'         XPEOMB
     C*   EOMB 205: "This procedure is inclueded in the basic allowance "
     C*             "of another code"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   END                                                    END X
     C*
     C     CAKY9         READE     PCA09                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C                   END                                                    END XPPMOD
     C*****************************************************************
     C*-- Execute all Subroutines here ---------------------------------
     C*
     C     DHPRCD        IFGE      '00100'                                      ANESTHESIA
     C     DHPRCD        ANDLE     '01999'
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
     C                   CLOSE     FACALKS
     C                   CLOSE     FACAPKS
     C                   CLOSE     FACAL1KS
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*
     C*     to FACAL1KS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACKS'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*
     C*     to FACAL1KS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALKS
     C                   OPEN      FACAPKS
     C                   OPEN      FACAL1KS
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
     C                   EXSR      RADI2
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALKS
     C                   CLOSE     FACAPKS
     C                   CLOSE     FACAL1KS
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*
     C*     to FACAL1KS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACKS'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*
     C*     to FACAL1KS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALKS
     C                   OPEN      FACAPKS
     C                   OPEN      FACAL1KS
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
     C*-------------------------------------------------------------------
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the workfile is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALKS
     C                   CLOSE     FACAPKS
     C                   CLOSE     FACAL1KS
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1KS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACKS'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1KS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALKS
     C                   OPEN      FACAPKS
     C                   OPEN      FACAL1KS
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
     C                   EXSR      RADI2
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALKS
     C                   CLOSE     FACAPKS
     C                   CLOSE     FACAL1KS
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1KS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACKS'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1KS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALKS
     C                   OPEN      FACAPKS
     C                   OPEN      FACAL1KS
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
     C*
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
     C                   EXSR      MEDI
     C*
     C                   ELSE                                                      ELSE MEDI
     C     DHPRCD        IFGE      '99201'                                      EVAL & MGMT
     C     DHPRCD        ANDLE     '99499'
     C                   EXSR      OFFI
     C*
     C                   ELSE                                                      ELSE OFFI
     C     DHPRCD        IFGE      'D0000'                                      DENTISTRY
     C     DHPRCD        ANDLE     'D9999'
     C                   EXSR      DENT
     C*
     C                   ELSE                                                      ELSE DENT
     C*
     C     DHPRCD        IFEQ      'QRPPS'                                      REHAB
     C     DHPRCD        OREQ      'QRPNP'
     C                   EXSR      REHB
     C*
     C                   ELSE                                                      ELSE REHB
     C*
     C     DHPRCD        IFEQ      'DEPOS'                                      DEPOSITIONS
     C     DHPRCD        OREQ      'IMESE'
     C     DHPRCD        OREQ      'MEDRC'
     C     DHPRCD        OREQ      'CNCDT'
     C                   EXSR      DEPO
     C*
     C                   ELSE                                                      ELSE DEPO
     C                   EXSR      OTHE                                         OTHER CODES
     C*
     C                   END                                                       END DEPO
     C                   END                                                       END REHB
     C                   END                                                       END DENT
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
     C*
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     DHKY2         CHAIN     PDH03                              54
     C*
     C     *IN54         DOWEQ     *OFF
     C*
     C                   Z-ADD     DHDSEQ        KKDSEQ
     C*
     C     CAKYP2        CHAIN     PCA0P                              61
     C*
     C     *IN61         IFEQ      *OFF
     C*
     C     PXFDAT        ANDEQ     CAFDAT
     C     PXFEIN        ANDEQ     CAFEIN
     C     PXDSEQ        ANDNE     CADSEQ
     C     PXFLAG        ANDNE     'P'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     SVDSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C* Err Msg 03: Possible Duplicate claim exists for this item.
     C*
     C                   ENDIF
     C*
     C     DHKY2         READE     PDH03                                  54
     C*
     C                   ENDDO
     C*
     C     CAKY1A        CHAIN     PCA01                              60
     C     *IN60         IFEQ      '0'
     C*   If override type, we must still output the allowed/disallowed
     C     CAMRA         IFEQ      0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CATOTA        ANDEQ     0
     C     CAPAID        ANDEQ     0
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C*
     C*   If there is an override percentage, calculate
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CACHRG        MULT      XXOVRP        XPTOTA
     C                   END
     C*
     C     CACHRG        SUB       XPTOTA        CAMRA
     C                   Z-ADD     XPTOTA        CAPAID
     C                   END
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     SXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA01
     C*
     C                   END
     C                   END                                                    END EQ OVRRD
     C*
     C                   ENDIF                                                  CAFDAT IFGE
     C*
     C*** Process next record *****************************************
     C     NXTRCD        TAG
     C*
     C*
     C     CAKYH1        READE     PCA01                                  54      GET NEXT RCD
     C*
     C                   ENDDO
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
     C* INITIALIZE VALUES FROM ENTRY QQS, AND QQC
     C                   MOVE      *ZEROS        QQS
     C                   MOVE      *ZEROS        QQC
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
     C                   Z-ADD     *ZEROS        KKYMD
     C*
     C     CAKYH         SETLL     PCA01
     C     CAKYH1        READE     PCA01                                  54
     C*
     C*+*+*+*+*+*+* Do Loop for Each Claim Detail Record +*+*+*+*+*+*+*+*+*+**
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
     C*
     C                   EXCEPT    ECA01
     C*
     C     CAFDAT        IFGE      970901
     C*** BEG ADD *** 11/13/99 ****************************************
     C     CAFDAT        ANDLT     991001
     C*** END ADD *** 11/13/99 ****************************************
     C     CARCN         ANDEQ     XXRCN
     C*
     C                   MOVE      '0'           OFCPD
     C*
     C                   Z-ADD     CAFDAT        XXYMD
     C                   Z-ADD     CADSEQ        KKDSEQ
     C*
     C     DHKY          CHAIN     PDH01                              60
     C*
     C                   SELECT
      *
     C     DHMOD1        WHENNE    *BLANKS
     C                   MOVEL     DHMOD1        XPPMOD
      *
     C     DHMOD2        WHENNE    *BLANKS
     C                   MOVEL     DHMOD2        XPPMOD
      *
     C     DHMOD3        WHENNE    *BLANKS
     C                   MOVEL     DHMOD3        XPPMOD
      *
     C     DHMOD4        WHENNE    *BLANKS
     C                   MOVEL     DHMOD4        XPPMOD
      *
     C                   ENDSL
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     DHUNTS        SVUNTS
     C                   MOVEL     DHPRCD        XPCPT4
     C                   MOVEL     XPPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C*
     C                   MOVE      QQSPCD        SVSPCD
     C*
     C* If the Service Date is greater than the system date, give err message
     C     CAFDAT        IFGT      SYDATE
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '79'          QQC(Q)
     C*** Err Msg 79: "Service Date entered is greater than today's date
     C*                (system date)."
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
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '97'          QQC(Q)
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
     C*** BEGIN CHG *** 08/09/02 **************************************
     C*
     C* 2nd - Get the corresponding Geographic Zip Area record:
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
     C     XPPMOD        IFNE      *BLANK
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
     C                   ELSE                                                   ELSE XPPMOD NE *BLAN
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '91'          QQC(Q)
     C* Err Msg 91: "Carrier to set payment on individual case basis
     C*             based on documentation"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD
     C                   END                                                    END XPPMOD NE *BLANK
     C*
     C                   ELSE                                                   ELSE RCRPT EQ 'Y'
     C*
     C     CACHRG        COMP      RCMRA                              61
     C   61              MOVE      '206'         XPEOMB
     C* EOMB Code 206: "This charge has been paid per resonable
     C*                 and customary fees"
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
     C     CAKY1A        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD
     C                   END
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
     C*   Hospital Subroutine or Regular Providers or pharmacy records
     C     CARCFL        IFEQ      'S'
     C     CARCFL        OREQ      'N'
     C     CARCFL        OREQ      'X'
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
      *** BEG CHG *** 11/05/98 ****************************************
     C**********           Z-ADD1         Q
     C********** '00000'   LOKUPQQS,Q                    60
     C**N60*****           Z-ADD30        Q
     C**********           MOVELCADSEQ    QQS,Q
     C**********           MOVE '98'      QQC,Q
     C********** Err Msg 98: "Invalid Procedure Code, See Programmer"
     C**********           GOTO TERM
      *** BEG CHG *** 12/10/98 ****************************************
     C******************** GOTO NXTRCD
     C                   GOTO      N2TRCD
      *** END CHG *** 12/10/98 ****************************************
      *** END CHG *** 11/05/98 ****************************************
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
     C   60              MOVE      '206'         XPEOMB
     C* EOMB Code 206: "This charge has been paid per resonable
     C*                 and customary fees"
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
     C                   MOVE      PR92N2        SX92N2
     C                   MOVE      PR92N3        SX92N3
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
     C* ===============================================================
     C* PER P. 191 MEDICINE  - MATERIALS SUPPLIED BY HEALTH CARE PROV.
     C* Supplies and materials provided by the health care provider
     C* over and above those usually included with the office visits or
     C* other services rendered may be charged for seperately.  Code
     C* 99070 will return error message.
     C*
     C     DHPRCD        IFEQ      '99070'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EW'          QQC(Q)
     C* Err Msg EW: "SUPPLIES = invoice plus 25% or $15.00, MEDS = AWP
     C*              + $6.00 whichever is less"
     C*
     C                   ENDIF                                                  END DHPRCD
     C* ===============================================================
     C* PER P. 149 RADIOLOGY
     C* Payment for prof services (mod -26) will not be made when X-ray
     C* taken elsewhere are reviewed during an IME, medical visit or
     C* consultation.  (or as X-Ray Consultation CPT 76140)
     C*
     C     DHPRCD        IFEQ      '76140'
     C*
     C                   MOVE      '99201'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '99201'                                      OFFICE VIS
     C     XRCPT4        ANDLE     '99285'
     C     XRCPT4        ORGE      '99354'                                      OFFICE VIS
     C     XRCPT4        ANDLE     '99358'
     C     XRCPT4        OREQ      '99499'                                      OFFICE VIS
     C     XRTOTP        IFGT      0
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '29'          QQC(Q)
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '215'         XPEOMB
     C*** EOMB 215: "This procedure was paid under a complete code".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END XRCPT4
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END DOW *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '91'          QQC(Q)
     C* Err Msg 91: "Carrier to set payment on individual case basis
     C*             based on documentation"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD                                       GET NEXT RECORD
     C*
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
     C     XPPMOD        IFNE      *BLANK
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
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '24'          QQC(Q)                         ANE MESSAGE
     C* Err Msg 24: "The item is ANE - requires further inspection
     C*             by an adjuster"
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
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C*
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '83'          QQC(Q)                         ANE MESSAGE
     C* Err Msg 83: "This item is I.C. - requires further inspection
     C*             by an adjuster"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '83'          QQC(Q)                         ANE MESSAGE
     C* Err Msg 83: "This item is I.C. - requires further inspection
     C*             by an adjuster"
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
     C*================================================================
     C* If modifiers -26, -27 is billed, the same date, the opposite
     C* mod will not pay
     C*
     C                   Z-ADD     0             X
     C     XPPMOD        IFEQ      '26'
     C     XPPMOD        OREQ      '27'
     C                   MOVEL     DHPRCD        XXCPT4
     C*
     C     CAKY9         SETLL     PCA09
     C     CAKY9         READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XRPMOD        IFEQ      '26'
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRTOTP        ANDGT     0
     C     XRPMOD        OREQ      '27'
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRTOTP        ANDGT     0
     C                   ADD       1             X
     C                   ENDIF                                                  END XRPMOD
     C*****************************************************************
     C     XRCPT4        IFEQ      DHPRCD
     C     XRFDAT        ANDEQ     CAFDAT
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRFLAG        ANDNE     'P'
     C     XRTOTP        ANDGT     0
     C*
     C     X             IFGE      2
     C     XRPMOD        OREQ      XPPMOD
     C     XRPMOD        ORNE      '26'
     C     XRPMOD        ANDNE     '27'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 61
     C  N61              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '205'         XPEOMB
     C*   EOMB 205: "This procedure is inclueded in the basic allowance "
     C*             "of another code"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD
     C                   END                                                    END X
     C                   END                                                    END XRCPT4
     C*****************************************************************
     C*
     C     CAKY9         READE     PCA09                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C                   END                                                    END XPPMOD
     C*
     C*****************************************************************
     C* If modifiers -26, -27 is billed previously do not pay
     C*
     C     XPPMOD        IFEQ      '  '
     C                   MOVEL     DHPRCD        XXCPT4
     C*
     C     CAKY9         SETLL     PCA09
     C     CAKY9         READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XRPMOD        IFEQ      '26'
     C     XRTOTP        ANDGT     0
     C     XRFLAG        ANDNE     'P'
     C     XRPMOD        OREQ      '27'
     C     XRTOTP        ANDGT     0
     C     XRFLAG        ANDNE     'P'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 61
     C  N61              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '205'         XPEOMB
     C*   EOMB 205: "This procedure is inclueded in the basic allowance "
     C*             "of another code"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      N2TRCD
     C                   END                                                    END X
     C*
     C     CAKY9         READE     PCA09                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C                   END                                                    END XPPMOD
     C*****************************************************************
     C     DHPRCD        IFGE      '00100'                                      ANESTHESIA
     C     DHPRCD        ANDLE     '01999'
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
     C                   CLOSE     FACALKS
     C                   CLOSE     FACAPKS
     C                   CLOSE     FACAL1KS
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1KS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACKS'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1KS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALKS
     C                   OPEN      FACAPKS
     C                   OPEN      FACAL1KS
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
     C                   EXSR      RADI2
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALKS
     C                   CLOSE     FACAPKS
     C                   CLOSE     FACAL1KS
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1KS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACKS'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1KS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALKS
     C                   OPEN      FACAPKS
     C                   OPEN      FACAL1KS
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
     C*-------------------------------------------------------------------
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the workfile is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALKS
     C                   CLOSE     FACAPKS
     C                   CLOSE     FACAL1KS
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1KS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACKS'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1KS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALKS
     C                   OPEN      FACAPKS
     C                   OPEN      FACAL1KS
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
     C                   EXSR      RADI2
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALKS
     C                   CLOSE     FACAPKS
     C                   CLOSE     FACAL1KS
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1KS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACKS'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1KS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALKS
     C                   OPEN      FACAPKS
     C                   OPEN      FACAL1KS
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
     C                   EXSR      MEDI
     C*
     C                   ELSE                                                      ELSE MEDI
     C     DHPRCD        IFGE      '99201'                                      EVAL & MGMT
     C     DHPRCD        ANDLE     '99499'
     C                   EXSR      OFFI
     C*
     C                   ELSE                                                      ELSE EVAL
     C     DHPRCD        IFGE      'D0000'                                      DENTISTRY
     C     DHPRCD        ANDLE     'D9999'
     C                   EXSR      DENT
     C*
     C                   ELSE                                                      ELSE DENT
     C*
     C*
     C     DHPRCD        IFEQ      'QRPPS'                                      REHAB
     C     DHPRCD        OREQ      'QRPNP'
     C                   EXSR      REHB
     C*
     C                   ELSE                                                      ELSE REHB
     C*
     C     DHPRCD        IFEQ      'DEPOS'                                      DEPOSITIONS
     C     DHPRCD        OREQ      'IMESE'
     C     DHPRCD        OREQ      'MEDRC'
     C     DHPRCD        OREQ      'CNCDT'
     C                   EXSR      DEPO
     C*
     C                   ELSE                                                      ELSE DEPO
     C*
     C                   EXSR      OTHE                                         OTHER CODES
     C*
     C                   END                                                       END DEPO
     C                   END                                                       END REHB
     C                   END                                                       END DENT
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
     C*
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     DHKY2         CHAIN     PDH03                              54
     C*
     C     *IN54         DOWEQ     *OFF
     C*
     C                   Z-ADD     DHDSEQ        KKDSEQ
     C*
     C     CAKYP2        CHAIN     PCA0P                              61
     C*
     C     *IN61         IFEQ      *OFF
     C*
     C     PXFDAT        ANDEQ     CAFDAT
     C     PXFEIN        ANDEQ     CAFEIN
     C     PXDSEQ        ANDNE     CADSEQ
     C     PXFLAG        ANDNE     'P'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     SVDSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C* Err Msg 03: Possible Duplicate claim exists for this item.
     C*
     C                   ENDIF
     C*
     C     DHKY2         READE     PDH03                                  54
     C*
     C                   ENDDO
     C*
     C     CAKY1A        CHAIN     PCA01                              60
     C     *IN60         IFEQ      '0'
     C*   If override type, we must still output the allowed/disallowed
     C     CAMRA         IFEQ      0
     C     CAOTHR        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CATOTA        ANDEQ     0
     C     CAPAID        ANDEQ     0
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C*
     C*   If there is an override percentage, calculate
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CACHRG        MULT      XXOVRP        XPTOTA
     C                   END
     C*
     C     CACHRG        SUB       XPTOTA        CAMRA
     C                   Z-ADD     XPTOTA        CAPAID
     C                   END
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     SXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA01
     C*
     C                   END
     C                   END                                                    END EQ OVRRD
     C*
     C                   ENDIF                                                  CAFDAT IFGE
     C*
     C*** Process next record *****************************************
     C     N2TRCD        TAG
     C*
     C*
     C     CAKYH1        READE     PCA01                                  54      GET NEXT RCD
     C*
     C                   ENDDO
     C*  END DUP OF MAIN CODE
     C*****************************************************************
     C*
     C*** Execute SURG2 if a SURG Code has been processed
     C     OPNFLG        IFEQ      '1'
     C                   EXSR      SURG2                                        Process the SURG cod
     C                   EXSR      RADI2                                        Process the RADI cod
     C                   END                                                    END OPNFLG EQ '1'
     C*
     C*  End the program
     C     TERM          TAG
     C*
     C*  Release all the formats for a save return.
     C*
     C     DUMMY         CHAIN     PCA01                              54
     C  N54              EXCEPT    ECA01
     C     DUMMY         CHAIN     PCA08                              54
     C  N54              EXCEPT    ECA08
     C*
     C*----------------------------------------------------------------------
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALKS
     C                   CLOSE     FACAPKS
     C                   CLOSE     FACAL1KS
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPKS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1KS
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*---------------------------------------------------------------------
     C*
     C* Write adjudication messages to FDMGP
     C                   EXSR      MSG
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     0             SXMRA
     C*** END ADD *** 09/04/98 ****************************************
     C                   RETURN
     C                   MOVE      '1'           *INLR
     *********************************************************************
     *** Anesthesia                 Codes: 00100 - 01999         A N E S *
     *********************************************************************
     C     ANES          BEGSR
     C*
     C*=====================================================================
     C*=== Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '23'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '53'
     C     XPPMOD        ANDNE     '59'
     C     XPPMOD        ANDNE     'P1'
     C     XPPMOD        ANDNE     'P2'
     C     XPPMOD        ANDNE     'P3'
     C     XPPMOD        ANDNE     'P4'
     C     XPPMOD        ANDNE     'P5'
     C     XPPMOD        ANDNE     'P6'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      '201'         XPEOMB
     C* EOMB 201 "SERVICE IS NOT REIMBURSEABLE UNDER THE WORKER'S
     C*          " COMPENSATION PROGRAM".
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C*
     C                   ELSE
     C*
     C                   EXSR      PMOD
     C*
     C                   END                                                    END EQ '22'
     C*
     C                   END                                                    END NE *BLANK
     C*
     C*
     C***  USING UNUSUAL SERVICE / UNUSUAL ANESTHESIA MODIFIER
     C     XPPMOD        IFEQ      '22'
     C     XPPMOD        OREQ      '23'
     C     XPPMOD        OREQ      '32'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '91'          QQC(Q)
     C* Err Msg 91: "Carrier to set payment on individual case basis
     C*             based on documentation"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C                   END                                                    END XPPMOD NE *BLANK
     C*
     C* Per P. 6 Anesthesia - Physical status modifiers
     C* See table in Fee Sched manual
     C     XPPMOD        IFEQ      'P1'
     C     XPPMOD        OREQ      'P2'
     C     XPPMOD        OREQ      'P3'
     C     XPPMOD        OREQ      'P4'
     C     XPPMOD        OREQ      'P5'
     C     XPPMOD        OREQ      'P6'
     C*
     C                   Z-ADD     1             X
     C     XPPMOD        LOOKUP    ANEPM1(X)                              60
     C     *IN60         IFEQ      '1'
     C     ANEPM2(X)     ADD       SXANAM        SXANAM
     C                   END                                                    END 1 *IN60
     C                   END                                                    END EQ 'P1'
     C*
     C*
     C***  PER P. 5  ANESTHESIA
     C     SVSPCD        IFNE      'ANE'
     C     SVSPCD        ANDNE     'RNA'
     C     SVSPCD        ANDNE     'PHY'
     C     SVSPCD        ANDNE     'MD '
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BP'          QQC(Q)
     C*** Err Msg BP: "Specialty code and level of service don't
     C***              match, please verify"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      '209'         XPEOMB
     C* EOMB 209:"Invalid procedure based on provider's specialty".
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C*
     C                   END
     C* ===============================================================
     C***  EDIT FOR MISSING ANESTHESIA UNITS
     C     DHUNTS        IFLT      1
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AH'          QQC(Q)
     C*** Err Msg AH: "Refer to corresponding Anesthesia code for
     C***             base units".
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      '201'         XPEOMB
     C* EOMB 201:"Service is not reimburseable under the
     C*            worker's compensation program".
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C*
     C                   END
     C*
     C*=== Calculating Time Units =====================================
     C*
     C* The Time Units are calculated by allowing one unit for each
     C* 15 minutes or significant fraction thereof of anesthesia time.
     C* If time > 3 hours, 1 unit is allowed for every 10 minutes.
     C*
     C                   Z-ADD     *ZEROS        XXADDM            5 0
     C     XAANMN        IFGT      180
     C     XAANMN        SUB       180           XXADDM
     C                   Z-ADD     180           XXMIN             5 0
     C                   ELSE
     C                   Z-ADD     XAANMN        XXMIN
     C                   ENDIF
     C*
     C* PER P. 8 THESE CPT-4 CODES DO NOT HAVE A TIME FACTOR
     C*
     C     DHPRCD        IFEQ      '99100'
     C     DHPRCD        OREQ      '99116'
     C     DHPRCD        OREQ      '99135'
     C     DHPRCD        OREQ      '99140'
     C     DHPRCD        OREQ      '01997'
     C                   Z-ADD     *ZEROS        XXMIN
     C                   Z-ADD     *ZEROS        XXADDM
     C                   ENDIF
     C*
     C*  1.0 Unit for each 15 minutes or significant fraction
     C     XXMIN         DIV(H)    15            XXUNIT            5 0
     C     SXANAM        ADD       XXUNIT        SXANAM
     C*
     C*  1.0 Unit for each 10 minutes (OVER 3 HOURS)
     C     XXADDM        DIV(H)    10            XXUNIT
     C     SXANAM        ADD       XXUNIT        SXANAM
     C*
     C** Total Anesthesia Value * Conversion Factor = Fee
     C     SXANAM        MULT(H)   XXANES        SXMRA
     C*
     C* ===============================================================
     C* ===============================================================
     C* PER P. 5 ANESTHESIA
     C* When multiple surgical procedures are performed during a
     C* single anesthetic administration allow the unit value with
     C* the highest unit value.
     C                   Z-ADD     *ZEROS        COUNT
     C     CAKYA2        CHAIN     PCA0A                              60
     C*
     C     *IN60         DOWEQ     '0'
     C     XACPT4        IFGE      '00100'
     C     XACPT4        ANDLE     '01999'
     C     XADSEQ        IFNE      CADSEQ
     C     XATOTP        ANDGT     *ZEROS
     C*
     C                   MOVE      XACPT4        XXCPT4
     C                   MOVE      XAFDAT        XXEFDT
     C                   Z-ADD     *ZEROS        XXBAS             9 2
     C*
     C     PRKY          SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         DOWEQ     '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        IFEQ      'A'
     C                   Z-ADD     PRANAM        XXBAS
     C* Per P. 6 Anesthesia - Physical status modifiers
     C     XAPMOD        IFEQ      'P1'
     C     XAPMOD        OREQ      'P2'
     C     XAPMOD        OREQ      'P3'
     C     XAPMOD        OREQ      'P4'
     C     XAPMOD        OREQ      'P5'
     C     XAPMOD        OREQ      'P6'
     C*
     C                   Z-ADD     1             X
     C     XAPMOD        LOOKUP    ANEPM1(X)                              60
     C     *IN60         IFEQ      '1'
     C     ANEPM2(X)     ADD       XXBAS         XXBAS
     C                   END                                                    END 1 *IN60
     C                   END                                                    END EQ 'P1'
     C*
     C* GET TIME FACTOR FOR OTHER ANES PROCEDURE
     C                   Z-ADD     *ZEROS        XXADDM            5 0
     C     DHUNTS        IFGT      180
     C     DHUNTS        SUB       180           XXADDM
     C                   Z-ADD     180           XXMIN             5 0
     C                   ELSE
     C                   Z-ADD     DHUNTS        XXMIN
     C                   ENDIF
     C*
     C* PER P. 8 THESE CPT-4 CODES DO NOT HAVE A TIME FACTOR
     C*
     C     XACPT4        IFEQ      '99100'
     C     XACPT4        OREQ      '99116'
     C     XACPT4        OREQ      '99135'
     C     XACPT4        OREQ      '99140'
     C     XACPT4        OREQ      '01997'
     C                   Z-ADD     *ZEROS        XXMIN
     C                   Z-ADD     *ZEROS        XXADDM
     C                   ENDIF
     C*
     C*  1.0 Unit for each 15 minutes or significant fraction
     C     XXMIN         DIV(H)    15            XXUNIT            5 0
     C     XXBAS         ADD       XXUNIT        XXBAS
     C*
     C*  1.0 Unit for each 10 minutes (OVER 3 HOURS)
     C     XXADDM        DIV(H)    10            XXUNIT
     C     XXBAS         ADD       XXUNIT        XXBAS
     C*
     C     XXBAS         IFGE      SXANAM
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '54'          QQC(Q)
     C*** Err Msg 54: "Exceeds number of procedures allowed
     C***              in time period"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '204'         XPEOMB
     C*** EOMB 204: "Exceeds number of allowable procedures
     C*              in reimbursement manual".
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C                   ENDIF                                                  END XXBAS
     C                   ENDIF                                                  END DSEQ
     C*
     C                   READP     PPR01                                  54
     C                   ENDDO                                                  END DOW 54
     C*
     C                   END                                                    END NE DSEQ ...
     C                   END                                                    END NE CPT4 ...
     C     CAKYA2        READE     PCA0A                                  60
     C*
     C                   END                                                    END *IN60
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
     *** LOAD FOLLOW UP ARRAYS SUBROUTINE                      L O D F U *
     *********************************************************************
     C     LODFU         BEGSR
     C*
     C*** LOAD ARRAYS WITH FOLLOW UP DAYS
     C*** THE FOLLOWING WILL COMPUTE THE FOLLOW UP DAYS FOR ALL
     C*** CPT4 SERVICES FOR THIS CASE WHICH HAVE A VALUE IN FUD.
     C*
     C                   CLEAR                   FUF
     C                   CLEAR                   FUT
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
     C*
     C                   Z-ADD     XAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C                   Z-ADD     PRFOLU        SRFUTR            5 0
     C*
     C                   MOVE      ' '           SRRTCD            1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C*
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLDDAT            7 0
     C*
     C                   Z-ADD     XAFDAT        FUF(U)
     C                   Z-ADD     HLDDAT        FUT(U)
     C                   ADD       1             U
     C*
     C                   END                                                    END *IN61
     C     CAKYA1        READE     PCA0A                                  60
     C                   END
     C*
     C     LODFUE        ENDSR
     *********************************************************************
     C*  Medical                    Codes: 90000 - 99199         M E D I *
     *********************************************************************
     C     MEDI          BEGSR
     C*
      *  CONVERSION FACTOR FOR MEDICINE
      *
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97799'
      *
     C     DHPRCD        ORGE      '98925'
     C     DHPRCD        ANDLE     '98929'
      *
     C     DHPRCD        ORGE      '98940'
     C     DHPRCD        ANDLE     '98943'
      *
     C     DHPRCD        ORGE      '99000'
     C     DHPRCD        ANDLE     '99199'
      *
      * SPECIALTY CODE
     C     XXSPEC        MULT      SXMRA         SXMRA
      *
     C                   ELSE
      * MEDICINE
     C     XXMEDI        MULT      SXMRA         SXMRA
     C                   ENDIF
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '24'
     C     XPPMOD        ANDNE     '25'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '27'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '53'
     C     XPPMOD        ANDNE     '55'
     C     XPPMOD        ANDNE     '56'
     C     XPPMOD        ANDNE     '59'
     C     XPPMOD        ANDNE     '76'
     C     XPPMOD        ANDNE     '77'
     C     XPPMOD        ANDNE     '78'
     C     XPPMOD        ANDNE     '79'
     C     XPPMOD        ANDNE     '90'
     C     XPPMOD        ANDNE     '99'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      '201'         XPEOMB
     C* EOMB 201:"Service is not reimburseable under the
     C*            worker's compensation program".
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END                                                    END EQ 22
     C*
     C                   END                                                    END NE *BLANK
     C*
     C*==============================================================
     C***  PER P. 213 PHYSICAL MEDICINE
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97999'
     C     SVSPCD        ANDNE     'PHY'
     C     SVSPCD        ANDNE     'MD '
     C     SVSPCD        ANDNE     'PHT'
     C     SVSPCD        ANDNE     'OCT'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BP'          QQC(Q)
     C*** Err Msg BP: "Specialty code and level of service don't
     C***              match, please verify"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      '209'         XPEOMB
     C* EOMB 209:"Invalid procedure based on provider's specialty".
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END
     C* ===============================================================
     C* PER P. 5  Anesthesia Ground Rules
     C* Basic unit value:  The basic unit Value includes usual preoperative
     C* and post-operative visits, the administration of fluids and/or n
     C* blood incident to the anesthesia care.
     C*
     C     DHPRCD        IFGE      '93000'
     C     DHPRCD        ANDLE     '94799'
     C                   MOVEL     '00100'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '00100'
     C     XRCPT4        ANDLE     '01999'
     C     XRTOTP        ANDGT     *ZEROS
     C     XRDSEQ        ANDNE     CADSEQ
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '29'          QQC(Q)
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END NE CPT4 ...
     C     CAKY9A        READE     PCA09                                  60
     C                   END
     C*
     C                   END                                                    END *IN60
     C*
     C* ===============================================================
     C*
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
     C     XRCPT4        IFGE      '10040'                                      SURG CODES
     C     XRCPT4        ANDLE     '69979'
     C     XRCPT4        OREQ      '76000'
     C     XRCPT4        OREQ      '89350'
     C     XRCPT4        OREQ      '91000'
     C     XRCPT4        OREQ      '91055'
     C     XRCPT4        OREQ      '92020'
     C     XRCPT4        OREQ      '92325'
     C     XRCPT4        OREQ      '92511'
     C     XRCPT4        OREQ      '93561'
     C     XRCPT4        OREQ      '94150'
     C     XRCPT4        OREQ      '94250'
     C     XRCPT4        OREQ      '94690'
     C     XRCPT4        OREQ      '94762'
     C     XRCPT4        OREQ      '95829'
     C     XRCPT4        OREQ      '95831'
     C     XRCPT4        OREQ      '95851'
     C     XRCPT4        OREQ      '99195'
     C     XRCPT4        IFEQ      DHPRCD
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRCPT4        ORNE      DHPRCD
     C     XRTOTP        IFGT      0
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'CX'          QQC(Q)
     C* Err Msg CX: "Possible unbundling of a total proc,
     C*              separate procedure coded W/SURG".
     C*
     C                   LEAVE
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END XRCPT4
     C                   END                                                    END XRCPT4
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END DOW *IN60
     C*
     C                   END                                                    END *IN60 EQ 1
     C*
     C* ===============================================================
     C* PER P. 233  PHYSICAL MEDICINE
     C* When health care provider renders treatment in a patient's home
     C* increase the Maximum Allowable Fee by 50%.  An explanation
     C* substantiating the need for home therapy shall be submitted.
     C*
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97799'
     C*
     C     DHPOS         IFEQ      '12'
     C     DHPOS         OREQ      '4'
     C                   MULT      1.5           SXMRA
     C                   ENDIF                                                  IF POP = 12,4
     C                   ENDIF                                                  IF GE 97010
     C* ===================================================================
     C*** A MAJOR CODE MUST BE PRESENT ON SAME DAY FOR REIMBURSEMENT
     C***
     C     DHPRCD        IFEQ      '97221'
     C     DHPRCD        OREQ      '97241'
     C     DHPRCD        OREQ      '97501'
     C     DHPRCD        OREQ      '97521'
     C     DHPRCD        OREQ      '97531'
     C     DHPRCD        OREQ      '97541'
     C     DHPRCD        OREQ      '97546'
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
     C     DHPRCD        IFEQ      '97546'
     C                   MOVE      '97545'       XXCPT4
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
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '16'          QQC(Q)
     C* Err Msg 16: "A major code must be present to pay this item".
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '208'         XPEOMB
     C*** EOMB 208: "A major code must be present to pay this item"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END                                                    END 1 *IN60
     C                   END                                                    END DHPRCD 97221
     C*
     C* ===============================================================
     C* IF XXPASS = 1ST TIME DON'T PAY, ONLY DO THIS RANGE OF CODE ONCE
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97799'
     C     XXPASS        IFEQ      '1'
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   ENDIF                                                  IF EQ 1
     C                   ENDIF                                                  IF DHPRCD
     C* ===================================================================
     C* PER P. 233  PHYSICAL MEDICINE
     C* Treatment beyond 21 visits must be authorized by the self-insured
     C* employer, the insurance carrier, or the Director of Workers
     C* Compensation, unless previous authorization was received for larger #.
     C*
     C                   Z-ADD     *ZEROS        CNT               2 0
     C                   CLEAR                   PT
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97799'
     C                   MOVEL     '97010'       XXCPT4
     C     CAKYC         SETLL     PCA0C                              60
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        IFGE      '97010'
     C     XCCPT4        ANDLE     '97799'
     C     XCDSEQ        IFNE      CADSEQ
     C     XCTOTP        ANDGT     *ZEROS
     C*
     C     CNT           IFLE      21
     C                   Z-ADD     1             X
     C     XCFDAT        LOOKUP    PT(X)                                  61
     C     *IN61         IFEQ      *OFF
     C                   ADD       1             CNT
     C                   MOVEL     XCFDAT        PT(CNT)
     C                   END                                                    END *IN61   ...
     C                   END                                                    END LE 21   ...
     C                   ENDIF                                                  END CADSEQ
     C                   ENDIF                                                  END 97010
     C     CAKYC2        READE     PCA0C                                  60
     C                   ENDDO                                                  END *IN60   ...
     C*
     C     CNT           IFGE      21
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FD'          QQC(Q)
     C* Err Msg FD: "21 Visits paid, Authorization required."
     C*
     C                   END                                                    END COUNT/CHk
     C*
     C                   ENDIF                                                  IF GE 97010
     C* =================================================================
     C* PER P. 217  OSTEOPATHIC MANIPULATIVE TREATMENT
     C* NOTE: Body regions are specified by the CPT4 code used.
     C* Per SH: Only one code from range 98925-98929 is allowed per date
     C* of service.                                                         #.
     C*
     C                   Z-ADD     *ZEROS        CNT               2 0
     C     DHPRCD        IFGE      '98925'
     C     DHPRCD        ANDLE     '98929'
     C                   MOVEL     '98925'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '98925'
     C     XRCPT4        ANDLE     '98929'
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRTOTP        ANDGT     *ZEROS
     C     SXMRA         ANDLT     XRTOTP
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '54'          QQC(Q)
     C*** Err Msg 54: "Exceeds number of procedures allowed
     C***              in time period"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '204'         XPEOMB
     C*** EOMB 204: "Exceeds number of allowable procedures
     C*              in reimbursement manual".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   ENDIF                                                  END DHPRCD
     C     CAKY9A        READE     PCA09                                  60
     C                   ENDDO                                                  END *IN60
     C                   ENDIF                                                  IF DHPRCD
     C* =================================================================
     C* PER P. 217  OSTEOPATHIC MANIPULATIVE TREATMENT
     C* NOTE: Body regions are specified by the CPT4 code used.
     C* Per SH: Only one code from range 98940-98943 is allowed per date
     C* of service.                                                         #.
     C*
     C                   Z-ADD     *ZEROS        CNT               2 0
     C     DHPRCD        IFGE      '98940'
     C     DHPRCD        ANDLE     '98943'
     C                   MOVEL     '98940'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '98940'
     C     XRCPT4        ANDLE     '98943'
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRTOTP        ANDGT     *ZEROS
     C     SXMRA         ANDLT     XRTOTP
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '54'          QQC(Q)
     C*** Err Msg 54: "Exceeds number of procedures allowed
     C***              in time period"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '204'         XPEOMB
     C*** EOMB 204: "Exceeds number of allowable procedures
     C*              in reimbursement manual".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   ENDIF                                                  END DHPRCD
     C     CAKY9A        READE     PCA09                                  60
     C                   ENDDO                                                  END *IN60
     C                   ENDIF                                                  IF DHPRCD
     C* =================================================================
     C* PER P. 18 SURGERY - Separate procedures.
     C* NOTE: If a separate proc is coded w/another SURG code,
     C*       display error message ONLY.
     C*
     C*                    Z-ADD1         R       50
     C*          DHPRCD    LOKUPSEPP,R                   60
     C*          *IN60     IFEQ '1'
     C*
     C*                    MOVE '10040'   XXCPT4
     C*          CAKY9     SETLLPCA09
     C*          CAKY9A    READEPCA09                    60
     C*          *IN60     DOWEQ'0'
     C*          XRCPT4    IFGE '10040'                    SURG CODES
     C*          XRCPT4    ANDLE'69979'
     C*          XRCPT4    IFEQ DHPRCD
     C*          XRDSEQ    ANDNECADSEQ
     C*          XRCPT4    ORNE DHPRCD
     C*          XRTOTP    IFGT 0
     C*                    Z-ADD1         Q
     C*          '00000'   LOKUPQQS,Q                    60
     C* N60                Z-ADD30        Q
     C*                    MOVELCADSEQ    QQS,Q
     C*                    MOVE 'CX'      QQC,Q
     C* Err Msg CX: "Possible unbundling of a total proc,
     C*              separate procedure coded W/SURG".
     C*
     C*                    LEAVE
     C*                    END                             END XRTOTP > 0
     C*                    END                             END XRCPT4
     C*                    END                             END XRCPT4
     C*          CAKY9A    READEPCA09                    60
     C*                    END                             END DOW *IN60
     C*
     C*                    END                             END *IN60 EQ 1
     C*
     C* PER P. 192 #11 PROFESSIONAL/TECHNICAL COMPONENTS
     C* Professional component = 40% of MRA.
     C* Technical component = 60% of MRA.
     C*
     C* Modifier -26
     C*
     C     XPPMOD        IFEQ      '26'
     C     SXMRA         MULT      .40           SXMRA                          ELSE PAY 40%
     C                   ENDIF                                                  END XPPMOD
     C*
     C*
     C* Modifier -27
     C     XPPMOD        IFEQ      '27'
     C     SXMRA         MULT      .60           SXMRA                          PAY 60%
     C                   END                                                    END PMOD EQ 27
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
     C                   Z-ADD     HLDSEQ        CADSEQ                         SAVE FIELD VALS
     C                   MOVE      HLFEIN        CAFEIN                         FOR RECORD WE
     C                   MOVE      HLMOD         CAMOD                          ARE PROCESSING
     C                   MOVE      HLPRTY        CAPRTY
     C                   Z-ADD     HLEOB#        CAEOB#
     C                   Z-ADD     HLCHK#        CACHK#
     C*   Reget record.
     C     CAKYZ         CHAIN     PCA01                              54
     C*
     C  N54              EXCEPT    ECA01
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
      *
      * EVAL (OFFI)
     C     XXEVAL        MULT      SXMRA         SXMRA
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* EVALUATION AND MANAGEMENT
     C* VALID MODIFIERS
     C     XPPMOD        IFNE      '21'
     C     XPPMOD        ANDNE     '24'
     C     XPPMOD        ANDNE     '25'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '78'
     C     XPPMOD        ANDNE     '79'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      '201'         XPEOMB
     C* EOMB 201:"Service is not reimburseable under the worker's
     C*           compensation program."
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END                                                    END EQ 21
     C*
     C                   END                                                    END NE *BLANK
     C*
     C* =================================================================
     C*
     C***  USING MODIFIERS WHICH REQUIRE 'DOP'
     C     XPPMOD        IFEQ      '21'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BQ'          QQC(Q)
     C*** Err Msg BQ: "Documentation is required for reimbursement"
     C                   END
     C*
     C* ==============================================================
     C*** A MAJOR CODE MUST BE PRESENT ON SAME DAY FOR REIMBURSEMENT
     C***
     C     DHPRCD        IFEQ      '99292'
     C     DHPRCD        OREQ      '99355'
     C     DHPRCD        OREQ      '99357'
     C     DHPRCD        OREQ      '99359'
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
     C     CAKY9         CHAIN     PCA09                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '16'          QQC(Q)
     C* Err Msg 16: "A major code must be present to pay this item".
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '208'         XPEOMB
     C*** EOMB 208: "A major code must be present to pay this item"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END                                                    END 1 *IN60
     C                   END                                                    END DHPRCD 99292
     C***
     C* ===============================================================
     C*** PER PG. 18: Follow-up care for theapeutic surgical procedures
     C*** includes all normal postoperative care, that care which is
     C*** usually part of the surgical service.
     C*
     C     DHPRCD        IFGE      '99211'
     C     DHPRCD        ANDLE     '99215'
     C*
     C                   Z-ADD     1             X
     C     X             DOWLE     500
     C     FUF(X)        ANDGT     *ZERO
     C*
     C     CAFDAT        IFGT      FUF(X)
     C     CAFDAT        ANDLE     FUT(X)
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   Z-ADD     CACHRG        XPTOTD
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure".
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '29'          QQC(Q)
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END
     C*
     C                   ADD       1             X
     C                   END
     C*
     C                   END
     C*
     C* ====================================================================
     C*** Per P. 20  SURGERY
     C*** When a starred procedure is carried out at the time of an
     C*** initial visit (new patient) and this procedure constitutes
     C*** the major service rendered during the initial visit,
     C*** Procedure Code 99025 is listed in lieu of the usual initial
     C*** as an additional service.
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99205'
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
     C     XRTOTP        ANDGT     *ZEROS
     C*
     C                   MOVE      XRCPT4        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKY          SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C     PRSUPP        ANDEQ     'Y'                                             STARRED PROC
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
     C*
     C*** BEG CHG *** 02/24/1999 **************************************
     C***********XXEVAL    MULT SXMRA     SXMRA
     C     XXSPEC        MULT      SXMRA         SXMRA
     C*** END CHG *** 02/24/1999 **************************************
     C*
     C                   Z-ADD     PRANAM        SXANAM
     C                   Z-ADD     PRPROF        SXPROF
     C                   MOVE      PRRPT         SXRPT
     C                   MOVE      PRSUPP        SXSUPP
     C                   MOVE      PRSURG        SXSURG
     C                   MOVE      PR92N1        SX92N1
     C                   MOVE      PR92N2        SX92N2
     C                   MOVE      PR92N3        SX92N3
     C                   MOVE      PR1A1         SX1A1
     C                   MOVE      PR1A2         SX1A2
     C                   MOVE      PR1A3         SX1A3
     C                   MOVE      PR1A4         SX1A4
     C                   MOVE      PR1A5         SX1A5
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'CY'          QQC(Q)
     C*** Err Msg CY: "Office visit, reimbursed at code 99025 amount'
     C                   MOVE      '207'         XPEOMB
     C*** EOMB 207: "Payment is made for code 99025 instead of listed
     C***            Office Visit"
     C*
     C                   ELSE
     C                   GOTO      AGAIN
     C                   END
     C                   END
     C*
     C                   END
     C* ====================================================================
     C*** Per P. 20  SURGERY
     C*** When a starred
     C*** procedure performed at the time of a follow-up (established
     C*** patient) visit and this procedure constitutes the major            for
     C*** service at that visit, the follow-up visit is usually not
     C*** added.
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
     C*
     C  N54XRCPT4        IFGE      '10020'
     C     XRCPT4        ANDLE     '69979'
     C     XRTOTP        ANDGT     *ZEROS
     C*
     C                   MOVE      XRCPT4        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C     PRSUPP        ANDEQ     'Y'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'CZ'          QQC(Q)
     C*** Err Msg CZ: "Followup visit included in the charge of a
     C***              starred procedure".
     C*
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure".
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
     C*================================================================
     C*  PER P. 245 Evaluation and Management
     C*  Office or other Outpatient Services for new patients.
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99205'
     C                   MOVEL     '99201'       XXCPT4
     C     CAKYA1        SETLL     PCA0A                              60
     C     CAKYA1        READE     PCA0A                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XACPT4        IFGE      '99201'
     C     XACPT4        ANDLE     '99205'
     C     XATOTP        IFGT      *ZEROS
     C     XADSEQ        ANDNE     CADSEQ
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '29'          QQC(Q)
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C*
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C                   END                                                    END XATOTP  ...
     C                   END                                                    END NE CPT4 ...
     C     CAKYA1        READE     PCA0A                                  60
     C                   END
     C*
     C                   END                                                    END *IN60
     C*
     C* ===============================================================
     C*================================================================
     C*  PER P. 17.3 Intermediate pre-operative visits and other services
     C*              by the surgeon.
     C*  Undermost circumstances, including ordinary referrals, the
     C*  immediate pre-operative visit in the hospital or elsewhere
     C*  that is necessary to examine the patient, or to and initiate
     c*  the treatment program is included in fee listed in surgical.
     C*
     C     DHPRCD        IFGE      '99211'
     C     DHPRCD        ANDLE     '99215'
     C                   MOVEL     '10040'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '10040'
     C     XRCPT4        ANDLE     '69979'
     C     XRTOTP        ANDGT     *ZEROS
     C     XRDSEQ        ANDNE     CADSEQ
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '29'          QQC(Q)
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C*
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '202'         XPEOMB
     C*** EOMB 202: "CHARGE EXCEEDS MAXIMUM ALLOWANCE IN THE REIMBURSMENT
     C***            MANUAL.".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C                   END                                                    END NE CPT4 ...
     C     CAKY9A        READE     PCA09                                  60
     C                   END
     C*
     C                   END                                                    END *IN60
     C*
     C* ===============================================================
     C*================================================================
     C*  Only one code from range: 99201 - 99285 is allowable per date
     C*  of service per provider.
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99285'
     C                   MOVEL     '99201'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '99201'
     C     XRCPT4        ANDLE     '99285'
     C     XRTOTP        IFGT      *ZEROS
     C     XRDSEQ        ANDNE     CADSEQ
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '29'          QQC(Q)
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C*
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C                   END                                                    END XATOTP  ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9A        READE     PCA09                                  60
     C                   END
     C*
     C                   END                                                    END *IN60
     C*
     C* ===============================================================
     C*   If modifier exists, execute the mod check subroutine
     C     XPPMOD        CASGT     *BLANK        PMOD
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
     C* ===============================================================
     C* PER P. 5 ANESTHESIA
     C* Payment for anesthesia includes all usual pre-operative and
     C* post-operative visits, the anesthesia care during the proc...
     C* NOTE: THIS CODE APPLIES TO ALL SUBROUTINES
     C*
     C                   MOVE      '00100'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '00100'
     C     XRCPT4        ANDLE     '01999'
     C     XRTOTP        IFGT      *ZEROS
     C     XRDSEQ        ANDNE     CADSEQ
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '29'          QQC(Q)
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C*
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OTHEE
     C                   END                                                    END XRTOTP  ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9A        READE     PCA09                                  60
     C  N60XRCPT4        IFGT      '01999'
     C                   LEAVE
     C                   END
     C*
     C                   END                                                    END *IN60
     C* ===============================================================
     C** PER P. 275
     C** Reimbursement for ambulance services will be limited to
     C** emergency medical service's usual and customary charge
     C** less 10%.
     C**
     C     DHPRCD        IFEQ      'AMBUL'
     C     DHPRCD        OREQ      'NHICF'
     C     CACHRG        MULT      .90           SXMRA
     C                   END                                                                     0
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     OTHEE         ENDSR
     *********************************************************************
     C** DENTISTRY                  Codes: D0000 - D9999         D E N T *
     *********************************************************************
     C     DENT          BEGSR
     C*
      *
      * DENT
     C     XXDENT        MULT      SXMRA         SXMRA
     *********************************************************************
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '50'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '53'
     C     XPPMOD        ANDNE     '55'
     C     XPPMOD        ANDNE     '56'
     C     XPPMOD        ANDNE     '60'
     C     XPPMOD        ANDNE     '99'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      '201'         XPEOMB
     C* EOMB 201:"Service is not reimburseable under the
     C*            worker's compensation program".
     C                   EXSR      UPDAT
     C                   GOTO      DENTE
     C*
     C                   END                                                    END EQ 22
     C*
     C                   END                                                    END NE *BLANK
     C*
     C     SVSPCD        IFNE      'DDS'
     C     SVSPCD        ANDNE     'DMD'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BP'          QQC(Q)
     C*** Err Msg BP: "Specialty code and level of service don't
     C***              match, please verify"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      '209'         XPEOMB
     C* EOMB 209:"Invalid procedure based on provider's specialty".
     C                   EXSR      UPDAT
     C                   GOTO      DENTE
     C*
     C                   END                                                    END SVSPCD
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     DENTE         ENDSR
     *********************************************************************
     C** VOCATIONAL REHABILITATION  Codes: QRPPS, QRPNP   R E H        B *
     *********************************************************************
     C     REHB          BEGSR
     C     XXPASS        IFEQ      '1'
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   EXSR      UPDAT
     C                   GOTO      REHBE
     C                   ENDIF                                                  XXPASS = 1
     C* PER P. 287
     C*  VOCATIONAL REHABILITATION SERVICES SECTION.
     C*  QRPNP- FOUR HOURS PER DATE OF SERVICE
     C*  ONE UNIT = ONE HOUR, $63.00 PER HOUR. $1,260.00 LIMIT.
     C*  CNT1 HOLDS TOTAL FOR QRPNP,CNT2 HOLDS TOTAL PAID FOR QRPPS
     C*  CNT HOLDS PAID PER DAY FOR QRPNP, CNT3 HOLDS BOTH TOTALS
     C*
     C                   Z-ADD     *ZEROS        CNT
     C                   Z-ADD     *ZEROS        CNT1              8 2
     C                   Z-ADD     *ZEROS        CNT2              8 2
     C                   Z-ADD     *ZEROS        CNT3              8 2
     C                   MOVEL     'QRPNP'       XXCPT4
     C*
     C     CAKYC         SETLL     PCA0C                              60
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        IFEQ      'QRPNP'
     C     XCTOTP        ANDGT     *ZEROS
     C     XCDSEQ        ANDNE     CADSEQ
     C     XCFDAT        IFEQ      CAFDAT
     C                   ADD       1             CNT
     C                   ADD       XCTOTP        CNT1
     C                   ELSE
     C                   ADD       XCTOTP        CNT1
     C                   ENDIF                                                  END EFDAT ARE EQUAL
     C                   ENDIF                                                  END QRPNP
     C     XCCPT4        IFEQ      'QRPPS'
     C     XCTOTP        ANDGT     *ZEROS
     C     XCDSEQ        ANDNE     CADSEQ
     C                   ADD       XCTOTP        CNT2
     C                   ENDIF                                                  END QRPPS
     C     CAKYC2        READE     PCA0C                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C*  FIRST LIMITED TO 4 HOURS PER DAY THEN CHECK IF EXCEEDS
     C*  20 HOURS OR $1,260.00 PER LIFE TIME AT $60.00 PER HOUR.
     C*
     C     DHPRCD        IFEQ      'QRPNP'
     C     CNT           IFGE      4
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FC'          QQC(Q)
     C*** Err Msg FC: "limited to four hours per date of service."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      '204'         XPEOMB
     C*** EOMB 204: "Exceeds number of allowable procedures
     C                   EXSR      UPDAT
     C                   GOTO      REHBE
     C                   ENDIF                                                  END GT 4
     C                   ENDIF                                                  END QRPNP
     C*
     C     CNT1          ADD       CNT2          CNT3                           HOLDS ACCUMULATED TO
     C*
     C* PER P. 287
     C*  VOCATIONAL REHABILITATION SERVICES SECTION.
     C*  QRPPS- QUALIFIED REHAB PROFESSIONAL PROFESSIONAL SERVICES
     C*  ONE UNIT = ONE HOUR, $63.00 PER HOUR. $3,500.00 LIMIT.
     C*
     C     CNT3          IFGT      3500.00
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EZ'          QQC(Q)
     C*** Err Msg EZ: "The payment amount of this line item exceeds
     C***              $3,500.00."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      '204'         XPEOMB
     C*** EOMB 204: "Exceeds number of allowable procedures
     C*              in reimbursement manual".
     C                   EXSR      UPDAT
     C                   GOTO      REHBE
     C                   ENDIF                                                  END CNT3
     C*
     C*  CHECK IF EXCEEDS 20 HOURS OR $1,260.00
     C     DHPRCD        IFEQ      'QRPNP'
     C     CNT1          IFGE      1260.00
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FA'          QQC(Q)
     C*** Err Msg FA: "The payment amount of this line item exceeds
     C***              $1,260.00."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      '204'         XPEOMB
     C*** EOMB 204: "Exceeds number of allowable procedures
     C                   EXSR      UPDAT
     C                   GOTO      REHBE
     C                   ENDIF                                                  END GT 1260.00
     C                   ENDIF                                                  END QRPNP
     C*
     C* Calculate Payment
     C* PAYMENT FOR EACH CODE IS $63.00 PER HOUR
     C                   Z-ADD     63.00         SXMRA
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     REHBE         ENDSR
     *********************************************************************
     C** DEPOSITIONS/TESTIMONY Codes: DEPOS, IMESE, MEDRC, CNCDT         *
     *********************************************************************
     C     DEPO          BEGSR
     C* PER P. 273
     C*  DEPOSITIONS/TESTIMONY & REPRODUCTION OF MEDICAL RECORDS SECT.
     C*  DEPOS- IN DETERMINING THE DOLLAR PRICE OF MEDICALLY RELATED
     C*  DEPOSITIONS OR TERTIMONY RENDERED ON BEHALF OF EMPLOYEES REC.
     C*  BENEFITS UNDER THE KANSAS W.C. ACT. $5.00 PER MINUTE IS PAID.
     C*
     C     DHPRCD        IFEQ      'DEPOS'
     C     DHUNTS        MULT      SXANAM        SXMRA
     C                   ENDIF                                                   DEPOS
     C*
     C*  IMESE- IN DETERMINING THE DOLLAR PRICE OF ANY NECESSARY IME'S
     C*  AND OTHER SPECIAL EXAMINATIONS AND/OR REPORTS RENDERED ON BEHALF
     C*  OF EMPLOYEES RECEIVING BENEFITS UNDER THE KANSAS W.C. ACT
     C*  PAY $5.00 PER MINUTE, THEN ROUND UP.
     C*
     C     DHPRCD        IFEQ      'IMESE'
     C     300.00        DIV       60            COST              8 6
     C     DHUNTS        MULT(H)   COST          SXMRA
     C                   ENDIF                                                   IMESE
     C*
     C*  MEDRC- Reimbursement for the reproduction of an employee's
     C*  medical record is to be at teh health care providers usual andLF
     C*  customary charge not to exceed, however, the following.
     C*  1-10 $15.00, 11-50 $25.00, above 50 $25.00 + $.35 per page.
     C*
     C     DHPRCD        IFEQ      'MEDRC'
     C     DHUNTS        IFGE      1
     C     DHUNTS        ANDLE     10
     C                   Z-ADD     15.00         SXMRA
     C                   ELSE
     C     DHUNTS        IFGE      11
     C     DHUNTS        ANDLE     50
     C                   Z-ADD     25.00         SXMRA
     C                   ELSE
     C     DHUNTS        IFGT      50
     C                   Z-ADD     *ZERO         UNTOV             3 0
     C     DHUNTS        SUB       50            UNTOV
     C                   Z-ADD     *ZEROS        SXMRA
     C     UNTOV         MULT      .35           SXMRA
     C                   ADD       25.00         SXMRA
     C                   ENDIF                                                   >50
     C                   ENDIF                                                   11- 50
     C                   ENDIF                                                   1-10
     C                   ENDIF                                                   MEDRC
     C*
     C*  CNCDT- Whenever a deposition/testimony is to be canceled, more
     C*  than two working days notice is required to avoid a charge.
     C*  if notice is less than two working days, a $150.00 allowed charge
     C*
     C     DHPRCD        IFEQ      'CNCDT'
     C                   Z-ADD     150.00        SXMRA
     C                   ENDIF                                                   CNCDT
     C*
     C*
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     DEPOS         ENDSR
     *********************************************************************
     C** Pathology                  Codes: 82009 - 89399         P A T H *
     *********************************************************************
     C     PATH          BEGSR
     C*
      *
      * PATH
     C     XXPATH        MULT      SXMRA         SXMRA
     C*
     C* ===============================================================
     C* VALID MODIFIERS ARE: 22, 26, 27, 32, 52, 90
     C*
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per P. 170  MODIFIERS
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '27'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '53'
     C     XPPMOD        ANDNE     '59'
     C     XPPMOD        ANDNE     '90'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      '211'         XPEOMB
     C* EOMB 211:"These services not reimburseable under the
     C*           Fee Schedule".
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
     C                   END                                                    END EQ 22
     C*
     C                   END                                                    END NE *BLANK
     C*
     C* ===============================================================
     C* PER P. 178  REVISION FOR ANATOMIC PATHOLOGY PAY 80/20 NOT 60/40
     C* PER P. 169  Pathology Maximum Fees
     C* The maximum allowable fee includes the professional component
     C* (PC) plus the technical component(TC). 26 Professional componen
     C* (40% of the Maximum Allowable Fee).
     C* Modifier -26
     C*
     C     XPPMOD        IFEQ      '26'
     C     DHPRCD        IFGE      '88000'
     C     DHPRCD        ANDLE     '88399'
     C                   MULT      .8            SXMRA
     C                   ELSE
     C     SXMRA         MULT      .6            SXMRA                          ELSE PAY 60%
     C                   ENDIF                                                  END DHPRCD
     C                   ENDIF                                                  END XPPMOD
     C*
     C*
     C* ===============================================================
     C*
     C*
     C* PER P. 169  Pathology Maximum Fees
     C* The maximum allowable fee includes the professional component
     C* (PC) plus the technical component(TC). 27 Technical component
     C* (60% of the Maximum Allowable Fee).
     C* Modifier -27
     C     XPPMOD        IFEQ      '27'
     C     DHPRCD        IFGE      '88000'
     C     DHPRCD        ANDLE     '88399'
     C                   MULT      .2            SXMRA
     C                   ELSE
     C*
     C     SXMRA         MULT      .4            SXMRA                          PAY 60%
     C*
     C                   END                                                    END DHPRCD
     C                   END                                                    END PMOD EQ 27
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
     C                   MOVEL     XPPMOD        TABKEY
     C*
     C     PMKY          CHAIN     PPM01                              64
     C*
     C     PMPERC        IFNE      *ZERO
     C     PMPERC        DIV(H)    100           XXPERC            5 2
     C                   END
     C*
     C*=====================================================================
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '76'          QQC(Q)
     C*** Err Msg 76: "CPT modifier used with this charge.  Verify use
     C***              and payment."
     C*
     C* ===============================================================
     C* Modifier -26
     C     XPPMOD        IFEQ      '26'
     C*
     C     SXMRA         MULT      .40           SXMRA
     C*
     C                   END                                                    END PMOD EQ 26
     C*
     C*
     C* ===============================================================
     C* Modifier -27
     C     XPPMOD        IFEQ      '27'
     C*
     C     SXMRA         MULT      .60           SXMRA
     C*
     C                   END                                                    END PMOD EQ 27
     C*
     C* ===============================================================
     C*
     C     PMODE         ENDSR
     *********************************************************************
     C** Radiology                  Codes: 70010 - 79999         R A D I *
     *********************************************************************
     C     RADI          BEGSR
     C*
      * RADI
     C     XXRADI        MULT      SXMRA         SXMRA
     C*
     C* ===============================================================
     C* VALID MODIFIERS ARE: 26, 27
     C*
     C* Procedure Codes with modifiers:
     C*----------------------------------------------------------------
     C* Per P. 150 - 152 RADIOLOGY
     C     XPPMOD        IFNE      *BLANKS
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '27'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '53'
     C     XPPMOD        ANDNE     '58'
     C     XPPMOD        ANDNE     '59'
     C     XPPMOD        ANDNE     '62'
     C     XPPMOD        ANDNE     '66'
     C     XPPMOD        ANDNE     '76'
     C     XPPMOD        ANDNE     '77'
     C     XPPMOD        ANDNE     '78'
     C     XPPMOD        ANDNE     '79'
     C     XPPMOD        ANDNE     '80'
     C     XPPMOD        ANDNE     '90'
     C     XPPMOD        ANDNE     '99'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      '211'         XPEOMB
     C* EOMB 211:"These services not reimburseable under the
     C*           Fee Schedule".
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
     C                   END                                                    END NE 26
     C                   END                                                    END XPPMOD
     C*
     C* ===============================================================
     C*
     C* Modifier -26
     C* Mod 26 is billed allow 40% of the MRA
     C     XPPMOD        IFEQ      '26'
     C*
     C     SXMRA         MULT      .40           SXMRA
     C*
     C                   END                                                    END PMOD EQ 26
     C*
     C*
     C* ===============================================================
     C*
     C* Modifier -27
     C* Mod 27 is billed allow 60% of the MRA
     C     XPPMOD        IFEQ      '27'
     C*
     C     SXMRA         MULT      .60           SXMRA
     C*
     C                   END                                                    END PMOD EQ 27
     C*
     C* ====================================================================
     C* Write record to Workfile
     C* Code has passed all edits - Write to the FACAPKS Workfile
     C* Only load "current charges" into the FACAPKS Workfile
     C*
     C*
     C     DHPRCD        IFGE      '77261'
     C     DHPRCD        ANDLE     '77799'
     C     DHPRCD        ORGE      '79000'
     C     DHPRCD        ANDLE     '79999'
     C                   Z-ADD     SXMRA         CAMRA
     C                   Z-ADD     CACHRG        CATOTC
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   MOVE      'R'           CATYPE
     C                   WRITE     PCAWK1
     C                   ENDIF                                                  END DHPRCD
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
     C*
     C     RADI2         BEGSR
     C*
     C*----------------------------------------------------------------
     C*--- Reading the RADI Detail Charge Workfile --------------------
     C*
     C* ===============================================================
     C* Need to determine the number of Radiology procedures for day
     C*
     C                   Z-ADD     0             COUNT
     C     *LOVAL        SETLL     PCAWK3
     C                   READ      PCAWK3                                 60
     C     *IN60         DOWEQ     '0'
     C     P3CPT4        IFGE      '77261'
     C     P3CPT4        ANDLE     '77799'
     C     P3CPT4        ORGE      '79000'
     C     P3CPT4        ANDLE     '79999'
     C     COUNT         IFGE      1
     C                   Z-ADD     .50           P3BRP                          2nd: 50%
     C                   ADD       1             COUNT
     C                   ELSE
     C                   Z-ADD     1             P3BRP                          1ST: 100%
     C                   ADD       1             COUNT
     C                   END                                                    END
     C                   END                                                    END LE 69979
     C*
     C     P3BRP         IFEQ      .50
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     P3DSEQ        QQS(Q)
     C                   MOVE      'FB'          QQC(Q)
     C* Err Msg FB: "Review Operative Report/Multiple RADIOLOGY Rule".
     C*
     C                   END                                                    END P3BRP   # =  0
     C*
     C     P3MRA         MULT      P3BRP         P3MRA
     C*
     C     P3TOTC        COMP      P3MRA                              54        >>>>>
     C   54              Z-ADD     P3MRA         P3TOTA
     C   54              Z-ADD     *ZEROS        P3DIFF
     C  N54              Z-ADD     P3TOTC        P3TOTA
     C  N54P3MRA         SUB       P3TOTA        P3DIFF
     C     P2TOTC        SUB       P3TOTA        P3TOTD
     C                   Z-ADD     P3TOTA        P3TOTP
     C                   Z-ADD     P3MRA         P3BRTC
     C*
     C*  Using FACAPLA record, chain to FDCAP record to update
     C*  Only process the record if not an override type
     C                   Z-ADD     P3FDAT        PPFDAT
     C                   Z-ADD     P3FEIN        PPFEIN
     C                   Z-ADD     P3MOD         PPMOD
     C                   MOVE      P3CPT4        PPCPT4
     C                   Z-ADD     P3DSEQ        PPDSEQ
     C     CAKY8A        CHAIN     PCA08                              61
     C*
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     XRCSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              62
     C*
     C     CSOVRD        IFNE      'Y'
     C                   Z-ADD     P3TOTA        XRTOTA
     C                   Z-ADD     P3TOTD        XRTOTD
     C                   Z-ADD     P3TOTP        XRTOTP
     C                   Z-ADD     P3DIFF        XRDIFF
     C                   Z-ADD     P3BRTC        XRBRTC
     C                   MOVEL     P3PMOD        XRPMOD
     C*
     C     P3TOTD        IFGT      *ZERO
     C     P3EOMB        ANDEQ     *BLANK
     C     P3TOTP        ANDGT     *ZEROS
     C*
     C                   MOVE      '002'         P3EOMB
     C*** EOMB 002: "Charges exceed maximum allowance found in
     C***            reimbursement manual".
     C                   END
     C*
     C     P3TOTD        IFGT      *ZERO
     C     P3EOMB        ANDEQ     *BLANKS
     C                   MOVE      '201'         P3EOMB
     C*
     C*** EOMB 201: "Service is not reimburseable under the
     C***            worker's compensation program".
     C                   END                                                    END P3TOTD GT *ZERO
     C*
     C                   MOVE      P3EOMB        XREOMB
      *
     C     XREOMB        IFNE      *BLANKS
     C     XXPASS        ANDEQ     '2'
      *
     C                   MOVE      P3EOMB        XPEOMB
     C                   Z-ADD     P3DSEQ        KKDSEQ
     C                   EXSR      EOMB
      *
     C                   ENDIF
     C*
     C                   MOVE      Q1USER        XRUSER
     C                   MOVE      Q1PGM         XRPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XRDT
     C*
     C                   Z-ADD     DSTIME        XRTIME
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     SXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C  N61              UPDATE    PCA08
     C*
     C                   END                                                    END NE 'Y'
     C*
     C                   READ      PCAWK3                                 60
     C                   END                                                    END DOWEQ 0 *IN60
     C*
     C*
     C     RADI2E        ENDSR
     *********************************************************************
     *** Surgery                    Codes: 10000 - 69979         S U R G *
     *********************************************************************
     C     SURG          BEGSR
     C*
      * SURG
     C     XXSURG        MULT      SXMRA         SXMRA
     C*
     C* ==================================================================
     C     XPPMOD        IFNE      *BLANK
     C*
     C     XPPMOD        IFEQ      '20'
     C     XPPMOD        OREQ      '22'
     C     XPPMOD        OREQ      '26'
     C     XPPMOD        OREQ      '27'
     C     XPPMOD        OREQ      '32'
     C     XPPMOD        OREQ      '47'
     C     XPPMOD        OREQ      '50'
     C     XPPMOD        OREQ      '51'
     C     XPPMOD        OREQ      '52'
     C     XPPMOD        OREQ      '53'
     C     XPPMOD        OREQ      '54'
     C     XPPMOD        OREQ      '55'
     C     XPPMOD        OREQ      '56'
     C     XPPMOD        OREQ      '57'
     C     XPPMOD        OREQ      '58'
     C     XPPMOD        OREQ      '59'
     C     XPPMOD        OREQ      '62'
     C     XPPMOD        OREQ      '66'
     C     XPPMOD        OREQ      '76'
     C     XPPMOD        OREQ      '77'
     C     XPPMOD        OREQ      '78'
     C     XPPMOD        OREQ      '79'
     C     XPPMOD        OREQ      '80'
     C     XPPMOD        OREQ      '81'
     C     XPPMOD        OREQ      '82'
     C     XPPMOD        OREQ      '90'
     C     XPPMOD        OREQ      '99'
     C*
     C     XPPMOD        IFEQ      '47'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EJ'          QQC(Q)
     C* Err Msg EJ: "Verify anesthesia MAP, override and pay at 50%"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      '201'         XPEOMB
     C* EOMB 201: "Service is not reimburseable under the
     C*            worker's compensation program".
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END                                                    END XPPMOD=47
     C*
     C*
     C     XPPMOD        IFEQ      '66'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '66'          QQC(Q)
     C* Err Msg 66: "Item is surgical team mod-66,
     C*             Refer to fee schedule for reimbursement"
     C                   END                                                    END XPPMOD=66
     C*
     C*
     C     XPPMOD        IFEQ      '80'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EN'          QQC(Q)
     C* Err Msg EN: "Verify assistant as MD"
     C                   END                                                    END XPPMOD=80
     C*
     C                   ELSE                                                   ELSE XPPMOD
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      '201'         XPEOMB
     C* EOMB 201: "Service is not reimburseable under the
     C*            worker's compensation program".
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END                                                    END XPPMOD=47
     C*
     C                   END                                                    END XPPMOD
     C* ===============================================================
     C* PER P. 17 SURGERY
     C* Package or global fee concept:   The maximum allowable fee for
     C* all surgical procedures includes the surgery, local infiltration
     C* digital block and/ or topical anesthesia when used.
     C*
     C     DHPRCD        IFGE      '64400'
     C     DHPRCD        ANDLE     '64530'
     C                   MOVEL     '10040'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '10040'
     C     XRCPT4        ANDLE     '69979'
     C     XRTOTP        IFGT      *ZEROS
     C     XRDSEQ        ANDNE     CADSEQ
     C*
     C* SETLL AND READ THE FILE TESTING FOR AN '*' PROCEDURE.
     C*
     C                   MOVE      XRCPT4        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XRCPT4        ANDEQ     PRCPT4
     C     PRSUPP        ANDNE     'Y'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '29'          QQC(Q)
     C* Err Msg 29: "This procedure was previously paid under a
     C*              complete code."
     C*
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END *IN54   ...
     C                   END                                                    END XRTOTP  ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9A        READE     PCA09                                  60
     C                   END
     C*
     C                   END                                                    END *IN60
     C*
     C* ===============================================================
     C* PER. P. 17 Surgery - Muliple or Bilateral Procedures
     C* When multiple procedures, unrelated to the major procedure
     C* and adding significant time or complexity are provided at the
     C* same operative session, payment is for the procedure with the
     C* highest allowance, 150%.
     C*
     C                   Z-ADD     *ZEROS        COUNT
     C                   Z-ADD     *ZEROS        COUNT1            2 0          ADD 10/23/97
     C*
     C     DHPRCD        IFGE      '10040'
     C     DHPRCD        ANDLE     '69970'
     C     XPPMOD        ANDEQ     '50'
     C*
     C                   MOVEL     DHPRCD        XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9         READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *
     C     XRTOTP        IFGT      *ZEROS
     C     CADSEQ        ANDNE     XRDSEQ
     C     CAFDAT        ANDEQ     XRFDAT
     C     XRFLAG        ANDNE     'P'
     C     XRPMOD        IFEQ      '50'
     C                   ADD       1             COUNT1
     C                   ELSE
     C                   ADD       1             COUNT
     C                   ENDIF
     C                   ENDIF
     C*
     C     CAKY9         READE     PCA09                                  60
     C                   ENDDO
     C*
     C     COUNT1        IFEQ      0
     C     COUNT         ANDEQ     1
     C     SXMRA         MULT      .75           SXMRA
     C                   ENDIF
     C*
     C     COUNT1        IFGE      1
     C*
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C* Err Msg 03: Possible Duplicate claim exists for this item.
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is inclueded in the basic
     C***            allowance of another procedure".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END                                                    END CNTADD
     C*
     C     COUNT1        IFEQ      0
     C     COUNT         ANDEQ     0
     C                   Z-ADD     1             Q
     C     '0Z000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '16'          QQC(Q)
     C* Err Msg 16: "A major code must be present to pay this item'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is inclueded in the basic
     C***            allowance of another procedure".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   ENDIF                                                  ONLY ONE MOD
     C                   END                                                    END EQ '10040'
     C*
     C*
     C* ===============================================================
     C* PER P.18 Surgery
     C* Follow-up or Aftercase
     C* When an additional surgical procedure is performed during any
     C* follow-up care and is related to the prev performed but is
     C* not an intrinsic part of the latter, additional paid at 1/2 max
     C*
     C     DHPRCD        IFGE      '10040'
     C     DHPRCD        ANDLE     '69979'
     C* FIND DATE 30 DAYS PRIOR
     C                   Z-ADD     30            SRPST
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C*
     C                   MOVE      WKFDAT        SRDTE
     C*
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C*
     C                   MOVE      SRDTE         XXPAST            8 0
     C                   Z-ADD     XXPAST        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPAST
     C*
     C*
     C* FIND DATE 30 DAYS FUTURE OF PAST DATE
     C                   Z-ADD     30            SRFUT
     C                   Z-ADD     CAFDAT        SRDTI
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    WKFDAT
     C                   PARM                    SRFUT
     C                   PARM                    SRTERM
     C*
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFUTR            7 0
     C*
     C                   MOVE      '10040'       XXCPT4
     C                   MOVE      XXPAST        XYYMD
     C     CAKYA3        SETLL     PCA0A                              60
     C     CAKYA1        READE     PCA0A                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XAFDAT        IFGE      XXPAST
     C     XAFDAT        ANDLE     XXFUTR
     C     XAFDAT        ANDNE     CAFDAT
     C     XACPT4        IFGE      '10040'
     C     XACPT4        ANDLE     '69979'
     C     XATOTP        IFGT      *ZEROS
     C     XADSEQ        ANDNE     CADSEQ
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EX'          QQC(Q)
     C* Err Msg EX: "Previous surgery paid, RVW history before
     C*              paying at 50%."
     C*
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END XRTOTP  ...
     C                   END                                                    END NE CPT4 ...
     C                   END
     C     CAKYA1        READE     PCA0A                                  60
     C*
     C                   ENDDO                                                  END *IN60
     C                   END                                                    END DHPRCD
     C*
     C* ====================================================================
     C* PER P. 18 SURGERY - Separate or independent procedures.
     C* Some of the listed surgical procedures are commonly carried
     C* out as an integral part of a total service, and do not warrant
     C* a separate identification.
     C                   Z-ADD     1             R                 5 0
     C     DHPRCD        LOOKUP    SEPP(R)                                60
     C     *IN60         IFEQ      '1'
     C*
     C                   MOVE      '10040'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '10040'                                      SURG CODES
     C     XRCPT4        ANDLE     '69979'
     C     XRCPT4        IFEQ      DHPRCD
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRCPT4        ORNE      DHPRCD
     C     XRTOTP        IFGT      0
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'CX'          QQC(Q)
     C* Err Msg CX: "Possible unbundling of a total proc,
     C*              separate procedure coded W/SURG".
     C*
     C                   LEAVE
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END XRCPT4
     C                   END                                                    END XRCPT4
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END DOW *IN60
     C*
     C                   END                                                    END *IN60 EQ 1
     C* ===============================================================
     C*
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
     C     R             IFLT      90
     C                   ADD       1             R
     C                   GOTO      SURLOK
     C                   ELSE
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      '208'         XPEOMB
     C*** EOMB 208: "A major code must be present to pay this item"
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
     C                   MOVE      '208'         XPEOMB
     C*** EOMB 208: "A major code must be present to pay this item"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C                   END
     C*
     C* ====================================================================
     C*
     C* NOTE: ALL SURGERY PROCEDURES ARE WRITTEN TO THE WORKFILE
     C*       EXCEPT THOSE WITH MODIFIER -50
     C*       AND THOSE WHICH ARE IN THE NOCUT TABLE
     C*
     C     XPPMOD        IFNE      '50'
     C* ===============================================================
     C* Write record to Workfile
     C* Code has passed all edits - Write to the FACAPKS Workfile
     C* Only load "current charges" into the FACAPKS Workfile
     C*
     C* Exceptions are found in NOCT table and cannot be cut
     C*
     C                   Z-ADD     1             T
     C     DHPRCD        LOOKUP    NOCT(T)                                60
     C     *IN60         CABEQ     '1'           NOCUT
     C*
     C                   Z-ADD     SXMRA         CAMRA
     C                   Z-ADD     CACHRG        CATOTC
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   MOVE      'S'           CATYPE
     C                   WRITE     PCAWK1
     C*
     C                   END                                                    END XPPMOD EQ ' '42
     C*
     C     NOCUT         TAG
     C*
     C* =================================================================
     C* MODIFIERS WHICH CAUSE A CHANGE IN THE MAP
     C*
     C     XPPMOD        IFEQ      '80'
     C                   MULT      .25           SXMRA
     C                   ENDIF
     C     XPPMOD        IFEQ      '81'
     C                   MULT      .10           SXMRA
     C                   ENDIF
     C* ===============================================================
     C* PER P. 20 #21 PROFESSIONAL/TECHNICAL COMPONENTS
     C* Professional component = 60% of MRA.
     C* Technical component = 40% of MRA.
     C*
     C* Modifier -26
     C*
     C*
     C     XPPMOD        IFEQ      '26'
     C     SXMRA         MULT      .40           SXMRA                          ELSE PAY 40%
     C                   ENDIF                                                  END XPPMOD
     C*
     C*
     C* Modifier -27
     C     XPPMOD        IFEQ      '27'
     C     SXMRA         MULT      .60           SXMRA                          PAY 60%
     C                   END                                                    END PMOD EQ 27
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
     C                   CLEAR                   MOD
     C                   Z-ADD     *ZEROS        M                 5 0
     C     *LOVAL        SETLL     PCAWK2
     C                   READ      PCAWK2                                 60
     C     *IN60         IFEQ      '0'
     C* Read history to determine if a SURG code has already been paid
     C                   Z-ADD     P2FDAT        PPFDAT
     C                   Z-ADD     P2FEIN        PPFEIN
     C                   Z-ADD     P2MOD         PPMOD
     C                   MOVE      '10000'       PPCPT4
     C     CAKY9C        SETLL     PCA09
     C     CAKY9D        READE     PCA09                                  61
     C     *IN61         DOWEQ     '0'
     C     XRCPT4        IFLE      '69979'
     C     XRTOTP        IFNE      *ZEROS
     C     XREOB#        ANDNE     QQEOB#
     C     XRPART        IFEQ      'S1'
     C     XRPART        OREQ      ' '
     C                   ADD       1             COUNT
     C                   ELSE
     C     XRPART        IFEQ      'S2'
     C                   ADD       1             CNTB1
     C                   ELSE
     C     XRPART        IFEQ      'S3'
     C                   ADD       1             CNTB2
     C                   END                                                    END EQ 'S3'
     C                   END                                                    END EQ 'S2'
     C                   END                                                    END EQ 'S1'
     C                   END                                                    END NE *ZEROS
     C                   END                                                    END LE 69979
     C     CAKY9D        READE     PCA09                                  61
     C                   END                                                    END 0 *IN61
     C*
     C                   END                                                    END 0 *IN60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C*
     C* ===============================================================
     C*  MULTIPLE PROCEDURES:  100% of the MAR for the
     C*  primary procedure, (major procedure reflecting the
     C*  greatest value).
     C*  50% of the MAR for secondary or subsequent procedure(s)
     C*
     C* SURG: INCISION 1
     C*  Highest value 100%
     C*  after 1st      50%
     C*
     C     P2PART        IFEQ      'S1'
     C     P2PART        OREQ      ' '
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
     C                   ELSE                                                   ELSE EQ 'S1'
     C*
     C* ===============================================================
     C* SURG: INCISION 2
     C*  Highest value 100%
     C*  after 1st      50%
     C*
     C     P2PART        IFEQ      'S2'
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
     C                   ELSE                                                   ELSE EQ 'S2'
     C*
     C* SURG: INCISION 3
     C*  Highest value 100%
     C*  after 1st      50%
     C*
     C     P2PART        IFEQ      'S3'
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
     C                   END                                                    END EQ 'S3'
     C                   END                                                    END EQ 'S2'
     C                   END                                                    END EQ 'S1'
     C*
     C*
     C* PER P 19 SURGERY
     C* The surgical assistant will be paid 20% of the MAP amount for
     C* the initial procedure and 10% for subsequent procedures.
     C*
     C     P2PMOD        IFNE      *BLANKS
     C     P2PMOD        LOOKUP    MOD                                    60
     C     *IN60         IFEQ      '0'
     C                   ADD       1             M
     C                   MOVE      P2PMOD        MOD(M)
     C                   Z-ADD     1             P2BRP
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL     'N'           XXREDC            1            FLAG FOR DZ EOB
     C     P2BRP         IFEQ      .50
     C                   MOVEL     'Y'           XXREDC                         FLAG FOR DZ EOB
     C                   ENDIF
     C*
     C     P2PMOD        IFEQ      '80'
     C     P2BRP         IFEQ      1
     C                   Z-ADD     .25           XXBRP             4 3          1st: 25%
     C                   ENDIF
     C     P2BRP         IFEQ      .50
     C                   Z-ADD     .125          XXBRP             4 3          2nd: 12.5%
     C                   ENDIF
     C                   ENDIF
     C*
     C* (40% of the Maximum Allowable Fee).
     C* Modifier -26
     C*
     C     P2PMOD        IFEQ      '26'
     C     P2BRP         IFEQ      1
     C                   Z-ADD     .40           P2BRP                          ELSE PAY 40%
     C                   ENDIF
     C     P2BRP         IFEQ      .50
     C                   Z-ADD     .15           P2BRP                          2nd: 15%
     C                   ENDIF
     C                   ENDIF                                                  END XPPMOD
     C*
     C* (60% of the Maximum Allowable Fee).
     C* Modifier -27
     C*
     C     P2PMOD        IFEQ      '27'
     C     P2BRP         IFEQ      1
     C                   Z-ADD     .60           P2BRP                          ELSE PAY 60%
     C                   ENDIF
     C     P2BRP         IFEQ      .50
     C                   Z-ADD     .10           P2BRP                          2nd: 10%
     C                   ENDIF
     C                   ENDIF                                                  END XPPMOD
     C* =================================================================
     C*
     C* PER P 19 SURGERY
     C* Health Care Provider's Assistant
     C* A Health Care Providers Assistant with current certification
     C* assisting in surgery is to be identified.  Bill at 10%.
     C*
     C     P2PMOD        IFEQ      '81'
     C     P2BRP         IFEQ      1
     C                   Z-ADD     .10           P2BRP                          1st: 10%
     C                   ENDIF
     C     P2BRP         IFEQ      .50
     C                   Z-ADD     .05           P2BRP                          2nd: 05%
     C                   ENDIF
     C                   ENDIF
     C*
     C** NEED TO CHECK FOR MOD 80, P2BRP CAN'T CONTAIN .125 SO CREATED
     C*  XXBRP TO HOLD THE VALUE.
     C     P2PMOD        IFEQ      '80'
     C     P2MRA         MULT      XXBRP         P2MRA
     C                   ELSE
     C     P2MRA         MULT      P2BRP         P2MRA
     C                   ENDIF
     C*
     C     XXREDC        IFEQ      'Y'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     P2DSEQ        QQS(Q)
     C                   MOVE      'DZ'          QQC(Q)
     C* Err Msg DZ: "Review Operative Report/Multiple Surgical Rule".
     C*
     C                   END                                                    END X2BRP NE1
     C*
     C     P2TOTC        COMP      P2MRA                              54        >>>>>
     C   54              Z-ADD     P2MRA         P2TOTA
     C   54              Z-ADD     *ZEROS        P2DIFF
     C  N54              Z-ADD     P2TOTC        P2TOTA
     C  N54P2MRA         SUB       P2TOTA        P2DIFF
     C     P2TOTC        SUB       P2TOTA        P2TOTD
     C                   Z-ADD     P2TOTA        P2TOTP
     C                   Z-ADD     P2MRA         P2BRTC
     C*
     C*  Using FACAPLA record, chain to FDCAP record to update
     C*  Only process the record if not an override type
     C                   Z-ADD     P2FDAT        PPFDAT
     C                   Z-ADD     P2FEIN        PPFEIN
     C                   Z-ADD     P2MOD         PPMOD
     C                   MOVE      P2CPT4        PPCPT4
     C                   Z-ADD     P2DSEQ        PPDSEQ
     C     CAKY8A        CHAIN     PCA08                              61
     C*
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     XRCSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              62
     C*
     C     CSOVRD        IFNE      'Y'
     C                   Z-ADD     P2TOTA        XRTOTA
     C                   Z-ADD     P2TOTD        XRTOTD
     C                   Z-ADD     P2TOTP        XRTOTP
     C                   Z-ADD     P2DIFF        XRDIFF
     C                   Z-ADD     P2BRTC        XRBRTC
     C                   MOVEL     P2PMOD        XRPMOD
     C*
     C     P2TOTD        IFGT      *ZERO
     C     P2EOMB        ANDEQ     *BLANK
     C     P2TOTP        ANDGT     *ZEROS
     C*
     C                   MOVE      '002'         P2EOMB
     C*** EOMB 002: "Charges exceed maximum allowance found in
     C***            reimbursement manual".
     C                   END
     C*
     C     P2TOTD        IFGT      *ZERO
     C     P2EOMB        ANDEQ     *BLANKS
     C                   MOVE      '201'         P2EOMB
     C*
     C*** EOMB 201: "Service is not reimburseable under the
     C***            worker's compensation program".
     C                   END                                                    END P2TOTD GT *ZERO
     C*
     C                   MOVE      P2EOMB        XREOMB
      *
     C     XPEOMB        IFNE      *BLANKS
     C     XXPASS        ANDEQ     '2'
      *
     C                   MOVE      P2EOMB        XPEOMB
     C                   Z-ADD     P2DSEQ        KKDSEQ
     C                   EXSR      EOMB
      *
     C                   ENDIF
     C*
     C                   MOVE      Q1USER        XRUSER
     C                   MOVE      Q1PGM         XRPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XRDT
     C*
     C                   Z-ADD     DSTIME        XRTIME
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     SXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C  N61              UPDATE    PCA08
     C*
     C                   END                                                    END NE 'Y'
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
     C*
     C                   MOVE      '002'         XPEOMB
     C*** EOMB 002: "Charges exceed maximum allowance found in
     C***            reimbursement manual".
     C                   END
      *
     C     XPEOMB        IFNE      *BLANKS
     C     XXPASS        ANDEQ     '2'
      *
     C                   Z-ADD     SVDSEQ        KKDSEQ
     C                   EXSR      EOMB
      *
     C                   ENDIF
     C*
     C* ===============================================================
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM.
     C** REMOVED MODIFIER FROM DUPLICATE CLAIM EDIT
     C**
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     DHKY2         CHAIN     PDH03                              54
     C*
     C     *IN54         DOWEQ     *OFF
     C*
     C                   Z-ADD     DHDSEQ        KKDSEQ
     C*
     C     CAKYP2        CHAIN     PCA0P                              61
     C*
     C     *IN61         IFEQ      *OFF
     C*
     C     PXFDAT        ANDEQ     CAFDAT
     C     PXFEIN        ANDEQ     CAFEIN
     C     PXDSEQ        ANDNE     CADSEQ
     C     PXFLAG        ANDNE     'P'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     SVDSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C* Err Msg 03: Possible Duplicate claim exists for this item.
     C*
     C                   ENDIF
     C*
     C     DHKY2         READE     PDH03                                  54
     C*
     C                   ENDDO
     C*
     C*
     C* ===============================================================
     C** Only process the record if not an override type
     C*
     C     CAKY1A        CHAIN     PCA01                              54
     C     CSOVRD        IFNE      'Y'
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPTOTD        CAMRA
     C                   Z-ADD     XPTOTP        CAPAID
     C                   Z-ADD     XPBRTC        CABRTC
     C                   MOVE      XPPMOD        XPPMOD
     C                   MOVE      XPEOMB        CAEOMB
     C*
     C                   MOVE      Q1USER        CAUSER
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C*
     C                   Z-ADD     DSTIME        CATIME
     C*
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     SXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C  N54              UPDATE    PCA01
     C* If Fee Schedule non-reimburseable ... display error message
     C* XX25EM Flag is set at beginning of program where PRFSR eq 'N'
     C     XX25EM        IFEQ      '1'
     C                   MOVE      DHPRCD        XXCPT4
     C     CAKY6         CHAIN     PCA06                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 61
     C  N61              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '19'          QQC(Q)
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
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      *BLANKS       XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C* ===============================================================
     C*
     C     UPDAE         ENDSR
     *********************************************************************
     OPCA01     E            ECA01
     OPCA08     E            ECA08
     *********************************************************************
**  QCA QCMDEXC TABLE
DLTOVR FILE(FACALKS)
OVRDBF FILE(FACALKS)  TOFILE(QTEMP/FACALKS)
DLTOVR FILE(FACAPKS)
OVRDBF FILE(FACAPKS)  TOFILE(QTEMP/FACAPKS)
DLTOVR FILE(FACAL1KS)
OVRDBF FILE(FACAL1KS) TOFILE(QTEMP/FACAL1KS)
**  ANEPM1/ ANEPM2
P10
P20
P31
P42
P53
P60
**  SUR1/SUR2: SUR1 MUST HAVE SUR2 MINOR/MAJOR
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
1720117200                                                            F
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
91000
91055
92020
92060
92100
92325
92511
93561
94150
94250
94690
94762
95829
95831
95851
99195
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
17307
19001
19126
19340
19291
20690
20692
20974
20975
22103
22116
22145
22148
22216
22226
22230
22328
22585
22614
22632
22650
22820
22840
22842
22845
26125
26861
26863
27358
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
36218
36248
36522
37206
37208
38102
38746
38747
41114
43635
44121
44139
44950
44955
47001
49568
49905
58611
59412
59525
61106
61130
61609
61610
61611
61612
61712
61795
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
