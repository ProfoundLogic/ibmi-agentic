     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H* PROGRAM: F4070RSC
     H* DESCRIPTION: ADJUDICATION (SOUTH CAROLINA RULES - 05/01/95)
     H*   This program is called from F0070R (Claim Adjudication) and will
     H* perform the actual adjudication of the detail claim records writ-
     H* ten by F0070R.  This program was derived from the South Carolina
     H* State Fee Schedule Reimbursment Manual issued on 05/01/95.
     H* This Sub-program is intended to be used to calculate the amount
     H* that is due a Provider of services, by the Carrier of the Work comp
     H* insurance.  It should be used for any service provided on or after
     H* 05/01/95.
     H*****************************************************************
     H*  CREATED: 6/06/96   C:32   PROJECT: SC001
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
     H*        XXCRNA  - CERTIFIED REGISTERED NURSE
     H*        XXCRNS  - SUPERVISED CRNA
     H*****************************************************************
     H*  SPECIALTY CODES USED IN THIS PROGRAM:
     H*  ANE - ANESTHESIOLOGISTS
     H*  RNA - REGISTERED NURSE ANESTHESIOLOGIST
     H*  CHI - CHIROPRACTOR
     H*  DO  - DOCTOR OF OSTEOPATHY
     H*  MD  - MEDICAL DOCTOR
     H*  OST - OSTEOPATHY
     H*  PHT - PHYSICAL THERAPY
     H*  PHY - PHYSICIAN
     H*  PSC - PSYCHOLOGIST
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  08/09/02 C:062 PROJECT: FS244
     H*                 USE CAZIPA.
     H*
     H*  09/25/01 C:062 PROJECT: FSMNT
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.
     H*
     H*  09/07/99 C:79  Project: SC005
     H*                 Convert to work with provider bills
     H*
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
     H*****************************************************************
     H*  INDICATOR USAGE:
     H*  54    Use with Primary FDCAP file (i.e. DOWEQ, etc on reading charges)
     H*  60    General Use
     H*  61    General Use
     H*  62    General Use
     H*****************************************************************
     H*  SECTION                                  SUBR         CODES
     H*  Anesthesia                               ANES         00100-01999
     H*  Chiropratic Services                     CHIR         CH000-CH800
     H*  Medical/Physical                         MEDI         90000-99199
     H*  Office Visits                            OFFI         99201-99499
     H*  Pathology                                PATH         80002-89399
     H*  Radiology                                RADI         70010-79999
     H*  Surgery                                  SURG         10000-69979
     H*                                           SURG2        10000-69979
     H*****************************************************************
     F*  FDCAL7 is the Primary File Read (PCA01)
     FFDCALH    UF   E           K DISK
     F*
     FFACAPSC   O    E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK1)
     FFACALSC   IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK2)
     FFDCAL5    UF   E           K DISK
     F                                     RENAME(PCA01:PCA08)
     FFDCAL9    IF   E           K DISK
     F                                     RENAME(PCA01:PCA09)
     FFDCALA    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0A)
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0P)
     FFMCML1    IF   E           K DISK
     F*** BEGIN DEL *** 08/09/02 **************************************
     F**********FMGZL   IF  E           K        DISK
     F*** END   DEL *** 08/09/02 **************************************
     FFMPDL     IF   E           K DISK
     FFMPRLSC   IF   E           K DISK
     FFMRCL     IF   E           K DISK
     FFTABLSC   IF   E           K DISK
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
     D FUF             S              7  0 DIM(500)                             FOLLOW UP FRM DY
     D FUT             S              7  0 DIM(500)                             FOLLOW UP TO DAY
     D*
     D* Compile-time Arrays
     D QCA             S             55    DIM(4) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D RADS            S              5    DIM(135) CTDATA PERRCD(1)            RADI CPT4 COMP
     D SUR1            S              5    DIM(90) CTDATA PERRCD(1)             CUT/MAJOR-MIN
     D SUR2            S              5    DIM(90) ALT(SUR1)
     D SEPP            S              5    DIM(269) CTDATA PERRCD(1)            SEPARATE PROCEDURE
     D* Run-time Arrays
     D CPT4            S              5    DIM(30)                              CPT4 ARRAY
     D PRT             S              2    DIM(20)                              PART TBL
     D MDAT            S              6  0 DIM(30)                              DATE ARRAY
     D PDAT            S              6  0 DIM(30)                              DATE ARRAY
     D*
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
     I* Record Format for SURG2 Workfile FACALSC
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
     IPDH03
     I              DHCO#                       D1CO#
     I              DHSUB#                      D1SUB#
     I              DHFND                       D1FND
     I              DHFYR                       D1FYR
     I              DHCASE                      D1CASE
     I              DHSS#                       D1SS#
     I              DHACDT                      D1ACDT
     I              DHEOB#                      D1EOB#
     I              DHDSEQ                      D1DSEQ
     I              DHPOS                       D1POS
     I              DHTOS                       D1TOS
     I              DHPRCD                      D1PRCD
     I              DHPRDS                      D1PRDS
     I              DHMOD1                      D1MOD1
     I              DHMOD2                      D1MOD2
     I              DHMOD3                      D1MOD3
     I              DHMOD4                      D1MOD4
     I              DHDXR1                      D1DXR1
     I              DHDXR2                      D1DXR2
     I              DHDXR3                      D1DXR3
     I              DHDXR4                      D1DXR4
     I              DHUNTS                      D1UNTS
     I              DHAREA                      D1AREA
     I              DHPPLN                      D1PPLN
     I              DHSTAT                      D1STAT
     I              DHUSER                      D1USER
     I              DHPGM                       D1PGM
     I              DHTIME                      D1TIME
     I              DHDT                        D1DT
     I              DHIUSR                      D1IUSR
     I              DHIPGM                      D1IPGM
     I              DHITIM                      D1ITIM
     I              DHIDT                       D1IDT
     I              DHRCFL                      D1RCFL
     I              DHBPRC                      D1BPRC
     I              DHBPRD                      D1BPRD

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
     C     CAKYP         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    KKDSEQ
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
     C*== Initialization Section ======================================
     C*
     C                   MOVE      *ZEROS        QQS
     C                   MOVE      *ZEROS        QQC
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
     C     *LIKE         DEFINE    DHUNTS        SVUNTS
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
     C     *LIKE         DEFINE    CAPMOD        XXPMOD
     C     *LIKE         DEFINE    DHUNTS        XXUNIT
     C     *LIKE         DEFINE    DHUNTS        XXMIN
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
     C*
     C*** BEGIN CHANGE ********* 11/18/97 ********************************
     C*********  *LIKE     DEFN CAFDAT    WKFDAT     NEEDS TO BE 8 LONG
     C*** END   CHANGE ********* 11/18/97 ********************************
     C*
     C                   Z-ADD     022.00        XXANES            5 2           ANES Conversion
     C                   Z-ADD     018.00        XXCRNA            5 2           CERT ANES Conv
     C                   Z-ADD     011.00        XXCRNS            5 2           CERT ANES SUPR
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
     C                   Z-ADD     0             HLANMN            9 0          DHUNTS for 60 minute
     C                   Z-ADD     0             HLANTV            7 0          Anes Time Value
     C                   Z-ADD     0             XXANTV            5 2          Anes WORKFIELD
     C                   MOVE      ' '           OPNFLG            1            File Open Flag
     C                   MOVE      '0'           OFCPD             1            CLEAR TMP IN(NOT USD
     C                   MOVE      ' '           OVRFLG            1            File Override Flag
     C                   Z-ADD     0             SRFUT             5 0
     C                   Z-ADD     0             SRPST             5 0
     C                   MOVE      *BLANKS       SRDTE             8
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
     C     CAFDAT        IFGE      950501
     C     CAFDAT        ANDLE     970101
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
     C  N60              Z-ADD     999           Q
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
     C  N60              Z-ADD     999           Q
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
     C*** BEGIN END *** 08/09/02 **************************************
     C**********           MOVE *BLANKS   XXFRZP
     C*** END   END *** 08/09/02 **************************************
     C                   MOVE      *BLANKS       XXZIPA
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C* Now that the Provider is established, get its Geographic Zip Area
     C* 1st - establish the first 3-bytes of the zip code
     C*** BEGIN END *** 08/09/02 **************************************
     C**********           MOVELPDZIP     XXFRZP
     C*** END   END *** 08/09/02 **************************************
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
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     RCMRA         XCMRA
     C*** END ADD *** 09/04/98 ****************************************
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
     C  N60              Z-ADD     999           Q
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
     C*   Hospital Subroutine or Regular Providers.
     C     CARCFL        IFEQ      'S'
     C     CARCFL        OREQ      'N'
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
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '98'          QQC(Q)
     C* Err Msg 98: "Invalid Procedure Code, See Programmer"
     C                   GOTO      TERM
     C                   END
     C     PRSTAT        CABEQ     'I'           NXTPR2
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     PRMRA         XCMRA
     C*** END ADD *** 09/04/98 ****************************************
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
     C* PER P. 250 EMG
     C* Payment for EMG services includes all supplies necessary to
     C* perform the service. Additional payment must not be made for
     C* disposable needles.
     C*
     C     DHPRCD        IFEQ      '99070'
     C*
     C                   MOVE      '95860'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '95860'                                      needles
     C     XRCPT4        ANDLE     '95872'
     C     XRTOTP        IFGT      0
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EF'          QQC(Q)
     C* Err Msg EF: "Supplies are not allowed with this code range"
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
     C* PER P. 28 SUPPLIES AND MATERIALS
     C* CPT 99070 -  Payment for supplies must not exceed the actual
     C* cost of the item(s) plus 20%.
     C*
     C     DHPRCD        IFEQ      '99070'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BI'          QQC(Q)
     C* Err Msg BI: "Depending on type of supply, payment for 99070
     C*             may be increased 20%".
     C                   END
     C*
     C* ===============================================================
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
     C  N60              Z-ADD     999           Q
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
     C  N60              Z-ADD     999           Q
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
     C  N60              Z-ADD     999           Q
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
     C  N60              Z-ADD     999           Q
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
     C  N60              Z-ADD     999           Q
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
     C*-----------------------------------------------------------------
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
     C                   CLOSE     FACALSC
     C                   CLOSE     FACAPSC
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALSC
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPSC
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACSC'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALSC
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPSC
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALSC
     C                   OPEN      FACAPSC
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
     C                   CLOSE     FACALSC
     C                   CLOSE     FACAPSC
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALSC
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPSC
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACSC'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALSC
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPSC
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALSC
     C                   OPEN      FACAPSC
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
     C                   EXSR      MEDI
     C*
     C                   ELSE                                                      ELSE MEDI
     C     DHPRCD        IFGE      '99201'                                      EVAL & MGMT
     C     DHPRCD        ANDLE     '99499'
     C                   EXSR      OFFI
     C*
     C                   ELSE                                                      ELSE OFFI
     C*
     C                   EXSR      OTHE                                         OTHER CODES
     C*
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
     C     PXFLAG        ANDNE     'P'
     C     PXDSEQ        ANDNE     CADSEQ
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
     C     CAMRA         ANDEQ     0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CATOTA        ANDEQ     0
     C     CAPAID        ANDEQ     0
     C*
     C*   If there is an override percentage, calculate
     C     CSOVRP        ANDGT     *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CACHRG        MULT      XXOVRP        CATOTA
     C*
     C     CACHRG        SUB       CATOTA        CAMRA
     C                   Z-ADD     CATOTA        CAPAID
     C*
     C                   Z-ADD     XCMRA         CAMRAA
     C                   UPDATE    PCA01
     C*
     C                   ELSE
     C                   EXCEPT    ECA01
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END                                                    END EQ OVRRD
     C*
     C*** Process next record *****************************************
     C     NXTRCD        TAG
     C*
     C*
     C     CAKYH1        READE     PCA01                                  54      GET NEXT RCD
     C*                                                      TO PROCESS
     C*
     C                   ENDDO
     C*
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
     C                   MOVE      *BLANKS       XPEOMB
     C*
     C                   EXCEPT    ECA01
     C*
     C     CAFDAT        IFGE      950501
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
     C                   OTHER
     C                   MOVE      *BLANKS       XPPMOD
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
     C  N60              Z-ADD     999           Q
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
     C  N60              Z-ADD     999           Q
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
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     RCMRA         XCMRA
     C*** END ADD *** 09/04/98 ****************************************
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
     C  N60              Z-ADD     999           Q
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
     C*   Hospital Subroutine or Regular Providers.
     C     CARCFL        IFEQ      'S'
     C     CARCFL        OREQ      'N'
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
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '98'          QQC(Q)
     C* Err Msg 98: "Invalid Procedure Code, See Programmer"
     C                   GOTO      TERM
     C                   END
     C     PRSTAT        CABEQ     'I'           N2TPR2
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     PRMRA         XCMRA
     C*** END ADD *** 09/04/98 ****************************************
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
     C* PER P. 250 EMG
     C* Payment for EMG services includes all supplies necessary to
     C* perform the service. Additional payment must not be made for
     C* disposable needles.
     C*
     C     DHPRCD        IFEQ      '99070'
     C*
     C                   MOVE      '95860'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '95860'                                      needles?
     C     XRCPT4        ANDLE     '95872'
     C     XRTOTP        IFGT      0
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EF'          QQC(Q)
     C* Err Msg EF: "Supplies are not allowed with this code range"
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
     C* PER P. 28 SUPPLIES AND MATERIALS
     C* CPT 99070 -  Payment for supplies must not exceed the actual
     C* cost of the item(s) plus 20%.
     C*
     C     DHPRCD        IFEQ      '99070'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BI'          QQC(Q)
     C* Err Msg BI: "Depending on type of supply, payment for 99070
     C*             may be increased 20%".
     C                   END
     C*
     C* ===============================================================
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
     C  N60              Z-ADD     999           Q
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
     C  N60              Z-ADD     999           Q
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
     C  N60              Z-ADD     999           Q
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
     C  N60              Z-ADD     999           Q
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
     C  N60              Z-ADD     999           Q
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
     C                   CLOSE     FACALSC
     C                   CLOSE     FACAPSC
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALSC
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPSC
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACSC'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALSC
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPSC
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALSC
     C                   OPEN      FACAPSC
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
     C                   CLOSE     FACALSC
     C                   CLOSE     FACAPSC
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALSC
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPSC
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACSC'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALSC
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAPSC
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALSC
     C                   OPEN      FACAPSC
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
     C                   EXSR      MEDI
     C*
     C                   ELSE                                                      ELSE MEDI
     C     DHPRCD        IFGE      '99201'                                      EVAL & MGMT
     C     DHPRCD        ANDLE     '99499'
     C                   EXSR      OFFI
     C*
     C                   ELSE                                                      ELSE EVAL
     C*
     C*
     C                   EXSR      OTHE                                         OTHER CODES
     C*
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
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     DHKY2         CHAIN     PDH03                              54
     C*
     C     *IN54         DOWEQ     *OFF
     C*
     C                   Z-ADD     D1DSEQ        KKDSEQ
     C*
     C     CAKYP         CHAIN     PCA0P                              61
     C*
     C     *IN61         IFEQ      *OFF
     C*
     C     PXFDAT        ANDEQ     CAFDAT
     C     PXFEIN        ANDEQ     CAFEIN
     C     PXFLAG        ANDNE     'P'
     C     PXDSEQ        ANDNE     CADSEQ
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
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
     C     CAMRA         ANDEQ     0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CATOTA        ANDEQ     0
     C     CAPAID        ANDEQ     0
     C*
     C*   If there is an override percentage, calculate
     C     CSOVRP        ANDGT     *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CACHRG        MULT      XXOVRP        CATOTA
     C*
     C     CACHRG        SUB       CATOTA        CAMRA
     C                   Z-ADD     CATOTA        CAPAID
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XCMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA01
     C*
     C                   ELSE
     C                   EXCEPT    ECA01
     C*
     C                   END
     C*
     C                   END
     C                   END                                                    END EQ OVRRD
     C*
     C*** Process next record *****************************************
     C     N2TRCD        TAG
     C*
     C*
     C     CAKYH1        READE     PCA01                                  54      GET NEXT RCD
     C*                                                      TO PROCESS
     C                   ENDDO
     C*
     C*  END DUP OF MAIN CODE
     C*****************************************************************
     C*
     C*** Execute SURG2 if a SURG Code has been processed
     C     OPNFLG        IFEQ      '1'
     C                   EXSR      SURG2                                        Process the SURG cod
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
     C                   CLOSE     FACALSC
     C                   CLOSE     FACAPSC
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALSC
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPSC
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*---------------------------------------------------------------------
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     0             XCMRA             9 2
     C*** END ADD *** 09/04/98 ****************************************
     C*
     C* Write adjudication messages to FDMGP
     C                   EXSR      MSG
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
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '23'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '51'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BQ'          QQC(Q)
     C*** Err Msg BQ: "Documentation is required for reimbursement"
     C                   END
     C*
     C*
     C***  PER P. 37 ANESTHESIA
     C     SVSPCD        IFNE      'ANE'
     C     SVSPCD        ANDNE     'RNA'
     C     SVSPCD        ANDNE     'PHY'
     C     SVSPCD        ANDNE     'MD '
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C*** SET UP BASIC VALUE AMOUNT, TIME FACTOR, AND CONVERSION AMT
     C*** FOR AMAP CALCULATION
     C*
     C* PER P. 39 - 49                             UNIT BASED (MIN)
     C* RATES:  ANESTHESIOLOGIST PERFORMS  22.00       15 MIN
     C*         ANES UNDER MEDICAL DIR     22.00       30 MIN
     C*         CRNA                       18.00       15 MIN
     C*         CRNA UNDER MEDICAL DIR     11.00       15 MIN
     C*
     C                   Z-ADD     *ZEROS        XXBASV            9 2
     C                   Z-ADD     *ZEROS        XXMIN
     C                   Z-ADD     *ZEROS        XXAMT
     C     SVSPCD        IFEQ      'RNA'
     C     DHAREA        ANDEQ     'SU'
     C                   Z-ADD     SX92N3        XXBASV
     C                   Z-ADD     15            XXMIN
     C                   Z-ADD     XXCRNS        XXAMT
     C                   ELSE
     C     SVSPCD        IFEQ      'RNA'
     C                   Z-ADD     SX92N2        XXBASV
     C                   Z-ADD     15            XXMIN
     C                   Z-ADD     XXCRNA        XXAMT
     C                   ELSE
     C*** IF SUPERVISED, 'ANE'
     C     DHAREA        IFEQ      'SU'
     C                   Z-ADD     SXANAM        XXBASV
     C                   Z-ADD     30            XXMIN
     C                   Z-ADD     XXANES        XXAMT
     C                   ELSE
     C*** ALL OTHERS GET 'ANE' -  BASE VALUE, MINUTES, AND CONV FACTOR
     C                   Z-ADD     SXANAM        XXBASV
     C                   Z-ADD     15            XXMIN
     C                   Z-ADD     XXANES        XXAMT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C* ===============================================================
     C* PER P. 38 ANESTHESIA
     C* When multiple surgical procedures are performed during an
     C* operative session, use the CPT code which has the highest
     C* Basic Value.
     C                   Z-ADD     *ZEROS        COUNT
     C                   MOVE      '00100'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '00100'
     C     XRCPT4        ANDLE     '01999'
     C     XRDSEQ        IFNE      CADSEQ
     C     XRTOTP        ANDGT     *ZEROS
     C*
     C                   MOVE      XRCPT4        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C                   Z-ADD     *ZEROS        XXBAS             9 2
     C*
     C     PRKY          SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         DOWEQ     '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        IFEQ      'A'
     C     SVSPCD        IFEQ      'RNA'
     C     XRPART        ANDEQ     'SU'
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     PRMRA         XCMRA
     C*** END ADD *** 09/04/98 ****************************************
     C                   Z-ADD     PR92N3        XXBAS
     C                   ELSE
     C     SVSPCD        IFEQ      'RNA'
     C                   Z-ADD     PR92N2        XXBAS
     C                   ELSE
     C                   Z-ADD     PRANAM        XXBAS
     C                   ENDIF                                                  END SVSPCD
     C                   ENDIF                                                  END SVSPCD
     C*
     C     XXBAS         IFGE      XXBASV
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C                   ENDIF                                                  END XXBAS GE 2
     C                   ENDIF                                                  END PRSTAT
     C*
     C                   READP     PPR01                                  54
     C                   ENDDO                                                  END DOW 54
     C*
     C                   END                                                    END NE DSEQ ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C* ===============================================================
     C***  EDIT FOR MISSING ANESTHESIA UNITS
     C     DHUNTS        IFLT      1
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C*================================================================
     C*=== Calculating Time Units =====================================
     C* PER P 38 ANESTHESIA
     C* The Time Units for all procedures is computed by allowing
     C* for each 15 minutes of anesthesia time.
     C* (When an ANES provides the service under physician
     C* direction, time is measured in units of 30 minutes (XXMIN)).
     C*
     C*  1.0 Unit for each 15 minutes or any fraction thereof
     C     DHUNTS        DIV       XXMIN         HLANMN
     C** Round up all fractions of 15 minutes to an additional Time Unit
     C                   MVR                     HLADDT
     C                   Z-ADD     *ZEROS        HLANTV
     C     HLADDT        IFGT      *ZEROS
     C                   ADD       1             HLANTV
     C                   END
     C                   ADD       HLANMN        HLANTV
     C*
     C** Time Units * Conversion Factor = Time Value Amount(TVA)
     C     HLANTV        MULT      XXAMT         XXWORK
     C*
     C*
     C** Basic MAP amount + TVA = Fee (AMAP)
     C     XXBASV        ADD(H)    XXWORK        SXMRA
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
     C                   Z-ADD     1             N                 4 0
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
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     PRMRA         XCMRA
     C*** END ADD *** 09/04/98 ****************************************
     C                   Z-ADD     XAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C                   Z-ADD     PRFOLU        SRFUTR            5 0
     C                   MOVE      ' '           SRRTCD            1
     C*** BEGIN CHANGE ********* 11/18/97 ********************************
     C***********          CALL 'SRFUTR'
     C                   CALL      'SRFUTR8'
     C*** END   CHANGE ********* 11/18/97 ********************************
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
     C     XPPMOD        IFNE      *BLANK
     C*
     C     XPPMOD        IFNE      '26'
     C     XPPMOD        ANDNE     '27'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C* ===============================================================
     C*
     C*  VALIDATE IF CPT4 CODE CAN BE USED WITH MODIFIERS 26 OR 27
     C*
     C     XPPMOD        IFEQ      '26'
     C     SXPROF        ANDEQ     *ZEROS
     C     XPPMOD        OREQ      '27'
     C     SX92N1        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C*           compensation program".
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END
     C*
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C                   ENDIF
     C*
     C* ===============================================================
     C* PER P. 250 MEDICINE
     C* When diagnostic services are provided at a hospital or ASC,
     C* the physician will be paid only for the professional component.
     C*
     C     XXPASS        IFEQ      '2'
     C     DHPOS         IFNE      ' 11'
     C     XPPMOD        IFEQ      *BLANK
     C     SXPROF        IFNE      *ZEROS
     C                   MOVE      '26'          XPPMOD
     C                   MOVE      '26'          XPPMOD
     C                   ENDIF                                                  END SXPROF
     C                   ENDIF                                                  END XPPMOD
     C                   ENDIF                                                  END DHPOS
     C                   ENDIF                                                  END XXPASS
     C* ===============================================================
     C* PER P. 37 ANESTHESIA
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
     C  N60              Z-ADD     999           Q
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
     C                   GOTO      MEDIE
     C                   END                                                    END XRTOTP  ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9A        READE     PCA09                                  60
     C  N60XRCPT4        IFGT      '01999'
     C                   LEAVE
     C                   END
     C*
     C                   END                                                    END *IN60
     C*
     C* PER P. 53 SURGERY - Separate procedures.
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
     C     XRCPT4        OREQ      '94250'
     C     XRCPT4        OREQ      '95829'
     C     XRCPT4        OREQ      '95851'
     C     XRCPT4        OREQ      '99195'
     C     XRCPT4        IFEQ      DHPRCD
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRCPT4        ORNE      DHPRCD
     C     XRTOTP        IFGT      0
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C* PER P. 247, 248 OSTEOPATHIC MANIPULATIVE TREATMENT
     C* Chiropractors and osteopaths may use the following Osteopathic
     C* manipulation codes as appropriate
     C* CPT (98925-98929).
     C     DHPRCD        IFGE      '98925'
     C     DHPRCD        ANDLE     '98929'
     C     SVSPCD        IFNE      'CHI'
     C     SVSPCD        ANDNE     'DO '
     C     SVSPCD        ANDNE     'OST'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C                   END                                                    END SVSPCD
     C                   END                                                    END DHPRCD
     C*
     C*
     C* PER P. 271  INITIAL ASSESSMENT
     C* Usually performed by attending physician...
     C     DHPRCD        IFEQ      '97700'
     C     SVSPCD        IFNE      'MD '
     C     SVSPCD        ANDNE     'PHY'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C                   END                                                    END SVSPCD
     C                   END                                                    END DHPRCD
     C*
     C*
     C* PER P. 277 PSYCHOLOGICAL AND BIOFEEDBACK SERVICES
     C*
     C     DHPRCD        IFGE      '90801'
     C     DHPRCD        ANDLE     '90899'
     C     SVSPCD        IFNE      'PSC'
     C     SVSPCD        ANDNE     'PHT'
     C     SVSPCD        ANDNE     'MD '
     C     SVSPCD        ANDNE     'PHY'
     C     SVSPCD        ANDNE     'PHS'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C                   END                                                    END SVSPCD
     C                   END                                                    END DHPRCD
     C*
     C* PER P. 272 WORK HARDENING AND WORK CONDITIONING
     C* Use 97545 for the first hour and 97546 for each addit. hour.
     C* Total payment must not exceed $300 per date of service.
     C*
     C     DHPRCD        IFEQ      '97546'
     C                   Z-ADD     *ZEROS        XXPAID            9 2
     C                   MOVE      '97546'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9         READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFEQ      '97546'
     C     XRDSEQ        IFNE      CADSEQ
     C     XRTOTP        ANDGT     *ZEROS
     C*
     C                   ADD       XRTOTP        XXPAID
     C                   END                                                    END NE DSEQ ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9         READE     PCA09                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C     XXPAID        IFGE      200
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C                   GOTO      MEDIE
     C                   END                                                    END XXPAID > 2
     C                   END                                                    END DHPRCD
     C*
     C*
     C* PER P. 272 WORK HARDENING AND WORK CONDITIONING
     C* Use 97545 for the first hour and 97546 for each addit. hour.
     C* Total payment must not exceed $300 per date of service.
     C*
     C*** EDIT TO ALLOW ONLY 1 (97545 CODE)
     C     DHPRCD        IFEQ      '97545'
     C                   Z-ADD     *ZEROS        COUNT
     C                   MOVE      '97545'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9         READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFEQ      '97545'
     C     XRDSEQ        IFNE      CADSEQ
     C     XRTOTP        ANDGT     *ZEROS
     C*
     C                   ADD       1             COUNT
     C                   END                                                    END NE DSEQ ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9         READE     PCA09                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFGE      1
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C                   GOTO      MEDIE
     C                   END                                                    END COUNT GE 2
     C                   END                                                    END DHPRCD
     C* ===================================================================
     C* PER P. 273  PHYSICAL MEDICINE
     C* The printout of test results is considered part of the service
     C* and will not be paid separately; do not use CPT 99080 in
     C* conjunction with 97750.
     C*
     C     DHPRCD        IFEQ      '99080'
     C                   Z-ADD     *ZEROS        COUNT
     C                   MOVE      '97750'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9         READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFEQ      '97750'
     C     XRDSEQ        IFNE      CADSEQ
     C     XRTOTP        ANDGT     *ZEROS
     C*
     C                   ADD       1             COUNT
     C                   END                                                    END NE DSEQ ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9         READE     PCA09                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFGE      1
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '27'          QQC(Q)
     C* Err Msg 27: "If the necessary documentation exists, override
     C*              and pay at MRA"
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
     C                   GOTO      MEDIE
     C                   END                                                    END COUNT GE 1
     C                   END                                                    END DHPRCD
     C* =================================================================
     C* Per P. 273 PHYSICAL MEDICINE
     C* Payment for CPT 97750 is limited to one test every two weeks.
     C*
     C     DHPRCD        IFEQ      '97750'
     C*     SETLL and READ the file
     C                   MOVE      '97750'       XXCPT4
     C     CAKYC2        SETLL     PCA0A                              54
     C     CAKYC2        READE     PCA0A                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C     XACPT4        IFEQ      DHPRCD
     C     XATOTP        IFGT      *ZEROS
     C     XADSEQ        ANDNE     CADSEQ
     C     XAFDAT        ANDNE     CAFDAT
     C*
     C* FIND DATE 14 DAYS PRIOR
     C                   Z-ADD     14            SRPST
     C                   Z-ADD     XAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT            8 0
     C                   MOVE      WKFDAT        SRDTE
     C*** BEGIN CHANGE ********* 11/18/97 ********************************
     C***********          CALL 'SRPAST'
     C                   CALL      'SRPAST8'
     C*** END   CHANGE ********* 11/18/97 ********************************
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C*
     C                   MOVE      SRDTE         XXPAST
     C                   Z-ADD     XXPAST        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPAST
     C*
     C* FIND DATE 14 DAYS FUTURE
     C                   Z-ADD     14            SRFUT
     C                   Z-ADD     XAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C*** BEGIN CHANGE ********* 11/18/97 ********************************
     C***********          CALL 'SRFUTR'
     C                   CALL      'SRFUTR8'
     C*** END   CHANGE ********* 11/18/97 ********************************
     C                   PARM                    WKFDAT
     C                   PARM                    SRFUT
     C                   PARM                    SRTERM
     C*
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFUTR            7 0
     C*
     C     CAFDAT        IFGE      XXPAST
     C     CAFDAT        ANDLE     XAFDAT
     C     CAFDAT        ORGE      XAFDAT
     C     CAFDAT        ANDLE     XXFUTR
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '204'         XPEOMB
     C* EOMB Code 204: "Exceeds number of allowable procedures in
     C*                 Reimbursement Manual"
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EG'          QQC(Q)
     C*** Err Msg EG: "This procedure is limited to once every 2 weeks"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END CAFDAT
     C                   END                                                    END XATOTP
     C                   END                                                    END XACPT4
     C*
     C     CAKYC2        READE     PCA0A                                  54
     C                   END                                                    END 0 DOWEQ
     C                   END                                                    END DHPRCD
     C* ====================================================================
     C* PER P 273 PHYSICIAL MEDICINE
     C* The MAP for a functional capacity assessment is $350.
     C*
     C     DHPRCD        IFEQ      '97750'
     C*     SETLL and READ the file
     C                   MOVE      '97750'       XXCPT4
     C                   Z-ADD     *ZERO         XXTOTP
     C                   Z-ADD     *ZERO         XXMRA
     C                   Z-ADD     *ZERO         XXWORK
     C                   Z-ADD     *ZERO         XXDIFF
     C     CAKY9         SETLL     PCA09                              54
     C     CAKY9         READE     PCA09                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C     XRCPT4        IFEQ      '97750'
     C     XRDSEQ        ANDNE     CADSEQ
     C                   ADD       XRTOTP        XXTOTP
     C                   ENDIF                                                  END XRCPT4
     C*
     C     CAKY9         READE     PCA09                                  54
     C                   END                                                    END 0 DOWEQ
     C*
     C*=== Calculating Time Units =====================================
     C*
     C* The Time Unit for this procedure is computed by allowing 1 unit
     C* for each 15 minutes.
     C* Move default of 1 to DHUNTS
     C*
     C     DHUNTS        IFEQ      *ZEROS
     C                   Z-ADD     1             DHUNTS
     C                   ENDIF
     C*
     C*  1.0 Unit for each 15 minutes or any fraction thereof
     C     DHUNTS        DIV       15            HLANMN
     C** Round up all fractions of 15 minutes to an additional Time Unit
     C                   MVR                     HLADDT
     C*
     C                   Z-ADD     *ZEROS        HLANTV
     C     HLADDT        IFGT      *ZEROS
     C                   ADD       1             HLANTV
     C                   END
     C                   ADD       HLANMN        HLANTV
     C** Total unit Value * Conversion Factor = Fee
     C     HLANTV        MULT(H)   SXANAM        XXWORK
     C*
     C     XXTOTP        IFGE      350
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '213'         XPEOMB
     C*** EOMB 213: "Reimbursement reflects difference between
     C***            previously paid procedure".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   ENDIF                                                  END XXTOTP
     C*
     C     XXTOTP        IFEQ      *ZEROS
     C     XXWORK        IFLT      350
     C                   Z-ADD     XXWORK        SXMRA
     C                   ELSE
     C                   Z-ADD     350           SXMRA
     C                   END                                                    END XXWORK
     C                   ELSE
     C     350           SUB       XXTOTP        XXDIFF
     C     XXWORK        IFLT      XXDIFF
     C                   Z-ADD     XXWORK        SXMRA
     C                   ELSE
     C                   Z-ADD     XXDIFF        SXMRA
     C                   END                                                    END XXWORK
     C                   END                                                    END XXTOTP
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '56'          QQC(Q)
     C*** Err Msg 56: "Adjuster should verify use of this procedure"
     C*
     C     XXDIFF        IFGT      *ZEROS
     C                   MOVE      '213'         XPEOMB
     C*** EOMB 213: "Reimbursement reflects difference between
     C***            previously paid procedure".
     C*
     C                   ENDIF                                                  END XXDIFF
     C*
     C                   END                                                    END DHPRCD
     C*
     C* ===============================================================
     C*
     C* PER P. 277 PSYCHOLOGICAL AND BIOFEEDBACK SERVICES
     C* Payment is limited to providers currently licensed or cert.
     C* or who meet minimum certification req in Biofeedback...
     C*
     C     DHPRCD        IFGE      '90900'
     C     DHPRCD        ANDLE     '90915'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EH'          QQC(Q)
     C*** Err Msg EH: "Verify provider's license or certification
     C***              in biofeedback"
     C*
     C                   END                                                    END DHPRCD
     C* ===================================================================
     C* PER P. 277  PSYCHOLOGICAL AND BIOFEEDBACK SERVICES
     C* Payment for CPT 90801 includes ... and the preparation of
     C* a written report which must be submitted with the required
     C* billing form.
     C*
     C     DHPRCD        IFEQ      '99080'
     C                   Z-ADD     *ZEROS        COUNT
     C                   MOVE      '90801'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9         READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFEQ      '90801'
     C     XRDSEQ        IFNE      CADSEQ
     C     XRTOTP        ANDGT     *ZEROS
     C*
     C                   ADD       1             COUNT
     C                   END                                                    END NE DSEQ ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9         READE     PCA09                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFGE      1
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '27'          QQC(Q)
     C* Err Msg 27: "If the necessary documentation exists, override
     C*              and pay at MRA"
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
     C                   GOTO      MEDIE
     C                   END                                                    END COUNT GE 1
     C                   END                                                    END DHPRCD
     C***
     C***
     C*** NOTE: DISPLAY MSG CONCERNING 99080 - 2 PAYABLE RATES
     C*
     C     DHPRCD        IFEQ      '99080'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EE'          QQC(Q)
     C* Err Msg EE: "If check-list report ($48 allowed),
     C*              if written report ($60 allowed)"
     C*
     C                   END                                                    END DHPRCD
     C* ===============================================================
     C* PER P 277 SECTION 8 PSYCHOLOGICAL AND BIOFEEDBACK SERVICES
     C* May be paid when ordered by a physicain and performed by a
     C* licensed, independently practicing psychologist.
     C*
     C     DHPRCD        IFGE      '90801'
     C     DHPRCD        ANDLE     '90899'
     C     SVSPCD        IFNE      'MD'
     C     SVSPCD        ANDNE     'PSC'
     C     SVSPCD        ANDNE     'PHS'
     C     SVSPCD        ANDNE     'PHY'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BP'          QQC(Q)
     C*** Err Msg BP: "Specialty Code and level of service don't match,
     C***              please verify".
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
     C                   END                                                    END SVSPCD
     C                   END                                                    END DHPRCD
     C*
     C* ===============================================================
     C* PER P. 273 PHYSICAL MEDICINE
     C* CPT codes 97010 - 97028 are Physical Medicine Modalities for
     C* a single modality or procedure and are paid per service only.
     C* When billing for more than one body area the service notes
     C* must be attached so that the body areas can be identified by
     C* claim reviewer.
     C*
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97028'
     C                   Z-ADD     *ZEROS        COUNT
     C                   MOVE      DHPRCD        XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9         READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFEQ      DHPRCD
     C     XRDSEQ        IFNE      CADSEQ
     C     XRTOTP        ANDGT     *ZEROS
     C     XRPART        ANDEQ     DHAREA
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C                   GOTO      MEDIE
     C                   END                                                    END NE DSEQ ...
     C                   END                                                    END EQ CPT4 ...
     C     CAKY9         READE     PCA09                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C* =================================================================
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
     C  N60              Z-ADD     999           Q
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
     C* PER P 273 BILLING GUIDELINES
     C* Physical Medicine Therapies are paid according to time.
     C* CPTS 97500, 97520, 97540, 97700 are based on 'initial
     C* 30 minutes'.
     C* NOTE: The std fee is paid regardless of time entered.
     C*
     C     DHPRCD        IFEQ      '97500'
     C     DHPRCD        OREQ      '97520'
     C     DHPRCD        OREQ      '97540'
     C     DHPRCD        OREQ      '97700'
     C                   Z-ADD     *ZEROS        COUNT
     C                   MOVE      DHPRCD        XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9         READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFEQ      DHPRCD
     C     XRDSEQ        IFNE      CADSEQ
     C     XRTOTP        ANDGT     *ZEROS
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EM'          QQC(Q)
     C*** Err Msg EM: "Only one initial minutes code is allowed
     C*                per date"
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
     C                   GOTO      MEDIE
     C                   END                                                    END NE DSEQ ...
     C                   END                                                    END EQ CPT4 ...
     C     CAKY9         READE     PCA09                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C* ===================================================================
     C* PER P 273 BILLING GUIDELINES
     C* Physical Medicine Therapies are paid according to time.
     C* Note: TIME MUST BE ENTERED IN MINUTES
     C*
     C     DHPRCD        IFGE      '97032'
     C     DHPRCD        ANDLE     '97139'
     C     DHPRCD        OREQ      '97530'
     C     DHPRCD        OREQ      '97501'
     C     DHPRCD        OREQ      '97521'
     C     DHPRCD        OREQ      '97541'
     C     DHPRCD        OREQ      '97701'
     C     DHPRCD        OREQ      '97770'
     C*
     C*=== Calculating Time Units =====================================
     C*
     C* The Time Unit for these procedures is computed by
     C* allowing 1 unit for each 15 minutes.
     C*
     C     DHUNTS        IFEQ      *ZEROS
     C                   Z-ADD     15            DHUNTS
     C                   ENDIF
     C*
     C*  1.0 Unit for each 15 minutes
     C** Round to nearest 15 minute increment
     C     DHUNTS        DIV(H)    15            HLANTV
     C*
     C** Total unit Value * Conversion Factor = Fee
     C     HLANTV        MULT(H)   SXANAM        SXMRA
     C                   END                                                    END DHPRCD
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
     C     XPPMOD        ANDNE     '57'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C  N60              Z-ADD     999           Q
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
     C  N60              Z-ADD     999           Q
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
     C*** NOTE: DISPLAY MSG CONCERNING 99499 - 2 PAYABLE RATES
     C*
     C     DHPRCD        IFEQ      '99499'
     C     DHAREA        IFEQ      'I1'
     C                   Z-ADD     350           SXMRA
     C                   ELSE
     C     DHAREA        IFEQ      'I2'
     C                   Z-ADD     500           SXMRA
     C                   ELSE
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EO'          QQC(Q)
     C* Err Msg EO: "If 1-2 body areas ($350 allowed),
     C*              more than 2 areas ($500 allowed)"
     C*
     C                   END                                                    END DHAREA
     C                   END                                                    END DHAREA
     C                   END                                                    END DHPRCD
     C*
     C* ===============================================================
     C* PER P. 37 ANESTHESIA
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
     C  N60              Z-ADD     999           Q
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
     C                   END                                                    END XRTOTP  ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9A        READE     PCA09                                  60
     C  N60XRCPT4        IFGT      '01999'
     C                   LEAVE
     C                   END
     C*
     C                   END                                                    END *IN60
     C*
     C* ===============================================================
     C* PER P. 250 MEDICINE
     C* When an EMG is performed on the same day as a followup
     C* visit or consult., payment may be made for the visit only when
     C* doc of med nec substantiates the need for the visit.
     C*
     C* PER P. 251 MEDICINE
     C* When a nerve conduction study is perf on same day as followup
     C* visit or consult., payment may be made for the visit only when
     C* doc of med nec substantiates the need for the visit.
     C*
     C     DHPRCD        IFGE      '99211'
     C     DHPRCD        ANDLE     '99215'
     C     DHPRCD        ORGE      '99241'
     C     DHPRCD        ANDLE     '99245'
     C*
     C                   MOVE      '95860'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '95860'
     C     XRCPT4        ANDLE     '95872'
     C     XRCPT4        ORGE      '95900'
     C     XRCPT4        ANDLE     '95904'
     C     XRTOTP        IFGT      *ZEROS
     C     XRDSEQ        ANDNE     CADSEQ
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BJ'          QQC(Q)
     C* Err Msg BJ: "Verify submitted documentation indicates necessary
     C*              office visit."
     C*
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C*
     C                   MOVE      '201'         XPEOMB
     C* EOMB 201: "Service is not reimburseable under the
     C*            worker's compensation program".
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C                   END                                                    END XRTOTP  ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END *IN60
     C                   END                                                    END DHPRCD
     C* ====================================================================
     C* PER P. 271 ELIGIBLE PROVIDERS
     C* Evaluation and mgmt services will also be paid in addition to
     C* manipulation services when the office notes substantiate
     C* an E/M service.
     C*
     C     DHPRCD        IFGE      '99211'
     C     DHPRCD        ANDLE     '99215'
     C*
     C                   MOVE      '97260'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '98925'
     C     XRCPT4        ANDLE     '98929'
     C     XRCPT4        OREQ      '97260'
     C     XRTOTP        IFGT      *ZEROS
     C     XRDSEQ        ANDNE     CADSEQ
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BJ'          QQC(Q)
     C* Err Msg BJ: "Verify submitted documentation indicates necessary
     C*              office visit."
     C*
     C*
     C                   END                                                    END XRTOTP  ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END *IN60
     C                   END                                                    END DHPRCD
     C* ====================================================================
     C* PER P. 27 INJECTABLE PHARMACEUTICALS
     C* If the single purpose of an office visit is for the injection
     C* the physician may be paid for the cost of the injectable
     C* pharm. plus a fee for the injection but cannot be paid for
     C* an office visit.
     C*
     C                   Z-ADD     *ZEROS        COUNT
     C                   Z-ADD     *ZEROS        COUNT1            5 0
     C                   MOVE      '10000'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '90782'
     C     XRCPT4        ANDLE     '90799'
     C     XRTOTP        IFGT      *ZEROS
     C*
     C                   ADD       1             COUNT                          INJECTABLES
     C                   END                                                    END XRTOTP  ...
     C                   ELSE                                                   ELSE   CPT4 ...
     C     XRCPT4        IFNE      '99070'
     C     XRDSEQ        IFNE      CADSEQ
     C     XRTOTP        ANDGT     *ZEROS
     C*
     C                   ADD       1             COUNT1                         NON-INJECT..
     C                   END                                                    END NE DSEQ ...
     C                   END                                                    END NE CPT4 ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C* IF THERE ARE ONLY INJECTABLES DISALLOW THE OFFICE VISIT
     C     COUNT         IFGE      1
     C     COUNT1        IFEQ      *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C                   END                                                    END COUNT1 0 1
     C                   END                                                    END COUNT GE 1
     C* ====================================================================
     C* Per P. 28 CHIROPRACTORS
     C* Per P. 247 OSTEOPATHS
     C* May only be paid for one office visit per week.
     C*
     C     SVSPCD        IFEQ      'CHI'
     C     SVSPCD        OREQ      'DO'
     C     SVSPCD        OREQ      'OST'
     C*     SETLL and READ the file
     C                   MOVE      '99201'       XXCPT4
     C     CAKYC2        SETLL     PCA0A                              54
     C     CAKYC2        READE     PCA0A                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C     XACPT4        IFGE      '99201'
     C     XACPT4        ANDLE     '99499'
     C     XATOTP        IFGT      *ZEROS
     C     XADSEQ        ANDNE     CADSEQ
     C*
     C* FIND DATE 7 DAYS PRIOR
     C                   Z-ADD     7             SRPST
     C                   Z-ADD     XAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C                   MOVE      WKFDAT        SRDTE
     C*** BEGIN CHANGE ********* 11/18/97 ********************************
     C***********          CALL 'SRPAST'
     C                   CALL      'SRPAST8'
     C*** END   CHANGE ********* 11/18/97 ********************************
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C*
     C                   MOVE      SRDTE         XXPAST            7 0
     C                   Z-ADD     XXPAST        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPAST
     C*
     C* FIND DATE 7 DAYS FUTURE
     C                   Z-ADD     7             SRFUT
     C                   Z-ADD     XAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C*** BEGIN CHANGE ********* 11/18/97 ********************************
     C***********          CALL 'SRFUTR'
     C                   CALL      'SRFUTR8'
     C*** END   CHANGE ********* 11/18/97 ********************************
     C                   PARM                    WKFDAT
     C                   PARM                    SRFUT
     C                   PARM                    SRTERM
     C*
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFUTR            7 0
     C*
     C     CAFDAT        IFGE      XXPAST
     C     CAFDAT        ANDLE     XAFDAT
     C     CAFDAT        ORGE      XAFDAT
     C     CAFDAT        ANDLE     XXFUTR
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '204'         XPEOMB
     C* EOMB Code 204: "Exceeds number of allowable procedures in
     C*                 Reimbursement Manual"
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EK'          QQC(Q)
     C*** Err Msg EK: "This procedure is limited to once every 7 days"
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C                   END                                                    END CAFDAT
     C                   END                                                    END XATOTP
     C                   END                                                    END XACPT4
     C*
     C     CAKYC2        READE     PCA0A                                  54
     C                   END                                                    END 0 DOWEQ
     C                   END                                                    END SVSPCD
     C*
     C* ====================================================================
     C* Per P. 271 PHYSICAL MEDICINE
     C* Physicians, ostepaths and chiro. who provide therapy in their
     C* office may be paid for an office visit one time per week in
     C* addition to therapy services.
     C*
     C     DHPRCD        IFGE      '99211'
     C     DHPRCD        ANDLE     '99215'
     C*
     C     SVSPCD        IFEQ      'PHY'
     C     SVSPCD        OREQ      'MD '
     C     SVSPCD        OREQ      'OST'
     C     SVSPCD        OREQ      'DO '
     C     SVSPCD        OREQ      'CHI'
     C* DETERMINE IF PT CODES HAVE ALSO BEEN BILLED ON THIS DATE
     C                   Z-ADD     *ZEROS        COUNT
     C                   MOVE      '97010'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '97010'
     C     XRCPT4        ANDLE     '97139'
     C*
     C                   ADD       1             COUNT                          PT CODES
     C                   END                                                    END XRCPT4  ...
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFGT      *ZEROS                                       PT CODES EXIST
     C*     SETLL and READ the file
     C                   MOVE      '99211'       XXCPT4
     C     CAKYC2        SETLL     PCA0A                              54
     C     CAKYC2        READE     PCA0A                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C     XACPT4        IFGE      '99211'
     C     XACPT4        ANDLE     '99215'
     C     XATOTP        IFGT      *ZEROS
     C     XADSEQ        ANDNE     CADSEQ
     C*
     C* FIND DATE 7 DAYS PRIOR
     C                   Z-ADD     7             SRPST
     C                   Z-ADD     XAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C                   MOVE      WKFDAT        SRDTE
     C*** BEGIN CHANGE ********* 11/18/97 ********************************
     C***********          CALL 'SRPAST'
     C                   CALL      'SRPAST8'
     C*** END   CHANGE ********* 11/18/97 ********************************
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C*
     C                   MOVE      SRDTE         XXPAST
     C                   Z-ADD     XXPAST        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPAST
     C*
     C* FIND DATE 7 DAYS FUTURE
     C                   Z-ADD     7             SRFUT
     C                   Z-ADD     XAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C*** BEGIN CHANGE ********* 11/18/97 ********************************
     C***********          CALL 'SRFUTR'
     C                   CALL      'SRFUTR8'
     C*** END   CHANGE ********* 11/18/97 ********************************
     C                   PARM                    WKFDAT
     C                   PARM                    SRFUT
     C                   PARM                    SRTERM
     C*
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFUTR            7 0
     C*
     C     CAFDAT        IFGE      XXPAST
     C     CAFDAT        ANDLE     XAFDAT
     C     CAFDAT        ORGE      XAFDAT
     C     CAFDAT        ANDLE     XXFUTR
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '204'         XPEOMB
     C* EOMB Code 204: "Exceeds number of allowable procedures in
     C*                 Reimbursement Manual"
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EK'          QQC(Q)
     C*** Err Msg EK: "This procedure is limited to once every 7 days"
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C                   END                                                    END CAFDAT
     C                   END                                                    END XATOTP
     C                   END                                                    END XACPT4
     C****
     C     CAKYC2        READE     PCA0A                                  54
     C                   END                                                    END 0 DOWEQ
     C                   END                                                    END COUNT
     C                   END                                                    END SVSPCD
     C                   END                                                    END DHPRCD
     C*
     C* ===============================================================
     C* ONLY 1 INITIAL VISIT CODE IS ALLOWED
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99205'
     C*
     C                   MOVE      '99201'       XXCPT4
     C*
     C     CAKYA1        SETLL     PCA0A
     C     CAKYA1        READE     PCA0A                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     XACPT4        IFGE      '99201'
     C     XACPT4        ANDLE     '99205'
     C     XADSEQ        ANDNE     CADSEQ
     C     XATOTP        ANDGT     *ZEROS
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '54'          QQC(Q)
     C*** Err Msg 54: "Exceeds number of procedures allowed
     C***              in time period"
     C*
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '204'         XPEOMB
     C* EOMB Code 204: "Exceeds number of allowable procedures in
     C*                 Reimbursement Manual"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END                                                    END XACPT4
     C     CAKYA1        READE     PCA0A                                  60
     C                   ENDDO
     C*
     C                   END                                                    END DHPRCD
     C* ===============================================================
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
     C  N60              Z-ADD     999           Q
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
     C*** Per P. 55  SURGERY
     C*** When a starred procedures is performed at the time of the initial
     C*** office visit (new patient) and this procedure constitutes
     C*** the major service rendered during the initial visit,
     C*** Procedure Code 99025 is listed in lieu of the usual initial
     C*** as an additional service.
     C*
     C     DHPRCD        IFGE      '99202'
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
     C     PRSUPP        ANDEQ     'Y'
     C                   MOVE      '99025'       XXCPT4
     C     PRKY          SETGT     PPR01
     C     NXTPRB        TAG
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTPRB
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     PRMRA         XCMRA
     C*** END ADD *** 09/04/98 ****************************************
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
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'CY'          QQC(Q)
     C*** Err Msg CY: "Office visit, reimbursed at code 99025 amount'
     C*
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
     C*** Per P. 55  SURGERY
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
     C  N60              Z-ADD     999           Q
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
     C* ===============================================================
     C* PER P. 16 - 17 PAYMENT POLICY
     C* Only one patient visit per day per authorized provider may be
     C* paid. If a provider submits a claim for two office visits on
     C* the same day, only the highest level of service will be paid.
     C*
     C                   MOVE      '99201'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '99201'
     C     XRCPT4        ANDLE     '99499'
     C     XRTOTP        IFGT      SXMRA
     C     XRDSEQ        ANDNE     CADSEQ
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BJ'          QQC(Q)
     C* Err Msg BJ: "Verify submitted documentation indicates necessary
     C*              office visit."
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
     C                   END                                                    END XRTOTP  ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C* ===============================================================
     C*
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
     C* PER P. 37 ANESTHESIA
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
     C  N60              Z-ADD     999           Q
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
     C*
     C*
     C* ===============================================================
     C* Per P. 28 Travel Reimbursement
     C* An injured worker who travels more than five (5) miles one
     C* way to receive medical care is entitled to collect 25.5
     C* cents per mile for travel.
     C*
     C*=== Calculating MILES ==========================================
     C*
     C     DHPRCD        IFEQ      'TRAVL'
     C*
     C     DHUNTS        IFLT      5
     C                   Z-ADD     5             DHUNTS
     C                   ENDIF
     C*
     C     DHUNTS        SUB       5             XXWORK
     C     SXANAM        DIV       100           XXRATE            4 3
     C     XXWORK        MULT(H)   XXRATE        SXMRA
     C*
     C     SXMRA         IFEQ      *ZEROS
     C                   MOVE      '002'         XPEOMB
     C                   ENDIF
     C                   END                                                    END DHPRCD
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
     C* VALID MODIFIERS ARE: 26, 27
     C*
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per P. 243  MODIFIERS
     C     XPPMOD        IFNE      '26'
     C     XPPMOD        ANDNE     '27'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C*
     C*  VALIDATE IF CPT4 CODE CAN BE USED WITH MODIFIERS 26 OR 27
     C*
     C     XPPMOD        IFEQ      '26'
     C     SXPROF        ANDEQ     *ZEROS
     C     XPPMOD        OREQ      '27'
     C     SX92N1        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C                   END
     C*
     C* ===============================================================
     C* PER P. 37 ANESTHESIA
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
     C  N60              Z-ADD     999           Q
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
     C                   GOTO      PATHE
     C                   END                                                    END XRTOTP  ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9A        READE     PCA09                                  60
     C  N60XRCPT4        IFGT      '01999'
     C                   LEAVE
     C                   END
     C*
     C                   END                                                    END *IN60
     C*
     C* ====================================================================
     C* PER P. 53 SURGERY - Separate procedures.
     C* NOTE: If a separate proc is coded w/another SURG code,
     C*       display error message ONLY.
     C*
     C     DHPRCD        IFEQ      '89350'
     C*
     C                   MOVE      '10040'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '10040'                                      SURG CODES
     C     XRCPT4        ANDLE     '69979'
     C     XRTOTP        IFGT      0
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'CX'          QQC(Q)
     C* Err Msg CX: "Possible unbundling of a total proc,
     C*              separate procedure coded W/SURG".
     C*
     C                   LEAVE
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END XRCPT4
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END DOW *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C* ===============================================================
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C                   ENDIF
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
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '76'          QQC(Q)
     C*** Err Msg 76: "CPT modifier used with this charge.  Verify use
     C***              and payment."
     C*
     C* ===============================================================
     C* Modifier -26
     C     XPPMOD        IFEQ      '26'
     C*
     C                   Z-ADD     SXPROF        SXMRA
     C*
     C                   END                                                    END PMOD EQ 26
     C*
     C*
     C* ===============================================================
     C* Modifier -27
     C     XPPMOD        IFEQ      '27'
     C*
     C                   Z-ADD     SX92N1        SXMRA
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
     C* ===============================================================
     C* PER P. 220 RADIOLOGY  Payment
     C* Should the provider fail to add mod -26 to a radiology proc.
     C* provided in a hosp, ambul. surgical center or similar facility,
     C* payment will be made only for the professional component.
     C*
     C     DHPOS         IFNE      ' 11'
     C     XPPMOD        ANDEQ     *BLANK
     C     SXPROF        ANDNE     *ZEROS
     C                   MOVE      '26'          XPPMOD
     C                   MOVE      '26'          XPPMOD
     C                   END                                                    END DHPOS
     C* ===============================================================
     C* VALID MODIFIERS ARE: 26, 27
     C*
     C* Procedure Codes with modifiers:
     C*----------------------------------------------------------------
     C* Per P. 218 - 219 RADIOLOGY
     C     XPPMOD        IFNE      *BLANKS
     C     XPPMOD        IFNE      '26'
     C     XPPMOD        ANDNE     '27'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C***
     C*  VALIDATE IF CPT4 CODE CAN BE USED WITH MODIFIERS 26 OR 27
     C*
     C     XPPMOD        IFEQ      '26'
     C     SXPROF        ANDEQ     *ZEROS
     C     XPPMOD        OREQ      '27'
     C     SX92N1        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C                   END
     C*
     C* ===============================================================
     C* Modifier -26
     C     XPPMOD        IFEQ      '26'
     C*
     C                   Z-ADD     SXPROF        SXMRA
     C*
     C                   END                                                    END PMOD EQ 26
     C*
     C*
     C* ===============================================================
     C* Modifier -27
     C     XPPMOD        IFEQ      '27'
     C*
     C                   Z-ADD     SX92N1        SXMRA
     C*
     C                   END                                                    END PMOD EQ 27
     C*
     C* ===============================================================
     C* PER P. 37 ANESTHESIA
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
     C  N60              Z-ADD     999           Q
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
     C                   GOTO      RADIE
     C                   END                                                    END XRTOTP  ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9A        READE     PCA09                                  60
     C  N60XRCPT4        IFGT      '01999'
     C                   LEAVE
     C                   END
     C*
     C                   END                                                    END *IN60
     C*
     C* ====================================================================
     C* PER P. 53 SURGERY - Separate procedures.
     C* NOTE: If a separate proc is coded w/another SURG code,
     C*       display error message ONLY.
     C*
     C     DHPRCD        IFEQ      '76000'
     C*
     C                   MOVE      '10040'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '10040'                                      SURG CODES
     C     XRCPT4        ANDLE     '69979'
     C     XRTOTP        IFGT      0
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'CX'          QQC(Q)
     C* Err Msg CX: "Possible unbundling of a total proc,
     C*              separate procedure coded W/SURG".
     C*
     C                   LEAVE
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END XRCPT4
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END DOW *IN60
     C*
     C                   END                                                    END DHPRCD
     C* ===============================================================
     C* PER P. 220 RADIOLOGY
     C* Payment for prof services (mod -26) will not be made when X-ray
     C* taken elsewhere are reviewed during an IME, medical visit or
     C* consultation.
     C*
     C     XPPMOD        IFEQ      '26'
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
     C  N60              Z-ADD     999           Q
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
     C                   GOTO      RADIE
     C                   END                                                    END XRTOTP > 0
     C                   END                                                    END XRCPT4
     C     CAKY9A        READE     PCA09                                  60
     C                   END                                                    END DOW *IN60
     C*
     C                   END                                                    END XPPMOD
     C*
     C*
     C* ===============================================================
     C* PER P. 219 RADIOLOGY - Complete procedures.
     C* To bill for the service rendered by the radiologist,
     C* use the applicable radiology procedure code with a
     C* descriptor which specifies 'supervision and interpretation
     C* only'.  Mod -26 must not be added to the proc. code.
     C*
     C     XPPMOD        IFEQ      '26'
     C     XPPMOD        OREQ      '27'
     C                   Z-ADD     1             R                 5 0
     C     DHPRCD        LOOKUP    RADS(R)                                60
     C     *IN60         IFEQ      '1'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EI'          QQC(Q)
     C* Err Msg EI: "Supervision/interpretation included in basic
     C*              allowance,REVIEW payment"
     C*
     C                   ENDIF                                                  END *IN60
     C                   ENDIF                                                  END XPPMOD
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
     C* ==================================================================
     C     XPPMOD        IFNE      *BLANK
     C*
     C     XPPMOD        IFEQ      '47'
     C     XPPMOD        OREQ      '51'
     C     XPPMOD        OREQ      '62'
     C     XPPMOD        OREQ      '66'
     C     XPPMOD        OREQ      '75'
     C     XPPMOD        OREQ      '80'
     C*
     C     XPPMOD        IFEQ      '47'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C  N60              Z-ADD     999           Q
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
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EN'          QQC(Q)
     C* Err Msg EN: "Verify assistant as MD"
     C                   END                                                    END XPPMOD=80
     C*
     C                   ELSE                                                   ELSE XPPMOD
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C* PER P. 37 ANESTHESIA
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
     C  N60              Z-ADD     999           Q
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
     C                   END                                                    END XRTOTP  ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9A        READE     PCA09                                  60
     C  N60XRCPT4        IFGT      '01999'
     C                   LEAVE
     C                   END
     C*
     C                   END                                                    END *IN60
     C*
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
     C* ====================================================================
     C* PER P. 51 SURGERY
     C* GLOBAL payment includes: Local anesthesia, such as infiltration,
     C* digital or topical anesthesia.
     C*
     C     DHPRCD        IFGE      '62273'
     C     DHPRCD        ANDLE     '62279'
     C     DHPRCD        ORGE      '64400'
     C     DHPRCD        ANDLE     '64530'
     C                   Z-ADD     *ZEROS        COUNT
     C                   MOVE      '10040'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '10040'
     C     XRCPT4        ANDLE     '69979'
     C     XRDSEQ        IFNE      CADSEQ
     C     XRTOTP        ANDGT     *ZEROS
     C*
     C                   ADD       1             COUNT
     C                   END                                                    END NE DSEQ ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFGE      1
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C                   MOVE      '215'         XPEOMB
     C*** EOMB 215: "This procedure was paid under a complete code".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END COUNT GE 1
     C                   END
     C*
     C**
     C* ====================================================================
     C* PER P. 53 SURGERY - Separate procedures.
     C* If procedure is designated as "separate procedure",
     C* this procedure must not be paid as a separate service.
     C* But, when performed independently of, and not related to,
     C* other services, they may be billed and paid.
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
     C     XRCPT4        OREQ      '94250'
     C     XRCPT4        OREQ      '95829'
     C     XRCPT4        OREQ      '95851'
     C     XRCPT4        OREQ      '99195'
     C     XRCPT4        IFEQ      DHPRCD
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRCPT4        ORNE      DHPRCD
     C     XRTOTP        IFGT      0
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C* PER P. 219 RADIOLOGY - Complete procedures.
     C* Note: Payment for a complete procedure includes all pre-
     C* and post-injection services... These may not be billed
     C* separately.
     C*
     C     DHPRCD        IFEQ      '36410'
     C     DHPRCD        OREQ      '36415'
     C     DHPRCD        OREQ      '36800'
     C     DHPRCD        OREQ      '36810'
     C     DHPRCD        OREQ      '36821'
     C     DHPRCD        OREQ      '61055'
     C     DHPRCD        OREQ      '62284'
     C                   Z-ADD     *ZEROS        COUNT
     C                   MOVE      '70010'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '70010'
     C     XRCPT4        ANDLE     '76948'
     C     XRDSEQ        IFNE      CADSEQ
     C     XRTOTP        ANDGT     *ZEROS
     C*
     C                   Z-ADD     1             R                 5 0
     C     XRCPT4        LOOKUP    RADS(R)                                60
     C     *IN60         IFEQ      '1'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C                   MOVE      '215'         XPEOMB
     C*** EOMB 215: "This procedure was paid under a complete code".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END                                                    END *IN60
     C                   END                                                    END NE DSEQ ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END DHPRCD
     C*
     C* ====================================================================
     C* PER P. 57 SURGERY
     C* Casting and Strapping - Casts and traction devices are included
     C* in the initial procedure code code.
     C*
     C     DHPRCD        IFGE      '29000'
     C     DHPRCD        ANDLE     '29799'
     C                   Z-ADD     *ZEROS        COUNT
     C                   MOVE      '10040'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '10040'
     C     XRCPT4        ANDLE     '69979'
     C     XRDSEQ        IFNE      CADSEQ
     C     XRTOTP        ANDGT     *ZEROS
     C*
     C                   ADD       1             COUNT
     C                   END                                                    END NE DSEQ ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFGE      1
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
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
     C                   MOVE      '215'         XPEOMB
     C*** EOMB 215: "This procedure was paid under a complete code".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END COUNT GE 1
     C                   END
     C* ===================================================================
     C* PER P 272 TNS
     C* Payment for TNS testing and training is limited to 4 sessions
     C* per injury.
     C* NOTE: Read all FEINS
     C     DHPRCD        IFEQ      '64550'
     C                   Z-ADD     *ZEROS        COUNT
     C     CAKY9E        SETLL     PCA09                              60
     C     CAKY9E        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFEQ      '64550'
     C     XRDSEQ        IFNE      CADSEQ
     C     XRTOTP        ANDGT     *ZEROS
     C*
     C                   ADD       1             COUNT
     C                   END                                                    END NE DSEQ ...
     C                   END                                                    END EQ CPT4 ...
     C     CAKY9E        READE     PCA09                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFGE      4
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '54'          QQC(Q)
     C*** Err Msg 54: "Exceeds number of procedures allowed
     C***              in time period"
     C*
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
     C                   GOTO      SURGE
     C                   END                                                    END COUNT GE 4
     C                   END
     C* ===============================================================
     C*
     C* PER P. 57 BURNS, LOCAL TREATMENT
     C* Major debridement of foreign bodies, grease, epidermis...
     C* Modifier -51 does not apply. (NOTE: Do not reduce in SUR2)
     C* (NOTE: xxpyal is used to skip surg2
     C                   MOVE      'N'           XXPYAL            1
     C     DHPRCD        IFGE      '11000'
     C     DHPRCD        ANDLE     '11042'
     C     DHPRCD        ORGE      '15050'
     C     DHPRCD        ANDLE     '15261'
     C     DHPRCD        ORGE      '16000'
     C     DHPRCD        ANDLE     '16030'
     C                   Z-ADD     *ZEROS        COUNT
     C                   Z-ADD     *ZEROS        COUNT1
     C                   MOVE      '11000'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '11000'
     C     XRCPT4        ANDLE     '11042'
     C     XRCPT4        ORGE      '15050'
     C     XRCPT4        ANDLE     '15261'
     C*
     C                   ADD       1             COUNT
     C                   END                                                    END GE CPT4 ...
     C     XRCPT4        IFGE      '16000'
     C     XRCPT4        ANDLE     '16030'
     C*
     C                   ADD       1             COUNT1
     C                   END                                                    END GE CPT4 ...
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFGE      1
     C     COUNT1        ANDGE     1
     C                   MOVE      'Y'           XXPYAL            1
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EL'          QQC(Q)
     C* Err Msg EL: "Must verify degree of burn"
     C*
     C*
     C                   END                                                    END COUNT GE 1
     C                   END
     C*
     C*
     C* NOTE: ALL SURGERY PROCEDURES ARE WRITTEN TO THE WORKFILE
     C*       EXCEPT THOSE WITH XXPYAL = 'Y'
     C*
     C     XXPYAL        IFEQ      'N'
     C* ===============================================================
     C* Write record to Workfile
     C* Code has passed all edits - Write to the FACAPSC Workfile
     C* Only load "current charges" into the FACAPSC Workfile
     C*
     C*
     C                   MOVE      'S'           CATYPE
     C                   Z-ADD     SXMRA         CAMRA
     C                   Z-ADD     CACHRG        CATOTC
     C*
     C                   WRITE     PCAWK1
     C*
     C* Clear the payment to process correctly in SURG2
     C                   Z-ADD     0             XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   EXSR      UPDAT
     C*
     C                   GOTO      SURGE
     C*
     C                   END                                                    END XXPYAL
     C*
     C     NOCUT         TAG
     C*
     C* =================================================================
     C* MODIFIERS WHICH CAUSE A CHANGE IN THE MAP
     C*
     C     XPPMOD        IFEQ      '47'
     C                   MULT      .50           SXMRA
     C                   ENDIF
     C*
     C     XPPMOD        IFEQ      '62'
     C                   MULT      .60           SXMRA
     C                   ENDIF
     C*
     C     XPPMOD        IFEQ      '80'
     C                   MULT      .20           SXMRA
     C                   ENDIF
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
     C     CAKY9C        SETLL     PCA09
     C     CAKY9D        READE     PCA09                                  61
     C     *IN61         DOWEQ     '0'
     C     XRCPT4        IFLE      '69979'
     C     XRTOTP        IFNE      *ZEROS
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
     C     P2BRP         IFEQ      .50
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     P2DSEQ        QQS(Q)
     C                   MOVE      'DZ'          QQC(Q)
     C* Err Msg DZ: "Review Operative Report/Multiple Surgical Rule".
     C*
     C                   END                                                    END P2BRP   # =  0
     C*
     C* PER P 53 SURGERY
     C* The surgical assistant will be paid 20% of the MAP amount for
     C* the initial procedure and 10% for subsequent procedures.
     C*
     C     P2PMOD        IFEQ      '80'
     C     P2BRP         IFEQ      1
     C                   Z-ADD     .20           P2BRP                          1st: 20%
     C                   ENDIF
     C     P2BRP         IFEQ      .50
     C                   Z-ADD     .10           P2BRP                          2nd: 10%
     C                   ENDIF
     C                   ENDIF
     C*
     C     P2MRA         MULT      P2BRP         P2MRA
     C*
     C* ===============================================================
     C* MODIFIERS WHICH CAUSE A CHANGE IN THE MAP
     C* NOTE: THESE CHANGES OCCUR AFTER THE PRIMARY/SECONDARY
     C* PROCEDURES ARE FOUND (PRIMARY = HIGHEST MAP)
     C*
     C     P2PMOD        IFEQ      '47'
     C                   MULT      .50           P2MRA
     C                   ENDIF
     C*
     C     P2PMOD        IFEQ      '62'
     C                   MULT      .60           P2MRA
     C                   ENDIF
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
     C*
     C* Write EOMB to FDDEP file by sequence number
     C     XREOMB        IFNE      *BLANKS
     C     XXPASS        ANDEQ     '2'
     C*
     C                   MOVE      P2EOMB        XPEOMB
     C                   Z-ADD     P2DSEQ        KKDSEQ
     C                   EXSR      EOMB
     C*
     C                   ENDIF
     C*
     C                   MOVE      Q1USER        XRUSER
     C                   MOVE      Q1PGM         XRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XRDT
     C                   Z-ADD     DSTIME        XRTIME
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XCMRA         CAMRAA
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
     C*
      *
     C     XPEOMB        IFNE      *BLANKS
     C     XXPASS        ANDEQ     '2'
     C                   Z-ADD     SVDSEQ        KKDSEQ
     C                   EXSR      EOMB
      *
     C                   ENDIF
     C*
     C* ===============================================================
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM.
     C** REMOVED MODIFIER FROM DUPLICATE CLAIM EDIT
     C**
     C*
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     DHKY2         CHAIN     PDH03                              54
     C*
     C     *IN54         DOWEQ     *OFF
     C*
     C                   Z-ADD     D1DSEQ        KKDSEQ
     C*
     C     CAKYP         CHAIN     PCA0P                              61
     C*
     C     *IN61         IFEQ      *OFF
     C*
     C     PXFDAT        ANDEQ     CAFDAT
     C     PXFEIN        ANDEQ     CAFEIN
     C     PXFLAG        ANDNE     'P'
     C     PXDSEQ        ANDNE     CADSEQ
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
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
     C* If Fee Schedule non-reimburseable ... display error message
     C* XX25EM Flag is set at beginning of program where PRFSR eq 'N'
     C     XX25EM        IFEQ      '1'
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     DHKY2         CHAIN     PDH03                              54
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C                   Z-ADD     D1DSEQ        KKDSEQ
     C*
     C     CAKYP         CHAIN     PCA0P                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     PXFDAT        ANDEQ     XXYMD
     C     PXFEIN        ANDEQ     CAFEIN
     C     PXMOD         ANDEQ     CAMOD
     C     PXPMOD        ANDEQ     XPPMOD
     C     PXTOTP        ANDNE     *ZEROS
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '25'          QQC(Q)
     C* Err Msg 25: "This item is non-reimburseable under the Fee Sched
     C                   MOVE      '0'           XX25EM
     C                   LEAVE
     C                   END                                                    *IN60=OFF
     C     DHKY2         READE     PDH03                                  54
     C                   ENDDO                                                  ENDIF *IN54
     C                   END                                                    XX25EM='1'
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
DLTOVR FILE(FACALSC)
OVRDBF FILE(FACALSC)  TOFILE(QTEMP/FACALSC)
DLTOVR FILE(FACAPSC)
OVRDBF FILE(FACAPSC)  TOFILE(QTEMP/FACAPSC)
**  RADS RADI CPT4 CODES (PERTAIN TO COMPLETE PROCEDURES)
70010
70015
70170
70332
70373
70390
71036
71040
71060
71090
72240
72255
72265
72270
72285
72295
73040
73085
73115
73525
73580
73615
74235
74300
74301
74305
73420
74327
74328
74329
74330
74340
74350
74360
74363
74425
74430
74440
74445
74450
74455
74470
74475
74480
74485
74740
74742
75600
75605
75625
75630
75650
75658
75660
75662
75665
75671
75676
75680
75685
75705
75710
75716
75722
75724
75726
75731
75733
75736
75741
75743
75746
75756
75774
75790
75801
75803
75805
75807
75809
75810
75820
75822
75825
75827
75831
75833
75840
75842
75860
75870
75872
75880
75885
75887
75889
75891
75893
75894
75896
75900
75940
75960
75961
75962
75964
75966
75968
75970
75978
75980
75982
75984
75989
75992
75993
75994
75995
75996
76080
76086
76088
76095
76096
76360
76365
76930
76932
76934
76938
76941
76942
76945
76946
76948
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
92325
92511
93561
94250
95829
95851
99195
