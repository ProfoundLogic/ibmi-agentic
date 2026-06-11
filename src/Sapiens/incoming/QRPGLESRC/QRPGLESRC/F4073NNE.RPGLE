     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*             I M P O R T A N T    N O T E                      *
     H*                                                               *
     H* TABLE PCD2 HAS BEEN CONVERTED TO REFLECT THE ACTUAL MAXIMUM   *
     H* DOLLAR AMOUNT PAYABLE PHYSICAL THERAPY CODES.  THIS TABLE     *
     H* WILL NEED TO BE UPDATED IF THE PHYSICAL THERAPY CONVERSION    *
     H* FACTOR CHANGES FROM YEAR TO YEAR.                             *
     H*                                                               *
     H*****************************************************************
     H*    PROGRAM: F4073RNE
     H*    DESCRIPTION: ADJUDICATION (NEBRASKA RULES - 11/15/95)
     H*   This program is called from F0070R (Claim Adjudication) and will
     H* perform the actual adjudication of the detail claim records writ-
     H* ten by F0070R.  This program was derrived from the NEBRASKA State
     H* Fee Schedule Reimbursment Manual issued on 11/15/95
     H* This Sub-program is intended to be used to calculate the amount
     H* that is due a Provder of services, by the Carrier of the Work comp
     H* insurance.  It should be used for any serice provided on or after
     H* 11/15/95.
     H*****************************************************************
     H*  CREATED: 12/18/95   C:41   PROJECT: NE004
     H*****************************************************************
     H*  SPECIALTY CODES USED IN THIS PROGRAM:
     H*
     H*  ANE - ANESTHESIOLOGIST
     H*  AS1 - FIRST SURGICAL ASSISTANT
     H*  RNA - CERTIFIED REGISTERED NURSE ANESTHETIST
     H*  PHT - LICENSED PHYSICAL THERAPIST
     H*  OT  - LICENSED OCCUPATIONAL THERAPIST
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  08/09/02 C:062 PROJECT: FS244
     H*                 USE CAZIPA.
     H*
     H*  09/25/01 C:062 PROJECT: FSMNT
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.
     H*
     H*  04/27/00 C:079 Proj: NE007
     H*                 Convert to use with provider bills- no rev comm
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
     H*  07/20/98  C:91  PROJECT: NE005
     H*                  Added END DATE due to creation of new 1998
     H*                  Fee Schedule
     H*
     H*  07/16/98  C:94  PROJECT: MC145
     H*                  CHANGED ALL SIX POSITION DATES TO SEVEN
     H*                  CHECKED OUT CVTDT PROGRAMS
     H*
     H*****************************************************************
     H*  INDICATOR USAGE:
     H*  54    Use with Primary FDCAP file (i.e. DOWEQ, etc on reading charges)
     H*  60    General Use
     H*  61    General Use
     H*  62    General Use
     H*****************************************************************
     H*  SECTION                                        SUBR         CODES
     H*  Anesthesia                                     ANES    00100 - 01999
     H*  Surgery                                        SURG    10040 - 69979
     H*                                                 SURG2   10040 - 69979
     H*  Radiology                                      RADI    70010 - 79999
     H*  Labratory and Pathology                        PATH    80002 - 89399
     H*  Medicine                                       MEDI    90701 - 99199
     H*  Evaluation and Mangement Services              OFFI    99201 - 99499
     H*  Physical Medicine                              PHYS    97000 - 97799
     H*                                                 PHYS2   97000 - 97799
     H*****************************************************************
     F*  FDCAL7 is the Primary File Read (PCA01)
     FFDCALH    UF   E           K DISK
     F*
     FFACAPNE   O    E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK1)
     FFACALNE   IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK2)
     FFACAL1NE  IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK3)
     FFACAL2NE  IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK4)
     FFDCAL6    IF   E           K DISK
     F                                     RENAME(PCA01:PCA06)
     FFDCAL5    UF   E           K DISK
     F                                     RENAME(PCA01:PCA08)
     FFDCAL9    IF   E           K DISK
     F                                     RENAME(PCA01:PCA09)
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0P)
     FFDCALA    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0A)
     FFMCML1    IF   E           K DISK
     F*** BEGIN DEL *** 08/09/02 **************************************
     F**********FMGZL   IF  E           K        DISK
     F*** END   DEL *** 08/09/02 **************************************
     FFMPDL     IF   E           K DISK
     FFMPRLNE   IF   E           K DISK
     FFMRCL     IF   E           K DISK
     FFTABLNE   IF   E           K DISK
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
     D PTSC            S              5    DIM(50) ASCEND                       PT Summary Cde
     D PTSM            S              9  2 DIM(50)                              PT Summary MRA
     D PTSS            S             14    DIM(50) DESCEND                      PT Summary
     D*
     D PCD1            S              5    DIM(24) CTDATA PERRCD(1) ASCEND      PT Code/Summary Code
     D SMCD            S              5    DIM(24) ALT(PCD1)
     D PCD2            S              5    DIM(24) CTDATA PERRCD(1) ASCEND      PT Code/Max MRA Valu
     D MRA             S              9  2 DIM(24) ALT(PCD2)
     D PCD3            S              5    DIM(13) CTDATA PERRCD(1) ASCEND      PT Code: Add to SmCd
     D*
     D* Compile-time Arrays
     D ANEPM1          S              2    DIM(6) CTDATA PERRCD(1)              ANES Px Modifiers
     D ANEPM2          S              1  0 DIM(6) ALT(ANEPM1)
     D QCA             S             55    DIM(8) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D NC1             S              5    DIM(76) CTDATA PERRCD(10)            NO CUT ON THESE
     D SUR1            S              5    DIM(363) CTDATA PERRCD(1)            NO CUT/MAJOR-MIN
     D SUR2            S              5    DIM(363) ALT(SUR1)
     D IPT1            S              5    DIM(12) CTDATA PERRCD(1)             Initial PT for 97001
     D IPT2            S              5    DIM(12) CTDATA PERRCD(1)             Initial PT for 97101
     D RAD1            S              7    DIM(28) CTDATA PERRCD(1)             RADI CODES/UNIT VAL
     D RUNT            S              9  2 DIM(28) ALT(RAD1)
     D RAD2            S              7    DIM(28) CTDATA PERRCD(1)             RADI CODES/P.C.UNIT
     D RUNT1           S              9  2 DIM(28) ALT(RAD2)
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*  Physical Medicine field for sorting array
     D                 DS
     D  DSSORT                 1     14
     D  DSMRA                  1      9  2
     D  DSCPT4                10     14

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
     I              CATOTC                      X6TOTC
     I              CATOTD                      X6TOTD
     I              CATOTP                      X6TOTP
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
     I              CACHRG                      X6CHRG
     I              CAPAID                      X6PAID
     I              CAMRA                       X6MRA
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
     I* Record Format for SURG2 Workfile FACALNE
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
     I              CAMMRA                      P2MMRA
     I              CABREC                      P2BREC
     I              CABRP                       P2BRP
     I              CASMCD                      P2SMCD
     I              CATYPE                      P2TYPE
     I*
     I* Record Format for PHYS2 Workfile FACAL1NE
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
     I              CAPGM                       P3PGM
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
     I              CAMMRA                      P3MMRA
     I              CABREC                      P3BREC
     I              CABRP                       P3BRP
     I              CASMCD                      P3SMCD
     I              CATYPE                      P3TYPE
     I* Record Format for ANES2 Workfile FACAL2NE
     IPCAWK4
     I              CACO#                       P4CO#
     I              CASUB#                      P4SUB#
     I              CAFND                       P4FND
     I              CAFYR                       P4FYR
     I              CACASE                      P4CASE
     I              CACSCD                      P4CSCD
     I              CAFDAT                      P4FDAT
     I              CATDAT                      P4TDAT
     I              CARDAT                      P4RDAT
     I              CAIPOP                      P4IPOP
     I              CAFEIN                      P4FEIN
     I              CAMOD                       P4MOD
     I              CAPRTY                      P4PRTY
     I              CATRCD                      P4TRCD
     I              CASVCD                      P4SVCD
     I              CAPMOD                      P4PMOD
     I              CACPT4                      P4CPT4
     I              CAICD9                      P4ICD9
     I              CAANMN                      P4ANMN
     I              CAUNTS                      P4UNTS
     I              CATOTC                      P4TOTC
     I              CATOTA                      P4TOTA
     I              CATOTD                      P4TOTD
     I              CAEOMB                      P4EOMB
     I              CATOTP                      P4TOTP
     I              CADSEQ                      P4DSEQ
     I              CADELF                      P4DELF
     I              CAFLAG                      P4FLAG
     I              CACHK#                      P4CHK#
     I              CAEOB#                      P4EOB#
     I              CAPART                      P4PART
     I              CAP1                        P4P1
     I              CASS#                       P4SS#
     I              CAACDT                      P4ACDT
     I              CASTAT                      P4STAT
     I              CAUSER                      P4USER
     I              CAPGM                       P4PGM
     I              CATIME                      P4TIME
     I              CADT                        P4DT
     I              CAIUSR                      P4IUSR
     I              CAIDT                       P4IDT
     I              CADXR                       P4DXR
     I              CADCS#                      P4DCS#
     I              CALTR#                      P4LTR#
     I              CARCFL                      P4RCFL
     I              CAOTHR                      P4OTHR
     I              CANDIS                      P4NDIS
     I              CARDTP                      P4RDTP
     I              CABRSV                      P4BRSV
     I              CABRPM                      P4BRPM
     I              CABRPC                      P4BRPC
     I              CABRAM                      P4BRAM
     I              CABRUN                      P4BRUN
     I              CABRTC                      P4BRTC
     I              CAZIPA                      P4ZIPA
     I              CAAST                       P4AST
     I              CADIFF                      P4DIFF
     I              CABRPT                      P4BRPT
     I              CADAFL                      P4DAFL
     I              CAMRA                       P4MRA
     I              CAMMRA                      P4MMRA
     I              CABREC                      P4BREC
     I              CABRP                       P4BRP
     I              CASMCD                      P4SMCD
     I              CATYPE                      P4TYPE

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
     C*** Used with PCAPT2 (FACALNE - Detail Charge Workfile) in PHYS2
     C     CAKYP2        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    KKDSEQ
     C*** Used with PCAPT3 (FACAL1FL - Detail Charge Workfile) in PHYS3
     C     CAKYP3        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXDSEQ
     C*** Used with PCAPT2 (FACALNE - Detail Charge Workfile) in PHYS2
     C     CAKYP4        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    HDYMD
     C                   KFLD                    HDFEIN
     C                   KFLD                    HDMOD
     C                   KFLD                    XXSMCD
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
     C     CAKEY7        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C*                                                                *****
     C     CAKYA3        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    P3DSEQ
     C*                                                                *****
     C     CAKYA4        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    P4EOB#
     C                   KFLD                    P4DSEQ
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
     C     CAKYA         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    SXFEIN
     C                   KFLD                    SXMOD
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
     C     *LIKE         DEFINE    CAFDAT        H2FDAT
     C     *LIKE         DEFINE    CAFDAT        HDYMD
     C     *LIKE         DEFINE    CAFDAT        SVYMD
     C     *LIKE         DEFINE    CAMOD         HLMOD
     C     *LIKE         DEFINE    CAMOD         HDMOD
     C     *LIKE         DEFINE    CAPRTY        HLPRTY
     C     *LIKE         DEFINE    DHAREA        HLPART
     C     *LIKE         DEFINE    DHAREA        HLDPRT
     C     *LIKE         DEFINE    CATOTA        HLTOTA
     C     *LIKE         DEFINE    PRMRA         HLMRA
     C     *LIKE         DEFINE    PRMRA         HLDMRA
     C     *LIKE         DEFINE    CAANMN        XGT180
     C     *LIKE         DEFINE    CAANMN        XLE180
     C     *LIKE         DEFINE    DHPRCD        XXSMCD
     C     *LIKE         DEFINE    DHPRCD        ZZCPT4
     C*
     C     *LIKE         DEFINE    DHPRCD        PPCPT4
     C     *LIKE         DEFINE    CADSEQ        PPDSEQ
     C     *LIKE         DEFINE    CAFDAT        PPFDAT
     C     *LIKE         DEFINE    CAFEIN        PPFEIN
     C     *LIKE         DEFINE    CAMOD         PPMOD
     C*
     C     *LIKE         DEFINE    CACHK#        PTCHK#
     C     *LIKE         DEFINE    CADSEQ        PTDSEQ
     C     *LIKE         DEFINE    CAEOB#        PTEOB#
     C     *LIKE         DEFINE    CAFEIN        PTFEIN
     C     *LIKE         DEFINE    CAMOD         PTMOD
     C     *LIKE         DEFINE    CAPRTY        PTPRTY
     C*
     C     *LIKE         DEFINE    PRANAM        SXANAM
     C     *LIKE         DEFINE    CAFEIN        SXFEIN
     C     *LIKE         DEFINE    CAMOD         SXMOD
     C     *LIKE         DEFINE    PRMRA         SXMRA
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
     C     *LIKE         DEFINE    DHUNTS        SVUNTS
     C*
     C     *LIKE         DEFINE    PMKEY         TABKEY
     C*
     C     *LIKE         DEFINE    DHPRCD        XPCPT4
     C     *LIKE         DEFINE    CAEOMB        XPEOMB
     C     *LIKE         DEFINE    CAPMOD        XPPMOD
     C     *LIKE         DEFINE    CAPMOD        X1PMOD
     C     *LIKE         DEFINE    CATOTA        XPTOTA
     C     *LIKE         DEFINE    CACHRG        XPTOTC
     C     *LIKE         DEFINE    CAMRA         XPTOTD
     C     *LIKE         DEFINE    CAPAID        XPTOTP
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
     C     *LIKE         DEFINE    CAFDAT        XXYMD
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
     C     *LIKE         DEFINE    PRMRA         X1MRA
     C     *LIKE         DEFINE    CAFDAT        WKFDAT
     C*
     C                   Z-ADD     0             Q                 4 0
     C                   Z-ADD     0             S                 3 0
     C                   Z-ADD     0             T                 5 0
     C                   Z-ADD     0             X                 5 0
     C                   Z-ADD     0             I                 3 0
     C                   Z-ADD     0             P                 5 0
     C                   Z-ADD     0             COUNT             5 0
     C                   Z-ADD     0             HLADDT            7 7          To hold MVR
     C                   Z-ADD     0             HLANMN           10 7          CAANMN for 15 minute
     C                   Z-ADD     0             HLANTV            3 0          Anes Time Value
     C                   MOVE      ' '           OPNFLG            1            File Open Flag
     C                   MOVE      '0'           OFCPD             1            CLEAR TMP IN(NOT USD
     C                   MOVE      ' '           OVRFLG            1            File Override Flag
     C                   MOVE      *BLANKS       SVSPCD            3
     C                   MOVE      ' '           XFIRST            1            First time Wrkf buil
     C                   Z-ADD     0             XXALOW            9 2
     C                   Z-ADD     0             XXANMN            5 2
     C                   MOVE      *BLANKS       XXEND             2
     C                   Z-ADD     0             XXMODU            2 0
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
     C                   MOVE      *BLANKS       X25CUT            1
     C                   MOVE      *BLANKS       X50CUT            1
     C                   Z-ADD     0             M95834            9 2          MRA for 95834
     C                   Z-ADD     0             FUTRCT            5 0
     C                   Z-ADD     0             PASTCT            5 0
     C                   Z-ADD     0             PHYSCT            5 0
     C*************        MOVE *BLANKS   SRDTE   6        DEL 07/16/98
     C                   MOVE      *BLANKS       SRDTE             7
     C                   MOVE      *BLANKS       SRERR             1
     C                   MOVE      *BLANKS       SRTERM            1
     C                   Z-ADD     0             SRFUT             5 0
     C                   Z-ADD     0             SRPST             5 0
     C                   Z-ADD     0             T95831            9 2          Tot for 95831 series
     C                   Z-ADD     0             PPHYMD           10 2
     C                   Z-ADD     0             LPHYMD           10 2
     C                   Z-ADD     0             MAXADD            5 0
     C                   Z-ADD     0             XX75CT            9 2
     C                   MOVE      *BLANK        XX50              1            Flag to pay PT at 50
     C                   MOVE      *BLANKS       XXRDMD            7            RADI CPT4 AND MOD
     C*
     C                   Z-ADD     6.50          XXMEDI            5 2           MEDI CONVERSION
     C                   Z-ADD     6.00          XXPHYS            5 2           PHYS CONVERSION
     C                   Z-ADD     35.00         XXANES            5 2           ANES CONVERSION
     C                   Z-ADD     175.00        XXSURG            5 2           SURG CONVERSION
     C                   Z-ADD     37.00         XXRADI            5 2           RADI CONVERSION
     C                   Z-ADD     2.00          XXPATH            5 2           PATH CONVERSION
     C                   Z-ADD     6.50          XXOFFI            5 2           OFFI CONVERSION
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
     C                   EXSR      LODFU                                        LOAD FOLLOW-UP
     C*
     C* INITIALIZE VALUES FOR QQS, AND QQC
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
     C     CAFDAT        IFGE      951115
     C     CAFDAT        ANDLT     980615
     C     XXRCN         ANDEQ     CARCN
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
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     DHUNTS        SVUNTS
     C                   MOVEL     DHPRCD        XPCPT4
     C                   MOVEL     XPPMOD        X1PMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C*
     C                   EXSR      LODFU                                        LOAD FOLLOW-UP
     C*
     C                   Z-ADD     0             M95834
     C                   MOVE      QQSPCD        SVSPCD
     C*
     C*** GET MRA AMOUNT FOR SPECIAL RULE
     C                   MOVEL     '95834'       XXCPT4
     C                   MOVE      930801        XXEFDT
     C*
     C     PRKY          SETGT     PPR01
     C     NXTPR1        TAG
     C                   READP     PPR01                                  60     RECORD
     C     *IN60         IFEQ      '0'
     C     PRCPT4        ANDEQ     '95834'
     C     PRSTAT        CABEQ     'I'           NXTPR1
     C                   Z-ADD     PRMRA         M95834
     C                   ELSE
     C                   Z-ADD     *ZERO         M95834
     C                   END
     C                   Z-ADD     *ZERO         T95831
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
     C     XPPMOD        IFNE      *BLANK
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
     C                   ELSE                                                   ELSE XPPMOD NE *BLAN
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '91'          QQC(Q)
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   END                                                    END XPPMOD NE *BLANK
     C*
     C                   ELSE                                                   ELSE RCRPT EQ 'Y'
     C*
     C     CACHRG        COMP      RCMRA                              61
     C   61              MOVE      '229'         XPEOMB
     C* EOMB Code 229: "This charge has been paid per R&C fees"
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
     C     CAKY1A        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   END
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
     C     CAPRTY        IFEQ      '4'
     C                   GOTO      NXTRCD
     C*
     C*== Non-Hospital ================================================
     C*
     C                   ELSE                                                   Else eq '4'
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
     C   60              MOVE      '202'         XPEOMB
     C* EOMB Code 202: "Charges exceed Reasonable & Customary"
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
     C*--------------------------------------------------------------------
     C*-- By Report -------------------------------------------------------
     C     PRRPT         IFEQ      'Y'
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
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   ELSE
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
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END                                                    END PRRPT EQ 'A'
     C*
     C*--------------------------------------------------------------------
     C*-- Possible Bundled Codes ------------------------------------------
     C     PRRPT         IFEQ      'B'
     C*
     C*   If modifier exists check the code through the Modifier Subr
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C                   END
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '205'         XPEOMB
     C* EOMB Code 205: "This procedure is included in the basic allowance
     C*                 of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END                                                    END PRRPT EQ 'A'
     C*
     C*================================================================
     C*== Normal Processing of Procedure Code =========================
     C*
     C*-----------------------------------------------------------------
     C*-- Give Message for codes that end in '99' ---------------------
     C                   MOVE      DHPRCD        XXEND
     C     XXEND         IFEQ      '99'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '19'          QQC(Q)
     C* Err Msg 19: *WARNING* Need Letter of Medical Necessity to pay this item
     C                   END                                                    END XXEND EQ '99'
     C*
     C*-----------------------------------------------------------------
     C*-- Execute all Subroutines here ---------------------------------
     C*
     C     DHPRCD        IFGE      '00100'                                      ANESTHESIA
     C     DHPRCD        ANDLE     '01999'
     C     DHPRCD        ORGE      '99100'
     C     DHPRCD        ANDLE     '99140'
     C*-------------------------------------------------------------------
     C* If first time into program,
     C* or the first code after the workfile has been used
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the workfile is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALNE
     C                   CLOSE     FACAL1NE
     C                   CLOSE     FACAL2NE
     C                   CLOSE     FACAPNE
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL2NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACNE'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL2NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACAPNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALNE
     C                   OPEN      FACAL1NE
     C                   OPEN      FACAL2NE
     C                   OPEN      FACAPNE
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
     C                   EXSR      PHYS2
     C                   EXSR      ANES2
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALNE
     C                   CLOSE     FACAL1NE
     C                   CLOSE     FACAL2NE
     C                   CLOSE     FACAPNE
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL2NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACNE'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL2NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACAPNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALNE
     C                   OPEN      FACAL1NE
     C                   OPEN      FACAL2NE
     C                   OPEN      FACAPNE
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
     C                   EXSR      ANES
     C*
     C                   ELSE                                                      ELSE ANES
     C*
     C     DHPRCD        IFGE      '10040'                                      SURGERY
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
     C                   CLOSE     FACALNE
     C                   CLOSE     FACAL1NE
     C                   CLOSE     FACAL2NE
     C                   CLOSE     FACAPNE
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL2NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACNE'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL2NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACAPNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALNE
     C                   OPEN      FACAL1NE
     C                   OPEN      FACAL2NE
     C                   OPEN      FACAPNE
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
     C                   EXSR      PHYS2
     C                   EXSR      ANES2
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALNE
     C                   CLOSE     FACAL1NE
     C                   CLOSE     FACAL2NE
     C                   CLOSE     FACAPNE
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL2NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACNE'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL2NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACAPNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALNE
     C                   OPEN      FACAL1NE
     C                   OPEN      FACAL2NE
     C                   OPEN      FACAPNE
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
     C     DHPRCD        IFGE      '90701'                                      MEDICAL
     C     DHPRCD        ANDLE     '96999'
     C     DHPRCD        ORGE      '97800'
     C     DHPRCD        ANDLE     '99199'
     C                   EXSR      MEDI
     C*
     C                   ELSE                                                      ELSE MEDI
     C*
     C     DHPRCD        IFGE      '99201'                                      OFFICE
     C     DHPRCD        ANDLE     '99499'
     C                   EXSR      OFFI
     C*
     C                   ELSE                                                      ELSE OFFI
     C*
     C     DHPRCD        IFGE      '97000'                                      PHYS. THERAPY
     C     DHPRCD        ANDLE     '97799'
     C*-------------------------------------------------------------------
     C* If first time into program,
     C* or the first code after the workfile has been used
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the workfile is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALNE
     C                   CLOSE     FACAL1NE
     C                   CLOSE     FACAL2NE
     C                   CLOSE     FACAPNE
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL2NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACNE'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL2NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACAPNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALNE
     C                   OPEN      FACAL1NE
     C                   OPEN      FACAL2NE
     C                   OPEN      FACAPNE
     C                   MOVE      '1'           OPNFLG
     C                   END
     C*
     C*     then, reset hold YMD field and provider/mod
     C                   Z-ADD     XXYMD         HDYMD
     C                   Z-ADD     CAFEIN        HDFEIN
     C                   Z-ADD     CAMOD         HDMOD
     C*
     C*     then, reset Phys Medicine arrays
     C                   MOVE      *BLANKS       PTSC
     C                   MOVE      *ZEROS        PTSM
     C                   MOVE      *BLANKS       PTSS
     C*
     C                   ELSE                                                   ELS XFIRST EQ ' '
     C*
     C* If this is a new date or new provider/modifier
     C     XXYMD         IFNE      HDYMD
     C     CAFEIN        ORNE      HDFEIN
     C     CAMOD         ORNE      HDMOD
     C*     then, Process all Codes for the existing date/provider
     C                   EXSR      PHYS2
     C                   EXSR      SURG2
     C                   EXSR      ANES2
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALNE
     C                   CLOSE     FACAL1NE
     C                   CLOSE     FACAL2NE
     C                   CLOSE     FACAPNE
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL2NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACNE'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL2NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACAPNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALNE
     C                   OPEN      FACAL1NE
     C                   OPEN      FACAL2NE
     C                   OPEN      FACAPNE
     C                   MOVE      '1'           OPNFLG
     C                   END
     C*
     C*     then, reset hold YMD field and provider/mod
     C                   Z-ADD     XXYMD         HDYMD
     C                   Z-ADD     CAFEIN        HDFEIN
     C                   Z-ADD     CAMOD         HDMOD
     C*
     C*     then, reset Phys Medicine arrays
     C                   MOVE      *BLANKS       PTSC
     C                   MOVE      *ZEROS        PTSM
     C                   MOVE      *BLANKS       PTSS
     C*
     C                   END
     C                   END                                                    END XFIRST EQ ' '
     C*-------------------------------------------------------------------
     C                   EXSR      PHYS
     C*
     C                   ELSE                                                      ELSE PHYS
     C*
     C                   EXSR      OTHE                                         OTHER CODES
     C*
     C                   END                                                       END PHYS
     C                   END                                                       END OFFI
     C                   END                                                       END MEDI
     C                   END                                                       END PATH
     C                   END                                                       END RADI
     C                   END                                                       END SURG
     C                   END                                                       END ANES
     C*
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
     C                   Z-ADD     D1DSEQ        KKDSEQ
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
     C  N60              Z-ADD     999           Q
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
     C                   Z-ADD     0             XPTOTA
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
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA01
     C*
     C                   END
     C                   END                                                    END EQ OVRRD
     C                   END
     C*
     C*** Process next record *****************************************
     C     NXTRCD        TAG
     C*
     C*   Before next record is read, save the key fields needed to
     C*   retrieve the physical therapy records to be updated in the
     C*   PHYS2 routine which is executed on a provider level break...
     C                   Z-ADD     CADSEQ        PTDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        PTFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         PTMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        PTPRTY
     C                   Z-ADD     CAEOB#        PTEOB#
     C                   Z-ADD     CACHK#        PTCHK#
     C*
     C*
     C     CAKYH1        READE     PCA01                                  54      GET NEXT RCD
     C*                                                      TO PROCESS
     C                   ENDDO
     C*
     C*** Execute SURG2 if a SURG Code has been processed
     C     OPNFLG        IFEQ      '1'
     C                   EXSR      SURG2                                        Process the SURG cod
     C                   EXSR      PHYS2                                        Process the PHYS cod
     C                   EXSR      ANES2                                        Process the ANES cod
     C                   END                                                    END OPNFLG EQ '1'
     C*
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
     C                   CLOSE     FACALNE
     C                   CLOSE     FACAL1NE
     C                   CLOSE     FACAL2NE
     C                   CLOSE     FACAPNE
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL2NE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPNE
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*---------------------------------------------------------------------
     C*
     C* Write adjudication messages to FDMGP
     C                   EXSR      MSG
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     0             XXMRA
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
     C* Per PG. 62-63
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '23'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '75'
     C     XPPMOD        ANDNE     'P1'
     C     XPPMOD        ANDNE     'P2'
     C     XPPMOD        ANDNE     'P3'
     C     XPPMOD        ANDNE     'P4'
     C     XPPMOD        ANDNE     'P5'
     C     XPPMOD        ANDNE     'P6'
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
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C*
     C                   ELSE                                                   ELSE EQ 22...
     C*----------------------------------------------------------------
     C* Per pg. 62.B,I...also Relative Value Guide, pg. vii
     C* If modifier - Px is used, add additional Time Units to calculation
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
     C*=== Specialty Code conditioned ======================================
     C*
     C*  Per pg. 61, 1:
     C*  ANE and RNA use the values set forth under the respective
     C*  anesthesia charge (as dictated by the ASA Relative Guide 1991)
     C*           ANE - Anesthesiologist
     C*           RNA - Certified Registered Nurse Anesthetist
     C*
     C     SVSPCD        IFEQ      'ANE'
     C     SVSPCD        OREQ      'RNA'
     C*
     C*=====================================================================
     C*   Per page 63,11: Qualifying circumstances are not multipled by Time
     C     DHPRCD        IFGE      '99100'
     C     DHPRCD        ANDLE     '99140'
     C*
     C* SETLL and read the file testing for a ADDITIONAL ANES CODE
     C*
     C                   MOVE      '00100'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C*
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     XRCPT4        IFGE      '00100'
     C     XRCPT4        ANDLE     '01999'
     C*
     C     SXMRA         MULT      XXANES        SXMRA
     C                   GOTO      NOCUT2
     C                   END                                                    END GE 00100
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END 0 DOWEQ 60
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '207'         XPEOMB
     C*** EOMB 207: "A major anesthesia code must be present to pay this
     C***            item."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C                   END                                                    END GE 99100
     C*
     C*=====================================================================
     C* PER PAGE 61, 6
     C* MONITORING SERVICES PAY ONLY TIME UNITS
     C     DHAREA        IFEQ      'MS'
     C                   Z-ADD     *ZEROS        SXANAM
     C                   END
     C*
     C*=====================================================================
     C* PER PAGE 62, 10, A
     C* ANY PROCEDURE REQUIRING FIELD AVOIDANCE WILL HAVE A BASE UNIT VALUE
     C* OF 5.0 IF FS VALUE IS LESS THAN FIVE ELSE WILL NOT CHAGE
     C     DHAREA        IFEQ      'FA'
     C     SXANAM        IFLT      5.0
     C                   Z-ADD     5.0           SXANAM
     C                   END
     C                   END
     C*
     C*=====================================================================
     C*
     C     SXANAM        MULT      XXANES        CAMRA
     C                   Z-ADD     SXANAM        CAMMRA
     C                   Z-ADD     CACHRG        CATOTC
     C                   Z-ADD     DHUNTS        CAANMN
     C                   MOVE      'A'           CATYPE
     C                   WRITE     PCAWK1
     C                   GOTO      ANESE
     C*
     C                   ELSE                                                   ELSE EQ 'ANE'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AK'          QQC(Q)
     C* Err Msg AK  "Anethesia administered by other than Anesthesiologist
     C*             or Anesthetist is included in the unit value for the surg."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C*
     C                   END                                                    END EQ ANE
     C*
     C*===============================================================
     C* Calculate Payment
     C*
     C     NOCUT2        TAG
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
     C********************************************************************
     C** Process ANES Codes 00100-01999             A N E S 2          2 *
     C********************************************************************
     C     ANES2         BEGSR
     C*
     C*----------------------------------------------------------------
     C*--- Reading the ANES Detail Charge Workfile --------------------
     C*
     C                   Z-ADD     0             COUNT
     C* Read ANES workfile record
     C*
     C     *LOVAL        SETLL     PCAWK4
     C                   READ      PCAWK4                                 60
     C     *IN60         DOWEQ     '0'
     C*
     C*================================================================
     C*=== Calculating Time Units =====================================
     C*
     C* Per pg. 34, 1:                                                 -------
     C*
     C                   Z-ADD     *ZEROS        XGT180
     C                   Z-ADD     *ZEROS        XLE180
     C                   Z-ADD     *ZEROS        H1ANTV
     C*
     C     P4ANMN        IFGT      180
     C     P4ANMN        SUB       180           XGT180
     C                   Z-ADD     180           XLE180
     C                   ELSE                                                   ELSE GT 180
     C                   Z-ADD     P4ANMN        XLE180
     C                   END                                                    END GT 180
     C*
     C*  1.0 Unit for each 15 minutes or any fraction thereof under 3 hours
     C     XLE180        DIV       15            HLANMN           10 7
     C** Round up all fractions of 15 minutes to an additional Time Unit
     C                   MVR                     HLADDT            7 7
     C                   ADD       HLANMN        HLADDT
     C                   Z-ADD     HLANMN        HLANTV            3 0          Anes Time Value
     C     HLADDT        IFGT      *ZEROS
     C                   ADD       1             HLANTV
     C                   END
     C*
     C*  1.0 Unit for each 10 minutes or any fraction thereof over 3 hours
     C     XGT180        IFGT      *ZERO
     C     XGT180        DIV       10            H1ANMN           10 7
     C** Round up all fractions of 10 minutes to an additional Time Unit
     C                   MVR                     H1ADDT            7 7
     C                   ADD       H1ANMN        H1ADDT
     C                   Z-ADD     H1ANMN        H1ANTV            3 0          Anes Time Value
     C     H1ADDT        IFGT      *ZEROS
     C                   ADD       1             H1ANTV
     C                   END                                                    END H1ADDT GT *ZERO
     C                   END                                                    END XGT180 GT *ZERO
     C     HLANTV        ADD       H1ANTV        HLANTV
     C*
     C*=====================================================================
     C* PER PAGE 61, 6
     C* MONITORING SERVICES PAY ONLY TIME UNITS
     C     P4PART        IFEQ      'MS'
     C                   Z-ADD     *ZEROS        P4MMRA
     C                   END
     C*
     C*=====================================================================
     C** Basic Value + Time Units = Total Anesthesia Value
     C     COUNT         IFEQ      0
     C     P4MMRA        ADD       HLANTV        XXWORK           10 7
     C                   ADD       1             COUNT
     C                   ELSE
     C                   Z-ADD     HLANTV        XXWORK
     C                   END
     C*
     C** Total Anesthesia Value * Conversion Factor = Fee
     C     XXWORK        MULT(H)   XXANES        SXMRA
     C*
     C* Per pg. 61, 1:                                                 -------
     C*  When anesthesia is administered by a CRNA, bill at 90% of     -------
     C*  calculated fee                                                -------
     C     SVSPCD        IFEQ      'RNA'
     C                   MULT      .90           SXMRA
     C                   END                                                    END EQ RNA
     C*
     C*
     C     P4TOTC        COMP      SXMRA                              54        >>>>>
     C   54              Z-ADD     SXMRA         XPTOTA
     C  N54              Z-ADD     P4TOTC        XPTOTA
     C     P4TOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C* Update the corresponding FDCAP record with the values calculated
     C* for the FACAPNE record
     C                   EXSR      UPDAT3
     C*
     C                   READ      PCAWK4                                 60
     C                   END                                                    END DOWEQ 0 *IN60
     C*
     C     ANES2E        ENDSR
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
     C*
     C     LODFU         BEGSR
     C*
     C*** LOAD ARRRAYS WITH FOLLOW UP DAYS
     C*
     C                   MOVE      *ZERO         FUF
     C                   MOVE      *ZERO         FUT
     C                   Z-ADD     1             U                 5 0
     C*
     C     CAKYA         CHAIN     PCA0A                              60
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
     C     CAKYA         READE     PCA0A                                  60
     C                   END
     C*
     C     LODFUE        ENDSR
     *********************************************************************
     *** Medical                    Codes: 90701 - 99199         M E D I *
     *********************************************************************
     C     MEDI          BEGSR
     C*
     C     SXMRA         MULT      XXMEDI        SXMRA
     C*
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per PG. 257
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '27'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '55'
     C     XPPMOD        ANDNE     '56'
     C     XPPMOD        ANDNE     '75'
     C     XPPMOD        ANDNE     '76'
     C     XPPMOD        ANDNE     '77'
     C     XPPMOD        ANDNE     '78'
     C     XPPMOD        ANDNE     '79'
     C     XPPMOD        ANDNE     '90'
     C     XPPMOD        ANDNE     '99'
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
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   ELSE                                                   ELSE EQ 22...
     C                   EXSR      PMOD
     C     XPPMOD        IFEQ      '26'
     C     SXMRA         MULT      XXMEDI        SXMRA
     C                   END
     C*
     C                   END                                                    END EQ 22
     C*
     C                   END                                                    END NE *BLANK
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C* Update charge with payment
     C                   EXSR      UPDAT
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
     C                   MOVE      QQSPCD        SVSPCD                         CHG 12/28/92
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
     *** Office                     Codes: 99201 - 99499         O F F I *
     *********************************************************************
     C     OFFI          BEGSR
     C*
     C     SXMRA         MULT      XXOFFI        SXMRA
     C*
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per PG. 257
     C     XPPMOD        IFNE      '21'
     C     XPPMOD        ANDNE     '24'
     C     XPPMOD        ANDNE     '25'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '52'
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
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   ELSE                                                   ELSE EQ 22...
     C                   EXSR      PMOD
     C*
     C                   END                                                    END EQ 22
     C*
     C                   END                                                    END NE *BLANK
     C*
     C* PER PG 61, 2
     C* ===============================================================
     C* DISALLOW OFFICE VISIT THAT IS SUBMITTED BY AN 'ANE' OR 'RNA'
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99205'
     C     DHPRCD        ORGE      '99221'
     C     DHPRCD        ANDLE     '99223'
     C     DHPRCD        ORGE      '99241'
     C     DHPRCD        ANDLE     '99255'
     C     DHPRCD        ORGE      '99281'
     C     DHPRCD        ANDLE     '99285'
     C     DHPRCD        ORGE      '99301'
     C     DHPRCD        ANDLE     '99303'
     C     DHPRCD        ORGE      '99321'
     C     DHPRCD        ANDLE     '99323'
     C     DHPRCD        ORGE      '99341'
     C     DHPRCD        ANDLE     '99343'
     C     DHPRCD        ORGE      '99211'
     C     DHPRCD        ANDLE     '99215'
     C     DHPRCD        ORGE      '99231'
     C     DHPRCD        ANDLE     '99233'
     C     DHPRCD        ORGE      '99261'
     C     DHPRCD        ANDLE     '99263'
     C     DHPRCD        ORGE      '99281'
     C     DHPRCD        ANDLE     '99285'
     C     DHPRCD        ORGE      '99311'
     C     DHPRCD        ANDLE     '99313'
     C     DHPRCD        ORGE      '99331'
     C     DHPRCD        ANDLE     '99333'
     C     DHPRCD        ORGE      '99351'
     C     DHPRCD        ANDLE     '99353'
     C*
     C     SVSPCD        IFEQ      'ANE'
     C     SVSPCD        OREQ      'RNA'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C*** EOMB 205: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END                                                    END EQ 'ANE'
     C*
     C                   END                                                    END GE '99201'
     C*
     C* =================================================================
     C*
     C*** The number in that column (FU) establishes the days during which no
     C*** additional reimbursement is allowed for the usual care provided
     C*** following surgery, absent complications or unusual circumstances.
     C*
     C     DHPRCD        IFGE      '99211'
     C     DHPRCD        ANDLE     '99215'
     C     DHPRCD        ORGE      '99231'
     C     DHPRCD        ANDLE     '99233'
     C     DHPRCD        ORGE      '99261'
     C     DHPRCD        ANDLE     '99263'
     C     DHPRCD        ORGE      '99281'
     C     DHPRCD        ANDLE     '99285'
     C     DHPRCD        ORGE      '99311'
     C     DHPRCD        ANDLE     '99313'
     C     DHPRCD        ORGE      '99331'
     C     DHPRCD        ANDLE     '99333'
     C     DHPRCD        ORGE      '99351'
     C     DHPRCD        ANDLE     '99353'
     C*
     C                   Z-ADD     1             U
     C     U             DOWLE     500
     C     FUF(U)        ANDGT     *ZERO
     C*
     C     CAFDAT        IFGT      FUF(U)
     C     CAFDAT        ANDLE     FUT(U)
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '203'         XPEOMB
     C*** EOMB 203: "This is a duplicate of another procedure/charge"
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '09'          QQC(Q)
     C*** Err Msg 09: "This visit occurred during another procedures
     C***              follow-up period"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END
     C*
     C                   ADD       1             U
     C*
     C                   END
     C*
     C                   END
     C*
     C*==================================================================
     C*
     C*** Per pg. 256, B, 1
     C*** When a starred procedure is perfromed at the time of the initial
     C*** visit, and this procedure constitutes the major service rendered
     C*** during the intial visit, Procedure Code 99025 must be used instead
     C*** of the usual intial office visit code.
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99205'
     C     DHPRCD        ORGE      '99221'
     C     DHPRCD        ANDLE     '99223'
     C     DHPRCD        ORGE      '99241'
     C     DHPRCD        ANDLE     '99255'
     C     DHPRCD        ORGE      '99281'
     C     DHPRCD        ANDLE     '99285'
     C     DHPRCD        ORGE      '99301'
     C     DHPRCD        ANDLE     '99303'
     C     DHPRCD        ORGE      '99321'
     C     DHPRCD        ANDLE     '99323'
     C     DHPRCD        ORGE      '99341'
     C     DHPRCD        ANDLE     '99343'
     C*
     C* SETLL AND READ THE FILE TESTING FOR AN '*' PROCEDURE.
     C*
     C                   MOVE      '10000'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     AGAIN3        TAG
     C*
     C     CAKEY7        READE     PCA09                                  54
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
     C                   MOVE      '216'         XPEOMB
     C*** EOMB 216: "Payment is made for code 99025 instead of O/V per Fee Sche"
     C*
     C     PRKY          SETGT     PPR01
     C     NXTPR4        TAG
     C*   NEED TO GET MRA FOR 99025 - CPT4 CODE WAS CHANGED ...
     C*
     C                   READP     PPR01                                  54
     C*
     C     *IN54         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTPR4
     C                   ELSE
     C                   Z-ADD     *ZERO         SXMRA
     C                   Z-ADD     *ZERO         SXANAM
     C                   Z-ADD     *ZERO         SXPROF
     C                   MOVE      *BLANK        SXRPT
     C                   MOVE      *BLANK        SXSUPP
     C                   MOVE      *BLANK        SXSURG
     C                   MOVE      *BLANK        SX1A1                          11/04/91 ADD
     C                   END
     C*
     C** SAVE THE PROCEDURE CODE FILE VALUES.
     C*
     C                   Z-ADD     PRMRA         SXMRA
     C                   Z-ADD     PRANAM        SXANAM
     C                   Z-ADD     PRPROF        SXPROF
     C                   MOVE      PRRPT         SXRPT
     C                   MOVE      PRSUPP        SXSUPP
     C                   MOVE      PRSURG        SXSURG
     C                   MOVE      PR1A1         SX1A1                          11/04/91 ADD
     C     SXMRA         MULT      XXOFFI        SXMRA
     C*
     C*
     C                   ELSE
     C                   GOTO      AGAIN3
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*================================================================
     C*--- Follow-up Office Visit with * Procedure
     C*
     C* Per pg. 256, B, 3
     C* When the starred (*) procedure is carried out at the time of a
     C* follow-up (established patient) visit and this procedure constitutes
     C* the major service at that visit, the service visit is usually not
     C* added
     C*
     C     DHPRCD        IFGE      '99211'
     C     DHPRCD        ANDLE     '99215'
     C     DHPRCD        ORGE      '99231'
     C     DHPRCD        ANDLE     '99233'
     C     DHPRCD        ORGE      '99261'
     C     DHPRCD        ANDLE     '99263'
     C     DHPRCD        ORGE      '99281'
     C     DHPRCD        ANDLE     '99285'
     C     DHPRCD        ORGE      '99311'
     C     DHPRCD        ANDLE     '99313'
     C     DHPRCD        ORGE      '99331'
     C     DHPRCD        ANDLE     '99333'
     C     DHPRCD        ORGE      '99351'
     C     DHPRCD        ANDLE     '99353'
     C*
     C* SETLL and read the file testing for an '*' procedure.
     C*
     C                   MOVE      '10000'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     AGAIN2        TAG
     C*
     C     CAKEY7        READE     PCA09                                  54
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
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '43'          QQC(Q)
     C*** Err Msg 43: "Adjustor should review charges for an
     C***                    unscheduled, starred procedure"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
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
     C*
     C* ===============================================================
     C* GLOBAL SURG - PAY OFFICE VISIT BUT GIVE ERROR MESSAGE THAT SERVICE
     C* FALLS WITHIN PRE-OPERATIVE POLICY
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99205'
     C     DHPRCD        ORGE      '99221'
     C     DHPRCD        ANDLE     '99223'
     C     DHPRCD        ORGE      '99241'
     C     DHPRCD        ANDLE     '99255'
     C     DHPRCD        ORGE      '99281'
     C     DHPRCD        ANDLE     '99285'
     C     DHPRCD        ORGE      '99301'
     C     DHPRCD        ANDLE     '99303'
     C     DHPRCD        ORGE      '99321'
     C     DHPRCD        ANDLE     '99323'
     C     DHPRCD        ORGE      '99341'
     C     DHPRCD        ANDLE     '99343'
     C     DHPRCD        ORGE      '99211'
     C     DHPRCD        ANDLE     '99215'
     C     DHPRCD        ORGE      '99231'
     C     DHPRCD        ANDLE     '99233'
     C     DHPRCD        ORGE      '99261'
     C     DHPRCD        ANDLE     '99263'
     C     DHPRCD        ORGE      '99281'
     C     DHPRCD        ANDLE     '99285'
     C     DHPRCD        ORGE      '99311'
     C     DHPRCD        ANDLE     '99313'
     C     DHPRCD        ORGE      '99331'
     C     DHPRCD        ANDLE     '99333'
     C     DHPRCD        ORGE      '99351'
     C     DHPRCD        ANDLE     '99353'
     C                   MOVE      '10000'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XRCPT4        IFGE      '10040'
     C     XRCPT4        ANDLE     '69979'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AJ'          QQC(Q)
     C* Err Msg AJ: "Service falls within pre-operative policy."
     C     CACHRG        IFGT      SXMRA
     C                   Z-ADD     SXMRA         XPTOTA
     C                   ELSE
     C                   Z-ADD     CACHRG        XPTOTA
     C                   END
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END 0 DOWEQ 60
     C                   END                                                    END GE '99201'
     C*
     C* ===============================================================
     C*** Per pg. 52, 7
     C*** Only allow initial visit with physical therapy
     C     DHPRCD        IFGE      '99211'
     C     DHPRCD        ANDLE     '99215'
     C     DHPRCD        ORGE      '99231'
     C     DHPRCD        ANDLE     '99233'
     C     DHPRCD        ORGE      '99261'
     C     DHPRCD        ANDLE     '99263'
     C     DHPRCD        ORGE      '99281'
     C     DHPRCD        ANDLE     '99285'
     C     DHPRCD        ORGE      '99311'
     C     DHPRCD        ANDLE     '99313'
     C     DHPRCD        ORGE      '99331'
     C     DHPRCD        ANDLE     '99333'
     C     DHPRCD        ORGE      '99351'
     C     DHPRCD        ANDLE     '99353'
     C*
     C                   MOVE      '97000'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '97000'
     C     XRCPT4        ANDLE     '97799'
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '204'         XPEOMB
     C*** EOMB 204: "Follow-up exam only allowed if new symptoms are
     C***           present."
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C                   END
     C     CAKY9A        READE     PCA09                                  60
     C                   END
     C*
     C                   END                                                    END GE 90030...
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C* Update charge with payment
     C                   EXSR      UPDAT
     C*
     C     OFFIE         ENDSR
     *********************************************************************
     *** HANDLE ALL OTHER CODES   XXXXX-XXXXX                    O T H E *
     *********************************************************************
     C     OTHE          BEGSR
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
     *** Pathology                  Codes: 80002 - 89399         P A T H *
     *********************************************************************
     C     PATH          BEGSR
     C*
     C*=====================================================================
     C     SXMRA         MULT      XXPATH        SXMRA
     C*
     C*----------------------------------------------------------------
     C*   Per pg. 281:  "When a physician is hospital-based...charge
     C*                  will be 40% of listed value (Prof Component)"
     C*   Should the provider fail to add modifier -26 to a pathology procedure
     C*   provided in a hospital, ambulatory surgical center or similiar
     C*   facility, reimbursement must only be made for the professional
     C*   component.
     C*
     C     DHPOS         IFEQ      '1'
     C     DHPOS         OREQ      'B'
     C     XPPMOD        IFEQ      *BLANKS
     C                   MOVE      '26'          XPPMOD
     C                   MOVE      '214'         XPEOMB
     C*** EOMB 214: "Based upon provider's facility, reimbursement is made for
     C***            the professional component"
     C                   END                                                    END EQ BLANKS
     C                   END                                                    END EQ 1/2/B
     C*
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per PG. 218-219
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '27'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '90'
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
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
     C                   ELSE                                                   ELSE EQ 22...
     C*
     C*----------------------------------------------------------------
     C*  Per pg. 176, 3  Professional Component.
     C*                  Where a physician is hospital based and is not
     C*                  salaried or otherwise recompensed for services
     C*                  listed in this section, a separate bill will
     C*                  be rendered for the particular service.
     C*
     C*                  The charge will be 40% of listed value.
     C*
     C*                  In no circumstances will the sum of the two
     C*                  charges (PC and TC) be greater than that
     C*                  listed for any service or procedure
     C*
     C*----------------------------------------------------------------
     C*  Per pg. 176, 3
     C*                  The sections of
     C*                    Anatomic Pathology    88000-88099
     C*                    Cytopahtology         88104-88199
     C*                    Surgical Pathology    88300-88399
     C*                  will be exempt from the 60/40 split.
     C*
     C*  Special Calc for -26 Professional Component
     C*       (80% of listed value)
     C*
     C*  Special Calc for -27 Professional Component
     C*       (20% of listed value)
     C*
     C*
     C*----------------------------------------------------------------
     C*
     C     DHPRCD        IFGE      '88000'
     C     DHPRCD        ANDLE     '88099'
     C     DHPRCD        ORGE      '88104'
     C     DHPRCD        ANDLE     '88199'
     C     DHPRCD        ORGE      '88300'
     C     DHPRCD        ANDLE     '88399'
     C*
     C     XPPMOD        IFEQ      '26'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '76'          QQC(Q)
     C*** Err Msg 76: "CPT modifier used with this charge.  Verify use
     C***              and payment."
     C*
     C*-- Check for previously paid Complete Procedure ---------------
     C*                   The listed values are total values that
     C*                   include both the professional and technical
     C*                   components.  Utilization of the listed value
     C*                   implies that there will only be one charge
     C*
     C* Chain to detail charge file, if you find a charge w/o mod-26
     C* that has been paid, then deny this (current) charge and print
     C* message (Error msg #29, EOMB #206) that says "This procedure
     C* was previously paid under a complete code"
     C                   MOVE      DHPRCD        XXCPT4
     C                   MOVE      *BLANKS       XXPMOD
     C     CAKY8         CHAIN     PCA06                              60
     C     *IN60         IFEQ      '0'
     C     X6TOTP        ANDNE     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '29'          QQC(Q)
     C*** Err Msg 29: "This procedure was previously paid under a complete code"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '206'         XPEOMB
     C*** EOMB 206: "This procedure was previously paid under a complete code"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END 1 *IN60
     C*
     C*-- If no previously paid Complete Procedure, continue on -------
     C*
     C*  Special Calc for -26 Professional Component (80% of listed)
     C     SXMRA         MULT      .80           XPTOTA
     C                   ELSE                                                   ELS EQ 26
     C*
     C     XPPMOD        IFEQ      '27'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '76'          QQC(Q)
     C*** Err Msg 76: "CPT modifier used with this charge.  Verify use
     C***              and payment."
     C*
     C*-- Check for previously paid Complete Procedure ---------------
     C*                   The listed values are total values that
     C*                   include both the professional and technical
     C*                   components.  Utilization of the listed value
     C*                   implies that there will only be one charge
     C*
     C* Chain to detail charge file, if you find a charge w/o mod-26
     C* that has been paid, then deny this (current) charge and print
     C* message (Error msg #29, EOMB #206) that says "This procedure
     C* was previously paid under a complete code"
     C                   MOVE      DHPRCD        XXCPT4
     C                   MOVE      *BLANKS       XXPMOD
     C     CAKY8         CHAIN     PCA06                              60
     C     *IN60         IFEQ      '0'
     C     X6TOTP        ANDNE     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '29'          QQC(Q)
     C*** Err Msg 29: "This procedure was previously paid under a complete code"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '206'         XPEOMB
     C*** EOMB 206: "This procedure was previously paid under a complete code"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END 1 *IN60
     C*
     C*-- If no previously paid Complete Procedure, continue on -------
     C*
     C*  Special Calc for -27 Technical Component (60% of listed)
     C     SXMRA         MULT      .20           XPTOTA
     C                   END                                                    END EQ 27
     C                   END                                                    END EQ 26
     C*                                              >
     C     CACHRG        COMP      XPTOTA                             54
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
     C*
     C                   ELSE                                                   ELS GE 88000
     C*
     C     XPPMOD        IFEQ      '26'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '76'          QQC(Q)
     C*** Err Msg 76: "CPT modifier used with this charge.  Verify use
     C***              and payment."
     C*
     C*-- Check for previously paid Complete Procedure ---------------
     C*                   The listed values are total values that
     C*                   include both the professional and technical
     C*                   components.  Utilization of the listed value
     C*                   implies that there will only be one charge
     C*
     C* Chain to detail charge file, if you find a charge w/o mod-26
     C* that has been paid, then deny this (current) charge and print
     C* message (Error msg #29, EOMB #206) that says "This procedure
     C* was previously paid under a complete code"
     C                   MOVE      DHPRCD        XXCPT4
     C                   MOVE      *BLANKS       XXPMOD
     C     CAKY8         CHAIN     PCA06                              60
     C     *IN60         IFEQ      '0'
     C     X6TOTP        ANDNE     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '29'          QQC(Q)
     C*** Err Msg 29: "This procedure was previously paid under a complete code"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '206'         XPEOMB
     C*** EOMB 206: "This procedure was previously paid under a complete code"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END 1 *IN60
     C*
     C*-- If no previously paid Complete Procedure, continue on -------
     C*
     C*  Special Calc for -26 Professional Component (40% of listed)
     C     SXMRA         MULT      .40           XPTOTA
     C*
     C*  Determine if payment has already been paid for Mod -27
     C                   MOVE      DHPRCD        XXCPT4
     C                   MOVE      '27'          XXCPT4
     C     CAKY8         CHAIN     PCA06                              60
     C     *IN60         IFEQ      '0'
     C     X6TOTP        ANDNE     *ZEROS
     C     SXMRA         SUB       X6TOTP        HLTOTA
     C*
     C*  If yes, payment for Mod-26 cannot exceed XXMRA
     C     XPTOTA        IFGT      HLTOTA
     C                   Z-ADD     HLTOTA        XPTOTA
     C                   MOVE      '213'         XPEOMB
     C* EOMB Code 213: "Reimbursement reflects difference from"
     C*                 previously paid component"
     C                   END                                                    END GT TOTP
     C*
     C                   END                                                    END 1 *IN60
     C*
     C                   ELSE                                                   ELS EQ 26
     C*
     C     XPPMOD        IFEQ      '27'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '76'          QQC(Q)
     C*** Err Msg 76: "CPT modifier used with this charge.  Verify use
     C***              and payment."
     C*
     C*-- Check for previously paid Complete Procedure ---------------
     C*                   The listed values are total values that
     C*                   include both the professional and technical
     C*                   components.  Utilization of the listed value
     C*                   implies that there will only be one charge
     C*
     C* Chain to detail charge file, if you find a charge w/o mod-26
     C* that has been paid, then deny this (current) charge and print
     C* message (Error msg #29, EOMB #206) that says "This procedure
     C* was previously paid under a complete code"
     C                   MOVE      DHPRCD        XXCPT4
     C                   MOVE      *BLANKS       XXPMOD
     C     CAKY8         CHAIN     PCA06                              60
     C     *IN60         IFEQ      '0'
     C     X6TOTP        ANDNE     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '29'          QQC(Q)
     C*** Err Msg 29: "This procedure was previously paid under a complete code"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '206'         XPEOMB
     C*** EOMB 206: "This procedure was previously paid under a complete code"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   END                                                    END 1 *IN60
     C*
     C*-- If no previously paid Complete Procedure, continue on -------
     C*
     C*  Special Calc for -27 Technical Component (60% of listed)
     C     SXMRA         MULT      .60           XPTOTA
     C*
     C*  Determine if payment has already been paid for Mod -26
     C                   MOVE      DHPRCD        XXCPT4
     C                   MOVE      '26'          XXCPT4
     C     CAKY8         CHAIN     PCA06                              60
     C     *IN60         IFEQ      '0'
     C     X6TOTP        ANDNE     *ZEROS
     C     SXMRA         SUB       X6TOTP        HLTOTA
     C*
     C*  If yes, payment for Mod-27 cannot exceed XXMRA
     C     XPTOTA        IFGT      HLTOTA
     C                   Z-ADD     HLTOTA        XPTOTA
     C                   MOVE      '213'         XPEOMB
     C* EOMB Code 213: "Reimbursement reflects difference from"
     C*                 previously paid component"
     C                   END                                                    END GT TOTP
     C*
     C                   END                                                    END 1 *IN60
     C*
     C                   ELSE
     C                   EXSR      PMOD
     C*
     C                   END                                                    END EQ 27
     C                   END                                                    END EQ 26
     C*                                              >
     C     CACHRG        COMP      XPTOTA                             54
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
     C                   END                                                    END GE 88000
     C*
     C                   END                                                    END EQ 22
     C*
     C                   END                                                    END NE *BLANK
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
     *** PHYSICAL MEDICINE SUBROUTINE CODES 97000 - 97799        P H Y S *
     *********************************************************************
     C     PHYS          BEGSR
     C*
     C*** Per pg. 373, GR 9
     C*** Do not allow 97260 code if office visit on same day
     C     DHPRCD        IFEQ      '97260'
     C*
     C                   MOVE      '97201'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '99201'
     C     XRCPT4        ANDLE     '99205'
     C     XRCPT4        ORGE      '99211'
     C     XRCPT4        ANDLE     '99215'
     C     XRCPT4        ORGE      '99218'
     C     XRCPT4        ANDLE     '99223'
     C     XRCPT4        ORGE      '99231'
     C     XRCPT4        ANDLE     '99233'
     C     XRCPT4        ORGE      '99241'
     C     XRCPT4        ANDLE     '99245'
     C     XRCPT4        ORGE      '99251'
     C     XRCPT4        ANDLE     '99255'
     C     XRCPT4        ORGE      '99261'
     C     XRCPT4        ANDLE     '99263'
     C     XRCPT4        ORGE      '99271'
     C     XRCPT4        ANDLE     '99275'
     C     XRCPT4        ORGE      '99281'
     C     XRCPT4        ANDLE     '99288'
     C     XRCPT4        ORGE      '99291'
     C     XRCPT4        ANDLE     '99292'
     C     XRCPT4        ORGE      '99301'
     C     XRCPT4        ANDLE     '99303'
     C     XRCPT4        ORGE      '99311'
     C     XRCPT4        ANDLE     '99313'
     C     XRCPT4        ORGE      '99321'
     C     XRCPT4        ANDLE     '99323'
     C     XRCPT4        ORGE      '99331'
     C     XRCPT4        ANDLE     '99333'
     C     XRCPT4        ORGE      '99341'
     C     XRCPT4        ANDLE     '99343'
     C     XRCPT4        ORGE      '99351'
     C     XRCPT4        ANDLE     '99353'
     C     XRCPT4        ORGE      '99361'
     C     XRCPT4        ANDLE     '99362'
     C     XRCPT4        ORGE      '99371'
     C     XRCPT4        ANDLE     '99373'
     C     XRCPT4        OREQ      '99238'
     C     XRCPT4        OREQ      '99499'
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   MOVE      '205'         XPEOMB
     C* EOMB Code 205: "This procedure is included in the basic allowance
     C*                 of another procedure"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END
     C     CAKY9A        READE     PCA09                                  60
     C                   END
     C*
     C                   END                                                    END GE 99201...
     C*
     C*** END   ADD *** 10/23/95 *****************************************
     C     DHPRCD        IFNE      '97010'
     C     DHPRCD        ANDNE     '97012'
     C     DHPRCD        ANDNE     '97014'
     C     DHPRCD        ANDNE     '97016'
     C     DHPRCD        ANDNE     '97018'
     C     DHPRCD        ANDNE     '97022'
     C     DHPRCD        ANDNE     '97024'
     C     DHPRCD        ANDNE     '97026'
     C     DHPRCD        ANDNE     '97028'
     C     DHPRCD        ANDNE     '97039'
     C     DHPRCD        ANDNE     '97110'
     C     DHPRCD        ANDNE     '97112'
     C     DHPRCD        ANDNE     '97114'
     C     DHPRCD        ANDNE     '97116'
     C     DHPRCD        ANDNE     '97118'
     C     DHPRCD        ANDNE     '97122'
     C     DHPRCD        ANDNE     '97124'
     C     DHPRCD        ANDNE     '97126'
     C     DHPRCD        ANDNE     '97128'
     C     DHPRCD        ANDNE     '97139'
     C*
     C     SXMRA         MULT      XXPHYS        XXMRA
     C     XXMRA         DIV       2             X1MRA
     C     CACHRG        IFLE      X1MRA
     C                   Z-ADD     CACHRG        SXMRA
     C                   GOTO      NOCUT1
     C                   END                                                    END LT X1MRA
     C*
     C                   END                                                    END NE '97010'
     C*
     C*----------------------------------------------------------------
     C*
     C     XPPMOD        IFNE      *BLANK
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C* Err Msg 10: "InvaliD modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END NE *ZERO
     C*
     C*-------------------------------------------------------------------------
     C*- Additional minutes ---------------------------------------------------
     C     DHPRCD        IFEQ      '97001'
     C     DHPRCD        OREQ      '97101'
     C     DHPRCD        OREQ      '97201'
     C     DHPRCD        OREQ      '97221'
     C     DHPRCD        OREQ      '97241'
     C     DHPRCD        OREQ      '97501'
     C     DHPRCD        OREQ      '97521'
     C     DHPRCD        OREQ      '97531'
     C     DHPRCD        OREQ      '97541'
     C     DHPRCD        OREQ      '97701'
     C     DHPRCD        OREQ      '97755'
     C     DHPRCD        OREQ      '97764'
     C*
     C*   Disallow any additional minutes code that doesn't have the proper
     C*   major code associated with it
     C*
     C* Process 97001 additional minutes code
     C     DHPRCD        IFEQ      '97001'
     C                   MOVE      '97000'       XXCPT4
     C*
     C* Read thru entire 97000-type Phys Medicine Codes array to determine
     C* if there is a corresponding initial code
     C                   Z-ADD     1             I
     C     T97000        TAG
     C                   MOVE      IPT1(I)       XXCPT4
     C     CAKEY8        CHAIN     PCA09                              60
     C     *IN60         IFEQ      '1'
     C     I             IFLT      12
     C                   ADD       1             I
     C                   GOTO      T97000
     C                   END                                                    END LT 12
     C                   END                                                    END 1 *IN60
     C*
     C* If no hit on any of the 97000-type Phys Medicine Codes, then
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         XPTOTA
     C     CACHRG        SUB       CATOTA        XPTOTD
     C                   Z-ADD     CATOTA        XPTOTP
     C                   MOVE      '215'         XPEOMB
     C* EOMB Code 215: "A major code must be present to pay this item"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END 1 *IN60
     C*
     C                   ELSE                                                    ELS EQ 97001
     C*
     C* Else, Process 97101 additional minutes code
     C     DHPRCD        IFEQ      '97101'
     C*
     C* Read thru entire 97100-type Phys Medicine Codes array to determine
     C* if there is a corresponding initial code
     C                   Z-ADD     1             I
     C     T97100        TAG
     C                   MOVE      IPT2(I)       XXCPT4
     C     CAKEY8        CHAIN     PCA09                              60
     C     *IN60         IFEQ      '1'
     C     I             IFLT      12
     C                   ADD       1             I
     C                   GOTO      T97100
     C                   END                                                    END LT 12
     C                   END                                                    END 1 *IN60
     C*
     C* If no hit on any of the 97100-type Phys Medicine Codes, then
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         XPTOTA
     C     CACHRG        SUB       CATOTA        XPTOTD
     C                   Z-ADD     CATOTA        XPTOTP
     C                   MOVE      '215'         XPEOMB
     C* EOMB Code 215: "A major code must be present to pay this item"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END 1 *IN60
     C*
     C                   ELSE                                                    ELS EQ 97101
     C*
     C* Else, Process all other additional minutes codes
     C     DHPRCD        IFEQ      '97201'
     C                   MOVE      '97200'       XXCPT4
     C                   END
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
     C     DHPRCD        IFEQ      '97755'
     C                   MOVE      '97754'       XXCPT4
     C                   END
     C*
     C     DHPRCD        IFEQ      '97764'
     C                   MOVE      '97763'       XXCPT4
     C                   END
     C*
     C     CAKEY8        CHAIN     PCA09                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         XPTOTA
     C     CACHRG        SUB       CATOTA        XPTOTD
     C                   Z-ADD     CATOTA        XPTOTP
     C                   MOVE      '215'         XPEOMB
     C* EOMB Code 215: "A major code must be present to pay this item"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C*
     C                   END                                                    END 1 *IN60
     C*
     C                   END                                                     END EQ 97101
     C*
     C                   END                                                     END EQ 97001
     C*
     C                   END                                                    END EQ 97001, ETC.
     C*
     C* -----------------------------------------------------------------------
     C*-- Write record to Phys Medicine Workfile FACAPNE ---------------
     C                   MOVE      *BLANKS       CASMCD
     C*
     C* If working with 97000-97039 modality:
     C     DHPRCD        IFEQ      '97000'
     C     DHPRCD        OREQ      '97001'
     C     DHPRCD        OREQ      '97010'
     C     DHPRCD        OREQ      '97012'
     C     DHPRCD        OREQ      '97014'
     C     DHPRCD        OREQ      '97016'
     C     DHPRCD        OREQ      '97018'
     C     DHPRCD        OREQ      '97020'
     C     DHPRCD        OREQ      '97022'
     C     DHPRCD        OREQ      '97024'
     C     DHPRCD        OREQ      '97026'
     C     DHPRCD        OREQ      '97028'
     C     DHPRCD        OREQ      '97039'
     C*
     C*    a.  If there is a combination of modality(ies)
     C*        and procedure(s) on same day, then pay as a 97200
     C*          SETLL and READ the file for 97100 series
     C                   MOVE      '97100'       XXCPT4
     C                   Z-ADD     *ZERO         COUNT
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C     XRCPT4        IFGE      '97100'
     C     XRCPT4        ANDLE     '97139'
     C     XRCPT4        ANDNE     '97101'                                      ADD 09/17/92
     C                   ADD       1             COUNT
     C                   END                                                    END GE 97100
     C     CAKEY7        READE     PCA09                                  54
     C                   END                                                    END 0 DOWEQ
     C*
     C* If at least one procedure is found, then pay at 97200 code
     C     COUNT         IFGE      1
     C     10.0          MULT      XXPHYS        CAMRA
     C     10.0          MULT      XXPHYS        CAMMRA
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BG'          QQC(Q)
     C* Err Msg AH  "Paid per 97200-97201 combination:review time exceeded
     C*             30 minutes."
     C                   MOVE      '97200'       CASMCD
     C*
     C                   ELSE                                                   ELSE COUNT GE 1
     C*
     C* Check to see if there are any additional (minor) codes for this series
     C*          SETLL and READ the file for 97000 series
     C                   MOVE      '97000'       XXCPT4
     C                   Z-ADD     *ZERO         MAXADD
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  54
     C     *IN54         DOWEQ     '0'
     C     XRCPT4        IFEQ      '97001'
     C                   ADD       1             MAXADD
     C                   END                                                    END NE 97001
     C     CAKEY7        READE     PCA09                                  54
     C                   END                                                    END 0 DOWEQ
     C*
     C* Else, if no procedures exists, pay at 97000 OR AT 97001 FOR ADD MIN
     C     DHPRCD        IFNE      '97001'                                      ADD 9/17/92
     C     4.7           MULT      XXPHYS        CAMRA
     C     MAXADD        IFEQ      0
     C     4.7           MULT      XXPHYS        CAMMRA
     C                   ELSE                                                   ELSE MAXADD EQ 0
     C     6.4           MULT      XXPHYS        CAMMRA
     C                   END                                                    END MAXADD EQ 0
     C                   ELSE                                                   ELSE DHPRCD NE 97001
     C     1.7           MULT      XXPHYS        CAMRA
     C                   END                                                    END DHPRCD NE 97001
     C                   MOVE      '97000'       CASMCD
     C*
     C                   END                                                    END  COUNT GE 1
     C*
     C                   ELSE                                                   ELSE EQ 97000
     C*
     C* Else, If working with 97100-97139 procedure:
     C     DHPRCD        IFEQ      '97100'
     C     DHPRCD        OREQ      '97101'
     C     DHPRCD        OREQ      '97110'
     C     DHPRCD        OREQ      '97112'
     C     DHPRCD        OREQ      '97114'
     C     DHPRCD        OREQ      '97116'
     C     DHPRCD        OREQ      '97118'
     C     DHPRCD        OREQ      '97120'
     C     DHPRCD        OREQ      '97122'
     C     DHPRCD        OREQ      '97124'
     C     DHPRCD        OREQ      '97126'
     C     DHPRCD        OREQ      '97128'
     C     DHPRCD        OREQ      '97139'
     C*
     C*    a.  If there is a combination of modality(ies)
     C*        and procedure(s) on same day, then pay as a 97200
     C*          SETLL and READ the file for 97000 series
     C                   MOVE      '97000'       XXCPT4
     C                   Z-ADD     *ZERO         COUNT
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C     XRCPT4        IFGE      '97000'
     C     XRCPT4        ANDLE     '97039'
     C     XRCPT4        ANDNE     '97001'
     C                   ADD       1             COUNT
     C                   END                                                    END GE 97000
     C     CAKEY7        READE     PCA09                                  54
     C                   END                                                    END 0 DOWEQ
     C*
     C* If at least one modality is found, then pay at 97200 code
     C     COUNT         IFGE      1
     C     10.0          MULT      XXPHYS        CAMRA
     C     10.0          MULT      XXPHYS        CAMMRA
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BG'          QQC(Q)
     C* Err Msg AH  "Paid per 97200-97201 combination:review time exceeded
     C*             30 minutes."
     C                   MOVE      '97200'       CASMCD
     C                   ELSE                                                   ELSE COUNT GE 1
     C     6.0           MULT      XXPHYS        CAMRA
     C     6.0           MULT      XXPHYS        CAMMRA
     C                   MOVE      '97100'       CASMCD
     C*
     C                   END                                                    END  COUNT GE 1
     C*
     C                   ELSE                                                   ELSE EQ 97100
     C*
     C* Else, If working with any other code - look up in table for
     C*       corresponding summary code
     C                   Z-ADD     1             P
     C     DHPRCD        LOOKUP    PCD1(P)                                61
     C     *IN61         IFEQ      '1'
     C                   MOVE      SMCD(P)       CASMCD
     C                   ELSE                                                   ELS 1 *IN61
     C                   MOVE      DHPRCD        CASMCD
     C                   END                                                    END 1 *IN61
     C     SXMRA         MULT      XXPHYS        CAMRA
     C*       Look up in array to find out maximum MRA on this code
     C                   Z-ADD     1             P
     C     DHPRCD        LOOKUP    PCD2(P)                                61
     C     *IN61         IFEQ      '1'
     C                   Z-ADD     MRA(P)        CAMMRA
     C                   ELSE                                                   ELS 1 *IN61
     C                   Z-ADD     CAMRA         CAMMRA
     C                   END                                                    END 1 *IN61
     C*
     C                   END                                                    END EQ 97100
     C                   END                                                    END EQ 97000
     C*
     C*
     C* -----------------------------------------------------------------------
     C* Per pg. 51, 5
     C*             When a physical therapist renders treatment in a
     C*             patient's home, add 50% to the listed unit value.
     C*             Justification report required
     C*
     C*   Chain to Provider file to see if specialty code = PHT
     C*
     C     SVSPCD        IFEQ      'PHT'
     C     DHPOS         ANDEQ     '4'
     C     SVSPCD        OREQ      'OT'
     C     DHPOS         ANDEQ     '4'
     C     CAMRA         MULT(H)   1.50          CAMRA
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '55'          QQC(Q)
     C* Err Msg 55: "A copy of the justification report is required
     C*             for reimbursement"
     C                   END                                                    END EQ PHT
     C* -----------------------------------------------------------------------
     C*
     C* Load Summary Array
     C                   Z-ADD     1             P
     C     CASMCD        LOOKUP    PTSC(P)                                61
     C*
     C     *IN61         IFEQ      '0'
     C*   If no hit on Code in Summary Array, then establish new element
     C                   Z-ADD     1             P
     C     *BLANKS       LOOKUP    PTSC(P)                                61
     C                   MOVE      CASMCD        PTSC(P)
     C                   Z-ADD     CAMRA         PTSM(P)
     C*
     C                   ELSE                                                   ELSE 0 *IN61
     C*
     C*   Else, If the summary code already exists in the arry, then determine
     C*         whether this code's MRA should be added to the array bucket
     C                   Z-ADD     1             Q
     C     DHPRCD        LOOKUP    PCD3                                   61
     C     *IN61         IFEQ      '1'
     C                   ADD       CAMRA         PTSM(P)
     C                   ELSE                                                   ELSE 1 *IN61
     C     CAMRA         IFGT      PTSM(P)
     C                   ADD       CAMRA         PTSM(P)
     C                   END                                                    END GT PTSM,P
     C                   END                                                    END  1 *IN61
     C                   END                                                    END  0 *IN61
     C*
     C                   MOVE      'P'           CATYPE                         ADD 03/09/93
     C                   Z-ADD     CACHRG        CATOTC
     C                   WRITE     PCAWK1
     C                   GOTO      PHYSE
     C*
     C* ===============================================================
     C* Calculate Payment
     C     NOCUT1        TAG
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
     C     PHYSE         ENDSR
     *********************************************************************
     C*  Physical Medicine Subroutine Codes 97000 - 97799        P H Y S *
     *********************************************************************
     C     PHYS2         BEGSR
     C*
     C*-------------------------------------------------------------------------
     C*-- Process FACAPNE (Detail Charge Workfile) -------------------------
     C*
     C* Per pg. 31, 8   When multiple services or procedures
     C*                 (different, non-sequential, codes numbers)
     C*                 are rendered or performed on one day, the
     C*                 payments will be limited to the greatest
     C*                 allowable fee plus one-half of the lesser fee(s
     C*                 up to a maximum of twice the highest fee.
     C*
     C* If the Array MRAs are greater than the max allowance, reset Array MRAs
     C* NOTE:  This is only true if this code has an additional minutes charge
     C*
     C* 97000/97001
     C                   Z-ADD     1             P
     C     '97000'       LOOKUP    PTSC(P)                                61
     C     *IN61         IFEQ      '1'
     C     PTSM(P)       ANDGT     38.40
     C                   Z-ADD     38.40         PTSM(P)
     C                   END                                                    END GT 5.4
     C* 97100/97101
     C                   Z-ADD     1             P
     C     '97100'       LOOKUP    PTSC(P)                                61
     C     *IN61         IFEQ      '1'
     C     PTSM(P)       ANDGT     48.60
     C                   Z-ADD     48.60         PTSM(P)
     C                   END                                                    END GT 6.9
     C* 97200/97201
     C                   Z-ADD     1             P
     C     '97200'       LOOKUP    PTSC(P)                                61
     C     *IN61         IFEQ      '1'
     C     PTSM(P)       ANDGT     60.00
     C                   Z-ADD     60.00         PTSM(P)
     C                   END                                                    END GT 8.4
     C* 97220/97221
     C                   Z-ADD     1             P
     C     '97220'       LOOKUP    PTSC(P)                                61
     C     *IN61         IFEQ      '1'
     C     PTSM(P)       ANDGT     76.80
     C                   Z-ADD     76.80         PTSM(P)
     C                   END                                                    END GT 12.8
     C* 97240/97241
     C                   Z-ADD     1             P
     C     '97240'       LOOKUP    PTSC(P)                                61
     C     *IN61         IFEQ      '1'
     C     PTSM(P)       ANDGT     96.0
     C                   Z-ADD     96.0          PTSM(P)
     C                   END                                                    END GT 16.0
     C* 97500/97501
     C                   Z-ADD     1             P
     C     '97500'       LOOKUP    PTSC(P)                                61
     C     *IN61         IFEQ      '1'
     C     PTSM(P)       ANDGT     64.8
     C                   Z-ADD     64.8          PTSM(P)
     C                   END                                                    END GT 10.8
     C* 97540/97541
     C                   Z-ADD     1             P
     C     '97540'       LOOKUP    PTSC(P)                                61
     C     *IN61         IFEQ      '1'
     C     PTSM(P)       ANDGT     52.20
     C                   Z-ADD     52.20         PTSM(P)
     C                   END                                                    END GT 8.7
     C* 97700/97701/97702
     C                   Z-ADD     1             P
     C     '97700'       LOOKUP    PTSC(P)                                61
     C     *IN61         IFEQ      '1'
     C     PTSM(P)       ANDGT     78.0
     C                   Z-ADD     78.0          PTSM(P)
     C                   END                                                    END GT 13.0
     C*
     C* Load PT Summary Array records to "big" summary array and sort
     C* in DESCENDING order by MRA value (PTSM)
     C     1             DO        50            P
     C                   Z-ADD     PTSM(P)       DSMRA
     C                   MOVE      PTSC(P)       DSCPT4
     C                   MOVEL     DSSORT        PTSS(P)
     C                   END                                                    END DO 50 P
     C                   SORTA     PTSS
     C*
     C* Reload PT Summary Arrays PTSC & PTSM which are now in order
     C* by greatest amount to lowest amount.
     C     1             DO        50            P
     C                   MOVEL     PTSS(P)       DSSORT
     C                   Z-ADD     DSMRA         PTSM(P)
     C                   MOVEL     DSCPT4        PTSC(P)
     C                   END                                                    END DO 50 P
     C*
     C* Using PTSC Array CPT4 - SETLL on summary code and read all
     C* corresponding workfile charges with same summary code -
     C* processing at applicable percentage.
     C                   MOVE      *BLANK        XX50
     C     1             DO        50            P
     C                   MOVEL     PTSC(P)       XXSMCD
     C     CAKYP4        CHAIN     PCAWK3                             60
     C     *IN60         DOWEQ     '0'
     C*
     C* If this is an additional minutes code, then use the MRA from the
     C* summary code file,
     C     P3CPT4        IFEQ      '97001'
     C     P3CPT4        OREQ      '97101'
     C     P3CPT4        OREQ      '97201'
     C     P3CPT4        OREQ      '97221'
     C     P3CPT4        OREQ      '97241'
     C     P3CPT4        OREQ      '97501'
     C     P3CPT4        OREQ      '97521'
     C     P3CPT4        OREQ      '97531'
     C     P3CPT4        OREQ      '97541'
     C     P3CPT4        OREQ      '97701'
     C     P3CPT4        OREQ      '97755'
     C     P3CPT4        OREQ      '97764'
     C                   Z-ADD     PTSM(P)       XXMRA
     C                   ELSE                                                    ELS EQ Addtl Code
     C* Else, Calculate $ MRA based upon maximum MRA found in workfile record
     C                   Z-ADD     P3MMRA        XXMRA
     C                   END                                                     END EQ Addtl Code
     C*
     C* 2nd+ array elements and corresponding workfile records
     C* should be processed at 50%  (1st array element will pay at 100%)
     C     P             IFGT      1
     C                   MOVE      'Y'           XX50
     C                   END                                                    END P GT 1
     C*
     C* Run all records through the following additional FS rules before
     C* final payment
     C*
     C*-------------------------------------------------------------------------
     C*
     C* Based upon code, load variable fields, read all records to determine
     C* what has been paid on the code up to this point (PPHYMD)
     C*
     C     P3CPT4        IFEQ      '97000'
     C     P3CPT4        OREQ      '97010'
     C     P3CPT4        OREQ      '97012'
     C     P3CPT4        OREQ      '97014'
     C     P3CPT4        OREQ      '97016'
     C     P3CPT4        OREQ      '97018'
     C     P3CPT4        OREQ      '97020'
     C     P3CPT4        OREQ      '97022'
     C     P3CPT4        OREQ      '97024'
     C     P3CPT4        OREQ      '97026'
     C     P3CPT4        OREQ      '97028'
     C     P3CPT4        OREQ      '97039'
     C                   MOVE      '97000'       XXCPT4
     C     XXSMCD        IFEQ      '97200'
     C                   MOVE      '97139'       ZZCPT4
     C                   ELSE                                                   ELS EQ 97200
     C                   MOVE      '97039'       ZZCPT4
     C                   END                                                    END EQ 97200
     C                   END                                                    END EQ 97000
     C*
     C     P3CPT4        IFEQ      '97001'
     C                   MOVE      '97000'       XXCPT4
     C     XXSMCD        IFEQ      '97200'
     C                   MOVE      '97139'       ZZCPT4
     C                   ELSE                                                   ELS EQ 97200
     C                   MOVE      '97039'       ZZCPT4                         CHG 10/14/92
     C                   END                                                    END EQ 97200
     C                   END                                                    END EQ 97001
     C*
     C     P3CPT4        IFEQ      '97100'
     C     P3CPT4        OREQ      '97110'
     C     P3CPT4        OREQ      '97112'
     C     P3CPT4        OREQ      '97114'
     C     P3CPT4        OREQ      '97116'
     C     P3CPT4        OREQ      '97118'
     C     P3CPT4        OREQ      '97120'
     C     P3CPT4        OREQ      '97122'
     C     P3CPT4        OREQ      '97124'
     C     P3CPT4        OREQ      '97126'
     C     P3CPT4        OREQ      '97128'
     C     P3CPT4        OREQ      '97139'
     C     XXSMCD        IFEQ      '97200'
     C                   MOVE      '97000'       XXCPT4
     C                   ELSE                                                   ELS EQ 97200
     C                   MOVE      '97100'       XXCPT4
     C                   END                                                    END EQ 97200
     C                   MOVE      '97139'       ZZCPT4
     C                   END                                                    END EQ 97100
     C*
     C     P3CPT4        IFEQ      '97101'
     C     XXSMCD        IFEQ      '97200'
     C                   MOVE      '97000'       XXCPT4
     C                   ELSE                                                   ELS EQ 97200
     C                   MOVE      '97100'       XXCPT4
     C                   END                                                    END EQ 97200
     C                   MOVE      '97139'       ZZCPT4                         CHG 10/14/92
     C                   END                                                    END EQ 97101
     C*
     C     P3CPT4        IFEQ      '97200'
     C                   MOVE      '97200'       XXCPT4
     C                   MOVE      '97200'       ZZCPT4
     C                   END                                                    END EQ 97200
     C*
     C     P3CPT4        IFEQ      '97201'
     C                   MOVE      '97200'       XXCPT4
     C                   MOVE      '97201'       ZZCPT4
     C                   END                                                    END EQ 97201
     C*
     C     P3CPT4        IFEQ      '97220'
     C                   MOVE      '97220'       XXCPT4
     C                   MOVE      '97220'       ZZCPT4
     C                   END                                                    END EQ 97220
     C*
     C     P3CPT4        IFEQ      '97221'
     C                   MOVE      '97220'       XXCPT4
     C                   MOVE      '97221'       ZZCPT4
     C                   END                                                    END EQ 97221
     C*
     C     P3CPT4        IFEQ      '97240'
     C                   MOVE      '97240'       XXCPT4
     C                   MOVE      '97240'       ZZCPT4
     C                   END                                                    END EQ 97240
     C*
     C     P3CPT4        IFEQ      '97241'
     C                   MOVE      '97240'       XXCPT4
     C                   MOVE      '97241'       ZZCPT4
     C                   END                                                    END EQ 97241
     C*
     C     P3CPT4        IFEQ      '97260'
     C                   MOVE      '97260'       XXCPT4
     C                   MOVE      '97260'       ZZCPT4
     C                   END                                                    END EQ 97260
     C*
     C     P3CPT4        IFEQ      '97500'
     C                   MOVE      '97500'       XXCPT4
     C                   MOVE      '97500'       ZZCPT4
     C                   END                                                    END EQ 97500
     C*
     C     P3CPT4        IFEQ      '97501'
     C                   MOVE      '97500'       XXCPT4
     C                   MOVE      '97501'       ZZCPT4
     C                   END                                                    END EQ 97501
     C*
     C     P3CPT4        IFEQ      '97520'
     C                   MOVE      '97520'       XXCPT4
     C                   MOVE      '97520'       ZZCPT4
     C                   END                                                    END EQ 97520
     C*
     C     P3CPT4        IFEQ      '97521'
     C                   MOVE      '97520'       XXCPT4
     C                   MOVE      '97521'       ZZCPT4
     C                   END                                                    END EQ 97521
     C*
     C     P3CPT4        IFEQ      '97530'
     C                   MOVE      '97530'       XXCPT4
     C                   MOVE      '97530'       ZZCPT4
     C                   END                                                    END EQ 97530
     C*
     C     P3CPT4        IFEQ      '97531'
     C                   MOVE      '97530'       XXCPT4
     C                   MOVE      '97531'       ZZCPT4
     C                   END                                                    END EQ 97531
     C*
     C     P3CPT4        IFEQ      '97540'
     C                   MOVE      '97540'       XXCPT4
     C                   MOVE      '97540'       ZZCPT4
     C                   END                                                    END EQ 97540
     C*
     C     P3CPT4        IFEQ      '97541'
     C                   MOVE      '97540'       XXCPT4
     C                   MOVE      '97541'       ZZCPT4
     C                   END                                                    END EQ 97541
     C*
     C     P3CPT4        IFEQ      '97700'
     C                   MOVE      '97700'       XXCPT4
     C                   MOVE      '97700'       ZZCPT4
     C                   END                                                    END EQ 97700
     C*
     C     P3CPT4        IFEQ      '97701'
     C                   MOVE      '97700'       XXCPT4
     C                   MOVE      '97702'       ZZCPT4
     C                   END                                                    END EQ 97701
     C*
     C     P3CPT4        IFEQ      '97702'
     C                   MOVE      '97700'       XXCPT4
     C                   MOVE      '97702'       ZZCPT4
     C                   END                                                    END EQ 97702
     C*
     C     P3CPT4        IFEQ      '97752'
     C                   MOVE      '97752'       XXCPT4
     C                   MOVE      '97752'       ZZCPT4
     C                   END                                                    END EQ 97752
     C*
     C     P3CPT4        IFEQ      '97753'
     C                   MOVE      '97753'       XXCPT4
     C                   MOVE      '97753'       ZZCPT4
     C                   END                                                    END EQ 97753
     C*
     C     P3CPT4        IFEQ      '97754'
     C                   MOVE      '97754'       XXCPT4
     C                   MOVE      '97754'       ZZCPT4
     C                   END                                                    END EQ 97754
     C*
     C     P3CPT4        IFEQ      '97755'
     C                   MOVE      '97754'       XXCPT4
     C                   MOVE      '97755'       ZZCPT4
     C                   END                                                    END EQ 97755
     C*
     C     P3CPT4        IFEQ      '97763'
     C                   MOVE      '97763'       XXCPT4
     C                   MOVE      '97763'       ZZCPT4
     C                   END                                                    END EQ 97763
     C*
     C     P3CPT4        IFEQ      '97764'
     C                   MOVE      '97764'       XXCPT4
     C                   MOVE      '97764'       ZZCPT4
     C                   END                                                    END EQ 97764
     C*
     C                   Z-ADD     XXYMD         SVYMD
     C                   Z-ADD     HDYMD         XXYMD
     C*
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  54
     C*
     C                   Z-ADD     *ZEROS        PPHYMD
     C                   Z-ADD     *ZEROS        LPHYMD
     C*
     C     *IN54         DOWEQ     '0'
     C     XRCPT4        ANDLE     ZZCPT4
     C     XRDSEQ        IFNE      P3DSEQ                                       CHG 10/14/92
     C                   ADD       XRTOTP        PPHYMD
     C                   END                                                    ADD 10/14/92
     C     CAKEY7        READE     PCA09                                  54
     C                   END
     C*
     C                   Z-ADD     SVYMD         XXYMD
     C*
     C* If this is a 50% payment code, then mulitple MRA by .50
     C     XX50          IFEQ      'Y'
     C     XXMRA         MULT      .50           HLMRA
     C                   ELSE                                                   ELSE EQ 'Y'
     C                   Z-ADD     XXMRA         HLMRA
     C                   END                                                    END EQ 'Y'
     C*
     C* Based upon accumulated paid on this code, calculate payment for this
     C* charge
     C     HLMRA         SUB       PPHYMD        LPHYMD
     C                   Z-ADD     P3MRA         XXMRA
     C     LPHYMD        IFGT      XXMRA
     C                   Z-ADD     XXMRA         LPHYMD
     C                   END
     C     LPHYMD        IFLE      *ZERO
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     P3TOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     P3TOTC        XPTOTC
     C                   MOVE      '209'         XPEOMB
     C*** EOMB 209: "Exceeds number of allowable procedures in reimburse manual"
     C                   ELSE                                                   ELSE LPHYMD LE *ZERO
     C     P3TOTC        IFGT      LPHYMD
     C                   Z-ADD     LPHYMD        XPTOTA                         ADD 10/19/92
     C                   Z-ADD     XPTOTA        XPTOTP
     C     P3TOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     P3TOTC        XPTOTC
     C                   MOVE      '202'         XPEOMB
     C*** EOMB 202: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   ELSE                                                   ELSE CACHRG GT LPHYM
     C                   Z-ADD     P3TOTC        XPTOTA
     C                   Z-ADD     XPTOTA        XPTOTP
     C     P3TOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     P3TOTC        XPTOTC
     C                   END                                                    END CACHRG GT LPHYMD
     C                   END                                                    END LPHYMD LE *ZERO
     C*
     C* If this is an additional (minutes) code, and the major code is
     C* disallowed, then disallow the additional code
     C     P3CPT4        IFEQ      '97001'
     C     P3CPT4        OREQ      '97101'
     C     P3CPT4        OREQ      '97201'
     C     P3CPT4        OREQ      '97221'
     C     P3CPT4        OREQ      '97241'
     C     P3CPT4        OREQ      '97501'
     C     P3CPT4        OREQ      '97521'
     C     P3CPT4        OREQ      '97531'
     C     P3CPT4        OREQ      '97541'
     C     P3CPT4        OREQ      '97701'
     C     P3CPT4        OREQ      '97755'
     C     P3CPT4        OREQ      '97764'
     C*
     C     P3CPT4        IFEQ      '97001'
     C                   MOVE      '97000'       XXCPT4
     C                   END
     C*
     C     P3CPT4        IFEQ      '97101'
     C                   MOVE      '97100'       XXCPT4
     C                   END
     C*
     C     P3CPT4        IFEQ      '97201'
     C                   MOVE      '97200'       XXCPT4
     C                   END
     C*
     C     P3CPT4        IFEQ      '97221'
     C                   MOVE      '97220'       XXCPT4
     C                   END
     C*
     C     P3CPT4        IFEQ      '97241'
     C                   MOVE      '97240'       XXCPT4
     C                   END
     C*
     C     P3CPT4        IFEQ      '97501'
     C                   MOVE      '97500'       XXCPT4
     C                   END
     C*
     C     P3CPT4        IFEQ      '97521'
     C                   MOVE      '97520'       XXCPT4
     C                   END
     C*
     C     P3CPT4        IFEQ      '97531'
     C                   MOVE      '97530'       XXCPT4
     C                   END
     C*
     C     P3CPT4        IFEQ      '97541'
     C                   MOVE      '97540'       XXCPT4
     C                   END
     C*
     C     P3CPT4        IFEQ      '97701'
     C                   MOVE      '97700'       XXCPT4
     C                   END
     C*
     C     P3CPT4        IFEQ      '97755'
     C                   MOVE      '97754'       XXCPT4
     C                   END
     C*
     C     P3CPT4        IFEQ      '97764'
     C                   MOVE      '97763'       XXCPT4
     C                   END
     C*
     C                   Z-ADD     XXYMD         SVYMD
     C                   Z-ADD     HDYMD         XXYMD
     C*
     C     CAKEY8        CHAIN     PCA09                              60
     C     *IN60         IFEQ      '0'
     C     XRTOTP        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     P3DSEQ        QQS(Q)
     C                   MOVE      '78'          QQC(Q)
     C* Err Msg 78: "Addtl minutes code entered before initial minutes.
     C*             Verify payment"
     C                   END                                                    END 0 *IN60
     C*
     C                   Z-ADD     SVYMD         XXYMD
     C*
     C                   END                                                    END EQ 97001, ETC.
     C*
     C*
     C* Update the corresponding FDCAP record with the values calculated
     C* for the FACAPOK record
     C                   EXSR      UPDAT2
     C*
     C* Read next workfile record for this summary code
     C     CAKYP4        READE     PCAWK3                                 60
     C                   END                                                    END DOWEQ 60
     C*
     C                   END                                                    END DO 50  P
     C*
     C*
     C     PHYS2E        ENDSR
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
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '76'          QQC(Q)
     C*** Err Msg 76: "CPT modifier used with this charge.  Verify use
     C***              and payment."
     C*
     C* ===============================================================
     C* Modifier -26 = Professional Component
     C     XPPMOD        IFEQ      '26'
     C     DHPRCD        IFGE      '90701'
     C     DHPRCD        ANDLE     '99199'
     C     SXMRA         MULT      .40           SXMRA
     C                   ELSE
     C                   Z-ADD     SXPROF        SXMRA
     C                   END
     C                   END                                                    END EQ '26'
     C*
     C* ===============================================================
     C* Modifier -27 = TECHNICAL Component
     C     XPPMOD        IFEQ      '27'
     C     DHPRCD        IFGE      '90701'
     C     DHPRCD        ANDLE     '99199'
     C     SXMRA         MULT      .60           SXMRA
     C                   ELSE
     C                   Z-ADD     SX92N1        SXMRA
     C                   END
     C                   END                                                    END EQ '27'
     C*
     C* ===============================================================
     C* Modifier -80 = ASSISTANT SURGEON
     C     XPPMOD        IFEQ      '80'
     C* REDUCE 20%
     C     SXMRA         MULT      .20           SXMRA
     C*
     C                   END                                                    END XPPMOD
     C*
     C*
     C* ===============================================================
     C* Modifier -81 MINIMUM ASSITANT SURGEON
     C     XPPMOD        IFEQ      '81'
     C* REDUCE 10%
     C     SXMRA         MULT      .10           SXMRA
     C*
     C                   END                                                    END XPPMOD
     C* ===============================================================
     C*
     C     PMODE         ENDSR
     *********************************************************************
     *** Radiology                  Codes: 70010 - 79999         R A D I *
     *********************************************************************
     C     RADI          BEGSR
     C*
     C     DHAREA        IFNE      'PC'
     C*
     C                   Z-ADD     1             R                 5 0
     C                   MOVEL     DHPRCD        XXRDMD
     C                   MOVE      XPPMOD        XXRDMD
     C     XXRDMD        LOOKUP    RAD1(R)                                60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     RUNT(R)       SXMRA
     C                   END
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             R                 5 0
     C                   MOVEL     DHPRCD        XXRDMD
     C                   MOVE      XPPMOD        XXRDMD
     C     XXRDMD        LOOKUP    RAD2(R)                                60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     RUNT1(R)      SXMRA
     C                   END
     C*
     C                   END                                                    END DHAREA NE 'PC'
     C*
     C     SXMRA         MULT      XXRADI        SXMRA
     C*
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per PG. 218-219
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '27'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '55'
     C     XPPMOD        ANDNE     '56'
     C     XPPMOD        ANDNE     '75'
     C     XPPMOD        ANDNE     '76'
     C     XPPMOD        ANDNE     '77'
     C     XPPMOD        ANDNE     '79'
     C     XPPMOD        ANDNE     '80'
     C     XPPMOD        ANDNE     '90'
     C     XPPMOD        ANDNE     '99'
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
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
     C                   ELSE                                                   ELSE EQ 22...
     C                   EXSR      PMOD
     C     XPPMOD        IFEQ      '26'
     C     XPPMOD        OREQ      '27'
     C     SXMRA         MULT      XXRADI        SXMRA
     C                   END
     C*
     C                   END                                                    END EQ 22
     C*
     C                   END                                                    END NE *BLANK
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
     *** Surgery                    Codes: 10040 - 69979         S U R G *
     *********************************************************************
     C     SURG          BEGSR
     C*
     C     SXMRA         MULT      XXSURG        SXMRA
     C*
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per PG. 85-87
     C     XPPMOD        IFNE      '20'
     C     XPPMOD        ANDNE     '22'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '47'
     C     XPPMOD        ANDNE     '50'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '54'
     C     XPPMOD        ANDNE     '55'
     C     XPPMOD        ANDNE     '56'
     C     XPPMOD        ANDNE     '62'
     C     XPPMOD        ANDNE     '66'
     C     XPPMOD        ANDNE     '76'
     C     XPPMOD        ANDNE     '77'
     C     XPPMOD        ANDNE     '78'
     C     XPPMOD        ANDNE     '79'
     C     XPPMOD        ANDNE     '80'
     C     XPPMOD        ANDNE     '81'
     C     XPPMOD        ANDNE     '90'
     C     XPPMOD        ANDNE     '99'
     C*
     C                   Z-ADD     1             Q
     C     '0Z000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   ELSE                                                   ELSE EQ 20...
     C*
     C     XPPMOD        IFEQ      '47'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AH'          QQC(Q)
     C* Err Msg AH  "Refer to corresponding Anesthesia code for
     C*             base units."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END *IN62
     C*
     C     XPPMOD        IFEQ      '62'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '65'          QQC(Q)
     C* Err Msg 65: "Item is two surgeons mod-62, refer to fs
     C*             for reimbursement."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END *IN62
     C*
     C     XPPMOD        IFEQ      '66'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '66'          QQC(Q)
     C* Err Msg 66: "Item is surgical team mod-66, refer to fs
     C*             for reimbursement."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END *IN66
     C*
     C     XPPMOD        IFEQ      '54'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '70'          QQC(Q)
     C* Err Msg 70: "The item indicates Surgical Care Only mod-54
     C*             Refer to fee schedule for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END *IN66
     C*
     C     XPPMOD        IFEQ      '55'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '64'          QQC(Q)
     C* Err Msg 64: "The item indicates Postoper Mgmt Only mod-55
     C*             Refer to fee schedule for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END *IN66
     C*
     C                   EXSR      PMOD
     C     XPPMOD        IFEQ      '26'
     C     SXMRA         MULT      XXSURG        SXMRA
     C                   END
     C*
     C                   END                                                    END EQ 20
     C*
     C                   END                                                    END NE *BLANK
     C*
     C* ------------------------------------------------------------
     C*   Per pg. 84, 18:
     C*       A first surgical assitant with current certification
     C*       assisting in surgery identify surgery performed by
     C*       code number and description of procedure, bill at 10%
     C*       of the code fee.  The code(s) must coincide with
     C*       those of the surgeon.
     C*
     C     SVSPCD        IFEQ      'AS1'
     C     SXMRA         MULT      .10           SXMRA
     C* Chain to detail charge file, if you do NOT find a charge that
     C* was paid, then give an error message
     C                   MOVE      DHPRCD        XXCPT4
     C                   MOVE      *BLANKS       XXPMOD
     C     CAKY8         CHAIN     PCA06                              60
     C     *IN60         IFEQ      '1'
     C     X6TOTP        OREQ      *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '58'          QQC(Q)
     C*** Err Msg 58: "The surgeon's charge for this procedure has not
     C***              been paid - Please review"
     C                   END                                                    END 1 *IN60
     C                   END                                                    END EQ PA
     C* ===============================================================
     C* Write record to Workfile
     C* Code has passed all edits - Write to the FACAPNE Workfile
     C* Only load "current charges" into the FACAPNE Workfile
     C*
     C*  EXCEPTIONS TO THIS RULE WILL BE PROCEDURES FOUND IN TABLE NC1
     C*  THESE WILL NOT BE WRITTEN TO THE WORK FILE
     C*
     C                   Z-ADD     1             T                 5 0
     C     DHPRCD        LOOKUP    NC1(T)                                 60
     C     *IN60         CABEQ     '1'           NOCUT
     C*
     C* ===============================================================
     C* IF TOTAL CHARGED AMOUNT IS 50% OR LESS THAN 50% OF THE MRA, PAY
     C* THE CHARGE IN FULL IF THE PROCEDURES WERE PERFORMED THROUGH THE
     C* SAME INCISION.  IF THE PROCEDURES WERE PERFORMED THROUGH DIFFERENT
     C* INCISIONS THEN IF TOTAL CHARGED AMOUNT IS 75% OR LESS THAN 75% OF
     C* THE MRA, PAY THE CHARGE IN FULL
     C                   MOVE      *BLANKS       HLDPRT
     C                   Z-ADD     *ZEROS        HLDMRA
     C*
     C                   MOVE      '10000'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C     XRCPT4        IFGE      '10000'
     C     XRCPT4        ANDLE     '69979'
     C     XRDSEQ        ANDNE     CADSEQ
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKY          SETGT     PPR01
     C     NXTPR6        TAG
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTPR6
     C     PRMRA         MULT      XXSURG        X1MRA
     C     XRTOTC        IFLT      X1MRA
     C                   Z-ADD     XRTOTC        X1MRA
     C                   END
     C                   ELSE
     C                   Z-ADD     *ZEROS        X1MRA
     C                   END
     C*
     C     X1MRA         IFGT      HLMRA
     C                   Z-ADD     X1MRA         HLDMRA
     C                   MOVEL     XRPART        HLDPRT
     C                   END
     C                   END
     C     CAKY9A        READE     PCA09                                  54
     C                   END
     C*
     C     DHAREA        IFEQ      HLDPRT
     C     SXMRA         MULT      .5            X1MRA
     C     CACHRG        IFLE      X1MRA
     C                   GOTO      NOCUT
     C                   END
     C                   ELSE
     C     SXMRA         MULT      .75           XX75CT
     C     X1MRA         SUB       XX75CT        X1MRA
     C     CACHRG        IFLE      X1MRA
     C                   GOTO      NOCUT
     C                   END
     C                   END
     C*
     C* ===============================================================
     C* IF TOTAL CHARGED AMOUNT IS LOWER THAN THE MRA LOAD TOTAL CHARGED
     C* AMOUNT INTO WORKFILE INSTEAD OF MRA
     C     CACHRG        IFLT      SXMRA
     C                   Z-ADD     CACHRG        SXMRA
     C                   END
     C*
     C                   Z-ADD     SXMRA         CAMRA
     C                   MOVE      'S'           CATYPE
     C*
     C                   Z-ADD     1             R                 5 0
     C     DHPRCD        LOOKUP    SUR2(R)                                60
     C     *IN60         IFEQ      '1'
     C                   MOVE      SUR1(R)       XXCPT4
     C     CAKY9         CHAIN     PCA09                              54
     C     *IN54         IFEQ      '0'
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKY          SETGT     PPR01
     C     NXTPR5        TAG
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTPR5
     C     PRMRA         MULT      XXSURG        X1MRA
     C     XRTOTC        IFLT      X1MRA
     C                   Z-ADD     XRTOTC        X1MRA
     C                   END
     C     X1MRA         ADD       CAMRA         CAMMRA
     C                   ELSE
     C                   Z-ADD     CAMRA         CAMMRA
     C                   END
     C                   ELSE
     C                   Z-ADD     CAMRA         CAMMRA
     C                   END
     C                   ELSE
     C                   Z-ADD     CAMRA         CAMMRA
     C                   END
     C*
     C                   Z-ADD     CACHRG        CATOTC
     C*
     C                   WRITE     PCAWK1
     C                   GOTO      SURGE
     C*
     C     NOCUT         TAG
     C*
     C*   If current code exists in table, must have associated code else
     C*     error has occurred
     C*
     C                   Z-ADD     1             R                 5 0
     C                   MOVE      '0'           *IN54
     C     SURLOK        TAG
     C     DHPRCD        LOOKUP    SUR1(R)                                60
     C     *IN60         IFEQ      '1'
     C                   MOVE      SUR2(R)       XXCPT4
     C     CAKY9         CHAIN     PCA09                              54
     C     *IN54         IFEQ      '1'
     C     R             IFLT      363
     C                   ADD       1             R
     C                   GOTO      SURLOK
     C                   ELSE
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "A major code must be present to pay this item"
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
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "A major code must be present to pay this item"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
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
     C     SURGE         ENDSR
     C********************************************************************
     C** Process SURG Codes for DIFFERENT PERCENTAGES          S U R G 2 *
     C*  This subroutine is called at each date, provider level break    *
     C********************************************************************
     C     SURG2         BEGSR
     C*
     C*----------------------------------------------------------------
     C*--- Reading the SURG Detail Charge Workfile --------------------
     C*
     C* ===============================================================
     C*
     C                   Z-ADD     0             COUNT
     C                   MOVE      *BLANKS       HLPART
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
     C     XRCPT4        ANDLE     '69979'
     C     XRTOTP        IFNE      *ZEROS
     C                   ADD       1             COUNT
     C                   MOVEL     XRPART        HLPART
     C                   END                                                    END NE *ZEROS
     C     CAKY9D        READE     PCA09                                  61
     C                   END                                                    END 0 *IN61
     C*
     C                   END                                                    END 0 *IN60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       P2EOMB
     C*
     C* Per pg. 81
     C*
     C     COUNT         IFLE      0
     C                   Z-ADD     1             P2BRP                          100%
     C                   ADD       1             COUNT
     C                   MOVEL     P2PART        HLPART
     C                   ELSE                                                   ELSE EQ 0
     C     COUNT         IFGE      1
     C     P2PART        IFEQ      HLPART
     C                   Z-ADD     .50           P2BRP                          50%
     C                   ADD       1             COUNT
     C                   ELSE
     C                   Z-ADD     .75           P2BRP
     C                   END                                                    END P2PART
     C                   END                                                    END  EQ 1
     C                   END                                                    END  LE 0
     C*
     C     P2MRA         MULT      P2BRP         P2MRA
     C*
     C     P2TOTC        COMP      P2MRA                              54        >>>>>
     C   54              Z-ADD     P2MRA         P2TOTA
     C  N54              Z-ADD     P2TOTC        P2TOTA
     C     P2TOTC        SUB       P2TOTA        P2TOTD
     C                   Z-ADD     P2TOTA        P2TOTP
     C*
     C*  Using FACAPNE record, chain to FDCAP record to update
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
     C                   MOVEL     P2PMOD        XRPMOD
     C     P2TOTD        IFGT      *ZERO
     C     P2EOMB        ANDEQ     *BLANKS
     C                   MOVE      '202'         P2EOMB
     C* EOMB Code 202: "Charges exceed maximum allowance"
     C                   END                                                    END P2TOTD GT *ZERO
     C*
     C                   MOVE      P2EOMB        XREOMB
     C*
     C     P2EOMB        IFNE      *BLANKS
     C*
     C                   MOVE      P2EOMB        XPEOMB
     C                   Z-ADD     P2DSEQ        KKDSEQ
     C                   EXSR      EOMB
     C*
     C                   ENDIF                                                  P2EOMB NE BLANKZERO
     C*
     C                   MOVE      Q1USER        XRUSER
     C                   MOVE      Q1PGM         XRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XRDT
     C                   Z-ADD     DSTIME        XRTIME
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
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
     C                   MOVE      '202'         XPEOMB
     C*** EOMB 202: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   END
     C     XPEOMB        IFNE      *BLANKS
      *
     C                   Z-ADD     SVDSEQ        KKDSEQ
     C                   EXSR      EOMB
      *
     C                   ENDIF
     C*
     C* ===============================================================
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM.
     C*
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     DHKY2         CHAIN     PDH03                              54
     C*
     C     *IN54         DOWEQ     *OFF
     C*
     C                   Z-ADD     D1DSEQ        KKDSEQ
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
     C  N60              Z-ADD     999           Q
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
     C* If Fee Schedule non-reimburseable ... display error message
     C* XX25EM Flag is set at beginning of program where PRFSR eq 'N'
     C     XX25EM        IFEQ      '1'
     C                   MOVE      DHPRCD        XXCPT4
     C     CAKY6         CHAIN     PCA06                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 61
     C  N61              Z-ADD     999           Q
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
     C                   MOVE      *BLANKS       X1PMOD
     C                   MOVE      *BLANK        XPEOMB
     C* ===============================================================
     C*
     C     UPDAE         ENDSR
     C********************************************************************
     C** UPDATE SUBROUTINE FOR SURG  FILE FACAPNE             U P D A T2 *
     C********************************************************************
     C     UPDAT2        BEGSR
     C*
     C* ---------------------------------------------------------------
     C*
     C     XPTOTD        IFGT      *ZERO
     C     XPEOMB        ANDEQ     *BLANK
     C     XPTOTP        ANDGT     *ZEROS
     C                   MOVE      '202'         XPEOMB
     C* EOMB Code 202: "Charges exceed maximum allowance found in
     C*                 in reimbursement manual"
     C                   END
     C*
      *
     C     XPEOMB        IFNE      *BLANKS
     C                   Z-ADD     P3DSEQ        KKDSEQ
     C                   EXSR      EOMB
      *
     C                   ENDIF
     C* ---------------------------------------------------------------
     C*
     C*  Test for a possible duplicate claim.
     C     P3CPT4        IFNE      *BLANK
     C                   MOVE      P3CPT4        XXCPT4
     C                   MOVEL     P3PMOD        XXPMOD
     C                   Z-ADD     XXYMD         SVYMD
     C                   Z-ADD     HDYMD         XXYMD
     C*
     C     DHKY2         CHAIN     PDH03                              54
     C*
     C     *IN54         DOWEQ     *OFF
     C*
     C                   Z-ADD     D1DSEQ        XXDSEQ
     C*
     C     CAKYP3        CHAIN(N)  PCA0P                              61
     C*
     C     *IN61         IFEQ      *OFF
     C     PXFDAT        ANDEQ     P3FDAT
     C     PXFEIN        ANDEQ     P3FEIN
     C     P3DSEQ        ANDNE     PXDSEQ
     C     PXFLAG        ANDNE     'P'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     P3DSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C*
     C                   ENDIF                                                  *IN61=OFF
     C*
     C     DHKY2         READE     PDH03                                  54
     C*
     C                   ENDDO
     C*
     C                   ENDIF                                                  P3CPT4 NE BLANK
     C*
     C* ---------------------------------------------------------------
     C*  Using FACAPNE record, chain to FDCAP record to update
     C*  Only process the record if not an override type
     C     CAKYA3        CHAIN     PCA08                              54
     C*
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     XRCSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C*
     C     CSOVRD        IFNE      'Y'
     C                   Z-ADD     XPTOTA        XRTOTA
     C                   Z-ADD     XPTOTD        XRTOTD
     C                   Z-ADD     XPTOTP        XRTOTP
     C                   MOVE      XPEOMB        XREOMB
     C*
     C                   MOVE      Q1USER        XRUSER
     C                   MOVE      Q1PGM         XRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XRDT
     C                   Z-ADD     DSTIME        XRTIME
     C*
     C*  End - of conditional move routine.
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C  N54              UPDATE    PCA08
     C*
     C* ---------------------------------------------------------------
     C*
     C*   If fee schedule NON-REIMBURSEABLE ... display error message
     C*   XX25EM flag is set at beginning of program where PRFSR EQ 'N'
     C*
     C     XX25EM        IFEQ      '1'
     C                   Z-ADD     P3FEIN        CAFEIN
     C                   Z-ADD     P3MOD         CAMOD
     C                   MOVE      P3CPT4        XXCPT4
     C                   MOVEL     P3PMOD        XXPMOD
     C                   Z-ADD     XXYMD         SVYMD
     C                   Z-ADD     HDYMD         XXYMD
     C     CAKY8         CHAIN     PCA06                              54
     C     *IN54         IFEQ      '0'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     P3DSEQ        QQS(Q)
     C                   MOVE      '25'          QQC(Q)
     C* Err Msg 25: "This item is non-reimburseable under the Fee Sched
     C                   MOVE      '0'           XX25EM
     C                   END                                                    ENDIF *IN54
     C                   Z-ADD     SVYMD         XXYMD
     C                   END                                                    ENDEQ '1'
     C                   END
     C*
     C* ---------------------------------------------------------------
     C*
     C** Clear the work save fields.
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVEL     *BLANK        X1PMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE2        ENDSR
     C********************************************************************
     C** UPDATE SUBROUTINE FOR ANES  FILE FACAPNE             U P D A T3 *
     C********************************************************************
     C     UPDAT3        BEGSR
     C*
     C* ---------------------------------------------------------------
     C*
     C     XPTOTD        IFGT      *ZERO
     C     XPEOMB        ANDEQ     *BLANK
     C     XPTOTP        ANDGT     *ZEROS
     C                   MOVE      '202'         XPEOMB
     C* EOMB Code 202: "Charges exceed maximum allowance found in
     C*                 in reimbursement manual"
     C                   END
      *
     C     XPEOMB        IFNE      *BLANKS
      *
     C                   Z-ADD     P4DSEQ        KKDSEQ
     C                   EXSR      EOMB
      *
     C                   ENDIF
     C* ---------------------------------------------------------------
     C*
     C*  Test for a possible duplicate claim.
     C*
     C     P4CPT4        IFNE      *BLANK
     C                   MOVE      P4CPT4        XXCPT4
     C                   MOVEL     P4PMOD        XXPMOD
     C                   Z-ADD     XXYMD         SVYMD
     C                   Z-ADD     HDYMD         XXYMD
     C*
     C     DHKY2         CHAIN     PDH03                              54
     C*
     C     *IN54         DOWEQ     *OFF
     C*
     C                   Z-ADD     D1DSEQ        XXDSEQ
     C*
     C     CAKYP3        CHAIN(N)  PCA0P                              61
     C*
     C     *IN61         IFEQ      *OFF
     C     PXFDAT        ANDEQ     P4FDAT
     C     PXFEIN        ANDEQ     P4FEIN
     C     P4DSEQ        ANDNE     PXDSEQ
     C     PXFLAG        ANDNE     'P'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     P4DSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C*
     C                   ENDIF                                                  *IN61=OFF
     C*
     C     DHKY2         READE     PDH03                                  54
     C*
     C                   ENDDO
     C*
     C                   ENDIF                                                  P4CPT4 NE BLANK
     C* ---------------------------------------------------------------
     C*  Using FACAPNE record, chain to FDCAP record to update
     C*  Only process the record if not an override type
     C     CAKYA4        CHAIN     PCA08                              54
     C*
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     XRCSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C*
     C     CSOVRD        IFNE      'Y'
     C                   Z-ADD     XPTOTA        XRTOTA
     C                   Z-ADD     XPTOTD        XRTOTD
     C                   Z-ADD     XPTOTP        XRTOTP
     C                   MOVE      XPEOMB        XREOMB
     C*
     C                   MOVE      Q1USER        XRUSER
     C                   MOVE      Q1PGM         XRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XRDT
     C                   Z-ADD     DSTIME        XRTIME
     C*
     C*  End - of conditional move routine.
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C  N54              UPDATE    PCA08
     C*
     C* ---------------------------------------------------------------
     C*
     C*   If fee schedule NON-REIMBURSEABLE ... display error message
     C*   XX25EM flag is set at beginning of program where PRFSR EQ 'N'
     C*
     C     XX25EM        IFEQ      '1'
     C                   Z-ADD     P4FEIN        CAFEIN
     C                   Z-ADD     P4MOD         CAMOD
     C                   MOVE      P4CPT4        XXCPT4
     C                   MOVEL     P4PMOD        XXPMOD
     C                   Z-ADD     XXYMD         SVYMD
     C                   Z-ADD     HDYMD         XXYMD
     C     CAKY8         CHAIN     PCA06                              54
     C     *IN54         IFEQ      '0'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     P4DSEQ        QQS(Q)
     C                   MOVE      '25'          QQC(Q)
     C* Err Msg 25: "This item is non-reimburseable under the Fee Sched
     C                   MOVE      '0'           XX25EM
     C                   END                                                    ENDIF *IN54
     C                   Z-ADD     SVYMD         XXYMD
     C                   END                                                    ENDEQ '1'
     C                   END
     C*
     C* ---------------------------------------------------------------
     C*
     C** Clear the work save fields.
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVEL     *BLANK        X1PMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE3        ENDSR
     *********************************************************************
     OPCA01     E            ECA01
     OPCA08     E            ECA08
     *********************************************************************
**  PCD1 - Phys Therapy Code vs. SMCD Summary Code
9720097200
9720197200
9722097220
9722197200
9724097240
9724197240
9726097260
9750097500
9750197500
9752097520
9752197520
9753097530
9753197530
9754097540
9754197540
9770097700
9770197700
9770297700
9775297752
9775397753
9775497754
9775597754
9776397763
9776497763
**  PCD2 - Phys Therapy Code vs. MRA - Phys Therapy maximum MRA (Unit Value)
97200000004440
97201000006000
97220000004320
97221000007680
97240000005280
97241000009600
97260000002880
97500000003600
97501000006480
97520000003600
97521999999999
97530000003360
97531999999999
97540000003600
97541000005220
97700000005400
97701000007800
97702000007800
97752000005820
97753000004200
97754000005820
97755000009600
97763000005820
97764999999999
**  PCD3 - Phys Therapy Code vs. ADD - Should we add to Summ Code bucket
97001
97101
97201
97221
97241
97501
97521
97531
97541
97701
97702
97755
97764
**  ANEPM1/ANEPM2 - Anesthesia Px Physical Status Modifiers/Addtl Unit Values
P10
P20
P31
P42
P53
P60
**  QCA QCMDEXC TABLE
DLTOVR FILE(FACALNE)
OVRDBF FILE(FACALNE)  TOFILE(QTEMP/FACALNE)
DLTOVR FILE(FACAL1NE)
OVRDBF FILE(FACAL1NE)  TOFILE(QTEMP/FACAL1NE)
DLTOVR FILE(FACAL2NE)
OVRDBF FILE(FACAL2NE)  TOFILE(QTEMP/FACAL2NE)
DLTOVR FILE(FACAPNE)
OVRDBF FILE(FACAPNE)  TOFILE(QTEMP/FACAPNE)
**  NC1 CODES NOT TO CUT LIST 1
11001111011120111701117111173115261157871700117002
17101171022258522650261252686126863273584111444955
47001586115941259525630666307663078630826308663088
64787648306483264837648596487211732119221510115121
15201152211524117201173071900119340221452214822230
27692333503353033830356813621836248611066113061712
61795630356304863057630916330864443647276477864783
6487464876649016490267335
**  SUR1/SUR2: A MUST HAVE B AND A WILL NOT BE CUT
1100111000
1110111100
1120111200
1170111700
1171111710
1173111730
1173211730
1510115100
1512115120
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
1192211920
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
**  IPT1 - Initial Phys Therapy Codes for additional minutes code 97001
97000
97010
97012
97014
97016
97018
97020
97022
97024
97026
97028
97039
**  IPT2 - Initial Phys Therapy Codes for additional minutes code 97101
97100
97110
97112
97114
97116
97118
97120
97122
97124
97126
97128
97139
**  RAD1/RUNT - RADIOLOGY CODES THAT HAVE SPECIFIC VALUES IF MOD 22 OR 52 USED
7054022000002450
7054052000001720
7055122000002450
7055152000001720
7155022000002360
7155052000001620
7214122000002740
7214152000002030
7214622000002630
7214652000002030
7214822000002630
7214852000002030
7219622000002630
7219652000002030
7322022000002060
7322052000001500
7322122000000000
7322152000000000
7372022000002060
7372052000001500
7372122000000000
7372152000000000
7418122000002630
7418152000002030
7555222000002440
7555252000001830
7640022000002340
7640052000001740
**  RAD2/RUNT1 - RADIOLOGY CODES THAT HAVE SPECIFIC VALUES IF MOD 22 OR 52 USED
7054022000000470
7054052000000310
7055122000000470
7055152000000310
7155022000000470
7155052000000301
7214122000000470
7214152000000310
7214622000000470
7214652000000310
7214822000000470
7214852000000310
7219622000000470
7219652000000310
7322022000000470
7322052000000310
7322122000000000
7322152000000000
7372022000000470
7372052000000310
7372122000000000
7372152000000000
7418122000000470
7418152000000310
7555222000000470
7555252000000310
7640022000000470
7640052000000310
