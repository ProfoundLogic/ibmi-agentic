     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4072RLA
     H*    DESCRIPTION: ADJUDICATION (LOUISIANA RULES - 08/01/94)
     H*   This program is called from F0070R (Claim Adjudication) and will
     H* perform the actual adjudication of the detail claim records writ-
     H* ten by F0070R.  This program was derrived from the Louisiana State
     H* Fee Schedule Reimbursment Manual issued on 08/01/94
     H* This Sub-program is intended to be used to calculate the amount
     H* that is due a Provder of services, by the Carrier of the Work comp
     H* insurance.  It should be used for any serice provided on or after
     H* 08/01/94.
     H*****************************************************************
     H*  CREATED: 9/15/94   C:05   PROJECT: LA008
     H*****************************************************************
     H* Information needed for each year's adjudication program rules:
     H*    1.  Update the effective date range for reading the detail charges
     H*        (found in the mainline section)
     H*    2.  Update the XXANES conversion factor
     H*        (found in *LIKE section)
     H*****************************************************************
     H*  SPECIALTY CODES USED IN THIS PROGRAM:
     H*
     H*  HEAR Subroutine
     H*          HAD = Hearing Aid Dispenser
     H*
     H*  PHYS Subroutine
     H*          PHT = Physical Therapist
     H*          OCT = Occupational Therapist
     H*          PSC = Psychologist
     H*          VOC = Vocational (Rehab) Specialist
     H*
     H*  SURG Subroutine
     H*          AS  = Assistant Surgeon
     H*          PA  = Physician Assistant
     H*          NP  = Nurse Practioner
     H*          ST  = Surgical Technician
     H*
     H*  VSCR Subroutine
     H*          OPC = Optician
     H*
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  12/18/00 C:62 PROJ: MC048 11/20/00
     H*                LOAD UNITS INTO ANESTHESIA MINUTES.
     H*
     H*  06/24/99 C:79 PROJ: LA013
     H*                Modified to work with Provider Bills
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
     H*  09/22/98 C:79  Support: 13895
     H*                 The date parm for SRPAST8 is now 8 numeric
     H*                 instead of 8 alpha
     H*
     H*  05/27/98 C:31  Support: 10954
     H*                 CPT4 Issue
     H*                 1. Add test for EOMB 217
     H*
     H*  12/20/96 C:59  REQ/PROJ: MC046/#6923
     H*                 1. ADDED NECESSARY CODE TO CORRECT EDIT FOR
     H*                    CPT4 CODES 99201 - 99412.  REMOVE EDIT FOR
     H*                    92500-92599.
     H*
     H*  12/09/96 C:59  REQ/PROJ: MC046/#5911
     H*                 1. Added necessary code to check for technical
     H*                    component, Modifier (27).  If exists pay
     H*                    predefined amount, else pay 100% sxmra.
     H*
     H*
     H*  11/01/96 C:59  REQ/PROJ: MC046/#5514
     H*                 1. ADDED NECESSARY CODE TO CORRECT MODIFIER
     H*                    ERRORS.  ADDED CAKYP1, CAKY8X TO REPLACE
     H*                    CAKYP AND CAKY8 IN UPDAT2, UPDAT3.
     H*
     H*  04/11/96 C:45  REQ/PROJ: LAMNT
     H*                 1. ADDED NECESSARY CODE TO CORRECTLY CALCULATE
     H*                    MODIFIER 81, EOMB 243.
     H*
     H*  03/15/95 C:20  REQ/PROJ: 145/LA010
     H*                 1. IF PROCEDURE IS AN AWP(AVERAGE WHOLESALE PRICE)
     H*                    PROCEDURE, CHAIN TO FDHCP TO GET THE AVERAGE
     H*                    WHOLESALE PRICE THAT WAS ENTERED.
     H*
     H*  11/30/94 C:20  REQ/PROJ: LAMNT
     H*                 1. BEFORE TAKING 20% FOR SPLTY CODE 'AS' SEE IF CHARGES
     H*                    HAVE MOD 80 ATTACHED.
     H*
     H*****************************************************************
     H*  INDICATOR USAGE:
     H*  54    Use with Primary FDCAP file (i.e. DOWEQ, etc on reading charges)
     H*  60    General Use
     H*  61    General Use
     H*  62    General Use
     H*****************************************************************
     H*  SECTION                                 CHPT   SUBR         CODES
     H*  Visions Care Services                    31    VSCR    V2020 - V2799
     H*  Hearing Aid Equipment                    33    HEAR    V5000 - V5364
     H*  Nursing/Attendant Care & Home Health     35    NURS    W0050 - W0902
     H*  Home & Vehicle Modification              37    OTHE    HVMOD (BR)
     H*  Medical Transportation                   39    MDTR    A0010 - A0999
     H*  Durable Medical Equipment                41    DME     E0100 - E1702
     H*  Prosthetic & Orthopedic Equipment        43    PROR    L0100 - L9999
     H*  Respiratory Services                     45    DME     E0424 - E1406
     H*  Misc. Claimant                           47    OTHE    MILES/BURY
     H*  Vocational Rehabilitation Services       49    VOC     X0710 - X0750
     H*  Medical                                  51    ANES    00100 - 01999
     H*                                                 ANES2   00100 - 01999
     H*                                                 MEDI    90700 - 96999
     H*                                                         97800 - 99199
     H*                                                 OCT     OT010 - OT680
     H*                                                 OFFI    99201 - 99499
     H*                                                 OFFI2   99201 - 99499
     H*                                                 PATH    80002 - 89399
     H*                                                 PHT     PT010 - PT680
     H*                                                 PHYS    97010 - 97799
     H*                                                 RADI    70010 - 79999
     H*                                                 SURG    10040 - 69979
     H*                                                 SURG2   10040 - 69979
     H*  Medical & Surgical Supplies              none  MDSU    A4190 - A5149
     H*****************************************************************
     F*  FDCALH is the Primary File Read (PCA01)
     FFDCALH    UF   E           K DISK
     F*
     FFACAPLA   O    E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK1)
     FFACALLA   IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK2)
     FFACAL1LA  IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK3)
     FFACAL2LA  IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK4)
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
     FFMGZL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMPRLLA   IF   E           K DISK
     FFMRCL     IF   E           K DISK
     FFTABLLA   IF   E           K DISK
     F                                     IGNORE(PRC01)
     FSMUPL     IF   E           K DISK
     F*** BEGIN ADD *** 03/15/95 ***********************************************
     FFDHCL     IF   E           K DISK
     F*** END ADD *** 03/15/95 *************************************************
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
     FFAEHL     IF   E           K DISK

     D*
     D* "Entry" Arrays
     D QQS             S              5    DIM(9999)                            DETAIL SEQ #
     D QQC             S              2    DIM(9999)                            ERROR MESSAGES
     D*
     D* Compile-time Arrays
     D ANEPM1          S              2    DIM(6) CTDATA PERRCD(1)              ANES Px Modifiers
     D ANEPM2          S              1  0 DIM(6) ALT(ANEPM1)
     D QCA             S             55    DIM(8) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D SUR1            S              5    DIM(205) CTDATA PERRCD(1)            NO CUT/MAJOR-MIN
     D SUR2            S              5    DIM(205) ALT(SUR1)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     I*
     I*  Rename the duplicate field names.
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
     I* Record Format for SURG2 Workfile FACALLA
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
     I* Record Format for OFFI2 Workfile FACAL1LA
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
     I              CABREC                      P3BREC
     I              CABRP                       P3BRP
     I              CATYPE                      P3TYPE
     I*
     I* Record Format for ANES2 Workfile FACAL2LA
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
     I              CABREC                      P4BREC
     I              CABRP                       P4BRP
     I              CATYPE                      P4TYPE
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
     C     CAKYP         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    KKDSEQ
     C*
     C*** BEGIN ADD *** 11/01/96 **************************************
     C*  CREATED KEY LIST TO REPLACE CAKYP.  IN CAKYP THEY WERE OVER
     C*  WRITING CA FIELDS
     C*
     C     CAKYP1        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXPMOD
     C*
     C     CAKY8X        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXPMOD
     C*** END ADD ***** 11/01/96 **************************************
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
     C                   KFLD                    QQEOB#
     C                   KFLD                    P4DSEQ
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
     C     GZKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    XXFRZP
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
     C*** BEGIN ADD *** 03/15/95 ***********************************************
     C*
     C* DETAIL CHARGE AUXILIARY FILE
     C*
     C     HCKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    CADSEQ
     C*** END ADD *** 03/15/95 *************************************************
     C*
     C*  Physician header file key list (FAEHL)
     C     EHKY          KLIST
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
     C     DHKY3         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
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
     C                   MOVE      *ZEROS        QQC
     C                   MOVE      *ZEROS        QQS
     C*
     C* = Initialization Section ======================================
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDATE            7 0
     C*
     C     *LIKE         DEFINE    DHPRCD        CPTFR
     C     *LIKE         DEFINE    DHPRCD        CPTTO
     C*
     C     *LIKE         DEFINE    CACHK#        HLCHK#
     C     *LIKE         DEFINE    CADSEQ        HLDSEQ
     C     *LIKE         DEFINE    CAEOB#        HLEOB#
     C     *LIKE         DEFINE    CAFEIN        HLFEIN
     C     *LIKE         DEFINE    CAFEIN        HDFEIN
     C     *LIKE         DEFINE    CAFEIN        XXFEIN                         ADD 11/01/96
     C     *LIKE         DEFINE    CAMOD         XXMOD                          ADD 11/01/96
     C     *LIKE         DEFINE    CAFDAT        HLFDAT
     C     *LIKE         DEFINE    CAFDAT        HDYMD
     C     *LIKE         DEFINE    CAFDAT        SVYMD
     C     *LIKE         DEFINE    CAMOD         HLMOD
     C     *LIKE         DEFINE    CAMOD         HDMOD
     C     *LIKE         DEFINE    CAPRTY        HLPRTY
     C*
     C     *LIKE         DEFINE    CADSEQ        H1DSEQ
     C*
     C     *LIKE         DEFINE    CAFDAT        H2FDAT
     C     *LIKE         DEFINE    CAPMOD        H2PMOD
     C*
     C     *LIKE         DEFINE    DHUNTS        PHOURS
     C     *LIKE         DEFINE    DHUNTS        LHOURS
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
     C     *LIKE         DEFINE    PR92N2        SX92N2
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
     C     *LIKE         DEFINE    CABRTC        XPBRTC                         ADD 09/15/94
     C     *LIKE         DEFINE    DHPRCD        XPCPT4
     C     *LIKE         DEFINE    CADIFF        XPDIFF                         ADD 09/15/94
     C     *LIKE         DEFINE    CACHRG        HLTOTC                         ADD 10/10/94
     C     *LIKE         DEFINE    CAEOMB        XPEOMB
     C     *LIKE         DEFINE    CAPMOD        XPPMOD
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
     C     *LIKE         DEFINE    GZFRZP        XXFRZP
     C     *LIKE         DEFINE    PRMRA         XXMRA
     C     *LIKE         DEFINE    DHAREA        XXPART
     C     *LIKE         DEFINE    CAPMOD        XXPMOD
     C     *LIKE         DEFINE    DHUNTS        XXUNIT
     C     *LIKE         DEFINE    CAFDAT        XXYMD
     C     *LIKE         DEFINE    GZZIPA        XXZIPA
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
     C                   Z-ADD     0             Q                 5 0
     C                   Z-ADD     0             S                 3 0
     C                   Z-ADD     0             T                 5 0
     C                   Z-ADD     0             X                 5 0
     C                   Z-ADD     0             COUNT             5 0
     C                   Z-ADD     0             COUNTA            5 0
     C                   Z-ADD     0             CNTB1             5 0
     C                   Z-ADD     0             CNTB2             5 0
     C                   MOVE      *BLANK        FDT1              1
     C                   MOVE      *BLANK        FDT2              1
     C                   Z-ADD     0             FUTRCT            5 0
     C                   Z-ADD     0             HLADDT            7 7          To hold MVR
     C                   Z-ADD     0             HLANMN           10 7          DHUNTS for 15 minute
     C                   Z-ADD     0             HLANTV            3 0          Anes Time Value
     C                   Z-ADD     0             M95834            9 2          MRA for 95834
     C                   MOVE      ' '           OPNFLG            1            File Open Flag
     C                   MOVE      '0'           OFCPD             1            CLEAR TMP IN(NOT USD
     C                   MOVE      ' '           OVRFLG            1            File Override Flag
     C                   Z-ADD     0             PASTCT            5 0
     C                   MOVE      *BLANK        PDT1              1
     C                   MOVE      *BLANK        PDT2              1
     C                   Z-ADD     0             SRFUT             5 0
     C                   Z-ADD     0             SRPST             5 0
      *** BEG CHG *** 09/22/98 ****************************************
     C******************** MOVE *BLANKS   SRDTE   8
     C                   MOVE      *ZEROS        SRDTE             8 0
      *** END CHG *** 09/22/98 ****************************************
     C                   MOVE      *BLANKS       SRERR             1
     C                   MOVE      *BLANKS       SRTERM            1
     C                   MOVE      *BLANKS       SVSPCD            3
     C                   MOVE      ' '           XFIRST            1            First time Wrkf buil
     C                   Z-ADD     0             T95831            9 2          Tot for 95831 series
     C                   Z-ADD     0             XXALOW            9 2
     C                   Z-ADD     0             XXANMN            5 2
     C                   MOVE      *BLANKS       XXEND             2
     C                   Z-ADD     0             XXMODU            2 0
     C                   Z-ADD     0             XXOVRP            3 2
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
     C*
     C                   Z-ADD     50.00         XXANES            5 2           ANES Conversion
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
     C* Chain to the physicain header file to have access to the diag
     C* codes
     C     EHKY          CHAIN     PEH01                              60
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
     C     CAFDAT        IFGE      940801                                       ADD 09/15/94
     C     CARCN         ANDEQ     XXRCN
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
     C* Move body area to FDCAP to be mapped into workfile (P2PART)
     C                   MOVE      DHAREA        CAPART
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
     C                   Z-ADD     0             M95834
     C                   MOVE      QQSPCD        SVSPCD
     C*
     C*** GET MRA AMOUNT FOR SPECIAL RULE
     C                   MOVEL     '95834'       XXCPT4
     C                   MOVE      921101        XXEFDT
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
     C  N60              Z-ADD     9999          Q
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
     C  N60              Z-ADD     9999          Q
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
     C                   MOVE      *ZEROS        XPDIFF                         ADD 09/15/94
     C                   MOVE      *ZEROS        XPBRTC                         ADD 09/15/94
     C*
     C                   MOVE      *BLANKS       XXFRZP
     C                   MOVE      *BLANKS       XXZIPA
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C* Now that the Provider is established, get its Geographic Zip Area
     C* 1st - establish the first 3-bytes of the zip code
     C                   MOVEL     PDZIP         XXFRZP
     C                   END                                                    END 0 *IN60
     C*
     C* 2nd - Get the corresponding Geographic Zip Area record:
     C     GZKY          SETGT     PGZ01
     C                   READP     PGZ01                                  60
     C     *IN60         IFEQ      '0'
     C     XXFRZP        ANDLE     GZTOZP
     C                   MOVE      GZZIPA        XXZIPA
     C                   END                                                    END 0 *IN60
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
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     XXMRA         XPBRTC                         ADD 09/15/94
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   ELSE                                                   ELSE XPPMOD NE *BLAN
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '91'          QQC(Q)
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
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
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
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
     C  N60              Z-ADD     9999          Q
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
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   ELSE
     C*
     C*  If fee schedule reimbursement equals 'N' But the MRA
     C*    is greater than zero, this CPT4 Code has been assigned a
     C*    specific MRA by the insurance company.
     C     CACHRG        COMP      PRMRA                              60
     C   60              MOVE      '002'         XPEOMB
     C* EOMB Code 002: "Charges exceed Reasonable & Customary"
     C   60              Z-ADD     PRMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
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
     C     XPPMOD        IFEQ      'RT'
     C     SXANAM        ANDGT     0
     C                   GOTO      SKPRPT
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
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '91'          QQC(Q)
     C* Err Msg 91: "Carrier to set payment on individual case basis
     C*             based on documentation"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
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
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '24'          QQC(Q)                         ANE MESSAGE
     C* Err Msg 24: "The item is ANE - requires further inspection
     C*             by an adjuster"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
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
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '005'         XPEOMB
     C* EOMB Code 005: "This procedure is included in the basic allowance
     C*                 of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END                                                    END PRRPT EQ 'A'
     C*
     C     SKPRPT        TAG
     C*
     C*** BEGIN ADD *** 03/15/95 ***********************************************
     C*
     C*--------------------------------------------------------------------
     C*-- AVERAGE WHOLESALE PRICE -----------------------------------------
     C     PR1A4         IFEQ      'A'
     C*
     C* CHAIN TO THE DETAIL CHARGE AUXILIARY FILE TO GET AWP
     C*
     C     HCKY          CHAIN     PHC01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     SXMRA         ADD       HC92N2        SXMRA
     C*
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C* Update charge with payment
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END                                                    END *IN60 EQ '0'
     C*
     C                   END                                                    END PR1A4 EQ 'A'
     C*** END ADD *** 03/15/95 *************************************************
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
     C  N60              Z-ADD     9999          Q
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
     C* If first time into program,
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the program is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALLA
     C                   CLOSE     FACAL1LA
     C                   CLOSE     FACAL2LA
     C                   CLOSE     FACAPLA
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL2LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACLA'
     C                   PARM      ' '           QQTYPE
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL2LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACAPLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALLA
     C                   OPEN      FACAL1LA
     C                   OPEN      FACAL2LA
     C                   OPEN      FACAPLA
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
     C*     then, Process all Phys Medicine Codes for the existing date/provider
     C                   EXSR      ANES2
     C                   EXSR      OFFI2
     C                   EXSR      SURG2
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALLA
     C                   CLOSE     FACAL1LA
     C                   CLOSE     FACAL2LA
     C                   CLOSE     FACAPLA
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL2LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACLA'
     C                   PARM      ' '           QQTYPE
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL2LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACAPLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALLA
     C                   OPEN      FACAL1LA
     C                   OPEN      FACAL2LA
     C                   OPEN      FACAPLA
     C                   MOVE      '1'           OPNFLG
     C                   END
     C*
     C*     then, reset hold YMD field and provider/mod
     C                   Z-ADD     XXYMD         HDYMD
     C                   Z-ADD     CAFEIN        HDFEIN
     C                   Z-ADD     CAMOD         HDMOD
     C*
     C                   END                                                    END chg in YMD/FEIN
     C*
     C                   END                                                    END XFIRST EQ ' '
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
     C                   CLOSE     FACALLA
     C                   CLOSE     FACAL1LA
     C                   CLOSE     FACAL2LA
     C                   CLOSE     FACAPLA
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL2LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACLA'
     C                   PARM      ' '           QQTYPE
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL2LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACAPLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALLA
     C                   OPEN      FACAL1LA
     C                   OPEN      FACAL2LA
     C                   OPEN      FACAPLA
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
     C                   EXSR      ANES2
     C                   EXSR      OFFI2
     C                   EXSR      SURG2
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALLA
     C                   CLOSE     FACAL1LA
     C                   CLOSE     FACAL2LA
     C                   CLOSE     FACAPLA
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL2LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACLA'
     C                   PARM      ' '           QQTYPE
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL2LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACAPLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALLA
     C                   OPEN      FACAL1LA
     C                   OPEN      FACAL2LA
     C                   OPEN      FACAPLA
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
     C     DHPRCD        ANDLE     '96999'
     C     DHPRCD        ORGE      '97800'
     C     DHPRCD        ANDLE     '99199'
     C                   EXSR      MEDI
     C*
     C                   ELSE                                                      ELSE MEDI
     C*
     C     DHPRCD        IFGE      '97010'                                      PHYS. THERAPY
     C     DHPRCD        ANDLE     '97799'
     C                   EXSR      PHYS
     C*
     C                   ELSE                                                      ELSE PHYS
     C*
     C     DHPRCD        IFGE      '99201'                                      OFFICE VISITS
     C     DHPRCD        ANDLE     '99499'
     C*
     C* If first time into program,
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the program is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALLA
     C                   CLOSE     FACAL1LA
     C                   CLOSE     FACAL2LA
     C                   CLOSE     FACAPLA
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL2LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACLA'
     C                   PARM      ' '           QQTYPE
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL2LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACAPLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALLA
     C                   OPEN      FACAL1LA
     C                   OPEN      FACAL2LA
     C                   OPEN      FACAPLA
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
     C*     then, Process all Phys Medicine Codes for the existing date/provider
     C                   EXSR      ANES2
     C                   EXSR      OFFI2
     C                   EXSR      SURG2
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALLA
     C                   CLOSE     FACAL1LA
     C                   CLOSE     FACAL2LA
     C                   CLOSE     FACAPLA
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL2LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACLA'
     C                   PARM      ' '           QQTYPE
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACAL1LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*     FACAL2LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C*     FACAPLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALLA
     C                   OPEN      FACAL1LA
     C                   OPEN      FACAL2LA
     C                   OPEN      FACAPLA
     C                   MOVE      '1'           OPNFLG
     C                   END
     C*
     C*     then, reset hold YMD field and provider/mod
     C                   Z-ADD     XXYMD         HDYMD
     C                   Z-ADD     CAFEIN        HDFEIN
     C                   Z-ADD     CAMOD         HDMOD
     C*
     C                   END                                                    END chg in YMD/FEIN
     C*
     C                   END                                                    END XFIRST EQ ' '
     C                   EXSR      OFFI
     C*
     C                   ELSE                                                      ELSE OFFI
     C*
     C     DHPRCD        IFGE      'OT010'                                      OCCUPATIONL THERAPIS
     C     DHPRCD        ANDLE     'OT680'
     C                   EXSR      OCT
     C*
     C                   ELSE                                                      ELSE OCT
     C*
     C     DHPRCD        IFGE      'PT010'                                      PHYSICAL THERAPIST
     C     DHPRCD        ANDLE     'PT680'
     C                   EXSR      PHT
     C*
     C                   ELSE                                                      ELSE PHT
     C*
     C     DHPRCD        IFGE      'A0010'                                      MEDICAL TRANSPORTATI
     C     DHPRCD        ANDLE     'A0999'
     C                   EXSR      MDTR
     C*
     C                   ELSE                                                      ELSE MDTR
     C*
     C     DHPRCD        IFGE      'E0100'                                      DME/RESPIRATORY
     C     DHPRCD        ANDLE     'E1702'
     C                   EXSR      DME
     C*
     C                   ELSE                                                      ELSE DME
     C*
     C     DHPRCD        IFGE      'L0100'                                      PROSTHETIC/ORTHOPEDI
     C     DHPRCD        ANDLE     'L9999'
     C                   EXSR      PROR
     C*
     C                   ELSE                                                      ELSE PROR
     C*
     C     DHPRCD        IFGE      'V2020'                                      VISION CARE SERVICES
     C     DHPRCD        ANDLE     'V2799'
     C                   EXSR      VSCR
     C*
     C                   ELSE                                                      ELSE VSCR
     C*
     C     DHPRCD        IFGE      'V5000'                                      HEARING AID EQUIP
     C     DHPRCD        ANDLE     'V5364'
     C                   EXSR      HEAR
     C*
     C                   ELSE                                                      ELSE HEAR
     C*
     C     DHPRCD        IFGE      'W0050'                                      NURSING/ATTENDANT
     C     DHPRCD        ANDLE     'W0902'
     C                   EXSR      NURS
     C*
     C                   ELSE                                                      ELSE NURS
     C*
     C     DHPRCD        IFGE      'X0710'                                      VOCATIONAL REHAB
     C     DHPRCD        ANDLE     'X0750'
     C                   EXSR      VOC
     C*
     C                   ELSE                                                      ELSE VOC
     C*
     C     DHPRCD        IFGE      'A4190'                                      MED/SURG SUPPLIES
     C     DHPRCD        ANDLE     'A5149'
     C                   EXSR      VOC
     C*
     C                   ELSE                                                      ELSE MDSU
     C*
     C                   EXSR      OTHE                                         OTHER CODES
     C*
     C                   END                                                       END MDSU
     C                   END                                                       END VOC
     C                   END                                                       END NURS
     C                   END                                                       END HEAR
     C                   END                                                       END VSCR
     C                   END                                                       END PROR
     C                   END                                                       END DME
     C                   END                                                       END MDTR
     C                   END                                                       END PHT
     C                   END                                                       END OCT
     C                   END                                                       END OFFI
     C                   END                                                       END PHYS
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
     C                   MOVEL     KKDSEQ        QQS(Q)
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
     C                   Z-ADD     0             XPTOTA
     C*   If there is an override percentage, calculate
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CACHRG        MULT      XXOVRP        CATOTA
     C                   END
     C*
     C     CACHRG        SUB       CATOTA        CAMRA
     C                   Z-ADD     CATOTA        CAPAID
     C                   END
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     SXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA01
     C*
     C                   END
     C                   END                                                    END EQ OVRRD
     C                   ENDIF                                                  CAFDAT IFGE
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
     C*
     C                   ENDDO
     C*
     C*** Execute SURG2 if a SURG Code has been processed
     C     OPNFLG        IFEQ      '1'
     C                   EXSR      ANES2
     C                   EXSR      OFFI2
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
     C                   CLOSE     FACALLA
     C                   CLOSE     FACAL1LA
     C                   CLOSE     FACAL2LA
     C                   CLOSE     FACAPLA
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL1LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*     to FACAL2LA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPLA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*---------------------------------------------------------------------
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
     C*  If ANES = BR
     C*  If anesthesia amount from the FMPRP file is zero, then give
     C*  error message that this anesthesia is BR or not established
     C     SXANAM        IFLE      *ZEROS
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '57'          QQC(Q)
     C* Err Msg 57: "Anesthesia amount is either BR or not established
     C*             - Please review"
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      *BLANKS       XPEOMB
     C*
     C                   END                                                    END LE *ZERO
     C**********
     C*=====================================================================
     C*=== Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per Chpt. 51, pg. 24-25
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
     C                   END                                                    END NE *BLANK
     C*
     C*================================================================
     C* Write record to workfile for further processing
     C                   MOVE      'A'           CATYPE
     C     SXANAM        MULT(H)   XXANES        CAMRA
     C                   Z-ADD     CACHRG        CATOTC
     C*** BEGIN ADD *** 12/18/00 **************************************
     C                   Z-ADD     CAUNTS        CAANMN
     C*** END   ADD *** 12/18/00 **************************************
     C                   Z-ADD     0             XPTOTA
     C                   WRITE     PCAWK1
     C                   GOTO      ANESE
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
     C                   Z-ADD     0             TTANMN
     C* Read all ANES workfile records to accumulate the total time units
     C* for all the ANES codes.
     C*
     C     *LOVAL        SETLL     PCAWK4
     C                   READ      PCAWK4                                 60
     C     *IN60         DOWEQ     '0'
     C*
     C*=====================================================================
     C*=== Procedure Codes with modifiers:
     C     P4PMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per Chpt. 51, pg. 24-25
     C* If modifier - Px is used, add additional Time Units to calculation
     C     P4PMOD        IFEQ      'P1'
     C     P4PMOD        OREQ      'P2'
     C     P4PMOD        OREQ      'P3'
     C     P4PMOD        OREQ      'P4'
     C     P4PMOD        OREQ      'P5'
     C     P4PMOD        OREQ      'P6'
     C*
     C                   Z-ADD     1             X
     C     P4PMOD        LOOKUP    ANEPM1(X)                              60
     C     *IN60         IFEQ      '1'
     C                   ADD       ANEPM2(X)     TTANMN
     C                   END                                                    END 1 *IN60
     C*
     C                   ELSE                                                   ELSE EQ P1...
     C                   EXSR      PMOD
     C*
     C                   END                                                    END EQ 'P1'
     C*
     C                   END                                                    END NE *BLANK
     C*
     C*================================================================
     C*=== Calculating Time Units =====================================
     C*
     C* Per Chpt. 51, pg. 24:
     C* The Time Units for all procedures is computed by allowing 1 unit
     C* for each 15 minutes of anesthesia time.  In each instance, 5 minutes
     C* or greater is considered a significant portion of a time unit.
     C*
     C*
     C*  1.0 Unit for each 15 minutes or any fraction thereof under 3 hours
     C     P4ANMN        DIV       15            HLANMN
     C** Round up all fractions of 15 minutes to an additional Time Unit
     C                   MVR                     HLADDT
     C                   ADD       HLANMN        HLADDT
     C                   Z-ADD     HLANMN        HLANTV
     C     HLADDT        IFGE      .3                                           5 minutes
     C                   ADD       1             HLANTV
     C                   END
     C*
     C** Basic Value + Time Units = Total Anesthesia Value
     C                   ADD       HLANTV        TTANMN
     C*
     C                   READ      PCAWK4                                 60
     C                   END
     C*
     C* After getting the total time unit value we must re-read the
     C* file to process payment correctly for the highest code
     C* all other codes will be disallowed
     C*
     C     *LOVAL        SETLL     PCAWK4
     C                   READ      PCAWK4                                 60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C                   ADD       1             COUNT
     C*
     C     COUNT         IFGT      1
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C********** P3TOTC    SUB  XPTOTA    XPTOTD           CHG 11/01/96
     C     P4TOTC        SUB       XPTOTA        XPTOTD                         CHG 11/01/96
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C                   ELSE
     C*
     C                   MOVE      P4CPT4        XXCPT4
     C                   MOVE      P4FDAT        XXEFDT
     C     PRKY          SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     P4CO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C                   ADD       PRANAM        TTANMN
     C                   END                                                    END *IN54
     C*
     C     TTANMN        MULT(H)   XXANES        P4MRA
     C*
     C* ===============================================================
     C* Modifier -46 = Anesthesia by CRNA
     C* Per Chpt. 51, pg. 27, II, A, 3, e
     C* Pay 80% of medical reimbursement guideline total anesthesia value
     C     XPPMOD        IFEQ      '46'
     C     P4MRA         MULT      .80           P4MRA
     C                   END                                                    END XPPMOD
     C* ===============================================================
     C*
     C     P4TOTC        COMP      P4MRA                              54        >
     C   54              Z-ADD     P4MRA         XPTOTA
     C  N54              Z-ADD     P4TOTC        XPTOTA
     C                   Z-ADD     XPTOTA        XPTOTP
     C     P4TOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     P4MRA         XPBRTC                         ADD 09/15/94
     C                   END
     C                   EXSR      UPDAT3
     C*
     C                   READ      PCAWK4                                 60
     C                   END                                                    END DOWEQ 0 *IN60
     C*
     C     ANES2E        ENDSR
     *********************************************************************
     *** Durable Medical Equip      Codes: E0100 - E1702         D M E   *
     *** Respiratory                                                     *
     *********************************************************************
     C     DME           BEGSR
     C*
     C* ===============================================================
     C* Purchase Used Equipment will be identified by PMOD = PU
     C* Rental Equipment will be identified by PMOD = RT
     C     XPPMOD        IFNE      *BLANK
     C*
     C     XPPMOD        IFNE      'PU'
     C     XPPMOD        ANDNE     'RT'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C*** Err Msg 10: "Invalid Modifier used with this Item., Re-enter Item
     C***              with valid modifier"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '001'         XPEOMB
     C*** EOMB 001: "Service is not reimubrsable under the Worker's Comp Pgm"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      DMEE
     C*
     C                   ELSE                                                   ELSE NE 'PU/RT'
     C*
     C******************************
     C* Rental
     C******************************
     C     XPPMOD        IFEQ      'RT'
     C* ------------------------------------------------------------------
     C* Determine if Rental is By Report
     C     SX1A2         IFEQ      'B'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C* Err Msg 06: "The item is BY REPORT - requires further
     C*             inspection by an adjuster"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C*
     C                   EXSR      UPDAT
     C                   GOTO      DMEE
     C*
     C                   END                                                    END SX1A2 = B
     C*
     C* ------------------------------------------------------------------
     C* Determine if Rental is I.C.
     C     SX1A2         IFEQ      'I'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '83'          QQC(Q)
     C* Err Msg 83: "The item is I.C. - requires further
     C*             inspection by an adjuster"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C*
     C                   EXSR      UPDAT
     C                   GOTO      DMEE
     C*
     C                   END                                                    END SX1A2 = I
     C*
     C* ------------------------------------------------------------------
     C* Determine if this code is Purchase Only
     C     SX1A2         IFEQ      'P'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '84'          QQC(Q)
     C* Err Msg 84: "The item is considered for "PURCHASE" only.
     C*              Please verify usage."
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C*
     C                   EXSR      UPDAT
     C                   GOTO      DMEE
     C*
     C                   END                                                    END SX1A2 = P
     C*
     C* ------------------------------------------------------------------
     C* Determine if this Rental item has a Purchase Only code.
     C     SX1A4         IFEQ      'P'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '84'          QQC(Q)
     C* Err Msg 84: "The item is considered "PURCHASE" only.
     C*              Please verify usage."
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '214'         XPEOMB
     C*** EOMB 214: "Incorrect submission of charge.  Purchase only item
     C*              submitted as rental"
     C                   EXSR      UPDAT
     C                   GOTO      DMEE
     C*
     C                   END                                                    END SX1A4 = P
     C* ------------------------------------------------------------------
     C*
     C     DHUNTS        IFGT      0
     C     DHUNTS        MULT      SXANAM        SXMRA
     C                   ELSE
     C                   Z-ADD     SXANAM        SXMRA
     C                   END
     C*
     C                   ELSE                                                   ELSE EQ 'RT'
     C*
     C******************************
     C* Purchase Used (PMOD = PU)
     C******************************
     C* ------------------------------------------------------------------
     C* Determine if Rental is By Report
     C     SX1A2         IFEQ      'B'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C* Err Msg 06: "The item is BY REPORT - requires further
     C*             inspection by an adjuster"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C*
     C                   EXSR      UPDAT
     C                   GOTO      DMEE
     C*
     C                   END                                                    END SX1A2 = B
     C*
     C* ------------------------------------------------------------------
     C* Determine if Rental is I.C.
     C     SX1A2         IFEQ      'I'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '83'          QQC(Q)
     C* Err Msg 83: "The item is I.C. - requires further
     C*             inspection by an adjuster"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C*
     C                   EXSR      UPDAT
     C                   GOTO      DMEE
     C*
     C                   END                                                    END SX1A2 = I
     C*
     C* ------------------------------------------------------------------
     C* Determine if this Purchase item has a Rental Only code.
     C     SX1A2         IFEQ      'R'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '85'          QQC(Q)
     C* Err Msg 85: "The item is considered "RENTAL" only.
     C*              Please verify usage."
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '215'         XPEOMB
     C*** EOMB 215: "Incorrect submission of charge.  Rental only item submitted
     C*              as purchase"
     C                   EXSR      UPDAT
     C                   GOTO      DMEE
     C*
     C                   END                                                    END SX1A2 = R
     C* ------------------------------------------------------------------
     C*
     C                   Z-ADD     SX92N2        SXMRA
     C*
     C                   END                                                    END EQ 'RT'
     C*
     C                   END                                                    END NE 'PU/RT'
     C************************
     C*
     C                   ELSE                                                   ELSE PMOD NE BLKS
     C*
     C* ------------------------------------------------------------------
     C* Determine if Purchase is By Report
     C     SX1A4         IFEQ      'B'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C* Err Msg 06: "The item is BY REPORT - requires further
     C*             inspection by an adjuster"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C*
     C                   EXSR      UPDAT
     C                   GOTO      DMEE
     C*
     C                   END                                                    END SX1A4 = B
     C* ------------------------------------------------------------------
     C* Determine if Purchase is I.C.
     C     SX1A4         IFEQ      'I'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '83'          QQC(Q)
     C* Err Msg 84: "The item is I.C. - requires further
     C*             inspection by an adjuster"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C*
     C                   EXSR      UPDAT
     C                   GOTO      DMEE
     C*
     C                   END                                                    END SX1A4 = I
     C*
     C* ------------------------------------------------------------------
     C* Determine if this Purchase item has a Rental Only code.
     C     SX1A2         IFEQ      'R'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '85'          QQC(Q)
     C* Err Msg 85: "The item is considered "RENTAL" only.
     C*              Please verify usage."
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '215'         XPEOMB
     C*** EOMB 215: "Incorrect submission of charge.  Rental only item submitted
     C*              as purchase"
     C                   EXSR      UPDAT
     C                   GOTO      DMEE
     C*
     C                   END                                                    END SX1A2 = R
     C* ------------------------------------------------------------------
     C*
     C                   END                                                    END PMOD NE BLKS
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 09/15/94
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     DMEE          ENDSR
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
     C                   Z-ADD     1             N                 5 0
      *
     C     N             DOWLE     9999
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
     *********************************************************************
     *** Hearing Aid Equipment      Codes: V5000 - V5299         H E A R *
     *********************************************************************
     C     HEAR          BEGSR
     C*
     C* ===============================================================
     C* If Procedure Code Modifier attached, goto PMOD
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C                   END
     C*
     C* ===============================================================
     C* Per Chpt 33, Pg. 5
     C* Dispensing Hearing Aides
     C* If V5000-V5010 submitted by specailty code "HAD" disallow
     C     DHPRCD        IFGE      'V5000'
     C     DHPRCD        ANDLE     'V5010'
     C     SVSPCD        IFEQ      'HAD'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '203'         XPEOMB
     C*** EOMB 203: "Service is not reimburseable based on Provider's Specialty"
     C                   EXSR      UPDAT
     C                   GOTO      HEARE
     C                   END                                                    END  EQ 'HAD'
     C                   END                                                    END  EQ 'V5000'
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 09/15/94
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     HEARE         ENDSR
     *********************************************************************
     *** Medical/Surgical Supplies  Codes: A4190 - A5149         M D S U *
     *********************************************************************
     C     MDSU          BEGSR
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 09/15/94
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     MDSUE         ENDSR
     *********************************************************************
     *** Medical Transportation     Codes: A0010 - A0999         M D T R *
     *********************************************************************
     C     MDTR          BEGSR
     C*
     C* ===============================================================
     C* Chpt. 39, per pg. 4-5
     C* The following codes will use the "Anes/Mi/Hr Amt" MRA value:
     C*        Codes A0020 and A0221 are "Per Mile" codes
     C*        Code A0060 is a "Per 1/2 Hour" code
     C     DHPRCD        IFEQ      'A0020'
     C     DHPRCD        OREQ      'A0060'
     C     DHPRCD        OREQ      'A0221'
     C                   Z-ADD     DHUNTS        XXUNIT
     C*
     C*  Per pg. 9, Code A0060
     C*      Need to determine Time Value Unit based on # minutes entered
     C     DHPRCD        IFEQ      'A0060'
     C     DHUNTS        IFGE      30
     C     DHUNTS        ANDLE     60
     C                   Z-ADD     1             XXUNIT
     C                   ELSE                                                    ELSE GE30/LE60
     C     DHUNTS        IFGE      61
     C     DHUNTS        ANDLE     90
     C                   Z-ADD     2             XXUNIT
     C                   ELSE                                                    ELSE GT60/LE90
     C     DHUNTS        IFGE      91
     C     DHUNTS        ANDLE     120
     C                   Z-ADD     3             XXUNIT
     C                   ELSE                                                    ELSE GT91/LE120
     C     DHUNTS        IFGE      121
     C     DHUNTS        ANDLE     150
     C                   Z-ADD     4             XXUNIT
     C                   ELSE                                                    ELSE GT121/LE150
     C     DHUNTS        IFGE      151
     C     DHUNTS        ANDLE     180
     C                   Z-ADD     5             XXUNIT
     C                   ELSE                                                    ELSE GT151/LE180
     C     DHUNTS        IFGE      181
     C     DHUNTS        ANDLE     210
     C                   Z-ADD     6             XXUNIT
     C                   ELSE                                                    ELSE GE181/LE210
     C     DHUNTS        IFGE      211
     C     DHUNTS        ANDLE     240
     C                   Z-ADD     7             XXUNIT
     C                   ELSE                                                    ELSE GT211/LE240
     C     DHUNTS        IFGE      241
     C     DHUNTS        ANDLE     270
     C                   Z-ADD     8             XXUNIT
     C                   ELSE                                                    ELSE GT241/LE270
     C     DHUNTS        IFGE      271
     C     DHUNTS        ANDLE     300
     C                   Z-ADD     9             XXUNIT
     C                   ELSE                                                    ELSE GT271/LE300
     C     DHUNTS        IFGE      301
     C     DHUNTS        ANDLE     330
     C                   Z-ADD     10            XXUNIT
     C                   END                                                     END GE301/LE330
     C                   END                                                     END GE271/LE300
     C                   END                                                     END GE241/LE270
     C                   END                                                     END GE211/LE240
     C                   END                                                     END GE181/LE210
     C                   END                                                     END GE151/LE180
     C                   END                                                     END GE121/LE150
     C                   END                                                     END GE91/LE120
     C                   END                                                     END GE61/LE90
     C                   END                                                     END GE30/LE60
     C*
     C                   END                                                     END EQ A0060
     C*
     C     XXUNIT        MULT      SXANAM        SXMRA
     C*
     C                   END                                                     END EQ A0010
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 09/15/94
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C*
     C     MDTRE         ENDSR
     *********************************************************************
     *** Medical                    Codes: 90701 - 96999         M E D I *
     ***                                   97800 - 99199                 *
     *********************************************************************
     C     MEDI          BEGSR
     C*
     C* ===============================================================
     C                   MOVE      *BLANKS       H2PMOD
     C*
     C*-------------------------------------------------------------------
     C*-- Evaluation/Management -------
     C*** BEGIN DEL *** 12/20/96 ***************************************
     C*   REMOVE EDIT
     C*
     C*   Per Chpt 51, pg. 4-6, B
     C*   If 99201-99412 vs.92500-92599 submitted on the same day, then
     C*   pay the highest only.
     C*
     C********
     C********   DHPRCD    IFGE '92500'
     C********   DHPRCD    ANDLE'92599'
     C********
     C**SETLL*and read the file testing for an ov code 99221-99233
     C********             MOVE '99201'   XXCPT4
     C********   CAKY9     SETLLPCA09
     C********   AGAIN4    TAG
     C********
     C********   CAKY9A    READEPCA09                    54
     C**N54***   XRCPT4    IFLE '99412'
     C********
     C**Determine if ov code MRA is greater
     C********             MOVE XRCPT4    XXCPT4
     C********             MOVE XRFDAT    XXEFDT
     C********   PRKY      SETGTPPR01
     C********             READPPPR01                    54
     C********   *IN54     IFEQ '0'
     C********   CACO#     ANDEQPRCO#
     C********   XXCPT4    ANDEQPRCPT4
     C********   PRMRA     IFGT SXMRA
     C********             Z-ADD*ZERO     XPTOTA
     C********             Z-ADD*ZERO     XPTOTP
     C********   CACHRG    SUB  XPTOTA    XPTOTD
     C********             Z-ADDCACHRG    XPTOTC
     C********             MOVE '009'     XPEOMB
     C****EOMB 009: "Exceeds reimbursement limitations"
     C********             EXSR UPDAT
     C********             GOTO MEDIE
     C********             ELSE                            ELSE PRMRA
     C********             GOTO AGAIN4
     C********             END                             END GT PRMRA
     C********             END                             END *IN54
     C********             END                             END LE '99233'
     C********             END                             END GE '92500'
     C*
     C*** END DEL ***** 12/20/96 ***************************************
     C*===================================================================
     C*   99070 Supplies Code
     C*   Per Chpt 51, pg. 19, N
     C*   Per Chpt 51, pg. 23, IV, A, 2
     C*   Per Chpt 51, pg. 51, B, 3  and B, 6
     C*   Will pay at 100%, but give error message for possible additional
     C*   reimbursement
     C     DHPRCD        IFEQ      '99070'
     C                   Z-ADD     CACHRG        XPTOTP
     C                   Z-ADD     XPTOTP        XPTOTA
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BI'          QQC(Q)
     C*** Err Msg BI: "Depending on type of supply, payment for 99070
     C***              may be increased by 20%"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END EQ '99070'
     C*
     C* ===============================================================
     C*-- Emergency Room (After Hours) -------
     C*   Per Chpt 51/Per Pat Allen
     C*   Emergency room(after hours) are not paid with any other office
     C*   visit code
     C*
     C     DHPRCD        IFEQ      '99050'
     C     DHPRCD        OREQ      '99052'
     C     DHPRCD        OREQ      '99054'
     C     DHPRCD        OREQ      '99056'
     C     DHPRCD        OREQ      '99058'
     C*
     C* SETLL and read the file testing for an office visit.
     C                   MOVE      '99201'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C*
     C     CAKY9A        READE     PCA09                                  54
     C  N54XRCPT4        IFLE      '99412'
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '005'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AA'          QQC(Q)
     C*** Err Msg AA: "ER visit submitted with another visit charge.
     C***              Please verify"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END LE '99412'
     C*
     C                   END                                                    END EQ '99050'
     C*
     C* ===============================================================
     C*** Chpt. 31, pg. 1, B,
     C*** Reimbursement for specailty code "OPC"
     C*** If 92002-92065 submitted by "OPC" then disallow charge
     C*** If 92081-92325, 92330-92335 submitted by "OPC" pay but give
     C*** message
     C*
     C     DHPRCD        IFGE      '92002'
     C     DHPRCD        ANDLE     '92065'
     C     SVSPCD        IFEQ      'OPC'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '203'         XPEOMB
     C*** EOMB 203: "Service is not reimburseable based on Provider's Specialty"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END  EQ 'OPC'
     C                   END                                                    END  EQ '92002'
     C*
     C     DHPRCD        IFGE      '92081'
     C     DHPRCD        ANDLE     '92325'
     C     DHPRCD        ORGE      '92330'
     C     DHPRCD        ANDLE     '92335'
     C     SVSPCD        IFEQ      'OPC'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '93'          QQC(Q)
     C*** Err Msg 86: "Please verify this service was provide by an
     C***              OPHTHALNOLOGIST"
     C                   END                                                    END  EQ 'OPC'
     C                   END                                                    END  EQ '92081'
     C*
     C* ===============================================================
     C*** Chpt. 51, pg. 45, D
     C*** Reimbursement for biofeedback training is limited to 12 visits.
     C*
     C     DHPRCD        IFGE      '90900'
     C     DHPRCD        ANDLE     '90915'
     C                   Z-ADD     *ZERO         COUNT
     C                   Z-ADD     *ZERO         HLFDAT
     C                   Z-ADD     *ZERO         H2FDAT
     C*
     C     CAKYC2        CHAIN     PCA0A                              60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XACPT4        IFGE      '90900'
     C     XACPT4        ANDLE     '90915'
     C     XAFDAT        ANDNE     HLFDAT
     C     XATOTP        ANDGT     *ZEROS
     C                   ADD       1             COUNT
     C                   Z-ADD     XAFDAT        HLFDAT
     C* Need to allow 2nd+ charge on the 12th day
     C     COUNT         IFEQ      12
     C                   Z-ADD     XAFDAT        H2FDAT
     C                   END                                                    END COUNT EQ 12
     C                   END                                                    END NE HLFDAT
     C*
     C     CAKYC2        READE     PCA0A                                  60
     C                   END                                                    END DOWEQ
     C*
     C     COUNT         IFGT      12
     C     COUNT         OREQ      12
     C     CAFDAT        ANDNE     H2FDAT
     C*** DISSALLOW CHARGES
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END                                                    END > 12
     C*
     C                   END                                                    END GE 90900
     C* ===============================================================
     C* Per chpt. 51, pg. 48, A, 6
     C* Should the provider fail to add modifier -26 to a diagnostic procedure
     C* provided in a Hospital or Ambulatory Surgical Center, the hospital or
     C* ASC bills for the technical services.
     C*
     C     XPPMOD        IFEQ      *BLANKS
     C     DHPOS         IFEQ      '1'
     C     DHPOS         OREQ      '2'
     C     DHPOS         OREQ      'B'
     C*** BEGIN ADD *** 05/27/98 **************************************
     C     DHPRCD        IFGE      '90900'
     C     DHPRCD        ANDLE     '95999'
     C*** END ADD *** 05/27/98 ****************************************
     C                   MOVE      XPPMOD        H2PMOD
     C                   MOVE      '90'          XPPMOD
     C                   MOVE      '217'         XPEOMB
     C*** EOMB 217: "Based upon provider's facility, reimbursement is made for
     C***            the technical component"
     C*** BEGIN ADD *** 05/27/98 **************************************
     C                   ENDIF                                                  CPT4 >= 90900
     C*** END ADD *** 05/27/98 ****************************************
     C                   END                                                    END EQ 1/2/B
     C                   END                                                    END XPPMOD
     C* ===============================================================
     C*
     C     XPPMOD        CASGT     *BLANKS       PMOD
     C                   END
     C     H2PMOD        IFNE      *BLANKS
     C                   MOVE      H2PMOD        XPPMOD
     C                   END
     C*
     C* ===============================================================
     C* Per chpt. 51, pg. 48, B, 1
     C*** State has specified two conditions (a1 and a2) of using Extremity
     C*** Testing, Muscle Testing and Range of Motion Measurements
     C*** (Codes 95831-95852 and 97720-97752) with Visits/Consultations
     C* Therefore, we will just test for Visits/Consultations codes used
     C* the same day as these codes and give user error message.
     C*
     C     DHPRCD        IFGE      '95831'
     C     DHPRCD        ANDLE     '95852'
     C     DHPRCD        ORGE      '97720'
     C     DHPRCD        ANDLE     '97752'
     C*
     C                   MOVE      '99201'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         IFEQ      '0'
     C     XRCPT4        ANDLE     '99412'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '86'          QQC(Q)
     C*** Err Msg 86: "A Visit/Consultation charge exists for this day,
     C***              Please review."
     C                   END                                                    END 0 *IN60
     C                   END                                                    END GE 95831...
     C*
     C* ===============================================================
     C* Per chpt. 51, pg. 49, B, 1, d, 1
     C*** Reimbursement for extremity testing, muscle testing and range of
     C*** motion measurements may be made only one in a 30-day period for the
     C*** same body area.
     C*
     C     DHPRCD        IFGE      '95831'
     C     DHPRCD        ANDLE     '95852'
     C     DHPRCD        ORGE      '97720'
     C     DHPRCD        ANDLE     '97752'
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
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     CAKYC         CHAIN     PCA0C                              60
     C                   Z-ADD     0             PASTCT
     C*
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        ANDEQ     DHPRCD
     C     XCFDAT        IFGE      WKFDAT
     C     XCFDAT        ANDLE     CAFDAT
     C     XCTOTP        ANDGT     *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C                   ADD       1             PASTCT
     C                   END
     C     CAKYC2        READE     PCA0C                                  60
     C                   END
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
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     CAKYC         CHAIN     PCA0C                              60
     C                   Z-ADD     0             FUTRCT
     C*
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        ANDEQ     DHPRCD
     C     XCFDAT        IFLE      WKFDAT
     C     XCFDAT        ANDGE     CAFDAT
     C     XCTOTP        ANDGT     *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C                   ADD       1             FUTRCT
     C                   END
     C     CAKYC2        READE     PCA0C                                  60
     C                   END
     C*
     C     PASTCT        IFGE      1
     C     FUTRCT        ORGE      1
     C*** DISSALLOW CHARGES
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END
     C*
     C                   END                                                    END EQ 95831...
     C*
     C* ===============================================================
     C* Per chpt. 51, pg. 49, B, 1, d, 2
     C*** When two or more procedures from 95831-95852 are performed on the
     C*** same day, reimbursement may not exceed the MRA for Procedure Code
     C*** 95834, total evaluation of the body, including hands.
     C*
     C     DHPRCD        IFGE      '95831'
     C     DHPRCD        ANDLE     '95852'
     C     DHPRCD        ANDNE     '95834'
     C                   MOVE      '95834'       XXCPT4
     C     CAKY9         CHAIN     PCA09                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '17'          QQC(Q)
     C*** Err Msg 17: "The total of codes 95831-95852 can't exceed MRA
     C***              for code 95834"
     C*
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END                                                    END 0 *IN60
     C                   END                                                    END GE 95831...
     C*
     C* ----------------------------------------------------------------
     C*
     C     DHPRCD        IFGE      '95831'
     C     DHPRCD        ANDLE     '95852'
     C     DHPRCD        IFNE      '95834'
     C                   MOVE      DHPRCD        XXCPT4
     C                   MOVE      CAFDAT        XXEFDT
     C*
     C                   Z-ADD     SXMRA         XXMRA
     C*
     C*** IF MODIFIER EXISTS, EXECUTE THE MOD CHECK SUBROUTINE
     C*
     C     XPPMOD        CASGT     *BLANKS       PMOD
     C                   END
     C*
     C                   ADD       XXMRA         T95831
     C*
     C     T95831        IFGT      M95834
     C*
     C                   SUB       XXMRA         T95831
     C                   Z-ADD     *ZERO         XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds Reimbursement Limitations"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '17'          QQC(Q)
     C*** Err Msg 17: "The total of codes 95831-95852 can't exceed MRA
     C***              for code 95834"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END
     C                   END
     C                   END                                                    END GE 95831...
     C*
     C* ===============================================================
     C* Per Chpt. 51, pg. 52, B, 4
     C*** Thermographic tests must not be authorized unless the date of service
     C*** is at least 45 days after the date of accident...
     C*
     C     DHPRCD        IFEQ      '93760'
     C     DHPRCD        OREQ      '93762'
     C*
     C                   Z-ADD     QQACDT        WKFDAT
     C                   Z-ADD     45            SRFUT
     C                   MOVE      ' '           SRTERM
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    WKFDAT
     C                   PARM                    SRFUT
     C                   PARM                    SRTERM
     C*
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKFDAT
     C     CAFDAT        IFLE      WKFDAT
     C** DISALLOW
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '001'         XPEOMB
     C*** EOMB 001: "Service is not reimbursable under the Workers Comp Program"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '19'          QQC(Q)
     C*** Err Msg 19: "*WARNING* Need Letter of Medical Necessity to pay this"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END                                                    END FDAT LE WKFDAT
     C*
     C* ------------------------------------------------------------------
     C* Per Chpt. 51, pg. 53, E, 1
     C* If both major and limited on same day, always pay major
     C*
     C     DHPRCD        IFEQ      '93760'
     C     DHPRCD        OREQ      '93762'
     C*
     C* SETLL and read the file testing for a thermography procedure.
     C                   MOVE      '93760'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     AGAIN3        TAG
     C*
     C     CAKY9A        READE     PCA09                                  54
     C  N54XRCPT4        IFLE      '93762'
     C     XRPART        IFEQ      'T1'
     C     XRPART        OREQ      'T2'
     C     XRPART        OREQ      'T3'
     C     XRPART        OREQ      'T4'
     C     XRPART        OREQ      'T5'
     C     DHAREA        IFEQ      'T6'
     C     DHAREA        OREQ      'T7'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '27'          QQC(Q)
     C*** Err Msg 27: "If the necessary documentations exists,
     C***              override and pay at MRA"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        CACHRG
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END EQ 'T6'
     C                   ELSE                                                   ELSE 'T1'
     C                   GOTO      AGAIN3
     C                   END                                                    END EQ 'T1'
     C                   END                                                    END LE '93762'
     C*
     C                   END                                                    END EQ '93760'
     C*
     C* ------------------------------------------------------------------
     C* Per Chpt. 51, pg. 53, E, 1
     C* Reimbursement is limited to ONE body area, either major or limited
     C* unless an additional area(s) is medically necessary and documentation
     C* of medical necessity is submitted to the carrier
     C*
     C                   Z-ADD     0             COUNT
     C*
     C     CAKYC2        CHAIN     PCA0A                              54
     C     *IN54         DOWEQ     '0'
     C*
     C* If the 'XA' charge is the same service date, payment is made and the
     C* code is equal to 93760 or 93762, then add to count
     C     XAFDAT        IFEQ      CAFDAT
     C     XACPT4        ANDEQ     '93760'
     C     XATOTP        ANDGT     *ZEROS
     C     XAFDAT        OREQ      CAFDAT
     C     XACPT4        ANDEQ     '93762'
     C     XATOTP        ANDGT     *ZEROS
     C                   ADD       1             COUNT
     C                   END                                                    END EQ CAFDAT
     C*
     C     CAKYC2        READE     PCA0A                                  54
     C                   END                                                    END DOWEQ 54
     C*
     C*** Disallow 2nd and subsequent charge on same day
     C     COUNT         IFGE      1
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '27'          QQC(Q)
     C*** Err Msg 27: "If the necessary documentations exists,
     C***              override and pay at MRA"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       CATOTA        XPTOTD
     C                   Z-ADD     CACHRG        CACHRG
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END COUNT GE 1
     C*
     C                   END                                                    END EQ 93760/93762
     C*
     C* ===============================================================
     C*** Chpt. 51, pg. 53, D, 1:
     C* When performed to the entire head, use 93760
     C     DHPRCD        IFEQ      '93760'
     C     DHAREA        ANDNE     'T1'
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 006: "Procedure is not appropriate to diagnosis"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END EQ 93760/NE T1
     C*
     C* ===============================================================
     C*** Chpt. 51, pg. 53, D, 3:
     C* When performed to any body area other than head, use 93762.
     C     DHPRCD        IFEQ      '93762'
     C     DHAREA        ANDNE     'T2'
     C     DHAREA        ANDNE     'T3'
     C     DHAREA        ANDNE     'T4'
     C     DHAREA        ANDNE     'T5'
     C     DHAREA        ANDNE     'T6'
     C     DHAREA        ANDNE     'T7'
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '006'         XPEOMB
     C*** EOMB 006: "Procedure is not appropriate to diagnosis"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END EQ 93762/NE T2..
     C*
     C* ===============================================================
     C*** Chpt. 51, pg. 53/54, E, 3:
     C* Reimbursement for thermography to a limited body area must be at
     C* the provider's usual charge or 50% of MRA, whichever is less.
     C     DHPRCD        IFEQ      '93762'
     C     DHAREA        ANDEQ     'T6'
     C     DHPRCD        OREQ      '93762'
     C     DHAREA        ANDEQ     'T7'
     C     SXMRA         DIV       2             SXMRA
     C                   END                                                    END EQ 93762/T6/T7
     C*
     C* ===============================================================
     C* Per Chpt 33, Pg. 5
     C* Dispensing Hearing Aides
     C* If 92500-92599 submitted by specailty code "HAD" disallow
     C     DHPRCD        IFGE      '92500'
     C     DHPRCD        ANDLE     '92599'
     C     SVSPCD        IFEQ      'HAD'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '203'         XPEOMB
     C*** EOMB 203: "Service is not reimburseable based on Provider's Specialty"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   END                                                    END  EQ 'HAD'
     C                   END                                                    END  EQ '92500'
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 09/15/94
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
     C* ===============================================================
     C*
     C     NEWPRE        ENDSR
     *********************************************************************
     *** Nursing/Attendant Care     Codes: W0050 - W0325         N U R S *
     *********************************************************************
     C     NURS          BEGSR
     C*
     C* ===============================================================
     C* Chpt. 35, couldn't find any mention of modifiers
     C     XPPMOD        IFNE      *BLANKS
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '82'          QQC(Q)
     C*** Err Msg 82: "A modifier is NOT allowed this code.Please verify usage."
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '001'         XPEOMB
     C*** EOMB 001: "Service is not reimubrsable under the Worker's Comp Pgm"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NURSE
     C*
     C                   END                                                     END XPPMOD ne BLK
     C*
     C* ===============================================================
     C* Chpt. 35, pg. 5:
     C* The following codes will use the "Anes/Mi/Hr Amt" MRA value:
     C     DHPRCD        IFEQ      'W0100'
     C     DHPRCD        OREQ      'W0110'
     C     DHPRCD        OREQ      'W0120'
     C     DHPRCD        OREQ      'W0125'
     C     DHPRCD        OREQ      'W0200'
     C     DHPRCD        OREQ      'W0210'
     C     DHPRCD        OREQ      'W0220'
     C     DHPRCD        OREQ      'W0225'
     C     DHPRCD        OREQ      'W0325'
     C     DHUNTS        DIV(H)    60            XXUNTS
     C*
     C*-------------------------------------------------------------------
     C* Chpt. 35, pg. 2, E: Nonprofessional family members are reimbursable up
     C*                     to 8 hours in any 24 hour period
     C     DHPRCD        IFEQ      'W0325'
     C                   MOVE      DHPRCD        XXCPT4
     C                   Z-ADD     0             PHOURS
     C                   Z-ADD     0             LHOURS
     C*
     C     CAKYA         CHAIN     PCA0A                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     XAPMOD        IFEQ      XPPMOD
     C     CADSEQ        IFNE      XADSEQ
     C     XATOTP        ANDGT     *ZERO
     C     XAANMN        DIV(H)    60            X1ANMN
     C                   ADD       X1ANMN        PHOURS
     C                   END                                                    END NE/EQ/NE
     C                   END                                                    END EQ XPPMOD
     C*
     C     CAKYA         READE     PCA0A                                  60
     C                   END                                                    END DOWEQ 60
     C*
     C*** If we've already paid more than 8 hours, then disallow code
     C     PHOURS        IFGT      8
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       CATOTA        XPTOTD
     C                   Z-ADD     CACHRG        CACHRG
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C                   EXSR      UPDAT
     C                   GOTO      NURSE
     C                   ELSE                                                   ELSE PHOURS GT 8
     C     8             SUB       PHOURS        LHOURS
     C     DHUNTS        DIV(H)    60            XXUNT
     C     XXUNT         IFGT      LHOURS
     C                   Z-ADD     LHOURS        XXUNTS
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C                   ELSE
     C                   Z-ADD     XXUNT         XXUNTS
     C                   END
     C                   END                                                    END PHOURS GT 8
     C*
     C                   END                                                    END EQ W0325
     C*-------------------------------------------------------------------
     C*
     C*
     C* Determine "MRA" for Hourly Charges
     C     XXUNTS        MULT(H)   SXANAM        SXMRA
     C                   END                                                    END EQ W0100...
     C*
     C* ===============================================================
     C* Per Chpt. 35, Pgs. 5-7
     C* Because Codes W0401-W0902 are "Per Diem/Daily" charges,
     C* the user should have entered them with units indicating # of Days
     C* The system will divide this into individual charges, then certain
     C* Also, the user should have keyed in the "AWP" as the Total Charge
     C* on certain items.
     C*
     C* Certain items not only get the "MRA" amount per day, but also the
     C* "AWP" which will correspond to the Total Charge entered.
     C     DHPRCD        IFEQ      'W0401'
     C     DHPRCD        OREQ      'W0402'
     C     DHPRCD        OREQ      'W0403'
     C     DHPRCD        OREQ      'W0404'
     C     DHPRCD        OREQ      'W0405'
     C     DHPRCD        OREQ      'W0602'
     C     DHPRCD        OREQ      'W0802'
     C     DHPRCD        OREQ      'W0804'
     C     DHPRCD        OREQ      'W0806'
     C     DHPRCD        OREQ      'W0902'
     C*
     C     CABRTC        IFGT      *ZEROS
     C*
     C     CABRTC        ADD       SXMRA         HLTOTC
     C     HLTOTC        IFEQ      CACHRG
     C                   Z-ADD     CABRTC        CACHRG                         ADD 10/10/94
     C                   END
     C*
     C                   END
     C*
     C                   Z-ADD     CACHRG        XPBRTC                         ADD 10/10/94
     C                   ADD       CACHRG        SXMRA
     C                   Z-ADD     SXMRA         CACHRG
     C                   ELSE                                                   ADD 10/10/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 10/10/94
     C                   END                                                    END EQ W0401...
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C*
     C     NURSE         ENDSR
     *********************************************************************
     *** Occupational Therapy       Codes: OT010 - OT680         O C T   *
     *********************************************************************
     C     OCT           BEGSR
     C*
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C* Chpt. 51, pg. 32, C, 2, a:
     C* For specialty code "OCT" only pay one of the same code/same day
     C*
     C     SVSPCD        IFEQ      'OCT'
     C                   MOVE      DHPRCD        XXCPT4
     C                   MOVE      XPPMOD        XXPMOD
     C                   Z-ADD     0             COUNT
     C*
     C     DHKY2         CHAIN     PDH03                              54
     C     *IN54         DOWEQ     '0'
     C*
     C                   Z-ADD     D1DSEQ        KKDSEQ
     C*
     C     CAKYP         CHAIN     PCA0P                              60
     C*
     C     CADSEQ        IFNE      D1DSEQ
     C     PXTOTP        ANDGT     *ZERO
     C     PXFDAT        ANDEQ     XXYMD
     C     PXFEIN        ANDEQ     CAFEIN
     C     PXMOD         ANDEQ     CAMOD
     C     DHMOD1        ANDEQ     XXPMOD
     C                   ADD       1             COUNT
     C                   END                                                    END NE/EQ/NE
     C*
     C     DHKY2         READE     PDH03                                  54
     C                   END                                                    END DOWEQ 54
     C*
     C*** DISSALLOW CHARGES
     C     COUNT         IFGE      1
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        CACHRG
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C                   EXSR      UPDAT
     C                   GOTO      OCTE
     C                   END                                                    END COUNT GE 1
     C                   END                                                    END EQ 'OCT'
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C* Chpt. 51, pg. 32, C, 2, a:
     C* Only one initial assessment per injury may be reimbursed
     C* (Codes: OT410, OT420, OT430, OT445, OT460)
     C*
     C     DHPRCD        IFEQ      'OT410'
     C     DHPRCD        OREQ      'OT420'
     C     DHPRCD        OREQ      'OT430'
     C     DHPRCD        OREQ      'OT445'
     C     DHPRCD        OREQ      'OT460'
     C                   MOVE      'OT410'       XXCPT4
     C                   Z-ADD     0             COUNT
     C*
     C     CAKYC         CHAIN     PCA0C                              60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        ANDLE     'OT460'
     C     XCTOTP        IFGT      *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C                   ADD       1             COUNT
     C                   END                                                    END XCTOTP GT 0
     C     CAKYC         READE     PCA0C                                  60
     C                   END                                                    END 0 DOWEQ 60
     C*
     C     COUNT         IFGE      1
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C                   EXSR      UPDAT
     C                   GOTO      OCTE
     C                   END                                                    END GE 1
     C*
     C                   END                                                    END EQ OT410...
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C* Chpt. 51, pg. 32, C, 2, b:
     C* Reimbursement for reassessmnet shall be recommended only once ia a
     C* seven(7) day period.
     C* (Codes: OT505, OT510, OT515, PT525, OT540)
     C*
     C     DHPRCD        IFEQ      'OT505'
     C     DHPRCD        OREQ      'OT510'
     C     DHPRCD        OREQ      'OT515'
     C     DHPRCD        OREQ      'OT525'
     C     DHPRCD        OREQ      'OT540'
     C*
     C                   Z-ADD     CAFDAT        SRDTI
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
     C                   MOVEL     SRDTO         HLDDAT            7 0
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C                   MOVE      SRDATE        SRDTE
     C                   Z-ADD     7             SRPST             5 0
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR             1
     C                   MOVE      SRDTE         SRDATE
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HL1DAT            7 0
     C*
     C                   MOVE      'OT505'       XXCPT4
     C                   Z-ADD     0             COUNT
     C*
     C     CAKYC         SETLL     PCA0C
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        ANDLE     'OT540'
     C     XCTOTP        IFGT      *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C     XCFDAT        IFGE      HL1DAT
     C     XCFDAT        ANDLE     HLDDAT
     C                   ADD       1             COUNT
     C                   END                                                    END CAFDAT GE HLDDAT
     C                   END                                                    END XCTOTP GT 0
     C     CAKYC2        READE     PCA0C                                  60
     C                   END                                                    END 0 DOWEQ 60
     C*
     C     COUNT         IFGE      1
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C                   EXSR      UPDAT
     C                   GOTO      OCTE
     C                   END                                                    END GE 1
     C*
     C                   END                                                    END EQ OT505...
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C* Chpt. 51, pg. 33, D, 2, c:
     C* Since the Hubbard Tank or Therapeutic Pool is designed for full
     C* body immersion, unless full body immersion therapy is medically
     C* necessary and prescribed, Procedure code OT180 must not be
     C* reimbursed.
     C*
     C     DHPRCD        IFEQ      'OT180'
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '218'         XPEOMB
     C*** EOMB 218: "This item requires a prescription and proof of
     C***            Medical Necessity"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '27'          QQC(Q)
     C*** Err Msg 27: "If the necessary documentations exists,
     C***              override and pay at MRA"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OCTE
     C*
     C                   END                                                    END EQ 'OT180'
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C* Chpt. 51, pg. 33, D, 2, d:
     C* Prior written authorization must be obtained when billing for more than
     C* 8 modalities, procedures, or combination in one OCT session
     C* (Codes: OT010-OT099, OT120-OT199, OT210-OT275)
     C*
     C     DHPRCD        IFGE      'OT010'
     C     DHPRCD        ANDLE     'OT275'
     C                   MOVE      'OT010'       XXCPT4
     C                   Z-ADD     0             COUNT
     C*
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  61
     C     *IN61         DOWEQ     '0'
     C     XRCPT4        ANDLE     'OT275'
     C     XRTOTP        IFGT      *ZEROS
     C     CADSEQ        ANDNE     XRDSEQ
     C                   ADD       1             COUNT
     C                   END                                                    END XRTOTP GT 0
     C     CAKY9A        READE     PCA09                                  61
     C                   END                                                    END 0 *IN61
     C*
     C     COUNT         IFGE      8
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '87'          QQC(Q)
     C*** Err Msg 87: "Prior written authorization required for reimbursement"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OCTE
     C                   END                                                    END GT 8
     C*
     C                   END                                                    END EQ OT010...
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Chpt. 51, pg. 45, D
     C*** Reimbursement for biofeedback training is limited to 12 visits.
     C*
     C     DHPRCD        IFEQ      'OT260'
     C     DHPRCD        OREQ      'OT265'
     C                   Z-ADD     *ZERO         COUNT
     C                   Z-ADD     *ZERO         HLFDAT
     C                   Z-ADD     *ZERO         H2FDAT
     C*
     C     CAKYC2        CHAIN     PCA0A                              60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XACPT4        IFEQ      'OT260'
     C     XACPT4        OREQ      'OT265'
     C     XAFDAT        ANDNE     HLFDAT
     C     XATOTP        ANDGT     *ZEROS
     C                   ADD       1             COUNT
     C                   Z-ADD     XAFDAT        HLFDAT
     C* Need to allow 2nd+ charge on the 12th day
     C     COUNT         IFEQ      12
     C                   Z-ADD     XAFDAT        H2FDAT
     C                   END                                                    END COUNT EQ 12
     C                   END                                                    END NE HLFDAT
     C*
     C     CAKYC2        READE     PCA0A                                  60
     C                   END                                                    END DOWEQ
     C*
     C     COUNT         IFGT      12
     C     COUNT         OREQ      12
     C     CAFDAT        ANDNE     H2FDAT
     C*** DISSALLOW CHARGES
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OCTE
     C*
     C                   END                                                    END > 12
     C*
     C                   END                                                    END GE 90900
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 09/15/94
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     OCTE          ENDSR
     *********************************************************************
     *** Office Visits/Consults     Codes: 99201 - 99499         O F F I *
     *********************************************************************
     C     OFFI          BEGSR
     C*
     C* ===============================================================
     C*** Chpt. 33, pg. 46, D, b
     C*** A copy of a consultation report must be submitted with the bill in
     C*** order for reimbursement to be made.
     C*
     C     DHPRCD        IFGE      '99241'
     C     DHPRCD        ANDLE     '99275'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '237'         XPEOMB
     C*** EOMB 237: "Consultation report required for payment"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '47'          QQC(Q)
     C*** Err Msg 47: "Copy of consultation report required for
     C***              reimbursement"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END                                                    END EQ Consultation
     C*
     C*-------------------------------------------------------------------
     C*** BEGIN DEL *** 12/20/96 *****************************************
     C*-- Evaluation/Management -------
     C*   Per Chpt 51, pg. 46, B
     C*   If 99201-99412 vs.92500-92599 submitted on the same day, then
     C*   pay the highest only.
     C*
     C*********  DHPRCD    IFGE '99201'
     C*********  DHPRCD    ANDLE'99412'
     C*
     C* SETLL and read the file testing for an ov code 99221-99233
     C*
     C*
     C********             MOVE '92500'   XXCPT4
     C********   CAKY9     SETLLPCA09
     C********   AGAIN5    TAG
     C********
     C********   CAKY9A    READEPCA09                    54
     C**N54***   XRCPT4    IFLE '92599'
     C********
     C**Determine if ov code MRA is greater
     C********             MOVE XRCPT4    XXCPT4
     C********             MOVE XRFDAT    XXEFDT
     C********   PRKY      SETGTPPR01
     C********             READPPPR01                    54
     C********   *IN54     IFEQ '0'
     C********   CACO#     ANDEQPRCO#
     C********   XXCPT4    ANDEQPRCPT4
     C********   PRMRA     IFGT SXMRA
     C********             Z-ADD*ZERO     XPTOTA
     C********             Z-ADD*ZERO     XPTOTP
     C********   CACHRG    SUB  XPTOTA    XPTOTD
     C********             Z-ADDCACHRG    XPTOTC
     C********             Z-ADD*ZEROS    XPDIFF           ADD 09/15/94
     C********             Z-ADD*ZEROS    XPBRTC           ADD 09/15/94
     C********             MOVE '009'     XPEOMB
     C****EOMB 009: "Exceeds reimbursement limitations"
     C********             EXSR UPDAT
     C********             GOTO OFFIE
     C********             ELSE                            ELSE PRMRA
     C********             GOTO AGAIN5
     C********             END                             END GT PRMRA
     C********             END                             END *IN54
     C********             END                             END LE '92599'
     C********             END                             END GE '92500'
     C*** END DEL ***** 12/20/96 ****************************************
     C*
     C* ===============================================================
     C*  Chpt. 51, pg. 47, E
     C*  Reimbursement must not be made for 99238 in addition to a surgery
     C*  code(10000-69999) by the same physician on the same day
     C*
     C     DHPRCD        IFEQ      '99238'
     C*
     C* SETLL and read the file testing for an surgery procedure.
     C                   MOVE      '10000'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  54
     C  N54XRCPT4        IFLE      '69979'
     C*
     C*** DISSALLOW CHARGES
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        CACHRG
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '241'         XPEOMB
     C*** EOMB 009: "Charge submitted on same day as another hospital
     C***            visit"
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C                   END                                                    END EQ 69979
     C                   END                                                    END EQ 99238
     C*
     C* ===============================================================
     C*  Chpt. 51, pg. 47, E
     C*  Reimbursement must not be made for 99238 in addition to an office
     C*  code(99221-99223) by the same physician on the same day
     C*
     C     DHPRCD        IFEQ      '99238'
     C*
     C* SETLL and read the file testing for an surgery procedure.
     C                   MOVE      '99221'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  54
     C  N54XRCPT4        IFLE      '99233'
     C*
     C*** DISSALLOW CHARGES
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        CACHRG
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '241'         XPEOMB
     C*** EOMB 009: "Charge submitted on same day as another hospital
     C***            visit"
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C                   END                                                    END EQ 69979
     C                   END                                                    END EQ 99238
     C*
     C*-------------------------------------------------------------------
     C*-- Initial Evaluation/Consultation -------
     C*   Per Chpt 51,  pg. 16, I, A, 1
     C*   Initial evaluation/consultation only allowed if prior to the
     C*   day before surgery
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99205'
     C     DHPRCD        ORGE      '99221'
     C     DHPRCD        ANDLE     '99223'
     C     DHPRCD        ORGE      '99241'
     C     DHPRCD        ANDLE     '99255'
     C     DHPRCD        ORGE      '99271'
     C     DHPRCD        ANDLE     '99303'
     C     DHPRCD        ORGE      '99321'
     C     DHPRCD        ANDLE     '99323'
     C     DHPRCD        ORGE      '99341'
     C     DHPRCD        ANDLE     '99343'
     C     DHPRCD        ORGE      '99384'
     C     DHPRCD        ANDLE     '99387'
     C*
     C* SETLL and read the file testing for an surgery procedure.
     C                   MOVE      '10000'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     AGAIN1        TAG
     C*
     C     CAKY9A        READE     PCA09                                  54
     C  N54XRCPT4        IFLE      '69979'
     C*
     C                   Z-ADD     1             SRPST
     C                   Z-ADD     XRFDAT        SRDTI
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
     C     CAFDAT        IFGE      WKFDAT                                       END GE CAFDAT
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '240'         XPEOMB
     C*** EOMB 240: "Service falls within Pre-operative policy"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '27'          QQC(Q)
     C*** Err Msg 27: "If the necessary documentation exists, override
     C***              and pay at MRA"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C                   ELSE                                                   ELSE CAFDAT
     C                   GOTO      AGAIN1
     C                   END
     C*
     C                   END                                                    END LE '69979'
     C*
     C                   END                                                    END EQ '99201'
     C*
     C*-------------------------------------------------------------------
     C*-- Asterick Visit ---------------
     C*   Per Chpt 51,  pg. 18, I
     C*   When an asterick(*) procedure is carried out at the time of
     C*   an office visit don't pay any 99201-99499 code on same day
     C*
     C* Office Visit
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99499'
     C*
     C* SETLL and read the file testing for an '*' procedure.
     C                   MOVE      '10000'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     AGAIN         TAG
     C*
     C     CAKY9A        READE     PCA09                                  54
     C  N54XRCPT4        IFLE      '69979'
     C*
     C* Determine if surgery code is an asterick procedure (PRSUPP=Y)
     C                   MOVE      XRCPT4        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKY          SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C     PRSUPP        ANDEQ     'Y'
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        CACHRG
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '27'          QQC(Q)
     C*** Err Msg 27: "If the necessary documentation exists, override
     C***              and pay at MRA"
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C                   ELSE                                                   ELS EQ Y
     C                   GOTO      AGAIN
     C                   END                                                    END EQ Y
     C*
     C                   END                                                    END LE 69979
     C*
     C                   END                                                    END EQ 99201
     C*
     C*-------------------------------------------------------------------
     C*-- Therapeutic Procedures -
     C*   Per Chpt 51,  pg. 20, T
     C*   In follow-up cases for additional therapeutic injections and/or
     C*   aspirations, an office visit is only indicated if it is necessary
     C*   reevalute the patient.  In this case, a minimal visit may be
     C*   listed in addition to the injection. Documentation required.
     C     DHPRCD        IFGE      '99211'
     C     DHPRCD        ANDLE     '99215'
     C     DHPRCD        ORGE      '99231'
     C     DHPRCD        ANDLE     '99238'
     C     DHPRCD        ORGE      '99261'
     C     DHPRCD        ANDLE     '99263'
     C     DHPRCD        ORGE      '99311'
     C     DHPRCD        ANDLE     '99313'
     C     DHPRCD        ORGE      '99331'
     C     DHPRCD        ANDLE     '99333'
     C     DHPRCD        ORGE      '99351'
     C     DHPRCD        ANDLE     '99353'
     C     DHPRCD        ORGE      '99395'
     C     DHPRCD        ANDLE     '99397'
     C     DHPRCD        ORGE      '99401'
     C     DHPRCD        ANDLE     '99412'
     C*
     C                   MOVE      '20550'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         IFEQ      '0'
     C     XRCPT4        ANDLE     '20610'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BJ'          QQC(Q)
     C*** Err Msg BJ: "Verify submitted documentation indicates necessary
     C***              office visit"
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C                   END                                                    END 0 *IN60
     C*
     C                   MOVE      '64400'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C     *IN60         IFEQ      '0'
     C     XRCPT4        ANDLE     '64450'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BJ'          QQC(Q)
     C*** Err Msg BJ: "Verify submitted documentation indicates necessary
     C***              office visit"
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C                   END                                                    END 0 *IN60
     C*
     C                   END                                                    END GE '99211'
     C*
     C* Write record to workfile for further processing
     C                   MOVE      'O'           CATYPE
     C                   Z-ADD     SXMRA         CAMRA
     C                   Z-ADD     CACHRG        CATOTC
     C                   Z-ADD     0             XPTOTA
     C                   WRITE     PCAWK1
     C                   GOTO      OFFIE
     C* ===============================================================
     C*
     C     OFFIE         ENDSR
     C*
     C********************************************************************
     C** Process OFFI Codes 99201-99499             O F F I 2          2 *
     C********************************************************************
     C     OFFI2         BEGSR
     C*
     C*----------------------------------------------------------------
     C*--- Reading the OFFI Detail Charge Workfile --------------------
     C*
     C                   Z-ADD     0             COUNT
     C* Run all records through the following to determine whether it will
     C* be reimbursed at 100%, or disallowed
     C* Only pay one visit per physician per patient paer day at highest
     C* level of care provided.
     C*
     C     *LOVAL        SETLL     PCAWK3
     C                   READ      PCAWK3                                 60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C                   ADD       1             COUNT
     C*
     C     COUNT         IFGT      1
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     P3TOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C                   ELSE
     C     P3TOTC        COMP      P3MRA                              54        >
     C   54              Z-ADD     P3MRA         XPTOTA
     C  N54              Z-ADD     P3TOTC        XPTOTA
     C                   Z-ADD     XPTOTA        XPTOTP
     C     P3TOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     P3MRA         XPBRTC                         ADD 09/15/94
     C                   END
     C                   EXSR      UPDAT2
     C*
     C                   READ      PCAWK3                                 60
     C                   END                                                    END DOWEQ 0 *IN60
     C*
     C     OFFI2E        ENDSR
     C*
     *********************************************************************
     *** HANDLE ALL OTHER CODES   XXXXX-XXXXX                    O T H E *
     *********************************************************************
     C     OTHE          BEGSR
     C* ===============================================================
     C* Chpt. 47, pg. 2, C, 1
     C* Misc. Claimant: Mileage
     C     DHPRCD        IFEQ      'MILES'
     C     DHUNTS        MULT      SXANAM        SXMRA
     C                   END                                                     END EQ MILES
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 09/15/94
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     OTHEE         ENDSR
     *********************************************************************
     *** Pathology                  Codes: 70010 - 79999         P A T H *
     *********************************************************************
     C     PATH          BEGSR
     C*
     C* ===============================================================
     C                   MOVE      *BLANKS       H2PMOD
     C*
     C* ===============================================================
     C* Per chpt. 51, pg. 48, A, 6
     C* Should the provider fail to add modifier -26 to a diagnostic procedure
     C* provided in a Hospital or Ambulatory Surgical Center, the hospital or
     C* ASC bills for the technical services.
     C*
     C     XPPMOD        IFEQ      *BLANKS
     C     DHPOS         IFEQ      '1'
     C     DHPOS         OREQ      '2'
     C     DHPOS         OREQ      'B'
     C                   MOVE      XPPMOD        H2PMOD
     C                   MOVE      '90'          XPPMOD
     C                   MOVE      '217'         XPEOMB
     C*** EOMB 217: "Based upon provider's facility, reimbursement is made for
     C***            the technical component"
     C                   END                                                    END EQ 1/2/B
     C                   END                                                    END XPPMOD
     C* ===============================================================
     C*
     C     XPPMOD        CASGT     *BLANKS       PMOD
     C                   END
     C     H2PMOD        IFNE      *BLANKS
     C                   MOVE      H2PMOD        XPPMOD
     C                   END
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 09/15/94
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     PATHE         ENDSR
     *********************************************************************
     *** Physical Therapy           Codes: PT010 - PT680         P C T   *
     *********************************************************************
     C     PHT           BEGSR
     C*
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C* Chpt. 51, pg. 32, C, 2, a
     C* For specialty code "PHT" only pay one of the same code/same day
     C*
     C     SVSPCD        IFEQ      'PHT'
     C                   MOVE      DHPRCD        XXCPT4
     C                   MOVE      XPPMOD        XXPMOD
     C                   Z-ADD     0             COUNT
     C*
     C     DHKY2         CHAIN     PDH03                              54
     C     *IN54         DOWEQ     '0'
     C*
     C                   Z-ADD     D1DSEQ        KKDSEQ
     C*
     C     CAKYP         CHAIN     PCA0P                              60
     C*
     C     CADSEQ        IFNE      D1DSEQ
     C     PXTOTP        ANDGT     *ZERO
     C     PXFDAT        ANDEQ     XXYMD
     C     PXFEIN        ANDEQ     CAFEIN
     C     PXMOD         ANDEQ     CAMOD
     C     DHMOD1        ANDEQ     XXPMOD
     C                   ADD       1             COUNT
     C                   END                                                    END NE/EQ/NE
     C*
     C     DHKY2         READE     PDH03                                  54
     C                   END                                                    END DOWEQ 54
     C*
     C*** DISSALLOW CHARGES
     C     COUNT         IFGE      1
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        CACHRG
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C                   EXSR      UPDAT
     C                   GOTO      PHTE
     C                   END                                                    END COUNT GE 1
     C                   END                                                    END EQ 'OCT'
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C* Chpt. 51, pg. 32, C, 2, a:
     C* Only one initial assessment per injury may be reimbursed
     C* (Codes: PT410, PT420, PT430, PT445, PT460)
     C*
     C     DHPRCD        IFEQ      'PT410'
     C     DHPRCD        OREQ      'PT420'
     C     DHPRCD        OREQ      'PT430'
     C     DHPRCD        OREQ      'PT445'
     C     DHPRCD        OREQ      'PT460'
     C                   MOVE      'PT410'       XXCPT4
     C                   Z-ADD     0             COUNT
     C*
     C     CAKYC         CHAIN     PCA0C                              60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        ANDLE     'PT460'
     C     XCTOTP        IFGT      *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C                   ADD       1             COUNT
     C                   END                                                    END XCTOTP GT 0
     C     CAKYC         READE     PCA0C                                  60
     C                   END                                                    END 0 DOWEQ 60
     C*
     C     COUNT         IFGE      1
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C                   EXSR      UPDAT
     C                   GOTO      PHTE
     C                   END                                                    END GE 1
     C*
     C                   END                                                    END EQ PT410...
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C* Chpt. 51, pg. 32, C, 2, b:
     C* Reimbursement for reassessmnet shall be recommended only once ia a
     C* seven(7) day period.
     C* (Codes: PT505, PT510, PT515, PT525, PT540)
     C*
     C     DHPRCD        IFEQ      'PT505'
     C     DHPRCD        OREQ      'PT510'
     C     DHPRCD        OREQ      'PT515'
     C     DHPRCD        OREQ      'PT525'
     C     DHPRCD        OREQ      'PT540'
     C*
     C                   Z-ADD     CAFDAT        SRDTI
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
     C                   MOVEL     SRDTO         HLDDAT            7 0
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C                   MOVE      SRDATE        SRDTE
     C                   Z-ADD     7             SRPST             5 0
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR             1
     C                   MOVE      SRDTE         SRDATE
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HL1DAT            7 0
     C*
     C                   MOVE      'PT505'       XXCPT4
     C                   Z-ADD     0             COUNT
     C*
     C     CAKYC         SETLL     PCA0C
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        ANDLE     'PT540'
     C     XCTOTP        IFGT      *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C     XCFDAT        IFGE      HL1DAT
     C     XCFDAT        ANDLE     HLDDAT
     C                   ADD       1             COUNT
     C                   END                                                    END CAFDAT GE HLDDAT
     C                   END                                                    END XCTOTP GT 0
     C     CAKYC2        READE     PCA0C                                  60
     C                   END                                                    END 0 DOWEQ 60
     C*
     C     COUNT         IFGE      1
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C                   EXSR      UPDAT
     C                   GOTO      PHTE
     C                   END                                                    END GE 1
     C*
     C                   END                                                    END EQ PT505...
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*** Chpt. 51, pg. 33, D, 2, C:
     C*** Since the Hubbard Tank or Therapeutic Pool is designed for full
     C*** body immersion, unless full body immersion therapy is medically
     C*** necessary and prescribed, Procedure code PT180 must not be
     C*** reimbursed.
     C*
     C     DHPRCD        IFEQ      'PT180'
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '218'         XPEOMB
     C*** EOMB 218: "This item requires a prescription and proof of
     C***            Medical Necessity"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '27'          QQC(Q)
     C*** Err Msg 27: "If the necessary documentations exists,
     C***              override and pay at MRA"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHTE
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C* Chpt. 51, pg. 33, D, 2, d:
     C* Prior written authorization must be obtained when billing for more than
     C* 8 modalities, procedures, or combination in one OCT session
     C* (Codes: PT010-PT099, PT120-PT199, PT210-PT275)
     C*
     C     DHPRCD        IFGE      'PT010'
     C     DHPRCD        ANDLE     'PT275'
     C                   MOVE      'PT010'       XXCPT4
     C                   Z-ADD     0             COUNT
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  61
     C     *IN61         DOWEQ     '0'
     C     XRCPT4        ANDLE     'PT275'
     C     XRTOTP        IFGT      *ZEROS
     C     CADSEQ        ANDNE     XRDSEQ
     C                   ADD       1             COUNT
     C                   END                                                    END XRTOTP GT 0
     C     CAKY9A        READE     PCA09                                  61
     C                   END                                                    END 0 *IN61
     C*
     C     COUNT         IFGE      8
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '87'          QQC(Q)
     C*** Err Msg 87: "Prior written authorization required for reimbursement"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHTE
     C                   END                                                    END GT 8
     C*
     C                   END                                                    END EQ PT010...
     C*
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Chpt. 51, pg. 45, D
     C*** Reimbursement for biofeedback training is limited to 12 visits.
     C*
     C     DHPRCD        IFEQ      'PT260'
     C     DHPRCD        OREQ      'PT265'
     C                   Z-ADD     *ZERO         COUNT
     C                   Z-ADD     *ZERO         HLFDAT
     C                   Z-ADD     *ZERO         H2FDAT
     C*
     C     CAKYC2        CHAIN     PCA0A                              60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XACPT4        IFEQ      'PT260'
     C     XACPT4        OREQ      'PT265'
     C     XAFDAT        ANDNE     HLFDAT
     C     XATOTP        ANDGT     *ZEROS
     C                   ADD       1             COUNT
     C                   Z-ADD     XAFDAT        HLFDAT
     C* Need to allow 2nd+ charge on the 12th day
     C     COUNT         IFEQ      12
     C                   Z-ADD     XAFDAT        H2FDAT
     C                   END                                                    END COUNT EQ 12
     C                   END                                                    END NE HLFDAT
     C*
     C     CAKYC2        READE     PCA0A                                  60
     C                   END                                                    END DOWEQ
     C*
     C     COUNT         IFGT      12
     C     COUNT         OREQ      12
     C     CAFDAT        ANDNE     H2FDAT
     C*** DISSALLOW CHARGES
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHTE
     C*
     C                   END                                                    END > 12
     C*
     C                   END                                                    END GE 90900
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 09/15/94
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     PHTE          ENDSR
     *********************************************************************
     *** PHYSICAL MEDICINE SUBROUTINE CODES 97010 - 97799        P H Y S *
     *********************************************************************
     C     PHYS          BEGSR
     C*
     C* ===============================================================
     C* Per Chpt 51, Pg. 35, I, 1, g
     C* The only valid Work Hardening Providers are:  PHT, OCT, PSC, VOC
     C     DHPRCD        IFEQ      '97545'
     C     DHPRCD        OREQ      '97546'
     C     SVSPCD        IFNE      'PHT'
     C     SVSPCD        ANDNE     'OCT'
     C     SVSPCD        ANDNE     'PSC'
     C     SVSPCD        ANDNE     'VOC'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '203'         XPEOMB
     C*** EOMB 203: "Service is not reimburseable based on Provider's Specialty"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END  NE PHT...
     C                   END                                                    END  EQ 97545...
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 09/15/94
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     PHYSE         ENDSR
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
     C* Per Chpt. 51, pg. 28, B, 4
     C* The use of modifiers does not imply or guarantee that a provider will
     C* receive reimbursement as billed.  Reimbursement for modified services
     C* or procedures must be based on documentation of medical necessity and
     C* must be determined on a case by case basis.
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '19'          QQC(Q)
     C*** Err Msg 19: "*WARNING* Need Letter of Medical Necessity to pay
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '76'          QQC(Q)
     C*** Err Msg 76: "CPT modifier used with this charge.  Verify use
     C***              and payment."
     C*
     C*=====================================================================
     C* Per Chpt. 51, pg. 28, B, 3
     C* If modifier -22 is used, inform user
     C*
     C     XPPMOD        IFEQ      '22'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '08'          QQC(Q)
     C*** Err Msg 08: "Unusual services modifier.  Adjuster should verify
     C***              amount to be paid"
     C                   END                                                    END EQ '22'
     C*
     C* ===============================================================
     C* Modifier -26 = Professional Component
     C     XPPMOD        IFEQ      '26'
     C*
     C     SXPROF        IFEQ      *ZEROS
     C*
     C* IF NO AMOUNT EXIST FOR THE PROF COMPONENT THEN CALCULATE IT BASED ON
     C* 85% OF MRA AMOUNT
     C*
     C     SXMRA         MULT      .85           SXMRA
     C*
     C                   ELSE                                                   ELSE PROF = 0
     C*
     C                   Z-ADD     SXPROF        SXMRA
     C*
     C                   END                                                    END PROF = 0
     C*
     C                   END                                                    END EQ '26'
     C*
     C* ===============================================================
     C* ===============================================================
     C*** BEGIN ADD *** 12/09/96 **************************************
     C* Modifier -27 = Technical Component
     C* If Technical component does not exist it will pay 100% of SXMRA
     C* else it will pay the predefined amount held in PR92N1.
     C*
     C     XPPMOD        IFEQ      '27'
     C*
     C     PR92N1        IFEQ      *ZEROS
     C*
     C     SXMRA         MULT      1.0           SXMRA
     C*
     C                   ELSE
     C*
     C                   Z-ADD     PR92N1        SXMRA
     C*
     C                   END                                                    END TECH
     C                   END                                                    END EQ '27'
     C*** END ADD ***** 12/09/96 **************************************
     C* ===============================================================
     C* Per Chpt 51, pg. 20, Q. 1
     C* Modifier -80 Assistant Surgeon
     C     XPPMOD        IFEQ      '80'
     C     SXMRA         MULT      .20           SXMRA
     C                   END                                                    END XPPMOD
     C*
     C* ===============================================================
     C* Per Chpt 51, pg. 20, Q. 1
     C* Modifier -81 Minimal Assistant Surgeon
     C     XPPMOD        IFEQ      '81'
     C     SXMRA         MULT      .20           SXMRA
     C*** BEGIN ADD *** 04/11/96 ***********************************************
     C*** EOMB 243 REIEMBURSED PER PHYSICIAN ASSISTANT/NURSE PRACTIIONER
     C*** GUIDLINES.
     C     SXMRA         MULT      .65           SXMRA
     C                   MOVE      '243'         XPEOMB
     C*** END ADD *** 04/11/96 *************************************************
     C                   END                                                    END XPPMOD
     C*
     C* ===============================================================
     C* Modifier -90 = Technical Component
     C     XPPMOD        IFEQ      '90'
     C     SX92N1        IFEQ      *ZEROS
     C*
     C* IF NO AMOUNT EXIST FOR THE TECH COMPONENT THEN CALCULATE IT BASED ON
     C* 15% OF MRA AMOUNT
     C*
     C     SXMRA         MULT      .15           SXMRA
     C*
     C                   ELSE                                                   ELSE TECH = 0
     C*
     C                   Z-ADD     SX92N1        SXMRA
     C*
     C                   END                                                    END TECH = 0
     C*
     C                   END                                                    END EQ '90'
     C* ===============================================================
     C*
     C     PMODE         ENDSR
     *********************************************************************
     *** Prosthetic & Orthopedic    Codes: L0100 - L8501         P R O R *
     *********************************************************************
     C     PROR          BEGSR
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 09/15/94
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     PRORE         ENDSR
     *********************************************************************
     *** Radiology                  Codes: 80002 - 89399         R A D I *
     *********************************************************************
     C     RADI          BEGSR
     C*
     C* ===============================================================
     C                   MOVE      *BLANKS       H2PMOD
     C*** BEG DEL *** 06/24/99 ****************************************
     C********** DHUNTS    IFNE *ZEROS
     C**********
     C**Write*record to workfile for further processing
     C**********           MOVE 'A'       CATYPE
     C********** SXANAM    MULT XXANES    CAMRA     H
     C**********           Z-ADDCACHRG    CATOTC
     C**********           Z-ADD0         XPTOTA
     C**********           WRITEPCAWK1
     C**********           GOTO RADIE
     C**********           END
     C*** END DEL *** 06/24/99 ****************************************
     C*
     C* ===============================================================
     C* Per chpt. 51, pg. 48, A, 6
     C* Should the provider fail to add modifier -26 to a diagnostic procedure
     C* provided in a Hospital or Ambulatory Surgical Center, the hospital or
     C* ASC bills for the technical services.
     C*
     C     XPPMOD        IFEQ      *BLANKS
     C     DHPOS         IFEQ      '1'
     C     DHPOS         OREQ      '2'
     C     DHPOS         OREQ      'B'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AB'          QQC(Q)
     C*** Err Msg AB: "Based on POS: if HOSP or ASC submitting charge
     C***              use tech comp"
     C                   MOVE      XPPMOD        H2PMOD
     C                   MOVE      '90'          XPPMOD
     C                   MOVE      '217'         XPEOMB
     C*** EOMB 217: "Based upon provider's facility, reimbursement is made for
     C***            the technical component"
     C*
     C                   END                                                    END EQ 1/2/B
     C                   END                                                    END EQ *BLANKS
     C* ===============================================================
     C*
     C     XPPMOD        CASGT     *BLANKS       PMOD
     C                   END
     C     H2PMOD        IFNE      *BLANKS
     C                   MOVE      H2PMOD        XPPMOD
     C                   END
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 09/15/94
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     RADIE         ENDSR
     *********************************************************************
     *** Respiratory Services       Codes: E0400 - E1406         R E S P *
     *********************************************************************
     C     RESP          BEGSR
     C*
     C* ===============================================================
     C* Rental Equipment will be identified by PMOD = R
     C     XPPMOD        IFNE      *BLANK
     C*
     C     XPPMOD        IFNE      'RT'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C*** Err Msg 10: "Invalid Modifier used with this Item., Re-enter Item
     C***              with valid modifier"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '001'         XPEOMB
     C*** EOMB 001: "Service is not reimubrsable under the Worker's Comp Pgm"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RESPE
     C*
     C                   ELSE                                                   ELSE NE 'RT'
     C*
     C* ------------------------------------------------------------------
     C* Per Chpt 45
     C* Q0036 can have two different MRAs  DME: 326.00  RESP: 242.13
     C* give user message notifing them of this situation
     C*
     C     DHPRCD        IFEQ      'Q0036'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '92'          QQC(Q)
     C* Err Msg 92: "If DME: MRA = $326.00, if Respiratory: MRA = &242.13
     C                   END                                                    END EQ 'Q0036'
     C*
     C* ------------------------------------------------------------------
     C* Determine if Rental is By Report
     C     SX1A2         IFEQ      'B'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C* Err Msg 06: "The item is BY REPORT - requires further
     C*             inspection by an adjuster"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RESPE
     C*
     C                   END                                                    END SX1A2 = B
     C*
     C* ------------------------------------------------------------------
     C* Determine if Rental is I.C.
     C     SX1A2         IFEQ      'I'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '83'          QQC(Q)
     C* Err Msg 83: "The item is I.C. - requires further
     C*             inspection by an adjuster"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RESPE
     C*
     C                   END                                                    END SX1A2 = I
     C*
     C* ------------------------------------------------------------------
     C* Determine if this code is Purchase Only
     C     SX1A2         IFEQ      'P'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '84'          QQC(Q)
     C* Err Msg 84: "The item is considered for "PURCHASE" only.
     C*              Please verify usage."
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RESPE
     C*
     C                   END                                                    END SX1A2 = P
     C*
     C* ------------------------------------------------------------------
     C* Determine if this Rental item has a Purchase Only code.
     C     SX1A4         IFEQ      'P'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '84'          QQC(Q)
     C* Err Msg 84: "The item is considered "PURCHASE" only.
     C*              Please verify usage."
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '214'         XPEOMB
     C*** EOMB 214: "Incorrect submission of charge.  Purchase only item
     C*              submitted as purchase"
     C                   EXSR      UPDAT
     C                   GOTO      RESPE
     C*
     C                   END                                                    END SX1A4 = P
     C* ------------------------------------------------------------------
     C*
     C     DHUNTS        MULT      SXANAM        SXMRA
     C                   END                                                    END NE 'RT'
     C*
     C                   ELSE                                                   ELSE PMOD NE BLKS
     C*
     C* ------------------------------------------------------------------
     C* Determine if Purchase is By Report
     C     SX1A4         IFEQ      'B'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C* Err Msg 06: "The item is BY REPORT - requires further
     C*             inspection by an adjuster"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RESPE
     C*
     C                   END                                                    END SX1A4 = B
     C* ------------------------------------------------------------------
     C* Determine if Purchase is I.C.
     C     SX1A4         IFEQ      'I'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '83'          QQC(Q)
     C* Err Msg 84: "The item is I.C. - requires further
     C*             inspection by an adjuster"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RESPE
     C*
     C                   END                                                    END SX1A4 = I
     C*
     C* ------------------------------------------------------------------
     C* Determine if this Purchase item has a Rental Only code.
     C     SX1A2         IFEQ      'R'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '85'          QQC(Q)
     C* Err Msg 85: "The item is considered "RENTAL" only.
     C*              Please verify usage."
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '215'         XPEOMB
     C*** EOMB 215: "Incorrect submission of charge.  Rental only item submitted
     C*              as purchase"
     C                   EXSR      UPDAT
     C                   GOTO      RESPE
     C*
     C                   END                                                    END SX1A2 = R
     C* ------------------------------------------------------------------
     C*
     C                   END                                                    END PMOD NE BLKS
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 09/15/94
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     RESPE         ENDSR
     *********************************************************************
     *** Surgery                    Codes: 10040 - 69979         S U R G *
     *********************************************************************
     C     SURG          BEGSR
     C*
     C*** BEG DEL *** 06/24/99 ****************************************
     C********** DHUNTS    IFNE *ZEROS
     C**********
     C**Write*record to workfile for further processing
     C**********           MOVE 'A'       CATYPE
     C********** SXANAM    MULT XXANES    CAMRA     H
     C**********           Z-ADDCACHRG    CATOTC
     C**********           Z-ADD0         XPTOTA
     C**********           WRITEPCAWK1
     C**********           GOTO SURGE
     C**********           END
     C*** END DEL *** 06/24/99 ****************************************
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C*
     C* Anesthesia by a surgeon
     C* Per Chpt. 51, 21, U
     C     XPPMOD        IFEQ      '47'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AH'          QQC(Q)
     C* Err Msg AH: "Refer to corresponding anesthesia code for
     C*             base units."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END 47
     C*
     C                   EXSR      PMOD
     C                   END                                                    END XPPMOD NE *BLANK
     C*
     C* ===============================================================
     C* Per Chpt. 51, per pg. 20, Q, 2
     C     SVSPCD        IFEQ      'PA '
     C     SVSPCD        OREQ      'NP '
     C     SVSPCD        OREQ      'ST '
     C     SXMRA         MULT      .65           SXMRA
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BH'          QQC(Q)
     C* Err Msg BH: "Verify Prov/Mod: Payment should be made to employer,
     C*              not individual."
     C                   END                                                    END SPCD EQ PA...
     C*
     C* ===============================================================
     C* Per Chpt. 51, per pg. 33, E, 2
     C* Reimbursement for TNS is limited to four sessions per injury
     C*
     C     DHPRCD        IFEQ      '64550'
     C                   MOVE      '64550'       XXCPT4
     C                   Z-ADD     0             COUNT
     C*
     C* Read file for all other TNS charges for life of this case/same provider
     C* Count only paid charges
     C     CAKYC         CHAIN     PCA0C                              60
     C     *IN60         DOWEQ     '0'
     C     XCTOTP        IFGT      *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C                   ADD       1             COUNT
     C                   END                                                    END TOTP GT *ZERO
     C     CAKYC         READE     PCA0C                                  60
     C                   END                                                    END 0 DOWEQ *IN60
     C*
     C* If we have already paid 4 TNS charges, then disallow this charge
     C     COUNT         IFGE      4
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END                                                    END COUNT GE 4
     C*
     C                   END                                                    END CPT4 = 64550
     C*
     C* ===============================================================
     C* Per Chpt. 51, pg. 51, A, 3
     C*** Replacement casting and strapping codes (29000-29799) must not
     C*** be used for initial care.
     C*
     C     DHPRCD        IFGE      '29000'
     C     DHPRCD        ANDLE     '29799'
     C*
     C* SETLL and read the file testing for an initial visit.
     C                   MOVE      '99201'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XRCPT4        IFEQ      '99201'
     C     XRCPT4        OREQ      '99202'
     C     XRCPT4        OREQ      '99203'
     C     XRCPT4        OREQ      '99205'
     C     XRCPT4        OREQ      '99221'
     C     XRCPT4        OREQ      '99222'
     C     XRCPT4        OREQ      '99223'
     C     XRCPT4        OREQ      '99301'
     C     XRCPT4        OREQ      '99302'
     C     XRCPT4        OREQ      '99303'
     C     XRCPT4        OREQ      '99341'
     C     XRCPT4        OREQ      '99342'
     C     XRCPT4        OREQ      '99343'
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '005'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END 0 DOWEQ 60
     C                   END                                                    END EQ 29000...
     C*
     C* ===============================================================
     C* Per Chpt. 51, pg. 18, I
     C* Diagnostic procedures during the same session and entry site are
     C* incidental to the major procedure
     C*
     C     DHPRCD        IFEQ      '43200'
     C*
     C* SETLL and read the file testing for another Endospcopic procedure
     C                   MOVE      '43201'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XRCPT4        IFGE      '43201'
     C     XRCPT4        ANDLE     '43499'
     C     XRCPT4        ORGE      '43750'
     C     XRCPT4        ANDLE     '43831'
     C     XRCPT4        ORGE      '44360'
     C     XRCPT4        ANDLE     '44393'
     C     XRCPT4        ORGE      '45300'
     C     XRCPT4        ANDLE     '45385'
     C     XRCPT4        ORGE      '46600'
     C     XRCPT4        ANDLE     '46614'
     C     XRCPT4        ORGE      '47550'
     C     XRCPT4        ANDLE     '47556'
     C     XRCPT4        ORGE      '49300'
     C     XRCPT4        ANDLE     '49311'
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '005'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END 0 DOWEQ 60
     C                   END                                                    END EQ 43200...
     C*
     C* ===============================================================
     C*
     C* Per Chpt. 51, pg. 18, I
     C* Diagnostic procedures followed by another surgery code
     C* (non-Endoscopic), pay Diagnostic at 50%
     C*
     C     DHPRCD        IFEQ      '43200'
     C*
     C* SETLL and read the file testing for another Surgery procedure
     C                   MOVE      '10000'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XRCPT4        IFGE      '10000'
     C     XRCPT4        ANDLE     '69979'
     C*
     C     SXMRA         MULT      .50           SXMRA
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END 0 DOWEQ 60
     C                   END                                                    END EQ 43200...
     C*
     C* ===============================================================
     C* Write record to Workfile
     C* Code has passed all edits - Write to the FACAPLA Workfile
     C* Only load "current charges" into the FACAPLA Workfile
     C*
     C* Exceptions are found in SUR1/SUR2 table and cannot be cut
     C*
     C                   Z-ADD     1             T
     C     DHPRCD        LOOKUP    SUR1(T)                                60
     C     *IN60         CABEQ     '1'           NOCUT
     C     DHPRCD        LOOKUP    SUR2(T)                                60
     C     *IN60         CABEQ     '1'           NOCUT
     C*
     C     SXRPT         IFNE      'M'
     C     SXRPT         ANDNE     'X'
     C     SXRPT         ANDNE     'Z'
     C                   Z-ADD     SXMRA         CAMRA
     C                   Z-ADD     CACHRG        CATOTC
     C                   Z-ADD     0             XPTOTA
     C                   MOVE      'S'           CATYPE
     C                   WRITE     PCAWK1
     C                   END                                                    END CAEOB# EQ *ZERO
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
     C     R             IFLT      205
     C                   ADD       1             R
     C                   GOTO      SURLOK
     C                   ELSE
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '238'         XPEOMB
     C* EOMB Code 238: "A major code must be present to pay this item"
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
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '238'         XPEOMB
     C* EOMB Code 238: "A major code must be present to pay this item"
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
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 09/15/94
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     SURGE         ENDSR
     C********************************************************************
     C** Process SURG Codes for 100%, 75%, 50%                 S U R G 2 *
     C*  This subroutine is called at each date, provider level break    *
     C********************************************************************
     C     SURG2         BEGSR
     C*
     C*----------------------------------------------------------------
     C*--- Reading the SURG Detail Charge Workfile --------------------
     C*
     C* ===============================================================
     C* Need to determine the number of surgical procedures for day
     C*
     C                   Z-ADD     0             COUNT
     C                   Z-ADD     0             COUNTA
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
     C     XRCPT4        ANDLE     '69979'
     C     XRDSEQ        ANDNE     P2DSEQ
     C     XRTOTP        ANDGT     0
     C     XREOB#        ANDNE     QQEOB#
     C     XRPART        IFEQ      'O'
     C                   ADD       1             COUNT
     C                   ELSE
     C     XRPART        IFEQ      'A'
     C                   ADD       1             COUNTA
     C                   ELSE
     C     XRPART        IFEQ      'B1'
     C                   ADD       1             CNTB1
     C                   ELSE
     C     XRPART        IFEQ      'B2'
     C                   ADD       1             CNTB2
     C                   END                                                    END EQ 'B1'
     C                   END                                                    END EQ 'B2'
     C                   END                                                    END EQ 'A'
     C                   END                                                    END EQ 'O'
     C     CAKY9D        READE     PCA09                                  61
     C                   END                                                    END 0 *IN61
     C*
     C                   END                                                    END 0 *IN60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C* ===============================================================
     C* Per Chpt. 51, per pg. 21, U
     C* Pay specialty code "AS" at 20% of normal fee
     C     SVSPCD        IFEQ      'AS '
     C*** BEGIN CHG *** 11/30/94 ***********************************************
     C     P2PMOD        IFNE      '80'
     C     P2MRA         MULT(H)   .20           P2MRA
     C                   END
     C*** END CHG *** 11/30/94 *************************************************
     C                   END                                                    END EQ 'AS'
     C*
     C* ===============================================================
     C* Per pg. 21/22, A: Multiple Procedures (O/A/B1)
     C* One operative site: Body Part "O"
     C*  Highest value 100%
     C*  2nd            60%
     C*  3rd            40%
     C*  4th-5th        25%
     C*  after 5th      requires report
     C*
     C     P2PART        IFEQ      'O'
     C     COUNT         IFLE      0
     C                   Z-ADD     1             P2BRP                          1st: 100%
     C                   ADD       1             COUNT
     C                   ELSE                                                   ELSE EQ 0
     C     COUNT         IFEQ      1
     C                   Z-ADD     .60           P2BRP                          2nd: 60%
     C                   ADD       1             COUNT
     C                   ELSE                                                   ELSE EQ 1
     C     COUNT         IFEQ      2
     C                   Z-ADD     .40           P2BRP                          3rd: 40%
     C                   ADD       1             COUNT
     C                   ELSE                                                   ELSE EQ 1
     C     COUNT         IFGE      3
     C     COUNT         ANDLE     4
     C                   Z-ADD     .25           P2BRP                          4th/5th: 25%
     C                   ADD       1             COUNT
     C                   ELSE                                                   ELSE GE 3/LE 4
     C     COUNT         IFGT      4
     C                   Z-ADD     *ZERO         P2BRP                          0% requires report
     C                   ADD       1             COUNT
     C                   MOVEL     '009'         P2EOMB
     C* EOMB Code 009: "Exceeds reimbursement limitations"
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     P2DSEQ        QQS(Q)
     C                   MOVE      '55'          QQC(Q)
     C* Err Msg 55: "A copy of the justification report is required for reimbur"
     C                   END                                                    END  GT 4
     C                   END                                                    END  GE 3/LE 4
     C                   END                                                    END  EQ 2
     C                   END                                                    END  EQ 1
     C                   END                                                    END  LE 0
     C*
     C*
     C                   ELSE                                                   ELSE EQ 'O'
     C*
     C* ===============================================================
     C* Per pg. 21/22, A: Multiple Procedures (A)
     C* Separate Organ/Anatomical: Body Part "A"
     C*  Highest value 100%
     C*  2nd            60%
     C*  3rd            40%
     C*  4th-5th        25%
     C*  after 5th      requires report
     C*
     C     P2PART        IFEQ      'A'
     C     COUNTA        IFLE      0
     C                   Z-ADD     1             P2BRP                          1st: 100%
     C                   ADD       1             COUNTA
     C                   ELSE                                                   ELSE EQ 0
     C     COUNTA        IFEQ      1
     C                   Z-ADD     .60           P2BRP                          2nd: 60%
     C                   ADD       1             COUNTA
     C                   ELSE                                                   ELSE EQ 1
     C     COUNTA        IFEQ      2
     C                   Z-ADD     .40           P2BRP                          3rd: 40%
     C                   ADD       1             COUNTA
     C                   ELSE                                                   ELSE EQ 1
     C     COUNTA        IFGE      3
     C     COUNTA        ANDLE     4
     C                   Z-ADD     .25           P2BRP                          4th/5th: 25%
     C                   ADD       1             COUNTA
     C                   ELSE                                                   ELSE GE 3/LE 4
     C     COUNTA        IFGT      4
     C                   Z-ADD     *ZERO         P2BRP                          0% requires report
     C                   ADD       1             COUNTA
     C                   MOVEL     '009'         P2EOMB
     C* EOMB Code 009: "Exceeds reimbursement limitations"
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     P2DSEQ        QQS(Q)
     C                   MOVE      '55'          QQC(Q)
     C* Err Msg 55: "A copy of the justification report is required for reimbur"
     C                   END                                                    END  GT 4
     C                   END                                                    END  GE 3/LE 4
     C                   END                                                    END  EQ 2
     C                   END                                                    END  EQ 1
     C                   END                                                    END  LE 0
     C*
     C                   ELSE                                                   ELSE EQ 'A'
     C*
     C* ===============================================================
     C* Per pg. 21, B: Bilateral Procedures
     C* Bilateral Site/Primary: Body Part "B1"
     C*  Highest value 100%
     C*  2nd            60%
     C*  3rd            40%
     C*  4th-5th        25%
     C*  after 5th      requires report
     C*
     C     P2PART        IFEQ      'B1'
     C     CNTB1         IFLE      0
     C                   Z-ADD     1             P2BRP                          1st: 100%
     C                   ADD       1             CNTB1
     C                   ELSE                                                   ELSE EQ 0
     C     CNTB1         IFEQ      1
     C                   Z-ADD     .60           P2BRP                          2nd: 60%
     C                   ADD       1             CNTB1
     C                   ELSE                                                   ELSE EQ 1
     C     CNTB1         IFEQ      2
     C                   Z-ADD     .40           P2BRP                          3rd: 40%
     C                   ADD       1             CNTB1
     C                   ELSE                                                   ELSE EQ 1
     C     CNTB1         IFGE      3
     C     CNTB1         ANDLE     4
     C                   Z-ADD     .25           P2BRP                          4th/5th: 25%
     C                   ADD       1             CNTB1
     C                   ELSE                                                   ELSE GE 3/LE 4
     C     CNTB1         IFGT      4
     C                   Z-ADD     *ZERO         P2BRP                          0% requires report
     C                   ADD       1             CNTB1
     C                   MOVEL     '009'         P2EOMB
     C* EOMB Code 009: "Exceeds reimbursement limitations"
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     P2DSEQ        QQS(Q)
     C                   MOVE      '55'          QQC(Q)
     C* Err Msg 55: "A copy of the justification report is required for reimbur"
     C                   END                                                    END  GT 4
     C                   END                                                    END  GE 3/LE 4
     C                   END                                                    END  EQ 2
     C                   END                                                    END  EQ 1
     C                   END                                                    END  LE 0
     C*
     C                   ELSE                                                   ELSE EQ 'B1'
     C*
     C* Per pg. 21, B
     C* Bilateral Site/Remote: Body Part "B2"
     C*  Highest value  75%
     C*  2nd            60%
     C*  3rd            40%
     C*  4th-5th        25%
     C*  after 5th      requires report
     C*
     C*
     C     P2PART        IFEQ      'B2'
     C     CNTB2         IFLE      0
     C                   Z-ADD     .75           P2BRP                          1st: 75%
     C                   ADD       1             CNTB2
     C                   ELSE                                                   ELSE EQ 0
     C     CNTB2         IFEQ      1
     C                   Z-ADD     .60           P2BRP                          2nd: 60%
     C                   ADD       1             CNTB2
     C                   ELSE                                                   ELSE EQ 1
     C     CNTB2         IFEQ      2
     C                   Z-ADD     .40           P2BRP                          3rd: 40%
     C                   ADD       1             CNTB2
     C                   ELSE                                                   ELSE EQ 1
     C     CNTB2         IFGE      3
     C     CNTB2         ANDLE     4
     C                   Z-ADD     .25           P2BRP                          4th/5th: 25%
     C                   ADD       1             CNTB2
     C                   ELSE                                                   ELSE GE 3/LE 4
     C     CNTB2         IFGT      4
     C                   Z-ADD     *ZERO         P2BRP                          0% requires report
     C                   ADD       1             CNTB2
     C                   MOVEL     '009'         P2EOMB
     C* EOMB Code 009: "Exceeds reimbursement limitations"
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     P2DSEQ        QQS(Q)
     C                   MOVE      '55'          QQC(Q)
     C* Err Msg 55: "A copy of the justification report is required for reimbur"
     C                   END                                                    END  GT 4
     C                   END                                                    END  GE 3/LE 4
     C                   END                                                    END  EQ 2
     C                   END                                                    END  EQ 1
     C                   END                                                    END  LE 0
     C*
     C                   END                                                    END EQ 'B2'
     C                   END                                                    END EQ 'B1'
     C                   END                                                    END EQ 'A'
     C                   END                                                    END EQ 'O'
     C*
     C     P2MRA         MULT      P2BRP         P2MRA
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
     C                   Z-ADD     P2DIFF        XRDIFF                         ADD 09/15/94
     C                   Z-ADD     P2BRTC        XRBRTC                         ADD 09/15/94
     C                   MOVEL     P2PMOD        XRPMOD
     C     P2TOTD        IFGT      *ZERO
     C     P2EOMB        ANDEQ     *BLANKS
     C                   MOVE      '002'         P2EOMB
     C* EOMB Code 002: "Charges exceed maximum allowance"
     C                   END                                                    END P2TOTD GT *ZERO
      *
     C                   MOVE      P2EOMB        XPEOMB
      *
     C     XPEOMB        IFNE      *BLANKS
      *
     C                   Z-ADD     P2DSEQ        KKDSEQ
     C                   EXSR      EOMB
      *
     C                   ENDIF
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
     C                   MOVE      '002'         XPEOMB
     C*** EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   END
     C*
     C     XPEOMB        IFNE      *BLANKS
     C                   Z-ADD     SVDSEQ        KKDSEQ
     C                   EXSR      EOMB
      *
     C                   ENDIF
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
     C                   MOVEL     KKDSEQ        QQS(Q)
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
     C                   Z-ADD     XPDIFF        CADIFF                         ADD 09/15/94
     C                   Z-ADD     XPBRTC        CABRTC                         ADD 09/15/94
     C*** BEGIN DEL *** 11/01/96 **************************************
     C***********          MOVE XPPMOD    XPPMOD
     C*** END DEL ***** 11/01/96 **************************************
     C                   MOVE      XPEOMB        CAEOMB
     C*** BEGIN DEL *** 03/15/95 ***********************************************
     C*
     C* IF CODE ENTERED IS ONE OF THE FOLLOWING CODES, THE AWP WAS ENTERED
     C* IN THE TOTAL CHARGES FIELD.  THE CODE SHOULD PAY MRA PLUS AWP.  WE MUST
     C* UPDATE THE CHARGE AMOUNT SO PAYMENT AMOUNT IS NOT GREATER THAN CHARGE
     C* AMOUNT
     C*
     C********** DHPRCD    IFEQ 'W0401'
     C********** DHPRCD    OREQ 'W0402'
     C********** DHPRCD    OREQ 'W0403'
     C********** DHPRCD    OREQ 'W0404'
     C********** DHPRCD    OREQ 'W0405'
     C********** DHPRCD    OREQ 'W0602'
     C********** DHPRCD    OREQ 'W0802'
     C********** DHPRCD    OREQ 'W0804'
     C********** DHPRCD    OREQ 'W0806'
     C********** DHPRCD    OREQ 'W0902'
     C**********           Z-ADDXPTOTA    CACHRG
     C**********           END                             END EQ W0401...
     C*** END DEL *** 03/15/95 *************************************************
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
     C                   Z-ADD     SXMRA         CAMRAA
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
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      *BLANKS       XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C* ===============================================================
     C*
     C     UPDAE         ENDSR
     C*
     C********************************************************************
     C** UPDATE SUBROUTINE FOR OFFI  FILE FACAPLA             U P D A T2 *
     C********************************************************************
     C     UPDAT2        BEGSR
     C*
     C* ---------------------------------------------------------------
     C*
     C     XPTOTD        IFGT      *ZERO
     C     XPEOMB        ANDEQ     *BLANK
     C                   MOVE      '002'         XPEOMB
     C* EOMB Code 002: "Charges exceed maximum allowance found in
     C*                 in reimbursement manual"
     C                   END
      *
     C     XPEOMB        IFNE      *BLANKS
      *
     C                   Z-ADD     P3DSEQ        KKDSEQ
     C                   EXSR      EOMB
      *
     C                   ENDIF
     C* ---------------------------------------------------------------
     C*
     C*  Test for a possible duplicate claim.
     C*
     C                   MOVE      P3CPT4        XXCPT4
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
     C     PXFDAT        ANDEQ     P3FDAT
     C     PXFEIN        ANDEQ     P3FEIN
     C     PXFLAG        ANDNE     'P'
     C     PXDSEQ        ANDNE     P3DSEQ
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     P3DSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C* Err Msg 03: Possible Duplicate claim exists for this item.
     C*
     C                   ENDIF
     C*
     C     DHKY2         READE     PDH03                                  54
     C*
     C                   ENDDO
     C* ---------------------------------------------------------------
     C*  Using FACAPLA record, chain to FDCAP record to update
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
     C                   Z-ADD     XPDIFF        XRDIFF                         ADD 09/15/94
     C                   Z-ADD     XPBRTC        XRBRTC                         ADD 09/15/94
     C**********           MOVELXPPMOD    XRPMOD           DEL 11/01/96
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
     C                   Z-ADD     SXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C  N54              UPDATE    PCA08
     C*
     C* ---------------------------------------------------------------
     C*
     C*   If fee schedule NON-REIMBURSEABLE ... display error message
     C*   XX25EM flag is set at beginning of program where PRFSR EQ 'N'
     C*
     C     XX25EM        IFEQ      '1'
     C                   Z-ADD     P3FEIN        XXFEIN
     C                   Z-ADD     P3MOD         XXMOD
     C                   MOVE      P3CPT4        XXCPT4
     C                   MOVEL     P3PMOD        XXPMOD
     C                   Z-ADD     XXYMD         SVYMD
     C                   Z-ADD     HDYMD         XXYMD
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
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVEL     *BLANK        XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE2        ENDSR
     C********************************************************************
     C** UPDATE SUBROUTINE FOR ANES  FILE FACAPLA             U P D A T3 *
     C********************************************************************
     C     UPDAT3        BEGSR
     C*
     C* ---------------------------------------------------------------
     C*
     C     XPTOTD        IFGT      *ZERO
     C     XPEOMB        ANDEQ     *BLANK
     C                   MOVE      '002'         XPEOMB
     C* EOMB Code 002: "Charges exceed maximum allowance found in
     C*                 in reimbursement manual"
     C                   END
     C*
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
     C                   MOVE      P4CPT4        XXCPT4
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
     C     PXFDAT        ANDEQ     P4FDAT
     C     PXFEIN        ANDEQ     P4FEIN
     C     PXFLAG        ANDNE     'P'
     C     PXDSEQ        ANDNE     P4DSEQ
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     P4DSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C* Err Msg 03: Possible Duplicate claim exists for this item.
     C*
     C                   ENDIF
     C*
     C     DHKY2         READE     PDH03                                  54
     C*
     C                   ENDDO
     C* ---------------------------------------------------------------
     C*  Using FACAPLA record, chain to FDCAP record to update
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
     C                   Z-ADD     XPDIFF        XRDIFF                         ADD 09/15/94
     C                   Z-ADD     XPBRTC        XRBRTC                         ADD 09/15/94
     C**********           MOVELXPPMOD    XRPMOD           DEL 11/01/96
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
     C                   Z-ADD     SXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C  N54              UPDATE    PCA08
     C*
     C* ---------------------------------------------------------------
     C*
     C*   If fee schedule NON-REIMBURSEABLE ... display error message
     C*   XX25EM flag is set at beginning of program where PRFSR EQ 'N'
     C*
     C     XX25EM        IFEQ      '1'
     C                   Z-ADD     P4FEIN        XXFEIN
     C                   Z-ADD     P4MOD         XXMOD
     C                   MOVE      P4CPT4        XXCPT4
     C                   MOVEL     P4PMOD        XXPMOD
     C                   Z-ADD     XXYMD         SVYMD
     C                   Z-ADD     HDYMD         XXYMD
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
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVEL     *BLANK        XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE3        ENDSR
     C*
     *********************************************************************
     *** Vocational Rehab Services  Codes: X0710 - X0750         V O C   *
     *********************************************************************
     C     VOC           BEGSR
     C*
     C* Chpt. 49, couldn't find any mention of modifiers
     C     XPPMOD        IFNE      *BLANKS
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '82'          QQC(Q)
     C*** Err Msg 82: "A modifier is NOT allowed this code.Please verify usage."
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/15/94
     C                   MOVE      '001'         XPEOMB
     C*** EOMB 001: "Service is not reimubrsable under the Worker's Comp Pgm"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      VOCE
     C*
     C                   END                                                     END XPPMOD ne BLK
     C*
     C* ===============================================================
     C* Chpt. 49, pg. 5:
     C* The following codes will use the "Anes/Mi/Hr Amt" MRA value:
     C     DHPRCD        IFEQ      'X0710'                                      Per Hour
     C     DHPRCD        OREQ      'X0720'                                      Per Hour
     C* We must now convert minutes to hours for these codes
     C     DHUNTS        DIV(H)    60            XVUNTS                         ADD 06/23/93
     C     XVUNTS        MULT(H)   SXANAM        SXMRA
     C                   END                                                    END EQ X0710
     C*
     C     DHPRCD        IFEQ      'X0730'                                      Per Mile
     C                   Z-ADD     DHUNTS        XXUNTS
     C* Determine "MRA" for Hourly/Mileage Charges
     C     XXUNTS        MULT(H)   SXANAM        SXMRA
     C                   END                                                    END EQ X0730...
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 09/15/94
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C*
     C* ===============================================================
     C     VOCE          ENDSR
     *********************************************************************
     *** Vision Care                Codes: V2020 - V2780         V S C R *
     *********************************************************************
     C     VSCR          BEGSR
     C*
     C* ===============================================================
     C* Per Chpt. 31, pg. 29-30
     C* If Procedure Code Modifier attached, goto PMOD
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C                   END
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF                         ADD 09/15/94
     C                   Z-ADD     SXMRA         XPBRTC                         ADD 09/15/94
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     VSCRE         ENDSR
     *********************************************************************
     OPCA01     E            ECA01
     OPCA08     E            ECA08
     *********************************************************************
**  ANEPM1/ANEPM2 - Anesthesia Px Physical Status Modifiers/Addtl Unit Values
P10
P20
P31
P42
P53
P60
**  QCA QCMDEXC TABLE
DLTOVR FILE(FACALLA)
OVRDBF FILE(FACALLA)  TOFILE(QTEMP/FACALLA)
DLTOVR FILE(FACAL1LA)
OVRDBF FILE(FACAL1LA) TOFILE(QTEMP/FACAL1LA)
DLTOVR FILE(FACAL2LA)
OVRDBF FILE(FACAL2LA) TOFILE(QTEMP/FACAL2LA)
DLTOVR FILE(FACAPLA)
OVRDBF FILE(FACAPLA)  TOFILE(QTEMP/FACAPLA)
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
1730517304
1730617304
1730717304
1731017304
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
2282022548
2282022554
2282022556
2282022558
2282022590
2282022595
2282022600
2282022610
2282022612
2282022625
2282022630
2282022650
2686126860
2686326862
2735827355
2735827356
2735827357
2769227691
3353033510
3353033511
3353033512
3353033513
3353033514
3353033516
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
4495544950
4700147000
4861148600
4861148605
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
6305363030
6304863040
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
