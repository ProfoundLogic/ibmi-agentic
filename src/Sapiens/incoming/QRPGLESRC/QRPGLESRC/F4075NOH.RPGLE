     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4074ROH
     H*    DESCRIPTION: ADJUDICATION (OHIO: 03/01/97)
     H*   This program is called from F0070R (Claim Adjudication) and will
     H* perform the actual adjudication of the detail claim records writ-
     H* ten by F0070R.
     H*
     H* This Sub-program is intended to be used to calculate the amount
     H* that is due a Provider of services, by the Carrier of the Work comp
     H* insurance.  It should be used for any service provided on or after
     H* 03/01/98.
     H*****************************************************************
     H*  CREATED:  C:79  04/28/98  Project: OH010                     *
     H*****************************************************************
     H* Information needed for each year's adjudication program rules:
     H*    1.  Update the effective date range for reading the detail charges
     H*        (found in the mainline section)
     H*    2.  Update the XXANES conversion factor
     H*        (found in COMPILE TIME ARRAY BY GEO ZIP AREA)
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  08/09/02 C:062 PROJECT: FS244
     H*                 USE CAZIPA.
     H*
     H*  09/25/01 C:062 PROJECT: FSMNT
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.
     H*                                                               *
     H*  06/15/99 C:79 PROJ: OH011
     H*                Modified to work with Provider Bills
     H*                                                               *
     H*  04/08/99 C:117 Support: MC095 Seq# 17568
     H*                 Added QQCO#, QQSUB#, QQFND to the *ENTRY PARMs
     H*                 and changed all occurences of UPCO#, UPSUB#,
     H*                 UPFND to the above mentioned fields. I had
     H*                 to change the QQCO#, QQSUB#, & QQFND
     H*                 variables that were all ready in the pgm to
     H*                 Q1CO#, Q1SUB#, & Q1FND.  The API call to
     H*                 F4200ROH the only place where the variables
     H*                 were all ready used.
     H*
     H*  03/26/99 C:117 Project: MC060 #16778
     H*                 CPT code 99423 paying nothing when it should
     H*                 pay the MRA.
     H*
     H*  09/04/98 C:31  Project: 95MAL
     H*                 Ntwk Disc:Lesser of FS/Chr
     H*                 1. Add update of CAMRAA
     H*
     H*  07/07/98 C:56  Project MC060 #12209
     H*                 1. Populate Total Paid, Total ALlowed and Total
     H*                    Disallowed for 482 code.
     H*****************************************************************
     H*  INDICATOR USAGE:                                            **
     H*****************************************************************
     H*  SECTION                                        SUBR         CODES
     H*  Evaluation & Management                        EVAL    99201 - 99499
     H*  Anesthesia                                     ANES    00100 - 01999
     H*                                                         02000 - 02020
     H*  Surgery                                        SURG    10000 - 69999
     H*                                                 SURG2   10000 - 69999
     H*  Radiology                                      RADI    70010 - 79999
     H*                                                         99065 - 99066
     H*  Labratory and Pathology                        PATH    80104 - 89999
     H*                                                         99007 - 99023
     H*                                                         99026 - 99026
     H*  Medicine                                       MEDI    90700 - 96999
     H*                                                         98000 - 99002
     H*                                                         99024 - 99025
     H*                                                         99027 - 99064
     H*                                                         99069 - 99199
     H*  Physical Medicine                              PHYS    97000 - 97999
     H*****************************************************************
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F*** BEGIN DEL *** 08/09/02 **************************************
     F**********FMGZL   IF  E           K        DISK
     F*** END   DEL *** 08/09/02 **************************************
     FFMPDL     IF   E           K DISK
     FFMRCL     IF   E           K DISK
     FFMPRLOH   IF   E           K DISK
     FFTABLUC   IF   E           K DISK
     F                                     IGNORE(PRC01)
     FFTABLOH   IF   E           K DISK
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PAC01)
     F                                     IGNORE(PAT01)
     F                                     RENAME(PEB01:PEB01OH)
     F                                     RENAME(PPM01:PPM01OH)
     F                                     RENAME(PPA01:PPA01OH)
     F                                     RENAME(PCS01:PCS01OH)
     F                                     RENAME(PSP01:PSP01OH)
     F                                     RENAME(PPS01:PPS01OH)
     F                                     RENAME(PCC01:PCC01OH)
     F                                     RENAME(PFT01:PFT01OH)
     F                                     RENAME(PTS01:PTS01OH)
     FFDCALH    UF   E           K DISK
     FFDCALCP   IF   E           K DISK
     F                                     RENAME(PCA01:PCA0C)
     F*
     FFACAPOH   O    E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK1)
     FFACALOH   IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAWK2)
     FFDCAL6P   IF   E           K DISK
     F                                     RENAME(PCA01:PCA06)
     FFDCAL5    UF   E           K DISK
     F                                     RENAME(PCA01:PCA08)
     FFDCAL9P   IF   E           K DISK
     F                                     RENAME(PCA01:PCA09)
     FFDCALAP   IF   E           K DISK
     F                                     RENAME(PCA01:PCA0A)
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0P)
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
     F*
     F*

     D QQS             S              5    DIM(999)                             DETAIL SEQ #
     D QQC             S              2    DIM(999)                             ERROR MESSAGES
     D*
     D FUF             S              7  0 DIM(500)                             FOLLOW UP FRM DY
     D FUT             S              7  0 DIM(500)                             FOLLOW UP TO DAY
     D*
     D* Compile-time Arrays
     D QCA             S             55    DIM(6) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D ASC             S              5  2 DIM(8) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*
     D NC1             S              5    DIM(88) CTDATA PERRCD(1)             NO CUT ON THESE
     D*
     D** MODIFIER ARRAY
     D DATE            S              7  0 DIM(25)                              Dates 12 limit
     D CNT             S              3  0 DIM(6)                               COUNTER FOR SURG2
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  DATE/TIME DATA STRUCTURE.
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     I* Record Format for SURG2 Workfile FACALOH
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
     IPCA06
     I*
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
     I              CAUNTS                      XAANMN
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
     C*  Provider Master
     C     FMKEY         KLIST
     C                   KFLD                    EOSFEI
     C                   KFLD                    EOSMOD
     C*
     C*
     C*  Claim Detail File
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
     C     CAKY1C        KLIST
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
     C     CAKY9B        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    HLDDAT            7 0
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
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C*** Used with PCA09
     C     CAKY9E        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    CAFDAT
     C*
     C*  Claim Master File
     C     CMKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C*
     C*  Dummy Key used for release of all CA records
     C     DUMMY         KLIST
     C                   KFLD                    QQCO#
     C*
     C*  Provider Code File
     C     PDKY          KLIST
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C*
     C*  Geographical Zip Area File
     C*** BEGIN DEL *** 08/09/02 **************************************
     C********** GZKY      KLIST
     C**********           KFLD           QQCO#
     C**********           KFLD           XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C*
     C*  Provider Master File
     C     PDKEY         KLIST
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C*
     C*  Reasonable & Customary File
     C     RCKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    XXZIPA
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXYMD
     C*
     C*  Procedure Code file
     C     PRKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXEFDT
     C*
     C*  Claim Status Code Table File
     C     TCSKY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    TABKEY           10
     C*
     C*  Procedure Code Modifier File
     C     PMKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    TABKEY
     C*
     C*  User Parameter File
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
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
     C*  Physician header file key list (FAEHL)
     C     EHKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*  Entry Parameter List
     C     *ENTRY        PLIST
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQSS#             9 0          CHAIN TO EE
     C                   PARM                    QQACDT            6 0          (MMDDYY ORD)
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
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDATE            7 0
     C*
     C     *LIKE         DEFINE    CACHK#        HLCHK#
     C     *LIKE         DEFINE    CADSEQ        HLDSEQ
     C     *LIKE         DEFINE    CAEOB#        HLEOB#
     C     *LIKE         DEFINE    CAFEIN        HLFEIN
     C     *LIKE         DEFINE    CAMOD         HLMOD
     C     *LIKE         DEFINE    CAPRTY        HLPRTY
     C*
     C     *LIKE         DEFINE    CACHK#        SVCHK#
     C     *LIKE         DEFINE    CADSEQ        SVDSEQ
     C     *LIKE         DEFINE    CAEOB#        SVEOB#
     C     *LIKE         DEFINE    CAFEIN        SVFEIN
     C     *LIKE         DEFINE    CAMOD         SVMOD
     C     *LIKE         DEFINE    CAPRTY        SVPRTY
     C     *LIKE         DEFINE    DHUNTS        SVUNTS
     C*
     C     *LIKE         DEFINE    CAFEIN        HDFEIN
     C     *LIKE         DEFINE    CAFDAT        HLFDAT
     C     *LIKE         DEFINE    CAFDAT        H2FDAT
     C     *LIKE         DEFINE    CAFDAT        HDYMD
     C     *LIKE         DEFINE    CAFDAT        SVYMD
     C     *LIKE         DEFINE    CAMOD         HDMOD
     C     *LIKE         DEFINE    DHAREA        HLPART
     C     *LIKE         DEFINE    DHAREA        HLDPRT
     C     *LIKE         DEFINE    CATOTA        HLTOTA
     C     *LIKE         DEFINE    PRMRA         HLMRA
     C     *LIKE         DEFINE    PRMRA         HLDMRA
     C     *LIKE         DEFINE    DHUNTS        XGT240
     C     *LIKE         DEFINE    DHUNTS        XLE240
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
     C     *LIKE         DEFINE    RCANAM        SXANAM
     C     *LIKE         DEFINE    CAFEIN        SXFEIN
     C     *LIKE         DEFINE    RCMRA         SXMRA
     C     *LIKE         DEFINE    CAMOD         SXMOD
     C     *LIKE         DEFINE    RCPROF        SXPROF
     C     *LIKE         DEFINE    RCRPT         SXRPT
     C     *LIKE         DEFINE    RCSUPP        SXSUPP
     C     *LIKE         DEFINE    RCSURG        SXSURG
     C     *LIKE         DEFINE    PR1A1         SX1A1
     C     *LIKE         DEFINE    PR1A2         SX1A2
     C     *LIKE         DEFINE    PR1A3         SX1A3
     C     *LIKE         DEFINE    PR1A4         SX1A4
     C     *LIKE         DEFINE    PR1A5         SX1A5
     C     *LIKE         DEFINE    PR92N1        SX92N1
     C*
     C     *LIKE         DEFINE    DHPRCD        XPCPT4
     C     *LIKE         DEFINE    CAEOMB        XPEOMB
     C     *LIKE         DEFINE    CAPMOD        XPPMOD
     C     *LIKE         DEFINE    CATOTA        XPTOTA
     C     *LIKE         DEFINE    CAMRA         XPTOTD
     C     *LIKE         DEFINE    CAPAID        XPTOTP
     C*
     C     *LIKE         DEFINE    DHPRCD        XXCPT4
     C*** BEGIN DEL *** 08/09/02 **************************************
     C********** *LIKE     DEFN GZFRZP    XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C     *LIKE         DEFINE    CAPMOD        XXPMOD
     C*** BEGIN CHG *** 08/09/02 **************************************
     C********** *LIKE     DEFN GZZIPA    XXZIPA
     C     *LIKE         DEFINE    RCZIPA        XXZIPA
     C*** END   CHG *** 08/09/02 **************************************
     C     *LIKE         DEFINE    CAFDAT        XXEFDT
     C     *LIKE         DEFINE    CARCN         XXRCN
     C     *LIKE         DEFINE    DENOT1        XXNOT1
     C     *LIKE         DEFINE    MGMSG#        XXMSG#
     C     *LIKE         DEFINE    MGMSEQ        XXMSEQ
     C     *LIKE         DEFINE    CAEOB#        XXEOB#
     C     *LIKE         DEFINE    CAACDT        XXACDT
     C     *LIKE         DEFINE    CAFDAT        XXYMD
     C*
     C     *LIKE         DEFINE    CAPMOD        X1PMOD
     C*
     C     *LIKE         DEFINE    CAFDAT        KKYMD
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    DEESRC        KKESRC
     C*
     C                   Z-ADD     0             HLADDT            7 7          To hold MVR
     C                   Z-ADD     0             HLANMN           10 7          DHUNTS for 15 minute
     C                   Z-ADD     0             HLANTV            3 0          Anes Time Value
     C                   Z-ADD     0             XXWORK           10 7
     C                   Z-ADD     0             XXANES            5 2
     C                   Z-ADD     0             T                 5 0
     C                   Z-ADD     0             COUNT             5 0
     C                   Z-ADD     0             EOBCNT            2 0
     C                   Z-ADD     0             EOBCN2            2 0
     C                   Z-ADD     0             ZZMRA             9 2
     C                   Z-ADD     *ZEROS        SRDTE             8 0          CHG 11/11/97
     C                   MOVE      *BLANKS       SRERR             1
     C                   MOVE      *BLANKS       SRTERM            1
     C                   MOVE      ' '           XFIRST            1            First time into pgm
     C                   Z-ADD     0             HLADDT            7 7          To hold MVR
     C                   Z-ADD     0             HLANTV            3 0          Anes Time Value
     C                   Z-ADD     0             CNTADD            5 0
     C                   Z-ADD     0             CNTPRT            5 0
     C*** BEGIN CHG *** 09/25/01 **************************************
     C**********           Z-ADD0         XXOVRP  32
     C                   Z-ADD     0             XXOVRP            5 4
     C*** END   CHG *** 09/25/01 **************************************
     C                   Z-ADD     0             XXPERC            5 2
     C                   MOVE      *BLANKS       OTHMRA            1            Other MRA indicator
     C                   MOVE      *BLANKS       OTCODE            7            CPT4/Suffix
     C                   Z-ADD     0             SRFUT             5 0
     C                   Z-ADD     0             SRPST             5 0
     C                   MOVE      *BLANKS       SVSPCD            3
     C                   MOVE      ' '           OPNFLG            1            File Open Flag
     C                   MOVE      ' '           OVRFLG            1            File Override Flag
     C*
     C                   MOVE      '0'           XX25EM
     C*
     C* ==================================================================
     C** Get the User Parameter File
     C     UPKEY         CHAIN     PUP01                              60
     C** If the get was unsuccessful, end the program
     C     *IN60         IFEQ      '1'
     C                   MOVE      '99999'       QQS(1)
     C                   MOVE      '95'          QQC(1)                         INVALID USER PARM
     C* Err Msg 95: Invalid User Parameter. See Programmer
     C                   GOTO      TERM
     C                   END                                                    END 1 *IN60 = PUP01
     C*
     C* ==================================================================
     C** Get the Claim Master record
     C     CMKEY         CHAIN     PCM01                              60
     C** If the get was unsuccessful, end the program
     C     *IN60         IFEQ      '1'
     C                   MOVE      '99998'       QQS(1)
     C                   MOVE      '96'          QQC(1)                         INVALID CASE #
     C* Err Msg 96: Invalid Case Number. See Programmer
     C                   GOTO      TERM
     C                   END                                                    END 1 *IN60 - PCM01
     C*
     C* ==================================================================
     C* CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
     C*
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C*
     C                   CALL      'FA701R'
     C                   PARM      ' '           ##RTCD            1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##SUB#            5
     C                   PARM                    ##FND             3
     C                   PARM      ' '           ##OUTS            1
      *   CALL API WA900R TO DETERMINE FUND TYPE
      *
     C                   MOVEL     *BLANK        @@RCTD            1
     C                   MOVEL     CACO#         @@CO#
     C                   MOVEL     CASUB#        @@SUB#
     C                   MOVEL     CAFND         @@FND
     C                   MOVEL     *BLANKS       @@TYPE
     C*
     C*** THE VALUE OF 'SI' IN @@TYPE WILL ALLOW THE ENTRY OF AN EOB CODE
     C*** OR ELSE THE EOB CODE WILL BE BLANK
     C*
     C     ##WCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C     ##OUTS        ANDNE     'Y'
     C                   CALL      'WA900R'
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@TYPE            2
     C     @@RTCD        IFEQ      'F'
     C                   MOVE      'SI'          @@TYPE
     C                   ENDIF
     C                   ELSE
     C                   MOVE      'SI'          @@TYPE
     C                   ENDIF
     C* ==================================================================
     C*
     C*** XXPASS - USED AS FLAG TO ZERO OUT PAID FIELDS WHEN ERRORS
     C*** OCCUR ON FIRST PASS OF DATA
     C*** XXPASS, 1 = FIRST PASS;  2 = SECOND PASS
     C*
     C                   MOVE      '1'           XXPASS            1
     C*
     C                   EXSR      LODFU                                        LOAD FOLLOW-UP
     C*
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
     C                   EXCEPT    ECA01
     C*
     C     CAFDAT        IFGE      980301
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
     C* --- Get and save the Provider's Specialty Code ----------------
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD
     C                   END
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C*
     C                   Z-ADD     0             MRA1              9 2
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
     C                   MOVE      '0'           XX25EM            1
     C*
     C* ===============================================================
     C* EOMB # 125,
     C* If the Date of service is 2years or more after the carrier rcvdessage
     C* date, disallow item.
     C*
     C* FIND DATE 2 years prior
     C                   Z-ADD     730           SRPST
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT            8 0
     C                   Z-ADD     WKFDAT        SRDTE
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C*
     C                   Z-ADD     SRDTE         XXPST8            8 0          CHG 11/11/97
     C                   Z-ADD     XXPST8        SRDTI                          CHG 11/11/97
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPAST            7 0          CHG 11/11/97
     C     EOCRDT        IFLT      XXPAST
     C                   MOVE      '125'         XPEOMB
     C* EOMB Code 125: This Charge has been paid per Reasonable & Customary
     C                   Z-ADD     *ZEROS        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       Get next record
     C                   END                                                    END GT SYDATE
     C*
     C*** CALL ICD9 EDIT PROGRAM
     C                   MOVEL     EHDIA1        QQICD9
     C                   MOVE      'P'           QQPRSC
     C                   MOVE      'A'           Q2CALL
     C                   MOVE      'P'           QQSYS
     C                   MOVE      *BLANKS       QQSTYP
     C                   MOVE      *BLANKS       QQVLD
     C                   MOVE      *BLANKS       QQGRP
     C                   MOVE      *BLANKS       QQERR
     C                   MOVE      *BLANKS       QQSTS
     C                   MOVE      CACO#         Q1CO#             3
     C                   MOVE      CASUB#        Q1SUB
     C                   MOVE      CAFND         Q1FND
     C                   MOVE      CAFYR         Q2FYR
     C                   MOVE      CACASE        Q2CASE
     C                   CALL      'F4200ROH'                           99
     C                   PARM                    QQICD9            5            IDC9 CODE
     C                   PARM                    QQPRSC            1            PRIMARY/SECOND
     C                   PARM                    Q2CALL            1            CALLED PGM CODE
     C                   PARM                    QQSYS             1            SYSTEM (P/H)
     C                   PARM                    QQSTYP            1            INPAT/OUTPAT
     C                   PARM                    QQVLD             1            VALID/INVALID
     C                   PARM                    QQGRP             1            GROUP COVERAGE
     C                   PARM                    QQERR             2            Ennn/Vnnn ICD9
     C                   PARM                    QQSTS             2            CLAIM STATUS
     C                   PARM                    Q1CO#             3            CO #
     C                   PARM                    Q1SUB             5            SUB #
     C                   PARM                    Q1FND             3            FUND
     C                   PARM                    Q2FYR             2            FUND YEAR
     C                   PARM                    Q2CASE            7            CASE#
     C*
     C* CHECK WHETHER ICD9 CODE HAS BEEN FLAGGED AS BWC INVALID
     C*   ONLY FOR CLIENTS WITH PROGRAM F4200ROH
     C*
     C     *IN99         IFEQ      '0'
     C     QQVLD         IFEQ      'I'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '344'         XPEOMB
     C* EOMB Code 344: "Payment is denied as the ICD9 code is invalid" e"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FW'          QQC(Q)
     C* Err Msg FW: "Invalid ICD9 Code, send to UR"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   ENDIF
     C*
     C* CHECK WHETHER ICD9 CODE HAS DISALLOWED STATUS
     C     QQGRP         IFEQ      'G'
     C     QQSTS         ANDEQ     'DA'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '270'         XPEOMB
     C* EOMB Code 270: "Payment is denied as this ICD9 code has been   e"
     C*    disallowed, there is no proof of relationship to injury"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FX'          QQC(Q)
     C* Err Msg FX: "Diag unrelated, send to UR"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   ENDIF
     C*
     C*
     C* CHECK WHETHER ICD9 CODE HAS DISALLOWED STATUS
     C     QQGRP         IFEQ      'X'
     C     QQSTS         OREQ      '  '                                         ADD 09/24/97
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '276'         XPEOMB
     C* EOMB Code 276: "Payment is denied as the billed diagnosis is   e"
     C*    not allowed in this claim"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FY'          QQC(Q)
     C* Err Msg FY: "Billed Diag is not allowed on claim, send to UR"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  END *IN99
     C* ==================================================================
     C** Get the Claim Status record.  See if override flag is 'Y'.
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C** A hard error will cause the program to terminate ASAP.
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '97'          QQC(Q)                         INVALID CLM STS CODE
     C* Err Msg 97: Invalid Claim Status Code. See Programmer
     C*
     C                   END                                                    END 1 *IN60 - PCS01
     C*
     C* ==================================================================
     C*** If override flag is not equal to 'Y', then process this record
     C     CSOVRD        IFNE      'Y'
     C*
     C* ==================================================================
     C***  Reasonable & Customary Processing
     C     CARCFL        IFEQ      'Y'                                           REAS/CUST
     C*
     C                   MOVEL     DHPRCD        XXCPT4
     C                   MOVE      *BLANKS       XPEOMB
     C                   MOVE      *ZEROS        XPTOTA
     C                   MOVE      *ZEROS        XPTOTP
     C                   MOVE      *ZEROS        XPTOTD
     C*
     C*** BEGIN DEL *** 08/09/02 **************************************
     C**********           MOVE *BLANKS   XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C                   MOVE      *BLANKS       XXZIPA
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C* Now that the Provider is established, get its Geographic Zip Area
     C* 1st - establish the first 3-bytes of the zip code
     C*** BEGIN DEL *** 08/09/02 **************************************
     C**********           MOVELPDZIP     XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C                   END                                                    END 0 *IN60 - PPD01
     C*
     C* 2nd - Get the corresponding Geographic Zip Area record:
     C*** BEGIN CHG *** 08/09/02 **************************************
     C********** GZKY      SETGTPGZ01
     C**********           READPPGZ01                    60
     C********** *IN60     IFEQ '0'
     C********** XXFRZP    ANDLEGZTOZP
     C**********           MOVE GZZIPA    XXZIPA
     C     CAZIPA        IFNE      *BLANKS
     C                   MOVE      CAZIPA        XXZIPA
     C*** END   CHG *** 08/09/02 **************************************
     C*
     C                   ELSE
     C*
     C* USE GEO-ZIP 430 IF ZIP CODE IS OUT OF STATE
     C                   MOVEL     '430'         XXZIPA
     C*
     C                   END                                                    END 0 *IN60 - PGZ01
     C*
     C     RCKEY         SETGT     PRC01
     C     NXRC1         TAG
     C                   READP     PRC01                                  60
     C     *IN60         IFEQ      '0'
     C     RCCO#         ANDEQ     QQCO#
     C     RCZIPA        ANDEQ     XXZIPA
     C     RCCPT4        ANDEQ     XXCPT4
     C     RCSTAT        IFNE      'I'
     C*
     C                   Z-ADD     RCMRA         SXMRA
     C                   Z-ADD     RCANAM        SXANAM
     C                   Z-ADD     RCPROF        SXPROF
     C                   MOVE      RCRPT         SXRPT
     C                   MOVE      RCSUPP        SXSUPP
     C                   MOVE      RCSURG        SXSURG
     C                   MOVE      RC92N1        SX92N1
     C                   MOVE      RC1A1         SX1A1
     C                   MOVE      RC1A2         SX1A2
     C                   MOVE      RC1A3         SX1A3
     C                   MOVE      RC1A4         SX1A4
     C                   MOVE      RC1A5         SX1A5
     C*
     C* ==================================================================
     C*   RETURN ERROR MESSAGE IF EDIT DOES NOT FALL INTO FEE SCHEDULE
     C*   RANGE.  IF CODE HAS NOT BEEN UPDATED FOR 3/01/97 RETURN ERROR
     C*
     C     RCEFDT        IFNE      980301
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GH'          QQC(Q)
     C* Err Msg GH: "Code has not been updated to current fee schedule"
     C*
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   ENDIF
     C*
     C* ==================================================================
     C* PAGE 17-1 ASC GROUP
     C*   ONLY GO THROUGH SELECTION IF SPECIALTY IS ONE OF 5 CODES
     C*   ASC, OSC, OSU, SC, OR VA6
     C*
     C     SVSPCD        IFEQ      'ASC'
     C     SVSPCD        OREQ      'OSC'
     C     SVSPCD        OREQ      'OSU'
     C     SVSPCD        OREQ      'SC '
     C     SVSPCD        OREQ      'VA6'
     C*
     C* ==================================================================
     C*
     C*
     C                   SELECT
     C     RC1A4         WHENEQ    '1'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(1)        SXMRA
     C*
     C     RC1A4         WHENEQ    '2'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(2)        SXMRA
     C*
     C     RC1A4         WHENEQ    '3'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(3)        SXMRA
     C*
     C     RC1A4         WHENEQ    '4'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(4)        SXMRA
     C*
     C     RC1A4         WHENEQ    '5'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(5)        SXMRA
     C*
     C     RC1A4         WHENEQ    '6'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(6)        SXMRA
     C*
     C     RC1A4         WHENEQ    '7'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(7)        SXMRA
     C*
     C     RC1A4         WHENEQ    '8'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(8)        SXMRA
     C                   OTHER
     C*
     C                   MOVE      '379'         XPEOMB
     C* EOMB Code 379: Denied, procedure/servied not elgible for       mary
     C*                reimbursement to ambulatory.                    mary
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   ENDSL
     C                   END                                                          END SVSPCD
     C* ==================================================================
     C* PAGE 2-8, 17  EOMB #524
     C* If a consult code is billed from a provider, mark a counter fory
     C* that provider, if any other codes are billed from this provider
     C* (except any consult code) have msg to user consult.
     C*
     C     DHPRCD        IFLT      '99241'
     C     DHPRCD        ORGT      '99275'
     C*
     C                   MOVE      '99241'       XXCPT4
     C*
     C     CAKYC         SETLL     PCA0C
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        IFGE      '99241'
     C     XCCPT4        ANDLE     '99275'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C*
     C*   CHANGE ERROR MESSAGE
     C*
     C                   MOVE      'GM'          QQC(Q)
     C* Err Msg GM: "A Consultation has been paid to this provider"
     C*
     C                   ENDIF                                                  END XRCPT4
     C*
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C                   ENDDO
     C                   ENDIF                                                  END DHPRCD
     C*
     C*
     C* ===============================================================
     C* PAGE 10 - 8 #17,4
     C* The following supply codes are not covered:
     C* 99070 - Supplies and materials provided by the physician over and
     C* above those usually included in the office visit or other services
     C* rendered.
     C* 64550 - Application of surface neurostimulator.
     C*
     C*
     C     DHPRCD        IFEQ      '99070'
     C     DHPRCD        OREQ      '64550'
     C                   MOVE      'Y0715'       XXCPT4
     C*
     C     CAKY9         SETLL     PCA09
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XRCPT4        IFEQ      'Y0715'
     C     XRCPT4        OREQ      'Y0720'
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '509'         XPEOMB
     C* EOMB Code 509: "Procedure denied, procedure may not be billed  e"
     C*                 with other tens codes.                         e"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   END
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END 0 DOWEQ 60
     C                   END                                                    END GE '90000'
     C*
     C* ===============================================================
     C* EOMB #332
     C* If the servicing provider (DCSFEI) not main fein# has one
     C* of the following specialty codes, and are billed with a code   nd
     C* from the following ranges: disallow                            ces
     C*
     C*
     C                   MOVEL     'N'           DISALL            1
     C*
     C     FMKEY         CHAIN     PPD01                              54
     C     *IN54         IFEQ      '0'
     C     PDSPCD        IFEQ      'CHI'
     C     DHPRCD        IFGE      '00100'
     C     DHPRCD        ANDLE     '69979'
     C     DHPRCD        ORGE      '80002'
     C     DHPRCD        ANDLE     '89399'
     C     DHPRCD        ORGE      '90801'
     C     DHPRCD        ANDLE     '96999'
     C     DHPRCD        ORGE      '99100'
     C     DHPRCD        ANDLE     '99199'
     C*
     C                   MOVEL     'Y'           DISALL
     C                   ENDIF                                                  END GT 00100
     C                   ELSE
     C     PDSPCD        IFEQ      'RNA'
     C     DHPRCD        IFGE      '10040'
     C     DHPRCD        ANDLE     '99090'
     C                   MOVEL     'Y'           DISALL
     C                   ENDIF                                                  END GT 10040
     C                   ENDIF                                                  END EQ RNA
     C                   ENDIF                                                  END EQ CHI
     C                   ENDIF                                                  END *IN54
     C*
     C     DISALL        IFEQ      'Y'
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '332'         XPEOMB
     C* EOMB Code 332: "Procedure denied, procedure may not be billed  e"
     C*                 with other tens codes.                         e"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   ENDIF
     C*
     C*
     C* ==================================================================
     C*** IF PROCEDURE IS "BY-REPORT" FLAG IT AS SUCH AND UPDATE RECORD.
     C*
     C     RCRPT         IFEQ      'Y'
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C* Err Msg 06: This item is By Report - requires further inspection by
     C*             an adjuster.
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       Get next record
     C*
     C                   END                                                    END RCRPT EQ 'Y'
     C*
     C* ==================================================================
     C*** IF PROCEDURE IS "NC CODES"                                   .
     C*
     C     RCRPT         IFEQ      'X'
     C*
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*  CHANGING Eomb from 395 to 351
     C                   MOVE      '351'         XPEOMB
     C* EOMB Code 351: Denied code billed indicates treatment of       mary
     C*                condition not allowed.                          mary
     C*
     C***********          MOVE '395'     XPEOMB
     C* EOMB Code 395: Payment is denied as the service does not appearmary
     C***********      related to injury                               mary
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       Get next record
     C*
     C                   END                                                    END RCRPT EQ 'Y'
     C*
     C* ==================================================================
     C*-- INDIVIDUAL CONSIDERATION ----------------------------------------
     C     RCRPT         IFEQ      'I'
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
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END                                                    END RCRPT EQ 'I'
     C*
     C     RCFSR         IFNE      'Y'
     C*
     C     CACHRG        COMP      RCMRA                              54
     C   54              MOVE      *BLANKS       XPEOMB
     C* EOMB Code 201: This Charge has been paid per Reasonable & Customary
     C   54              Z-ADD     RCMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   END                                                    END RCFSR
     C*
     C                   END                                                    END RCSTAT NE 'I'
     C                   END                                                    END 0 *IN60 - PRC01
     C                   END
     C* ==================================================================
     C* PROCESS THE FEE SCHEDULED CODES HERE
     C*
     C     CARCFL        IFNE      'Y'
     C*
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
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '98'          QQC(Q)
     C* Err Msg 98: "Invalid Procedure Code, See Programmer"
     C                   Z-ADD     *ZEROS        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   END
     C     PRSTAT        CABEQ     'I'           NXTPR2
     C* ===============================================================
     C* PAGE 10-6 #17 (2)
     C* Note:  Do not bill code Y0715 with E0720 or E0730.  For TENS units
     C* approved for rental/purchase prior to March 23, 1994, continue using
     C* the E0720 or E0730 codes, once each month until the purchase price is
     C* paid, not to exceed five months.
     C*
     C     DHPRCD        IFEQ      'E0720'
     C     DHPRCD        OREQ      'E0730'
     C                   MOVE      'Y0715'       XXCPT4
     C*
     C     CAKY9         SETLL     PCA09
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XRCPT4        IFEQ      'Y0715'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AJ'          QQC(Q)
     C* Err Msg AJ: "Service falls within pre-operative policy."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '509'         XPEOMB
     C* EOMB Code 509: "Procedure denied, procedure may not be billed  e"
     C*                 with other tens codes."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   END
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END 0 DOWEQ 60
     C                   END                                                    END GE E0720
     C*
     C* ==================================================================
     C*** IF BY REPORT IS "X" PROCEDURE IS NON-COVERED                 .
     C*
     C     PRRPT         IFEQ      'X'
     C*
     C* EOMB Code 207: "Payment denied, this is a non-covered service".
     C*
     C                   MOVE      '351'         XPEOMB
     C* EOMB Code 351: Denied code billed indicates treatment of       mary
     C*                condition not allowed.                          mary
     C*
     C* EOMB Code 326: "Payment denied, this service not covered on this
     C*                 date of service"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       Get next record
     C*
     C                   END                                                    END RCRPT EQ 'Y'
     C*
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
     C* EOMB Code 202: "Charges exceed maximum reimbursement amount."
     C   60              MOVE      *BLANKS       XPEOMB
      *   CALL API WA900R TO DETERMINE FUND TYPE
     C*
     C     @@TYPE        IFEQ      'SI'
      *
     C                   MOVE      '202'         XPEOMB
     C* EOMB Code 202: "Charges exceed maximum reimbursement amount."
     C*                 reimbursement manual
      *
     C                   ENDIF
     C*****************************************************************
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
     C*-- By Report -------------------------------------------------------
     C     PRRPT         IFEQ      'Y'
     C                   MOVE      *BLANKS       XPEOMB
     C*
     C*   If modifier exists check the code through the Modifier Subr
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C* Err Msg 91: "Carrier to set payment on individual case basis
     C                   MOVE      'FZ'          QQC(Q)
     C* Err Msg FZ: "Send to Utiliztion Review"
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C* Err Msg 06: This item is By Report - requires further inspection by
     C*             an adjuster.
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END                                                    END PRRPT EQ 'Y'
     C*
     C*--------------------------------------------------------------------
     C*-- ANE/Amount Not Established --------------------------------------
     C     PRRPT         IFEQ      'A'
     C*
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
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END                                                    END PRRPT EQ 'A'
     C*
     C*--------------------------------------------------------------------
     C*-- INDIVIDUAL CONSIDERATION ----------------------------------------
     C     PRRPT         IFEQ      'I'
     C*
     C*   If modifier exists check the code through the Modifier Subr
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD1
     C*
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
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
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END                                                    END PRRPT EQ 'I'
     C                   END                                                    END PRFSR EQ 'Y'
     C                   END                                                    END CARCFL NE 'Y'
     *** BEG ADD *** 04/28/98 ****************************************
     C*
     C*== Normal Processing of Procedure Code =========================
     C* ==================================================================
     C* PAGE 17-1 ASC GROUP
     C*   ONLY GO THROUGH SELECTION IF SPECIALTY IS ONE OF 5 CODES
     C*   ASC, OSC, OSU, SC, OR VA6
     C*
     C     SVSPCD        IFEQ      'ASC'
     C     SVSPCD        OREQ      'OSC'
     C     SVSPCD        OREQ      'OSU'
     C     SVSPCD        OREQ      'SC '
     C     SVSPCD        OREQ      'VA6'
     C*
     C* ==================================================================
     C*
     C*
     C                   SELECT
     C     PR1AF         WHENEQ    '1'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(1)        SXMRA
     C*
     C     PR1AF         WHENEQ    '2'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(2)        SXMRA
     C*
     C     PR1AF         WHENEQ    '3'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(3)        SXMRA
     C*
     C     PR1AF         WHENEQ    '4'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(4)        SXMRA
     C*
     C     PR1AF         WHENEQ    '5'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(5)        SXMRA
     C*
     C     PR1AF         WHENEQ    '6'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(6)        SXMRA
     C*
     C     PR1AF         WHENEQ    '7'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(7)        SXMRA
     C*
     C     PR1AF         WHENEQ    '8'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(8)        SXMRA
     C                   OTHER
     C*
     C                   MOVE      '379'         XPEOMB
     C* EOMB Code 379: Denied, procedure/servied not elgible for       mary
     C*                reimbursement to ambulatory.                    mary
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   ENDSL
     C                   END                                                          END SVSPCD
     *** END ADD *** 04/28/98 *****************************************
     C*
     C* PAGE 4-1 #2
     C* Physician Assistant
     C* Physician assistants are not elgible for direct reimbursement from BWC
     C* Some services, excluding surgery assistant services, provided by a
     C* physician assistant under the supervision of a licensed, enrolled
     C* physician may be reimbursed directly to the physician.
     C*
     C     SVSPCD        IFEQ      'PA '
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FT'          QQC(Q)
     C* Err Msg FT: "Verify specialty of provider for this treatment"
     C*
     C                   MOVE      '332'         XPEOMB
     C*** EOMB 332: "Payment denied, servicing provider is not elgible
     C*             "for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END                                                    END SVSPCD EQ PA
     C*================================================================
     C* If modifier 22, 52, 53, 55, 56, 58, 57, 66 or 99 is billed
     C* with any code, have a message and allow                        rom BWC
     C*
     C     XPPMOD        IFEQ      '22'
     C     XPPMOD        OREQ      '52'
     C     XPPMOD        OREQ      '53'
     C     XPPMOD        OREQ      '55'
     C     XPPMOD        OREQ      '56'
     C     XPPMOD        OREQ      '58'
     C     XPPMOD        OREQ      '66'
     C     XPPMOD        OREQ      '99'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GC'          QQC(Q)
     C* Err Msg GC: "Manual pricing required"
     C                   ENDIF                                                   END EQ 22
     C*================================================================
     C* If modifier 90 is billed with code range from 99201- 99499
     C* have a message and allow                                       rom BWC
     C*
     C     XPPMOD        IFEQ      '90'
     C     DHPRCD        IFGE      '00100'
     C     DHPRCD        ANDLE     '79999'
     C     DHPRCD        ORGE      '99201'
     C     DHPRCD        ANDLE     '99499'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GD'          QQC(Q)
     C* Err Msg GC: "Medical review required"
     C                   ENDIF                                                   END DHPRCD
     C                   ENDIF                                                   END EQ 90
     C*   Replaced with next edit
     C*================================================================
     C* If modifier 54 is billed with code range from 70000 - 99499
     C* have a message and allow                                       rom BWC
     C*================================================================
     C* If modifier 54 is billed with code range from 10000 - 69999
     C* have a message and allow                                       rom BWC
     C*
     C     XPPMOD        IFEQ      '54'
     C     DHPRCD        IFGE      '10000'
     C     DHPRCD        ANDLE     '69999'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GC'          QQC(Q)
     C* Err Msg GC: "Manual pricing required"
     C*
     C                   ELSE
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GD'          QQC(Q)
     C* Err Msg GD: "Medical review required"
     C*
     C                   ENDIF                                                   END DHPRCD
     C                   ENDIF                                                   END EQ 54
     C*================================================================
     C* Modifier -57 when billed with code from range 99201 - 99499
     C* pay regardless of all other edits else EOMB 169.               thesia
     C*
     C     XPPMOD        IFEQ      '57'
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99499'
     C*
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     CACHRG        XPTOTP
     C*
     C                   ELSE
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '169'         XPEOMB
     C* EOMB Code 169: "Payment is denied as the modifier is not valid
     C*                 with the procedure code billed"
     C*
     C                   END                                                    END DHPRCD
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END                                                    END XPPMOD
     C*****************************************************************
     C*
     C*   PHASE 4 Revisions:  If modifier 24 is billed with a code
     C*   equal to 99211 , , 99499 (displayed below) have msg, else deny.
     C*
     C     XPPMOD        IFEQ      '24'
     C     XPPMOD        OREQ      '25'                                         ADD 09/30/97
     C*
     C*   NEED 25 TO PAY ALL 9XXXX RANGE NOT JUST SPECIFIC RANGES LIKE
     C*   MODIFIER -24
     C     XPPMOD        IFEQ      '25'                                         ADD 09/30/97
     C     DHPRCD        ANDGE     '99201'
     C     DHPRCD        ANDLE     '99499'
     C*
     C*
      *   CHANGE TO OR SO IT ONLY APPLIES TO MOD 24
     C     DHPRCD        ORGE      '99211'
     C     DHPRCD        ANDLE     '99239'
     C*
     C     DHPRCD        ORGE      '99281'
     C     DHPRCD        ANDLE     '99292'
     C*
     C     DHPRCD        ORGE      '99296'
     C     DHPRCD        ANDLE     '99313'
     C*
     C     DHPRCD        ORGE      '99331'
     C     DHPRCD        ANDLE     '99333'
     C*
     C     DHPRCD        ORGE      '99351'
     C     DHPRCD        ANDLE     '99499'
     C*
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C* Update charge with payment
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   ELSE
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '169'         XPEOMB
     C* EOMB Code 169  "Payment is denied as the modifier is not valid with the
     C*                 procedure code billed"
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   END                                                    END EQ '99211'
     C                   END                                                    END EQ '24'
     C*
     C*================================================================
     C*** END ADD ***** 09/30/97 **************************************
     C*================================================================
     C* If modifiers 76, 77, 78, 79 are billed with codes from ranges
     C* 00100-02020 or 7000-89999 have msg and allow                   rom BWC
     C*
     C     XPPMOD        IFGE      '76'
     C     XPPMOD        ANDLE     '79'
     C     DHPRCD        IFGE      '10000'
     C     DHPRCD        ANDLE     '69999'
     C     DHPRCD        ORGE      '90000'
     C     DHPRCD        ANDLE     '99499'
     C                   GOTO      PAST
     C                   ELSE
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GD'          QQC(Q)
     C* Err Msg GC: "Medical review required"
     C*
     C                   ENDIF                                                   END DHPRCD
     C     PAST          TAG                                                    ADD 09/30/97
     C                   ENDIF                                                   END EQ 76
     C*================================================================
     C* Modifier -62,-80 when billed with code from range 00100 - 02020
     C* or70000-99499 deny line EOB 169.                               thesia
     C*
     C     XPPMOD        IFEQ      '62'
     C     XPPMOD        OREQ      '80'
     C*
     C     DHPRCD        IFGE      '00100'
     C     DHPRCD        ANDLE     '02020'
     C*
     C     DHPRCD        ORGE      '70000'
     C     DHPRCD        ANDLE     '99499'
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '169'         XPEOMB
     C* EOMB Code 169: "Payment is denied as the modifier is not valid
     C*                 with the procedure code billed"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END                                                    END DHPRCD
     C                   END                                                    END XPPMOD
     C*================================================================
     C* If multiple units are billed in one line item for any CPT4 code
     C* or range 70000-99499 in Reasonable and Customary file
     C*
     C     DHUNTS        IFGT      1
     C*
     C     CARCFL        IFEQ      'C'
     C*
     C     SXANAM        MULT      DHUNTS        SXMRA
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GE'          QQC(Q)
     C* Err Msg GE: "Multiple units entered, Verify payment."
     C                   ENDIF                                                   END CARCFL
     C                   ENDIF                                                   END DHUNTS
     C*================================================================
     C* If multiple units are billed in one line item for any CPT4 code
     C* From range 00100-99499 only pay one unit
     C*
     C     DHUNTS        IFGT      1
     C*
     C     CARCFL        IFNE      'C'
     C     DHPRCD        ANDGE     '10000'
     C     DHPRCD        ANDLE     '99499'
     C*
     C     SXANAM        MULT      1             SXMRA
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GE'          QQC(Q)
     C* Err Msg GE: "Multiple units entered, Verify payment."
     C                   ENDIF                                                   END CARCFL
     C                   ENDIF                                                   END DHUNTS
     C*================================================================
     C* If modifiers -26, -27 is billed, the same date, the opposite
     C* mod will not pay, same fein eomb 525, different fein (523)
     C*
     C                   Z-ADD     0             X
     C     XPPMOD        IFEQ      '26'
     C     XPPMOD        OREQ      '27'
     C*
     C     CAKY9E        SETLL     PCA09
     C     CAKY9E        READE     PCA09                                  60
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
     C     XRFEIN        ANDEQ     CAFEIN
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRTOTP        ANDGT     0
     C*
     C     X             IFGE      2
     C     XRPMOD        OREQ      XPPMOD
     C     XRPMOD        ORNE      '26'
     C     XRPMOD        ANDNE     '27'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '525'         XPEOMB
     C*   EOMB 525: "Payment denied, this procedure is mutually exclusive "
     C*             "to another code"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   END                                                    END X
     C                   END                                                    END XRCPT4
     C*****************************************************************
     C** DIFFERENT PROVIDER RETURNS DIFFERENT EOMB
     C*
     C     XRCPT4        IFEQ      DHPRCD
     C     XRFDAT        ANDEQ     CAFDAT
     C     XRFEIN        ANDNE     CAFEIN
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRTOTP        ANDGT     0
     C*
     C     X             IFGE      2
     C     XRPMOD        OREQ      XPPMOD
     C     XRPMOD        ORNE      '26'
     C     XRPMOD        ANDNE     '27'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '523'         XPEOMB
     C*   EOMB 525: "Payment denied, this procedure is mutually exclusive "
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   END                                                    END X
     C                   END                                                    END XRPMOD
     C*****************************************************************
     C     SXPROF        IFEQ      *ZEROS
     C     SX92N1        ANDEQ     *ZEROS
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '117'         XPEOMB
     C*   EOMB 117: "Payment denied, as the modifier is invalid"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   END                                                    END EQ SXPROF
     C*
     C     CAKY9E        READE     PCA09                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C                   END                                                    END XPPMOD
     C*
     C*
     C* PAGE 4-3 #10
     C* You may not bill CPT codes for services other than general anesthesia
     C* with general anesthesia modifiers.  Bills will be denied with E.O.B. 380
     C*
     C     XPPMOD        IFEQ      '23'
     C     XPPMOD        OREQ      '30'
     C     XPPMOD        OREQ      '95'
     C     XPPMOD        OREQ      'QX'
     C     DHPRCD        IFGE      '10040'
     C     DHPRCD        ANDLE     '99499'
     C*
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '380'         XPEOMB
     C*** EOMB 380: ""
     C*
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END                                                    END DHPRCD
     C                   END                                                    END XPPMOD
     C*-- Execute all Subroutines here ---------------------------------
     C*
     C     DHPRCD        IFGE      '99201'                                      EVAL/MGMT
     C     DHPRCD        ANDLE     '99499'
     C                   EXSR      EVAL
     C                   ELSE                                                      ELSE EVAL
     C*
     C     DHPRCD        IFGE      '00100'                                      ANESTHESIA
     C     DHPRCD        ANDLE     '02020'
     C                   EXSR      ANES
     C                   ELSE                                                      ELSE ANES
     C*
     C     DHPRCD        IFGE      '10000'                                      SURGERY
     C     DHPRCD        ANDLE     '69979'                                      SURGERY
     C*
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the workfile is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALOH
     C                   CLOSE     FACAPOH
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALOH
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPOH
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACOH'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALOH
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACALPOH
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALOH
     C                   OPEN      FACAPOH
     C                   MOVE      '1'           OPNFLG
     C                   END
     C*
     C*     then, reset hold YMD field and provider/mod
     C                   Z-ADD     XXYMD         HDYMD
     C                   Z-ADD     CAFEIN        HDFEIN
     C                   Z-ADD     CAMOD         HDMOD
     C*
     C                   ELSE                                                   ELSE XFIRST
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
     C                   CLOSE     FACALOH
     C                   CLOSE     FACAPOH
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALOH
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACALPOH
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACOH'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALOH
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACALPOH
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALOH
     C                   OPEN      FACAPOH
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
     C                   EXSR      SURG
     C*
     C                   ELSE                                                      ELSE SURG
     C*
     C     DHPRCD        IFGE      '70000'                                      RADIOLOGY
     C     DHPRCD        ANDLE     '79999'
     C     DHPRCD        ORGE      '99065'                                      RADIOLOGY
     C     DHPRCD        ANDLE     '99066'
     C                   EXSR      RADI
     C*
     C                   ELSE                                                      ELSE RADI
     C*
     C     DHPRCD        IFGE      '80000'                                      PATHOLOGY
     C     DHPRCD        ANDLE     '89999'
     C     DHPRCD        ORGE      '99077'                                      PATHOLOGY
     C     DHPRCD        ANDLE     '99023'
     C     DHPRCD        OREQ      '99026'
     C                   EXSR      PATH
     C*
     C                   ELSE                                                      ELSE PATH
     C*
     C     DHPRCD        IFGE      '90700'                                      MEDICAL
     C     DHPRCD        ANDLE     '96999'
     C     DHPRCD        ORGE      '98000'
     C     DHPRCD        ANDLE     '99002'
     C     DHPRCD        ORGE      '99024'
     C     DHPRCD        ANDLE     '99025'
     C     DHPRCD        ORGE      '99027'
     C     DHPRCD        ANDLE     '99064'
     C     DHPRCD        ORGE      '99069'
     C     DHPRCD        ANDLE     '99199'
     C                   EXSR      MEDI
     C*
     C                   ELSE                                                      ELSE MEDI
     C*
     C     DHPRCD        IFGE      '97000'                                      PHYSICAL MEDICINE
     C     DHPRCD        ANDLE     '97999'
     C                   EXSR      PHYS
     C*
     C                   ELSE                                                      ELSE PHYS
     C*
     C                   EXSR      OTHE                                         OTHER CODES
     C*
     C                   END                                                       END PHYS
     C                   END                                                       END MEDI
     C                   END                                                       END PATH
     C                   END                                                       END RADI
     C                   END                                                       END SURG
     C                   END                                                       END ANES
     C                   END                                                       END EVAL
     C*
     C*
     C*
     C                   ELSE                                                   OVERRIDE TYPE
     C*
     C* ==================================================================
     C*
     C     CAKY1A        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C*** IF OVERRIDE TYPE WE MUST STILL OUTPUT THE ALLOWED/DISALLOWED ETC.
     C     CAMRA         IFEQ      0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CATOTA        ANDEQ     0
     C     CAPAID        ANDEQ     0
     C*
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
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     SXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA01
     C*
     C                   END                                                    END 0 *IN54 - PCA01
     C                   END                                                    END OVERRIDE TYPE
     C                   END                                                    CAFDAT IFGE
     C*
     C     NXTRCD        TAG
     C*
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
     C     CAKYH1        READE     PCA01                                  54      GET NEXT RCD
     C*                                                      TO PROCESS
     C*
     C                   ENDDO                                                  END 0 DOWEQ *IN54
     C*
     C*
     C*** Execute SURG2 if a SURG Code has been processed
     C     OPNFLG        IFEQ      '1'
     C                   EXSR      SURG2                                        Process the SURG cod
     C                   END                                                    END OPNFLG EQ '1'
     C*
     C*****************************************************************
     C* THE FOLLOWING CODE WAS 14PLICATED TO CATCH ERRORS NOT FOUND
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
     C* INITIALIZE VALUES FOR QQS, AND QQC
     C                   MOVE      *ZEROS        QQS
     C                   MOVE      *ZEROS        QQC
     C*
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
     C                   EXCEPT    ECA01
     C*
     C*
     C     CAFDAT        IFGE      980301
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
     C* --- Get and save the Provider's Specialty Code ----------------
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD
     C                   END
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C*
     C                   Z-ADD     0             MRA1              9 2
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
     C                   MOVE      '0'           XX25EM            1
     C*
     C* ==================================================================
     C* EOMB # 125,
     C* If the Date of service is 2years or more after the carrier rcvdessage
     C* date, disallow item.
     C*
     C* FIND DATE 2 years prior
     C                   Z-ADD     730           SRPST
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C                   Z-ADD     WKFDAT        SRDTE
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C                   Z-ADD     SRDTE         XXPST8                         CHG 11/11/97
     C                   Z-ADD     XXPST8        SRDTI                          CHG 11/11/97
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPAST
     C     EOCRDT        IFLT      XXPAST
     C                   MOVE      '125'         XPEOMB
     C* EOMB Code 125: This Charge has been paid per Reasonable & Customary
     C                   Z-ADD     *ZEROS        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       Get next record
     C                   END                                                    END GT SYDATE
     C*
     C*** CALL ICD9 EDIT PROGRAM
     C                   MOVEL     EHDIA1        QQICD9
     C                   MOVE      'P'           QQPRSC
     C                   MOVE      'A'           Q2CALL
     C                   MOVE      'P'           QQSYS
     C                   MOVE      *BLANKS       QQSTYP
     C                   MOVE      *BLANKS       QQVLD
     C                   MOVE      *BLANKS       QQGRP
     C                   MOVE      *BLANKS       QQERR
     C                   MOVE      *BLANKS       QQSTS
     C                   MOVE      CACO#         Q1CO#             3
     C                   MOVE      CASUB#        Q1SUB
     C                   MOVE      CAFND         Q1FND
     C                   MOVE      CAFYR         Q2FYR
     C                   MOVE      CACASE        Q2CASE
     C                   CALL      'F4200ROH'                           99
     C                   PARM                    QQICD9            5            IDC9 CODE
     C                   PARM                    QQPRSC            1            PRIMARY/SECOND
     C                   PARM                    Q2CALL            1            CALLED PGM CODE
     C                   PARM                    QQSYS             1            SYSTEM (P/H)
     C                   PARM                    QQSTYP            1            INPAT/OUTPAT
     C                   PARM                    QQVLD             1            VALID/INVALID
     C                   PARM                    QQGRP             1            GROUP COVERAGE
     C                   PARM                    QQERR             2            Ennn/Vnnn ICD9
     C                   PARM                    QQSTS             2            CLAIM STATUS
     C                   PARM                    Q1CO#             3            CO #
     C                   PARM                    Q1SUB             5            SUB #
     C                   PARM                    Q1FND             3            FUND
     C                   PARM                    Q2FYR             2            FUND YEAR
     C                   PARM                    Q2CASE            7            CASE#
     C*
     C* CHECK WHETHER ICD9 CODE HAS BEEN FLAGGED AS BWC INVALID
     C*   ONLY FOR CLIENTS WITH PROGRAM F4200ROH
     C*
     C     *IN99         IFEQ      '0'
     C     QQVLD         IFEQ      'I'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '344'         XPEOMB
     C* EOMB Code 344: "Payment is denied as the ICD9 code is invalid" e"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FW'          QQC(Q)
     C* Err Msg FW: "Invalid ICD9 Code, send to UR"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1
     C*
     C                   ENDIF
     C*
     C* CHECK WHETHER ICD9 CODE HAS DISALLOWED STATUS
     C     QQGRP         IFEQ      'G'
     C     QQSTS         ANDEQ     'DA'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '270'         XPEOMB
     C* EOMB Code 270: "Payment is denied as this ICD9 code has been   e"
     C*    disallowed, there is no proof of relationship to injury"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FX'          QQC(Q)
     C* Err Msg FX: "Diag unrelated, send to UR"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1
     C*
     C                   ENDIF
     C*
     C*
     C* CHECK WHETHER ICD9 CODE HAS DISALLOWED STATUS
     C     QQGRP         IFEQ      'X'
     C     QQSTS         OREQ      '  '                                         ADD 09/24/97
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '276'         XPEOMB
     C* EOMB Code 276: "Payment is denied as the billed diagnosis is   e"
     C*    not allowed in this claim"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FY'          QQC(Q)
     C* Err Msg FY: "Billed Diag is not allowed on claim, send to UR"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  END *IN99
     C* ==================================================================
     C** Get the Claim Status record.  See if override flag is 'Y'.
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C** A hard error will cause the program to terminate ASAP.
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '97'          QQC(Q)                         INVALID CLM STS CODE
     C* Err Msg 97: Invalid Claim Status Code. See Programmer
     C                   END                                                    END 1 *IN60 - PCS01
     C*
     C* ==================================================================
     C*** If override flag is not equal to 'Y', then process this record
     C     CSOVRD        IFNE      'Y'
     C*
     C* ==================================================================
     C***  Reasonable & Customary Processing
     C     CARCFL        IFEQ      'Y'                                           REAS/CUST
     C*
     C                   MOVEL     DHPRCD        XXCPT4
     C                   MOVE      *BLANKS       XPEOMB
     C                   MOVE      *ZEROS        XPTOTA
     C                   MOVE      *ZEROS        XPTOTP
     C                   MOVE      *ZEROS        XPTOTD
     C*
     C*** BEGIN DEL *** 08/09/02 **************************************
     C**********           MOVE *BLANKS   XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C                   MOVE      *BLANKS       XXZIPA
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C* Now that the Provider is established, get its Geographic Zip Area
     C* 1st - establish the first 3-bytes of the zip code
     C*** BEGIN DEL *** 08/09/02 **************************************
     C**********           MOVELPDZIP     XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C                   END                                                    END 0 *IN60 - PPD01
     C*
     C* 2nd - Get the corresponding Geographic Zip Area record:
     C*** BEGIN CHG *** 08/09/02 **************************************
     C********** GZKY      SETGTPGZ01
     C**********           READPPGZ01                    60
     C********** *IN60     IFEQ '0'
     C********** XXFRZP    ANDLEGZTOZP
     C**********           MOVE GZZIPA    XXZIPA
     C     CAZIPA        IFNE      *BLANKS
     C                   MOVE      CAZIPA        XXZIPA
     C*** END   CHG *** 08/09/02 **************************************
     C*
     C                   ELSE
     C*
     C* USE GEO-ZIP 430 IF ZIP CODE IS OUT OF STATE
     C                   MOVEL     '430'         XXZIPA
     C*
     C                   END                                                    END 0 *IN60 - PGZ01
     C*
     C     RCKEY         SETGT     PRC01
     C     NXRC2         TAG
     C                   READP     PRC01                                  60
     C     *IN60         IFEQ      '0'
     C     RCCO#         ANDEQ     QQCO#
     C     RCZIPA        ANDEQ     XXZIPA
     C     RCCPT4        ANDEQ     XXCPT4
     C     RCSTAT        IFNE      'I'
     C*
     C                   Z-ADD     RCMRA         SXMRA
     C                   Z-ADD     RCANAM        SXANAM
     C                   Z-ADD     RCPROF        SXPROF
     C                   MOVE      RCRPT         SXRPT
     C                   MOVE      RCSUPP        SXSUPP
     C                   MOVE      RCSURG        SXSURG
     C                   MOVE      RC92N1        SX92N1
     C                   MOVE      RC1A1         SX1A1
     C                   MOVE      RC1A2         SX1A2
     C                   MOVE      RC1A3         SX1A3
     C                   MOVE      RC1A4         SX1A4
     C                   MOVE      RC1A5         SX1A5
     C*
     C* ==================================================================
     C*   RETURN ERROR MESSAGE IF EDIT DOES NOT FALL INTO FEE SCHEDULE
     C*   RANGE.  IF CODE HAS NOT BEEN UPDATED FOR 3/01/97 RETURN ERROR
     C*
     C     RCEFDT        IFNE      980301
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GH'          QQC(Q)
     C* Err Msg GH: "Code has not been updated to current fee schedule"
     C*
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1
     C                   ENDIF
     C*
     C* ==================================================================
     C* PAGE 17-1 ASC GROUP
     C*
     C*   ONLY GO THROUGH SELECTION IF SPECIALTY IS ONE OF 5 CODES
     C*   ASC, OSC, OSU, SC, OR VA6
     C*
     C     SVSPCD        IFEQ      'ASC'
     C     SVSPCD        OREQ      'OSC'
     C     SVSPCD        OREQ      'OSU'
     C     SVSPCD        OREQ      'SC '
     C     SVSPCD        OREQ      'VA6'
     C*
     C* ==================================================================
     C*
     C                   SELECT
     C     RC1A4         WHENEQ    '1'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(1)        SXMRA
     C*
     C     RC1A4         WHENEQ    '2'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(2)        SXMRA
     C*
     C     RC1A4         WHENEQ    '3'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(3)        SXMRA
     C*
     C     RC1A4         WHENEQ    '4'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(4)        SXMRA
     C*
     C     RC1A4         WHENEQ    '5'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(5)        SXMRA
     C*
     C     RC1A4         WHENEQ    '6'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(6)        SXMRA
     C*
     C     RC1A4         WHENEQ    '7'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(7)        SXMRA
     C*
     C     RC1A4         WHENEQ    '8'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(8)        SXMRA
     C                   OTHER
     C*
     C                   MOVE      '379'         XPEOMB
     C* EOMB Code 379: Denied, procedure/servied not elgible for       mary
     C*                reimbursement to ambulatory.                    mary
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1
     C                   ENDSL
     C                   END                                                          END RCIPOP
     C* ==================================================================
     C* ===============================================================
     C* PAGE 2-8, 17  EOMB #524
     C* If a consult code is billed from a provider, mark a counter fory
     C* that provider, if any other codes are billed from this provider
     C* (except any consult code) have msg to user consult.
     C*
     C     DHPRCD        IFLT      '99241'
     C     DHPRCD        ORGT      '99275'
     C*
     C                   MOVE      '99241'       XXCPT4
     C*
     C     CAKYC         SETLL     PCA0C
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        IFGE      '99241'
     C     XCCPT4        ANDLE     '99275'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C*
     C*   CHANGE ERROR MESSAGE
     C*
     C                   MOVE      'GM'          QQC(Q)
     C* Err Msg GM: "A Consultation has been paid to this provider"
     C*
     C                   ENDIF                                                  END XRCPT4
     C*
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C                   ENDDO
     C                   ENDIF                                                  END DHPRCD
     C*
     C*
     C* ===============================================================
     C* ===============================================================
     C* PAGE 10 - 8 #17,4
     C* The following supply codes are not covered:
     C* 99070 - Supplies and materials provided by the physician over and
     C* above those usually included in the office visit or other services
     C* rendered.
     C* 64550 - Application of surface neurostimulator.
     C*
     C*
     C     DHPRCD        IFEQ      '99070'
     C     DHPRCD        OREQ      '64550'
     C                   MOVE      'Y0715'       XXCPT4
     C*
     C     CAKY9         SETLL     PCA09
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XRCPT4        IFEQ      'Y0715'
     C     XRCPT4        OREQ      'Y0720'
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '509'         XPEOMB
     C* EOMB Code 509: "Procedure denied, procedure may not be billed  e"
     C*                 with other tens codes.                         e"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1
     C*
     C                   END
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END 0 DOWEQ 60
     C                   END                                                    END GE '90000'
     C*
     C* ===============================================================
     C* EOMB #332
     C* If the servicing provider (E1SFEI) not main fein# has one
     C* of the following specialty codes, and are billed with a code   nd
     C* from the following ranges: disallow                            ces
     C*
     C*
     C                   MOVEL     'N'           DISALL            1
     C*
     C*
     C     FMKEY         CHAIN     PPD01                              54
     C     *IN54         IFEQ      '0'
     C     PDSPCD        IFEQ      'CHI'
     C     DHPRCD        IFGE      '00100'
     C     DHPRCD        ANDLE     '69979'
     C     DHPRCD        ORGE      '80002'
     C     DHPRCD        ANDLE     '89399'
     C     DHPRCD        ORGE      '90801'
     C     DHPRCD        ANDLE     '96999'
     C     DHPRCD        ORGE      '99100'
     C     DHPRCD        ANDLE     '99199'
     C*
     C                   MOVEL     'Y'           DISALL
     C                   ENDIF                                                  END GT 00100
     C                   ELSE
     C     PDSPCD        IFEQ      'RNA'
     C*
     C     DHPRCD        IFGE      '10040'
     C     DHPRCD        ANDLE     '99090'
     C                   MOVEL     'Y'           DISALL
     C                   ENDIF                                                  END GT 10040
     C                   ENDIF                                                  END EQ RNA
     C                   ENDIF                                                  END EQ CHI
     C                   ENDIF                                                  END EQ *IN54
     C*
     C     DISALL        IFEQ      'Y'
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '332'         XPEOMB
     C* EOMB Code 332: "Procedure denied, procedure may not be billed  e"
     C*                 with other tens codes.                         e"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1
     C*
     C                   ENDIF
     C*
     C*
     C* ==================================================================
     C*** IF PROCEDURE IS "BY-REPORT" FLAG IT AS SUCH AND UPDATE RECORD.
     C*
     C     RCRPT         IFEQ      'Y'
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C* Err Msg 06: This item is By Report - requires further inspection by
     C*             an adjuster.
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       Get next record
     C*
     C                   END                                                    END RCRPT EQ 'Y'
     C*
     C* ==================================================================
     C*** IF PROCEDURE IS "NC CODES"                                   .
     C*
     C     RCRPT         IFEQ      'X'
     C*
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*  CHANGING Eomb from 395 to 351
     C                   MOVE      '351'         XPEOMB
     C* EOMB Code 351: Denied code billed indicates treatment of       mary
     C*                condition not allowed.                          mary
     C*
     C***********          MOVE '395'     XPEOMB
     C* EOMB Code 395: Payment is denied as the service does not appearmary
     C***********      related to injury                               mary
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       Get next record
     C*
     C                   END                                                    END RCRPT EQ 'Y'
     C*
     C* ==================================================================
     C*-- INDIVIDUAL CONSIDERATION ----------------------------------------
     C     RCRPT         IFEQ      'I'
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
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       GET NEXT RECORD
     C*
     C                   END                                                    END PRRPT EQ 'I'
     C*
     C     RCFSR         IFNE      'Y'
     C*
     C     CACHRG        COMP      RCMRA                              54
     C* EOMB Code 201: This Charge has been paid per Reasonable & Customary
     C   54              MOVE      *BLANKS       XPEOMB
     C   54              Z-ADD     RCMRA         XPTOTA
     C  N54              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1
     C*
     C                   END                                                    END RCFSR
     C*
     C                   END                                                    END RCSTAT NE 'I'
     C                   END                                                    END 0 *IN60 - PRC01
     C                   END
     C* ==================================================================
     C* PROCESS THE FEE SCHEDULED CODES HERE
     C*
     C     CARCFL        IFNE      'Y'
     C*
     C*
     C*  Test for Fee Schedule Reimbursement = 'Y'
     C                   MOVEL     DHPRCD        XXCPT4
     C                   MOVE      CAFDAT        XXEFDT
     C*
     C     PRKY          SETGT     PPR01                                         GET PROC CODE
     C     NXTPR3        TAG
     C                   READP     PPR01                                  60     RECORD
     C*
     C*  A hard error will cause the program to terminate ASAP.
     C     *IN60         IFEQ      '1'
     C     PRCPT4        ORNE      DHPRCD
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '98'          QQC(Q)
     C* Err Msg 98: "Invalid Procedure Code, See Programmer"
     C                   Z-ADD     *ZEROS        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1
     C                   END
     C     PRSTAT        CABEQ     'I'           NXTPR3
     C* ==================================================================
     C* PAGE 10-6 #17 (2)
     C* Note:  Do not bill code Y0715 with E0720 or E0730.  For TENS units
     C* approved for rental/purchase prior to March 23, 1994, continue using
     C* the E0720 or E0730 codes, once each month until the purchase price is
     C* paid, not to exceed five months.
     C*
     C     DHPRCD        IFEQ      'E0720'
     C     DHPRCD        OREQ      'E0730'
     C                   MOVE      'Y0715'       XXCPT4
     C*
     C     CAKY9         SETLL     PCA09
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XRCPT4        IFEQ      'Y0715'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AJ'          QQC(Q)
     C* Err Msg AJ: "Service falls within pre-operative policy."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '509'         XPEOMB
     C* EOMB Code 509: "Procedure denied, procedure may not be billed  e"
     C*                 with other tens codes."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1
     C*
     C                   END
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END 0 DOWEQ 60
     C                   END                                                    END GE E0720
     C*
     C* ===============================================================
     C*** IF BY REPORT IS "X" PROCEDURE IS NON-COVERED                 .
     C*
     C     PRRPT         IFEQ      'X'
     C*
     C* EOMB Code 207: "Payment denied, this is a non-covered service".
     C* EOMB Code 326: "Payment denied, this service not covered on this
     C*                 date of service"
     C*
     C                   MOVE      '351'         XPEOMB
     C* EOMB Code 351: Denied code billed indicates treatment of       mary
     C*                condition not allowed.                          mary
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       Get next record
     C*
     C                   END                                                    END RCRPT EQ 'Y'
     C*
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
     C                   GOTO      NXTRC1                                       GET NEXT RECORD
     C*
     C                   ELSE
     C*
     C*  If fee schedule reimbursement equals 'N' But the MRA
     C*    is greater than zero, this CPT4 Code has been assigned a
     C*    specific MRA by the insurance company.
     C     CACHRG        COMP      PRMRA                              60
     C* EOMB Code 202: "Charges exceed maximum reimbursement amount."
     C   60              MOVE      *BLANKS       XPEOMB
      *   CALL API WA900R TO DETERMINE FUND TYPE
      *
     C*
     C     @@TYPE        IFEQ      'SI'
      *
     C                   MOVE      '202'         XPEOMB
     C* EOMB Code 202: "Charges exceed maximum reimbursement amount."
     C*                 reimbursement manual
      *
     C                   ENDIF
     C*****************************************************************
     C   60              Z-ADD     PRMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       GET NEXT RECORD
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
     C* ===============================================================
     C*-- By Report -------------------------------------------------------
     C     PRRPT         IFEQ      'Y'
     C                   MOVE      *BLANKS       XPEOMB
     C*
     C*   If modifier exists check the code through the Modifier Subr
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C* Err Msg 91: "Carrier to set payment on individual case basis
     C                   MOVE      'FZ'          QQC(Q)
     C* Err Msg FZ: "Send to Utiliztion Review"
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C* Err Msg 06: This item is By Report - requires further inspection by
     C*             an adjuster.
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       GET NEXT RECORD
     C*
     C                   END                                                    END PRRPT EQ 'Y'
     C*
     C*--------------------------------------------------------------------
     C*-- ANE/Amount Not Established --------------------------------------
     C     PRRPT         IFEQ      'A'
     C*
     C*   If modifier exists check the code through the Modifier Subr
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD1
     C*
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       GET NEXT RECORD
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
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       GET NEXT RECORD
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
     C                   EXSR      PMOD1
     C*
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
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
     C                   GOTO      NXTRC1                                       GET NEXT RECORD
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
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       GET NEXT RECORD
     C*
     C                   END
     C                   END                                                    END PRRPT EQ 'I'
     C                   END                                                    END PRFSR EQ 'Y'
     C                   END                                                    END CARCFL NE 'Y'
     C*
     *** BEG ADD *** 04/28/98 *****************************************
     C*== Normal Processing of Procedure Code =========================
     C* ==================================================================
     C* PAGE 17-1 ASC GROUP
     C*   ONLY GO THROUGH SELECTION IF SPECIALTY IS ONE OF 5 CODES
     C*   ASC, OSC, OSU, SC, OR VA6
     C*
     C     SVSPCD        IFEQ      'ASC'
     C     SVSPCD        OREQ      'OSC'
     C     SVSPCD        OREQ      'OSU'
     C     SVSPCD        OREQ      'SC '
     C     SVSPCD        OREQ      'VA6'
     C*
     C* ==================================================================
     C*
     C*
     C                   SELECT
     C     PR1AF         WHENEQ    '1'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(1)        SXMRA
     C*
     C     PR1AF         WHENEQ    '2'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(2)        SXMRA
     C*
     C     PR1AF         WHENEQ    '3'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(3)        SXMRA
     C*
     C     PR1AF         WHENEQ    '4'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(4)        SXMRA
     C*
     C     PR1AF         WHENEQ    '5'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(5)        SXMRA
     C*
     C     PR1AF         WHENEQ    '6'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(6)        SXMRA
     C*
     C     PR1AF         WHENEQ    '7'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(7)        SXMRA
     C*
     C     PR1AF         WHENEQ    '8'
     C                   Z-ADD     *ZEROS        SXMRA
     C                   MOVE      ASC(8)        SXMRA
     C                   OTHER
     C*
     C                   MOVE      '379'         XPEOMB
     C* EOMB Code 379: Denied, procedure/servied not elgible for       mary
     C*                reimbursement to ambulatory.                    mary
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   ENDSL
     C                   END                                                          END SVSPCD
     *** BEG ADD *** 04/28/98 *****************************************
     C*
     C* PAGE 4-1 #2
     C* Physician Assistant
     C* Physician assistants are not elgible for direct reimbursement from BWC
     C* Some services, excluding surgery assistant services, provided by a
     C* physician assistant under the supervision of a licensed, enrolled
     C* physician may be reimbursed directly to the physician.
     C*
     C     SVSPCD        IFEQ      'PA '
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FT'          QQC(Q)
     C* Err Msg FT: "Verify specialty of provider for this treatment"
     C*
     C                   MOVE      '332'         XPEOMB
     C*** EOMB 332: "Payment denied, servicing provider is not elgible
     C*             "for reimbursement"
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       GET NEXT RECORD
     C*
     C                   END                                                    END SVSPCD EQ PA
     C*================================================================
     C* If modifier 22, 52, 53, 55, 56, 58, 57, 66 or 99 is billed
     C* with any code, have a message and allow                        rom BWC
     C*
     C     XPPMOD        IFEQ      '22'
     C     XPPMOD        OREQ      '52'
     C     XPPMOD        OREQ      '53'
     C     XPPMOD        OREQ      '55'
     C     XPPMOD        OREQ      '56'
     C     XPPMOD        OREQ      '58'
     C     XPPMOD        OREQ      '66'
     C     XPPMOD        OREQ      '99'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GC'          QQC(Q)
     C* Err Msg GC: "Manual pricing required"
     C                   ENDIF                                                   END EQ 22
     C*================================================================
     C* If modifier 90 is billed with code range from 99201- 99499
     C* have a message and allow                                       rom BWC
     C*
     C     XPPMOD        IFEQ      '90'
     C     DHPRCD        IFGE      '00100'
     C     DHPRCD        ANDLE     '79999'
     C     DHPRCD        ORGE      '99201'
     C     DHPRCD        ANDLE     '99499'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GD'          QQC(Q)
     C* Err Msg GC: "Medical review required"
     C                   ENDIF                                                   END DHPRCD
     C                   ENDIF                                                   END EQ 90
     C*================================================================
     C* If modifier 54 is billed with code range from 10000 - 69999
     C* have a message and allow                                       rom BWC
     C*
     C     XPPMOD        IFEQ      '54'
     C     DHPRCD        IFGE      '10000'
     C     DHPRCD        ANDLE     '69999'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GC'          QQC(Q)
     C* Err Msg GC: "Manual pricing required"
     C*
     C                   ELSE
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GD'          QQC(Q)
     C* Err Msg GD: "Medical review required"
     C*
     C                   ENDIF                                                   END DHPRCD
     C                   ENDIF                                                   END EQ 54
     C*================================================================
     C* Modifier -57 when billed with code from range 99201 - 99499
     C* pay regardless of all other edits else EOMB 169.               thesia
     C*
     C     XPPMOD        IFEQ      '57'
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99499'
     C*
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     CACHRG        XPTOTP
     C*
     C                   ELSE
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '169'         XPEOMB
     C* EOMB Code 169: "Payment is denied as the modifier is not valid
     C*                 with the procedure code billed"
     C*
     C                   END                                                    END DHPRCD
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       GET NEXT RECORD
     C*
     C                   END                                                    END XPPMOD
     C*****************************************************************
     C*
     C*   PHASE 4 Revisions:  If modifier 24 is billed with a code
     C*   equal to 99211 , , 99499 (displayed below) have msg, else deny.
     C*
     C     XPPMOD        IFEQ      '24'
     C     XPPMOD        OREQ      '25'                                         ADD 09/30/97
     C*
     C*   NEED 25 TO PAY ALL 9XXXX RANGE NOT JUST SPECIFIC RANGES LIKE
     C*   MODIFIER -24
     C     XPPMOD        IFEQ      '25'                                         ADD 09/30/97
     C     DHPRCD        ANDGE     '99201'
     C     DHPRCD        ANDLE     '99499'
     C*
     C*
      *   CHANGE TO OR SO IT ONLY APPLIES TO MOD 24
     C     DHPRCD        ORGE      '99211'
     C     DHPRCD        ANDLE     '99239'
     C*
     C     DHPRCD        ORGE      '99281'
     C     DHPRCD        ANDLE     '99292'
     C*
     C     DHPRCD        ORGE      '99296'
     C     DHPRCD        ANDLE     '99313'
     C*
     C     DHPRCD        ORGE      '99331'
     C     DHPRCD        ANDLE     '99333'
     C*
     C     DHPRCD        ORGE      '99351'
     C     DHPRCD        ANDLE     '99499'
     C*
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C* Update charge with payment
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       GET NEXT RECORD
     C*
     C                   ELSE
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '169'         XPEOMB
     C* EOMB Code 169  "Payment is denied as the modifier is not valid with the
     C*                 procedure code billed"
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1
     C                   END                                                    END EQ '99211'
     C                   END                                                    END EQ '24'
     C*
     C*================================================================
     C* If modifiers 76, 77, 78, 79 are billed with codes from ranges
     C* 00100-02020 or 7000-89999 have msg and allow                   rom BWC
     C*
     C     XPPMOD        IFGE      '76'
     C     XPPMOD        ANDLE     '79'
     C*   CHANGED EDIT TO GIVE ERROR MESSAGE FOR EVERYTHING OTHER THAN
     C*   10000 - 69999, 90000-99499
     C     DHPRCD        IFGE      '10000'
     C     DHPRCD        ANDLE     '69999'
     C     DHPRCD        ORGE      '90000'
     C     DHPRCD        ANDLE     '99499'
     C                   GOTO      SKIP
     C                   ELSE
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GD'          QQC(Q)
     C* Err Msg GC: "Medical review required"
     C*
     C                   ENDIF                                                   END DHPRCD
     C     SKIP          TAG                                                    ADD 09/30/97
     C                   ENDIF                                                   END EQ 76
     C*================================================================
     C* If multiple units are billed in one line item for any CPT4 code
     C* or range 70000-99499 in Reasonable and Customary file
     C*
     C     DHUNTS        IFGT      1
     C*
     C*   CHANGED UNIT CALCULATION FOR R&C CODES ONLY PAY 1 UNIT
     C*   WILL PAY MRA X UNITS
     C     CARCFL        IFEQ      'C'
     C*
     C     SXANAM        MULT      DHUNTS        SXMRA
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GE'          QQC(Q)
     C* Err Msg GE: "Multiple units entered, Verify payment."
     C                   ENDIF                                                   END CARCFL
     C                   ENDIF                                                   END DHUNTS
     C*================================================================
     C* If multiple units are billed in one line item for any CPT4 code
     C* From range 00100-99499 only pay one unit
     C*
     C     DHUNTS        IFGT      1
     C*
     C     CARCFL        IFEQ      'Y'
     C     DHPRCD        ANDGE     '10000'
     C     DHPRCD        ANDLE     '99499'
     C*
     C     SXANAM        MULT      1             SXMRA
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GE'          QQC(Q)
     C* Err Msg GE: "Multiple units entered, Verify payment."
     C                   ENDIF                                                   END CARCFL
     C                   ENDIF                                                   END DHUNTS
     C*================================================================
     C* If modifiers -26, -27 is billed, the same date, the opposite
     C* mod will not pay, same fein eomb 525, different fein (523)
     C*
     C                   Z-ADD     0             X
     C     XPPMOD        IFEQ      '26'
     C     XPPMOD        OREQ      '27'
     C*
     C     CAKY9E        SETLL     PCA09
     C     CAKY9E        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
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
     C     XRFEIN        ANDEQ     CAFEIN
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRTOTP        ANDGT     0
     C*
     C     X             IFGE      2
     C     XRPMOD        OREQ      XPPMOD
     C     XRPMOD        ORNE      '26'
     C     XRPMOD        ANDNE     '27'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '525'         XPEOMB
     C*   EOMB 525: "Payment denied, this procedure is mutually exclusive "
     C*             "to another code"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1
     C                   END                                                    END X
     C                   END                                                    END XRCPT4
     C*****************************************************************
     C** DIFFERENT PROVIDER RETURNS DIFFERENT EOMB
     C*
     C     XRCPT4        IFEQ      DHPRCD
     C     XRFDAT        ANDEQ     CAFDAT
     C     XRFEIN        ANDNE     CAFEIN
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRTOTP        ANDGT     0
     C*
     C     X             IFGE      2
     C     XRPMOD        OREQ      XPPMOD
     C     XRPMOD        ORNE      '26'
     C     XRPMOD        ANDNE     '27'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '523'         XPEOMB
     C*   EOMB 525: "Payment denied, this procedure is mutually exclusive "
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1
     C                   END                                                    END X
     C                   END                                                    END XRPMOD
     C*****************************************************************
     C     SXPROF        IFEQ      *ZEROS
     C     SX92N1        ANDEQ     *ZEROS
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '117'         XPEOMB
     C*   EOMB 117: "Payment denied, as the modifier is invalid"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1
     C                   END                                                    END EQ SXPROF
     C*
     C     CAKY9E        READE     PCA09                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C                   END                                                    END XPPMOD
     C*
     C*
     C* PAGE 4-3 #10
     C* You may not bill CPT codes for services other than general anesthesia
     C* with general anesthesia modifiers.  Bills will be denied with E.O.B. 380
     C*
     C     XPPMOD        IFEQ      '23'
     C     XPPMOD        OREQ      '30'
     C     XPPMOD        OREQ      '95'
     C     XPPMOD        OREQ      'QX'
     C     DHPRCD        IFGE      '10040'
     C     DHPRCD        ANDLE     '99499'
     C*
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '380'         XPEOMB
     C*** EOMB 380: ""
     C*
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       GET NEXT RECORD
     C*
     C                   END                                                    END DHPRCD
     C                   END                                                    END XPPMOD
     C*-- Execute all Subroutines here ---------------------------------
     C*
     C     DHPRCD        IFGE      '99201'                                      EVAL/MGMT
     C     DHPRCD        ANDLE     '99499'
     C                   EXSR      EVAL
     C                   ELSE                                                      ELSE EVAL
     C*
     C     DHPRCD        IFGE      '00100'                                      ANESTHESIA
     C     DHPRCD        ANDLE     '02020'
     C                   EXSR      ANES
     C                   ELSE                                                      ELSE ANES
     C*
     C     DHPRCD        IFGE      '10000'                                      SURGERY
     C     DHPRCD        ANDLE     '69979'                                      SURGERY
     C*
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the workfile is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALOH
     C                   CLOSE     FACAPOH
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALOH
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPOH
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURG Codes
     C                   CALL      'CRTFACOH'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALOH
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACALPOH
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFEQ      ' '                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALOH
     C                   OPEN      FACAPOH
     C                   MOVE      '1'           OPNFLG
     C                   END
     C*
     C*     then, reset hold YMD field and provider/mod
     C                   Z-ADD     XXYMD         HDYMD
     C                   Z-ADD     CAFEIN        HDFEIN
     C                   Z-ADD     CAMOD         HDMOD
     C*
     C                   ELSE                                                   ELSE XFIRST
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
     C                   CLOSE     FACALOH
     C                   CLOSE     FACAPOH
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALOH
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACALPOH
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACOH'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALOH
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACALPOH
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALOH
     C                   OPEN      FACAPOH
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
     C                   EXSR      SURG
     C*
     C                   ELSE                                                      ELSE SURG
     C*
     C     DHPRCD        IFGE      '70000'                                      RADIOLOGY
     C     DHPRCD        ANDLE     '79999'
     C     DHPRCD        ORGE      '99065'                                      RADIOLOGY
     C     DHPRCD        ANDLE     '99066'
     C                   EXSR      RADI
     C*
     C                   ELSE                                                      ELSE RADI
     C*
     C     DHPRCD        IFGE      '80000'                                      PATHOLOGY
     C     DHPRCD        ANDLE     '89999'
     C     DHPRCD        ORGE      '99077'                                      PATHOLOGY
     C     DHPRCD        ANDLE     '99023'
     C     DHPRCD        OREQ      '99026'
     C                   EXSR      PATH
     C*
     C                   ELSE                                                      ELSE PATH
     C*
     C     DHPRCD        IFGE      '90700'                                      MEDICAL
     C     DHPRCD        ANDLE     '96999'
     C     DHPRCD        ORGE      '98000'
     C     DHPRCD        ANDLE     '99002'
     C     DHPRCD        ORGE      '99024'
     C     DHPRCD        ANDLE     '99025'
     C     DHPRCD        ORGE      '99027'
     C     DHPRCD        ANDLE     '99064'
     C     DHPRCD        ORGE      '99069'
     C     DHPRCD        ANDLE     '99199'
     C                   EXSR      MEDI
     C*
     C                   ELSE                                                      ELSE MEDI
     C*
     C     DHPRCD        IFGE      '97000'                                      PHYSICAL MEDICINE
     C     DHPRCD        ANDLE     '97999'
     C                   EXSR      PHYS
     C*
     C                   ELSE                                                      ELSE PHYS
     C*
     C                   EXSR      OTHE                                         OTHER CODES
     C*
     C                   END                                                       END PHYS
     C                   END                                                       END MEDI
     C                   END                                                       END PATH
     C                   END                                                       END RADI
     C                   END                                                       END SURG
     C                   END                                                       END ANES
     C                   END                                                       END EVAL
     C*
     C*
     C*
     C                   ELSE                                                   OVERRIDE TYPE
     C*
     C* ==================================================================
     C*
     C     CAKY1A        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C*** IF OVERRIDE TYPE WE MUST STILL OUTPUT THE ALLOWED/DISALLOWED ETC.
     C     CAMRA         IFEQ      0
     C     CAOTHR        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CATOTA        ANDEQ     0
     C     CAPAID        ANDEQ     0
     C*
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
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     SXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA01
     C*
     C                   END                                                    END 0 *IN54 - PCA01
     C                   END                                                    END OVERRIDE TYPE
     C                   END                                                    CAFDAT IFGE
     C*
     C     NXTRC1        TAG
     C*
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
     C     CAKYH1        READE     PCA01                                  54      GET NEXT RCD
     C*                                                      TO PROCESS
     C*
     C                   ENDDO                                                  END 0 DOWEQ *IN54
     C*
     C*
     C*** Execute SURG2 if a SURG Code has been processed
     C     OPNFLG        IFEQ      '1'
     C                   EXSR      SURG2                                        Process the SURG cod
     C                   END                                                    END OPNFLG EQ '1'
     C*
     C*  END THE PROGRAM
     C     TERM          TAG
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALOH
     C                   CLOSE     FACAPOH
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C** RELEASE ALL THE FORMATS FOR A SAVE RETURN.
     C*
     C     DUMMY         CHAIN     PCA01                              54
     C  N54              EXCEPT    ECA01
     C*
     C* Write adjudication messages to FDMGP
     C                   EXSR      MSG
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     0             SXMRA
     C*** END ADD *** 09/04/98 ****************************************
     C                   RETURN
     C                   MOVE      '1'           *INLR
     *********************************************************************
     *** Anesthesia     Codes: 00100 - 02020                     A N E S *
     *********************************************************************
     C     ANES          BEGSR
     C*
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per ANES, pg. 2-4
     C     XPPMOD        IFNE      '23'
     C     XPPMOD        ANDNE     '30'
     C     XPPMOD        ANDNE     '95'
     C     XPPMOD        ANDNE     'QX'
     C*
     C                   Z-ADD     1             Q
     C     '0Z000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '165'         XPEOMB
     C* EOMB Code 165: "Payment is denied as the modifier is invalid for
     C*                 Anesthesia services"
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C                   ENDIF                                                  END NE -23
     C                   ELSE                                                   ELSE NE P1...
     C*
     C                   Z-ADD     1             Q
     C     '0Z000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '030'         XPEOMB
     C* EOMB Code 030: " PAYMENT DENIED, NO MODIFIER WAS BILLED WITH   r
     C*                  THIS ANESTHESIA PROCEDURE CODE"
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C* ELSE IF CORRECT MODIFIER
     C                   END                                                    END NE *BLANK
     C                   EXSR      PMOD
     C*
     C*=====================================================================
     C*  If ANES = BR
     C*  If anesthesia amount from the FMPRP file is zero, then give
     C*  error message that this anesthesia is BR or not established
     C     SXANAM        IFLE      *ZEROS
     C     SX1A2         OREQ      'B'
     C     SX1A2         OREQ      'A'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '57'          QQC(Q)
     C* Err Msg 57: "Anesthesia amount is either BR or not established
     C*             - Please review"
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     CACHRG        XPTOTD
     C                   MOVE      *BLANKS       XPEOMB
     C                   END                                                    END LE *ZERO
     C* ===============================================================
     C* PAGE 2-6, #1
     C* Chiropractic means utilization of the relationship between the muscular
     C* skeletal structures of the body, the spinal column ans the nervous syste
     C* in the restoration and maintenance of health.
     C*
     C     SVSPCD        IFEQ      'CHI'
     C     DHPRCD        ANDGE     '00100'
     C     DHPRCD        ANDLE     '02020'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BP'          QQC(Q)
     C* Err Msg BP: "Specialty code and level of service don't match'
     C*             "please verify"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '332'         XPEOMB
     C* EOMB Code 332: "Procedure denied, procedure may not be billed  e"
     C*                 with other tens codes.                         e"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C                   END                                                    END SVSPCD
     C*                                                                A
     C*                                                                A
     C*================================================================
     C*  If any modifier in Anesthesia code range is billed with anothe
     C*  modifier, same date same code, provider Disallow EOMB 525
     C*  else if same date, same code, different provieder eomb 523
     C*
     C                   Z-ADD     0             X
     C     DHPRCD        IFGE      '00100'
     C     DHPRCD        ANDLE     '02020'
     C*
     C                   MOVEL     DHPRCD        XXCPT4
     C*
     C     CAKY9E        CHAIN     PCA09                              60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRPMOD        IFEQ      '95'
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRTOTP        ANDGT     0
     C     XRPMOD        OREQ      'QX'
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRTOTP        ANDGT     0
     C                   ADD       1             X
     C                   ENDIF                                                  END XRPMOD
     C*****************************************************************
     C     XRCPT4        IFEQ      DHPRCD
     C     XRFEIN        ANDEQ     CAFEIN
     C     XRFDAT        ANDEQ     CAFDAT
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRTOTP        ANDGT     0
     C*
     C     XRPMOD        IFEQ      '95'
     C     XRPMOD        OREQ      'QX'
     C     XPPMOD        IFEQ      'QX'
     C     X             ANDLT     2
     C     XPPMOD        OREQ      '95'
     C     X             ANDLT     2
     C                   GOTO      NEXT
     C                   ELSE
     C                   GOTO      ERR1
     C                   ENDIF
     C                   ENDIF
     C*
     C     XRPMOD        IFNE      XPPMOD
     C*
     C     ERR1          TAG
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '525'         XPEOMB
     C*   EOMB 525: "Payment denied, this procedure is mutually exclusive "
     C*             "to another code"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C     NEXT          TAG
     C                   END                                                    END XATOTP  ...
     C                   END                                                    END NE CPT4 ...
     C*****************************************************************
     C     XRCPT4        IFEQ      DHPRCD
     C     XRFEIN        ANDNE     CAFEIN
     C     XRFDAT        ANDEQ     CAFDAT
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRTOTP        ANDGT     0
     C*
     C     XRPMOD        IFEQ      '95'
     C     XRPMOD        OREQ      'QX'
     C     XPPMOD        IFEQ      'QX'
     C     X             ANDLT     2
     C     XPPMOD        OREQ      '95'
     C     X             ANDLT     2
     C                   GOTO      NEXT1
     C                   ELSE
     C                   GOTO      ERR2
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C     XRPMOD        IFNE      XPPMOD
     C*
     C     ERR2          TAG
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '523'         XPEOMB
     C* EOMB Code 523: "PAYMENT FOR SERVICE HAS BEEN MADE TO A DIFFERENT"
     C*                PROVIDER."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C     NEXT1         TAG
     C                   END                                                    END XATOTP  ...
     C                   END                                                    END NE CPT4 ...
     C*****************************************************************
     C     CAKY9E        READE     PCA09                                  60
     C                   END
     C*
     C                   END                                                    END *IN60
     C*
     C* The Time Units for all procedures is computed by allowing 1 unit
     C* for each 15 minutes of anesthesia time.  In addition each fraction
     C* of a quarter hour will be reimbursed at 1 unit.
     C*
     C*  1.0 Unit for each 15 minutes or any fraction thereof greater than 5 min
     C     DHUNTS        DIV       15            HLANMN
     C** Round up all fractions of 15 minutes to an additional Time Unit
     C                   MVR                     HLADDT
     C                   ADD       HLANMN        HLADDT
     C                   Z-ADD     HLANMN        HLANTV
     C     HLADDT        IFGT      *ZEROS
     C                   ADD       1             HLANTV
     C                   END
     C*
     C** Basic Value + Time Units = Total Anesthesia Value
     C     SXANAM        ADD       HLANTV        XXWORK
     C*
     C** BASED UPON GEO ZIP FINE CORRECT ANE CONVERSION FACTOR
     C                   Z-ADD     1             T
     C                   Z-ADD     37            XXANES
     C*
     C** Total Anesthesia Value * Conversion Factor = Fee
     C     XXWORK        MULT(H)   XXANES        SXMRA
     C*
     C** Modifier calculations
     C* ===============================================================
     C* Page 4-3 #4
     C* General Anesthesia modifiers -23, -30, -95, or -QX must
     C* accompany the appropriate anesthesia CPT code
     C*
     C     XPPMOD        IFEQ      '23'
     C     XPPMOD        OREQ      '30'
     C     SXMRA         MULT      1.0           SXMRA
     C*
     C     XPPMOD        IFEQ      '23'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GC'          QQC(Q)
     C* Err Msg GC: "Manual pricing required"
     C                   ENDIF
     C*
     C                   ELSE
     C     XPPMOD        IFEQ      '95'
     C     XPPMOD        OREQ      'QX'
     C     SXMRA         MULT      .5            SXMRA
     C                   ELSE
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '165'         XPEOMB
     C* EOMB Code 165: "Payment is denied as the modifier is invalid
     C*                 for anesthesia services"
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C*
     C                   ENDIF                                                  END EQ -30
     C                   ENDIF                                                  END EQ -95
     C*  EDIT NEEDS TO BE MOVED AFTER CALCULATIONS -- CHECKS AGAINST MRA
     C*================================================================
     C*  PAGE 4-3 #4D,3
     C*  If multiple procedures are performed, bill only the anesthesia
     C*  CPT code of the primary procedure.
     C*
     C     DHPRCD        IFGE      '00100'
     C     DHPRCD        ANDLE     '02020'
     C                   MOVEL     '00100'       XXCPT4
     C     CAKY9         SETLL     PCA09                              60
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '00100'
     C     XRCPT4        ANDLE     '02020'
     C     XRPMOD        ANDNE     '95'
     C     XRPMOD        ANDNE     'QX'
     C     XRTOTP        IFGE      SXMRA
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
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '518'         XPEOMB
     C* EOMB Code 518: "Payment denied, This service is considered to be"
     C*                 part of the global fee."
     C*
     C*
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C                   END                                                    END XATOTP  ...
     C                   END                                                    END NE CPT4 ...
     C     CAKY9A        READE     PCA09                                  60
     C                   END
     C*
     C                   END                                                    END *IN60
     C*
     C*
     C*=== Calculating Time Units =====================================
     C* PAY NOTHING
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
     C     ANESE         ENDSR
     *********************************************************************
     *** UPDATE FADEP WITH EOMB                                E O M B   *
     *********************************************************************
     C     EOMB          BEGSR
     C*
     C* Find the EOMB description
     C                   MOVEL(P)  XPEOMB        EBKEY
     C*
     C     EBKY          CHAIN     PEB01OH                            60
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
     *** Evaluation/Management      Codes: 99201 - 99499         E V A L *
     *********************************************************************
     C     EVAL          BEGSR
     C*
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per EVAL, pg. 5-6
     C     XPPMOD        IFNE      '21'
     C     XPPMOD        ANDNE     '22'
     C     XPPMOD        ANDNE     '24'
     C     XPPMOD        ANDNE     '25'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '27'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '53'
     C     XPPMOD        ANDNE     '54'
     C     XPPMOD        ANDNE     '55'
     C     XPPMOD        ANDNE     '56'
     C     XPPMOD        ANDNE     '57'
     C     XPPMOD        ANDNE     '58'
     C     XPPMOD        ANDNE     '59'
     C     XPPMOD        ANDNE     '66'
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
     C                   MOVE      '169'         XPEOMB
     C* EOMB Code 169: "Payment is denied as the modifier is not valid
     C*                 with the procedure code billed"
     C                   EXSR      UPDAT
     C                   GOTO      EVALE
     C*
     C                   END                                                    END NE 21
     C                   END                                                    END NE *BLANK
     C*   PHASE 3 Revisions:  If modifier 25 is billed with a code from
     C*   range 99201-99499, disregard all edits.
     C*
     C     XPPMOD        IFEQ      '25'
     C*
     C* Calculate Payment
     C     CACHRG        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C* Update charge with payment
     C                   EXSR      UPDAT
     C                   GOTO      EVALE
     C                   ENDIF
     C*
     C*   CALL SUBROUTINE PMOD FOR MOD -26, -27
     C*
     C                   EXSR      PMOD
     C*
     C*
     C*
     C*   PHASE 4 Revisions:  If modifier 21 is billed with a code
     C*   equal to 99205 , , 99412 (displayed below) have msg, else deny.
     C*
     C     XPPMOD        IFEQ      '21'
     C*
     C     DHPRCD        IFEQ      '99205'
     C     DHPRCD        OREQ      '99215'
     C     DHPRCD        OREQ      '99220'
     C     DHPRCD        OREQ      '99223'
     C     DHPRCD        OREQ      '99233'
     C     DHPRCD        OREQ      '99239'
     C     DHPRCD        OREQ      '99245'
     C     DHPRCD        OREQ      '99255'
     C     DHPRCD        OREQ      '99263'
     C     DHPRCD        OREQ      '99275'
     C     DHPRCD        OREQ      '99295'
     C     DHPRCD        OREQ      '99296'
     C     DHPRCD        OREQ      '99303'
     C     DHPRCD        OREQ      '99313'
     C     DHPRCD        OREQ      '99323'
     C     DHPRCD        OREQ      '99333'
     C     DHPRCD        OREQ      '99353'
     C     DHPRCD        ORGE      '99381'
     C     DHPRCD        ANDLE     '99387'
     C     DHPRCD        ORGE      '99391'
     C     DHPRCD        ANDLE     '99397'
     C     DHPRCD        OREQ      '99404'
     C     DHPRCD        OREQ      '99412'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GC'          QQC(Q)
     C* Err Msg GC: "Manual pricing required."
     C                   ELSE
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '169'         XPEOMB
     C* EOMB Code 169  "Payment is denied as the modifier is not valid with the
     C*                 procedure code billed"
     C                   EXSR      UPDAT
     C                   GOTO      EVALE
     C                   END                                                    END EQ '99205'
     C                   END                                                    END EQ '21'
     C*
     C* ===============================================================
     C* SAME DAY
     C* IF AN OFFICE OR HOSPITAL VISIT 99201-99215, 99221-99233 IS PERFORMED
     C* VERIFY THAT IT IS NOT IN THE SURGERY PRE OPERATIVE PERIOD
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99215'
     C     DHPRCD        ORGE      '99221'
     C     DHPRCD        ANDLE     '99233'
     C                   MOVE      '10000'       XXCPT4
     C     CAKY9         SETLL     PCA09
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XRCPT4        IFGE      '10000'
     C     XRCPT4        ANDLE     '69999'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AJ'          QQC(Q)
     C* Err Msg AJ: "Service falls within pre-operative policy."
     C                   END
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END 0 DOWEQ 60
     C                   END                                                    END GE '90000'
     C*
     C* ==================================================================
     C* DAY BEFORE
     C* IF AN OFFICE OR HOSPITAL VISIT 99201-99215, 99221-99233 IS PERFORMED
     C* VERIFY THAT IT IS NOT IN THE SURGERY PRE OPERATIVE PERIOD
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99215'
     C     DHPRCD        ORGE      '99221'
     C     DHPRCD        ANDLE     '99233'
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C                   Z-ADD     1             SRFUTR            5 0
     C                   MOVE      ' '           SRRTCD            1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLDDAT
     C                   MOVE      '10000'       XXCPT4
     C     CAKY9B        SETLL     PCA09
     C*
     C     CAKY9C        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XRCPT4        IFGE      '10000'
     C     XRCPT4        ANDLE     '69999'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AJ'          QQC(Q)
     C* Err Msg AJ: "Service falls within pre-operative policy."
     C                   END
     C*
     C     CAKY9C        READE     PCA09                                  60
     C*
     C                   END                                                    END 0 DOWEQ 60
     C                   END                                                    END GE '90000'
     C*
     C* =================================================================
     C* The number in that column (FU) establishes the days during which no
     C* additional reimbursement is allowed for the usual care provided
     C* following surgery, absent complications or unusual circumstances.
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99499'
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
     C                   MOVE      '104'         XPEOMB
     C* EOMB Code 104: "PAYMENT DENIED, OFFICE/HOSPITAL VISIT FALLS"
     C*                WITHIN SURGICAL FOLLOW-UP"
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '09'          QQC(Q)
     C*** Err Msg 09: "This visit occurred during another procedures
     C***              follow-up period"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      EVALE
     C*
     C                   END
     C*
     C                   ADD       1             U
     C*
     C                   END
     C*
     C                   END
     C* =================================================================
     C* 2 - 24 #C
     C* The number in that column (FU) establishes the days during which no
     C* additional reimbursement is allowed for the usual care provided
     C* following surgery, absent complications or unusual circumstances.
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ORLE      '99499'
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
     C                   MOVE      '104'         XPEOMB
     C* EOMB Code 104: "PAYMENT DENIED, OFFICE/HOSPITAL VISIT FALLS"
     C*                WITHIN SURGICAL FOLLOW-UP"
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '09'          QQC(Q)
     C*** Err Msg 09: "This visit occurred during another procedures
     C***              follow-up period"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      EVALE
     C*
     C                   END
     C*
     C                   ADD       1             U
     C*
     C                   END
     C*
     C                   END
     C*
     C* =================================================================
     C* PAGE 3-5  #7,8
     C* If code 98925 - 98929 is billed with any code from range: 99201 - 99499,
     C* disallow 99201-99499.  If modifier -25 is billed on the 99201-99499,
     C* have msg to user, verify medical necessity prior to payment.
     C* For #8 check for 98940-98943
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99499'
     C*
     C                   MOVE      '98925'       XXCPT4
     C*
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '98925'
     C     XRCPT4        ANDLE     '98929'
     C     XRCPT4        ORGE      '98940'
     C     XRCPT4        ANDLE     '98943'
     C     XRTOTP        IFGT      0
     C     XRFDAT        ANDEQ     CAFDAT
     C*
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '764'         XPEOMB
     C*
     C                   EXSR      UPDAT
     C                   GOTO      EVALE
     C*
     C                   END
     C*
     C                   END
     C*
     C     CAKY9A        READE     PCA09                                  60
     C                   ENDDO
     C                   ENDIF                                                  END EQ 99201
     C*
     C* =================================================================
     C* 8 - 2 #D
     C* NON-COVERED SERVICES AND CODES
     C* Nursing homes may not bill BWC or the Self-Insuring Employer using
     C* physician service CPT codes 99301 through 99313.
     C*
     C     DHPRCD        IFGE      '99301'
     C     DHPRCD        ORLE      '99313'
     C*
     C*
     C*  CHANGED EDIT TO CHECK SPECIALTY FOR CUS, SNF INSTAEAD OF POS
     C*
     C     SVSPCD        IFEQ      'CUS'
     C     SVSPCD        OREQ      'SNF'
     C*
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '766'         XPEOMB
     C* EOMB Code 766: "PAYMENT DENIED AS THIS PROCEDURE IS MUTUALLY   "
     C*                EXCLUSIVE TO ANOTHER"
     C*
     C* FIX THIS CHANGE TO EOB# SPEC OF PROV 332  AND REMOVE ERR MSG
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FZ'          QQC(Q)
     C* Err Msg FZ: "Send to Utiliztion Review"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      EVALE
     C*
     C                   END                                                    END CAIPOP
     C*
     C                   END                                                    END DHPRCD
     C     DHPRCD        IFLE      '99275'                                      ADD 07/16/97
     C     DHPRCD        ANDGE     '99241'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C* Err Msg FP: "Physician of Record may only be reimbursed for"
     C*
     C                   MOVE      'GA'          QQC(Q)
     C* Err Msg GA: "Consultation must be ordered by the physician of
     C*             "record"
     C*** BEG DEL *** 03/26/99 ****************************************
     C***************      Z-ADDCACHRG    XPTOTD
     C***************      Z-ADD*ZERO     XPTOTA
     C***************      Z-ADD*ZERO     XPTOTP
     C***************      MOVE '327'     XPEOMB
     C* EOMB Code 327: "Payment denied, medical document not adequate  e"
     C***************   to justify."
     C***************
     C***************      EXSR UPDAT
     C***************      GOTO EVALE
     C*** END DEL *** 03/26/99 ****************************************
     C*
     C                   ENDIF                                                  END DHPRCD /SVSPCD
     C*
     C*
     C* ===============================================================
     C* PAGE 2-24 #B
     C* Self-Insuring Employers
     C* Physicians should continue to obtain prior approval from self-
     C* insuring employers for both inpatient and outpatient hospital services.
     C*
     C     SXSUPP        IFNE      'Y'
     C*
     C     DHPRCD        IFEQ      '99070'
     C                   MOVE      '10000'       XXCPT4
     C*
     C     CAKY9         SETLL     PCA09
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XRCPT4        IFGE      '10000'
     C     XRCPT4        ANDLE     '69999'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AJ'          QQC(Q)
     C* Err Msg AJ: "Service falls within pre-operative policy."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '518'         XPEOMB
     C* EOMB Code 518: "Payment denied, This service is considered to be"
     C*                 part of the global fee."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      EVALE
     C*
     C                   END
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END 0 DOWEQ 60
     C                   END                                                    END GE '90000'
     C                   END                                                    END PRSUPP EQ N
     C* ===============================================================
     C*
     C* PAGE 2-24 #C
     C* Global surgery
     C* BWC's global surgery policy only applies to non-starred surgical
     C* procedures.  Pre-operative visits beginning with the day before
     C* a major surgery.  Payment may be considered.
     C*
     C*   Reasonable and customary does not have Asterisk flag with
     C*   this update
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99215'
     C     DHPRCD        ORGE      '99221'
     C     DHPRCD        ANDLE     '99233'
     C                   MOVE      '00100'       XXCPT4
     C*
     C*  NEED TO LOOK FOR ONE DAY PRIOR TO SURGERY CODES NOT HOSPITAL
     C*  VISITS.  NEED TO MOVE AHEAD ONE DAY.
     C* FIND DATE 1 DAY FUTURE
     C                   Z-ADD     1             SRFUT
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C                   CALL      'SRFUTR8'
     C                   PARM                    WKFDAT
     C                   PARM                    SRFUT
     C                   PARM                    SRTERM
     C                   Z-ADD     WKFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFUTR            7 0
     C* FIND DATE 1 DAY PRIOR
     C                   Z-ADD     1             SRPST
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT            8 0
     C                   Z-ADD     WKFDAT        SRDTE
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C*
     C                   Z-ADD     SRDTE         XXPST8
     C                   Z-ADD     XXPST8        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPAST
     C*
     C     CAKYC         SETLL     PCA0C
     C*
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XCCPT4        IFGE      '10000'
     C     XCCPT4        ANDLE     '69999'
     C     XCFDAT        ANDGE     CAFDAT
     C     XCFDAT        ANDLE     XXFUTR
      *
     C     XCCPT4        ORGE      '00100'
     C     XCCPT4        ANDLE     '02020'
     C     XCFDAT        ANDGE     XXPAST
     C     XCFDAT        ANDLE     XXFUTR
     C*
     C*
     C                   MOVEL     XCCPT4        XXCPT4
     C*
     C     RCKEY         SETGT     PRC01
     C                   READP     PRC01                                  60
     C     *IN60         IFEQ      '0'
     C*
     C     RCCPT4        IFGE      '10000'
     C     RCCPT4        ANDLE     '69999'
     C     RCFOLU        ANDGT     *ZERO
      *
     C     RCCPT4        ORGE      '00100'
     C     RCCPT4        ANDLE     '02020'
     C*
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AJ'          QQC(Q)
     C* Err Msg AJ: "Service falls within pre-operative policy."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '518'         XPEOMB
     C* EOMB Code 518: "Payment denied, This service is considered to be"
     C*                 part of the global fee."
     C* EOMB Code 104: "PAYMENT DENIED, OFFICE/HOSPITAL VISIT FALLS"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      EVALE
     C*
     C                   END                                                      END RCCPT4
     C                   END                                                      END *IN60
     C                   END                                                      END XXCPT4
     C*
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C                   END                                                    END 0 DOWEQ 60
     C                   END                                                    END GE '90000'
     C* PAGE 4-3  #4
     C* Reimbursement
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99499'
     C                   MOVE      '99201'       XXCPT4
     C*
     C     CAKYC         SETLL     PCA0C
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XCCPT4        IFGE      '99201'
     C     XCCPT4        ANDLE     '99499'
     C     XCFDAT        ANDEQ     CAFDAT
     C     XCTOTP        ANDGT     SXMRA
     C     XCDSEQ        ANDNE     CADSEQ
     C*
     C     XCCPT4        OREQ      DHPRCD
     C     XCCPT4        ANDGE     '99201'
     C     XCCPT4        ANDLE     '99205'
     C     XCTOTP        ANDGT     0
     C     XCDSEQ        ANDNE     CADSEQ
     C*
     C                   MOVE      '482'         XPEOMB
     C* EOMB Code 482: "Payment is denied as this service has already"
     C*                "been reimbursed the maximum number of times allowed"
     C*
     C*** BEG ADD *** 07/07/98 ****************************************
     C                   Z-ADD     0             XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*** END ADD *** 07/07/98 ****************************************
     C*
     C***********          MOVE '481'     XPEOMB
     C*** EOMB 481: "Payment is denied as this service has already "
     C***********   "been reimbursed the maximum number of times allowed"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      EVALE
     C*
     C                   END
     C*
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C                   END                                                    END 0 DOWEQ 60
     C                   END                                                    END GE '90000'
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
     C     EVALE         ENDSR
     *********************************************************************
     *** LOAD FOLLOW UP ARRAYS SUBROUTINE                      L O D F U *
     *********************************************************************
     C     LODFU         BEGSR
     C*
     C*** LOAD ARRRAYS WITH FOLLOW UP DAYS
     C*
     C                   Z-ADD     *ZEROS        FUF
     C                   Z-ADD     *ZEROS        FUT
     C                   Z-ADD     1             U                 5 0
     C*
     C     CAKYA         CHAIN     PCA0A                              60
     C     *IN60         DOWEQ     '0'
     C     U             ANDLE     500
     C*
     C                   MOVE      XAZIPA        XXZIPA
     C                   MOVE      XACPT4        XXCPT4
     C                   MOVE      XAFDAT        XXEFDT
     C     RCKEY         SETGT     PRC01
     C                   READP     PRC01                                  61
     C     *IN61         IFEQ      '0'
     C     CACO#         ANDEQ     RCCO#
     C     XXZIPA        ANDEQ     RCZIPA
     C     XXCPT4        ANDEQ     RCCPT4
     C     RCFOLU        ANDGT     *ZERO
     C                   Z-ADD     XAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C                   Z-ADD     RCFOLU        SRFUTR            5 0
     C                   MOVE      ' '           SRRTCD            1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLDDAT
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
     *** Medical                    Codes: 90700 - 96999         M E D I *
     ***                                   98000 - 99002                 *
     ***                                   99024 - 99025                 *
     ***                                   99027 - 99064                 *
     ***                                   99069 - 99199                 *
     *********************************************************************
     C     MEDI          BEGSR
     C*
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per MEDI, pg. 2-3
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '27'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '53'
     C     XPPMOD        ANDNE     '55'
     C     XPPMOD        ANDNE     '56'
     C     XPPMOD        ANDNE     '57'
     C     XPPMOD        ANDNE     '58'
     C     XPPMOD        ANDNE     '59'
     C     XPPMOD        ANDNE     '66'
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
     C                   MOVE      '169'         XPEOMB
     C* EOMB Code 169  "Payment is denied as the modifier is not valid with the
     C*                 procedure code billed"
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END                                                    END EQ 22
     C*
     C                   END                                                    END NE *BLANK
     C*
     C*   CALL SUBROUTINE PMOD FOR MOD -26, -27
     C*
     C                   EXSR      PMOD
     C*
     C* ===============================================================
     C* PAGE 2-10, #21      Error message
     C* Doppler (venous) examinations:
     C* The examination will not be rimbursed unless the injured worker
     C* has a circulatory condition allowed in the claim and the service
     C* is medically necessary.
     C* ===============================================================
     C* PAGE 2-10, #22
     C* Electromyohram:  needle electromyography testing and nerve conduction
     C* testing.   Electromyography testing shall be reimbursed only when
     C* provided by a licensed physician within his/hers cope of practive.
     C*
     C     SVSPCD        IFEQ      'CHI'
     C     DHPRCD        IFGE      '95860'
     C     DHPRCD        ANDLE     '95869'
     C     DHPRCD        ORGE      '95900'
     C     DHPRCD        ANDLE     '95904'
     C     DHPRCD        OREQ      '95872'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FT'          QQC(Q)
     C* Err Msg FT: "Verify specialty of provider for this treatment"  rify'
     C                   ENDIF                                                   END DHPRCD
     C*
     C                   ENDIF                                                   END DHPRCD
     C* ===============================================================
     C* IF XXPASS = 1ST TIME DON'T PAY, ONLY DO THIS RANGE OF CODE ONCE
     C     DHPRCD        IFEQ      '90782'
     C     XXPASS        IFEQ      '1'
     C                   Z-ADD     *ZEROS        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C                   ENDIF                                                  IF EQ 1
     C                   ENDIF                                                  IF DHPRCD
     C* ===============================================================
     C* PAGE 2-14, #2
     C* Injections:  For all other injections in this group, a total of
     C* twelve injections, per injured worker, may be reimburesed before
     C* prior authorization is required from BWC.
     C*
     C                   CLEAR                   DATE
     C                   Z-ADD     *ZEROS        COUNT
     C                   Z-ADD     *ZEROS        X                 2 0
     C     DHPRCD        IFEQ      '90782'
     C*
     C                   MOVE      '20550'       XXCPT4
     C*
     C     CAKYC         SETLL     PCA0C
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        IFEQ      '20550'
     C     XCCPT4        ORGE      '20600'
     C     XCCPT4        ANDLE     '20610'
     C     XCCPT4        ORGE      '64400'
     C     XCCPT4        ANDLE     '64530'
     C     XCCPT4        OREQ      '90782'
     C     XCTOTP        IFGT      0
     C*
     C     XCFDAT        LOOKUP    DATE                                   61
     C     *IN61         IFEQ      '0'                                          IFNOT THERE ADD
     C     COUNT         ANDLT     15
     C                   ADD       1             X
     C                   MOVEL     XCFDAT        DATE(X)
     C*
     C                   ADD       1             COUNT
     C                   ENDIF                                                  END XRTOTP
     C                   ENDIF                                                  END *IN61
     C                   ENDIF                                                  END XRCPT4
     C*
     C     CAKYC2        READE     PCA0C                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C     COUNT         IFGE      12
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FR'          QQC(Q)
     C* Err Msg FR: "12 visits paid, Authorization required"
     C                   ENDIF                                                  END COUNT GT 13
     C*
     C                   ENDIF                                                  END DHPRCD
     C*
     C*
     C*** BEGIN ADD *** 08/15/97 **************************************
     C* PHASE 4 -- Only one of hte following codes can be paid per
     C* date, per provider.  Pay the highest value. disallow EOB 764
     C* range 98925-98929, 98940 - 98943.
     C*
     C     DHPRCD        IFGE      '98925'
     C     DHPRCD        ANDLE     '98929'
     C*
     C     DHPRCD        ORGE      '98940'
     C     DHPRCD        ANDLE     '98943'
     C*
     C                   MOVE      '98925'       XXCPT4
     C*
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*****************************************************************
     C*
     C     DHPRCD        IFGE      '98925'
     C     DHPRCD        ANDLE     '98929'
     C*
     C     XRCPT4        ANDGE     '98925'
     C     XRCPT4        ANDLE     '98929'
     C     XRFDAT        ANDEQ     CAFDAT
     C     XRTOTP        ANDGT     SXMRA
     C     XRDSEQ        ANDNE     CADSEQ
     C*
     C                   MOVE      '764'         XPEOMB
     C*   EOMB 764: "Payment denied, this procedure code is part of
     C*             "another service"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END
     C*****************************************************************
     C*
     C     DHPRCD        IFGE      '98940'
     C     DHPRCD        ANDLE     '98943'
     C*
     C     XRCPT4        ANDGE     '98940'
     C     XRCPT4        ANDLE     '98943'
     C*
     C     XRFDAT        ANDEQ     CAFDAT
     C     XRTOTP        ANDGT     SXMRA
     C     XRDSEQ        ANDNE     CADSEQ
     C*
     C                   MOVE      '764'         XPEOMB
     C*   EOMB 764: "Payment denied, this procedure code is part of
     C*             "another service"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      MEDIE
     C*
     C                   END
     C*****************************************************************
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END 0 DOWEQ 60
     C                   END                                                    END GE '90000'
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
     *** HANDLE ALL OTHER CODES   XXXXX-XXXXX                    O T H E *
     *********************************************************************
     C     OTHE          BEGSR
     C*
     C* ===============================================================
     C* IF PROCEDURE CODE MODIFIER IS PRESENT SEND MESSAGE
     C*
     C*
     C     XPPMOD        IFNE      *BLANKS
     C*
     C     XPPMOD        IFEQ      'RT'
     C                   EXSR      PMOD1
     C                   END                                                    END XPPMOD EQ 'RT'
     C*
     C     XPPMOD        IFEQ      '62'
     C     XPPMOD        OREQ      '80'
     C     XPPMOD        OREQ      '81'
     C     XPPMOD        OREQ      '82'
     C*
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '169'         XPEOMB
     C* EOMB Code 169  "Payment is denied as the modifier is not valid with the
     C*                 procedure code billed"
     C                   EXSR      UPDAT
     C                   GOTO      OTHEE
     C                   END                                                    END XPPMOD EQ 62
     C*
     C                   END                                                    END XPPMOD NE *BLANK
     C*
     C* ===============================================================
     C* WE MUST CHECK TO SEE IF THIS CODE IS A RENTAL/HOURLY/MILEAGE CODE THAT
     C* IS BY REPORT
     C*
     C     XPPMOD        IFEQ      'RT'
     C*
     C     SX1A2         IFEQ      'B'
     C     SX1A2         OREQ      'I'
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
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OTHEE                                        GET NEXT RECORD
     C*
     C                   END                                                    END SX1A2 EQ 'B'
     C*
     C                   Z-ADD     SXANAM        SXMRA
     C                   END                                                    END XPPMOD EQ 'RT'
     C*
     C* ===============================================================
     C* WE MUST CHECK TO SEE IF THIS CODE IS A HOURLY/MILEAGE CODE
     C*
     C     SX1A2         IFEQ      'H'
     C     SX1A2         OREQ      'M'
     C     DHUNTS        IFGT      *ZEROS
     C     DHUNTS        MULT      SXANAM        SXMRA
     C                   ELSE                                                   ELSE DHUNTS
     C                   Z-ADD     SXANAM        SXMRA
     C                   END                                                    END SXANAM GT *ZEROS
     C                   END                                                    END SX1A2
     C*
     C* ===============================================================
     C* PER PROVIDER BULLETIN 1991 #91-15 A4557 ONLY ALLOWED TWICE PER YEAR
     C*
     C     DHPRCD        IFEQ      'A4557'
     C*
     C                   Z-ADD     *ZEROS        COUNT
     C                   MOVE      'A4557'       XXCPT4
     C     CAKYC         SETLL     PCA0C
     C     CAKYC2        READE     PCA0C                                  60
     C     *IN60         IFEQ      '0'
     C     XCCPT4        ANDEQ     'A4557'
     C     XCDSEQ        IFNE      CADSEQ
     C     XCTOTP        IFGT      0
     C                   ADD       1             COUNT
     C                   END                                                    END XCTOTP GT 0
     C                   END
     C     CAKYC2        READE     PCA0C                                  60
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFGE      1
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BA'          QQC(Q)
     C* Err Msg BA  "History exists for A4557; Review payment of this
     C*             charge."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OTHEE
     C                   END                                                    END COUNT GT 1
     C*
     C                   END                                                    END DHPRCD EQ 'A4557
     C*
     C* ===============================================================
     C* PER PROVIDER BULLETIN 1991 #91-15 Z6300 ONLY ALLOWED TWICE PER YEAR
     C*
     C     DHPRCD        IFEQ      'Z6300'
     C*
     C                   Z-ADD     *ZEROS        COUNT
     C                   MOVE      'Z6300'       XXCPT4
     C     CAKYC         SETLL     PCA0C
     C     CAKYC2        READE     PCA0C                                  60
     C     *IN60         IFEQ      '0'
     C     XCCPT4        ANDEQ     'Z6300'
     C     XCDSEQ        IFNE      CADSEQ
     C     XCTOTP        IFGT      0
     C                   ADD       1             COUNT
     C                   END                                                    END XCTOTP GT 0
     C                   END
     C     CAKYC2        READE     PCA0C                                  60
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFGE      1
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BB'          QQC(Q)
     C* Err Msg BB  "History exists for Z6300; Review payment of this
     C*             charge."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OTHEE
     C                   END                                                    END COUNT GT 1
     C*
     C                   END                                                    END DHPRCD EQ 'Z6300
     C*
     C* ===============================================================
     C* Page 11-3 I
     C* Travel
     C* Orthotist travel expeses are reimbursable only if the injured worker
     C* is physically unable to travel due to allowed compensable injury
     C* or occupational disease, or the physician of record has required
     C* via prescription, that travel to the hospital, extended care
     C* facility or injured worker's home is necessary to provide initial
     C* fitting and/or application.
     C*
     C     DHPRCD        IFEQ      'W4215'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '90'          QQC(Q)
     C* Err Msg 90: "Adjustor should review charges for significant
     C*             identifiable services"
     C*
     C*
     C                   END                                                    END DHPRCD EQ 'W4215
     C*
     C* ===============================================================
     C* PER PROVIDER MEMORANDUM Z7611 ONLY ALLOWED ONCE
     C*
     C     DHPRCD        IFEQ      'Z7611'
     C*
     C                   Z-ADD     *ZEROS        COUNT
     C                   MOVE      'Z7611'       XXCPT4
     C     CAKYC         SETLL     PCA0C
     C     CAKYC2        READE     PCA0C                                  60
     C     *IN60         IFEQ      '0'
     C     XCCPT4        ANDEQ     'Z7611'
     C     XCDSEQ        IFNE      CADSEQ
     C     XCTOTP        IFGT      0
     C                   ADD       1             COUNT
     C                   END                                                    END XCTOTP GT 0
     C                   END
     C     CAKYC2        READE     PCA0C                                  60
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFGE      1
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '113'         XPEOMB
     C* EOMB Code 113: "PAYMENT DENIED, EXCEEDS BWC LIMIT FOR PROCEDURE"
     C*                PER DATE OF SERVICE"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OTHEE
     C                   END                                                    END COUNT GT 1
     C*
     C                   END                                                    END DHPRCD EQ 'Z7611
     C*
     C*
     C* ===============================================================
     C* Page 10 - 8 #3
     C* Y0720 Tens Purchase, All Units
     C* Begining July 28, 1994, this code replaces E0720 and E0730 and
     C* must be used when billing for purchase of the TENS units.  Four
     C* months rental must have been completed.
     C*
     C     DHPRCD        IFEQ      'Y0720'
     C*
     C                   Z-ADD     *ZEROS        COUNT
     C                   MOVE      'Y0715'       XXCPT4
     C     CAKYC         SETLL     PCA0C
     C     CAKYC2        READE     PCA0C                                  60
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        ANDEQ     'Y0715'
     C     XCDSEQ        IFNE      CADSEQ
     C     XCTOTP        IFGT      0
     C                   ADD       1             COUNT
     C                   END                                                    END XCTOTP GT 0
     C                   END
     C     CAKYC2        READE     PCA0C                                  60
     C                   END                                                    END *IN60
     C*
     C     COUNT         IFLE      3
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '522'         XPEOMB
     C* EOMB Code 522: "DENIED, PENDING RECEIPT FOR MED REVIEW         "
     C*                SUBMIT NEW INVOICE"
     C*
     C*  CHANGED TO USE EOMB 522
     C*
     C* EOMB Code 113: "PAYMENT DENIED, EXCEEDS BWC LIMIT FOR PROCEDURE"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OTHEE
     C                   END                                                    END COUNT GT 1
     C*
     C                   END                                                    END DHPRCD EQ 'Z7611
     C*
     C*
     C* ===============================================================
     C* IF XXPASS = 1ST TIME DON'T PAY, ONLY DO THIS RANGE OF CODE ONCE
     C     DHPRCD        IFEQ      'Y0715'
     C     XXPASS        IFEQ      '1'
     C                   Z-ADD     *ZEROS        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   EXSR      UPDAT
     C                   GOTO      OTHEE
     C                   ENDIF                                                  IF EQ 1
     C                   ENDIF                                                  IF DHPRCD
     C* ===============================================================
     C* Page 10-6 #17 (1)
     C* Beginning March 28, 1994, the following HCPCS code is to be used
     C* when billing for TENS rental:
     C*
     C     DHPRCD        IFEQ      'Y0715'
     C*
     C* FIND DATE 30 DAY PRIOR
     C                   Z-ADD     30            SRPST
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C                   Z-ADD     WKFDAT        SRDTE
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C*
     C                   Z-ADD     SRDTE         XXPST8                         CHG 11/11/97
     C                   Z-ADD     XXPST8        SRDTI                          CHG 11/11/97
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPAS1            7 0
     C*
     C* FIND DATE 60  DAY PRIOR
     C                   Z-ADD     60            SRPST
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C                   Z-ADD     WKFDAT        SRDTE
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C*
     C                   Z-ADD     SRDTE         XXPST8                         CHG 11/11/97
     C                   Z-ADD     XXPST8        SRDTI                          CHG 11/11/97
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPAS2            7 0
     C*
     C* FIND DATE  90 DAY PRIOR
     C                   Z-ADD     90            SRPST
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C                   Z-ADD     WKFDAT        SRDTE
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C*
     C                   Z-ADD     SRDTE         XXPST8                         CHG 11/11/97
     C                   Z-ADD     XXPST8        SRDTI                          CHG 11/11/97
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPAS3            7 0
     C* FIND DATE 120 DAY PRIOR
     C                   Z-ADD     120           SRPST
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C                   Z-ADD     WKFDAT        SRDTE
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C*
     C                   Z-ADD     SRDTE         XXPST8                         CHG 11/11/97
     C                   Z-ADD     XXPST8        SRDTI                          CHG 11/11/97
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPAS4            7 0
     C*
     C                   Z-ADD     *ZEROS        COUNT
     C                   Z-ADD     1             COUNT1            3 0
     C                   Z-ADD     *ZEROS        COUNT2            3 0
     C                   Z-ADD     *ZEROS        COUNT3            3 0
     C                   Z-ADD     *ZEROS        COUNT4            3 0
     C                   MOVE      DHPRCD        XXCPT4
     C     CAKYC         SETLL     PCA0C
     C     CAKYC2        READE     PCA0C                                  60
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        ANDEQ     DHPRCD
     C     XCDSEQ        IFNE      CADSEQ
     C     XCTOTP        IFGT      0
     C*
     C                   SELECT
     C     XCFDAT        WHENLE    CAFDAT
     C     XCFDAT        ANDGE     XXPAS1
     C                   ADD       1             COUNT1
     C*
     C     XCFDAT        WHENLE    CAFDAT
     C     XCFDAT        ANDGE     XXPAS2
     C                   ADD       1             COUNT2
     C*
     C     XCFDAT        WHENLE    CAFDAT
     C     XCFDAT        ANDGE     XXPAS3
     C                   ADD       1             COUNT3
     C*
     C     XCFDAT        WHENLE    CAFDAT
     C     XCFDAT        ANDGE     XXPAS4
     C                   ADD       1             COUNT4
     C*                    OTHER
     C*                    Z-ADD4         COUNT
     C                   ENDSL
     C                   END                                                    END XCTOTP GT 0
     C                   END
     C     CAKYC2        READE     PCA0C                                  60
     C                   END                                                    END *IN60
     C*
     C     COUNT1        IFGE      2
     C     COUNT2        ORGE      2
     C     COUNT3        ORGE      2
     C     COUNT4        ORGE      2
     C     COUNT         ORGE      2
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C*
     C                   MOVE      '481'         XPEOMB
     C*** EOMB 481: "Payment is denied as this service has already "
     C*             "been reimbursed the maximum number of times allowed"
     C*
     C**********           MOVE '113'     XPEOMB
     C* EOMB Code 113: "PAYMENT DENIED, EXCEEDS BWC LIMIT FOR PROCEDURE"
     C**********       PER DATE OF SERVICE"
     C                   EXSR      UPDAT
     C                   GOTO      OTHEE
     C                   END                                                    END COUNT GT 2
     C*
     C                   END                                                    END DHPRCD EQ 'Z7611
     C*
     C*
     C* ===============================================================
     C* Page 9-6 I
     C* Billing Information:
     C* Programs must bill on the C-19 BWC Service Invoice for TBI services
     C* once a month using the approved HCPCS code for the TBI service.  Service
     C* provided to the injured worker must be itemized on a separate sheet.
     C*
     C     DHPRCD        IFGE      'W0177'
     C     DHPRCD        ANDLE     'W0185'
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '455'         XPEOMB
     C* EOMB Code 455: "PAYMENT DENIED, DOCUMENTATION REQUEST HAS NOT
     C*                BEEN RECEIVED"
     C*
     C***********          MOVE '113'     XPEOMB
     C* EOMB Code 113: "PAYMENT DENIED, EXCEEDS BWC LIMIT FOR PROCEDURE"
     C***********      PER DATE OF SERVICE"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FS'          QQC(Q)
     C* Err Msg FS: "Traumatic brain injury claim, submit to utilization
     C*             management."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OTHEE
     C*
     C                   END                                                    END DHPRCD EQ 'W0177
     C*
     C*================================================================
     C* If code Y0715 is billed with units greater
     C* than 1, pay only one unit. Error Message GE                    rom BWC
     C*
     C     DHUNTS        IFGT      1
     C     DHPRCD        ANDEQ     'Y0715'
     C*
     C     SXANAM        MULT      1             SXMRA
     C*
     C*  REMOVAL OF ERROR BECAUSE ERROR IS GIVEN PRIOR TO ANY EDIT FOR
     C*  ALL HCPCS
     C                   ENDIF                                                   END DHUNTS
     C* ==================================================================
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
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C*
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '27'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '53'
     C     XPPMOD        ANDNE     '54'
     C     XPPMOD        ANDNE     '55'
     C     XPPMOD        ANDNE     '56'
     C     XPPMOD        ANDNE     '57'
     C     XPPMOD        ANDNE     '58'
     C     XPPMOD        ANDNE     '59'
     C     XPPMOD        ANDNE     '66'
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
     C                   MOVE      '169'         XPEOMB
     C* EOMB Code 169: "Payment is denied as the modifier is not valid
     C*                 with the procedure code billed"
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
     C                   END                                                    END NE 22
     C                   END                                                    END NE *BLANKS
     C*
     C                   EXSR      PMOD
     C*
     C* ===============================================================
     C* PAGE 2-16, #31
     C* Laboratory and Pathology Services
     C* Laboratory and pathology services are covered services and reimbursable
     C* when medically necessary and related to the allowed compensable
     C* condition.  Use CPT coding and the appropriate modifiers which are
     C* listed at the end of this chapter.
     C*
     C     DHPRCD        IFEQ      '81005'
     C     DHPRCD        OREQ      '81015'
     C*
     C                   MOVE      '81000'       XXCPT4
     C*
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFEQ      '81000'
     C*          XRTOTP    ANDGT0
     C     XRFDAT        ANDEQ     CAFDAT
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '764'         XPEOMB
     C**EOMB Code 764: "Payment is denied as this procedure code is
     C******            part of another service"
     C**EOMB Code 204: "This is a duplicate of another procedure
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   ENDIF
     C     CAKY9A        READE     PCA09                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C                   ENDIF                                                  END DHPRCD
     C*
     C*================================================================
     C* If modifier 90 is billed with code range from 80000- 89388
     C* on same date,same provider,same code eomb 525, else same date  rom BWC
     C* same code, different provider eomb 523
     C*
     C     XPPMOD        IFEQ      '90'
     C     DHPRCD        IFGE      '80000'
     C     DHPRCD        ANDLE     '89388'
     C*
     C     CAKY9E        CHAIN     PCA09                              60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFEQ      DHPRCD
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRFEIN        ANDEQ     CAFEIN
     C     XRFDAT        ANDEQ     CAFDAT
     C     XRTOTP        ANDGT     0
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '525'         XPEOMB
     C* EOMB Code 525: "PAYMENT IS DENIED THIS PROCEDURE IS MUTUALLY   "
     C*                EXCLUSIVE TO ANOTHER CODE."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   ENDIF                                                  END XRCPT4
     C***************************************************************
     C* SECOND SITUATION, DIFFERENT PROVIDERS
     C     XRCPT4        IFEQ      DHPRCD
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRFEIN        ANDNE     CAFEIN
     C     XRFDAT        ANDEQ     CAFDAT
     C     XRTOTP        ANDGT     0
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '523'         XPEOMB
     C* EOMB Code 523: "PAYMENT FOR SERVICE HAS BEEN MADE TO A DIFFERENT"
     C*                PROVIDER."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C                   ENDIF                                                  END XRCPT4
     C*
     C***************************************************************
     C     CAKY9E        READE     PCA09                                  60
     C                   ENDDO                                                  END *IN60
     C                   ENDIF                                                  END DHPRCD
     C                   ENDIF                                                  END EQ 90
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
     *** Physical Medicine          Codes: 97000 - 97999         P H Y S *
     *********************************************************************
     C     PHYS          BEGSR
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per PHYS, pg. 2-3
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '27'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '53'
     C     XPPMOD        ANDNE     '55'
     C     XPPMOD        ANDNE     '56'
     C     XPPMOD        ANDNE     '57'
     C     XPPMOD        ANDNE     '58'
     C     XPPMOD        ANDNE     '59'
     C     XPPMOD        ANDNE     '66'
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
     C                   MOVE      '169'         XPEOMB
     C* EOMB Code 169: "Payment is denied as the modifier is not valid
     C*                 with the procedure code billed"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C*
     C                   ELSE                                                   ELSE EQ 22...
     C*
     C     XPPMOD        IFEQ      '26'
     C     SXPROF        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BE'          QQC(Q)
     C* Err Msg BE: "No Professional Componenet Amount Established"
     C                   END                                                    END XPPMOD EQ '26'
     C*
     C                   EXSR      PMOD
     C*
     C                   END                                                    END EQ 22
     C*
     C                   END                                                    END NE *BLANK
     C*
     C* ===============================================================
     C* EOMB #255
     C* If a code form range 97010 - 97250, is billed with a code from cine
     C* range: 97545 - 97546, same or different provider, disallow     he body
     C* 97010 - 97250, EOB 555
     C*
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97250'
     C*
     C                   MOVE      '97545'       XXCPT4
     C*
     C     CAKY9E        CHAIN     PCA09                              60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFEQ      '97545'
     C     XRCPT4        OREQ      '97546'
     C     XRTOTP        ANDGT     0
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '555'         XPEOMB
     C* EOMB Code 555: "PAYMENT IS DENIED AS BWC WILL NOT REIMB. FOR   "
     C*                BOTH WORK HARDENING AND PT."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   ENDIF                                                  END XGCPT4
     C*
     C     CAKY9E        READE     PCA09                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C                   ENDIF                                                  END DHPRCD
     C* ===============================================================
     C* ===============================================================
     C* PAGE 2-5, #14
     C* Prior Authorization Required for more than two(2) physical medicine
     C* services or treatment to more than two (2) different areas of the body
     C* per injured worker, per date of service.
     C*
     C                   Z-ADD     *ZEROS        COUNT
     C                   Z-ADD     *ZEROS        COUNT1
     C     SVSPCD        IFEQ      'CHI'
     C*  CHANGE EDIT RANGE TO 97010-97250
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97250'
     C     DHPRCD        OREQ      '97265'
     C*
     C     DHAREA        IFGE      '01'
     C*
     C     DHAREA        ANDLE     '10'
     C*
     C                   MOVE      '97010'       XXCPT4
     C*
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*  CHANGE EDIT RANGE TO 97010-97250
     C     XRCPT4        IFGE      '97010'
     C     XRCPT4        ANDLE     '97250'
     C     XRCPT4        OREQ      '97265'
     C*
     C     XRTOTP        IFGT      0
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRFDAT        ANDEQ     CAFDAT
     C*
     C     XRPART        ANDGE     '01'
     C     XRPART        ANDLE     '10'
     C*
     C                   ADD       1             COUNT
     C     XRCPT4        IFEQ      DHPRCD
     C     XRPART        ANDEQ     DHAREA
     C     XRFDAT        ANDEQ     CAFDAT
     C                   ADD       3             COUNT
     C*
     C                   ENDIF                                                  END XRCPT4
     C                   ENDIF                                                  END XTTOTP
     C                   ENDIF                                                  END EQ 97014
     C     XRCPT4        IFEQ      DHPRCD
     C     XRTOTP        ANDGT     0
     C     XRPART        ANDNE     DHAREA
     C     XRFDAT        ANDEQ     CAFDAT
     C     XRPART        ANDGE     '01'
     C     XRPART        ANDLE     '10'
     C*
     C                   ADD       1             COUNT1
     C                   ENDIF                                                  END EQ XRCPT4
     C*
     C     CAKY9A        READE     PCA09                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C     COUNT         IFGE      2
     C     COUNT1        ORGE      2
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '87'          QQC(Q)
     C* Err Msg 87: "Prior written authorization required for reimbursement"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '357'         XPEOMB
     C* EOMB Code 357: "Denied physical med modality exceeds maximum
     C*                "allowed w/o authorization.                     owed"
     C***********          MOVE '113'     XPEOMB
     C* EOMB Code 113: "PAYMENT DENIED, EXCEEDS BWC LIMIT FOR PROCEDURE"
     C***********      PER DATE OF SERVICE"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   ENDIF                                                  END COUNT GT 2
     C*
     C                   ENDIF                                                  END CAAPRT
     C                   ENDIF                                                  END DHPRCD
     C                   ENDIF                                                  END SVSPCD
     C*
     C* ===============================================================
     C* PAGE 2-5, #14      Error message ADDITION TO EDIT
     C* If specialty is CHI and code equals 97750 give error message   y
     C*
     C     SVSPCD        IFEQ      'CHI'
     C     DHPRCD        ANDEQ     '97750'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FH'          QQC(Q)
     C* Err Msg FH: "Verify medical necessity"
     C*
     C                   ENDIF                                                  END DHPRCD /SVSPCD
     C*
     C*
     C* ===============================================================
     C* PAGE 2-20, #* 1
     C* Treatments will be allowed three (3) times per week in a supervied
     C* setting.
     C*
     C                   CLEAR                   DATE
     C                   Z-ADD     *ZEROS        X
     C                   Z-ADD     *ZEROS        COUNT
     C     DHPRCD        IFEQ      '97032'
     C     DHPRCD        OREQ      '97014'
     C*
     C                   MOVE      '97014'       XXCPT4
     C*
     C* FIND DATE 7 DAYS PRIOR
     C*   CHG TO LOOK AT 7 DAYS PRIOR NOT 6
     C                   Z-ADD     7             SRPST
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT
     C                   Z-ADD     WKFDAT        SRDTE
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C*
     C                   Z-ADD     SRDTE         XXPST8                         CHG 11/11/97
     C                   Z-ADD     XXPST8        SRDTI                          CHG 11/11/97
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPAST
     C*
     C     CAKYC         SETLL     PCA0C
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        IFEQ      '97032'
     C     XCCPT4        OREQ      '97014'
     C     XCTOTP        IFGT      0
     C     XCDSEQ        ANDNE     CADSEQ
     C     XCFDAT        ANDGE     XXPAST
     C*
     C     XCFDAT        LOOKUP    DATE                                   61
     C     *IN61         IFEQ      '0'                                          IFNOT THERE ADD
     C     COUNT         ANDLT     5
     C                   ADD       1             X
     C                   MOVEL     XCFDAT        DATE(X)
     C*
     C                   ADD       1             COUNT
     C*
     C     DHPRCD        IFEQ      XCCPT4
     C     CAFDAT        ANDEQ     XCFDAT
     C                   Z-ADD     1             COUNT
     C                   LEAVE
     C                   ENDIF                                                  END DHPRCD
     C*
     C                   ENDIF                                                  END *IN61
     C                   ENDIF                                                  END XCTOTP
     C                   ENDIF                                                  END EQ 97032
     C*
     C     CAKYC2        READE     PCA0C                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C     COUNT         IFGE      3
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '87'          QQC(Q)
     C* Err Msg 87: "Prior written authorization required for reimbursement"
     C*
      *   IF IT IS NOT SELF INSURED DISALLOW ELSE PAY FOR SI
     C*
      *   CALL API WA900R TO DETERMINE FUND TYPE
      *
     C*
     C     @@TYPE        IFNE      'SI'
      *
      *   IF IT IS NOT SELF INSURED DISALLOW ELSE PAY FOR SI
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '481'         XPEOMB
     C* EOMB Code 481: "Payment is denied as this service has already been"
     C*                "reimbursed the maximum number of times allowed"
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   ENDIF                                                  END WA900R
     C                   ENDIF                                                  END COUNT GT 4
     C*
     C                   ENDIF                                                  END DHPRCD
     C*
     C* ===============================================================
     C* IF XXPASS = 1ST TIME DON'T PAY, ONLY DO THIS RANGE OF CODE ONCE
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97139'
     C     XXPASS        IFEQ      '1'
     C                   Z-ADD     *ZEROS        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   ENDIF                                                  IF EQ 1
     C                   ENDIF                                                  IF DHPRCD
     C* ===============================================================
     C* PAGE 2-27, #C
     C* Massage therapy is considered to be a physical medicine modality,
     C* After the initial 10 treatments, all subsequent physical medicine
     C* treatments, excluding chiropractic treatments, require prior
     C* authorization for the life of the claim.
     C*
     C*
     C                   CLEAR                   DATE
     C                   Z-ADD     *ZEROS        COUNT
     C                   Z-ADD     *ZEROS        X                 2 0
     C     DHPRCD        IFEQ      '97124'
     C*
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     CAKYC         SETLL     PCA0C
     C     CAKYC         READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCTOTP        IFGT      0
     C*
     C     XCFDAT        LOOKUP    DATE                                   61
     C     *IN61         IFEQ      '0'                                          IFNOT THERE ADD
     C     COUNT         ANDLT     22
     C                   ADD       1             X
     C                   MOVEL     XCFDAT        DATE(X)
     C*
     C                   ADD       1             COUNT
     C*
     C                   ENDIF                                                  END XRTOTP
     C                   ENDIF                                                  END *IN61
     C*
     C     CAKYC         READE     PCA0C                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C     COUNT         IFGT      10
     C     COUNT         ANDLE     19
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FJ'          QQC(Q)
     C* Err Msg FJ: "Verify Pre-Authorization"
     C*
     C                   ENDIF                                                  END COUNT GT 10
     C*
     C     COUNT         IFGE      20
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FJ'          QQC(Q)
     C* Err Msg FJ: "Verify Pre-Authorization"
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '357'         XPEOMB
     C* EOMB Code 357: "Denied physical med modality exceeds maximum
     C*                "allowed w/o authorization.                     owed"
     C**********           MOVE '481'     XPEOMB
     C* EOMB Code 481: "Payment is denied as this service has already"
     C***********      "been reimbursed the maximum number of times allowed"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   ENDIF                                                  END COUNT GT 20
     C*
     C                   ENDIF                                                  END DHPRCD
     C*
     C* ===============================================================
     C* PAGE 2-27, #F
     C* Physical Therapy Bureau Rule 4123-7-12
     C* Prior Authorization required
     C* Physical medicine may be provided for no more than ten (10) treatments
     C* unless prior authorization is obtained through BWC.
     C*
     C*** CHI IS EXEMPT
     C     SVSPCD        IFNE      'CHI'                                        ADD 09/30/97
     C*
     C                   CLEAR                   DATE
     C                   Z-ADD     *ZEROS        COUNT
     C                   Z-ADD     *ZEROS        X                 2 0
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97139'
     C*
     C                   MOVE      '97010'       XXCPT4
     C*
     C     CAKYC         SETLL     PCA0C
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCTOTP        IFGT      0
     C     XCCPT4        ANDGE     '97010'
     C     XCCPT4        ANDLE     '97139'
     C*
     C     XCFDAT        LOOKUP    DATE                                   61
     C     *IN61         IFEQ      '0'                                          IFNOT THERE ADD
     C     COUNT         ANDLT     21
     C                   ADD       1             X
     C                   MOVEL     XCFDAT        DATE(X)
     C*
     C                   ADD       1             COUNT
     C*
     C                   ENDIF                                                  END *IN61
     C                   ENDIF                                                  END XCTOTP
     C*
     C     CAKYC2        READE     PCA0C                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C     COUNT         IFGE      10
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FJ'          QQC(Q)
     C* Err Msg FJ: "Verify Pre-Authorization"
     C                   ENDIF                                                  END COUNT GT 10
     C                   ENDIF                                                  END DHPRCD
     C                   ENDIF                                                  END 'CHI'
     C*
     C* ===============================================================
     C* PAGE 2-27, #4
     C* Massage therapists can only bill and be reimbursed using CPT code
     C* 97124 (massage therapy, initial 15 minutes).  Only one additional
     C* time unit will be condisdered for reimbursement per date of service.
     C*
     C     SVSPCD        IFEQ      'LMT'
     C     DHPRCD        IFEQ      '97124'
     C     DHPRCD        OREQ      '97145'
     C*
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     CAKYC         SETLL     PCA0C
     C     CAKYC         READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     CADSEQ        ANDNE     XCDSEQ
     C     XCTOTP        ANDGT     *ZEROS
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FJ'          QQC(Q)
     C* Err Msg FJ: "Verify Pre-Authorization"
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '482'         XPEOMB
     C* EOMB Code 482: "Payment is denied as this service has already"
     C*                "been reimbursed the maximum number of times allowed"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C     CAKYC         READE     PCA0C                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C*
     C                   ENDIF                                                  END DHPRCD
     C                   ENDIF                                                  END SVSPCD
     C*
     C*
     C* ===============================================================
     C* PAGE 2-27, #4,2
     C* If 97124 is billed with 97010 same date of service, msg to uservice.
     C* on 97010, "hot packs not reimbursable with massage".
     C*
     C     DHPRCD        IFEQ      '97010'
     C*
     C                   MOVE      97124         XXCPT4
     C*
     C     CAKYC         SETLL     PCA0C
     C     CAKYC         READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FU'          QQC(Q)
     C* Err Msg FU: "Hot packs not reimbursable with massage"
     C     CAKYC         READE     PCA0C                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C*
     C                   ENDIF                                                  END DHPRCD
     C*
     C* ===============================================================
      *   CALL API WA900R TO DETERMINE FUND TYPE
      *
     C*
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
     C* ===============================================================
     C* Modifier -26
     C     XPPMOD        IFEQ      '26'
     C                   Z-ADD     SXPROF        SXMRA
     C                   END                                                    END XPPMOD  EQ '26'
     C*
     C* ===============================================================
     C* Modifier -27
     C     XPPMOD        IFEQ      '27'
     C     SXMRA         SUB       SXPROF        SXMRA
     C                   END                                                    END XPPMOD  EQ '27'
     C*
     C* ===============================================================
     C*
     C     PMODE         ENDSR
     *********************************************************************
     *** Procedure Code Modifier                               P M O D 1 *
     *********************************************************************
     C     PMOD1         BEGSR
     C*
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     XPPMOD        TABKEY
     C*
     C     PMKY          CHAIN     PPM01OH                            64
     C*
     C     PMPERC        IFNE      *ZERO
     C     PMPERC        DIV(H)    100           XXPERC            5 2
     C                   END
     C*
     C* ===============================================================
     C* Modifier -26
     C     XPPMOD        IFEQ      '26'
     C                   Z-ADD     SXPROF        SXMRA
     C                   END                                                    END XPPMOD  EQ '26'
     C*
     C* ===============================================================
     C* Modifier -27
     C     XPPMOD        IFEQ      '27'
     C     SXMRA         SUB       SXPROF        SXMRA
     C                   END                                                    END XPPMOD  EQ '27'
     C*
     C* ===============================================================
     C*
     C     PMOD1E        ENDSR
     *********************************************************************
     *** Radiology                  Codes: 70010 - 79999         R A D I *
     *********************************************************************
     C     RADI          BEGSR
     C*
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per RADI, pg. 3-5
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '27'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '53'
     C     XPPMOD        ANDNE     '54'
     C     XPPMOD        ANDNE     '55'
     C     XPPMOD        ANDNE     '56'
     C     XPPMOD        ANDNE     '57'
     C     XPPMOD        ANDNE     '58'
     C     XPPMOD        ANDNE     '59'
     C     XPPMOD        ANDNE     '66'
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
     C                   MOVE      '169'         XPEOMB
     C* EOMB Code 169: "Payment is denied as the modifier is not valid
     C*                 with the procedure code billed"
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
     C                   ELSE                                                   ELSE EQ 22...
     C                   EXSR      PMOD
     C                   ENDIF                                                  END EQ 22
     C                   ENDIF                                                  XPPMOD NE *' '
     C* ===============================================================
     C*** If Mod -26 or -27 and not a Radiology code give message
     C*
     C     XPPMOD        IFEQ      '26'
     C     XPPMOD        OREQ      '27'
     C     DHPRCD        IFLT      '70000'
     C     DHPRCD        ORGT      '79999'
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
     C*****************    MOVE '201'     XPEOMB
     C**EOMB Code 201: "Service is not reimburseable under the Workers
     C                   MOVE      '169'         XPEOMB
     C* EOMB Code 169: "Payment is denied as the modifier is not valid
     C*                 with the procedure code billed"
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C                   END                                                    END DHPRCD LT 7000
     C                   END                                                    END XPPMOD EQ '26'
     C* ===============================================================
     C* PAGE 2-31, #53
     C* Total back of full spine x-rays are not reimbursable.  Only views
     C* of the body area associated with the allowed condition in the claim
     C* are reimbursable.
     C*
     C     DHPRCD        IFEQ      '72010'
     C*
     C*
     C                   Z-ADD     1             Q                                           'I'
     C     '00000'       LOOKUP    QQS(Q)                                 60                 'I'
     C  N60              Z-ADD     30            Q                                           'I'
     C                   MOVEL     CADSEQ        QQS(Q)                                      'I'
     C                   MOVE      'GF'          QQC(Q)                                      'I'
     C**Err Msg GF: "Does Diagnosis support multiple views."            'I'
     C                   END                                                    END DHPRCD
     C*
     C*================================================================
     C*  Addition to Rule 2-31 #53
     C*  If codes 72100, 72040, 72070 are billed with 72010 disallow
     C*  72100, 72040, 72070. EOB 765
     C*
     C     DHPRCD        IFEQ      '72100'
     C     DHPRCD        OREQ      '72040'
     C     DHPRCD        OREQ      '72070'
     C                   MOVEL     '72010'       XXCPT4
     C     CAKY9         CHAIN     PCA09                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '765'         XPEOMB
     C* EOMB Code 765: "Procedure has been added as it covers unbundled"
     C*                charges billed by prov."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C                   END                                                    END *IN60
     C                   END                                                    END DHPRCD
     C*
     C* ===============================================================
     C*
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
     *** Surgery                    Codes: 10000 - 69999         S U R G *
     *********************************************************************
     C     SURG          BEGSR
     C*
     C*=====================================================================
     C* Procedure Codes with modifiers:
     C     XPPMOD        IFNE      *BLANK
     C*----------------------------------------------------------------
     C* Per SURG, pg. 5-8
     C*
     C     XPPMOD        IFNE      '20'
     C     XPPMOD        ANDNE     '22'
     C     XPPMOD        ANDNE     '26'
     C     XPPMOD        ANDNE     '27'
     C     XPPMOD        ANDNE     '32'
     C     XPPMOD        ANDNE     '50'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     '52'
     C     XPPMOD        ANDNE     '53'
     C     XPPMOD        ANDNE     '54'
     C     XPPMOD        ANDNE     '55'
     C     XPPMOD        ANDNE     '56'
     C     XPPMOD        ANDNE     '57'
     C     XPPMOD        ANDNE     '58'
     C     XPPMOD        ANDNE     '59'
     C     XPPMOD        ANDNE     '62'
     C     XPPMOD        ANDNE     '66'
     C     XPPMOD        ANDNE     '76'
     C     XPPMOD        ANDNE     '77'
     C     XPPMOD        ANDNE     '78'
     C     XPPMOD        ANDNE     '79'
     C     XPPMOD        ANDNE     '80'
     C     XPPMOD        ANDNE     '81'
     C     XPPMOD        ANDNE     '82'
     C     XPPMOD        ANDNE     '90'
     C     XPPMOD        ANDNE     '99'
     C*
     C                   Z-ADD     1             Q
     C     '0Z000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '169'         XPEOMB
     C* EOMB Code 169: "Payment is denied as the modifier is not valid
     C*                 with the procedure code billed"
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END                                                    END NE 20
     C                   END                                                    END XPPMOD NE *BLANK
     C*
     C*   CALL SUBROUTINE PMOD FOR MOD -26, -27
     C*
     C                   EXSR      PMOD
     C*
     C* ===============================================================
     C* IF XXPASS = 1ST TIME DON'T PAY, ONLY DO THIS RANGE OF CODE ONCE
     C     XPPMOD        IFEQ      '50'
     C     XXPASS        IFEQ      '1'
     C                   Z-ADD     *ZEROS        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   ENDIF                                                  IF EQ 1
     C                   ENDIF                                                  IF DHPRCD
     C* ===============================================================
     C* ===============================================================
     C*  STAGE II OF EDITS
     C*  If a code from range 10040-69979 is billed, same date, same
     C*  provider, with modifier -50, pay 100% first procedure, 50%
     C*  second same code.  Also, If 1 line item is billed with mod -50s
     C*  pay 150%.
     C*
     C*  If the code is billed twice on the same day with modifier -50
     C*  they will both pay 50%.  If more than two it will not pay.
     C*
     C                   Z-ADD     *ZEROS        COUNT
     C                   Z-ADD     *ZEROS        COUNT1                         ADD 10/23/97
     C*
     C     DHPRCD        IFGE      '10040'
     C     DHPRCD        ANDLE     '69970'
     C     XPPMOD        ANDEQ     '50'
     C     SX1A3         IFEQ      'Y'
     C*
     C                   MOVEL     DHPRCD        XXCPT4
     C     CAKY9         SETLL     PCA09
     C     CAKY9         READE     PCA09                                  60
     C     *IN60         DOWEQ     '0'
      *
     C     XRTOTP        IFGT      *ZEROS
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
     C*
     C                   ELSE                                                   SX1A3
     C*
     C                   Z-ADD     1             Q
     C     '0Z000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '766'         XPEOMB
     C* EOMB Code 766: "PAYMENT DENIED AS THIS PROCEDURE IS MUTUALLY   "
     C*                EXCLUSIVE TO ANOTHER"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   ENDIF                                                  SX1A3
     C*
     C*
     C*
     C     COUNT1        IFEQ      0
     C     COUNT         ANDEQ     1
     C     SXMRA         MULT      .50           SXMRA
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
     C*   CHANGE EOB CODE TO 113
     C*
     C*********            MOVE '099'     XPEOMB
     C*** EOMB 099: "Payment is denied, BWC's records indicate that
     C*********      exact duplicate charge."
     C*
     C                   MOVE      '113'         XPEOMB
     C*** EOMB 113: "Payment is denied, Exceeds BWC limit for procedure
     C***            per date of service."
     C*
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
     C                   MOVE      '766'         XPEOMB
     C* EOMB Code 766: "PAYMENT DENIED AS THIS PROCEDURE IS MUTUALLY   "
     C*                EXCLUSIVE TO ANOTHER"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   ENDIF                                                  ONLY ONE MOD
     C                   END                                                    END EQ '10040'
     C*
     C* ===============================================================
     C*
     C* If modifiers -62, -80, -81, -82 is billed, the same code can   with
     C* not be billed on same date with same fein or different fein
     C* with a modifier from same range (62, 80, 81, 82).
     C*
     C     XPPMOD        IFEQ      '62'
     C     XPPMOD        OREQ      '80'
     C     XPPMOD        OREQ      '81'
     C     XPPMOD        OREQ      '82'
     C     DHPRCD        IFGE      '10000'
     C     DHPRCD        ANDLE     '69999'
     C*
     C     CAKY9E        SETLL     PCA09
     C     CAKY9E        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*****************************************************************
     C     SXSURG        IFEQ      'N'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '117'         XPEOMB
     C*   EOMB 117: "Payment denied, as the modifier is invalid"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END NE XPPMOD
     C*****************************************************************
     C     XRCPT4        IFEQ      DHPRCD
     C     XRFDAT        ANDEQ     CAFDAT
     C     XRFEIN        ANDEQ     CAFEIN
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRTOTP        ANDGT     0
     C*
     C     XRPMOD        IFGE      '80'
     C     XRPMOD        ANDLE     '82'
     C     XRPMOD        OREQ      '62'
     C*
     C     XRPMOD        IFNE      XPPMOD
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '525'         XPEOMB
     C*   EOMB 525: "Payment denied, this procedure is mutually exclusive "
     C*             "to another code"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END NE XPPMOD
     C                   END                                                    END XRPMOD
     C                   END                                                    END XRCPT4
     C*****************************************************************
     C** DIFFERENT PROVIDER RETURNS DIFFERENT EOMB
     C*
     C     XRCPT4        IFEQ      DHPRCD
     C     XRFDAT        ANDEQ     CAFDAT
     C     XRFEIN        ANDNE     CAFEIN
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRTOTP        ANDGT     0
     C*
     C     XRPMOD        IFGE      '80'
     C     XRPMOD        ANDLE     '82'
     C     XRPMOD        OREQ      '62'
     C*
     C     XRPMOD        IFNE      XPPMOD
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '523'         XPEOMB
     C*   EOMB 525: "Payment denied, this procedure is mutually exclusive "
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END XRPMOD
     C                   END                                                    END NE XPPMOD
     C                   END                                                    END XRCPT4
     C*****************************************************************
     C*
     C     CAKY9E        READE     PCA09                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C                   END                                                    END XPPMOD
     C                   END                                                    END DHPRCD
     C*
     C*** END ADD ***** 08/15/97 **************************************
     C* ===============================================================
     C* PAGE 2-6, #1
     C* Chiropractic means utilization of the relationship between the muscular
     C* skeletal structures of hte body, the spinal column ans the nervous syste
     C* in the restoration and maintenance of health.
     C*
     C     SVSPCD        IFEQ      'CHI'
     C     DHPRCD        ANDGE     '10040'
     C     DHPRCD        ANDLE     '69979'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BP'          QQC(Q)
     C* Err Msg BP: "Specialty code and level of service don't match'
     C*             "please verify"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '332'         XPEOMB
     C* EOMB Code 332: "Procedure denied, procedure may not be billed  e"
     C*                 with other tens codes.                         e"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END                                                    END SVSPCD
     C*
     C* ===============================================================
     C* IF XXPASS = 1ST TIME DON'T PAY, ONLY DO THIS RANGE OF CODE ONCE
     C     DHPRCD        IFEQ      '20550'
     C     DHPRCD        ORGE      '20600'
     C     DHPRCD        ANDLE     '20610'
     C     DHPRCD        ORGE      '62274'
     C     DHPRCD        ANDLE     '62298'
     C     DHPRCD        ORGE      '64400'
     C     DHPRCD        ANDLE     '64530'
     C     XXPASS        IFEQ      '1'
     C                   Z-ADD     *ZEROS        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   ENDIF                                                  IF EQ 1
     C                   ENDIF                                                  IF DHPRCD
     C* PAGE 2-14, #1a
     C* CPT code 20550 should be used when billing for trigger point injections
     C* To indicate the number of injections delivered, providers must bill
     C* one(1) for units, to indicated the encounter.
     C*
     C     DHPRCD        IFEQ      '20550'
     C*
     C                   MOVE      '20550'       XXCPT4
     C*
     C     CAKY9         SETLL     PCA09
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFEQ      '20550'
     C     XRTOTP        ANDGT     0
     C     XRFDAT        ANDEQ     CAFDAT
     C     XRDSEQ        ANDNE     CADSEQ
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '482'         XPEOMB
     C* EOMB Code 482: "Payment is denied as this service has already
     C*                 been reimbursed the maximum number of times per day"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   ENDIF
     C     CAKY9A        READE     PCA09                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C                   ENDIF                                                  END DHPRCD
     C*
     C* ===============================================================
     C* PAGE 2-13, #28
     C* Injections:  Prior authorization required after allowed amount has
     C* been administered.
     C*
     C                   CLEAR                   DATE
     C                   Z-ADD     *ZEROS        COUNT
     C                   Z-ADD     *ZEROS        X
     C     DHPRCD        IFEQ      '20550'
     C     DHPRCD        ORGE      '20600'
     C     DHPRCD        ANDLE     '20610'
     C     DHPRCD        ORGE      '64400'
     C     DHPRCD        ANDLE     '64530'
     C*
     C                   MOVE      '20550'       XXCPT4
     C*
     C     CAKYC         SETLL     PCA0C
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        IFEQ      '20550'
     C     XCCPT4        ORGE      '20600'
     C     XCCPT4        ANDLE     '20610'
     C     XCCPT4        ORGE      '64400'
     C     XCCPT4        ANDLE     '64530'
     C     XCCPT4        OREQ      '90782'
     C     XCTOTP        IFGT      0
     C*
     C     XCFDAT        LOOKUP    DATE                                   61
     C     *IN61         IFEQ      '0'                                          IFNOT THERE ADD
     C     COUNT         ANDLT     15
     C                   ADD       1             X
     C                   MOVEL     XCFDAT        DATE(X)
     C*
     C                   ADD       1             COUNT
     C                   ENDIF                                                  END *IN61
     C                   ENDIF                                                  END XRTOTP
     C                   ENDIF                                                  END XRCPT4
     C*
     C     CAKYC2        READE     PCA0C                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C     COUNT         IFGE      12
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FR'          QQC(Q)
     C* Err Msg FR: "12 visits paid, Authorization required"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*
     C                   MOVE      '455'         XPEOMB
     C* EOMB Code 455: "PAYMENT DENIED, DOCUMENTATION REQUEST HAS NOT
     C*                BEEN RECEIVED"
     C*
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   ENDIF                                                  END COUNT GT 13
     C*
     C                   ENDIF                                                  END DHPRCD
     C*
     C* ===============================================================
     C* PAGE 2-14, #B
     C* Subarachnoid, Subdural or Epidural Injections
     C* A maixmum of three (3) injections from this group, per injured
     C* worker, may be administered before prior authorization is required
     C* from BWC.
     C*
     C                   Z-ADD     *ZEROS        COUNT
     C     DHPRCD        IFGE      '62274'
     C     DHPRCD        ANDLE     '62298'
     C*
     C                   MOVE      '62274'       XXCPT4
     C*
     C     CAKYC         SETLL     PCA0C
     C     CAKYC2        READE     PCA0C                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XCCPT4        IFGE      '62274'
     C     XCCPT4        ANDLE     '62298'
     C     XCTOTP        ANDGT     0
     C     XCDSEQ        ANDNE     CADSEQ
     C*
     C                   ADD       1             COUNT
     C                   ENDIF
     C*
     C     CAKYC2        READE     PCA0C                                  60
     C                   ENDDO                                                  END *IN60
     C*
     C     COUNT         IFGE      3
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '87'          QQC(Q)
     C* Err Msg 87: "Prior written authorization required for reimbursement"
     C*
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C*
     C                   MOVE      '455'         XPEOMB
     C* EOMB Code 455: "PAYMENT DENIED, DOCUMENTATION REQUEST HAS NOT
     C*                BEEN RECEIVED"
     C*
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   ENDIF                                                  END COUNT GT 4
     C                   ENDIF                                                  END DHPRCD
     C*
     C* ===============================================================
     C* PAGE 3-5 #6
     C* Code 22505 is not reimbursable under BWC guidelines
     C*
     C     DHPRCD        IFEQ      '22505'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '631'         XPEOMB
     C* EOMB Code 631: "Payment is denied as this service is not
     C*                allowed in the claim."
     C*
     C********             MOVE '752'     XPEOMB
     C**EOMB*Code 752: "Payment has been made for a BWC NON-COVERED Service"
     C********
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   ENDIF                                                  END DHPRCD
     C*
     C* ===============================================================
     C* PAGE 2-24, #5
     C* Physicians assisting at major surgery (Assistant Surgeons) will be
     C* paid at a percent of the maximum allowed for the procedure.  Assistant
     C* Surgeons billing with modifier -80,-82 will be reimbursed at 20% and
     C* -81 will pay at 10%.
     C*
     C*  REMOVED SPECIALTY CONDITION IN PHASE 4
     C* ONLY PROCESS MODIFIER 80, 81, 82
     C     XPPMOD        IFEQ      '80'
     C     XPPMOD        OREQ      '81'
     C     XPPMOD        OREQ      '82'
     C*
     C     SXSURG        IFEQ      'Y'
     C     XPPMOD        IFEQ      '80'
     C     XPPMOD        OREQ      '82'
     C     SXMRA         MULT      .20           SXMRA
     C                   ELSE
     C     XPPMOD        IFEQ      '81'
     C     SXMRA         MULT      .10           SXMRA
     C                   ENDIF                                                  END -81
     C                   ENDIF                                                  END -80
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FT'          QQC(Q)
     C* Err Msg FT: "Verify specialty of provider for this treatment"
     C*
     C*
     C                   ELSE
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BP'          QQC(Q)
     C* Err Msg BP: "Specialty code and level of service don't match, verify"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '631'         XPEOMB
     C* EOMB Code 631: "Payment is denied as the service is not allowed
     C*                in this claim
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   ENDIF                                                   END SXSURG
     C                   ENDIF                                                   END XPPMOD
     C* ===============================================================
     C* PAGE ?  SPECIALTY LSA, PA, SCT, RN ARE NOT REIMBURSED WITH MOD
     C* -80 AND -81
     C*
     C     SVSPCD        IFEQ      'LSA'
     C     SVSPCD        OREQ      'PA '
     C     SVSPCD        OREQ      'SCT'
     C     SVSPCD        OREQ      'RN '
     C*
     C     XPPMOD        IFEQ      '80'
     C     XPPMOD        OREQ      '81'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'BP'          QQC(Q)
     C* Err Msg BP: "Specialty code and level of service don't match, verify"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C**********           MOVE '332'     XPEOMB
     C* EOMB Code 332: "PAYMENT DENIED, SERVICING PROVIDER NOT ELGIBLE"
     C**********       FOR REIMBURSEMENT"
     C*
     C                   MOVE      '631'         XPEOMB
     C* EOMB Code 631: "Payment is denied as the service is not allowed
     C*                in this claim
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   ENDIF                                                   END XPPMOD
     C                   ENDIF                                                   END SVSPCD
     C* ===============================================================
     C* PAGE 2-24 #B
     C*
     C     SXSUPP        IFEQ      'N'
     C*
     C     DHPRCD        IFGE      '64400'
     C     DHPRCD        ANDLE     '64530'
     C     DHPRCD        ORGE      '62273'
     C     DHPRCD        ANDLE     '62279'
     C                   MOVE      '10000'       XXCPT4
     C*
     C     CAKY9         SETLL     PCA09
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     XRCPT4        IFGE      '10000'
     C     XRCPT4        ANDLE     '69999'
     C     XRDSEQ        ANDNE     CADSEQ
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AJ'          QQC(Q)
     C* Err Msg AJ: "Service falls within pre-operative policy."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '518'         XPEOMB
     C* EOMB Code 518: "Payment denied, This service is considered to be"
     C*                 part of the global fee."
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END
     C*
     C     CAKY9A        READE     PCA09                                  60
     C*
     C                   END                                                    END 0 DOWEQ 60
     C                   END                                                    END GE '90000'
     C                   END                                                    END PRSUPP EQ N
     C*
     C* ===============================================================
     C*
     C*  EXCEPTIONS TO THIS RULE WILL BE PROCEDURES FOUND IN TABLE NC1
     C*  THESE WILL NOT BE WRITTEN TO THE WORK FILE
     C*
     C                   Z-ADD     1             T                 5 0
     C     DHPRCD        LOOKUP    NC1(T)                                 60
     C     *IN60         CABEQ     '1'           NOCUT
     C*
     C* ===============================================================
     C* Write record to Workfile
     C* Code has passed all edits - Write to the FACAPOH Workfile
     C* Only load "current charges" into the FACAPOH Workfile
     C*
     C     XPPMOD        IFNE      '50'
     C                   Z-ADD     SXMRA         CAMRA
     C                   Z-ADD     CACHRG        CATOTC
     C                   Z-ADD     0             XPTOTA
     C                   MOVE      'S'           CATYPE
     C                   WRITE     PCAWK1
     C                   GOTO      SURGE
     C                   ENDIF
     C*
     C     NOCUT         TAG                                                    ADD 07/16/97
     C* ===============================================================
     C*
     C* If MOD -62, calculate 62.5 of surgery code
     C     XPPMOD        IFEQ      '62'
     C     SXMRA         MULT      .625          SXMRA
     C                   ENDIF
     C*
     C* ===============================================================
     C*
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
     C     SURG2         BEGSR
     C*
     C*----------------------------------------------------------------
     C*--- Reading the SURG Detail Charge Workfile --------------------
     C*
     C* ===============================================================
     C*
     C                   CLEAR                   CNT
     C                   Z-ADD     1             X
     C                   Z-ADD     0             COUNT
     C                   CLEAR                   CNT
     C                   MOVE      *BLANKS       HLPART
     C     *LOVAL        SETLL     PCAWK2
     C                   READ      PCAWK2                                 60
     C*
     C     *IN60         DOWEQ     '0'
     C                   MOVE      'N'           DUPFLG            1
      *
     C*   USED TO DETERMINE HOW MANY ERROR MSG '03' HAVE ALREADY
     C*   BEEN ADDED TO LINE ITEM, ONLY NEED TO ALLOW ONE.
     C*
     C                   Z-ADD     0             DUPCNT            2 0
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM.
     C*
     C                   MOVE      P2CPT4        XXCPT4
     C*
     C     DHKY2         CHAIN     PDH03                              54
     C*
     C     *IN54         DOWEQ     *OFF
     C*
     C                   SELECT
      *
     C     D1MOD1        WHENNE    *BLANKS
     C                   MOVEL     DHMOD1        X1PMOD
      *
     C     D1MOD2        WHENNE    *BLANKS
     C                   MOVEL     DHMOD2        X1PMOD
      *
     C     D1MOD3        WHENNE    *BLANKS
     C                   MOVEL     DHMOD3        X1PMOD
      *
     C     D1MOD4        WHENNE    *BLANKS
     C                   MOVEL     DHMOD4        X1PMOD
      *
     C                   OTHER
     C                   MOVE      *BLANKS       X1PMOD
      *
     C                   ENDSL
     C*
     C                   Z-ADD     D1DSEQ        KKDSEQ
     C*
     C     CAKYP         CHAIN     PCA0P                              61
     C*
     C     *IN61         IFEQ      *OFF
     C*
     C     PXFDAT        ANDEQ     P2FDAT
     C     PXFEIN        ANDEQ     P2FEIN
     C     PXFLAG        ANDNE     'P'
     C     PXDSEQ        ANDNE     P2DSEQ
     C     X1PMOD        ANDNE     '50'
      *
     C     XXPASS        IFNE      '1'
     C     DUPCNT        ANDNE     1
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     P2DSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
      *
     C                   Z-ADD     1             DUPCNT
      *
     C                   ENDIF                                                  XXPASS NE 1
      *
     C                   ENDIF                                                  IN61 = OFF
     C*
     C     DHKY2         READE     PDH03                                  54
     C*
     C                   ENDDO
     C*
     C*  SKIP ALL PROCESSING OF CPT4 CODE IF IT IS A DUPLICATE.  THIS
     C*  WILL GIVE THE CORRECT PAYMENT TO THE CORRECT CODES.  IF THIS
     C*  IS NOT HERE, IT WILL CONSIDER THE NON PAYED CODE AS ONE TO
     C*  PROCESS, PUTTING THE REST OF THE CODES FURTHER DOWN IN LINE
     C*  TO BE PAID
     C*
     C     DUPFLG        IFEQ      'N'
     C*
     C* Per pg. 8
     C*  Highest value 100% - HIGHEST WILL NOT INCLUDE MOD 51 SO IT WILL
     C*                       NOT BE INCLUDED IN THE WORKFILE
     C*  2nd            50%
     C*  3rd            50%
     C*  4th            50%
     C*  5th            50%
     C*  6th +          Message Review for Global Fee
     C*
     C*   BODY PART LOGIC HAS BEEN REMOVED.
     C*
     C*
     C*
     C*
     C* Page 17-1 C Place of serice '24'
     C* Covered Services
     C* 1st -- 100%
     C* 2nd -- 50%
     C* 3rd and above -- 00%
     C*
     C*
     C*   ONLY GO THROUGH SELECTION IF SPECIALTY IS ONE OF 5 CODES
     C*   ASC, OSC, OSU, SC, OR VA6
     C*
     C     SVSPCD        IFEQ      'ASC'
     C     SVSPCD        OREQ      'OSC'
     C     SVSPCD        OREQ      'OSU'
     C     SVSPCD        OREQ      'SC '
     C     SVSPCD        OREQ      'VA6'
     C*
     C* ==================================================================
     C                   SELECT
     C     COUNT         WHENEQ    0
     C                   Z-ADD     1.0           P2BRP                          100%
     C                   ADD       1             COUNT
     C                   MOVE      '758'         P2EOMB
     C* EOMB Code 758: "This is the principal procedure of multiple
     C*                 procedures billed on this date of service"
     C     COUNT         WHENEQ    1
     C                   Z-ADD     .50           P2BRP                          50%
     C                   ADD       1             COUNT
     C                   MOVE      '759'         P2EOMB
     C* EOMB Code 759: "This is the second procedure of multiple
     C*                 procedures billed on this date of service"
     C     COUNT         WHENGE    2
     C                   Z-ADD     0             P2BRP                          0%
     C                   ADD       1             COUNT
     C                   Z-ADD     *ZERO         P2TOTP
     C     P2TOTC        SUB       P2TOTP        P2TOTD
     C                   Z-ADD     P2TOTP        P2TOTA
     C                   MOVE      '358'         P2EOMB
     C* EOMB Code 358: "Payment is denied, BWC policy only allows reimburesement
     C*                 for up to 2 procedures"
     C*
     C                   ENDSL                                                  END  GT 6
     C*
     C                   ELSE
     C*
     C                   SELECT
     C     CNT(X)        WHENEQ    0
     C                   Z-ADD     1             P2BRP                          100%
     C                   ADD       1             CNT(X)
     C                   MOVE      '758'         P2EOMB
     C* EOMB Code 758: "This is the principal procedure of multiple
     C*                 procedures billed on this date of service"
     C     CNT(X)        WHENEQ    1
     C                   Z-ADD     .50           P2BRP                          50%
     C                   ADD       1             CNT(X)
     C                   MOVE      '759'         P2EOMB
     C* EOMB Code 759: "This is the second procedure of multiple
     C*                 procedures billed on this date of service"
     C     CNT(X)        WHENEQ    2
     C                   Z-ADD     .50           P2BRP                          50%
     C                   ADD       1             CNT(X)
     C                   MOVE      '760'         P2EOMB
     C* EOMB Code 760: "This is the third procedure of multiple procedures
     C*                 billed on this date of service"
     C     CNT(X)        WHENEQ    3
     C                   Z-ADD     .50           P2BRP                          50%
     C                   ADD       1             CNT(X)
     C                   MOVE      '761'         P2EOMB
     C* EOMB Code 761: "This is the fourth procedure of multiple procedures
     C*                 billed on this date of service"
     C     CNT(X)        WHENEQ    4
     C                   Z-ADD     .50           P2BRP                          50%
     C                   ADD       1             CNT(X)
     C                   MOVE      '762'         P2EOMB
     C* EOMB Code 762: "This is the fifth procedure of multiple procedures
     C*                 billed on this date of service"
     C     CNT(X)        WHENGE    5
     C                   Z-ADD     0             P2BRP                          0%
     C                   ADD       1             CNT(X)
     C*
     C     CNT(X)        IFGE      6
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     P2DSEQ        QQS(Q)
     C                   MOVE      'FH'          QQC(Q)
     C* Err Msg FH: "Verify medical necessity"
     C                   ENDIF
     C                   Z-ADD     *ZERO         P2TOTP
     C     P2TOTC        SUB       P2TOTP        P2TOTD
     C                   Z-ADD     P2TOTP        P2TOTA
     C                   MOVE      '113'         P2EOMB
     C* EOMB Code 113: "Payment is denied because this procedure exceeds
     C*                 the BWC limit for procedures per date of service."
     C*
     C                   ENDSL                                                  END  GT 6
     C*
     C                   ENDIF                                                  END SVSPCD
     C*
     C                   ENDIF                                                  END P2BRP
     C     P2MRA         MULT      P2BRP         P2MRA
     C* ===============================================================
     C*
     C* If MOD -62, calculate 62.5 of surgery code
     C     P2PMOD        IFEQ      '62'
     C     P2MRA         ANDGT     0
     C     P2MRA         MULT      .625          P2MRA
     C                   ENDIF
     C*
     C* ===============================================================
     C*
     C     P2TOTC        COMP      P2MRA                              54        >>>>>
     C   54              Z-ADD     P2MRA         P2TOTA
     C   54              Z-ADD     *ZEROS        P2DIFF
     C  N54              Z-ADD     P2TOTC        P2TOTA
     C  N54P2MRA         SUB       P2TOTA        P2DIFF
     C     P2TOTC        SUB       P2TOTA        P2TOTD
     C                   Z-ADD     P2TOTA        P2TOTP
     C*
     C*  Using FACAPOH record, chain to FDCAP record to update
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
     C     P2TOTD        IFGT      *ZERO
     C     P2EOMB        ANDEQ     *BLANKS
     C     P2TOTP        ANDGT     *ZEROS
     C* EOMB Code 202: "Charges exceed maximum reimbursement amount."
     C                   MOVE      *BLANKS       XREOMB
      *   CALL API WA900R TO DETERMINE FUND TYPE
      *
     C*
     C     @@TYPE        IFEQ      'SI'
      *
     C                   MOVE      '202'         XPEOMB
     C* EOMB Code 202: "Charges exceed maximum reimbursement amount."
     C*                 reimbursement manual
      *
     C                   ENDIF
     C*****************************************************************
     C                   END                                                    END P2TOTD GT *ZERO
     C*
     C* ===============================================================
     C* USING XXPASS TO DETERMINE ON SECOND ROUND HOW MANY SURGERY CODES
     C* WERE BEING SENT TO SURGERY.  THIS WILL ALLOW EOMB 758 IF MORE
     C* THAN ONE CODE IS BILLED ELSE MOVE BLANKS INTO THAT POSITION.
      *
     C     XXPASS        IFEQ      '1'
     C                   Z-ADD     CNT(X)        EOBCNT
     C                   Z-ADD     COUNT         EOBCN2
     C                   ENDIF
      *
     C* ===============================================================
     C* IF PROCCESING ONLY ONE SURGERY, DON'T DISPLAY EOMB
     C     EOBCNT        IFLE      1
     C     EOBCN2        ANDLE     1
     C     XXPASS        ANDEQ     '2'
      *
     C                   MOVE      *BLANKS       XREOMB
      *   CALL API WA900R TO DETERMINE FUND TYPE
      *
     C*
     C     @@TYPE        IFEQ      'SI'
      *
     C                   MOVE      '202'         XREOMB
     C* EOMB Code 202: "Charges exceed maximum reimbursement amount."
     C*                 reimbursement manual
      *
     C                   ENDIF
      *
     C                   ELSE
     C*
     C                   MOVE      P2EOMB        XREOMB
     C*
     C                   ENDIF
     C* ===============================================================
     C* USING DUPLFG TO DETERMINE EOMB 99 AND 97                       S
      *
     C     DUPFLG        IFEQ      'Y'
      *
     C                   MOVE      P2EOMB        XREOMB
     C                   ENDIF
     C*
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
     C                   Z-ADD     SXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C  N61              UPDATE    PCA08
     C*
     C                   END                                                    END NE 'Y'
     C*
     C                   READ      PCAWK2                                 60
     C                   END                                                    END DOWEQ 0 *IN60
     C*
     C     SURG2E        ENDSR
     *********************************************************************
     *** UPDATE SUBROUTINE                                     U P D A T *
     *********************************************************************
     C     UPDAT         BEGSR
     C*
     C*
     C** CHARGES EXCEED MAXIMUM ALLOWANCE FOUND IN REIMBURSEMENT MANUAL
     C*
     C     XPTOTD        IFGT      *ZERO
     C     XPEOMB        ANDEQ     *BLANK
     C     XPTOTP        ANDGT     *ZERO
     C* EOMB Code 202: "Charges exceed maximum reimbursement amount."
     C*                 reimbursement manual
     C                   MOVE      *BLANKS       XPEOMB
      *   CALL API WA900R TO DETERMINE FUND TYPE
      *
     C*
     C     @@TYPE        IFEQ      'SI'
      *
     C                   MOVE      '202'         XPEOMB
     C* EOMB Code 202: "Charges exceed maximum reimbursement amount."
     C*                 reimbursement manual
      *
     C                   ENDIF
     C*****************************************************************
     C                   END
     C*
     C     XPEOMB        IFNE      *BLANKS
     C     XXPASS        ANDEQ     '2'
      *
     C                   Z-ADD     SVDSEQ        KKDSEQ
     C                   EXSR      EOMB
      *
     C                   ENDIF
     C*
     C*
     C*   IF CODE IS FROM RANGE 99201-99499 AND MODIFIER EQUALS 25
     C*   SKIP ALL EDITS AND PAY ACCORDING TO MRA OR TOTAL CHARGED,
     C*   WHICH EVER IS LOWER.
     C*
     C     XPPMOD        IFEQ      '25'
     C     DHPRCD        ANDGE     '99201'
     C     DHPRCD        ANDLE     '99499'
     C*   WANT TO DISPLAY EOMB 525 OR 523 ABOVE ONLY PHASE 4 SPECS
     C     XPEOMB        OREQ      '525'
     C     XPEOMB        OREQ      '523'
     C     XPPMOD        OREQ      '50'                                         ADD 10/23/97
     C                   GOTO      SKPDUP
     C                   ENDIF
     C*
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM.
     C*
     C                   MOVE      DHPRCD        XXCPT4
     C*
     C     DHKY2         CHAIN     PDH03                              54
     C*
     C     *IN54         DOWEQ     *OFF
     C*
     C                   SELECT
      *
     C     D1MOD1        WHENNE    *BLANKS
     C                   MOVEL     DHMOD1        X1PMOD
      *
     C     D1MOD2        WHENNE    *BLANKS
     C                   MOVEL     DHMOD2        X1PMOD
      *
     C     D1MOD3        WHENNE    *BLANKS
     C                   MOVEL     DHMOD3        X1PMOD
      *
     C     D1MOD4        WHENNE    *BLANKS
     C                   MOVEL     DHMOD4        X1PMOD
      *
     C                   OTHER
     C                   MOVE      *BLANKS       X1PMOD
      *
     C                   ENDSL
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
     C     X1PMOD        ANDNE     '50'
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
     C     SKPDUP        TAG                                                    ADD 07/16/97
     C*
     C     SX1A2         IFEQ      'P'
     C* ONLY ALLOW CODES W0813 AND W0814 WITH A 'P'
     C     SXANAM        ANDGT     0
     C     DHPRCD        IFEQ      'W0183'
     C     DHPRCD        OREQ      'W0184'
     C     SXANAM        MULT      CACHRG        XPTOTA
     C     SXANAM        MULT      CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C     XPTOTD        IFGT      *ZERO
     C     XPEOMB        ANDEQ     *BLANK
     C     XPTOTP        ANDGT     *ZERO
     C*****************    MOVE '202'     XPEOMB
     C* EOMB Code 202: "Charges exceed maximum reimbursement amount."
     C                   MOVE      *BLANKS       XPEOMB
      *   CALL API WA900R TO DETERMINE FUND TYPE
      *
     C*
     C     @@TYPE        IFEQ      'SI'
      *
     C                   MOVE      '202'         XPEOMB
     C* EOMB Code 202: "Charges exceed maximum reimbursement amount."
     C*                 reimbursement manual
      *
     C                   ENDIF
     C*****************************************************************
     C                   END
     C                   ENDIF                                                   END DHPRCD
     C                   ENDIF
     C*
     C** ONLY PROCESS THE RECORD IF NOT A OVERRIDE TYPE.
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
     C** CLEAR THE WORK SAVE FIELDS.
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE         ENDSR
     C*************************************************************************
     OPCA01     E            ECA01
**  QCA QCMDEXC TABLE
DLTOVR FILE(FACALOH)
OVRDBF FILE(FACALOH)  TOFILE(QTEMP/FACALOH)
DLTOVR FILE(FACAPOH)
OVRDBF FILE(FACAPOH)  TOFILE(QTEMP/FACAPOH)
**  ASC GROUP TABLE FILE -- REIMBURSEMENT VALUES DETERMINED BY 1-8
31400
42200
48200
59500
67800
78900
94100
92800
**  NC1 CODES NOT TO CUT LIST 1
11101
11201
11732
11922
15101
15121
15201
15221
15241
15261
15787
16042
17001
17002
17101
17102
17201
19001
19126
19291
22103
22116
22216
22226
22328
22585
22614
22623
22840
22841
22842
22843
22844
22845
22846
22847
22848
22851
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
33924
33961
35390
35700
36218
36248
37206
37208
37251
38102
38746
38747
43635
44015
44121
44139
56606
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
64623
64727
64778
64787
64830
64832
64872
64874
64876
64902
67335
