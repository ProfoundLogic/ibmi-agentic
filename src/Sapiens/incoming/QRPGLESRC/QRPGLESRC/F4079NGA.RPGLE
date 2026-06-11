     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4079RGA                                          *
     H*    DESCRIPTION: ADJUDICATION (GEORGIA RULES - 7/1/97)         *
     H*   This program is called from F0070R (Claim Adjudication) and  ill
     H* perform the actual adjudication of the detail claim records writ-
     H* ten by F0070R.  This program was derived from the Georgia State
     H* Fee Schedule Reimbursment Manual issued on 7/01/96.
     H* This Sub-program is intended to be used to calculate the amount
     H* that is due a Provder of services, by the Carrier of the Work comp
     H* insurance.  It should be used for services provided on or after
     H* 07/01/97 and before 07/01/98.                                 *
     H*****************************************************************
     H*  CREATED: 8/04/97   C:59   PROJECT: GA009                     *
     H*****************************************************************
     H*  SPECIALTY CODES USED IN THIS PROGRAM:                        *
     H*  1.  PHT       = Physical Therapist
     H*  2.  LMT       = Licensed Massage Therapist
     H*  3.  OCT or OT = Occupational Therapist
     H*  4.  PSY       = Psychiatric
     H*  5.  SPE       =
     H*  6.  OCM       =
     H*  7.  OCC       =
     H*  8.  PAI       =
     H*  9.  PR        =
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  08/09/02 C:062 PROJECT: FS244
     H*                 USE CAZIPA.
     H*
     H*  09/25/01 C:062 PROJECT: FSMNT
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.
     H*
     H*  06/08/99 C:79 PROJ: GA011
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
     H*****************************************************************
     H*  INDICATOR USAGE:                                            **
     H*****************************************************************
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMC1L     IF   E           K DISK
     FFMPRLGA   IF   E           K DISK
     FFTABLGA   IF   E           K DISK
     F                                     RENAME(PRC01:PRC0W)
     FFDCAL5    UF   E           K DISK
     F                                     RENAME(PCA01:PCA08)
     FFDCAL9    IF   E           K DISK
     F                                     RENAME(PCA01:PCA09)
     FFDCALH    UF   E           K DISK
     FFDCALC    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0C)
     FFDCALA    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0A)
     FFMRCL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0P)
     F*** BEGIN DEL *** 08/09/02 **************************************
     F**********FMGZL   IF  E           K        DISK
     F*** END   DEL *** 08/09/02 **************************************
     FFACAPGA   O    E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAPT1)
     FFACALGA   IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCAPT2)
     F*
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
     D QQS             S              5    DIM(9999)                            DETAIL SEQ #
     D QQC             S              2    DIM(9999)                            ERROR MESSAGES
     D*
     D NC1             S              5    DIM(110) CTDATA PERRCD(10)           NO CUT ON THESE
     D NC2             S              5    DIM(55) CTDATA PERRCD(10)            NO CUT ON THESE 2
     D SUR1            S              5    DIM(90) CTDATA PERRCD(1)             CUT/MAJOR-MIN
     D SUR2            S              5    DIM(90) ALT(SUR1)
     D SEPP            S              5    DIM(270) CTDATA PERRCD(1)            SEPARATE PROCEDURE
     D TABA            S              5    DIM(35) CTDATA PERRCD(1)             NO CUT/MAJOR-MIN
     D TABB            S              5    DIM(35) ALT(TABA)
     D*
     D TABMU1          S              2    DIM(6) CTDATA PERRCD(1)              MOD UNITS
     D TABMU2          S              1  0 DIM(6) ALT(TABMU1)
     D*
     D QCA             S             55    DIM(4) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     I*
     I*
     I*  Rename the duplicate field names.
     IPRC0W
     I              RCKEY                       RCKEYW
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
     I* Record Format for SURGERY Workfile FACALGA
     IPCAPT2
     I              CACPT4                      P2CPT4
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
     I              CAICD9                      P2ICD9
     I              CAANMN                      P2ANMN
     I              CAUNTS                      P2UNTS
     I              CATOTA                      P2TOTA
     I              CATOTC                      P2TOTC
     I              CATOTD                      P2TOTD
     I              CATOTP                      P2TOTP
     I              CADSEQ                      P2DSEQ
     I              CADELF                      P2DELF
     I              CAFLAG                      P2FLAG
     I              CASTAT                      P2STAT
     I              CAP1                        P2P1
     I              CAPMOD                      P2PMOD
     I              CAUSER                      P2USER
     I              CAPGM                       P2PGM
     I              CADT                        P2DT
     I              CATIME                      P2TIME
     I              CACHK#                      P2CHK#
     I              CAEOB#                      P2EOB#
     I              CAPART                      P2PART
     I              CAEOMB                      P2EOMB
     I              CARCFL                      P2RCFL
     I              CAMRA                       P2MRA
     I              CAMMRA                      P2MMRA
     I              CASMCD                      P2SMCD
     I              CADIFF                      P2DIFF
     I              CATYPE                      P2TYPE
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
     C     CAKEY7        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
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
     C     CAKEY9        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C*
     C     CAKEYA        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXYMD
     C                   KFLD                    SVDSEQ
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
     C     CAKEYZ        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXYMD
     C                   KFLD                    HLDSEQ
     C*
     C     CAKYP         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    KKDSEQ
     C*                                                                *****
     C     CAKYA3        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    P2DSEQ
     C*
     C*  Claim Master File
     C     CMKEY         KLIST
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
     C*  Procedure Code file
     C     PRKEY         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXEFDT
     C*
     C*  Provider Code File
     C     PDKEY         KLIST
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C*
     C* Reasonable & Customary File
     C     RCKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    XXZIPA
     C                   KFLD                    XXCPCD
     C*
     C*** Geographical Zip Area FILE
     C*** BEGIN DEL *** 08/09/02 **************************************
     C********** GZKY      KLIST
     C**********           KFLD           QQCO#
     C**********           KFLD           XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C*
     C*  User Parameter File
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
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
     C     *LIKE         DEFINE    DHPRCD        CPTFR
     C     *LIKE         DEFINE    DHPRCD        CPTTO
     C     *LIKE         DEFINE    CAFDAT        HDYMD
     C     *LIKE         DEFINE    DHUNTS        LHOURS
     C     *LIKE         DEFINE    DHUNTS        PHOURS
     C     *LIKE         DEFINE    CAFDAT        SVYMD
     C     *LIKE         DEFINE    DHPRCD        XXCPT4
     C     *LIKE         DEFINE    DHPRCD        XXCPCD
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    CAFDAT        XXDATE
     C     *LIKE         DEFINE    CAFDAT        XXEFDT
     C*** BEGIN DEL *** 08/09/02 **************************************
     C********** *LIKE     DEFN GZFRZP    XXFRZP
     C*** END   DEL *** 08/09/02 **************************************
     C     *LIKE         DEFINE    CAFDAT        XXYMD
     C     *LIKE         DEFINE    DHAREA        XXPART
     C     *LIKE         DEFINE    CAPMOD        XXPMOD
     C     *LIKE         DEFINE    CARCN         XXRCN
     C*** BEGIN CHG *** 08/09/02 **************************************
     C********** *LIKE     DEFN GZZIPA    XXZIPA
     C     *LIKE         DEFINE    RCZIPA        XXZIPA
     C*** END   CHG *** 08/09/02 **************************************
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
     C     *LIKE         DEFINE    CADIFF        XPDIFF
     C     *LIKE         DEFINE    PMKEY         TABKEY
     C*
     C     *LIKE         DEFINE    CADSEQ        HLDSEQ
     C     *LIKE         DEFINE    CADSEQ        H1DSEQ
     C     *LIKE         DEFINE    CAFEIN        HLFEIN
     C     *LIKE         DEFINE    CAFEIN        HDFEIN
     C     *LIKE         DEFINE    CAMOD         HLMOD
     C     *LIKE         DEFINE    CAMOD         HDMOD
     C     *LIKE         DEFINE    CAPRTY        HLPRTY
     C     *LIKE         DEFINE    CAEOB#        HLEOB#
     C     *LIKE         DEFINE    CACHK#        HLCHK#
     C*
     C     *LIKE         DEFINE    CADSEQ        PTDSEQ
     C     *LIKE         DEFINE    CAFEIN        PTFEIN
     C     *LIKE         DEFINE    CAMOD         PTMOD
     C     *LIKE         DEFINE    CAPRTY        PTPRTY
     C     *LIKE         DEFINE    CAEOB#        PTEOB#
     C     *LIKE         DEFINE    CACHK#        PTCHK#
     C*
     C     *LIKE         DEFINE    CAFEIN        SXFEIN
     C     *LIKE         DEFINE    CAMOD         SXMOD
     C     *LIKE         DEFINE    PRMRA         SXMRA
     C     *LIKE         DEFINE    PRANAM        SXANAM
     C     *LIKE         DEFINE    PRPROF        SXPROF
     C     *LIKE         DEFINE    PRRPT         SXRPT
     C     *LIKE         DEFINE    PRSUPP        SXSUPP
     C     *LIKE         DEFINE    PRSURG        SXSURG
     C     *LIKE         DEFINE    PR1A1         SX1A1
     C*
     C     *LIKE         DEFINE    CAFEIN        XXFEIN
     C     *LIKE         DEFINE    CAMOD         XXMOD
     C     *LIKE         DEFINE    PRMRA         XXAMT
     C     *LIKE         DEFINE    PRMRA         XXDIFF
     C     *LIKE         DEFINE    CADSEQ        SVDSEQ
     C     *LIKE         DEFINE    CAFEIN        SVFEIN
     C     *LIKE         DEFINE    CAMOD         SVMOD
     C     *LIKE         DEFINE    CAPRTY        SVPRTY
     C     *LIKE         DEFINE    DHUNTS        SVUNTS
     C     *LIKE         DEFINE    CAEOB#        SVEOB#
     C     *LIKE         DEFINE    CACHK#        SVCHK#
     C*
     C     *LIKE         DEFINE    CABRTC        XPBRTC
     C     *LIKE         DEFINE    DHPRCD        XPCPT4
     C     *LIKE         DEFINE    CAPMOD        XPPMOD
     C     *LIKE         DEFINE    CAEOMB        XPEOMB
     C*
     C                   Z-ADD     0             Q                 5 0
     C                   Z-ADD     0             S                 3 0
     C                   Z-ADD     0             T                 5 0
     C                   MOVE      '0'           OFCPD             1            CLEAR TMP IN(NOT USD
     C                   MOVE      '0'           XX25EM            1
     C                   Z-ADD     0             P92545           10 2
     C                   Z-ADD     0             L92545           10 2
     C                   Z-ADD     0             P97546            9 2
     C                   Z-ADD     0             L97546            9 2
     C                   Z-ADD     0             P97750            9 2
     C                   Z-ADD     0             MRA1              9 2
     C*** BEGIN CHG *** 09/25/01 **************************************
     C**********           Z-ADD0         XXOVRP  32
     C                   Z-ADD     0             XXOVRP            5 4
     C*** END   CHG *** 09/25/01 **************************************
     C                   Z-ADD     0             XPTOTA            9 2
     C                   Z-ADD     0             XPTOTD            9 2
     C                   Z-ADD     0             XPTOTP            9 2
     C                   Z-ADD     0             XXALOW            9 2
     C                   Z-ADD     0             XXMODU            2 0
     C                   Z-ADD     0             XXWORK            6 2
     C                   Z-ADD     0             XXANMN            5 2
     C                   Z-ADD     0             COUNT             5 0
     C                   Z-ADD     0             XXMRA             9 2
     C                   Z-ADD     0             ZZMRA             9 2
     C                   Z-ADD     0             XXPERC            5 2
     C                   Z-ADD     0             XXUNTS            7 4
     C                   Z-ADD     0             X1ANMN            7 4
     C                   Z-ADD     0             XXUNT             7 4
     C*
     C                   MOVE      *BLANK        OPNFLG            1
     C                   MOVE      *BLANK        OVRFLG            1
     C                   MOVE      ' '           XFIRST            1            First time into pgm
     C                   MOVE      *ZEROS        QQC
     C                   MOVE      *ZEROS        QQS
     C*
     C* ===============================================================
     C*
     C*  Get the user parameter file
     C     UPKEY         CHAIN     PUP01                              60
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
     C     CMKEY         CHAIN     PCM01                              60
     C*
     C*  If the get was unsuccessful, end the program
     C     *IN60         IFEQ      '1'
     C                   MOVE      '99998'       QQS(1)
     C                   MOVE      '96'          QQC(1)
     C* Err Msg 96: Invalid Case Number, See Programmer
     C                   GOTO      TERM
     C                   END
     C*
     C* ==================================================================
     C*
     C*** XXPASS - USED AS FLAG TO ZERO OUT PAID FIELDS WHEN ERRORS
     C*** OCCUR ON FIRST PASS OF DATA
     C*** XXPASS, 1 = FIRST PASS;  2 = SECOND PASS
     C*
     C                   MOVE      '1'           XXPASS            1
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
     C                   MOVE      *BLANKS       XPEOMB
     C*
     C                   EXCEPT    ECA01
     C*
     C     CAFDAT        IFGE      970701
     C     CAFDAT        ANDLT     980701
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
     C                   Z-ADD     *ZEROS        P92545                         ACCUM 92545 PAID
     C                   Z-ADD     *ZEROS        L92545                         AMT LEFT OF 92545
     C*
     C                   Z-ADD     0             MRA1
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
     C     RCKEY         SETGT     PRC01
     C     NXRC1         TAG
     C                   READP     PRC01                                  60
     C     *IN60         IFEQ      '0'
     C     RCCO#         ANDEQ     QQCO#
     C     RCCPT4        ANDEQ     XXCPCD
     C     RCSTAT        IFNE      'I'
     C*
     C*** IF PROCEDURE IS "BY-REPORT" FLAG IT AS SUCH AND UPDATE RECORD.
     C*
     C     RCRPT         IFEQ      'Y'
     C*
     C*** IF MODIFIER EXISTS CHECK THE CODE THROUGH THE MODIFIER SEC.
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTP
     C  N54              Z-ADD     CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
     C                   Z-ADD     1             Q                 5 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   ELSE                                                   ELSE RCRPT EQ 'Y'
     C     CACHRG        COMP      RCMRA                              54
     C   54              MOVE      '229'         XPEOMB
     C* EOMB Code 229: "This charge has been paid per R&C fees"
     C   54              Z-ADD     RCMRA         XPTOTP
     C  N54              Z-ADD     CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   END                                                    END RCRPT EQ 'Y'
     C*
     C                   ELSE
     C                   GOTO      NXRC1
     C                   END
     C                   END
     C*
     C     CAKEYA        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   END
     C                   END
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
     C     PRKEY         SETGT     PPR01                                         GET PROC CODE
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
     C                   Z-ADD     CACHRG        XPTOTP
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   ELSE
     C*
     C*  If fee schedule reimbursement equals 'N' But the MRA
     C*    is greater than zero, this CPT4 Code has been assigned a
     C*    specific MRA by the insurance company.
     C     CACHRG        COMP      PRMRA                              54
     C   54              MOVE      '202'         XPEOMB
     C* EOMB Code 202: "Charges exceed Reasonable & Customary"
     C   54              Z-ADD     PRMRA         XPTOTP
     C  N54              Z-ADD     CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   END
     C                   END
     C*
     C*== FS Reimbursement = "Y" ======================================
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
     C*
     C*== By Report ===================================================
     C*
     C     PRRPT         IFEQ      'Y'
     C*
     C*   If modifier exists check the code through the Modifier Subr
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTP
     C  N54              Z-ADD     CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C* Err Msg 06: "The item is by report - requires further
     C*             inspection by an adjuster"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END
     C*
     C*== ANE/Amount Not Established ==================================
     C*
     C     PRRPT         IFEQ      'A'
     C*
     C*   If modifier exists check the code through the Modifier Subr
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTP
     C  N54              Z-ADD     CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPBRTC
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
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END
     C*
     C*== Normal Processing of Procedure Code =========================
     C*   Execute all Subroutines here.
     C*
     C*
     C*  PAGE 105
     C*  Physical therapist, Occupational Therapist and Speech Therapist
     C*  are reimbursed according to fee schedule CPT codes plus $30.00
     C*  for each visit.
     C*
     C*  if specialty combination and pos combination return error.
     C*
     C     DHPOS         IFEQ      '8'
     C     DHPOS         OREQ      '7'
     C*
     C     QQSPCD        IFEQ      'PHT'
     C     QQSPCD        OREQ      'SPE'
     C     QQSPCD        OREQ      'OCT'
     C     QQSPCD        OREQ      'OT'
     C     QQSPCD        OREQ      'OCM'
     C     QQSPCD        OREQ      'OCC'
     C     QQSPCD        OREQ      'PAI'
     C     QQSPCD        OREQ      'PR '
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GB'          QQC(Q)
     C* Err Msg GB: "Pay additional $30.00 per home health visit."
     C                   ENDIF                                                  END QQSPCD
     C                   ENDIF                                                  END DHPOS
     C*****************************************************************
     C*
     C     DHPRCD        IFGE      '00100'                                      ANESTHESIA
     C     DHPRCD        ANDLE     '01995'
     C     DHPRCD        ORGE      '99100'
     C     DHPRCD        ANDLE     '99140'
     C                   EXSR      ANES
     C                   ELSE
     C     DHPRCD        IFGE      '10100'                                      SURGICAL
     C     DHPRCD        ANDLE     '69999'
     C* If first time into program,
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the program is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALGA
     C                   CLOSE     FACAPGA
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALGA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPGA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURGERY Codes
     C                   CALL      'CRTFACGA'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALGA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACALPGA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALGA
     C                   OPEN      FACAPGA
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
     C                   EXSR      SURG2
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALGA
     C                   CLOSE     FACAPGA
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALGA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACALPGA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACGA'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALGA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACALPGA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALGA
     C                   OPEN      FACAPGA
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
     C                   ELSE
     C     DHPRCD        IFGE      '70000'                                      RADIOLOGICAL
     C     DHPRCD        ANDLE     '79999'
     C                   EXSR      RADI
     C                   ELSE
     C     DHPRCD        IFGE      '80000'                                      PATHOLOGY
     C     DHPRCD        ANDLE     '89999'
     C                   EXSR      PATH
     C                   ELSE
     C     DHPRCD        IFGE      '90703'                                      OFFICE VISITS
     C     DHPRCD        ANDLE     '90900'
     C     DHPRCD        ORGE      '99201'
     C     DHPRCD        ANDLE     '99499'
     C     DHPRCD        OREQ      '99075'
     C                   EXSR      OFFI
     C                   ELSE
     C     DHPRCD        IFGE      '92002'                                      OPHTHAMOLOGCIAL
     C     DHPRCD        ANDLE     '92280'
     C                   EXSR      OPTH
     C                   ELSE
     C     DHPRCD        IFGE      '92541'                                      DIAGNOSTIC
     C     DHPRCD        ANDLE     '95935'
     C                   EXSR      DIAG
     C                   ELSE
     C     DHPRCD        IFGE      '97010'                                      PHYS. THERAPY
     C     DHPRCD        ANDLE     '97799'
     C                   EXSR      PHYS
     C                   ELSE
     C                   EXSR      OTHE                                         OTHER CODES
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
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
     C     CAKEYA        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C*   If override type, we must still output the allowed/disallowed
     C     CAMRA         IFEQ      0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CATOTA        ANDEQ     0
     C     CACHRG        ANDEQ     0
     C*
     C                   Z-ADD     0             XPTOTA
     C*   If there is an override percentage, calculate
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CACHRG        MULT      XXOVRP        XPTOTA
     C                   END
     C*
     C     CACHRG        SUB       XPTOTA        CAMRA
     C                   Z-ADD     XPTOTA        CACHRG
     C                   END
     C                   Z-ADD     *ZEROS        CADIFF
     C                   Z-ADD     *ZEROS        CABRTC
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
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
     C     OPNFLG        IFEQ      '1'
     C                   EXSR      SURG2
     C                   END
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
     C* INITIALIZE QQS, AND QQC
     C                   MOVE      *ZEROS        QQS
     C                   MOVE      *ZEROS        QQC
     C*
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
     C                   MOVE      *BLANKS       XPEOMB
     C*
     C                   EXCEPT    ECA01
     C*
     C     CAFDAT        IFGE      970701
     C     CAFDAT        ANDLT     980701
     C     XXRCN         ANDEQ     CARCN
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
     C                   Z-ADD     *ZEROS        P92545                         ACCUM 92545 PAID
     C                   Z-ADD     *ZEROS        L92545                         AMT LEFT OF 92545
     C*
     C                   Z-ADD     0             MRA1
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
     C     RCKEY         SETGT     PRC01
     C     NXRC2         TAG
     C                   READP     PRC01                                  60
     C     *IN60         IFEQ      '0'
     C     RCCO#         ANDEQ     QQCO#
     C     RCCPT4        ANDEQ     XXCPCD
     C     RCSTAT        IFNE      'I'
     C*
     C*** IF PROCEDURE IS "BY-REPORT" FLAG IT AS SUCH AND UPDATE RECORD.
     C*
     C     RCRPT         IFEQ      'Y'
     C*
     C*** IF MODIFIER EXISTS CHECK THE CODE THROUGH THE MODIFIER SEC.
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTP
     C  N54              Z-ADD     CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       GET NEXT RECORD
     C                   ELSE
     C*
     C                   Z-ADD     1             Q                 5 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       GET NEXT RECORD
     C*
     C                   END
     C                   ELSE                                                   ELSE RCRPT EQ 'Y'
     C     CACHRG        COMP      RCMRA                              54
     C   54              MOVE      '229'         XPEOMB
     C* EOMB Code 229: "This charge has been paid per R&C fees"
     C   54              Z-ADD     RCMRA         XPTOTP
     C  N54              Z-ADD     CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   END                                                    END RCRPT EQ 'Y'
     C*
     C                   ELSE
     C                   GOTO      NXRC2
     C                   END
     C                   END
     C*
     C     CAKEYA        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1
     C*
     C                   END
     C                   END
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
     C                   GOTO      NXTRC1
     C*
     C*== Non-Hospital ================================================
     C*
     C                   ELSE                                                   Else eq '4'
     C*
     C*  Test for Fee Schedule Reimbursement = 'Y'
     C                   MOVEL     DHPRCD        XXCPT4
     C                   MOVE      CAFDAT        XXEFDT
     C*
     C     PRKEY         SETGT     PPR01                                         GET PROC CODE
     C     NXTPR3        TAG
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
     C     PRSTAT        CABEQ     'I'           NXTPR3
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
     C                   Z-ADD     CACHRG        XPTOTP
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       GET NEXT RECORD
     C*
     C                   ELSE
     C*
     C*  If fee schedule reimbursement equals 'N' But the MRA
     C*    is greater than zero, this CPT4 Code has been assigned a
     C*    specific MRA by the insurance company.
     C     CACHRG        COMP      PRMRA                              54
     C   54              MOVE      '202'         XPEOMB
     C* EOMB Code 202: "Charges exceed Reasonable & Customary"
     C   54              Z-ADD     PRMRA         XPTOTP
     C  N54              Z-ADD     CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       GET NEXT RECORD
     C                   END
     C                   END
     C*
     C*== FS Reimbursement = "Y" ======================================
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
     C*
     C*== By Report ===================================================
     C*
     C     PRRPT         IFEQ      'Y'
     C*
     C*   If modifier exists check the code through the Modifier Subr
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTP
     C  N54              Z-ADD     CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       GET NEXT RECORD
     C                   ELSE
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C* Err Msg 06: "The item is by report - requires further
     C*             inspection by an adjuster"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       GET NEXT RECORD
     C*
     C                   END
     C                   END
     C*
     C*== ANE/Amount Not Established ==================================
     C*
     C     PRRPT         IFEQ      'A'
     C*
     C*   If modifier exists check the code through the Modifier Subr
     C     XPPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTP
     C  N54              Z-ADD     CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       GET NEXT RECORD
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
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRC1                                       GET NEXT RECORD
     C*
     C                   END
     C                   END
     C*
     C*== Normal Processing of Procedure Code =========================
     C*   Execute all Subroutines here.
     C*
     C*
     C*  PAGE 105
     C*  Physical therapist, Occupational Therapist and Speech Therapist
     C*  are reimbursed according to fee schedule CPT codes plus $30.00
     C*  for each visit.
     C*
     C*  if specialty combination and pos combination return error.
     C*
     C     DHPOS         IFEQ      '8'
     C     DHPOS         OREQ      '7'
     C*
     C     QQSPCD        IFEQ      'PHT'
     C     QQSPCD        OREQ      'SPE'
     C     QQSPCD        OREQ      'OCT'
     C     QQSPCD        OREQ      'OT'
     C     QQSPCD        OREQ      'OCM'
     C     QQSPCD        OREQ      'OCC'
     C     QQSPCD        OREQ      'PAI'
     C     QQSPCD        OREQ      'PR '
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GB'          QQC(Q)
     C* Err Msg GB: "Pay additional $30.00 per home health visit."
     C                   ENDIF                                                  END QQSPCD
     C                   ENDIF                                                  END DHPOS
     C*****************************************************************
     C*
     C     DHPRCD        IFGE      '00100'                                      ANESTHESIA
     C     DHPRCD        ANDLE     '01995'
     C     DHPRCD        ORGE      '99100'
     C     DHPRCD        ANDLE     '99140'
     C                   EXSR      ANES
     C                   ELSE
     C     DHPRCD        IFGE      '10100'                                      SURGICAL
     C     DHPRCD        ANDLE     '69999'
     C* If first time into program,
     C     XFIRST        IFEQ      ' '
     C*     then, mark that the first time into the program is complete
     C                   MOVE      'Y'           XFIRST
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALGA
     C                   CLOSE     FACAPGA
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALGA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACAPGA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for SURGERY Codes
     C                   CALL      'CRTFACGA'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALGA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACALPGA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALGA
     C                   OPEN      FACAPGA
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
     C                   EXSR      SURG2
     C*
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALGA
     C                   CLOSE     FACAPGA
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALGA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACALPGA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C*     then, create Detail Charge Workfile for Phys Therapy Codes
     C                   CALL      'CRTFACGA'
     C*
     C* Override to workfile in QTEMP
     C     OVRFLG        IFEQ      ' '
     C*     FACALGA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*     FACALPGA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           OVRFLG
     C                   END
     C*
     C* Open file for processing
     C     OPNFLG        IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FACALGA
     C                   OPEN      FACAPGA
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
     C                   ELSE
     C     DHPRCD        IFGE      '70000'                                      RADIOLOGICAL
     C     DHPRCD        ANDLE     '79999'
     C                   EXSR      RADI
     C                   ELSE
     C     DHPRCD        IFGE      '80000'                                      PATHOLOGY
     C     DHPRCD        ANDLE     '89999'
     C                   EXSR      PATH
     C                   ELSE
     C     DHPRCD        IFGE      '90703'                                      OFFICE VISITS
     C     DHPRCD        ANDLE     '90900'
     C     DHPRCD        ORGE      '99201'
     C     DHPRCD        ANDLE     '99499'
     C     DHPRCD        OREQ      '99075'
     C                   EXSR      OFFI
     C                   ELSE
     C     DHPRCD        IFGE      '92002'                                      OPHTHAMOLOGCIAL
     C     DHPRCD        ANDLE     '92280'
     C                   EXSR      OPTH
     C                   ELSE
     C     DHPRCD        IFGE      '92541'                                      DIAGNOSTIC
     C     DHPRCD        ANDLE     '95935'
     C                   EXSR      DIAG
     C                   ELSE
     C     DHPRCD        IFGE      '97010'                                      PHYS. THERAPY
     C     DHPRCD        ANDLE     '97799'
     C                   EXSR      PHYS
     C                   ELSE
     C                   EXSR      OTHE                                         OTHER CODES
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
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
     C     CAKEYA        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C*   If override type, we must still output the allowed/disallowed
     C     CAMRA         IFEQ      0
     C     CAOTHR        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CATOTA        ANDEQ     0
     C     CACHRG        ANDEQ     0
     C*
     C                   Z-ADD     0             XPTOTA
     C*   If there is an override percentage, calculate
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CACHRG        MULT      XXOVRP        XPTOTA
     C                   END
     C*
     C     CACHRG        SUB       XPTOTA        CAMRA
     C                   Z-ADD     XPTOTA        CACHRG
     C                   END
     C                   Z-ADD     *ZEROS        CADIFF
     C                   Z-ADD     *ZEROS        CABRTC
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA01
     C*
     C                   END
     C                   END                                                    END EQ OVRRD
     C                   ENDIF                                                  CAFDAT IFGE
     C*
     C*** Process next record *****************************************
     C     NXTRC1        TAG
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
     C     OPNFLG        IFEQ      '1'
     C                   EXSR      SURG2
     C                   END
     C*****************************************************************
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
     C*** Close file, if opened
     C     OPNFLG        IFEQ      '1'
     C                   CLOSE     FACALGA
     C                   CLOSE     FACAPGA
     C                   MOVE      ' '           OPNFLG
     C                   END
     C*
     C* Delete override
     C     OVRFLG        IFEQ      '1'
     C*     to FACALGA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*     to FACALPGA
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           OVRFLG
     C                   END
     C*
     C* Write adjudication messages to FDMGP
     C                   EXSR      MSG
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     0             XXMRA
     C*** END ADD *** 09/04/98 ****************************************
     C                   RETURN
     C                   MOVE      '1'           *INLR
     *********************************************************************
     *** ANESTHESIA SUBROUTINE CODES 00100 - 01999               A N E S *
     *********************************************************************
     C     ANES          BEGSR
     C*
     C*   Valid modifiers are "22", "23", "51", and  P1-6.
     C*
     C*   Calculate the ARA(ANESTHESIA REIMBURSMENT ALLOWANCE) by this
     C*   formula: ARA=(BASIC VALUE + TIME UNITS) * CONVERSION FACTOR($27)
     C*   WITH A MODIFIER: ARA=(BASIC VAL + TIME + MOD VAL) * CONVR. FACT($27)
     C*
     C*--- Procedure Codes with modifiers ---------------------------------
     C     XPPMOD        IFNE      *BLANK
     C     XPPMOD        IFNE      '22'
     C     XPPMOD        ANDNE     '23'
     C     XPPMOD        ANDNE     '51'
     C     XPPMOD        ANDNE     'P1'
     C     XPPMOD        ANDNE     'P2'
     C     XPPMOD        ANDNE     'P3'
     C     XPPMOD        ANDNE     'P4'
     C     XPPMOD        ANDNE     'P5'
     C     XPPMOD        ANDNE     'P6'
     C*
     C*  Error: Invalid modifier entered, clear the charge, snd error
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      '001'         XPEOMB
     C*
MG11 C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C*
     C                   ELSE
     C*
     C*   Calculate ARA for all valid proc/modifiers
     C     CACO#         CHAIN     PC101                              60
     C   60              Z-ADD     *ZERO         C1ANER
     C*
     C                   Z-ADD     *ZERO         XXMODU                          MODIFIER UNIT VALUE
     C*
     C*   Per page 85: anesthesia rate for P1-P6:
     C*      ((BASIC VALUE + UNITS) + MOD UNITS) X 27.00 = MRA
     C     XPPMOD        IFEQ      'P1'
     C     XPPMOD        OREQ      'P2'
     C     XPPMOD        OREQ      'P3'
     C     XPPMOD        OREQ      'P4'
     C     XPPMOD        OREQ      'P5'
     C     XPPMOD        OREQ      'P6'
     C*
     C     XPPMOD        LOOKUP    TABMU1        TABMU2                   60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     TABMU2        XXMODU
     C                   END                                                    END 1 *IN60
     C                   END                                                    END EQ P1-P6
     C*
     C     SXANAM        ADD       DHUNTS        XXWORK
     C                   ADD       XXMODU        XXWORK
     C     XXWORK        MULT      27.00         XXMRA                          27.00 IS C1ANER
     C*
     C*   Calculate normal proc mod values based on above XPTOTA
     C                   EXSR      PMOD
     C*
     C*   Calculate other detail charge fields
     C     XXMRA         IFLT      CACHRG
     C                   Z-ADD     XXMRA         XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPBRTC
     C                   MOVE      '002'         XPEOMB
     C* EOMB Code 002: "Charges exceed Maximum Reimbursement Allowance"
     C                   ELSE                                                   ELSE LT CACHRG
     C                   Z-ADD     CACHRG        XPTOTP
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPBRTC
     C                   END                                                    END LT CACHRG
     C*
     C*   Update detail charge record, then end subroutine
     C                   EXSR      UPDAT
     C                   GOTO      ANESE
     C                   END                                                    END 22/23/48
     C*
     C                   END                                                    END NE *ZERO
     C*
     C*--- Procedure codes without modifiers----------------------------------
     C     CACO#         CHAIN     PC101                              60
     C   60              Z-ADD     *ZERO         C1ANER
     C*
     C*   Per page 85: Qualifying circumstances are not multipled by Time
     C     DHPRCD        IFGE      '99100'
     C     DHPRCD        ANDLE     '99140'
     C                   Z-ADD     0             XXANMN
     C                   ELSE                                                   ELSE GE 99100
     C                   Z-ADD     DHUNTS        XXANMN
     C                   END                                                    END GE 99100
     C*
     C     SXANAM        ADD       XXANMN        XXWORK
     C*
     C     XXWORK        MULT      27.00         XXMRA                          27.00 IS C1ANER
     C*
     C     XXMRA         IFLT      CACHRG
     C                   Z-ADD     XXMRA         XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPBRTC
     C                   MOVE      '002'         XPEOMB
     C* EOMB Code 002: "Charges exceed Maximum Reimbursement Allowance"
     C*
     C                   ELSE
     C                   Z-ADD     CACHRG        XPTOTP
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPBRTC
     C*
     C                   END
     C*
     C                   EXSR      UPDAT
     C*
     C     ANESE         ENDSR
     *********************************************************************
     *** DIAGNOSTIC SERVICES          CODES 92541-95935          D I A G *
     *********************************************************************
     C     DIAG          BEGSR
     C                   Z-ADD     SXMRA         XXMRA
     C*
     C* PER SH  SURGERY - Separate procedures.
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
     C     XRCPT4        IFEQ      DHPRCD
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRCPT4        ORNE      DHPRCD
     C     XRTOTP        IFGT      0
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
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
     C* -----------------------------------------------------------------------
     C*
     C*   Per pg. 26: Codes 92541, 92542, 92543, 92544 and 92545
     C*               total 250.00 use 92545 for the value
     C*               Check for multiple 9254* records.
     C*               Total reimbursement cannot exceed 250.00
     C*
     C     DHPRCD        IFGE      '92541'
     C     DHPRCD        ANDLE     '92545'
     C                   MOVE      '92541'       XXCPT4
     C                   Z-ADD     0             COUNT
     C*
     C     DHKY2         SETLL     PDH03
     C     DHKY3         READE     PDH03                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C                   Z-ADD     D1DSEQ        KKDSEQ
     C*
     C     CAKYP         CHAIN     PCA0P                              60
     C*
     C     D1PRCD        IFGE      '92541'
     C     D1PRCD        ANDLE     '92545'
     C*
     C     CADSEQ        IFNE      D1DSEQ
     C     PXTOTP        ANDGT     *ZERO
     C     PXFDAT        ANDEQ     XXYMD
     C     PXFEIN        ANDEQ     CAFEIN
     C     PXMOD         ANDEQ     CAMOD
     C                   ADD       PXTOTP        P92545
     C                   END                                                    END NE/EQ/NE
     C                   END                                                    END X6CPT4
     C*
     C     DHKY3         READE     PDH03                                  54
     C                   END                                                    END DOWEQ 54
     C*
     C*   Disallow charges                                 ONLY ALLOW UPTO 150
     C     250.00        SUB       P92545        L92545
     C     P92545        IFGT      *ZEROS
     C     L92545        IFLE      *ZERO
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPDIFF
     C                   Z-ADD     *ZERO         XPBRTC
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'ER'          QQC(Q)                         250.00 TOT FOR 9254*
     C* Err Msg ER: "The total MRA of procedures 92541-92545 should
     C*              not exceed 250.00"
     C*
     C                   ELSE                                                   ELSE L92545 LE *ZERO
     C     CACHRG        IFGT      L92545
     C                   Z-ADD     L92545        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZERO         XPDIFF
     C                   Z-ADD     L92545        XPBRTC
     C                   MOVE      '002'         XPEOMB
     C* EOMB code 002: " Charges exceed Maximum Reimbursment Allowance"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'ER'          QQC(Q)                         150.00 TOT FOR 9254*
     C* Err Msg ER: "The total MRA of procedures 92541-92545 should
     C*              not exceed 250.00"
     C*
     C                   ELSE                                                   ELSE CACHRG GT L9750
     C                   Z-ADD     CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZERO         XPDIFF
     C                   Z-ADD     L92545        XPBRTC
     C                   MOVE      *BLANKS       XPEOMB
     C                   END                                                    END CACHRG GT L92545
     C                   END                                                    END L92545 LE *ZERO
     C                   EXSR      UPDAT
     C                   GOTO      DIAGE
     C                   END                                                    END GT *ZERO
     C                   END                                                    END EQ 92541
     C*
     C* -----------------------------------------------------------------------
     C*
     C*   Per pg. 28: Check for multiple 95925 - only one 95925 allowed
     C*               on same day.  Disallow 2nd and subsequent codes
     C*
     C     DHPRCD        IFEQ      '95925'
     C                   MOVE      DHPRCD        XXCPT4
     C                   MOVEL     XPPMOD        XXPMOD
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
     C*   Disallow charges
     C     COUNT         IFGE      1
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      *BLANKS       XPEOMB
     C                   MOVE      '009'         XPEOMB
     C*  EOMB Code 009: "Exceeds number of allowable procedures
     C*                  in reimbursement manual"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      DIAGE
     C                   END                                                    END COUNT GE 1
     C                   END                                                    END EQ 95925
     C*
     C* -----------------------------------------------------------------------
     C*
     C*   Per pg. 27:    93760 uses body parts to determine the MRA.
     C*                  User will be given messages to determine.     messages
     C*
     C     DHPRCD        IFEQ      '93760'
     C     XPPMOD        ANDEQ     *BLANKS
     C*
     C     DHAREA        IFEQ      *BLANKS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '32'          QQC(Q)
     C* Err Msg 32: "Procedure is paid at "Total body" value.
     C*              If needed, reduce paid amount"
     C*
     C                   Z-ADD     1             Q                 5 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '33'          QQC(Q)
     C* Err Msg 33: "Upper body - 325.00, Face & Head - 175.00,
     C*              Neck & TMJ - 300.00"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '34'          QQC(Q)
     C* Err Msg 34: "TMJ - 90.00, Lumbar Sacral incl. Legs - 425.00
     C*              Total body - 650.00"
     C*
     C                   ELSE
     C* IF BODY PART IS ENTERED, THEN THESE RATES ARE USED
     C*
     C     DHAREA        IFEQ      '7 '
     C                   Z-ADD     325.00        XXMRA
     C                   END
     C     DHAREA        IFEQ      '8 '
     C                   Z-ADD     175.00        XXMRA
     C                   END
     C     DHAREA        IFEQ      '9 '
     C                   Z-ADD     300.00        XXMRA
     C                   END
     C     DHAREA        IFEQ      '10'
     C                   Z-ADD     90.00         XXMRA
     C                   END
     C     DHAREA        IFEQ      '11'
     C                   Z-ADD     425.00        XXMRA
     C                   END
     C     DHAREA        IFEQ      '12'
     C                   Z-ADD     650.00        XXMRA
     C                   END
     C*
     C*     SETLL and READ the file
     C                   MOVE      '93760'       XXCPT4
     C                   Z-ADD     *ZERO         XXTOTP            9 2
     C                   Z-ADD     *ZERO         XXDIFF
     C     CAKY9         SETLL     PCA09                              54
     C     CAKY9A        READE     PCA09                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C     XRCPT4        IFEQ      '93760'
     C     XRDSEQ        ANDNE     CADSEQ
     C                   ADD       XRTOTP        XXTOTP
     C                   ENDIF                                                  END XRCPT4
     C*
     C     CAKY9A        READE     PCA09                                  54
     C                   END                                                    END 0 DOWEQ
     C*
     C     XXTOTP        IFGT      *ZEROS
     C     650.00        SUB       XXTOTP        XXDIFF
     C*
     C     XXDIFF        IFLE      *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'ES'          QQC(Q)
     C*** Err Msg ES: "The maximum allowed for code 93760 is $650"
     C*
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '255'         XPEOMB
     C*** EOMB 255: "Reimbursement reflects difference between
     C***            previously paid procedure".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      DIAGE
     C                   ENDIF                                                  END XXDIFF
     C*
     C     CACHRG        IFLT      XXMRA
     C                   Z-ADD     CACHRG        XXMRA
     C                   ENDIF                                                  END CACHRG
     C     XXMRA         IFGT      XXDIFF
     C                   Z-ADD     XXDIFF        XXMRA
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'ES'          QQC(Q)
     C*** Err Msg ES: "The maximum allowed for code 93760 is $650"
     C*
     C*
     C                   MOVE      '255'         XPEOMB
     C*** EOMB 255: "Reimbursement reflects difference between
     C***            previously paid procedure".
     C*
     C                   ENDIF                                                  END XXMRA
     C                   ENDIF                                                  END XXTOTP
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C* ===============================================================
     C***** PER SH REMOVE MINUTE CALC FROM THESE CODES
     C*****
     C* pg. 27: Hourly Charges
     C* The following codes will use the "Anes/Mi/Hr Amt" MRA value:
     C*****      DHPRCD    IFEQ '95880'
     C*****      DHPRCD    OREQ '95882'
     C*****      DHPRCD    OREQ '95883'
     C*****      DHUNTS    DIV  60        XXUNTS    H
     C* Determine "MRA" for Hourly Charges
     C*****      XXUNTS    MULT SXANAM    XXMRA     H
     C*****
     C*****                ELSE                            ELS EQ 95880...
     C*****
     C*****                Z-ADDSXMRA     XXMRA
     C*****
     C*****                END                             END EQ 95880...
     C* ===============================================================
     C*
     C*   Perform the calculation for adjudication.
     C*
     C*   If modifier exists, execute the mod check subroutine
     C     XPPMOD        CASGT     *BLANK        PMOD
     C                   END
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTP
     C  N54              Z-ADD     CACHRG        XPTOTP
     C*
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPBRTC
     C*
MG11 C                   EXSR      UPDAT
     C*
     C     DIAGE         ENDSR
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
     C                   Z-ADD     *ZEROS        P92545                         ACCUM 97530 PAID
     C                   Z-ADD     *ZEROS        L92545                         AMT LEFT OF 97530
     C*
     C*
     C                   Z-ADD     HLDSEQ        CADSEQ                         SAVE FIELD VALS
     C                   MOVE      HLFEIN        CAFEIN                         FOR RECORD WE
     C                   MOVE      HLMOD         CAMOD                          ARE PROCESSING
     C                   MOVE      HLPRTY        CAPRTY
     C                   Z-ADD     HLEOB#        CAEOB#
     C                   Z-ADD     HLCHK#        CACHK#
     C*   Reget record.
     C     CAKEYZ        CHAIN     PCA01                              54
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
     C                   MOVE      CAFEIN        SXFEIN
     C                   MOVE      CAMOD         SXMOD
     C*
     C*
     C     NEWPRE        ENDSR
     *********************************************************************
     *** OFFICE VISITS     SUBROUTINE CODES 90703 - 90900        O F F I *
     ***                                    99201 - 99499, 99075
     *********************************************************************
     C*
     C     OFFI          BEGSR
     C*
     C                   Z-ADD     SXMRA         XXMRA
     C*
     C*   Find specialty code of physician:
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*- Physical Therapists - Restrictions ------------------------------------
     C*
     C     QQSPCD        IFEQ      'PHT'
     C     QQSPCD        OREQ      'LMT'
     C     QQSPCD        OREQ      'OCT'
     C     QQSPCD        OREQ      'OT'
     C*
     C*  Per pg. 8: "There will be no reimbursement for physical
     C*             therapists for office visits."
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      '001'         XPEOMB
     C* EOMB Code 001: "Service is not reimburseable under
     C*                Workers' Compensation program"
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END                                                    END  EQ PHT/LMT/OT
     C*
     C*-- Psychiatric Office Visits --------------------------------------------
     C*
     C*  Per pg 24-25: Physicians whose practice is limited to Psychiatric Care
     C*       PSYCHIATRIC  (PSY)
     C*   Shall use the following codes and fees when billing for these procs
     C*
     C     QQSPCD        IFNE      'PSY'
     C*
     C     DHPRCD        IFEQ      '90801'
     C     DHPRCD        OREQ      '90825'
     C     DHPRCD        OREQ      '90830'
     C     DHPRCD        OREQ      '90841'
     C     DHPRCD        OREQ      '90843'
     C     DHPRCD        OREQ      '90844'
     C     DHPRCD        OREQ      '90849'
     C     DHPRCD        OREQ      '90853'
     C     DHPRCD        OREQ      '90862'
     C     DHPRCD        OREQ      '90870'
     C     DHPRCD        OREQ      '90880'
     C     DHPRCD        OREQ      '90882'
     C     DHPRCD        OREQ      '90887'
     C     DHPRCD        OREQ      '90889'
     C     DHPRCD        OREQ      '90900'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '40'          QQC(Q)
     C* Err Msg 40: "Invalid office procedure code used for
     C*              specialty type of provider"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      '234'         XPEOMB
     C* EOMB Code 234: "Invalid office code for this provider's specialty"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C*
     C                   END                                                    END EQ 90801
     C*
     C                   END                                                    END NE PSY
     C*
     C*-- Medical Testimony ----------------------------------------------------
     C*
     C*   Per page 23 OF '96 RULES: 99075 Medical Testimony:
     C*      1st hour or part thereof............250.00
     C*      Each addt'l hour or part thereof....100.00
     C     DHPRCD        IFEQ      '99075'
     C                   MOVE      '99075'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  54
     C                   Z-ADD     *ZERO         COUNT
     C     *IN54         DOWEQ     '0'
     C     XRCPT4        IFEQ      '99075'
     C     CADSEQ        IFNE      XRDSEQ
     C     XRTOTP        IFGT      *ZERO
     C                   ADD       1             COUNT
     C                   END                                                    END GT *ZERO
     C                   END                                                    END NE XRDSEQ
     C                   END                                                    END NE XRCPT4
     C     CAKEY7        READE     PCA09                                  54
     C                   END                                                    END DOWEQ 54
     C     COUNT         IFGE      1
     C                   Z-ADD     100           XXMRA
     C                   ELSE                                                   ELS GE 2
     C                   Z-ADD     250           XXMRA
     C                   END                                                    END GE 2
     C*
     C                   END                                                    END EQ '99075'
     C*
     C*** Per page 23 OF '96 RULES: 90782 Theraputic or diagnostic injection
     C**  can be reimbursed MRA of $10 plus charge for medication.
     C     DHPRCD        IFEQ      '90782'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '44'          QQC(Q)
     C* Err Msg 44: "You may include the charge for medication in the
     C*             payment of 90782."
     C                   Z-ADD     SXMRA         XXMRA
     C                   END                                                    END EQ '90782'
     C*
     C*
     C*-------------------------------------------------------------------
     C                   END                                                    END 0 *IN60
     C* ===============================================================
     C* IF XXPASS = 1ST TIME DON'T PAY, ONLY DO THIS RANGE OF CODE ONCE
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99205'
     C     XXPASS        IFEQ      '1'
     C                   Z-ADD     *ZEROS        XPTOTP
     C                   MOVEL     *BLANKS       XPPMOD
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C                   ENDIF                                                  IF EQ 1
     C                   ENDIF                                                  IF DHPRCD
     C*
     C*================================================================
     C*  Evaluation and Management-General Rule
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
     C  N60              Z-ADD     9999          Q
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
     C                   MOVE      '005'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
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
     C*
     C*** General Rule pay highest in range 99201 - 99215
     C*
     C     DHPRCD        IFGE      '99201'
     C     DHPRCD        ANDLE     '99215'
     C*
     C                   MOVE      '99201'       XXCPT4
     C*
     C     CAKEY8        SETLL     PCA09                              60
     C     CAKEY7        READE     PCA09                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     XRCPT4        IFGE      '99201'
     C     XRCPT4        ANDLE     '99215'
     C     XRTOTP        ANDGT     SXMRA
     C     XRDSEQ        ANDNE     CADSEQ
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
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
     C                   MOVE      '005'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure".
     C*
     C                   EXSR      UPDAT
     C                   GOTO      OFFIE
     C                   END                                                    END XRTOTP  ...
     C     CAKEY7        READE     PCA09                                  60
     C                   ENDDO
     C*
     C                   END                                                    END DHPRCD
     C*
     C* ===============================================================
     C*
     C*** Per pg. 8, 2. d
     C*** When a starred procdures requires hospitilization, an appropriate
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
     C                   MOVE      '10000'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     AGAIN         TAG
     C*
     C     CAKEY7        READE     PCA09                                  54
     C*
     C  N54XRCPT4        IFGE      '10020'
     C     XRCPT4        ANDLE     '69646'
     C*
     C                   MOVE      XRCPT4        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C     PRSUPP        ANDEQ     'Y'
     C                   Z-ADD     1             Q                 5 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '56'          QQC(Q)
     C*** Err Msg 56: "Adjustor should verify use of this procedure"
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTP
     C  N54              Z-ADD     CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPBRTC
     C*
MG11 C                   EXSR      UPDAT
MG11 C                   GOTO      OFFIE
     C*
     C                   ELSE
     C                   GOTO      AGAIN
     C                   END
     C*
     C                   END
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*** Per pg. 8, 2. a
     C*** When a starred procdures is performed at the time of the initial
     C*** office visits, and this procedure constitutes the major service
     C*** rendered during the initial visit, Procedure Code 99025 must be
     C*** used instead of the usual initial office visit code.
     C*
     C     DHPRCD        IFEQ      '99201'
     C     DHPRCD        OREQ      '99202'
     C     DHPRCD        OREQ      '99203'
     C     DHPRCD        OREQ      '99204'
     C     DHPRCD        OREQ      '99205'
     C     DHPRCD        OREQ      '99221'
     C     DHPRCD        OREQ      '99222'
     C     DHPRCD        OREQ      '99223'
     C     DHPRCD        OREQ      '99281'
     C     DHPRCD        OREQ      '99282'
     C     DHPRCD        OREQ      '99283'
     C     DHPRCD        OREQ      '99284'
     C     DHPRCD        OREQ      '99285'
     C     DHPRCD        OREQ      '99242'
     C     DHPRCD        OREQ      '99243'
     C     DHPRCD        OREQ      '99244'
     C     DHPRCD        OREQ      '99245'
     C     DHPRCD        OREQ      '99271'
     C     DHPRCD        OREQ      '99272'
     C     DHPRCD        OREQ      '99273'
     C     DHPRCD        OREQ      '99274'
     C     DHPRCD        OREQ      '99275'
     C     DHPRCD        OREQ      '99291'
     C     DHPRCD        OREQ      '99292'
     C*
     C* SETLL AND READ THE FILE TESTING FOR AN '*' PROCEDURE.
     C*
     C                   MOVE      '10000'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     AGAIN1        TAG
     C*
     C     CAKEY7        READE     PCA09                                  54
     C*
     C  N54XRCPT4        IFGE      '10020'
     C     XRCPT4        ANDLE     '69646'
     C*
     C                   MOVE      XRCPT4        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C     PRSUPP        ANDEQ     'Y'
     C                   Z-ADD     1             Q                 5 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '56'          QQC(Q)
     C*** Err Msg 56: "Adjustor should verify use of this procedure"
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTC            9 2
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      '216'         XPEOMB
     C*** EOMB 216: "Per Fee Schedule: Code 99025 should be listed in liue of
     C**             initial visit"
     C*
MG11 C                   EXSR      UPDAT
MG11 C                   GOTO      OFFIE
     C*
     C                   ELSE
     C                   GOTO      AGAIN1
     C                   END
     C*
     C                   END
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C*
     C*** Per pg. 8, 2. c
     C*** When a starred
     C*** procedure performed at the time of a follow-up office visit,
     C*** reimbursement is limited to the provider's usual charge or the MRA for
     C*** the starred procedure, whichever is less.
     C*
     C     DHPRCD        IFEQ      '99211'
     C     DHPRCD        OREQ      '99212'
     C     DHPRCD        OREQ      '99213'
     C     DHPRCD        OREQ      '99214'
     C     DHPRCD        OREQ      '99215'
     C     DHPRCD        OREQ      '99231'
     C     DHPRCD        OREQ      '99232'
     C     DHPRCD        OREQ      '99233'
     C     DHPRCD        OREQ      '99281'
     C     DHPRCD        OREQ      '99282'
     C     DHPRCD        OREQ      '99283'
     C     DHPRCD        OREQ      '99284'
     C     DHPRCD        OREQ      '99285'
     C     DHPRCD        OREQ      '99242'
     C     DHPRCD        OREQ      '99243'
     C     DHPRCD        OREQ      '99244'
     C     DHPRCD        OREQ      '99245'
     C     DHPRCD        OREQ      '99261'
     C     DHPRCD        OREQ      '99262'
     C     DHPRCD        OREQ      '99263'
     C     DHPRCD        OREQ      '99271'
     C     DHPRCD        OREQ      '99272'
     C     DHPRCD        OREQ      '99273'
     C     DHPRCD        OREQ      '99274'
     C     DHPRCD        OREQ      '99275'
     C     DHPRCD        OREQ      '99291'
     C     DHPRCD        OREQ      '99292'
     C*
     C* SETLL AND READ THE FILE TESTING FOR AN '*' PROCEDURE.
     C*
     C                   MOVE      '10000'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     AGAIN2        TAG
     C*
     C     CAKEY7        READE     PCA09                                  54
     C*
     C  N54XRCPT4        IFGE      '10020'
     C     XRCPT4        ANDLE     '69646'
     C*
     C                   MOVE      XRCPT4        XXCPT4
     C                   MOVE      XRFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C     PRSUPP        ANDEQ     'Y'
     C                   Z-ADD     1             Q                 5 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '56'          QQC(Q)
     C*** Err Msg 56: "Adjustor should verify use of this procedure"
     C*
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTC            9 2
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      '005'         XPEOMB
     C*** EOMB 005: "This procedure is included in the basic allowance
     C***            of another procedure"
     C*
MG11 C                   EXSR      UPDAT
MG11 C                   GOTO      OFFIE
     C*
     C                   ELSE
     C                   GOTO      AGAIN2
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*   If modifier exists, execute the mod check subroutine
     C     XPPMOD        CASGT     *BLANK        PMOD
     C                   END
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTP
     C  N54              Z-ADD     CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPBRTC
     C*
     C                   EXSR      UPDAT
     C*
     C     OFFIE         ENDSR
     *********************************************************************
     *** OPTHMOLOGICAL PROCS SUBROUTINE CODES                    O P T H *
     *********************************************************************
     C     OPTH          BEGSR
     C*
     C* PER SH  SURGERY - Separate procedures.
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
     C     XRCPT4        IFEQ      DHPRCD
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRCPT4        ORNE      DHPRCD
     C     XRTOTP        IFGT      0
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
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
     C* ====================================================================
     C*
     C*   Perform the calculation for adjudication.
     C                   Z-ADD     SXMRA         XXMRA
     C*
     C*   If modifier exists, execute the mod check subroutine
     C     XPPMOD        CASGT     *BLANK        PMOD
     C                   END
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTP
     C  N54              Z-ADD     CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPBRTC
     C*
     C                   EXSR      UPDAT
     C*
     C     OPTHE         ENDSR
     *********************************************************************
     *** HANDLE ALL OTHER CODES   XXXXX-XXXXX                    O T H E *
     *********************************************************************
     C     OTHE          BEGSR
     C*
     C* PER SH  SURGERY - Separate procedures.
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
     C     XRCPT4        IFEQ      DHPRCD
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRCPT4        ORNE      DHPRCD
     C     XRTOTP        IFGT      0
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
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
     C* pg. 105: Home Health Care
     C* The following codes will use the "Anes/Mi/Hr Amt" MRA value:
     C     DHPRCD        IFEQ      'CNAD '
     C     DHPRCD        OREQ      'CNAW '
     C     DHPRCD        OREQ      'FMD  '
     C     DHPRCD        OREQ      'LPND '
     C     DHPRCD        OREQ      'LPNW '
     C     DHPRCD        OREQ      'NAD  '
     C     DHPRCD        OREQ      'NAW  '
     C     DHPRCD        OREQ      'RND  '
     C     DHPRCD        OREQ      'RNW  '
     C     DHPRCD        OREQ      'SLPND'
     C     DHPRCD        OREQ      'SLPNW'
     C     DHPRCD        OREQ      'SRND '
     C     DHPRCD        OREQ      'SRNW '
     C     DHUNTS        DIV(H)    60            XXUNTS
     C*
     C*-------------------------------------------------------------------
     C* pg. 105: Family members are reimbursable up
     C*                     to 12 hours in any 24 hour period
     C     DHPRCD        IFEQ      'FMD  '
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
     C*** If we've already paid more than 12 hours, then disallow code
     C     PHOURS        IFGT      12
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       CATOTA        XPTOTD
     C                   Z-ADD     CACHRG        CACHRG
     C                   MOVE      '009'         XPEOMB
     C*** EOMB 009: "Exceeds reimbursement limitations"
     C                   EXSR      UPDAT
     C                   GOTO      OTHEE
     C                   ELSE                                                   ELSE PHOURS GT 8
     C     12            SUB       PHOURS        LHOURS
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
     C                   END                                                    END EQ FMD
     C*-------------------------------------------------------------------
     C*
     C* Determine "MRA" for Hourly Charges
     C     XXUNTS        MULT(H)   SXANAM        XXMRA
     C*
     C                   ELSE                                                   ELS EQ CNA...
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT(H)   SVUNTS        XXMRA
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C*
     C                   END                                                    END EQ CNA...
     C*
     C*   If modifier exists, execute the mod check subroutine
     C     XPPMOD        CASGT     *BLANK        PMOD
     C                   END
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTP
     C  N54              Z-ADD     CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPBRTC
     C*
     C                   EXSR      UPDAT
     C*
     C     OTHEE         ENDSR
     *********************************************************************
     *** PHYSICAL MEDICINE SUBROUTINE CODES 97010 - 97799        P H Y S *
     *********************************************************************
     C     PHYS          BEGSR
     C*
     C*
     C* PER SH  SURGERY - Separate procedures.
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
     C     XRCPT4        IFEQ      DHPRCD
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRCPT4        ORNE      DHPRCD
     C     XRTOTP        IFGT      0
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
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
     C* ====================================================================
     C*   Find specialty code of physician:
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*- Physical Therapists - Restrictions ------------------------------------
     C*
     C********   QQSPCD    IFEQ 'PHT'
     C********   QQSPCD    OREQ 'LMT'
     C********   QQSPCD    OREQ 'OCT'
     C********   QQSPCD    OREQ 'OT'
     C********
     C*  Per pg. 8:    "An initial evaluation (one time only) charge of
     C*                 $50.00 by a licensed physical therapist is
     C*                 effective March 1, 1992 and a licensed occupational
     C*                 therapist is effective March 1, 1994."
     C     DHPRCD        IFEQ      '97799'
     C                   MOVE      '97799'       XXCPT4
     C*
     C     CAKYC2        CHAIN     PCA0C                              54
     C                   Z-ADD     0             COUNT
     C*
     C     *IN54         DOWEQ     '0'
     C     XCCPT4        IFEQ      '97799'
     C     XCTOTP        IFGT      *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C                   ADD       1             COUNT
     C                   END                                                    END GT *ZERO
     C                   END                                                    END EQ 97799
     C     CAKYC2        READE     PCA0C                                  54
     C                   END                                                    END DOWEQ 54
     C*
     C     COUNT         IFGE      1
     C*   Disallow charges
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      '009'         XPEOMB
     C* EOMB Code 009: "Exceeds number of allowable procedures in
     C*                Reimbursement Manual."
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END GE 1
     C*
     C                   END                                                    END EQ 97799
     C*****************    END                             END  EQ PHT/LMT/OT
     C                   END                                                    END 0 *IN60    /OT
     C*
     *** Additional minutes ---------------------------------------------------
     C     DHPRCD        IFEQ      '97261'
     C     DHPRCD        OREQ      '97501'
     C     DHPRCD        OREQ      '97521'
     C     DHPRCD        OREQ      '97541'
     C*
     C*   Disallow any additional minutes code that doesn't have the proper
     C*   major code associated with it
     C*
     C     DHPRCD        IFEQ      '97261'
     C                   MOVE      '97260'       CPTFR
     C                   MOVE      '97260'       CPTTO
     C                   END
     C*
     C     DHPRCD        IFEQ      '97501'
     C                   MOVE      '97500'       CPTFR
     C                   MOVE      '97500'       CPTTO
     C                   END
     C*
     C     DHPRCD        IFEQ      '97521'
     C                   MOVE      '97520'       CPTFR
     C                   MOVE      '97520'       CPTTO
     C                   END
     C*
     C     DHPRCD        IFEQ      '97541'
     C                   MOVE      '97540'       CPTFR
     C                   MOVE      '97540'       CPTTO
     C                   END
     C*
     C*
     C*   Read the detail charge record to see if it is the major code
     C                   MOVE      CPTFR         XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  54
     C                   Z-ADD     *ZERO         COUNT
     C     *IN54         DOWEQ     '0'
     C     XRCPT4        IFGE      CPTFR
     C     XRCPT4        ANDLE     CPTTO
     C     CADSEQ        IFNE      XRDSEQ
     C                   ADD       1             COUNT
     C                   END                                                    END NE XRDSEQ
     C                   END                                                    END XRCPT4
     C     CAKEY7        READE     PCA09                                  54
     C                   END                                                    END DOWEQ 54
     C*
     C*
     C     COUNT         IFEQ      0
     C                   Z-ADD     *ZEROS        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      '009'         XPEOMB
     C* EOMB Code 009: "Exceeds number of allowable procedures in
     C*                 Reimbursement Manual."
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END
     C*
     C*
     C                   END                                                    END EQ 97261, ETC.
     C* -----------------------------------------------------------------------
     C*  Per pg. 27:  97022 has multiple choice dollar amounts.
     C*
     C     DHPRCD        IFEQ      '97022'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '35'          QQC(Q)
     C* Err Msg 35: "Procedure is paid at "Whirlpool, Small" value,
     C*              if needed adj. paid amt"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '36'          QQC(Q)
     C* Err Msg 36: "Whirlpool, Small - 20.00, Small, Sterile - 25.00,
     C*              Large - 25.00"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '37'          QQC(Q)
     C* Err Msg 37: "Large, Sterile - 30.00"
     C*
     C                   END
     C*
     C* -----------------------------------------------------------------------
     C*  Per pg. 9:    "No more than two therapeutic procedures (not to
     C*                 include physical agent procedures) to be performed per
     C*                 visit by an occupational therapist w/o documentation
     C*                 of necessity."
     C*
     C     DHPRCD        IFGE      '97010'
     C     DHPRCD        ANDLE     '97124'
     C                   MOVE      '97010'       XXCPT4
     C*
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  54
     C*
     C                   Z-ADD     0             COUNT
     C*
     C     *IN54         DOWEQ     '0'
     C     XRCPT4        IFGE      '97010'
     C     XRCPT4        ANDLE     '97124'
     C*
     C     CADSEQ        IFNE      XRDSEQ
     C                   ADD       1             COUNT
     C                   END
     C*
     C                   END
     C     CAKEY7        READE     PCA09                                  54
     C                   ENDDO
     C*
     C     COUNT         IFGE      2
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '41'          QQC(Q)
     C* Err Msg 41: "More than two therapeutic modalities should have
     C*              Document of Necessity"
     C*
     C                   END
     C                   END                                                    END DHPRCD
     C*
     C* -----------------------------------------------------------------------
     C*
     C*   Per page 31:  Work hardening/conditioning use 97545 for first 2 hours
     C*                 only (at $50/hr) then use 97546 (at $37.50/hr)
     C*
     C     DHPRCD        IFEQ      '97545'
     C                   MOVE      '97545'       XXCPT4
     C     CAKEY8        SETLL     PCA09
     C     CAKEY7        READE     PCA09                                  54
     C                   Z-ADD     *ZERO         COUNT
     C     *IN54         DOWEQ     '0'
     C     XRCPT4        IFEQ      '97545'
     C     CADSEQ        IFNE      XRDSEQ
     C     XRTOTP        IFGT      *ZERO
     C                   ADD       1             COUNT
     C                   END                                                    END GT *ZERO
     C                   END                                                    END NE XRDSEQ
     C                   END                                                    END EQ XRCPT4
     C     CAKEY7        READE     PCA09                                  54
     C                   END                                                    END DOWEQ 54
     C     COUNT         IFGT      1
     C                   Z-ADD     37.50         XXMRA
     C                   ELSE                                                   ELS GE 2
     C                   Z-ADD     50.00         XXMRA
     C                   END                                                    END GE 2
     C                   END                                                    END EQ 97545
     C*
     C*
     C*------------------------------------------------------------------------
     C*   Per page 31:  Work hardening/conditioning not to exceed $250.00/day
     C*
     C     DHPRCD        IFGE      '97545'
     C     DHPRCD        ANDLE     '97546'
     C                   MOVE      '97545'       XXCPT4
     C                   Z-ADD     *ZERO         P97546
     C                   Z-ADD     *ZERO         L97546
     C*
     C     DHKY2         SETLL     PDH03
     C     DHKY3         READE     PDH03                                  54
     C*
     C     *IN54         DOWEQ     '0'
     C*
     C                   Z-ADD     D1DSEQ        KKDSEQ
     C*
     C     CAKYP         CHAIN     PCA0P                              60
     C*
     C     D1PRCD        IFGE      '97545'
     C     D1PRCD        ANDLE     '97546'
     C     *IN60         ANDEQ     '0'
     C*
     C     CADSEQ        IFNE      D1DSEQ
     C     PXTOTP        ANDGT     *ZERO
     C     PXFEIN        ANDEQ     CAFEIN
     C     PXMOD         ANDEQ     CAMOD
     C     PXFDAT        ANDEQ     XXYMD
     C                   ADD       PXTOTP        P97546
     C                   END                                                    END NE/EQ/NE
     C                   END                                                    END X6CPT4
     C*
     C     DHKY3         READE     PDH03                                  54
     C                   END                                                    END DOWEQ 54
     C*
     C     250.00        SUB       P97546        L97546
     C     DHPRCD        IFEQ      '97545'
     C     L97546        IFGT      50.00
     C                   Z-ADD     XXMRA         L97546
     C                   END                                                    END GT 50.00
     C                   ELSE                                                   ELSE EQ '97545'
     C     L97546        IFGT      37.50
     C                   Z-ADD     37.50         L97546
     C                   END                                                    END GT 37.50
     C                   END                                                    END EQ '97545'
     C*
     C     L97546        IFLE      *ZERO
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     XPTOTP        XPTOTA
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     *ZEROS        XPBRTC                         STATE CALC MRA
     C                   MOVE      '253'         XPEOMB
     C*** EOMB 253: "Reimbursement for work hardening not to exceed $250/day"
     C                   ELSE                                                   ELSE L97546 LE *ZERO
     C     CACHRG        IFGT      L97546
     C                   Z-ADD     L97546        XPTOTP
     C                   Z-ADD     XPTOTP        XPTOTA
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     L97546        XPBRTC                         STATE CALC MRA
     C                   ELSE                                                   ELSE CACHRG GT L9754
     C                   Z-ADD     CACHRG        XPTOTP
     C                   Z-ADD     XPTOTP        XPTOTA
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     L97546        XPBRTC                         STATE CALC MRA
     C                   END                                                    END CACHRG GT L97546
     C                   END                                                    END L97546 LE *ZERO
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PHYSE
     C                   END                                                    END GE 97545/LE 9754
     C*
     C* ===============================================================
     C***** PER SH REMOVE MINUTE CALC FROM THESE CODES
     C*****
     C* pg. 27: Hourly Charges
     C* The following codes will use the "Anes/Mi/Hr Amt" MRA value:
     C*****      DHPRCD    IFEQ '95880'
     C*****      DHPRCD    OREQ '95882'
     C*****      DHPRCD    OREQ '95883'
     C*****      DHUNTS    DIV  60        XXUNTS    H
     C* Determine "MRA" for Hourly Charges
     C*****      XXUNTS    MULT SXANAM    XXMRA     H
     C*****
     C*****                ELSE                            ELS EQ 95880...
     C*****
     C*****                Z-ADDSXMRA     XXMRA
     C*****
     C*****                END                             END EQ 95880...
     C* ===============================================================
     C*   Per page 31:  Physical performance test or measurement
     C*   with written report, each 15 minutes
     C*   (User will enter units not minutes)
     C*   Not to exceed $600.00 per day.
     C*   NOTE: CHGD FROM PER CLAIM TO PER DAY ON REC RETRIEVAL
     C*
     C     DHPRCD        IFEQ      '97750'
     C                   MOVE      '97750'       XXCPT4
     C                   Z-ADD     *ZEROS        P97750
     C*
     C     CAKEY8        CHAIN     PCA09                              54
     C*
     C     *IN54         DOWEQ     '0'
     C     CAFDAT        IFEQ      XRFDAT
     C*
     C     CADSEQ        IFNE      XRDSEQ
     C     XRTOTP        ANDGT     *ZERO
     C                   ADD       XRTOTP        P97750
     C                   END                                                    END XRDSEQ
     C                   END                                                    END XRFDAT
     C*
     C     CAKEY8        READE     PCA09                                  54
     C                   END                                                    END DOWEQ 54
     C*
     C                   Z-ADD     *ZEROS        XXAMT
     C                   Z-ADD     *ZEROS        XXDIFF
     C     DHUNTS        MULT      SXMRA         XXAMT
     C     XXAMT         ADD       P97750        XXAMT
     C     XXAMT         IFGT      600
     C     600           SUB       P97750        SXMRA
     C                   MOVE      '256'         XPEOMB
     C*** EOMB 256: "Reimbursement for physical performance test
     C***            cannot exceed $600"
     C*
     C                   ELSE
     C     XXAMT         SUB       P97750        SXMRA
     C                   END                                                     END XXAMT
     C                   END                                                     END DHPRCD
     C*
     C* ===============================================================
     C*   Per YC #2  Physical Medicine
     C*   During Data entry, codes 97536 - 97542 on page 31 were found
     C*   to have 2 rates based o n additional minutes.
     C*   Pay accoriding to minutes. initial 15 minutes = initial rate
     C*   additional 15 gets secondary amount.
     C*
     C     DHPRCD        IFGE      '97536'
     C     DHPRCD        ANDLE     '97542'
     C*
     C*  CLEAR HOLDING FIELDS
     C                   Z-ADD     *ZEROS        XXAMT
     C                   Z-ADD     *ZEROS        XXUNTS
     C*
     C*    IF MORE THAN 15 MINUTES REMOVE THE FIRST 15 MINUTES AND
     C*    ADD THE PRIMARY CHARGE THEN CALCULATE THE SECONDARY CHARGE
     C*
     C     DHUNTS        IFGT      15
     C     DHUNTS        SUB       15            XXUNTS
     C     XXUNTS        DIV(H)    15            XXUNTS
     C     XXUNTS        MULT      SXANAM        XXAMT
     C                   ADD       XXAMT         SXMRA
     C*
     C*     IF CHARGE IS LESS THAN 15 MINUTES PAY PRIMARY CHARGE ONLY
     C                   ENDIF
     C                   END                                                     END DHPRCD
     C*
     C* -----------------------------------------------------------------------
     C*
     C*   Perform the calculation for adjudication.
     C*
     C     PHYSA         TAG
     C*
     C                   Z-ADD     SXMRA         XXMRA
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTP
     C  N54              Z-ADD     CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPBRTC
     C*
     C                   EXSR      UPDAT
     C*
     C     PHYSE         ENDSR
     *********************************************************************
     *** CALCULATE MRA BASED ON MODIFIER ENTERED                 P M O D *
     *********************************************************************
     C     PMOD          BEGSR
     C*
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     XPPMOD        TABKEY
     C*
     C     PMKY          CHAIN     PPM01                              64
     C*
     C     PMPERC        IFNE      *ZERO
     C     PMPERC        DIV(H)    100           XXPERC
     C                   END
     C*
     C* = 80/Assisting Surgeon ======================================
     C     XPPMOD        IFEQ      '80'
     C*
     C     SXSURG        IFEQ      'Y'
     C     XXMRA         MULT(H)   XXPERC        XXMRA
     C                   Z-ADD     1             Q                 5 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '71'          QQC(Q)
     C*** Err Msg 71: "Item uses assistant surgeon mod-80.  Adjuster
     C***              should verify usage".
     C*
     C                   ELSE
     C                   Z-ADD     *ZERO         XXMRA
     C     CAEOMB        IFEQ      *BLANK
     C                   MOVE      '004'         CAEOMB
     C                   MOVE      '004'         XPEOMB
     C* EOMB Code 004:Service is not reimburseable under the WC Program
     C                   END
     C*
     C                   END
     C                   GOTO      PMODE
     C*
     C                   END                                                    END EQ '80'
     C*
     C* = 26/Professional Component =================================
     C*
     C     XPPMOD        IFEQ      '26'
     C     DHPRCD        IFGE      '70000'
     C     DHPRCD        ANDLE     '89999'
     C     DHPRCD        OREQ      '93760'
     C     DHPRCD        OREQ      '95819'
     C*
     C                   Z-ADD     SXPROF        XXMRA
     C                   GOTO      PMODE
     C                   ELSE
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C* Err Msg 10: "Invalid modifier used with this item.
     C*              Re-enter item w/valid modifier."
     C*
     C                   Z-ADD     *ZERO         XXMRA
     C                   GOTO      PMODE
     C*
     C                   END
     C                   END
     C*
     C* = 27/Professional Component =================================
     C*
     C     XPPMOD        IFEQ      '27'
     C     DHPRCD        IFGE      '70000'
     C     DHPRCD        ANDLE     '89999'
     C     DHPRCD        OREQ      '93760'
     C     DHPRCD        OREQ      '95819'
     C*
     C                   Z-ADD     PR92N1        XXMRA
     C                   GOTO      PMODE
     C                   ELSE
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C* Err Msg 10: "Invalid modifier used with this item.
     C*              Re-enter item w/valid modifier."
     C*
     C                   Z-ADD     *ZERO         XXMRA
     C                   GOTO      PMODE
     C*
     C                   END
     C                   END
     C* = 20/Microsurgery Modifier ==================================
     C*
     C     XPPMOD        IFEQ      '20'
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '04'          QQC(Q)
     C* Err Msg 04: "This item contains the microsurgery modifier."
     C*
     C                   END
     C*
     C* = 22/Unusual Services Modifier ==============================
     C     XPPMOD        IFEQ      '22'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '08'          QQC(Q)
     C* Err Msg 08: "Unusual services modifier.  Adjuster should verify
     C*              amount to be paid."
     C                   GOTO      PMODE
     C                   END
     C*
     C* = 90 Modifier ===============================================
     C     XPPMOD        IFEQ      '90'
     C*
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTP
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     CACHRG        XPBRTC
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '23'          QQC(Q)
     C* Err Msg 23: "Contains modifier 90, must pay doctors charges in full:
     C                   MOVE      CAEOMB        XPEOMB
     C*
     C                   GOTO      PMODE
     C*
     C                   END
     C*
     C*
     C* = Any other modifier falls thru to here: ====================
     C*
     C     XXMRA         MULT(H)   XXPERC        XXMRA
     C                   GOTO      PMODE
     C*
     C*
     C     PMODE         ENDSR
     *********************************************************************
     *** RADIOLOGY SUBROUTINE  CODES 70010 - 79999               R A D I *
     *********************************************************************
     C     RADI          BEGSR
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT      SVUNTS        XXMRA
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C*
     C* ================================================================ *
     C*  Check for valid modifier
     C*   Valid modifiers are "26"
     C*
     C     XPPMOD        IFNE      *BLANKS
     C     XPPMOD        IFNE      '26'
     C     XPPMOD        ANDNE     '27'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C*** Err Msg 10: "Invalid modifier used with this item.  Re-enter item"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      '001'         XPEOMB
     C*** EOMB 001: "Service is not reimbursable under the Worker's Comp Pgm"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C*
     C                   ELSE                                                   ELS NE 26
     C*
     C* Chain to detail charge file, if you find a charge w/o MOD 26
     C* that has been paid, then deny this (current) charge.
     C                   MOVE      DHPRCD        XXCPT4
     C                   MOVE      *BLANKS       XXPMOD
     C*
     C     DHKY2         CHAIN     PDH03                              60
     C*
     C     *IN60         DOWEQ     '0'
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
     C*
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '29'          QQC(Q)
     C*** Err Msg 29: "This procedure was previously paid under a complete code"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      '222'         XPEOMB
     C*** EOMB 222: "This procedure was previously paid under a complete code"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      RADIE
     C                   END                                                    END 1 *IN60
     C*
     C     DHKY2         READE     PDH03                                  60
     C*
     C                   END                                                    END NE 26
     C*
     C                   EXSR      PMOD
     C*
     C                   END                                                    XPPMOD NE 26
     C*
     C                   END                                                    XPPMOD NE BLANK
     C*
     C* ================================================================ *
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTP
     C  N54              Z-ADD     CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPBRTC
     C*
     C                   EXSR      UPDAT
     C*
     C     RADIE         ENDSR
     *********************************************************************
     *** PATHOLOGY SUBROUTINE  CODES 80000 - 89999               P A T H *
     *********************************************************************
     C     PATH          BEGSR
     C*
     C* ================================================================ *
     C*  Check for valid modifier
     C*   Valid modifiers are "26"
     C*  PER AMELIA KEY - If the provider is billing mod-26
     C*  with pathology, it is up to the payor's discretion to
     C*  to decide how to pay the professional component.
     C*  Note: There are no prof comp amounts in manual for pathology.
     C*
     C     XPPMOD        IFNE      *BLANKS
     C     XPPMOD        IFEQ      '26'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '24'          QQC(Q)
     C*** Err Msg 24: "The item is ANE - requires futher inspection by an
     C*                adjuster"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      '001'         XPEOMB
     C*** EOMB 001: "Service is not reimbursable under the Worker's Comp Pgm"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
     C                   END                                                    END EQ 26
     C*
     C     XPPMOD        IFNE      '26'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C*** Err Msg 10: "Invalid modifier used with this item.  Re-enter item"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      '001'         XPEOMB
     C*** EOMB 001: "Service is not reimbursable under the Worker's Comp Pgm"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      PATHE
     C*
     C                   END                                                    END NE 26
     C                   END                                                    ENDNE *BLANKS
     C*
     C* ================================================================ *
     C     CACHRG        COMP      SXMRA                              54
     C   54              Z-ADD     SXMRA         XPTOTP
     C  N54              Z-ADD     CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     SXMRA         XPBRTC
     C*
     C                   EXSR      UPDAT
     C*
     C     PATHE         ENDSR
     *********************************************************************
     *** SURGERY SUBROUTINE    CODES 10000 - 69646               S U R G *
     *********************************************************************
     C     SURG          BEGSR
     C*
     C* ============================================================
     C*   20550 (Per pg. 36) Give user message:
     C     DHPRCD        IFEQ      '20550'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'EQ'          QQC(Q)
     C*   Err Msg EQ: "Minimum allowance is $35.00, MAX is $75.00"
     C                   END
     C*
     C* ============================================================
     C     DHPRCD        IFEQ      '49505'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '38'          QQC(Q)
     C*   Err Msg 38: "If this procedure is "Bilateral", the MRA is
     C*               $1300.
     C                   END
     C*
     C* ====================================================================
     C* PER SH  SURGERY - Separate procedures.
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
     C     XRCPT4        IFEQ      DHPRCD
     C     XRDSEQ        ANDNE     CADSEQ
     C     XRCPT4        ORNE      DHPRCD
     C     XRTOTP        IFGT      0
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
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
     C* ============================================================
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
     C                   MOVE      '252'         XPEOMB
     C*** EOMB 252: "A major code must be present to pay this item"
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
     C                   MOVE      '252'         XPEOMB
     C*** EOMB 252: "A major code must be present to pay this item"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C                   END
     C* ====================================================================
     C*    Disallow more than two arthroscopic procedures to the knee
     C*    without letter of necessity.
     C     DHPRCD        IFGE      '27310'
     C     DHPRCD        ANDLE     '27332'
     C                   MOVE      '27310'       XXCPT4
     C*
     C     CAKYC         SETLL     PCA0C
     C     CAKYC2        READE     PCA0C                                  54
     C                   Z-ADD     0             COUNT
     C*
     C     *IN54         DOWEQ     '0'
     C     XCCPT4        IFGE      '27310'
     C     XCCPT4        ANDLE     '27332'
     C     XCTOTP        IFGT      *ZERO
     C     CADSEQ        ANDNE     XCDSEQ
     C                   ADD       1             COUNT
     C                   END                                                    END XCTOTP
     C                   END                                                    END XCCPT4
     C     CAKYC2        READE     PCA0C                                  54
     C                   END
     C*
     C     COUNT         IFGE      3
     C*   Disallow charges
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '009'         XPEOMB
     C* EOMB Code 009: "Exceeds number of allowable procedures in
     C*                Reimbursement Manual."
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '30'          QQC(Q)
     C* Err Msg 30: "More than two Arthroscoptic procedures need
     C*              documentation of necessity"
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C*
     C                   END
     C*
     C* ============================================================
     C*   Handle modifider 50 & 51 to cut charges by 50%.
     C     XPPMOD        IFEQ      '50'
     C     XPPMOD        OREQ      '51'
     C     DHPRCD        ANDGE     '10000'
     C     DHPRCD        ANDLE     '69999'
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT(H)   SVUNTS        XXMRA
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C*
     C     XXMRA         DIV(H)    2.00          XPTOTP
     C*
     C     CACHRG        IFLT      XPTOTP
     C                   Z-ADD     CACHRG        XPTOTP
     C                   END
     C*
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XPTOTP        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END
     C*
     C* ============================================================
     C*  Surgical procedures (10000-69999) pay highest $ amount at
     C*  100% , all others pay 50%.
     C*
     C*
     C*  EXCEPTIONS TO THIS RULE WILL BE STARRED PROCEDURES - THESE
     C*  WILL NOT BE WRITTEN TO THE WORK FILE
     C*
     C                   MOVEL     DHPRCD        XXCPT4
     C                   MOVE      CAFDAT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  54
     C     *IN54         IFEQ      '0'
     C     CACO#         ANDEQ     PRCO#
     C     XXCPT4        ANDEQ     PRCPT4
     C     PRSUPP        ANDEQ     'Y'
     C                   GOTO      NOCUT
     C                   END
     C*
     C*   Exceptions are in the 3 following tables NC1,NC2, and TAB1/2.
     C*   If the current code is found in any of these tables, the item
     C*   cannot be cut by this rule and must bypass this section.
     C*
     C                   Z-ADD     1             T
     C     DHPRCD        LOOKUP    NC1(T)                                 60
     C     *IN60         CABEQ     '1'           NOCUT
     C*
     C                   Z-ADD     1             T
     C     DHPRCD        LOOKUP    NC2(T)                                 60
     C     *IN60         CABEQ     '1'           NOCUT
     C*
     C     DHPRCD        LOOKUP    TABA                                   60
     C     *IN60         CABEQ     '1'           NOCUT
     C     DHPRCD        LOOKUP    TABB                                   60
     C     *IN60         IFEQ      '1'
     C                   MOVE      TABA          XXCPT4
     C     CAKEY8        CHAIN     PCA09                              54
     C     *IN54         CABEQ     '0'           NOCUT
     C                   END
     C*
     C*  If the procedure code is between 10000 and 69999,
     C*  write to work file
     C     DHPRCD        IFGE      '10000'
     C     DHPRCD        ANDLE     '69999'
     C********** XPPMOD    ANDEQ*BLANK                     CHG PER SH
     C                   MOVE      'S'           CATYPE
     C                   Z-ADD     SXMRA         CAMRA
     C                   Z-ADD     CACHRG        CATOTC
     C                   Z-ADD     0             XPTOTA
     C                   WRITE     PCAPT1
     C                   GOTO      SURGE
     C*
     C                   END
     C*
     C*
     C*
     C     NOCUT         TAG
     C*
     C*   If current code exists in table, must have associated code else
     C*     error has occurred
     C     DHPRCD        LOOKUP    TABA          TABB                     60
     C     *IN60         IFEQ      '1'
     C                   MOVE      TABB          XXCPT4
     C     CAKEY8        CHAIN     PCA09                              54
     C     *IN54         IFEQ      '1'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '16'          QQC(Q)
     C* Err msg 16: "A major code must be present to pay this item"
     C*
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      '252'         XPEOMB
     C* EOMB Code 252: "A major code must be present to pay this item"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C                   END
     C*
     C                   END
     C*
     C* ============================================================
     C*   Handle modifier 62 to cut charges by 25%.
     C*
     C     XPPMOD        IFEQ      '62'
     C     DHPRCD        ANDGE     '10000'
     C     DHPRCD        ANDLE     '69999'
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT(H)   SVUNTS        XXMRA
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C*
     C     XXMRA         MULT(H)   .75           XPTOTP                                    LESS 25%
     C*
     C     CACHRG        IFLT      XPTOTP
     C                   Z-ADD     CACHRG        XPTOTP
     C                   END
     C*
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XPTOTP        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END
     C*
     C* ============================================================
     C*   Handle modifier 20 to ADD 25% OF MRA TO WHAT IS PAID
     C*   (STILL PAY LESSER OF CHARGE VS. CALCULATED MRA)
     C*
     C     XPPMOD        IFEQ      '20'
     C     DHPRCD        ANDGE     '10000'
     C     DHPRCD        ANDLE     '69999'
     C*
     C     SVUNTS        IFGT      1
     C     SXMRA         MULT(H)   SVUNTS        XXMRA
     C                   ELSE
     C                   Z-ADD     SXMRA         XXMRA
     C                   END
     C*
     C     XXMRA         MULT(H)   1.25          XPTOTP                                    LESS 25%
     C*
     C     CACHRG        IFLT      XPTOTP
     C                   Z-ADD     CACHRG        XPTOTP
     C                   END
     C*
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XPTOTP        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      SURGE
     C*
     C                   END
     C*
     C* ============================================================
     C*   All Other Codes To be handled here.
     C*   Perform the calculation for adjudciation.
     C*
     C                   Z-ADD     SXMRA         XXMRA
     C*
     C*   If modifier exists, execute the Mod check subroutine
     C     XPPMOD        CASGT     *BLANK        PMOD
     C                   END
     C*
     C     CACHRG        COMP      XXMRA                              54
     C   54              Z-ADD     XXMRA         XPTOTP
     C  N54              Z-ADD     CACHRG        XPTOTP
     C     CACHRG        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPBRTC
     C*
     C                   EXSR      UPDAT
     C*
     C     SURGE         ENDSR
     C********************************************************************
     C** Process SURG Codes 10000-69999             S U R G 2          2 *
     C********************************************************************
     C     SURG2         BEGSR
     C*
     C*----------------------------------------------------------------
     C*--- Reading the SURG Detail Charge Workfile --------------------
     C*
     C                   Z-ADD     0             COUNT
     C* Run all records through the following to determine whether it will
     C* be reimbursed at 100%, or 50%
     C     *LOVAL        SETLL     PCAPT2
     C                   READ      PCAPT2                                 60
     C*
     C     *IN60         DOWEQ     '0'
     C                   MOVE      P2CPT4        XXCPT4
     C     PRKEY         SETGT     PPR01
     C     NXTPR4        TAG
     C                   READP     PPR01                                  60
     C     *IN60         IFEQ      '0'
     C     PRCPT4        ANDEQ     XXCPT4
     C     PRSTAT        CABEQ     'I'           NXTPR4
     C                   ELSE
     C                   Z-ADD     0             PRMRA
     C                   END
     C*
     C                   ADD       1             COUNT
     C                   Z-ADD     PRMRA         ZZMRA                          Chrg's MRA
     C*
     C     COUNT         IFGT      1
     C     ZZMRA         DIV(H)    2.00          ZZMRA                          Percentage
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     P2DSEQ        QQS(Q)
     C                   MOVE      '88'          QQC(Q)
     C* Err Msg 88: "If this proc. is located at a different site,
     C*              override and pay at 100% of MRA"
     C                   END                                                    END  GE 2
     C*
     C*** ADDED MODIFIER REDUCTIONS HERE
     C* ============================================================
     C*   Handle modifier 62 to cut charges by 25%.
     C*
     C     P2PMOD        IFEQ      '62'
     C*
     C     P2UNTS        IFGT      1
     C     ZZMRA         MULT(H)   P2UNTS        ZZMRA
     C                   END
     C*
     C     ZZMRA         MULT(H)   .75           ZZMRA                                  LESS 25%
     C*
     C                   END
     C*
     C* ============================================================
     C*   Handle modifier 20 to ADD 25% OF MRA TO WHAT IS PAID
     C*   (STILL PAY LESSER OF CHARGE VS. CALCULATED MRA)
     C*
     C     P2PMOD        IFEQ      '20'
     C*
     C     P2UNTS        IFGT      1
     C     ZZMRA         MULT(H)   P2UNTS        ZZMRA
     C                   END
     C*
     C     ZZMRA         MULT(H)   1.25          ZZMRA
     C*
     C                   END
     C*
     C* ============================================================
     C*   Handle modifier 80, Reduce MRA TO 20%
     C*
     C     P2PMOD        IFEQ      '80'
     C*
     C     P2UNTS        IFGT      1
     C     ZZMRA         MULT(H)   P2UNTS        ZZMRA
     C                   END
     C*
     C     ZZMRA         MULT(H)   .20           ZZMRA
     C*
     C                   END
     C*
     C     P2TOTC        IFGT      ZZMRA
     C                   Z-ADD     ZZMRA         XPTOTP
     C     P2TOTC        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   MOVE      '002'         XPEOMB
     C*** EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   ELSE                                                   ELSE P2TOTC GT ZZMRA
     C                   Z-ADD     P2TOTC        XPTOTP
     C     P2TOTC        SUB       XPTOTP        XPTOTD
     C                   Z-ADD     XPTOTP        XPTOTA
     C     ZZMRA         SUB       XPTOTP        XPDIFF
     C                   END                                                    END P2TOTC GT ZZMRA
     C                   Z-ADD     ZZMRA         XPBRTC
     C                   MOVE      P2PMOD        XPPMOD
     C                   EXSR      UPDAT2
     C*
     C                   READ      PCAPT2                                 60
     C                   END                                                    END DOWEQ 0 *IN60
     C*
     C*
     C     SURG2E        ENDSR
     *********************************************************************
     *** UPDATE SUBROUTINE                                     U P D A T *
     *********************************************************************
     C     UPDAT         BEGSR
     C*
     C* ===============================================================
     C*
     C     XPTOTD        IFGT      *ZERO
     C     XPEOMB        ANDEQ     *BLANK
     C                   MOVE      '002'         XPEOMB
     C* EOMB Code 002: "Charges exceed maximum allowance found in
     C*                 in reimbursement manual"
     C                   END
      *
     C     XPEOMB        IFNE      *BLANKS
     C     XXPASS        ANDEQ     '2'
     C                   Z-ADD     SVDSEQ        KKDSEQ
     C                   EXSR      EOMB
      *
     C                   ENDIF
     C*
     C* ===============================================================
     C*
     C*  When procedure code is 99080 - Refer bill to adjuster.
     C     DHPRCD        IFEQ      '99080'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '01'          QQC(Q)
     C* Err Msg 01: The bill requires further inspection by an adjuster
     C                   END
     C*
     C* ===============================================================
     C*
     C*  When procedure code 29000 - 29999, display error, might be initial
     C*  casts.  Let operator know this.
     C*
     C     DHPRCD        IFGE      '29000'
     C     DHPRCD        ANDLE     '29799'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '07'          QQC(Q)
     C* Err Msg 07: "The item could be an initial cast, adjust the paid
     C*             amount accordingly"
     C                   END
     C*
     C* ===============================================================
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
     C*  Only process the record if not an override type
     C     CAKEYA        CHAIN     PCA01                              54
     C     CSOVRD        IFNE      'Y'
     C                   Z-ADD     XPTOTP        CAPAID
     C                   Z-ADD     XPTOTD        CAMRA
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPDIFF        CADIFF
     C                   Z-ADD     XPBRTC        CABRTC
     C******************** MOVELXPPMOD    XPPMOD
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
     C*  End - of conditional move routine.
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C  N54              UPDATE    PCA01
     C*
     C* ===============================================================
     C*
     C*   If fee schedule NON-REIMBURSEABLE ... display error message
     C*   XX25EM flag is set at beginning of program where PRFSR EQ 'N'
     C*
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
     C                   END                                                    CSOVRD IFEQ
     C*
     C* ===============================================================
     C*
     C** Clear the work save fields.
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPDIFF
     C                   Z-ADD     *ZERO         XPBRTC
     C                   MOVEL     *BLANK        XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE         ENDSR
     C********************************************************************
     C** UPDATE SUBROUTINE FOR SURG  FILE FACAPGA             U P D A T2 *
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
     C     XXPASS        ANDEQ     '2'
      *
     C                   Z-ADD     P2DSEQ        KKDSEQ
     C                   EXSR      EOMB
      *
     C                   ENDIF
     C* ---------------------------------------------------------------
     C*
     C*  Test for a possible duplicate claim.
     C*
     C                   MOVE      P2CPT4        XXCPT4
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
     C     PXFDAT        ANDEQ     P2FDAT
     C     PXFEIN        ANDEQ     P2FEIN
     C     PXFLAG        ANDNE     'P'
     C     PXDSEQ        ANDNE     P2DSEQ
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     9999          Q
     C                   MOVEL     P2DSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C* Err Msg 03: Possible Duplicate claim exists for this item.
     C*
     C                   ENDIF
     C*
     C     DHKY2         READE     PDH03                                  54
     C*
     C                   ENDDO
     C*
     C* ---------------------------------------------------------------
     C*  Using FACAPGA record, chain to FDCAP record to update
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
     C                   Z-ADD     XPDIFF        XRDIFF
     C                   Z-ADD     XPBRTC        XRBRTC
     C******************** MOVELXPPMOD    XRPMOD
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
     C                   Z-ADD     P2FEIN        XXFEIN
     C                   Z-ADD     P2MOD         XXMOD
     C                   MOVE      P2CPT4        XXCPT4
     C                   MOVEL     P2PMOD        XXPMOD
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
     C                   END                                                    CSOVRD IFNE Y
     C*
     C* ---------------------------------------------------------------
     C*
     C** Clear the work save fields.
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZERO         XPDIFF
     C                   Z-ADD     *ZERO         XPBRTC
     C                   MOVEL     *BLANK        XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE2        ENDSR
     *********************************************************************
     C***************************************************************
     OPCA01     E            ECA01
     OPCA08     E            ECA08
     O*
**  NC1 CODES NOT TO CUT LIST 1
22315223252232622327252602526325270252722529025295
25310256302563525645260602608026110261352614026145
26170263502635226356263582641026412264182642026440
26442264452645026455264602647626480264852652026526
26530265312653526536265452660026605266072661026615
26720267252672726730267352674026742267442674626750
26755267602676526861276582766427681280802812628150
28160282002820828450284552846528470284752848028485
28510285152852028525252652527425280253122564026128
26477264782647926483264892652526756276592768628070
2807228202282102822228270282722828828460
**  NC2 CODES NOT TO CUT LIST 2
20500206502066020670211002144021445252302617026180
26500265022709027685280602806228270282882831529815
29830298702988429890308053150531600320203641036620
38100387604383044005448504900049010495605200052310
61050612106228865800660302700527161273062760528232
2825028260298402987564508
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
97260
99195
**  TABA/TABB - A MUST HAVE B AND A WILL NOT BE CUT 50 %
1100111000
1170111700
1171111710
1173111730
1173211731
1510115100
1512115120
1520115200
1522115220
1524115240
1526115260
2258522548
2258522554
2258522556
2258522558
2265022595
2265022600
2265022610
2265022612
2265022625
2612626124
2769227691
6303563031
6303563030
6303563021
6307663075
6308263081
6309163090
6444364442
6462364622
6477864776
6483264831
6483734834
6483764835
6483764836
**  TABMU3/TABMU4 - MODIFIER UNIT VALUES
P10
P20
P31
P42
P53
P60
**  QCMDEXC TABLE
DLTOVR FILE(FACALGA)
OVRDBF FILE(FACALGA) TOFILE(QTEMP/FACALGA)
DLTOVR FILE(FACAPGA)
OVRDBF FILE(FACAPGA) TOFILE(QTEMP/FACAPGA)
